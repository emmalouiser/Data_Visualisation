#Interrogate the GitHub API to build visualisation of data available tht elucidates some 
#aspect of the softare engineering process, such as a social graph of developers and projects,
#or a visualisation of indiviudal of team performance. Provide a visualisation of this using
#the d3js library.

#install.packages("jsonlite")
#install.packages("httr")
#install.packages("httpuv")
#install.packages("plotly")
#install.packages('devtools')
library(plotly)
library(jsonlite)
library(httpuv)
library(httr)
library(devtools)
library(magrittr)
library(dplyr)
detach(package:plotly, unload=TRUE)

oauth_endpoints("github")
myapp <- oauth_app(appname = "ELR_Assignment",
                   key = "6b4cef7d873677784b87",
                   secret = "89644daf2f6c53c9f60cfb4402221a3a49b02897")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/emmalouiser/repos", gtoken)
req <- GET("https://api.github.com/users/emmalouiser/following", gtoken)
stop_for_status(req)
json1 = content(req)
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
gitDF$login

gitDF[gitDF$full_name == "emmalouiser/Data_Visualisation", "created_at"]

# Code above has been sourced from the following website:
#https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08

#Connecting to my Plotly account 
Sys.setenv("plotly_username"="emmalouiser")
Sys.setenv("plotly_api_key"="BfyUmOXhR5ez4G1FlOqe")

#Function that returns a list of the provided user's followers. 
getFollowers <- function(username)
{
  URL <- paste("https://api.github.com/users/", username , "/followers", sep="")
  followers = fromJSON(URL)
  return (followers$login)
}

#Function that returns a list of the people the provided user is following. 
getFollowing <- function(username)
{
  URL <- paste("https://api.github.com/users/", username , "/following", sep="")
  followers = fromJSON(URL)
  return (followers$login)
}

#Function that returns a list of the provided user's repositories
getRepositories <- function(username)
{
  URL <- paste("https://api.github.com/users/", username , "/repos", sep="")
  repos = fromJSON(URL) 
  repos$committs
  return (repos$name)
}

#Function that draws a netowrk graph of me and my followers to see who follows who. 
make_social_graph <- function(toPlot, labels)
{
  username <- 'emmalouiser'
  myFollowers <- getFollowers('emmalouiser')
  labels <- c(username)
  toPlot <- c()
  for(i in 1:length(myFollowers))
  {
    their_username <- myFollowers[i]
    labels = c(labels, their_username)
    toPlot = c(toPlot, username, their_username)
  }
  
  for(i in 1:length(myFollowers))
  {
    username <- myFollowers[i]
    theirFollowers <- getFollowers(username)
    for (j in 1:length(theirFollowers))
    {
      if (is.element(theirFollowers[j], myFollowers))
      {
        toPlot = c(toPlot, username, theirFollowers[j])
      }
      else
      {
        next
      }
    }
  }
  social_graph(toPlot, labels)
}

#REFERENCE
#Code below sourced from https://plot.ly/r/network-graphs/
social_graph <- function(toPlot, labels)
{
  library(plotly)
  library(quantmod)
  library(igraph)

  g<-make_graph(edges=c(toPlot))
  G <- upgrade_graph(g)
  L <- layout.circle(G)
  vs <- V(G)
  es <- as.data.frame(get.edgelist(G))
  Nv <- length(vs)
  Ne <- length(es[1]$V1)
  Xn <- L[,1]
  Yn <- L[,2]
  
  network <- plot_ly(x = ~Xn, y = ~Yn, mode = "markers", text = labels, hoverinfo = "text", type="scatter")
  
  edge_shapes <- list()
  for(i in 1:Ne) 
  {
    v0 <- es[i,]$V1
    v1 <- es[i,]$V2
    
    edge_shape = list(
      type = "line",
      line = list(color = "#030303", width = 0.3),
      x0 = Xn[v0],
      y0 = Yn[v0],
      x1 = Xn[v1],
      y1 = Yn[v1]
    )
    
    edge_shapes[[i]] <- edge_shape
  }
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  p <- layout(
    network,
    title = 'Followers',
    shapes = edge_shapes,
    xaxis = axis,
    yaxis = axis
  )
  chart_link = api_create(p, filename="social_graph")
  return (chart_link)
}

#Draws a bar chart of the ratio of the people I follow's followers to following. 
ratio_graph <- function()
{
  my_username <- 'emmalouiser'
  users <- c(my_username)
  myFollowers <- getFollowers(my_username)
  myFollowing <- getFollowing(my_username)
  my_ratio <- length(myFollowers)/length(myFollowing)
  ratios <- c(my_ratio)
  
  for(i in 1:length(myFollowing))
  {
    their_username <- myFollowing[i]
    users = c(users, their_username)
    theirFollowers <- getFollowers(their_username)
    theirFollowing <- getFollowing(their_username)
    if (theirFollowing == 0)
    {
      their_ratio <- length(theirFollowers)
    }
    else
    {
      their_ratio <- length(theirFollowers)/length(theirFollowing)
    }
    ratios <- c(ratios, their_ratio)
  }
  
  library(plotly)
  p <- plot_ly( x = users, y = ratios, name = "Ratio of Followers to Following", type = "bar")
  chart_link = api_create(p, filename="ratio_graph")
  return (chart_link)
}


#Bar chart that allows comparison of the number of repositories created by my users. 
repository_graph <- function()
{
  my_username <- 'emmalouiser'
  users <- c(my_username)
  myRepositories <- length(getRepositories(my_username))
  repositories <- c(myRepositories)
  
  for(i in 1:length(myFollowing))
  {
    their_username <- myFollowing[i]
    users = c(users, their_username)
    theirRepositories <- length(getRepositories(their_username))
    repositories <- c(repositories, theirRepositories)
  }
  
  library(plotly)
  p <- plot_ly( x = users, y = repositories, name = "Number of Repositories of Each User", type = "bar")
  chart_link = api_create(p, filename="repository_graph")
  return (chart_link)
}

#Function that creates a graph that allows comparison 
followers_graph <- function()
{
  my_username <- 'emmalouiser'
  users <- c(my_username)
  myFollowers <- (getFollowers(my_username))
  myFollowing <- (getFollowing(my_username))
  myFollowersNo <- length(getFollowers(my_username))
  myFollowingNo <- length(getFollowing(my_username))
  followers <- c(myFollowersNo)
  following <- c(myFollowingNo)
  
  for(i in 1:length(myFollowing))
  {
    their_username <- myFollowing[i]
    users = c(users, their_username)
    theirFollowers <- length(getFollowers(their_username))
    followers <- c(followers, theirFollowers)
    theirFollowing <- length(getFollowing(their_username))
    following <- c(following, theirFollowing)
  }
  
  library(plotly)
  data <- data.frame(users, followers, following)
  p <- plot_ly(x = ~users, y = ~followers, type = 'bar', name = 'Followers') %>%
    add_trace(y = ~following, name = 'Following') %>%
    layout(yaxis = list(title = 'Number of Users'), barmode = 'group')
  chart_link = api_create(p, filename="followers_graph")
  return (chart_link)
}

followers_graph <- followers_graph() 
repository_graph <- repository_graph()
social_graph <- make_social_graph()
ratio <- ratio_graph()


getAvgCommitts <- function()
{
  users <- c('emmalouiser', getFollowing('emmalouiser'))
  avg_committs = c()
  for (i in 1:length(users))
  {
    URL <- paste("https://api.github.com/users/", users[i] , "/repos", sep="")
    req <- GET(URL, gtoken)
    json1 = content(req)
    gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
    list_repos <- as.vector(gitDF$name)
    
    numberofcommits = c()
    for (j in 1:length(list_repos))
    {
      URL <- paste("https://api.github.com/repos/", users[i] , "/", list_repos[j], "/commits", sep="")
      req <- GET(URL, gtoken)
      json1 = content(req)
      gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
      numberofcommits = c(numberofcommits, length(gitDF$commit$committer$name))
    }
    avg_committs = c(avg_committs, mean(numberofcommits))
  }
  return(avg_committs)
}

average_committs_plot <- function()
{
  avg_committs <- getAvgCommitts()
  users <- c('emmalouiser', getFollowing('emmalouiser'))
  library(plotly)
  p <- plot_ly( x = ~users, y = ~avg_committs, name = "Number of Repositories of Each User", type = "bar")
  chart_link = api_create(p, filename="average_committs_graph")
  return (chart_link)
}

average_committs_plot()


getLanguages <- function()
{
  users <- c('emmalouiser', getFollowing('emmalouiser'))
  languages = c()
  for (i in 1:length(users))
  {
    URL <- paste("https://api.github.com/users/", users[i] , "/repos", sep="")
    repos = fromJSON(URL) 
    languages = c(languages, repos$language)
  }
  return(as.data.frame(table(languages)))
}

plot_languages <- function()
{
  languagesTable <- getLanguages()
  
  p <- plot_ly( x = ~languagesTable$languages, y = ~languagesTable$Freq, name = "Most Popular Languages", type = "bar")
  chart_link = api_create(p, filename="languages_graph")
  return (chart_link)
  
}

  





