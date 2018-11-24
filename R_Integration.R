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

getFollowers <- function(username)
{
  URL <- paste("https://api.github.com/users/", username , "/followers", sep="")
  followers = fromJSON(URL)
  return (followers$login)
}

getFollowing <- function(username)
{
  URL <- paste("https://api.github.com/users/", username , "/following", sep="")
  followers = fromJSON(URL)
  return (followers$login)
}

getRepositories <- function(username)
{
  URL <- paste("https://api.github.com/users/", username , "/repos", sep="")
  repos = fromJSON(URL) 
  return (repos$name)
}

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
  return (p)
}
social_graph(toPlot, labels)

