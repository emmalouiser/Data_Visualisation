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

myFollowers <- getFollowers('emmalouiser')
i <- 1
while (length(myFollowers) < 500)
{
  print(myFollowers)
  myFollowers = unique(c(myFollowers, getFollowers(myFollowers[i])))
  i <- i+1
}


