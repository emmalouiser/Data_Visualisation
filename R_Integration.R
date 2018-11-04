#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

oauth_endpoints("github")

myapp <- oauth_app(appname = "ELR_Assignment",
                   key = "6b4cef7d873677784b87",
                   secret = "89644daf2f6c53c9f60cfb4402221a3a49b02897")

# oauth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# take action on http error
stop_for_status(req)

# extract content from a request
json1 = content(req)

# convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"]


# Code above has been sourced from the following website:
#https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08
