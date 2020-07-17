### Sachsenwahl 2019: Bot check ###


## Prepare

load("twittertoken.RDa")
load("MashapeKey.RDa")

setwd("Sachsenwahl 2019")
load("sachsenwahl_tweets.RDa")
  #  tweets are saved as data.table
library(data.table)
library(httr)
library(RJSONIO)
library(stringr)


## Select active users

# identify unique users
users <- data.table(table(sachsenwahl_tweets$screen_name))
names(users) <- c("screen_name", "tweets")
  # = 36.567 unique users

# Filter for users with a minimum number of tweets (for relevance) 
summary(sachsenwahl_tweets$created_at)
  # -> Data collected for the time betweend th 15th of July and the 13th of September
  # -> Data collection started on the 25th of July, data between 15th and 25th might be sparse

# Threshold: minimum of 25 tweets per User in the Sample
active_users <- subset(users, tweets > 24)
  # -> 1093 "active users"


## Botometer 

# Call the Osome Botometer API
botometer <- function(screen_name, twitter_token, MashapeKey) {
  
  user.url <- "https://api.twitter.com/1.1/users/show.json?screen_name="
  timeline.url <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  mentions.url <- "https://api.twitter.com/1.1/search/tweets.json?q=%40"
  
  user <- httr::GET(paste0(user.url, screen_name), twitter_token)
  timeline <- httr::GET(paste0(timeline.url, screen_name, "&count=200&include_rts=true"), twitter_token)
  mentions <- httr::GET(paste0(mentions.url, screen_name, "&count=100"), twitter_token)
  
  payload <- list(
    user = content(user, type="application/json")[1:4],
    timeline = content(timeline, type="application/json"),
    mentions = content(mentions, type="application/json"))
  
  payload.json <- RJSONIO::toJSON(payload, auto_unbox = T)
  
  result = httr::POST("https://osome-botometer.p.mashape.com/2/check_account",
                      add_headers(
                        "X-Mashape-Key"=MashapeKey,
                        "Content-Type" = "application/json",
                        "Accept" = "application/json"),
                      body=payload.json,
                      encode = "json")
  
  result = httr::content(result, as = "parsed")
  return(result)
}


# Check active users
sachsenwahl_botscores <- data.table()
pb = txtProgressBar(min = 0, max = nrow(active_users), initial = 0) 
for(i in 1:length(active_users$screen_name)) {
  user <- active_users$screen_name[i]
  result <- botometer(user, twitter_token, MashapeKey)
  score <- data.table(t(sapply(unlist(result),c)))
  sachsenwahl_botscores <- rbind(sachsenwahl_botscores, score, fill=T, use.names=T)
  setTxtProgressBar(pb, i)
}
    # print() command of screen_name for troubleshooting?
save(sachsenwahl_botscores, file="sachsenwahl_botscores.RDa")

#   # How to deal with renamed accounts? (e.g. AB47115053 - now CD47115053)
# sum(complete.cases(sachsenwahl_botscores[,1:22]))
# sum(!is.na(sachsenwahl_botscores$message))
#   # 1138 complete cases, 18 missing 
# 
# sachsenwahl_botscores[,1:18] <- lapply(sachsenwahl_botscores, as.numeric)
# 
# summary(sachsenwahl_botscores$cap.universal)
# summary(sachsenwahl_botscores$scores.universal)


