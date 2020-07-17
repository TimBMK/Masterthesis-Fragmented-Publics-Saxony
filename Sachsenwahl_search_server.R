## Sachsenwahl 2019 - regular Search via REST API

load("twittertoken.RDa")
setwd("Sachsenwahl 2019")

library(rtweet)

# Search
q = "#sachsenwahl OR #ltwsachsen OR #ltwsn OR #ltwsn19 OR #sltw19"             

twitter_search <- search_tweets(q, n = 17000, token = twitter_token)  

search_time <- substr(gsub(" ", "_", gsub("-|:", "", Sys.time())), 3, 13) 

search_name <- paste("sachsenwahl_search", search_time, sep= "")  

assign(search_name, twitter_search, envir = .GlobalEnv)

# lokal speichern 
filename_search = paste(search_name,".RDa", sep= "")
save(list=search_name, file = filename_search)
