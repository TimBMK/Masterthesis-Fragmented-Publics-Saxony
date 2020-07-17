#### Sachsenwahl 2019 Combine ####

library(data.table)

rm(list = ls()) 

setwd("E:/Nextcloud/R/Sachsenwahl 2019/Data")

## Zusammenführen

filenames <- list.files(pattern = "sachsenwahl_search*")

for (i in 1:length(filenames)){
  load(filenames[i])
}

searches <- mget(ls(pattern="sachsenwahl_"))

sachsenwahl_search <- rbindlist(searches)


## Duplikate entfernen
sum(duplicated(sachsenwahl_search$status_id))
sachsenwahl_tweets <- unique(sachsenwahl_search, fromLast = T, by="status_id")
# fromLast um letzten favourite_count etc. zu behalten
sum(duplicated(sachsenwahl_tweets$status_id))
save(sachsenwahl_tweets, file="sachsenwahl_tweets_full.RDa")



# ### Zeitraum auf Hessenwahl Scrape - Länge einschränken ###
# 
# library(lubridate)
# 
# load("E:/Nextcloud/R/Hessenwahl 2018/hessenwahl_tweets.RDa")
# range(hessenwahl_tweets$created_at)
# # Hessenwahl Scrape: 2018-10-05 bis 2018-10-30; Wahl am: 2018-10-28
# 
# range(sachsenwahl_tweets$created_at)
# # Sachsenwahl Scrape: 2019-07-15 bis 2019-09-13; Wahl am: 2019-09-01
# 
# 
# # => Soll: 23 Tage vor Wahl bis 2 Tage nach, d.i.: 2019-08-09 bis 2019-09-03. 
# #   Aufgrund von erhöhtem Tweetaufkommen (siehe exploration): bis 5.9.
# 
# sachsenwahl_tweets <- sachsenwahl_tweets[sachsenwahl_tweets$created_at > "2019-08-09 00:00:00" &
#                                            sachsenwahl_tweets$created_at < "2019-09-05 23:59:59"]
# range(sachsenwahl_tweets$created_at)
# with_tz(range(sachsenwahl_tweets$created_at), "CET")
# 
# save(sachsenwahl_tweets, file="sachsenwahl_tweets_1month.RDa")


