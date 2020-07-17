### Sachsenwahl 2019 ###
#######################
### Data Exploration ###
########################

setwd("----")
load("sachsenwahl_tweets.RDa")
load("sachsenwahl_botscores.RDa")


library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(Cairo)



# Correct date-time format
sachsenwahl_tweets$created_at <- as_datetime(sachsenwahl_tweets$created_at, tz = "CET")



## Volume Timeline    
# (original code by Felix Haass, tweaked)

minDate <- min(sachsenwahl_tweets$created_at)
maxDate <- max(sachsenwahl_tweets$created_at)
dateBreaks <- seq(minDate, maxDate, by=60 * 60)
dateBreaks <- c(dateBreaks, maxDate + 60 * 60)
tweetCount <- hist(sachsenwahl_tweets$created_at, breaks=dateBreaks, plot=FALSE)                             
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]

# prepare plot data
volume_plot_data <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count))

# save plot
CairoPNG("SW19_Twitter_trend.png", width=1600, height=900, pointsize=8, dpi=160)
ggplot(volume_plot_data) +
  geom_bar(aes(x=dates, y=tweets), stat="identity") +
  scale_y_continuous("Number of tweets") +
  scale_x_datetime(date_breaks="2 day") +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
  labs(x="", title="")
# Rate Limit hit briefly on election day
dev.off()



## Hashtags

hashtags <- table(tolower(unlist(sachsenwahl_tweets$hashtags))) %>% as.data.table()
hashtags_top <- hashtags[!(hashtags$V1 %in% c("ltwsn19", "ltwsachsen", "sachsenwahl", "sltw19", "sachsenwahl2019", "sachsen")),]
hashtags_top <- tail(hashtags_top[order(hashtags_top$N), ], 10)


## Plot Top Hashtags over time ##

hashtags_date <- subset(sachsenwahl_tweets, select=c(hashtags, created_at)) %>% as.data.table()
hashtags_date <-  unnest(hashtags_date, hashtags)
hashtags_date$hashtags <- tolower(hashtags_date$hashtags)
hashtags_date <- subset(hashtags_date, !(hashtags_date$hashtags %in% 
                                           c("ltwsn19", "ltwsachsen", "sachsenwahl", "sltw19", "sachsenwahl2018", "sachsen"))) 
# (also drops NAs)


# top 10 only 
hashtags_date <- subset(hashtags_date, subset=(hashtags %in% hashtags_top$V1)) 

hashtags_date <- table(hashtags_date) %>% as.data.table()
hashtags_date$created_at <- as_date(hashtags_date$created_at)
# aggregate by day
hashtags_date <- aggregate(N ~ hashtags + created_at, FUN = sum, data=hashtags_date)
hashtags_date$hashtags <- paste0("#", hashtags_date$hashtags)

# save plot
CairoPNG("Plots/SW19_hashtag_trend.png", width=1600, height=900, pointsize=8, dpi=160)
ggplot(hashtags_date) +
  geom_line(aes(x=as.POSIXct(created_at), y=N, colour=hashtags), stat="identity") +
  scale_y_continuous("Number of tweets") +
  scale_x_datetime(date_breaks="1 day") +
  scale_color_brewer(type="qual", palette="Paired") +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
  labs(x="", title="")

dev.off()




