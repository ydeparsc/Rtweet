#install.packages("twitteR")
#install.packages("tidyverse")
#install.packages("reshape")

#graph package
#install.packages("ggplot2")

# stringr and dplyr
library(reshape)
library(dplyr)
library(stringr)
library(lubridate)
library(twitteR)
library(ggplot2) 

GROUP_DAYS_TWEETS = 10

BAD_WORDS = c("bad", "attack", "terrorist", "military", "disaster", "die", "problem", "failing", 
              "fake", "nuclear", "deaths", "dramatic", "hostile", "deadly", "corruption")

GOOD_WORDS = c("good","happy", "thanks", "victory", "great", "honor", "win", "love", "fantastic", 
               "proud", "amazing", "excitement")

# Twitter's keys and tokens
consumer_key <- "J8d4n2vUZSF8TdNjAdEvtYJVq"
consumer_secret <- "TtYswSPlOZCBLLA9JleCIpMBDLgbI8BEU12iGZcaGQGSGB9K5M"
access_token <- "1043294216426803201-9Q9luG3biEMHgWsgPEQSqOZAeYUAO0"
access_secret <- "HeMHyOBYzu8tcleg4OJCNhO0PlmlsssjsMdqTRxyyNPjt"

# oauth twitter (with plugin twitteR)
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# donald_trump user
donald_trump = getUser('realDonaldTrump')
user_tweets = userTimeline(donald_trump, n=3200, maxID=NULL, sinceID=NULL, includeRts=TRUE,
             excludeReplies=TRUE)

# transform object user_tweet into dataframe
tweets_df = twListToDF(user_tweets)

# Dataframe with column:
# - date
# - count (tweet with bad words)
bad_mood_df = tweets_df %>% 
  select(text, created) %>% 
  filter(str_detect(text, paste(BAD_WORDS, collapse = "|"))) %>%
  group_by(date = floor_date(as.Date(created), paste(GROUP_DAYS_TWEETS, "days", sep = " "))) %>%
  summarise(count_bad = n())

# Dataframe with column:
# - date
# - count (tweet with good words)
good_mood_df = tweets_df %>% 
  select(text, created) %>% 
  filter(str_detect(text, paste(GOOD_WORDS, collapse = "|"))) %>%
  group_by(date = floor_date(as.Date(created), paste(GROUP_DAYS_TWEETS, "days", sep = " "))) %>%
  summarise(count_good = n())

# get max and min date of all Donald Trump's tweets
min_date = min(tweets_df[, "created"])
max_date = max(tweets_df[, "created"])

# Create new DataFrame with column:
# - date
# - count (tweet with good word)
# - count (tweet with bad word)
all_mood_df = data.frame(date = seq(as.Date(min_date),as.Date(max_date), GROUP_DAYS_TWEETS)) %>% 
  right_join(bad_mood_df, by = 'date') %>% 
  right_join(good_mood_df, by = 'date')

# Replace all NA row by 0 and melt to get all values on one column
all_mood_df[is.na(all_mood_df)] = 0
all_mood_df = melt(all_mood_df, id=c("date"))

# PLOT !
ggplot(all_mood_df, aes(date, y = value, colour = variable)) +
  geom_step(aes(y = value)) +
  labs(title = "Mood Donald", y = "count tweets")

ggplot(all_mood_df, aes(date, y = value, colour = variable)) +
  geom_point(aes(y = value)) +
  labs(title = "Mood Donald", y = "count tweets")

ggplot(all_mood_df, aes(date, y = value, colour = variable)) +
  geom_line(aes(y = value)) +
  labs(title = "Mood Donald", y = "count tweets")

ggplot(all_mood_df, aes(date, weight = displ, fill = variable)) +
  geom_bar(aes(weight = value)) +
  labs(title = "Mood Donald", y = "count tweets")



