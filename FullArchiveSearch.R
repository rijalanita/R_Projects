#Title: Collecting and Analysing Tweets on COVID-19, Zika and Ebola epidemics.
#Author: Anita Rijal
#Version: 1
#Date: May 25, 2020

# loading library
library(rtweet)
library(dplyr)

# setting the connection
token_2 <- create_token(
  app <- "xx", 
  consumer_key <- "xx",
  consumer_secret <- "xx",
  access_token <- "x-x",
  access_secret <- "x" )

#THEME: EBOLA
# COLLECTING HISTORICAL TWITTER API DATA
final_query <- "(ebola OR Ebola OR Ebola virus OR ebola epidemic OR Ebola epidemic OR #ebola OR #ebolavirus) lang:en"

#search tweets on ebola from May 2014 to May 2015

#1
fromdates = c('201405010000', 
              '201406010000',
              '201407010000', 
              '201408010000', 
              '201409010000',
              '201410010000', 
              '201411010000',
              '201412010000',
              '201501010000',
              '201502010000',
              '201503010000',
              '201504010000')
              
todates = c('201405310000', 
              '201406300000',
              '201407310000', 
              '201408310000', 
              '201409300000',
              '201410310000', 
              '201411300000',
              '201412310000',
              '201501310000',
              '201502280000',
              '201503310000',
              '201504300000')

#Collecting historical tweets on Ebola from twitter API

for (i in (1:12)) {
  tweet_query <- search_fullarchive(q = final_query,
                                    n = 1, 
                                    fromDate = fromdates[i],
                                    toDate = todates[i],
                                    env_name = "textdata",
                                    safedir = NULL,
                                    parse = TRUE, 
                                    token = token_2)
  assign(paste("tweet", i, sep = "_"), tweet_query)
  i = i +1
}

all_tweets <- do.call(rbind.data.frame, list(tweet_1, tweet_2, tweet_3, tweet_4, tweet_5, tweet_6, tweet_7, tweet_8, tweet_9, tweet_10, tweet_11, tweet_12))
View(all_tweets)

