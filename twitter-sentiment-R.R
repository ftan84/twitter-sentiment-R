library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)

query <- 'slothweek') # The word we want to analyze. Change this
maxTweets <- 10000 # The maximum number of tweets to search

consumer.key <- 'v5b3mm9hRjZ7kL7gcOUvxwr4m'
consumer.secret <- 't2NZr2T18jOEFcX8h2B1jS8k8JH5jwD2eVUZBFyGAdEVI22GT6'
access.token <- '1149944478-vXoBfwZ718WW12m6xFjoGHEvjQk73H1qP1g2q0Z'
access.token.secret <- 'muJVicR46pW7dlMqxDkyAakJMDtijFyPeMCzcETU5ohf0'
setup_twitter_oauth(consumer_key=consumer.key, consumer_secret=consumer.secret, access_token=access.token, access_secret=access.token.secret)