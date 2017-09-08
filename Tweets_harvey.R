#connect all libraries
library(twitteR)
library(ROAuth)
library(dplyr)
library(stringr)
library(ggplot2)
library(quanteda)
library(tm)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
library(ggplot2)
library(graph)
library(Rgraphviz)
library(wordcloud)
library(qdap)
library(Rstem)
library(ggmap)



#Connection from twitter api
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

consumerKey <- 'tGupCLnGwczTfmC1bZxJfzSLG' #put the Consumer Key from Twitter Application
consumerSecret <- 'OzMp1mzSuim1b3hznLdhXUv1FLMvoMAl2x0qqDXiRC80ZjC5Ba'  #put the Consumer Secret from Twitter Application

Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)

Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) #There is URL in Console. You need to go to it, get code and enter it on Console
setup_twitter_oauth("tGupCLnGwczTfmC1bZxJfzSLG", "OzMp1mzSuim1b3hznLdhXUv1FLMvoMAl2x0qqDXiRC80ZjC5Ba", "96316198-8XxJ2bzHf8cOaCVatORSFBAAeQhFoWecIbZtx31jC", "OHxwSSmRgvRUBkzfOh1AOaeArVjOJwAZIVWWeY7yz76qq")
save(Cred, file='twitter authentication.Rdata')
load('twitter authentication.Rdata') #Once you launch the code first time, you can start from this line in the future (libraries should be connected)

tweets <- searchTwitter("#Harvey", n=1000,lang='en',resultType = 'NULL',since='2017-08-26',until='2017-09-07',geocode='29.76043,-95.3698,200mi')
#rdmTweets <- userTimeline(user='the_vibhuti',n=1000)

#Transforming text
df<-twListToDF(tweets)
write.csv(df,file="Tweets_Harvey.csv")

