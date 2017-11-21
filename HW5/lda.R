library(knitr)
library(ggplot2)
library(quanteda)
library(MASS)
library(caret)
library(tm)
library(pander)
library(dplyr)
library(qdap)
library(stringr)
library(e1071)
library(tidyverse)
library(bindr)
library(class)
library(scrime)
library(ROCR)
library(scales)
library(reshape)
library(Rmpfr)
library(wordcloud)
library(RColorBrewer)


k <- 25
burnin <- 1000
iter <- 1000
keep <- 50

#Read the raw data
tweet_raw<- read.csv("final_data.csv")
tweet_raw<- as.data.frame(tweet_raw[,2:3])


replacePunctuation <- function(x)
{
  x <- tolower(x)
  x <- gsub("[.]+[ ]"," ",x)
  x <- gsub("[:]+[ ]"," ",x)
  x <- gsub("[?]"," ",x)
  x <- gsub("[!]"," ",x)
  x <- gsub("[;]"," ",x)
  x <- gsub("[,]"," ",x)
  x
  
}

removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x) 
myStopwords <- c(stopwords('english'), "rt","&amp","/")
tweet_raw$text<-bracketX(tweet_raw$text,"angle")
tweet_raw$text<- gsub("@\\w+","",tweet_raw$text)
tweet_raw$text<- rm_nchar_words(tweet_raw$text, "1,2")
tweet_raw$text<- gsub( " *\\(.*?\\) *", "", tweet_raw$text)


#preprocessing
tweet_corpus <- Corpus(VectorSource(tweet_raw$text))
tweet_corpus_clean <- tweet_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removeWords,myStopwords) %>%
  tm_map(content_transformer(replacePunctuation)) %>%
  tm_map(stripWhitespace)%>%
  tm_map(content_transformer(removeURL))

preprocess<-as.data.frame(tweet_corpus_clean)

tweet_dtm <- DocumentTermMatrix(tweet_corpus_clean)

## Optimal Number of topics
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}


fitted <- topicmodels::LDA(tweet_dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )

## assuming that burnin is a multiple of keep
logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

## This returns the harmomnic mean for k = 25 topics.
harmonicMean(logLiks)

seqk <- seq(2, 100, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(tweet_dtm, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') 

optimal_topics <- seqk[which.max(hm_many)]

tweet.model <- topicmodels::LDA(tweet_dtm, 18, method = "Gibbs", control = list(iter=2000, seed = 0622))
tweet.topics <- topicmodels::topics(tweet.model, 1)

#Top 30 terms
## In this case I am returning the top 30 terms.
tweet.terms <- as.data.frame(topicmodels::terms(tweet.model, 30), stringsAsFactors = FALSE)
tweet.terms[1:5]

write.csv(tweet.terms,file="topics.csv")

# Visualization with word cloud
topic <- 1
df <- data.frame(term = tweet.model@terms, p = exp(tweet.model@beta[topic,]))
head(df[order(-df$p),])





