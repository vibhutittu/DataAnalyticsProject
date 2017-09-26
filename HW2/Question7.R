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

raw_data<- read.csv("raw_tweets.csv")
raw_data<- as.data.frame(raw_data[,2])
colnames(raw_data)<- "text"


#Labelling of tweets and data generation
t<- as.data.frame(raw_data[grepl("help|relief|hurricane|houston|houstonflood",raw_data$text),])
colnames(t)<- "text"
t$class<- "rescue"

p1<- as.data.frame(raw_data[-which(t$text %in% raw_data$text),])
colnames(p1)<- "text"
p1$class<- "nonrescue"

whole_data<- rbind(t,p1)
whole_data<- rbind(p1[1:75,],t[1:75,])

whole_data <- whole_data[sample(nrow(whole_data)),]
write.csv(whole_data,file="final_data.csv")

#Read the raw data
tweet_raw<- read.csv("final_data.csv")
tweet_raw<- as.data.frame(tweet_raw[,2:3])
tweet_raw$class <- factor(tweet_raw$class)

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
myStopwords <- c(stopwords('english'), "rt")
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
preprocess[,"class"]<- tweet_raw$class
preprocess<- preprocess[,2:3]

tweet_corpus <- corpus(preprocess$text)

preprocess_train<- preprocess[1:120,]
preprocess_test<- preprocess[121:150,]

#Divide in train and test set
tweet_corpus_train<- tweet_corpus[1:120,]
tweet_corpus_test<- tweet_corpus[121:150,]

dfm_whole<- dfm(tweet_corpus,remove = stopwords("english"), remove_punct = TRUE)
dfm_train<- dfm(tweet_corpus_train)
dfm_test<- dfm(tweet_corpus_test)
(final_test <- dfm_select(dfm_test, dfm_train))

p1<- read.csv("p1.csv")
p2<- read.csv("p2.csv")

#KNN with euclidean distance
train<- dfm_train
test<- final_test

set.seed(100)
r<-gknn(as.matrix(train), p1$class, as.matrix(test), nn = 10, distance = "euclidean")
cm2<-confusionMatrix(
  r,
  p2$class
)

sprintf("Confusion matrix for euclidean distance is:")
cm2

#KNN with minkowski distance
r1<-gknn(as.matrix(train), p1$class, as.matrix(test), nn = 3, distance = "minkowski")
cm3<-confusionMatrix(
  r1,
  p2$class
)
sprintf("Confusion matrix for minkowski distance is:")
cm3

#KNN with manhattan distance
r2<-gknn(as.matrix(train), p1$class, as.matrix(test), nn = 3, distance = "manhattan")
cm4<-confusionMatrix(
  r2,
  p2$class
)
sprintf("Confusion matrix for manhattan distance is:")
cm4

#KNN with different values of k
set.seed(100)
knn.1<-gknn(as.matrix(train), p1$class, as.matrix(test), nn = 1, distance = "euclidean")
knn.3<-gknn(as.matrix(train), p1$class, as.matrix(test), nn = 3, distance = "euclidean")
knn.5<-gknn(as.matrix(train), p1$class, as.matrix(test), nn = 5, distance = "euclidean")
knn.8<-gknn(as.matrix(train), p1$class, as.matrix(test), nn = 8, distance = "euclidean")
knn.10<-gknn(as.matrix(train), p1$class, as.matrix(test), nn = 10, distance = "euclidean")

cm_1<-confusionMatrix(
  knn.1,
  p2$class
)

cm_3<-confusionMatrix(
  knn.3,
  p2$class
)

cm_5<-confusionMatrix(
  knn.5,
  p2$class
)

cm_8<-confusionMatrix(
  knn.8,
  p2$class
)

cm_10<-confusionMatrix(
  knn.10,
  p2$class
)

sprintf("Confusion matrix for knn 1 distance is:")
cm_1
sprintf("Confusion matrix for knn 3 distance is:")
cm_3
sprintf("Confusion matrix for knn 5 distance is:")
cm_5
sprintf("Confusion matrix for knn 8 distance is:")
cm_8
sprintf("Confusion matrix for knn 10 distance is:")
cm_10

#Plot the classification results for various k
y<- p2$class
p<- prediction(knn.1, y)
p3<- prediction(knn.3, y)
p5<- prediction(knn.5, y)
p8<- prediction(knn.8, y)
p10<- prediction(knn.10, y)


perf <- performance(p, measure='tpr', x.measure='fpr')
perf3 <- performance(p3, measure='tpr', x.measure='fpr')
perf5 <- performance(p5, measure='tpr', x.measure='fpr')
perf8 <- performance(p8, measure='tpr', x.measure='fpr')
perf10 <- performance(p10, measure='tpr', x.measure='fpr')

plot(perf,main = "ROC curve for KNN Classifier",col="red")
plot(perf3,add=TRUE,col = "blue")
plot(perf5,add=TRUE,col = "violet")
plot(perf8,add=TRUE,col = "green")
plot(perf10,add=TRUE,col = "orange")

