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
library(arules)
library(arulesViz)


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

train<- dfm_train
test<- final_test

x<- train
y<- as.factor(preprocess_train$class)



#classification using svm
svm_.01 <- svm(x,as.factor(y),kernel="linear",cost=0.01,decision.values=TRUE)
pred_.01<- predict(svm_.01,test,type="prob")
cm2_.01<-confusionMatrix(
  pred_.01,
  preprocess_test$class
)


svm_.1 <- svm(x,as.factor(y),kernel="linear",cost=0.1)
pred_.1<- predict(svm_.1,test)
cm2_.1<-confusionMatrix(
  pred_.1,
  preprocess_test$class
)



svm_.2 <- svm(x,as.factor(y),kernel="linear",cost=0.2)
pred_.2<- predict(svm_.2,test)
cm2_.2<-confusionMatrix(
  pred_.2,
  preprocess_test$class
)


svm_.3 <- svm(x,as.factor(y),kernel="linear",cost=0.3)
pred_.3<- predict(svm_.3,test)
cm2_.3<-confusionMatrix(
  pred_.3,
  preprocess_test$class
)


svm_.4 <- svm(x,as.factor(y),kernel="linear",cost=0.4)
pred_.4<- predict(svm_.4,test)
cm2_.4<-confusionMatrix(
  pred_.4,
  preprocess_test$class
)


svm_.5 <- svm(x,as.factor(y),kernel="linear",cost=0.5)
pred_.5<- predict(svm_.5,test)
cm2_.5<-confusionMatrix(
  pred_.5,
  preprocess_test$class
)



sprintf("Confusion matrix for cost .01  is:")
cm2_.01
sprintf("Confusion matrix for cost .1  is:")
cm2_.1
sprintf("Confusion matrix for cost .2  is:")
cm2_.2
sprintf("Confusion matrix for cost .3  is:")
cm2_.3
sprintf("Confusion matrix for cost .4  is:")
cm2_.4
sprintf("Confusion matrix for cost .5  is:")
cm2_.5

#Plot the classification results for various cost
y<- preprocess_test$class
p_.01<- prediction(as.numeric(pred_.01),y)
p_.1<- prediction(as.numeric(pred_.1), y)
p_.2<- prediction(as.numeric(pred_.2), y)
p_.3<- prediction(as.numeric(pred_.3), y)
p_.4<- prediction(as.numeric(pred_.4), y)
p_.5<- prediction(as.numeric(pred_.5), y)

perf_.01 <- performance(p_.01, measure='tpr', x.measure='fpr')
perf_.1 <- performance(p_.1, measure='tpr', x.measure='fpr')
perf_.2 <- performance(p_.2, measure='tpr', x.measure='fpr')
perf_.3 <- performance(p_.3, measure='tpr', x.measure='fpr')
perf_.4 <- performance(p_.4, measure='tpr', x.measure='fpr')
perf_.5 <- performance(p_.5, measure='tpr', x.measure='fpr')

plot(perf_.01,main = "ROC curve for SVM Classifier",col="red")
plot(perf_.1,add=TRUE,col = "blue")
plot(perf_.2,add=TRUE,col = "yellow")
plot(perf_.3,add=TRUE,col = "green")
plot(perf_.4,add=TRUE,col = "orange")
plot(perf_.5,add=TRUE,col = "black")



