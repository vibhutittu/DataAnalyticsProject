## setting working directory
path <- "E:/Project/Other code"
setwd(path)


## loading libraries
library(bit64)
library(data.table)
library(caret)
library(doMC)
library(ROCR)
library(pROC)
library(InformationValue)
numCores <- detectCores()
registerDoMC(cores = numCores)


## loading data
sessions <- fread("./sessions.csv")
X_train <- fread("./train_users_2.csv")

#Remove records with NDF as country destination
X_train <- X_train[which(!X_train$country_destination == "NDF"),]

#Divide in training and test set
set.seed(107)
inTrain <- createDataPartition(y = X_train$country_destination, p = .7, list = FALSE)
X_train <- X_train[ inTrain,]
X_test <- X_train[-inTrain,]


## cleaning data

# keeping users present in sessions data
X_train <- subset(X_train, id %in% unique(sessions$user_id), select=c("id","country_destination"))
X_test <- subset(X_test, id %in% unique(sessions$user_id), select=c("id","country_destination"))

# features from sessions data
names(sessions)[1] <- "id"
sessions <- subset(sessions, id %in% c(unique(X_train$id), unique(X_test$id)))
sessions$count <- 1

# one-hot encoding action, action_type and action_detail
sessions_action <- dcast(sessions, id ~ action, mean, value.var="count")
sessions_action_type <- dcast(sessions, id ~ action_type, mean, value.var="count")
sessions_action_detail <- dcast(sessions, id ~ action_detail, mean, value.var="count")

# merging with train and test data
X_train <- merge(X_train, sessions_action, all.x=T, by="id")
X_test <- merge(X_test, sessions_action, all.x=T, by="id")

X_train <- merge(X_train, sessions_action_type, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_type, all.x=T, by="id")

X_train <- merge(X_train, sessions_action_detail, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_detail, all.x=T, by="id")

X_train[is.na(X_train)] <- 0
X_test[is.na(X_test)] <- 0

# removing duplicate columns
X_train <- X_train[, colnames(unique(as.matrix(X_train), MARGIN=2)), with=F]
X_test <- X_test[, names(X_train), with=F]

# extracting ids and target
train_ids <- X_train$id
test_ids <- X_test$id

target <- ifelse(X_train$country_destination == "US", 1, 0)
target_test <- ifelse(X_test$country_destination == "US", 1, 0)

# preparing final datasets
X_train <- X_train[, ":="(id = NULL, country_destination = NULL)]
X_test <- X_test[, ":="(id = NULL, country_destination = NULL)]

# removing variables with less than 4 occurrences
X_train <- as.data.frame(subset(X_train, select=c(names(X_train)[which(colSums(X_train) > 4)])))
X_test <- as.data.frame(X_test[, .SD, .SDcols=names(X_train)])

#Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)


##Model with caret

#Training the logistic regression model
model_lr<-train(X_train,target,method='glm',trControl=fitControl,tuneLength=3)
#Predicting using logistic regression model
X_test$pred_lr<-predict(object = model_lr,X_test)
X_train$pred_lr<-predict(object = model_lr,X_train)

##Important features
Imp_vars_glm <- varImp(model_lr)
plot(varImp(model_lr, useModel = TRUE), top = 20)

##auc
pr <- prediction(X_test$pred_lr, target_test)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,main = "ROC curve for logistic regression and gbm Classifier",col="red")
roc_lr <- roc(target_test, X_test$pred_lr)
auc(roc_lr)

#Training the gbm model
model_gbm<-train(X_train,target,method='gbm',trControl=fitControl,tuneLength=3)
#Predicting using gbm model
X_test$pred_gbm<-predict(object = model_gbm,X_test)
X_train$pred_gbm<-predict(object = model_gbm,X_train)


##auc
pr_gbm <- prediction(X_test$pred_gbm, target_test)
prf_gbm <- performance(pr_gbm, measure = "tpr", x.measure = "fpr")
plot(prf_gbm,add=TRUE,col = "blue")
roc_gbm <- roc(target_test, X_test$pred_gbm)
auc(roc_gbm)


##Ensembling
#####Averaging Technique
#Predicting the probabilities
X_test$pred_avg<-(X_test$pred_lr+X_test$pred_gbm)/2
##auc
pr_avg <- prediction(X_test$pred_avg, target_test)
prf_avg <- performance(pr_avg, measure = "tpr", x.measure = "fpr")
plot(prf_avg,col = "orange")
roc_avg <- roc(target_test, X_test$pred_avg)
auc(roc_avg)



# extracting train and test predictions
train_lr <- X_train$pred_lr
test_lr <- X_test$pred_lr

train_gbm <- X_train$pred_gbm
test_gbm <- X_test$pred_gbm

# saving predictions
train_lr <- data.frame("id"=train_ids, "pred_lr"=train_lr)
test_lr <- data.frame("id"=test_ids, "pred_lr"=test_lr)

train_gbm <- data.frame("id"=train_ids, "pred_gbm"=train_gbm)
test_gbm <- data.frame("id"=test_ids, "pred_gbm"=test_gbm)

write.csv(train_lr, "./train_lr.csv", row.names=F)
write.csv(test_lr, "./test_lr.csv", row.names=F)
write.csv(train_gbm, "./train_gbm.csv", row.names=F)
write.csv(test_gbm, "./test_gbm.csv", row.names=F)

