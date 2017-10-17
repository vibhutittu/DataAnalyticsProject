setwd("E:/OneDrive - Texas Tech University/Data Analytic/Project/workSpace")

#Read Data
MyData <- read.csv(file="train_users_2.csv", header=TRUE, sep=",")
attach(MyData)

dim(MyData)
summary(MyData)

na_count <-sapply(MyData, function(y) sum(length(which(is.na(y)))))
na_count[na_count>0]

#Data preprocessing

#Remove "NDF" output
MyData <- MyData[!MyData$country_destination == "NDF",]

#Remove age = NA
MyData <- MyData[!is.na(MyData$age),]

#correct age data
MyData$age[MyData$age > 250] <- 2015 - MyData$age[MyData$age > 250]

#remove age < 15
MyData <- MyData[!MyData$age < 18,]

#Correct Date/time format
MyData$timestamp_first_active <- as.Date(as.character(MyData$timestamp_first_active), "%Y%m%d%H%M%S")
MyData$date_account_created <- as.Date(MyData$date_account_created, "%Y-%m-%d")
MyData$date_first_booking <- as.Date(MyData$date_first_booking, "%Y-%m-%d")


#Drop features
drops <- c("language","signup_flow")
MyData <- MyData[ , !(names(MyData) %in% drops)]

