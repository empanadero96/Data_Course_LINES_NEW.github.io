library(tidyverse)
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)

rm(olympic100)

olympic100 <- read.csv("Olympic100mNewData.csv")

olympic100 <- t(olympic100) %>% as.data.frame()
colnames(olympic100) <- olympic100[1,]
olympic100$athlete <- rownames(olympic100)

olympic100 <- olympic100[2:20,c(16, 1:15)]


# Converting columns to numeric
olympic100[2:16] <- lapply(olympic100[2:16], FUN = as.numeric)



# Finds rows to use for training dataset
# createDataPartition(y=data$expensive, p=.7)
trainIndex <- sample(1:7, size = 7, replace = FALSE)
trainSet <- olympic100[unlist(trainIndex),]
testSet <- olympic100[-unlist(trainIndex),]

## Make a Support Vector Machine
SVM <- svm(TOTAL ~ `10-20m` + `20-30m` + `30-40m` + `40-50m`,
           data = trainSet)
predsSVM <- predict(object = SVM, newdata = trainSet)
SVM_MSE <- mean((predsSVM - trainSet$TOTAL)**2)

summary(SVM_MSE)

SVM

## Make a regression tree
DT <- rpart(control=rpart.control(minsplit=1, minbucket=1, cp=0.001),
            TOTAL ~ . -athlete, 
            data = olympic100, 
            method="anova")

rpart.plot(
           digits = 4,
           box.col = 0,
           DT)


predsDT <- predict(object = DT, newdata = trainSet)
DT_MSE <- mean((predsDT - trainSet$TOTAL)**2)

DT_MSE

?rpart.plot


