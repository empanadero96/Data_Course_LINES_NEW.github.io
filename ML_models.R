library(tidyverse)
library(e1071)
library(caret)
library(rpart)


olympic100 <- read.csv("Olympic100mNewData.csv")

olympic100 <- t(olympic100) %>% as.data.frame()
colnames(olympic100) <- olympic100[1,]
olympic100$athlete <- rownames(olympic100)

olympic100 <- olympic100[2:8,c(16, 1:15)]


# Converting columns to numeric
olympic100[2:16] <- lapply(olympic100[2:16], FUN = as.numeric)



# Finds rows to use for training dataset
# createDataPartition(y=data$expensive, p=.7)
trainIndex <- sample(1:7, size = 4, replace = FALSE)
trainSet <- olympic100[unlist(trainIndex),]
testSet <- olympic100[-unlist(trainIndex),]

## Make a Support Vector Machine
SVM <- svm(TOTAL ~ `height(cm)` + `10-20m` + `20-30m` + `30-40m` + `40-50m` + `10-20m`*`20-30m`*`30-40m`*`40-50m`, data = trainSet)
predsSVM <- predict(object = SVM, newdata = trainSet)
SVM_MSE <- mean((predsSVM - trainSet$TOTAL)**2)

## Make a regression tree
DT <- rpart(TOTAL ~ `height(cm)` + `10-20m` + `20-30m` + `30-40m` + `40-50m`, 
            data = trainSet, 
            method="anova")

predsDT <- predict(object = DT, newdata = trainSet)
DT_MSE <- mean((predsDT - trainSet$TOTAL)**2)


