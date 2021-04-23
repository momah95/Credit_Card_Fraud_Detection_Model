#load packages
library(ranger)
library(caret)
library(data.table)
library(readr)
library(dplyr)
library(randomForest)
library(ggplot2)
library(party)
library(MLmetrics)
library(jsonlite)
library(e1071)
library(caTools)


# Loading dataset
creditcard_data <- read.csv("creditcard.csv")

creditcard_VarImp <- creditcard_data[,c(5,11,12,13,15,31)]

my_data <- creditcard_VarImp

# Data preparation and Preprocessing of the train dataset

#make class a factor
my_data$Class <- as.factor(my_data$Class)

#modeling
set.seed(1234)

#split the dataset
data_sample<-sample.split(my_data, SplitRatio =0.80)
train_data<-subset(my_data,data_sample==TRUE)
test_data<-subset(my_data,data_sample==FALSE)

#cross validation
control <- trainControl(method="repeatedcv", number=5, repeats=3)

#fitting svm model 
svm<- train(form=Class~., data=train_data, method="svmLinear", preProcess = c("center", "scale"), trControl=control, tuneLength =5)

# Making Prediction
predsvm<-predict(svm,test_data,type="raw")
confusionMatrix(test_data$Class, predsvm)
#f1_score
f1_svm<-F1_Score(test_data$Class, predsvm)
f1_svm

# Save Output Object
modelOutput_list <- vector(mode = "list")

## Save model output
modelOutput_list$svm <- svm

# Serializing and saving 'modelOutput_list' for later/future retrieval
saveRDS(object =modelOutput_list, file ="modelOutputList.RDS")

