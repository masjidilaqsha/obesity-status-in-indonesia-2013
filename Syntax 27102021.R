#Load Library
library(memisc)
library(DMwR2)
library(caret)
library(pROC)
library(ROCR)
library(dplyr)
library(e1071)
library(haven)
library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ipred)
library(adabag)
library(xgboost)
library(gbm)

#Load Data
Train_DataX <- read.csv("~/zFile/Health Data Science/Train Stunting.csv")
Test_Data  <- read.csv("~/zFile/Health Data Science/Test Stunting.csv")
SMOTE150   <- read.csv("~/zFile/Health Data Science/SMOTE150_.csv")
SMOTE200   <- read.csv("~/zFile/Health Data Science/SMOTE200_.csv")

Train_SMOTE150 <- rbind(filter(Train_DataX, stunting %in% "Normal"),SMOTE150)
Train_SMOTE200 <- rbind(filter(Train_DataX, stunting %in% "Normal"),SMOTE200)

DataAll <- rbind(Train_DataX,Test_Data)
for (i in 1:37) {
  DataAll[,i] <- factor(DataAll[,i])
}
Train_Data <- DataAll[1:dim(Train_DataX)[1],]
Test_Data  <- DataAll[(dim(Train_DataX)[1]+1):(dim(Train_DataX)[1]+dim(Test_Data)[1]),]


DataAll <- rbind(Train_SMOTE150,Test_Data)
for (i in 1:37) {
  DataAll[,i] <- factor(DataAll[,i])
}
Train_Data <- DataAll[1:dim(Train_SMOTE150)[1],]
Test_Data  <- DataAll[(dim(Train_SMOTE150)[1]+1):(dim(Train_SMOTE150)[1]+dim(Test_Data)[1]),]


DataAll <- rbind(Train_SMOTE200,Test_Data)
for (i in 1:37) {
  DataAll[,i] <- factor(DataAll[,i])
}
Train_Data <- DataAll[1:dim(Train_SMOTE200)[1],]
Test_Data  <- DataAll[(dim(Train_SMOTE200)[1]+1):(dim(Train_SMOTE200)[1]+dim(Test_Data)[1]),]


#-Single Model
#--Classification and Regression Tree (CART) (Clear)
#---Parameter Tuning
hyper_grid_cart <- expand.grid(
  cpx = c(.0001,.0005,.001,.005,.01,.05,0.1,0.5),
  trainAcc = 0,
  testAcc = 0
)

METRIC_CART <- list(data.frame(),data.frame())
CONFUS_CART <- data.frame()

for (i in 1:nrow(hyper_grid_cart)) {
  crt <- rpart(stunting ~., data = Train_Data, method = "class", 
               control = rpart.control(cp = hyper_grid_cart$cpx[i]))
  #Training
  pred_crt <- predict(crt, newdata = Train_Data[,-1], type = "class")
  cfm_crt <- confusionMatrix(pred_crt, Train_Data[,1])
  hyper_grid_cart$trainAcc[i] <- cfm_crt$overall[1]
  METRIC_CART[[1]] <- rbind(METRIC_CART[[1]],cbind(t(cfm_crt$overall),t(cfm_crt$byClass)))
  
  temp <- cfm_crt$table
  
  #Validation
  pred_crt <- predict(crt, newdata = Test_Data[,-1], type = "class")
  cfm_crt <- confusionMatrix(pred_crt, Test_Data[,1])
  hyper_grid_cart$testAcc[i] <- cfm_crt$overall[1]
  METRIC_CART[[2]] <- rbind(METRIC_CART[[2]],cbind(t(cfm_crt$overall),t(cfm_crt$byClass)))
  
  temp <- cbind(temp,cfm_crt$table)
  CONFUS_CART <- rbind(CONFUS_CART,temp)
  
  print(hyper_grid_cart[i,])
}



#--Random Forest (Clear)
#---Parameter Tuning
hyper_grid_rfo <- expand.grid(
  m_try = c(floor(.5*sqrt(dim(Train_Data)[2])),floor(sqrt(dim(Train_Data)[2])),
            floor(2*sqrt(dim(Train_Data)[2]))),
  n_tree = c(25,51,101,201,501),
  trainAcc = 0,
  testAcc = 0,
  OOB = 0
)

METRIC_RFO <- list(data.frame(),data.frame())
CONFUS_RFO <- data.frame()

for (i in  1:nrow(hyper_grid_rfo)) {
  rfo <- randomForest::randomForest(stunting ~., data = Train_Data, 
                                    ntree = hyper_grid_rfo$n_tree[i], 
                                    mtry  = hyper_grid_rfo$m_try[i],
                                    replace = T)
  
  #Training
  pred_rfo <- predict(rfo, newdata = Train_Data[,-1], type = "class")
  cfm_rfo <- confusionMatrix(pred_rfo, Train_Data[,1])
  hyper_grid_rfo$trainAcc[i] <- cfm_rfo$overall[1]
  METRIC_RFO[[1]] <- rbind(METRIC_RFO[[1]],cbind(t(cfm_rfo$overall),t(cfm_rfo$byClass)))
  
  temp <- cfm_rfo$table
  
  #Validation
  pred_rfo <- predict(rfo, newdata = Test_Data[,-1], type = "class")
  cfm_rfo <- confusionMatrix(pred_rfo, Test_Data[,1])
  hyper_grid_rfo$testAcc[i] <- cfm_rfo$overall[1]
  METRIC_RFO[[2]] <- rbind(METRIC_RFO[[2]],cbind(t(cfm_rfo$overall),t(cfm_rfo$byClass)))
  
  temp <- cbind(temp,cfm_rfo$table)
  CONFUS_RFO <- rbind(CONFUS_RFO,temp)
  
  hyper_grid_rfo$OOB[i] <- mean(rfo$err.rate[,1])
  print(hyper_grid_rfo[i,])
}




















