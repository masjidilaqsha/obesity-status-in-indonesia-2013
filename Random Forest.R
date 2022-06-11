#Load packages
library('memisc')
library('ROCR')
library('dplyr')
library('e1071')
library('randomForest')
library('haven')
library('ggplot2')
library('DMwR')
library('pROC')
library('caret')

#Load data
Test  <- read.csv('')
Train <- read.csv('')

#AUC graph function
auc_graphF <- function(TPR,FPR,AUC,name,code) {
  
  H <- data.frame(FPR = FPR, TPR = TPR)
  X <- ggplot() + 
    geom_line(data = H, aes(x = FPR, y = TPR)) + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), panel.grid.major = element_line(color  = "lightgray", 
                                                                                                                             size = 0.25), panel.background = element_rect(fill = "white", color = "black", size = 2)) + 
    labs(x = "False Positive Rate", y = "True Positive Rate", title = paste("Area Under the ROC Curve =",round(AUC*100,digits = 2),"%"), subtitle = paste("Random Forest with",name,"&",code))
  
  ggsave(plot = X, filename = paste("/content/gdrive/My Drive/Result/",name," ",code," AUC.jpg",sep = ""))
  
}

#Change response variable name to "Y"
names(Test)[12] <- "Y"
names(Train)[12] <- "Y"

#Model function
RandomForestX <- function(train, test, ntree, mtry){
  for (i in 1:dim(test)[2]) {
    train[,i] <- as.factor(train[,i])
    test[,i] <- as.factor(test[,i])
  }
  
  #Declaration metrics matrix
  METRIC <- matrix(0,length(ntree)*length(mtry),20)
  colnames(METRIC) <- c("Accuracy","Kappa","AccuracyLower","AccuracyUpper","AccuracyNull","AccuracyPValue","McnemarPValue",
                        "Sensitivity","Specificity","Pos Pred Value","Neg Pred Value","Precision","Recall","F1","Prevalence",
                        "Detection Rate","Detection Prevalence","Balanced Accuracy","AUC","OOB")
  CFM <- list()
  nameX <- paste(ntree,"Trees")
  mtryX <- paste(mtry,"Mtry")
  
  #Random Forest Process
  i <- 0
  k <- 0
  for (ntrees in ntree) { #Looping with different trees
    i <- i + 1
    j <- 0
    for (mtrys in mtry) { #Looping with different m value (selected random variable)
      j <- j + 1
      k <- k + 1
      #Random Forest function
      rf <- randomForest(Y ~., data = train, ntree = ntrees, mtry = mtrys, replace = T, samplesize = nrow(train))
      
      #Y-prediction (label)
      pred <- predict(rf, newdata = test[-dim(train)[2]])
      
      #Y-prediction probability
      pred_prob <- predict(rf, newdata = test[-dim(train)[2]], type = "prob")
      
      #Confusion Matrix
      cfmRF <- confusionMatrix(pred,test$Y)
      
      #Save the metric value
      METRIC[k,] <- c(cfmRF$overall,cfmRF$byClass,auc(roc(test$Y,pred_prob[,2])),mean(rf$err.rate[,1]))
      
      #ROC Value for ROC & AUC Graph
      X <- prediction(pred_prob[,2],test$Y)
      R <- performance(X,"tpr","fpr")
      
      auc_graphF(R@y.values[[1]],R@x.values[[1]],METRIC[k,19],nameX[i],mtryX[j])
      
      #Save Confusion Matrix
      CFM[[paste(nameX[i],mtryX[j])]] <- cfmRF$table
      
      #Erase the value for save memory
      rf <- 0
      cfmRF <- 0
      pred <- 0
      pred_prob <- 0
      X <- 0
      R <- 0
    }
  }
  
  #Save the result
  write.csv(METRIC, file = "", row.names = F)
  write.csv(CFM, file = "", row.names = F)
}

#Call model function
result <- RandomForestX(train = Train, test = Test, ntree = c(25,50,75,100,125,150), mtry = c(1,3,5,7,9))