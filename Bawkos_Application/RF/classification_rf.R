class_rf<-function(){
  
  # ************************************************
  # NPREPROCESSING_splitdataset() :
  #
  # Randomise and split entire data set
  #
  # INPUT: data Frame - class_dataset - dataset
  #
  # OUTPUT : data Frame - testing_data - dataset
  #          data Frame - training_data - dataset
  #          List - retList
  # ************************************************
  
  NPREPROCESSING_splitdataset<-function(class_dataset){
    
    # **** Create a Training dataset using 80% of the records
    # and use the other 20% to act as unseen testing data ***
    
    class_dataset<-class_dataset[order(runif(nrow(class_dataset))),]
    training_records<-round(nrow(class_dataset)*(80/100))
    
    train <- 1:training_records
    test <- -train
    
    training_data <- class_dataset[train,]
    testing_data = class_dataset[test,]
    
    retList<-list("train"=training_data,
                  "test"=testing_data)
    return(retList)
  }
  
  # ****************************************************
  # classRandomForest() :
  #
  # Train and test a Random Forest classification model
  #
  # INPUT: data Frame - train - training dataset
  #        data Frame - test - testing dataset
  #
  # OUTPUT : data Frame - p1 - prediction of unseen data
  # ****************************************************
  
  classRandomForest<-function(train, test, plot=TRUE){
    
    set.seed(15)
    train_ctrl <- trainControl(method = "cv", number = 5, savePredictions=TRUE)
    rf_class<-randomForest::randomForest(FTR~., data=train, mtry=2, ntree=501)
    print(rf_class)
    
    p1<-predict(rf_class, test)
    
    plot(p1)
    tab1<-table(Predicted = p1, Actual = test$FTR)
    
    print(tab1)
  
    return(p1)
  }
  
  library(randomForest)
  library(stats)
  library(party)
  library(dplyr)
  library(ggraph)
  library(igraph)
  library(tidyverse) 
  library(caret)
  library(plyr)
  library(reshape2)
  library(ModelMetrics)
  library(recipes)
  library(foreach)
  library(pROC)
  library(e1071)
  
  MODELTYPE= "RF"
  class_data<-paste(getwd(),MODELTYPE,"class_feature.csv",sep = "/")
  
  class_dataset<-read.csv(file = class_data)
  
  class_dataset$FTR<-as.factor(class_dataset$FTR)
  
  class_matches<-NPREPROCESSING_splitdataset(class_dataset)
  
  class_dataset_train<-class_matches[[1]]
  class_dataset_test<-class_matches[[2]]
  
  class_result<-classRandomForest(train=class_dataset_train, test = class_dataset_test)
  print("class dataset completed")
  print("--------------------------------------------")
  
}

class_rf()
