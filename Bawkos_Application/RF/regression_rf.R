regression_rf<-function(){
 
  # ************************************************
  # awayRandomForest() :
  #
  # Train and test a Random Forest regression model 
  # for the away team dataset
  #
  # INPUT: data Frame - train - training dataset
  #
  # OUTPUT :  data Frame - xG_Away
  # ************************************************
  
  awayRandomForest<-function(train,plot=TRUE){
    myTitle<-"Preprocessed Dataset. Away Random Forest"
    
    set.seed(15)
    train_ctrl <- trainControl(method = "cv", number = 5, savePredictions=TRUE)
    rf_regression<- train(FTAG~., data = train, method = "rf", trControl=train_ctrl)
    
    print("Random Forest Model")
    print(rf_regression)
    
    p1<-predict(rf_regression, train)
    print("Away Training dataset prediction")
    print(head(p1))
    print("Actual away training data info")
    print(head(train$FTAG))
    
    xG_Away<-p1
    plot(rf_regression)
    
    #tuned<-tuneRF(train[,-8], train[,8], stepFactor = 1, plot = TRUE, ntreeTry = 501, trace = TRUE, improve = 0.05)
    
    print("--------------------------------------------")
    
    return(xG_Away)
  }
  
  # ************************************************
  # homeRandomForest() :
  #
  # Train and test a Random Forest regression model 
  # for the home team dataset
  #
  # INPUT: data Frame - train - training dataset
  #
  # OUTPUT :  data Frame - xG_Home
  # ************************************************
  
  homeRandomForest<-function(train,plot=TRUE){
    myTitle<-"Preprocessed Dataset. Home Random Forest"
    
    set.seed(15)
    train_ctrl <- trainControl(method = "cv", number = 5, savePredictions=TRUE)
    rf_regression<- train(FTHG~., data = train, method = "rf", trControl=train_ctrl)
    
    print("Random Forest Model")
    print(rf_regression)
    
    p1<-predict(rf_regression, train)
    print("Home Training dataset prediction")
    print(head(p1))
    print("Actual home training data info")
    print(head(train$FTHG))
    
    xG_Home<-p1
    plot(rf_regression)
    
    #tuned<-tuneRF(train[,-8], train[,8], stepFactor = 1, plot = TRUE, ntreeTry = 501, trace = TRUE, improve = 0.05)
    
    print("--------------------------------------------")
    
    return(xG_Home)
    
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
  
  
  home_data<-paste(getwd(),"xGNS_H_features.csv",sep = "/")
  home_data_ovr<-paste(getwd(),"xGNS_ovr_H_features.csv",sep = "/")
  away_data<-paste(getwd(),"xGNS_A_features.csv",sep = "/")
  away_data_ovr<-paste(getwd(),"xGNS_ovr_A_features.csv",sep = "/")
  
  home_dataset<-read.csv(file = home_data)
  home_dataset_ovr<-read.csv(file = home_data_ovr)
  away_dataset<-read.csv(file = away_data)
  away_dataset_ovr<-read.csv(file = away_data_ovr)
  
  home_xG_ns<-homeRandomForest(train=home_dataset)
  print("home_xG_ns completed")
  print("--------------------------------------------")
  
  home_xG_ns_ovr<-homeRandomForest(train= home_dataset_ovr)
  print("home_xG_ns_ovr completed")
  print("--------------------------------------------")
  
  away_xG_ns<-awayRandomForest(train=away_dataset)
  print("away_xG_ns completed")
  print("--------------------------------------------")
  
  away_xG_ns_ovr<-awayRandomForest(train=away_dataset_ovr)
  print("away_xG_ns_ovr completed")
  print("--------------------------------------------")
  
  xG_ns_list<-list(home_xG_ns, home_xG_ns_ovr, away_xG_ns, away_xG_ns_ovr)
  
  return(xG_ns_list)
  
}
