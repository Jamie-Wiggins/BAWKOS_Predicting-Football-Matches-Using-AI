# # KNN Regression

# Packages
library(tidyverse)
library(tidymodels)
library(gridExtra)
library(class)
library(gmodels)
library(caret)

#
# knn_reg()
#
# takes a dataset and parameter for whether the team is at home or away
# the dataset is trained and tested on itself in its totality
# this is done to get the expected goals for the dataset.  
#
#
# Input:
# dataset - the dataset of the features set 
# hometeam - whether the team is home or away
#
#
# Output:
#
# A new dataframe called output which holds the expected goals values
# for the whole dataset
#

knn_reg <- function(dataset, hometeam) {
  set.seed(length(dataset))

  if (hometeam == TRUE)
  { indexes = createDataPartition(dataset$FTHG, p = 1.00, list = F)
    train = dataset[indexes,]

    train_x = train[, -1] #train the data excludes the column
    train_y = train[, 1] #train the data for the FTHG 
    tsamples_count <- NROW(train_x)
    kroot_training <- sqrt(tsamples_count) #square root number of observations 

    knnmodel = knnreg(train_x, train_y, k = kroot_training) #runs the knnreg method to train and test 
    pred_y = predict(knnmodel, data.frame(train_x)) # a list of the expected goals 

    output <- data.frame(matrix(unlist(pred_y), nrow = length(pred_y), byrow = TRUE)) #output is a dataframe of the list of the expected goals 
    output$actual <- train_y } # the train y is the data of the actual goals scored and is added to the column actual in the output column 
  
  else
  
  # same methodology applied for away data 
  { indexes = createDataPartition(dataset$FTAG, p = 1.00, list = F)
    train = xGNS_ovr_H_features[indexes,]

    train_x = train[, -1]
    train_y = train[, 1]
    train_samples_count <- NROW(train_x)
    kroot_training_samples <- sqrt(train_samples_count)

    knnmodel = knnreg(train_x, train_y, k = kroot_training_samples)
    pred_y = predict(knnmodel, data.frame(train_x))

    output <- data.frame(matrix(unlist(pred_y), nrow = length(pred_y), byrow = TRUE)) }
  return(output)
}
