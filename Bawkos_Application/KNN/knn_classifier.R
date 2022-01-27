#####Packages#######
library(class)
library(gmodels)
library(tidyverse) 
library(caret)
library(tidymodels)
library(gridExtra)


#assigning class features data frame to data set 
class_data<-paste(getwd(),MODELTYPE,"class_feature.csv",sep = "/")

dataset<-read.csv(file = class_data)

#Creating function for normalising data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) }


#normalising data in the data set
normalised_dataset <- as.data.frame(lapply(dataset[,2:3],normalise))

#setting seeding for sample
set.seed(1301121211)

#randomly splitting data into test and training data with 80:20 split
sample <- sample(1:nrow(normalised_dataset),size=nrow(normalised_dataset)*0.8,replace = FALSE) #random selection of training data 80% 
trained_data <- normalised_dataset[sample,]
test_data <- normalised_dataset[-sample,]

#assigning data labels
train_labels <- dataset[sample,1]
test_labels <-dataset[-sample,1]

#getting predicted values for when k = sqrt of the training dataset
pred_values <- knn(train=trained_data, test=test_data, cl=train_labels, k=sqrt(NROW(trained_data)))

#confusion matrix
confusionMatrix(table(pred_values ,test_labels))

# plotting the accuracy of different k values  showing the sqrt of each for a clearer image
i=1                         
k_plot=1 
limit = sqrt(NROW(trained_data))
for (i in 1:limit){ 
  knn_output <-  knn(train=trained_data, test=test_data, cl=train_labels, k=i)
  k_plot[i] <- 100 * sum(test_labels == knn_output)/NROW(test_labels)
  
}
plot(k_plot, type="b", xlab="K-Value",ylab="Accuracy")


#evaluation methods
label_df = data.frame(as.numeric(test_labels))
pred_df = data.frame(as.numeric(pred_values))

dfsquared =data.frame((pred_df-label_df)^2)

j = 1
totalmse = 0
totalmae = 0
tss = 0
for (j in 1:NROW(dfsquared)){
  totalmse = totalmse + dfsquared[j,] 
  totalmae = totalmae +abs((pred_df[j,]-label_df[j,]))
  tss = tss + (pred_df[j,]-label_df[j,])^2
  
}


mse = totalmse/NROW(dfsquared)
mae = totalmae/NROW(dfsquared)
rsquared = R2(pred_df,label_df)
cat("MSE: ", mse, "MAE: ", mae, " Rsquared: ", rsquared)

