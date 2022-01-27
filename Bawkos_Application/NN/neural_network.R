##### Packages #####
install.packages("dplyr")
install.packages("plyr")
#install.packages("neuralnet")
install.packages("caret")
library(plyr)
library(dplyr)
library(neuralnet)
library(caret)
MODELTYPE= "NN"
#neural_network <- function() {
##### Set Seed #####
# Seed is set so that the results can be reproduced for marking
set.seed(1773894103)

##### Load Files #####
source_dir <- paste(getwd(),MODELTYPE,"class_feature.csv",sep = "/") # path location of files

# read in CSV data
data <- read.csv(file=source_dir)
unseen_cut = length(data$FTR)-379
unseen_data <- data[unseen_cut:length(data$FTR),]
data <- data[1:unseen_cut,]



##### Neural Network Model Fitting #####
#cv.error <- NULL
#k <- 10
#maxs <- apply(data, 2, max) 
#mins <- apply(data, 2, min)
#scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

#pbar <- create_progress_bar('text')
#pbar$init(k)
#for(i in 1:k){
#  index <- sample(1:nrow(data),round(0.9*nrow(data)))
#  train.cv <- scaled[index,]
#  test.cv <- scaled[-index,]
#  nn <- neuralnet(FTR~.,data=train.cv,hidden=1,stepmax=1e7,act.fct = "logistic",linear.output=TRUE)   
#  pr.nn <- compute(nn,test.cv[,1:3])
#  pr.nn <- pr.nn$net.result*(max(data$FTR)-min(data$FTR))+min(data$FTR)   
#  test.cv.r <- (test.cv$FTR)*(max(data$FTR)-min(data$FTR))+min(data$FTR)   
#  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
#  pbar$step()
#}

#### Neural Network Model no fitting ####
nn = neuralnet(FTR~.,
               data=data,
               hidden=1,
               stepmax=1e7,
               act.fct = "logistic",
               linear.output = TRUE
               )

##### Measuring Model #####
plot(nn)
Predict=neuralnet::compute(nn,unseen_data)
overall <- data.frame(Predict$net.result, unseen_data$FTR)
overall$Predict.net.result <- overall$Predict.net.result*3
overall$True_Value <- ifelse(round(overall$Predict.net.result, digits=0) >= 2, 3,
                             ifelse(round(overall$Predict.net.result, digits=0) < 2 &&
                                      round(overall$Predict.net.result, digits=0) > 0.5, 1,
                                    ifelse(round(overall$Predict.net.result, digits=0) < 0.5, 0, FALSE)))
overall$Winner <- ifelse(overall$True_Value == unseen_data$FTR, TRUE, FALSE)

print((sum(overall$Winner == TRUE)/length(unseen_data$FTR))*100)

matrix <- confusionMatrix(factor(overall$True_Value, levels=c("0", "1", "3")),
                          as.factor(unseen_data$FTR))
rmse = RMSE(overall$True_Value, unseen_data$FTR)
mae = MAE(overall$True_Value, unseen_data$FTR)
print(rmse)
print(mae)
print(matrix)

r_squared = R2(overall$True_Value, unseen_data$FTR)
mse = mean((unseen_data$FTR - overall$True_Value)**2)
print(r_squared)
print(mse)
#}