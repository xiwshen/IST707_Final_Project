#IST707 Final Project

dev.off()
cat('\014')
rm(list=ls())

getwd()
setwd("C:/Users/xiwsh/Desktop")

library(readxl)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(pastecs)
library(caret)
library(factoextra)
library(rpart)
library(corrplot)
library(randomForest)
library(ROSE)
library(rattle)
library(pROC)

default_train <- read_csv("default_train.csv")
str(default_train)

default_valid <- read_csv("default_valid.csv")
str(default_valid)

default_train <- mutate_if(default_train, is.character, as.factor)
str(default_train)

default_valid <- mutate_if(default_valid, is.character, as.factor)
str(default_valid)

default_train$PAY_0 <- as.factor(default_train$PAY_0)
default_train$PAY_2 <- as.factor(default_train$PAY_2)
default_train$PAY_3 <- as.factor(default_train$PAY_3)
default_train$PAY_4 <- as.factor(default_train$PAY_4)
default_train$PAY_5 <- as.factor(default_train$PAY_5)
default_train$PAY_6 <- as.factor(default_train$PAY_6)

default_train$PAY_2 <- as.numeric(default_train$PAY_2)
default_train$PAY_3 <- as.numeric(default_train$PAY_3)
default_train$PAY_4 <- as.numeric(default_train$PAY_4)
default_train$PAY_5 <- as.numeric(default_train$PAY_5)
default_train$PAY_6 <- as.numeric(default_train$PAY_6)

#In order to better understand the tree diagram, we did convert column "PAY_0" to "PAY_6" from numbers to its actual 
#meaning in words.
default_train$PAY_0[default_train$PAY_0 == -2] <- "No consumption"
default_train$PAY_0[default_train$PAY_0 == -1] <- "Paid in full"
default_train$PAY_0[default_train$PAY_0 == 0] <- "Use revolving credit"
default_train$PAY_0[default_train$PAY_0 == 1] <- "Payment delay for one month"
default_train$PAY_0[default_train$PAY_0 == 2] <- "Payment delay for two month"
default_train$PAY_0[default_train$PAY_0 == 3] <- "Payment delay for three month"
default_train$PAY_0[default_train$PAY_0 == 4] <- "Payment delay for four month"
default_train$PAY_0[default_train$PAY_0 == 5] <- "Payment delay for five month"
default_train$PAY_0[default_train$PAY_0 == 6] <- "Payment delay for six month"
default_train$PAY_0[default_train$PAY_0 == 7] <- "Payment delay for seven month"
default_train$PAY_0[default_train$PAY_0 == 8] <- "Payment delay for eight month or above"

default_train$PAY_2[default_train$PAY_2 == -2] <- "No consumption"
default_train$PAY_2[default_train$PAY_2 == -1] <- "Paid in full"
default_train$PAY_2[default_train$PAY_2 == 0] <- "Use revolving credit"
default_train$PAY_2[default_train$PAY_2 == 1] <- "Payment delay for one month"
default_train$PAY_2[default_train$PAY_2 == 2] <- "Payment delay for two month"
default_train$PAY_2[default_train$PAY_2 == 3] <- "Payment delay for three month"
default_train$PAY_2[default_train$PAY_2 == 4] <- "Payment delay for four month"
default_train$PAY_2[default_train$PAY_2 == 5] <- "Payment delay for five month"
default_train$PAY_2[default_train$PAY_2 == 6] <- "Payment delay for six month"
default_train$PAY_2[default_train$PAY_2 == 7] <- "Payment delay for seven month"
default_train$PAY_2[default_train$PAY_2 == 8] <- "Payment delay for eight month or above"

default_train$PAY_3[default_train$PAY_3 == -2] <- "No consumption"
default_train$PAY_3[default_train$PAY_3 == -1] <- "Paid in full"
default_train$PAY_3[default_train$PAY_3 == 0] <- "Use revolving credit"
default_train$PAY_3[default_train$PAY_3 == 1] <- "Payment delay for one month"
default_train$PAY_3[default_train$PAY_3 == 2] <- "Payment delay for two month"
default_train$PAY_3[default_train$PAY_3 == 3] <- "Payment delay for three month"
default_train$PAY_3[default_train$PAY_3 == 4] <- "Payment delay for four month"
default_train$PAY_3[default_train$PAY_3 == 5] <- "Payment delay for five month"
default_train$PAY_3[default_train$PAY_3 == 6] <- "Payment delay for six month"
default_train$PAY_3[default_train$PAY_3 == 7] <- "Payment delay for seven month"
default_train$PAY_3[default_train$PAY_3 == 8] <- "Payment delay for eight month or above"

default_train$PAY_4[default_train$PAY_4 == -2] <- "No consumption"
default_train$PAY_4[default_train$PAY_4 == -1] <- "Paid in full"
default_train$PAY_4[default_train$PAY_4 == 0] <- "Use revolving credit"
default_train$PAY_4[default_train$PAY_4 == 1] <- "Payment delay for one month"
default_train$PAY_4[default_train$PAY_4 == 2] <- "Payment delay for two month"
default_train$PAY_4[default_train$PAY_4 == 3] <- "Payment delay for three month"
default_train$PAY_4[default_train$PAY_4 == 4] <- "Payment delay for four month"
default_train$PAY_4[default_train$PAY_4 == 5] <- "Payment delay for five month"
default_train$PAY_4[default_train$PAY_4 == 6] <- "Payment delay for six month"
default_train$PAY_4[default_train$PAY_4 == 7] <- "Payment delay for seven month"
default_train$PAY_4[default_train$PAY_4 == 8] <- "Payment delay for eight month or above"

default_train$PAY_5[default_train$PAY_5 == -2] <- "No consumption"
default_train$PAY_5[default_train$PAY_5 == -1] <- "Paid in full"
default_train$PAY_5[default_train$PAY_5 == 0] <- "Use revolving credit"
default_train$PAY_5[default_train$PAY_5 == 1] <- "Payment delay for one month"
default_train$PAY_5[default_train$PAY_5 == 2] <- "Payment delay for two month"
default_train$PAY_5[default_train$PAY_5 == 3] <- "Payment delay for three month"
default_train$PAY_5[default_train$PAY_5 == 4] <- "Payment delay for four month"
default_train$PAY_5[default_train$PAY_5 == 5] <- "Payment delay for five month"
default_train$PAY_5[default_train$PAY_5 == 6] <- "Payment delay for six month"
default_train$PAY_5[default_train$PAY_5 == 7] <- "Payment delay for seven month"
default_train$PAY_5[default_train$PAY_5 == 8] <- "Payment delay for eight month or above"

default_train$PAY_6[default_train$PAY_6 == -2] <- "No consumption"
default_train$PAY_6[default_train$PAY_6 == -1] <- "Paid in full"
default_train$PAY_6[default_train$PAY_6 == 0] <- "Use revolving credit"
default_train$PAY_6[default_train$PAY_6 == 1] <- "Payment delay for one month"
default_train$PAY_6[default_train$PAY_6 == 2] <- "Payment delay for two month"
default_train$PAY_6[default_train$PAY_6 == 3] <- "Payment delay for three month"
default_train$PAY_6[default_train$PAY_6 == 4] <- "Payment delay for four month"
default_train$PAY_6[default_train$PAY_6 == 5] <- "Payment delay for five month"
default_train$PAY_6[default_train$PAY_6 == 6] <- "Payment delay for six month"
default_train$PAY_6[default_train$PAY_6 == 7] <- "Payment delay for seven month"
default_train$PAY_6[default_train$PAY_6 == 8] <- "Payment delay for eight month or above"


default_train$default_payment_next_month <- as.factor(default_train$default_payment_next_month)
default_valid$default_payment_next_month <- as.factor(default_valid$default_payment_next_month)

write.csv(default_train,"C:/Users/xiwsh/Desktop\\dtree_train.csv", row.names = FALSE)


default_valid$PAY_0[default_valid$PAY_0 == -2] <- "No consumption"
default_valid$PAY_0[default_valid$PAY_0 == -1] <- "Paid in full"
default_valid$PAY_0[default_valid$PAY_0 == 0] <- "Use revolving credit"
default_valid$PAY_0[default_valid$PAY_0 == 1] <- "Payment delay for one month"
default_valid$PAY_0[default_valid$PAY_0 == 2] <- "Payment delay for two month"
default_valid$PAY_0[default_valid$PAY_0 == 3] <- "Payment delay for three month"
default_valid$PAY_0[default_valid$PAY_0 == 4] <- "Payment delay for four month"
default_valid$PAY_0[default_valid$PAY_0 == 5] <- "Payment delay for five month"
default_valid$PAY_0[default_valid$PAY_0 == 6] <- "Payment delay for six month"
default_valid$PAY_0[default_valid$PAY_0 == 7] <- "Payment delay for seven month"
default_valid$PAY_0[default_valid$PAY_0 == 8] <- "Payment delay for eight month or above"

default_valid$PAY_2[default_valid$PAY_2 == -2] <- "No consumption"
default_valid$PAY_2[default_valid$PAY_2 == -1] <- "Paid in full"
default_valid$PAY_2[default_valid$PAY_2 == 0] <- "Use revolving credit"
default_valid$PAY_2[default_valid$PAY_2 == 1] <- "Payment delay for one month"
default_valid$PAY_2[default_valid$PAY_2 == 2] <- "Payment delay for two month"
default_valid$PAY_2[default_valid$PAY_2 == 3] <- "Payment delay for three month"
default_valid$PAY_2[default_valid$PAY_2 == 4] <- "Payment delay for four month"
default_valid$PAY_2[default_valid$PAY_2 == 5] <- "Payment delay for five month"
default_valid$PAY_2[default_valid$PAY_2 == 6] <- "Payment delay for six month"
default_valid$PAY_2[default_valid$PAY_2 == 7] <- "Payment delay for seven month"
default_valid$PAY_2[default_valid$PAY_2 == 8] <- "Payment delay for eight month or above"

default_valid$PAY_3[default_valid$PAY_3 == -2] <- "No consumption"
default_valid$PAY_3[default_valid$PAY_3 == -1] <- "Paid in full"
default_valid$PAY_3[default_valid$PAY_3 == 0] <- "Use revolving credit"
default_valid$PAY_3[default_valid$PAY_3 == 1] <- "Payment delay for one month"
default_valid$PAY_3[default_valid$PAY_3 == 2] <- "Payment delay for two month"
default_valid$PAY_3[default_valid$PAY_3 == 3] <- "Payment delay for three month"
default_valid$PAY_3[default_valid$PAY_3 == 4] <- "Payment delay for four month"
default_valid$PAY_3[default_valid$PAY_3 == 5] <- "Payment delay for five month"
default_valid$PAY_3[default_valid$PAY_3 == 6] <- "Payment delay for six month"
default_valid$PAY_3[default_valid$PAY_3 == 7] <- "Payment delay for seven month"
default_valid$PAY_3[default_valid$PAY_3 == 8] <- "Payment delay for eight month or above"

default_valid$PAY_4[default_valid$PAY_4 == -2] <- "No consumption"
default_valid$PAY_4[default_valid$PAY_4 == -1] <- "Paid in full"
default_valid$PAY_4[default_valid$PAY_4 == 0] <- "Use revolving credit"
default_valid$PAY_4[default_valid$PAY_4 == 1] <- "Payment delay for one month"
default_valid$PAY_4[default_valid$PAY_4 == 2] <- "Payment delay for two month"
default_valid$PAY_4[default_valid$PAY_4 == 3] <- "Payment delay for three month"
default_valid$PAY_4[default_valid$PAY_4 == 4] <- "Payment delay for four month"
default_valid$PAY_4[default_valid$PAY_4 == 5] <- "Payment delay for five month"
default_valid$PAY_4[default_valid$PAY_4 == 6] <- "Payment delay for six month"
default_valid$PAY_4[default_valid$PAY_4 == 7] <- "Payment delay for seven month"
default_valid$PAY_4[default_valid$PAY_4 == 8] <- "Payment delay for eight month or above"

default_valid$PAY_5[default_valid$PAY_5 == -2] <- "No consumption"
default_valid$PAY_5[default_valid$PAY_5 == -1] <- "Paid in full"
default_valid$PAY_5[default_valid$PAY_5 == 0] <- "Use revolving credit"
default_valid$PAY_5[default_valid$PAY_5 == 1] <- "Payment delay for one month"
default_valid$PAY_5[default_valid$PAY_5 == 2] <- "Payment delay for two month"
default_valid$PAY_5[default_valid$PAY_5 == 3] <- "Payment delay for three month"
default_valid$PAY_5[default_valid$PAY_5 == 4] <- "Payment delay for four month"
default_valid$PAY_5[default_valid$PAY_5 == 5] <- "Payment delay for five month"
default_valid$PAY_5[default_valid$PAY_5 == 6] <- "Payment delay for six month"
default_valid$PAY_5[default_valid$PAY_5 == 7] <- "Payment delay for seven month"
default_valid$PAY_5[default_valid$PAY_5 == 8] <- "Payment delay for eight month or above"

default_valid$PAY_6[default_valid$PAY_6 == -2] <- "No consumption"
default_valid$PAY_6[default_valid$PAY_6 == -1] <- "Paid in full"
default_valid$PAY_6[default_valid$PAY_6 == 0] <- "Use revolving credit"
default_valid$PAY_6[default_valid$PAY_6 == 1] <- "Payment delay for one month"
default_valid$PAY_6[default_valid$PAY_6 == 2] <- "Payment delay for two month"
default_valid$PAY_6[default_valid$PAY_6 == 3] <- "Payment delay for three month"
default_valid$PAY_6[default_valid$PAY_6 == 4] <- "Payment delay for four month"
default_valid$PAY_6[default_valid$PAY_6 == 5] <- "Payment delay for five month"
default_valid$PAY_6[default_valid$PAY_6 == 6] <- "Payment delay for six month"
default_valid$PAY_6[default_valid$PAY_6 == 7] <- "Payment delay for seven month"
default_valid$PAY_6[default_valid$PAY_6 == 8] <- "Payment delay for eight month or above"

default_valid$PAY_0 <- as.factor(default_valid$PAY_0)
default_valid$PAY_2 <- as.factor(default_valid$PAY_2)
default_valid$PAY_3 <- as.factor(default_valid$PAY_3)
default_valid$PAY_4 <- as.factor(default_valid$PAY_4)
default_valid$PAY_5 <- as.factor(default_valid$PAY_5)
default_valid$PAY_6 <- as.factor(default_valid$PAY_6)

write.csv(default_valid,"C:/Users/xiwsh/Desktop\\dtree_valid.csv", row.names = FALSE)

#Final model
tc <- rpart.control(minsplit = 50, minbucket = 20, maxdepth = 5)
dt_model3 <- train(default_payment_next_month ~ ., data = default_train[,-c(1)], method = "rpart",
                   metric = "Accuracy", tuneLength = 8, control = tc,
                   trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3))
dt_model3
fancyRpartPlot(dt_model3$finalModel)
print(dt_model3$finalModel)

#Prediction
dt_predict2 <- predict(dt_model3, newdata = default_valid[,-c(1)], type = "prob")
dt_predict <- predict(dt_model3, newdata = default_valid[,-c(1)], type = "raw")
confusionMatrix(dt_predict, default_valid$default_payment_next_month)

#updating the model object because of different factor levels
str(default_train$PAY_2)
str(default_valid$PAY_2)
dt_model3$xlevels[["PAY_2"]] <- union(dt_model3$xlevels[["PAY_2"]], levels(default_valid$PAY_2))

str(default_train$PAY_3)
str(default_valid$PAY_3)
dt_model3$xlevels[["PAY_3"]] <- union(dt_model3$xlevels[["PAY_3"]], levels(default_valid$PAY_3))

str(default_train$PAY_4)
str(default_valid$PAY_4)
dt_model3$xlevels[["PAY_4"]] <- union(dt_model3$xlevels[["PAY_4"]], levels(default_valid$PAY_4))

str(default_train$PAY_5)
str(default_valid$PAY_5)
dt_model3$xlevels[["PAY_5"]] <- union(dt_model3$xlevels[["PAY_5"]], levels(default_valid$PAY_5))

str(default_train$PAY_6)
str(default_valid$PAY_6)
dt_model3$xlevels[["PAY_6"]] <- union(dt_model3$xlevels[["PAY_6"]], levels(default_valid$PAY_6))

#Plot the ROC
roc_curve <- roc(default_valid$default_payment_next_month, dt_predict2$'1')
plot(roc_curve)
auc(roc_curve)
#AUC: 0.7784

