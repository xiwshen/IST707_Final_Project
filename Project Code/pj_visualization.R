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

default_train <- read_csv("default_train.csv")
str(default_train)

default_valid <- read_csv("default_valid.csv")
str(default_valid)

knn_train <- default_train
knn_valid <- default_valid

knn_train$SEX[knn_train$SEX == "Female"] <- 2 
knn_train$SEX[knn_train$SEX == "Male"] <- 1
knn_train$MARRIAGE[knn_train$MARRIAGE == "married"] <- 1 
knn_train$MARRIAGE[knn_train$MARRIAGE == "single"] <- 2
knn_train$MARRIAGE[knn_train$MARRIAGE == "others"] <- 3
knn_train$EDUCATION[knn_train$EDUCATION == "Graduate School"] <- 1 
knn_train$EDUCATION[knn_train$EDUCATION == "University"] <- 2
knn_train$EDUCATION[knn_train$EDUCATION == "High School"] <- 3
knn_train$EDUCATION[knn_train$EDUCATION == "Others"] <- 4
knn_train <- mutate_if(knn_train, is.character, as.numeric)
str(knn_train)

knn_valid$SEX[knn_valid$SEX == "Female"] <- 2 
knn_valid$SEX[knn_valid$SEX == "Male"] <- 1
knn_valid$MARRIAGE[knn_valid$MARRIAGE == "married"] <- 1 
knn_valid$MARRIAGE[knn_valid$MARRIAGE == "single"] <- 2
knn_valid$MARRIAGE[knn_valid$MARRIAGE == "others"] <- 3
knn_valid$EDUCATION[knn_valid$EDUCATION == "Graduate School"] <- 1 
knn_valid$EDUCATION[knn_valid$EDUCATION == "University"] <- 2
knn_valid$EDUCATION[knn_valid$EDUCATION == "High School"] <- 3
knn_valid$EDUCATION[knn_valid$EDUCATION == "Others"] <- 4
knn_valid <- mutate_if(knn_valid, is.character, as.numeric)
str(knn_valid)

knn_train$default_payment_next_month <- as.factor(knn_train$default_payment_next_month)
knn_valid$default_payment_next_month <- as.factor(knn_valid$default_payment_next_month)

knn_preprocess <- preProcess(knn_train, method = c("scale", "center"))
knn_preprocess

knn_train1 <- predict(knn_preprocess, newdata = knn_train)
knn_valid1 <- predict(knn_preprocess, newdata = knn_valid)

write.csv(knn_train1,"C:/Users/xiwsh/Desktop\\knn_train.csv", row.names = FALSE)
write.csv(knn_valid1,"C:/Users/xiwsh/Desktop\\knn_valid.csv", row.names = FALSE)

knn_new <- read_csv("knn_train.csv")
str(knn_new)

dtnew <- read_csv("dtree_train.csv")
str(dtnew)