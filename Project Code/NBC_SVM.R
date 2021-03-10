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
library(naivebayes)
library(pROC)
library(car)

default_train <- read_csv("default_train.csv")
str(default_train)

default_valid <- read_csv("default_valid.csv")
str(default_valid)

nbc_train <- default_train
nbc_valid <- default_valid

nbc_train <- mutate_if(nbc_train, is.character, as.factor)
str(nbc_train)

nbc_train$PAY_0 <- as.factor(nbc_train$PAY_0)
nbc_train$PAY_2 <- as.factor(nbc_train$PAY_2)
nbc_train$PAY_3 <- as.factor(nbc_train$PAY_3)
nbc_train$PAY_4 <- as.factor(nbc_train$PAY_4)
nbc_train$PAY_5 <- as.factor(nbc_train$PAY_5)
nbc_train$PAY_6 <- as.factor(nbc_train$PAY_6)


nbc_valid <- mutate_if(nbc_valid, is.character, as.factor)
str(nbc_valid)

nbc_valid$PAY_0 <- as.factor(nbc_valid$PAY_0)
nbc_valid$PAY_2 <- as.factor(nbc_valid$PAY_2)
nbc_valid$PAY_3 <- as.factor(nbc_valid$PAY_3)
nbc_valid$PAY_4 <- as.factor(nbc_valid$PAY_4)
nbc_valid$PAY_5 <- as.factor(nbc_valid$PAY_5)
nbc_valid$PAY_6 <- as.factor(nbc_valid$PAY_6)

nbc_train$default_payment_next_month <- as.factor(nbc_train$default_payment_next_month)
nbc_valid$default_payment_next_month <- as.factor(nbc_valid$default_payment_next_month)

#Base model
model_nb <- train(default_payment_next_month ~ ., data = nbc_train[,-c(1)], method = "naive_bayes")
model_nb

model_nbc2 <- train(default_payment_next_month ~ ., data = nbc_train[,-c(1)], method = "naive_bayes",
                   trControl = trainControl(method = "cv", number = 15),
                   tuneGrid = expand.grid(laplace = 1:3, usekernel = c(TRUE, FALSE), adjust = 1:3))
model_nbc2

#Final model
nbc_model <- train(default_payment_next_month ~ ., data = nbc_train[,-c(1)], method = "naive_bayes",
                    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                    tuneGrid = expand.grid(laplace = 1, usekernel = FALSE, adjust = 1))
nbc_model


nbc_pred <- predict(nbc_model, newdata = nbc_valid[,-c(1)])
confusionMatrix(nbc_pred, nbc_valid$default_payment_next_month)

str(nbc_train$PAY_3)
str(nbc_valid$PAY_3)
nbc_model$xlevels[["PAY_2"]] <- union(nbc_model$xlevels[["PAY_2"]], levels(nbc_valid$PAY_2))
nbc_model$xlevels[["PAY_3"]] <- union(nbc_model$xlevels[["PAY_3"]], levels(nbc_valid$PAY_3))
nbc_model$xlevels[["PAY_4"]] <- union(nbc_model$xlevels[["PAY_4"]], levels(nbc_valid$PAY_4))
nbc_model$xlevels[["PAY_5"]] <- union(nbc_model$xlevels[["PAY_5"]], levels(nbc_valid$PAY_5))
nbc_model$xlevels[["PAY_6"]] <- union(nbc_model$xlevels[["PAY_6"]], levels(nbc_valid$PAY_6))

roc.curve(nbc_valid$default_payment_next_month, nbc_pred)

nbc_predict <- predict(nbc_model, newdata = nbc_valid[,-c(1)], type = "prob")

roc_curve <- roc(nbc_valid$default_payment_next_month, nbc_predict$'1')
plot(roc_curve)
auc(roc_curve)
#Area under the curve: 0.7404

#################Logistic
glm_train <- default_train
glm_valid <- default_valid

glm_train <- mutate_if(glm_train, is.character, as.factor)
str(glm_train)
glm_train$PAY_0 <- as.factor(glm_train$PAY_0)
glm_train$PAY_2 <- as.factor(glm_train$PAY_2)
glm_train$PAY_3 <- as.factor(glm_train$PAY_3)
glm_train$PAY_4 <- as.factor(glm_train$PAY_4)
glm_train$PAY_5 <- as.factor(glm_train$PAY_5)
glm_train$PAY_6 <- as.factor(glm_train$PAY_6)


glm_valid <- mutate_if(glm_valid, is.character, as.factor)
str(glm_valid)
glm_valid$PAY_0 <- as.factor(glm_valid$PAY_0)
glm_valid$PAY_2 <- as.factor(glm_valid$PAY_2)
glm_valid$PAY_3 <- as.factor(glm_valid$PAY_3)
glm_valid$PAY_4 <- as.factor(glm_valid$PAY_4)
glm_valid$PAY_5 <- as.factor(glm_valid$PAY_5)
glm_valid$PAY_6 <- as.factor(glm_valid$PAY_6)

glm_train <- glm_train[,-c(1)]
glm_valid <- glm_valid[,-c(1)]
glm_train$default_payment_next_month <- as.factor(glm_train$default_payment_next_month)
glm_valid$default_payment_next_month <- as.factor(glm_valid$default_payment_next_month)

glm_trian2 <- glm_train[,-c(12,13,14,15,16,17)]
glm_valid2 <- glm_valid[,-c(12,13,14,15,16,17)]

model_glm <- train(default_payment_next_month ~ ., data = glm_train2, 
                   method = "glm", family = "binomial")
print(model_glm)
summary(model_glm)
varImp(model_glm)

model_glm$xlevels[["PAY_2"]] <- union(model_glm$xlevels[["PAY_2"]], levels(glm_valid2$PAY_2))
model_glm$xlevels[["PAY_3"]] <- union(model_glm$xlevels[["PAY_3"]], levels(glm_valid2$PAY_3))
model_glm$xlevels[["PAY_4"]] <- union(model_glm$xlevels[["PAY_4"]], levels(glm_valid2$PAY_4))
model_glm$xlevels[["PAY_5"]] <- union(model_glm$xlevels[["PAY_5"]], levels(glm_valid2$PAY_5))
model_glm$xlevels[["PAY_6"]] <- union(model_glm$xlevels[["PAY_6"]], levels(glm_valid2$PAY_6))

glm_pred <- predict(model_glm, newdata = glm_valid2)
confusionMatrix(glm_pred, glm_valid2$default_payment_next_month)

glm_pred2 <- predict(model_glm, newdata = glm_valid, type = "prob")
glm_roc_curve <- roc(glm_valid$default_payment_next_month, glm_pred2$'1')
plot(glm_roc_curve)
auc(glm_roc_curve)

#build a linear model, then calculate the Variance Inflation Factor to deal with multi-collinearity problem.
#We removed column Bill amount 1-6 since the vif value are much larger than other attributes.
linearmodel <- lm(default_payment_next_month ~ ., data = svm_train)
vif(linearmodel)
#####################SVMlinear#################
svm_train <- default_train
svm_valid <- default_valid
str(svm_train)

#convert categorical attributes into numerical
svm_train$SEX[svm_train$SEX == "Female"] <- 2 
svm_train$SEX[svm_train$SEX == "Male"] <- 1
svm_train$MARRIAGE[svm_train$MARRIAGE == "married"] <- 1 
svm_train$MARRIAGE[svm_train$MARRIAGE == "single"] <- 2
svm_train$MARRIAGE[svm_train$MARRIAGE == "others"] <- 3
svm_train$EDUCATION[svm_train$EDUCATION == "Graduate School"] <- 1 
svm_train$EDUCATION[svm_train$EDUCATION == "University"] <- 2
svm_train$EDUCATION[svm_train$EDUCATION == "High School"] <- 3
svm_train$EDUCATION[svm_train$EDUCATION == "Others"] <- 4
svm_train <- mutate_if(svm_train, is.character, as.numeric)
str(svm_train)

svm_valid$SEX[svm_valid$SEX == "Female"] <- 2 
svm_valid$SEX[svm_valid$SEX == "Male"] <- 1
svm_valid$MARRIAGE[svm_valid$MARRIAGE == "married"] <- 1 
svm_valid$MARRIAGE[svm_valid$MARRIAGE == "single"] <- 2
svm_valid$MARRIAGE[svm_valid$MARRIAGE == "others"] <- 3
svm_valid$EDUCATION[svm_valid$EDUCATION == "Graduate School"] <- 1 
svm_valid$EDUCATION[svm_valid$EDUCATION == "University"] <- 2
svm_valid$EDUCATION[svm_valid$EDUCATION == "High School"] <- 3
svm_valid$EDUCATION[svm_valid$EDUCATION == "Others"] <- 4
svm_valid <- mutate_if(svm_valid, is.character, as.numeric)
str(svm_valid)

svm_train <- svm_train[,-c(1)]
svm_valid <- svm_valid[,-c(1)]
svm_train$default_payment_next_month <- as.factor(svm_train$default_payment_next_month)
svm_valid$default_payment_next_month <- as.factor(svm_valid$default_payment_next_month)

#scale the data
svm_preprocess <- preProcess(svm_train, method = c("scale", "center"))
svm_preprocess

svm_train1 <- predict(svm_preprocess, newdata = svm_train)
svm_valid1 <- predict(svm_preprocess, newdata = svm_valid)

#Final SVM model
svm_model <- train(default_payment_next_month ~ ., data = svm_train1,
                    method = "svmLinear",
                    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                    tuneGrid = expand.grid(C = 2.8))
svm_model
plot(svm_model)
#2.8  0.8099603  0.2980107

predict_svm_linear <- predict(svm_model, newdata = svm_valid1)
confusionMatrix(predict_svm_linear, svm_valid1$default_payment_next_month)

