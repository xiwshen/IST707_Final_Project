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

pj_data <- read_excel("default_of_credit_card_clients.xls")
str(pj_data)
summary(pj_data)

#Check for missing values
sum(!complete.cases(pj_data))
sapply(pj_data,function(x) sum(is.na(x)))

#Check and remove duplicate data
nrow(pj_data)
nrow(pj_data[!duplicated(pj_data), ])

table(pj_data$SEX)
table(pj_data$EDUCATION)
table(pj_data$MARRIAGE)
table(pj_data$PAY_0)
table(pj_data$PAY_2)
table(pj_data$default_payment_next_month)

#Remove data in two columns that have unknown labels
pj_data <- pj_data[!(pj_data$MARRIAGE == 0),]
pj_data <- pj_data[!(pj_data$EDUCATION == 0),]
pj_data <- pj_data[!(pj_data$EDUCATION == 5),]
pj_data <- pj_data[!(pj_data$EDUCATION == 6),]

#Desc stats
res <- stat.desc(pj_data)
round(res, 2)

pj_data2 <- pj_data

pj_data2$SEX[pj_data2$SEX == 1] <- "Male"
pj_data2$SEX[pj_data2$SEX == 2] <- "Female"
pj_data2$MARRIAGE[pj_data2$MARRIAGE == 1] <- "married"
pj_data2$MARRIAGE[pj_data2$MARRIAGE == 2] <- "single"
pj_data2$MARRIAGE[pj_data2$MARRIAGE == 3] <- "others"
pj_data2$EDUCATION[pj_data2$EDUCATION == 1] <- "Graduate School"
pj_data2$EDUCATION[pj_data2$EDUCATION == 2] <- "University"
pj_data2$EDUCATION[pj_data2$EDUCATION == 3] <- "High School"
pj_data2$EDUCATION[pj_data2$EDUCATION == 4] <- "Others"

pj_data2$SEX <- as.factor(pj_data2$SEX)
pj_data2$EDUCATION <- as.factor(pj_data2$EDUCATION)
pj_data2$MARRIAGE <- as.factor(pj_data2$MARRIAGE)
pj_data2$PAY_0 <- as.factor(pj_data2$PAY_0)
pj_data2$PAY_2 <- as.factor(pj_data2$PAY_2)
pj_data2$PAY_3 <- as.factor(pj_data2$PAY_3)
pj_data2$PAY_4 <- as.factor(pj_data2$PAY_4)
pj_data2$PAY_5 <- as.factor(pj_data2$PAY_5)
pj_data2$PAY_6 <- as.factor(pj_data2$PAY_6)
pj_data2$default_payment_next_month <- as.factor(pj_data2$default_payment_next_month)
str(pj_data2)

write.csv(pj_data,"C:/Users/xiwsh/Desktop\\default_payment.csv", row.names = FALSE)
write.csv(pj_data2,"C:/Users/xiwsh/Desktop\\default.csv", row.names = FALSE)

set.seed(123)
train_index <- createDataPartition(pj_data2$default_payment_next_month, p = 0.7, list = FALSE)

default_train <- pj_data2[train_index, ]
default_valid <- pj_data2[-train_index, ]

write.csv(default_train,"C:/Users/xiwsh/Desktop\\default_train.csv", row.names = FALSE)
write.csv(default_valid,"C:/Users/xiwsh/Desktop\\default_valid.csv", row.names = FALSE)

str(pj_data)
#Plot a correlation matrix
pj_corr <- pj_data[,-c(1)]
cor_matrix <- cor(pj_corr[complete.cases(pj_corr), sapply(pj_corr, is.numeric)], method = "pearson")
corrplot(cor_matrix, type = "upper")


#########################################################################
par(mfrow=c(2,2))
boxplot(pj_data$LIMIT_BAL, main = "Boxplots for Limit_Bal", names = c("Limit_Bal"),
        col = c("orange"))
boxplot(pj_data$AGE, main = "Boxplots for Age", names = c("Age"),
        col = c("red"))
boxplot(pj_data$BILL_AMT1, main = "Boxplots for Bill Amount 1", names = c("Bill Amount 1"),
        col = c("blue"))
boxplot(pj_data$BILL_AMT2, main = "Boxplots for Bill Amount 2", names = c("Bill Amount 2"),
        col = c("green"))

boxplot(pj_data$BILL_AMT3, main = "Boxplots for Bill Amount 3", names = c("Bill Amount 3"),
        col = c("blue"))
boxplot(pj_data$BILL_AMT4, main = "Boxplots for Bill Amount 4", names = c("Bill Amount 4"),
        col = c("green"))
boxplot(pj_data$BILL_AMT5, main = "Boxplots for Bill Amount 5", names = c("Bill Amount 5"),
        col = c("blue"))
boxplot(pj_data$BILL_AMT6, main = "Boxplots for Bill Amount 6", names = c("Bill Amount 6"),
        col = c("green"))

boxplot(pj_data$PAY_AMT1, main = "Boxplots for Pay Amount 1", names = c("Pay Amount 1"),
        col = c("blue"))
boxplot(pj_data$PAY_AMT2, main = "Boxplots for Pay Amount 2", names = c("Pay Amount 2"),
        col = c("green"))
boxplot(pj_data$PAY_AMT3, main = "Boxplots for Pay Amount 3", names = c("Pay Amount 3"),
        col = c("blue"))
boxplot(pj_data$PAY_AMT4, main = "Boxplots for Pay Amount 4", names = c("Pay Amount 4"),
        col = c("green"))

boxplot(pj_data$PAY_AMT5, main = "Boxplots for Pay Amount 5", names = c("Pay Amount 5"),
        col = c("blue"))
boxplot(pj_data$PAY_AMT6, main = "Boxplots for Pay Amount 6", names = c("Pay Amount 6"),
        col = c("green"))
par(mfrow=c(1,1))
################################################################

#K-Means
kmeans_data <- pj_data[,-c(1)]

kmeans_data1 <- scale(kmeans_data, center = T, scale = T)

kmeans_train_index <- createDataPartition(kmeans_data1$default_payment_next_month, p = 0.7, list = FALSE)

kmeans_train <- kmeans_data1[kmeans_train_index, ]
kmeans_test <- kmeans_data1[-kmeans_train_index, ]


km_output <- kmeans(kmeans_data1, centers = 2, nstart = 25, iter.max = 1000, algorithm = "Hartigan-Wong")
str(km_output)

km <- km_output$cluster
fviz_cluster(km_output, data = kmeans_data1)

table(data.frame(kmeans_data1$default_payment_next_month,km_output$cluster))

##################################KNN
knn_train <- default_train
knn_valid <- default_valid

#convert categorical attributes to numerical attributes.
knn_train <- mutate_if(knn_train, is.factor, as.character)
str(knn_train)
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

knn_valid <- mutate_if(knn_valid, is.factor, as.character)
str(knn_valid)
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
#since we are doing classification, convert outcome column as a 2 level factor

knn_model <- train(default_payment_next_month ~ ., data = knn_train1, method = "knn",
                    tuneGrid = data.frame(k = seq(1, 25)),
                    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3))
knn_model
plot(knn_model)
#13  0.8045393  0.3107107

#Final KNN model
knn_output <- train(default_payment_next_month ~ ., data = knn_train1[,-c(1)], method = "knn",
                     tuneGrid = expand.grid(k = 13),
                     trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3))
knn_output

knn_predict <- predict(knn_output, newdata = knn_valid1[,-c(1)])
confusionMatrix(knn_predict, knn_valid1$default_payment_next_month)
roc.curve(knn_valid1$default_payment_next_month, knn_predict)
knn_predict2 <- predict(knn_output, newdata = knn_valid1[,-c(1)], type = "prob")
#Plot ROC
roc_curve <- roc(knn_valid1$default_payment_next_month, knn_predict2$'1')
plot(roc_curve)
auc(roc_curve)

###################DT
dt_data <- pj_data[,-c(1)]

dt_data$default_payment_next_month <- as.factor(dt_data$default_payment_next_month)
dt_train_index <- createDataPartition(dt_data$default_payment_next_month, p = 0.7, list = FALSE)

dt_train <- dt_data[dt_train_index, ]
dt_test <- dt_data[-dt_train_index, ]

dt_model <- train(default_payment_next_month ~ ., data = default_train[,-c(1)], metric = "Accuracy", method = "rpart")
print(dt_model)

dt_model1 <- train(default_payment_next_month ~ ., data = default_train[,-c(1)], method = "rpart", metric = "Accuracy",
                   tuneGrid = expand.grid(cp = seq(0, 0.1, 0.01)))
print(dt_model1)

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
tune.gridcart <- expand.grid(maxdepth = 2:10)
dt_model2 <- train(default_payment_next_month ~ ., data = default_train[,-c(1)], metric = "Accuracy", method = "rpart2",
                   tuneGrid =tune.gridcart, trControl = trctrl)
print(dt_model2)

tc <- rpart.control(minsplit = 50, minbucket = 20, maxdepth = 5)
dt_model3 <- train(default_payment_next_month ~ ., data = default_train[,-c(1)], method = "rpart",
                           metric = "Accuracy", tuneLength = 8, control = tc)
dt_model3
fancyRpartPlot(dt_model3$finalModel)
########################RF

rfmodel <- train(default_payment_next_month ~ ., data = default_train[,-c(1)], method = "rf")
rfmodel
plot(rfmodel)
varimp_rf <- varImp(rfmodel)
plot(varimp_rf, main = "Variable Importance with Random Forest")
varImpPlot(varimp_rf, sort = TRUE, n.var=min(20, nrow(varimp_rf$importance)))

plot(varimp_rf, top = 15, main = "Variable Importance with Random Forest")

rf_predict <- predict(rfmodel, newdata = default_valid[,-c(1)])
confusionMatrix(rf_predict, default_valid$default_payment_next_month)

