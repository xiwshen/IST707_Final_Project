#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(shinydashboard)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(factoextra)
library(rpart)
library(randomForest)
library(rattle)
library(pROC)
library(naivebayes)
library(e1071)

#setwd("C:/Users/xiwsh/Desktop/Project_Shiny")

#KNN
knn_train <- read_csv("knn_train.csv")
knn_valid <- read_csv("knn_valid.csv")
knn_train$default_payment_next_month <- as.factor(knn_train$default_payment_next_month)
knn_valid$default_payment_next_month <- as.factor(knn_valid$default_payment_next_month)

#NBC
nbc_train <- read_csv("default_train.csv")
nbc_valid <- read_csv("default_valid.csv")
nbc_train <- mutate_if(nbc_train, is.character, as.factor)
nbc_valid <- mutate_if(nbc_valid, is.character, as.factor)

nbc_train$PAY_0 <- as.factor(nbc_train$PAY_0)
nbc_train$PAY_2 <- as.factor(nbc_train$PAY_2)
nbc_train$PAY_3 <- as.factor(nbc_train$PAY_3)
nbc_train$PAY_4 <- as.factor(nbc_train$PAY_4)
nbc_train$PAY_5 <- as.factor(nbc_train$PAY_5)
nbc_train$PAY_6 <- as.factor(nbc_train$PAY_6)

nbc_valid$PAY_0 <- as.factor(nbc_valid$PAY_0)
nbc_valid$PAY_2 <- as.factor(nbc_valid$PAY_2)
nbc_valid$PAY_3 <- as.factor(nbc_valid$PAY_3)
nbc_valid$PAY_4 <- as.factor(nbc_valid$PAY_4)
nbc_valid$PAY_5 <- as.factor(nbc_valid$PAY_5)
nbc_valid$PAY_6 <- as.factor(nbc_valid$PAY_6)

nbc_train$default_payment_next_month <- as.factor(nbc_train$default_payment_next_month)
nbc_valid$default_payment_next_month <- as.factor(nbc_valid$default_payment_next_month)

#DT
dt_train <- read_csv("dtree_train.csv")
dt_valid <- read_csv("dtree_valid.csv")
dt_train <- mutate_if(dt_train, is.character, as.factor)
dt_valid <- mutate_if(dt_valid, is.character, as.factor)

dt_train$default_payment_next_month <- as.factor(dt_train$default_payment_next_month)
dt_valid$default_payment_next_month <- as.factor(dt_valid$default_payment_next_month)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "IST707 Final Project"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("KNN", tabName = "KNN", icon = icon("dashboard")),
            menuItem("NBC", tabName = "NBC", icon = icon("dashboard")),
            menuItem("DT", tabName = "DecisionTree", icon = icon("tree"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("KNN",
                    sliderInput("knum", "Select the number of K:", min = 1, max = 30, value = 11, step = 1),
                    verbatimTextOutput("knn"),
                    plotOutput("knnplot")),
            tabItem("NBC",
                    selectInput("kernal", "Use Kernal Function?", choices = c("TRUE","FALSE")),
                    sliderInput("laplace", "Select the amount of Laplace smoothing:", min = 0, max = 5, value = 1, step = 1),
                    sliderInput("adjust", "Select the level of bandwidth adjustment:", min = 1, max = 5, value = 1, step = 1),
                    verbatimTextOutput("nbc"),
                    plotOutput("nbcplot")),
            tabItem("DecisionTree",
                    sliderInput("maxdepth", "Select the level of maxdepth:", min = 1, max = 20, value = 5, step = 1),
                    sliderInput("minbucket", "Select the level of minbucket:", min = 2, max = 30, value = 20, step = 1),
                    sliderInput("minsplit", "Select the level of minsplit:", min = 10, max = 100, value = 50, step = 5),
                    verbatimTextOutput("dt"),
                    plotOutput("dtplot1"),
                    plotOutput("dtplot2"))
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$knn <- renderPrint({
        knn_output <- train(default_payment_next_month ~ ., data = knn_train[,-c(1)], method = "knn",
                            tuneGrid = expand.grid(k = input$knum),
                            trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3))
        knn_output
    })
    output$knnplot <- renderPlot({
        knn_output <- train(default_payment_next_month ~ ., data = knn_train[,-c(1)], method = "knn",
                            tuneGrid = expand.grid(k = input$knum),
                            trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3))
        knn_predict <- predict(knn_output, newdata = knn_valid[,-c(1)], type = "prob")
        roc_curve <- roc(knn_valid$default_payment_next_month, knn_predict$'1')
        plot(roc_curve)
        auc(roc_curve)
    })
    output$nbc <- renderPrint({
        nbc_model <- train(default_payment_next_month ~ ., data = nbc_train[,-c(1)], method = "naive_bayes",
                           trControl = trainControl(method = "cv", number = 15),
                           tuneGrid = expand.grid(laplace = input$laplace, usekernel = input$kernal, adjust = input$adjust))
        nbc_model
    })
    output$nbcplot <- renderPlot({
        nbc_model <- train(default_payment_next_month ~ ., data = nbc_train[,-c(1)], method = "naive_bayes",
                           trControl = trainControl(method = "cv", number = 15),
                           tuneGrid = expand.grid(laplace = input$laplace, usekernel = input$kernal, adjust = input$adjust))
        
        nbc_model$xlevels[["PAY_2"]] <- union(nbc_model$xlevels[["PAY_2"]], levels(nbc_valid$PAY_2))
        nbc_model$xlevels[["PAY_3"]] <- union(nbc_model$xlevels[["PAY_3"]], levels(nbc_valid$PAY_3))
        nbc_model$xlevels[["PAY_4"]] <- union(nbc_model$xlevels[["PAY_4"]], levels(nbc_valid$PAY_4))
        nbc_model$xlevels[["PAY_5"]] <- union(nbc_model$xlevels[["PAY_5"]], levels(nbc_valid$PAY_5))
        nbc_model$xlevels[["PAY_6"]] <- union(nbc_model$xlevels[["PAY_6"]], levels(nbc_valid$PAY_6))
        
        nbc_predict <- predict(nbc_model, newdata = nbc_valid[,-c(1)], type = "prob")
        roc_curve <- roc(nbc_valid$default_payment_next_month, nbc_predict$'1')
        plot(roc_curve)
        auc(roc_curve)
    })
    output$dt <- renderPrint({
        tc <- rpart.control(minsplit = input$minsplit, minbucket = input$minbucket, maxdepth = input$maxdepth)
        dt_model3 <- train(default_payment_next_month ~ ., data = dt_train[,-c(1)], method = "rpart",
                           metric = "Accuracy", tuneLength = 8, control = tc)
        dt_model3
    })
    output$dtplot1 <- renderPlot({
        tc <- rpart.control(minsplit = input$minsplit, minbucket = input$minbucket, maxdepth = input$maxdepth)
        dt_model3 <- train(default_payment_next_month ~ ., data = dt_train[,-c(1)], method = "rpart",
                           metric = "Accuracy", tuneLength = 8, control = tc)
        fancyRpartPlot(dt_model3$finalModel)
    })
    output$dtplot2 <- renderPlot({
        tc <- rpart.control(minsplit = input$minsplit, minbucket = input$minbucket, maxdepth = input$maxdepth)
        dt_model3 <- train(default_payment_next_month ~ ., data = dt_train[,-c(1)], method = "rpart",
                           metric = "Accuracy", tuneLength = 8, control = tc)
        dt_model3$xlevels[["PAY_2"]] <- union(dt_model3$xlevels[["PAY_2"]], levels(dt_valid$PAY_2))
        dt_model3$xlevels[["PAY_3"]] <- union(dt_model3$xlevels[["PAY_3"]], levels(dt_valid$PAY_3))
        dt_model3$xlevels[["PAY_4"]] <- union(dt_model3$xlevels[["PAY_4"]], levels(dt_valid$PAY_4))
        dt_model3$xlevels[["PAY_5"]] <- union(dt_model3$xlevels[["PAY_5"]], levels(dt_valid$PAY_5))
        dt_model3$xlevels[["PAY_6"]] <- union(dt_model3$xlevels[["PAY_6"]], levels(dt_valid$PAY_6))
        
        dt_predict2 <- predict(dt_model3, newdata = dt_valid[,-c(1)], type = "prob")
        roc_curve <- roc(dt_valid$default_payment_next_month, dt_predict2$'1')
        plot(roc_curve)
        auc(roc_curve)
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
