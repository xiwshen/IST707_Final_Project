library(dplyr)
library(tidyverse)
library(corrplot)
library(plyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(caret)

#read in data
rawdf <- readxl::read_xls("default of credit card clients.xls")
colnames(rawdf) <- unlist(rawdf[1,])
rawdf <- rawdf[-1,]
#make a NA table
NA_table <- sort(sapply(rawdf, function(x) sum(is.na(x))))
sum(NA_table)
#no NA found




#read in cleaned df

numdf <- read_csv("default_payment.csv")
chardf <- read_csv("default_payment2.csv")

tdf <- read_csv("default_train.csv")
vdf <- read_csv("default_valid.csv")

#visulization
cor_cols <- c(1,3,4,5,6,19,20,21,22,23,24,25)
cordf <- numdf[,-cor_cols]
tcor <- cor(cordf,method = "pearson")
corrplot(tcor,type = "upper")

#balance limit vs default next month


balance_default <- ggplot(tdf,aes(LIMIT_BAL))+
  geom_density(aes(fill=factor(default_payment_next_month)),alpha=0.7)+
  labs(title = "Density Plot", subtitle = "Limit Balance by Default Payment Next Month", fill="default payment next month", x="Limit Balance")

balance_default

#education & balance limit vs default next month
edu_default <- ggplot(tdf)+geom_bar(aes(x=EDUCATION,fill=factor(default_payment_next_month)),position = "fill")+
  labs(title = "Bar Plot", subtitle = "Default Payment Distribution by Education Level", y="proportion", fill = "default payment next month")
edu_bal <- ggplot(tdf,aes(x=EDUCATION,y=LIMIT_BAL))+geom_bar(stat = "summary",fun.y="mean")+
  labs(title="Bar Plot", subtitle = "Limit Balance by Education Level",y="Limit Balance")
  
grid.arrange(edu_bal,edu_default,ncol=2)

#age, marriage, sex vs default
age_default <- ggplot(tdf,aes(x = factor(default_payment_next_month), y = AGE))+
  geom_boxplot()+
  facet_grid(MARRIAGE ~ SEX)+
  labs(title = "Box Plot", subtitle = "Default Payment Distribution by Combinations of Marital Status, Age, and Gender")
age_default

#repayment status vs default
tempdf <- tdf
tempdf$PAY_DULY <- ifelse(tempdf$PAY_0==-1 & tempdf$PAY_2==-1 ,1,0)
past2pay <- ggplot(tempdf)+geom_bar(aes(x=factor(PAY_DULY),fill=factor(default_payment_next_month)),position="fill")+
  labs(title = "Bar Plot", subtitle = "Default Payment by Client Pay Duly in Last Two Months", y="proportion",x="pay duly",fill="default payment next month")
tempdf$PAY_DULY <- ifelse(tempdf$PAY_0==-1 & tempdf$PAY_2==-1 & tempdf$PAY_3==-1 & tempdf$PAY_4==-1 & tempdf$PAY_5==-1 & tempdf$PAY_6==-1,1,0)
past6pay <- ggplot(tempdf)+geom_bar(aes(x=factor(PAY_DULY),fill=factor(default_payment_next_month)),position="fill")+
  labs(title = "Bar Plot", subtitle = "Default Payment by Client Pay Duly in Last Six Months", y="proportion",x="pay duly",fill="default payment next month")
grid.arrange(past2pay,past6pay,ncol=2)
#people who payed duly tend to not have default next month than others, customers are consistent on their payment policy

#payment sum vs default
tempdf$tot_pay <- tempdf$PAY_AMT1+tempdf$PAY_AMT2+tempdf$PAY_AMT3+tempdf$PAY_AMT4+tempdf$PAY_AMT5+tempdf$PAY_AMT6
pay_default <- ggplot(tempdf,aes(tot_pay))+geom_density(aes(fill=factor(default_payment_next_month)),alpha=0.7)+coord_cartesian(xlim = c(0,200000))+
  labs(title = "Density Plot", subtitle = "Total Payment in Past Six Months by Default Payment Next Month", fill="default payment next month", x="total payment in last six months")
pay_default 




#logistic regression
logdf <- tdf[,-1]
cat_cols <- c('SEX','EDUCATION','MARRIAGE','PAY_0','PAY_2','PAY_3','PAY_4','PAY_5','PAY_6','default_payment_next_month')
logdf[cat_cols] <- lapply(logdf[cat_cols], as.factor)

log_reg <- train(factor(default_payment_next_month) ~., data = logdf, method = "glm", family = "binomial")
summary(log_reg)
varImp(log_reg)
