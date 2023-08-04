#Read In Data



#LOAD LIBRARIES
suppressMessages(library("readxl"))
library(parallel)
library(leaps)
suppressMessages(library(MASS))
suppressMessages(library(SmartEDA))
suppressMessages(library(twosamples))
suppressMessages(library(gridExtra))
suppressMessages(library(DAAG))
suppressMessages(library(scales))
suppressMessages(library(ggpubr))
suppressMessages(library(rstatix))
suppressMessages(library(verification))
suppressMessages(library(car))
suppressMessages(library(SIS))
suppressMessages(library(logistf))
suppressMessages(library(cvAUC))
suppressMessages(library(OptimalCutpoints))
suppressMessages(library(astsa))
suppressMessages(library(ncvreg))
suppressMessages(library(pROC))
suppressMessages(library(ggplot2))
suppressMessages(library(corrplot))
suppressMessages(library(nortest))
suppressMessages(library(reshape2))
suppressMessages(library(dplyr))
suppressMessages(library(randomForest))
suppressMessages(library(caret))
suppressMessages(library(janitor))
library(smotefamily)
library("foreach")
library(dplyr)
rm(list=ls(all.names=TRUE))
set.seed(123)


#Read Data
train<-read_excel("train.xlsx")
test<-  read_excel("test.xlsx")
 


#Check responses are balanced in train
ggplot(train, aes(x = factor(prostate.cancer), fill = factor(prostate.cancer))) + geom_bar()

#CONVERT TO DATAFRAME
train<-data.frame(train)
test<-data.frame(test)


