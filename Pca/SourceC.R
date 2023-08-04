#SET WORKING DIRECTORY
setwd("/home/peter/PCA_Project/VOC.PCa/MLProg")
source("ReadData.R")
source("EDA.R")
source("Standardization.R")

flag = readline(prompt = "Enter 1, 2 or any other number or letter : ")
# convert the inputted value to an integer
if(flag==1){
  source("NonPar.R")
  }else if(flag==2) {
    source("NonPar2.R")
  }else{
    source("NonPar3.R")
  }
source("ModelFitting.R")
source("Modeltesting.R")  
