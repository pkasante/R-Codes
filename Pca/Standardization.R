#Remove PCa column for standardization
train<-train[-c(1)]
test<-test[-c(1)]
#Columns to be standardized

for (i in 1: ncol(train)){
  if(sd(train[,i])>1e-6){
    xbar<-mean(train[,i])
    sdtrain<-sd(train[,i])
  test[,i] = (test[,i]-xbar)/sdtrain
  train[,i] = (train[,i]-xbar)/sdtrain
  }
  else{
    train[,i] = 0
    test[,i] =0
  }
}

#Check to confirm columns are equal and there are no NA's 
which(is.na(test), arr.ind=TRUE)
which(is.na(train), arr.ind=TRUE)
which(colnames(train)!=colnames(test))
