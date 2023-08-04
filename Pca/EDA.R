#Convert to 0's and 1's
train$prostate.cancer<-ifelse(train$prostate.cancer == "positive",1,0)

#Store test response
PCA_test<-test$prostate.cancer

# Store train responses
PCA_train<- train$prostate.cancer


#
#Store Column names
var.train = colnames(train)[-c(1, 2)]
var.test  = colnames(test)[-c(1, 2)]
 
#Remove chemicals in test not found in train and add missing chemicals from train to test
test = test[, intersect(var.train, var.test)]
test[, setdiff(var.train, var.test)] = 0.0
test<-cbind(PCA_test,test)
names(test)[1]<-"prostate.cancer"
train<-train[-1]





#Order columns of test to match train
test<-select(test, colnames(train))

#Check if all colnames are equal in both data sets
all.equal(colnames(test),colnames(train))

#Remove NA's
train[is.na(train)]<-0
test[is.na(test)]<-0

Train<-train
Test<-test

