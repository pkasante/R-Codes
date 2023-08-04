## Prediction with test data


dat<-test
# Choose corresponding chemicals from model coefficients
Vars<-names(fit.L1$coefficients[-1])
Vars<-gsub('X','',Vars)
Vars<-as.numeric(Vars)
Vars<-dat.names[Vars,1]
write.csv(Vars, file="Vars.csv", row.names=FALSE) 
vars2<-c("Intercept",Vars)
n <- NROW(dat)
MIT<-dat
Xs.selected <- as.character(Vars)
xname0 <- Xs.selected[!is.element(Xs.selected, names(MIT))]
MIT0 <- cbind(MIT, matrix(0, NROW(MIT), length(xname0)))
names(MIT0) <- c(names(MIT), xname0)  

dat.MIT <- MIT0[, Xs.selected]
names(dat.MIT); dim(dat.MIT)
dat.MIT[is.na(dat.MIT)] <- 0  # REPALCE NA WITH 0
names(fit.L1$coefficients)<-vars2
beta.hat <- coef(fit.L1)
beta.hat<-as.matrix(beta.hat)
dat.MIT<-as.matrix(dat.MIT)
eta <- as.matrix(dat.MIT)%*%as.vector(beta.hat[-1]) + beta.hat[1]
y.pred <- exp(eta)/(1+exp(eta))
y.pred<-as.numeric(y.pred)




y.obs <- PCA_test
y.obs<-as.integer(y.obs)
data.frame(y.pred=y.pred, y.obs=y.obs)
table(y.obs, sign(y.pred >=0.5)) 
n <- length(y.pred)

AUC <- ci.cvAUC(predictions=y.pred, labels=y.obs, folds=1:n, confidence=0.95)

auc_2 <- round(AUC$cvAUC, digits=2); auc.ci <- round(AUC$ci, digits=3)


auc_3<-auc(y.obs, y.pred) 

mod.glm <- verify(obs=y.obs, pred=y.pred)
roc.plot(mod.glm, plot.thres = NULL)
text(x=0.7, y=0.2, paste("Area under ROC =", AUC$cvAUC, "with 95% CI (", auc.ci[1], ",", auc.ci[2], ").",
                         sep=" "), col="blue", cex=1.2)

plot(roc(y.obs, y.pred, direction="<"), panel.first = grid(),
     col="black", lwd=2, main="ROC Curve for Logit Model")
text(x=0.4, y=0.2, paste("Area under ROC =", round(AUC$cvAUC,digits = 3), "with 95% CI (", auc.ci[1], ",", auc.ci[2], ").",
                         sep=" "), col="blue", cex=1.2)




#density plot of observed
d2 <- density(y.obs,bw=0.5)
plot(d2, type="n", main="observed")
polygon(d2, col="lightgray", border="gray")
rug(y.obs, col="red")


#density plot of predicted
d <- density(y.pred,bw=0.5)
plot(d, type="n", main="predicted")
polygon(d, col="lightgray", border="gray")
rug(y.pred, col="red")

#Selected variables in training set.
VarTrain<-train[colnames(dat.MIT)]

#check to for equal columns
which(colnames(dat.MIT)!=colnames(VarTrain))

##Density plot
Pred_train<-ifelse(PCA_train==1,"positive","negative")
Pred_test<-ifelse(PCA_test==1,"positive","negative")

VarTrain<-cbind(Pred_train,VarTrain)
VarTrain<-data.frame(VarTrain)

dat.MIT<-cbind(Pred_test,dat.MIT)
dat.MIT<-data.frame(dat.MIT)

flag = readline(prompt = "Enter 1 for the normalized density plot or any number or letter for the non-normalized density plot : ")

if(flag ==1){
  for (i in 2:ncol(dat.MIT)) {
    p1<-ggplot(VarTrain, 
             aes_string(x = names(VarTrain[i]),
                        fill = names(VarTrain[1]))) + 
    geom_density(alpha = 0.5)  + theme(axis.text.x = element_blank()) +  
    labs(title = "Density plot of predictors in train")
    
    p2<-ggplot(dat.MIT, 
             aes_string(x =names(dat.MIT[i]), 
                        fill = names(dat.MIT[1]))) + 
    geom_density(alpha = 0.5) + theme(axis.text.x = element_blank()) +
    labs(title = "Density plot of predictors in test")
    
    grid.arrange(p1,p2,ncol=2)
  }
  }else{ VarTrain2<-Train[colnames(dat.MIT[-1])] 
  VarTest<-Test[colnames(dat.MIT[-1])]
  
  VarTrain2<-cbind(Pred_train,VarTrain2)
  VarTest<-cbind(Pred_test,VarTest)
  
  for (i in 2:ncol(dat.MIT)){
    p1<-ggplot(VarTrain2, 
             aes_string(x = names(VarTrain2[i]), 
                        fill = names(VarTrain2[1]))) +
    geom_density(alpha = 0.5)  + theme(axis.text.x = element_blank()) +  
    labs(title = "Density plot of predictors in train")
    
    p2<-ggplot(VarTest, 
             aes_string(x =names(VarTest[i]), 
                        fill = names(VarTest[1]))) + 
    geom_density(alpha = 0.5) + theme(axis.text.x = element_blank()) +
    labs(title = "Density plot of predictors in test")
    grid.arrange(p1,p2,ncol=2)
  }
  }
