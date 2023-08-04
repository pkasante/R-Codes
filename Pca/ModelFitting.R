#SIS (Penalty: Lasso, MCP, SCAD)


set.seed(123)
defaultW <- getOption("warn") 
options(warn = -1) 

fit.SIS <- SIS(x=as.matrix(X), y=dat.logit$prostate.cancer, family = "binomial", nsis =2,nfolds =5 , type.measure="deviance", penalty = "SCAD", iter.max=2,varISIS = "cons")

fit.SIS$coef.est

terms.SIS <- names(fit.SIS$coef.est)[-1]
form <- as.formula(paste(c("prostate.cancer ~ ", as.character(terms.SIS)), collapse=" + "))
dat.new <- data.frame(cbind(prostate.cancer=dat.logit$prostate.cancer, X)) 
fit.glm <- glm(form, data = dat.new, family="binomial")
summary(fit.glm)




#Model fitting
fit.glm<- logistf(form, data = dat.new)
summary(fit.glm)
# USING ncvreg
# -------------
cvfit.ncvreg <- cv.ncvreg(X=as.matrix(X), y=dat.logit$prostate.cancer, nfolds=20,
                          family="binomial", penalty="SCAD", nlambda=150, max.iter=300)
plot(cvfit.ncvreg)
names(cvfit.ncvreg)


beta.hat <- coef(cvfit.ncvreg)  # THE COEFFICIENTS WITH MINIMUM CV ERROR
cutoff <- 0
terms <- (names(beta.hat)[abs(beta.hat) > cutoff])[-1]; terms

beta.hat[abs(beta.hat) > cutoff]

form <- as.formula(paste(c("prostate.cancer ~ ", as.character(terms)), collapse=" + "))
dat.new <- data.frame(cbind(prostate.cancer=dat.logit$prostate.cancer, X)) 
fit.L1 <- glm(form, data = dat.new, family="binomial")
summary(fit.L1)
fit.L1 <- logistf(form, data = dat.new)
summary(fit.L1)
coef(fit.L1)




dat.0 <- dat.new[, c("prostate.cancer", terms)]
n <- NROW(dat.0)
names(dat.0); dat.0$prostate.cancer
y.obs <- dat.0$"prostate.cancer"
y.pred <- rep(0, n)
for (i in 1:n){
  # print(i)
  fit.i <- logistf(prostate.cancer~., data=dat.0[-i,])
  beta.hat <- coef(fit.i)
  eta <- sum(dat.0[i, -1]*beta.hat[-1]) + beta.hat[1]
  y.pred[i] <- exp(eta)/(1+exp(eta))
}


#dat.0[1:5,1:5]
y.pred
y.obs
dat.tmp <- cbind(y.pred, y.obs)
colnames(dat.tmp) <- c("pred", "y")

dat.tmp <- data.frame(dat.tmp)

#Cut points with MaxEfficiency
result0 <- optimal.cutpoints(pred~y, data=dat.tmp, tag.healthy=0,
                             methods="MaxEfficiency", control=control.cutpoints())

#Cut points with Youden
result1 <- optimal.cutpoints(pred~y, data=dat.tmp, tag.healthy=0,
                             methods="Youden", control=control.cutpoints())

table(y.obs, sign(y.pred >=0.5)) 


AUC <- ci.cvAUC(predictions=y.pred, labels=y.obs, folds=1:n, confidence=0.95)
auc_1 <- round(AUC$cvAUC, digits=3); auc.ci <- round(AUC$ci, digits=3)
AUC

# Verification
mod.glm <- verify(obs=y.obs, pred=y.pred, bins = FALSE)
roc.plot(mod.glm, plot.thres = NULL)
text(x=0.7, y=0.2, paste("Area under ROC =", auc_1, "with 95% CI (", auc.ci[1], ",", auc.ci[2], ").",
                         sep=" "), col="blue", cex=1.2)

# ROC
auc(y.obs, y.pred) 
plot(roc(y.obs, y.pred, direction="<"), panel.first = grid(),
     col="black", lwd=2, main="ROC Curve for Logit Model")
text(x=0.4, y=0.2, paste("Area under ROC =", auc_1, "with 95% CI (", auc.ci[1], ",", auc.ci[2], ").",
                         sep=" "), col="blue", cex=1.2)

conf_Mat<-table(y.obs,sign(y.pred>=0.4))
conf_Mat
confusionMatrix(conf_Mat)





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





