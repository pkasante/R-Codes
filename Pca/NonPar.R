# Non Parametric Screening Using Wilcoxon test and Cramer von mises
vnames <- names(train)
dat<-train
defaultW <- getOption("warn") 
options(warn = -1) 
OUT <- NULL
n <- NROW(dat)
cols.x <- c(1:NCOL(dat)) 
for (j in cols.x){
  prostate.cancer<-PCA_train
  xj <- dat[,j]; 
  prostate.cancer <- prostate.cancer[!is.na(xj)]; xj <- xj[!is.na(xj)];  # MAINLY FOR PSA SCORE
  n.marker <- sum(xj>0)
  ybar.1 <- mean(xj[prostate.cancer==1])
  ybar.0 <- mean(xj[prostate.cancer==0])
  diff.mean <-  ybar.1 - ybar.0
  pvalue.equal.var <- pvalue.cvmtest <- logworth.cvmtest <- pvalue.wilcoxon <- logworth.wilcoxon <- NA
  
  if (n.marker >= 4) {
    # levene's test for homogeneity of variance across groups
    pvalue.equal.var <- (car::leveneTest(xj~as.factor(prostate.cancer))$"Pr(>F)")[1] 
    equal.var <- ifelse(pvalue.equal.var <= 0.05, FALSE, TRUE)
    
    #Cramer Von Mises Test
    Two_CVMtest <- (twosamples::cvm_test(xj[prostate.cancer ==0],xj[prostate.cancer==1]))
    pvalue.cvmtest<-Two_CVMtest[2]
    logworth.cvmtest <- -log10(pvalue.cvmtest)
    
    # WILCOXON RANK-SUM TEST
    pvalue.wilcoxon <- wilcox.test(xj~prostate.cancer, alternative="two.sided")$p.value
    logworth.wilcoxon <- -log10(pvalue.wilcoxon )
  }
  out <- c(n.marker, ybar.1, ybar.0, diff.mean, pvalue.equal.var, pvalue.cvmtest, 
           logworth.cvmtest, pvalue.wilcoxon, logworth.wilcoxon)  
  OUT <- rbind(OUT, out)
}
OUT <- as.data.frame(OUT)
options(warn = defaultW)


row.names(OUT) <- NULL #remove row names
OUT <- cbind(vnames[cols.x], OUT)
colnames(OUT) <- c("Biomarker", "n.marker", "ybar.1", "ybar.0", "diff.mean", "pvalue.equal.var", 
                   "pvalue.cvmtest", "logworth.cvmtest", "pvalue.wilcoxon", "logworth.wilcoxon")
head(OUT)
write.csv(OUT, file="OUT.csv", row.names=FALSE) 


# SORT WITH logworth.wilcoxon
OUT1 <- OUT[order(OUT$logworth.wilcoxon), ]
write.csv(OUT1, file="OUT1.csv", row.names=FALSE) 

# Selecting Variables
variables.selected<-OUT1[min(OUT1$pvalue.equal.var,OUT1$pvalue.cvmtest,OUT1$pvalue.wilcoxon,1.0,na.rm = TRUE)<=0.05 &OUT1$n.marker>=5,1]
length(na.omit(variables.selected))
length(variables.selected)  
dat.logit <- dat[order(prostate.cancer), c(variables.selected)]
dat.logit<-cbind(prostate.cancer,dat.logit)
dim(dat.logit)
train <- dat.logit[ , -which(names(dat.logit) %in% c("prostate.cancer"))]
X<-train

#Rename Chemicals and write to file

p <- NCOL(X); n <- NROW(X)
xnames <- names(X)
vnames <- paste("X", 1:length(xnames), sep="")
dat.names <- data.frame(var=xnames, name=vnames)
write.csv(dat.names, file="datnames.csv", row.names=FALSE)
names(X) <- vnames

