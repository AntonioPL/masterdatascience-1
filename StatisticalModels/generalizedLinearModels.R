## --------------------------
## Script : Logistic Regression 
## Author : Paulo Fernandez (based on the material and the data provided in the Statistical Modelling courses from Antonio Pita)
## --------------------------

## 1. Libraries loading block

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

if(!require("caTools")){
  install.packages("caTools")
  library("caTools")
}

if(!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")
}

## -------------------------------------------------------------------------
##       PART 1: LOGISTIC REGRESSION LOGIT
## -------------------------------------------------------------------------


## --------------------------
## 2. Data loading block

bank <- read.csv2("data/bank-full.csv")
##### Data extracted from https://archive.ics.uci.edu/ml/datasets/bank+Marketing

## --------------------------
## 3. Data inspection block

str(bank)
head(bank)
tail(bank)
summary(bank)

## The last variable y, is the success of the marketing campaign (if the client has created or not a term deposit)

## --------------------------
## 4. Data formatting block


bank$day=as.factor(bank$day)
bank$campaign=as.factor(bank$campaign)
bank$IndPrevio=as.factor(as.numeric(bank$pdays!=-1))

str(bank)
head(bank)
summary(bank)

count(bank[bank$poutcome=='success',])/count(bank[bank$poutcome %in% c('failure','success','other'),])
count(bank[bank$IndPrevio==1,])/count(bank)

## --------------------------
## 5. Logistic regression block

modelLogit <- glm(y~.,data=bank,family = binomial('logit'))
summary(modelLogit)

## Obviously, here are to many variables. We probably have the same problems of multicolinearity than in linear regression. Also, it is important to
## notice that every categorical variable is split into all their categories (i.e. if a given variable is 'yes', 'no' or 'na', then we will have a 
## binary variable for 'yes', another for 'no' and the other is included in the intercept)

## We can use the step function to create a more adequate logit model

modelLogit <- step(glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome,data=bank,family=binomial('logit')),direction='both',trace=1)
summary(modelLogit)

## We can see that the job is not especially relevant, except when we face a retired or a student.
## For the job, it would be better to put the retired in the intercept, since we already know that it is statistically significant.
bank$job = relevel(bank$job,'retired')
bank$martial = relevel(bank$marital,'married')   ## For marriage,I will put in the intercept married
bank$education = relevel(bank$education,'unknown') ## For education, I will separate the unknown education
bank$contact = relevel(bank$contact,'unknown') ## For contact, I will put unknown in the intercept
bank$month = relevel(bank$month,'jan')

modelLogitFinal <- step(glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome,data=bank,family=binomial('logit')),direction='both',trace=1)
summary(modelLogitFinal)

coef(modelLogitFinal)
exp(coef(modelLogitFinal))
exp(cbind(coef(modelLogitFinal), confint(modelLogitFinal,level=0.95))) 

## The interpretation of the coefficients is the following: each exponential is the odd ratio of the case selected vs the base case (i.e. the odds of
## the success of the campaign calling in April vs calling in jan are in the confidence interval [2.37,3.6])

## -------------------------------------------------------------------------
##       PARTE 2: BINOMIAL REGRESSION PROBIT
## -------------------------------------------------------------------------

## 6. Probit regression block

probitModel <- glm(y ~ job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=bank, family=binomial('probit'))
summary(probitModel)

## -------------------------------------------------------------------------
## 7. Comparision Logit/Probit block

X = seq(-4,4,0.1)
sigmoid = 1/(1+exp(-X))
normalFunction = pnorm(X,0,1)
plot(sigmoid,type='l',col='red')
lines(normalFunction,col='blue')

probitModelFinal <- step(probitModel,direction="both",trace=1)
summary(probitModelFinal)

anova(modelLogitFinal,probitModelFinal)

## From the ANOVA test, we can see that both models are simmilar. Let's check which one have more predictive power.

AIC(modelLogitFinal)
AIC(probitModelFinal)

BIC(modelLogitFinal)
BIC(probitModelFinal)

## The Logit model is a bit more predictive


## -------------------------------------------------------------------------
##       PART 3: POISSON REGRESSION
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 8. Data loading block #####

bikes <- read.csv("data/hour.csv")
##### datos extraidos de https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

## -------------------------------------------------------------------------

##### 9. Dataset inspection #####

str(bikes)
head(bikes)
tail(bikes)
summary(bikes)

## Is cnt a Poisson variable? It could make sense, since it is the counting of bikes at different times. Also, we can see if the sd and the
## mean are simmilar

hist(log(bikes$cnt))

mean(bikes$cnt)
sd(bikes$cnt)

## -------------------------------------------------------------------------

##### 10. Formatting variables #####

bikes$season=as.factor(bikes$season)
bikes$yr=as.factor(bikes$yr)
bikes$mnth=as.factor(bikes$mnth)
bikes$hr=as.factor(bikes$hr)
bikes$holiday=as.factor(bikes$holiday)
bikes$weekday=as.factor(bikes$weekday)
bikes$workingday=as.factor(bikes$workingday)
bikes$weathersit=as.factor(bikes$weathersit)


## -------------------------------------------------------------------------

##### 11. Poisson regression #####

poissonModel <- glm(cnt~.-instant-dteday, family=poisson(link = "log"),data=bikes)
summary(poissonModel)

## -------------------------------------------------------------------------

##### 12. Automatic selection of variables #####

poissonModelFinal=step(poissonModel,direction="both",trace=1)
anova(poissonModel,poissonModelFinal)

## -------------------------------------------------------------------------

##### 13. Interpretation of variables #####

coef(poissonModelFinal)
exp(coef(poissonModelFinal))

## The interpretation is that, the probability is n times higher that the base case (in the case of discrete variables)

##### 14. Prediction #####

bikes2 <- bikes
bikes2$pred <- predict(poissonModelFinal,type="response")

## We can see that we are not predicting well. Why could it be? Maybe we are introducing variables colinear with each other

poissonModel=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=bicis)
summary(poissonModel)
poissonModelFinal=step(poissonModel,direction="both",trace=1)
anova(poissonModel,poissonModelFinal)

bikes$prediccion=predict(poissonModelFinal,type="response")

##### 15. Representation of the Poisson distribution #####

case=35 #35,
bikes[case,]

lambda=bikes$prediccion[case]
lambda

plot(dpois(1:120,lambda), type="l")
round(dpois(1:120,lambda),4)*100


## -------------------------------------------------------------------------
##       PART 4: PREDICTIVE MODELLING WITH LOGISTIC REGRESSION. INTRODUCING MACHINE LEARNING
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 16. Creation of test and training sets #####

set.seed(12345)
SAMPLE <- sample.split(bank, SplitRatio = 0.75)
bankTrain <- subset(bank, SAMPLE=TRUE)
bankTest <- subset(bank, SAMPLE=FALSE)

##### 17. Logistic regression training #####

modelLogitFinal <- step(glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome,data=bank,family=binomial('logit')),direction='both',trace=1)
summary(modelLogitFinal)

##### 18. Model evaluation #####


bankTrain$prediccion=predict(modelLogitFinal,type="response")
Predauxiliar= prediction(bankTrain$prediccion, bankTrain$y, label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloLogittrain = as.numeric(auc.tmp@y.values)
aucModeloLogittrain

ModelLogitTrain <- performance(Predauxiliar,"tpr","fpr")
plot(ModelLogitTrain,colorize=TRUE)
abline(a=0,b=1)

GINItrain=2*aucModeloLogittrain-1

bankTest$prediccion=predict(modelLogitFinal, newdata=bankTest,type="response")
Predauxiliar = prediction(bankTest$prediccion, bankTest$y, label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloLogittest = as.numeric(auc.tmp@y.values)
aucModeloLogittest

CurvaRocModeloLogitTest <- performance(Predauxiliar,"tpr","fpr")
plot(CurvaRocModeloLogitTest,colorize=TRUE)
abline(a=0,b=1)

## GINI Index
GINItest=2*aucModeloLogittest-1

## Model cap
mean(as.numeric(bankTest$y)-1)
aggregate(bankTest$prediccion~bankTest$y,FUN=mean)

## -------------------------------------------------------------------------

##### 19. Model implementation: Threshold #####

ALPHA=0.5
TP <- sum(bankTest$y=='yes' & bankTest$prediccion>=ALPHA)
TN <- sum(bankTest$y=='no' & bankTest$prediccion<ALPHA)
FP <- sum(bankTest$y=='no' & bankTest$prediccion>=ALPHA)
FN <- sum(bankTest$y=='yes' & bankTest$prediccion<ALPHA)
confusionMatrix <- table(bankTest$y,bankTest$prediccion>=ALPHA)
accuracyTest <- (TP+TN)/length(bankTest$y)
precisionTest <- TP/(TP+FP)
recallTest <- TP/(TP+FN)
confusionMatrix
accuracyTest
precisionTest
recallTest

ALPHA=0.2
TP <- sum(bankTest$y=='yes' & bankTest$prediccion>=ALPHA)
TN <- sum(bankTest$y=='no' & bankTest$prediccion<ALPHA)
FP <- sum(bankTest$y=='no' & bankTest$prediccion>=ALPHA)
FN <- sum(bankTest$y=='yes' & bankTest$prediccion<ALPHA)
confusionMatrix <- table(bankTest$y,bankTest$prediccion>=ALPHA)
accuracyTest <- (TP+TN)/length(bankTest$y)
precisionTest <- TP/(TP+FP)
recallTest <- TP/(TP+FN)
confusionMatrix
accuracyTest
precisionTest
recallTest


ALPHA=0.8
TP <- sum(bankTest$y=='yes' & bankTest$prediccion>=ALPHA)
TN <- sum(bankTest$y=='no' & bankTest$prediccion<ALPHA)
FP <- sum(bankTest$y=='no' & bankTest$prediccion>=ALPHA)
FN <- sum(bankTest$y=='yes' & bankTest$prediccion<ALPHA)
confusionMatrix <- table(bankTest$y,bankTest$prediccion>=ALPHA)
accuracyTest <- (TP+TN)/length(bankTest$y)
precisionTest <- TP/(TP+FP)
recallTest <- TP/(TP+FN)
confusionMatrix
accuracyTest
precisionTest
recallTest

## -------------------------------------------------------------------------

##### 19. Model implementation: KS test #####

bankKS=bankTest[order(bankTest$prediccion, decreasing=TRUE),c("y","prediccion")]
bankKS$N=1:length(bankKS$y)
bankKS$EXITOSACUM=cumsum(as.numeric(bankKS$y)-1)
bankKS$FRACASOSACUM=bankKS$N-bankKS$EXITOSACUM
bankKS$EXITOSTOT=sum(bankKS$y=="yes")
bankKS$FRACASOSTOT=sum(bankKS$y=="no")
bankKS$TOTAL=bankKS$EXITOSTOT+bankKS$FRACASOSTOT
bankKS$TPR=bankKS$EXITOSACUM/bankKS$EXITOSTOT
bankKS$FPR=bankKS$FRACASOSACUM/bankKS$FRACASOSTOT
bankKS$DIFF=bankKS$TPR-bankKS$FPR
plot(bankKS$DIFF)
max(bankKS$DIFF)
which(bankKS$DIFF==max(bankKS$DIFF))
bankKS[2125,]

plot(bankKS$prediccion*1000,1-bankKS$TPR,xlab="SCORE",ylab="Porcentaje acumulado",main="Distribuciones por Score (rojo malos, azul buenos)",type="l",col="blue")
lines(bankKS$prediccion*1000,1-bankKS$FPR,col="red")

## -------------------------------------------------------------------------

##### 19. Model implementation: F1 Score #####

bankKS$Accuracy=(bankKS$EXITOSACUM+bankKS$FRACASOSTOT-bankKS$FRACASOSACUM)/bankKS$TOTAL
bankKS$Precision=bankKS$EXITOSACUM/bankKS$N
bankKS$Cobertura=bankKS$EXITOSACUM/bankKS$EXITOSTOT
bankKS$F1Score=2*(bankKS$Precision*bankKS$Cobertura)/(bankKS$Precision+bankKS$Cobertura)
plot(bankKS$F1Score)
max(bankKS$F1Score)
which(bankKS$F1Score==max(bankKS$F1Score))
bankKS[1378,]

ALPHAS=seq(0,1,0.05)
Accuracy=c()
Precision=c()
Cobertura=c()
F1Score=c()
for (i in 1:length(ALPHAS)){
  ALPHA=ALPHAS[i]
  Confusion=table(bankKS$y,bankKS$prediccion>=ALPHA)
  Accuracy=c(Accuracy,(sum(bankKS$y=="yes" & bankKS$prediccion>=ALPHA)+sum(bankKS$y=="no" & bankKS$prediccion<ALPHA))/length(bankKS$y))
  Precision=c(Precision,sum(bankKS$y=="yes" & bankKS$prediccion>=ALPHA)/sum(bankKS$prediccion>=ALPHA))
  Cobertura=c(Cobertura,sum(bankKS$y=="yes" & bankKS$prediccion>=ALPHA)/sum(bankKS$y=="yes"))
}
F1Score=2*(Precision*Cobertura)/(Precision+Cobertura)
DFF1=data.frame(ALPHAS,Accuracy,Precision,Cobertura,F1Score)

DFF1




