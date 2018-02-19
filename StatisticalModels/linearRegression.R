## --------------------------
## Script : Testing the linear Regression 
## Author : Paulo Fernandez (based on the material and the data provided in the Statistical Modelling courses from Antonio Pita)
## --------------------------

## Part 1: Libraries loading

## Part 2: Data loading

creditos <- read.csv("./data/creditos.csv")

## Part 3: Data inspection

str(creditos)
head(creditos)
summary(creditos)

## The data consists in a the income an the on-Balance for several clients. For each one, several characteristics are described. We would like to
## find the drivers of the income.

## There are outliers in the sample, mainly in the income (it could not be a DQ issue, since it just seems a high-income client). Probably, it will 
## be the same client with the 1809 of on-Balance. Let's check it:

creditos[creditos$Income == max(creditos$Income),]

## Indeed, is that client. It doesn't seem to be a data quality issue so we will keep it.

## Part 4: Data transformation

creditos$Gender <- factor(creditos$Gender)
creditos$Mortgage <- factor(creditos$Mortgage)
creditos$Married <- factor(creditos$Married)

summary(creditos)
str(creditos)

## Part 5: Linear regression modelling

modelGender <- lm(Income ~ Gender,data=creditos)
summary(modelGender)

## Median not exactly zero, it tells me that there may be additional variables that give info to the model
## Variable not statistically significant. I am not able to tell if it is random or a real explanation. Also, poor R^2

modelRating <- lm(Income~Rating, data=creditos)
summary(modelRating)

## Median at zero but not completely normal, but R^2 too low, it seems that it is needed additional variables
## Model fits, the variable Ratng is statistically significant

## Global regression

modelAll <- lm(Income~.,data=creditos)
summary(modelAll)
anova(modelRating,modelAll)

## Here, we can see that Balance, Mortgage and Rating are significant, but we need to prevent multicolinear effects

model1 <- lm(Income ~ Balance+Mortgage+Rating,data=creditos)
summary(model1)

## The model has almost all his explanatory power with these variables

## Part 6: Model analysis

plot(model1$residuals)
hist(model1$residuals)
qqnorm(model1$residuals)+qqline(model1$residuals)

## We can calculate the coefficients

model1$coefficients
confint(model1,level=0.95)

## In MCO, the residuals are anti-correlated with the variables
cor(model1$residuals,creditos$Rating)
cor(model1$residuals,creditos$Balance)

boxplot(model1$residuals~creditos$Mortgage)
aggregate(model1$residuals~creditos$Mortgage,FUN=mean)

shapiro.test(model1$residual)

## The Shapiro-Wilk test says that the residuals are already normally-distributed. But, let's compare the model with the one with all the variables

anova(model1,modelAll)

## As it can be seen, the models are not statistically different

## Part 7. Step-wise and backward regression

ModelBackward <- step(modelAll,direction = 'backward',trace=1)
summary(ModelBackward)
ModelAutoStepwise=step(modelAll,direction="both",trace=1)
summary(ModelAutoStepwise)
anova(modelAll,ModelAutoStepwise)
anova(modelAll,ModelBackward)

