## --------------------------
## Script : Testing the linear Regression 
## Author : Paulo Fernandez (based on the material and the data provided in the Statistical Modelling courses from Antonio Pita)
## --------------------------

## Part 1: Libraries loading

if (!require("dplyr")){
  install.packages("dplyr")
  library(dplyr)
}

if (!require("gap")){
  install.packages("gap")
  library(gap)
}

##---------------------------------------------

## Part 2: Data loading

sales <- read.csv2('./data/ventas.csv',stringsAsFactors = TRUE)

##---------------------------------------------


## Part 3: Data inspection

str(sales)
head(sales)
tail(sales)
summary(sales)

##---------------------------------------------


## Part 4. Data formatting

sales$Fecha <- as.Date(sales$Fecha)
sales$Producto <- factor(sales$Producto)

summary(sales)


##---------------------------------------------

## Part 5. Estimaton of sales according to the price (Basic econometric model)

modelPrice <- lm(Cantidad ~ Precio, data=sales)
summary(modelPrice)
confint(modelPrice,level = 0.95)

## There is clearly a negative relation between amount and price

plot(modelPrice$residuals)
hist(modelPrice$residuals)
qqnorm(modelPrice$residuals);qqline(modelPrice$residuals,col=2)
shapiro.test(modelPrice$residuals)

## Seems that the distribution of the residuals is normal, which means that the variance is explained by white noise, and there is no much more variables
## that can give information of the sales

##---------------------------------------------

## Part 6. Estimaton of semielasticity

## It does not make sense to assign this model, since it could arise negative quantities

modelPrice2 <- lm(log(Cantidad)~Precio,data=sales)
summary(modelPrice2)

plot(modelPrice2$residuals)
hist(modelPrice2$residuals)
qqnorm(modelPrice2$residuals);qqline(modelPrice$residuals,col=2)
shapiro.test(modelPrice2$residuals)

## What is happening?? Let's plot the data

plot(sales$Cantidad,sales$Precio)
abline(modelPrice,col='red')

## Obviously, there are two diferent sets (the products). Let's study the products separately

## Part 7. Structural changes

summary(sales)
modelA0143 <- sales %>% 
  filter(Producto=='A0143') %>% 
  lm(Cantidad~Precio,data=.)
summary(modelA0143)

modelA0351 <- sales %>% 
  filter(Producto=='A0351') %>% 
  lm(Cantidad~Precio,data=.)
summary(modelA0351)

plot(sales$Precio,sales$Cantidad)
abline(modelA0143,col='red')
abline(modelA0351,col='red')


## We are going to apply the Chow test to the two models

chow.test(sales$Cantidad[sales$Producto=="A0143"],sales$Precio[sales$Producto=="A0143"],sales$Cantidad[sales$Producto=="A0351"],sales$Precio[sales$Producto=="A0351"])

model_Chow=lm(Cantidad~Precio*Producto,data=sales)

summary(modelPrice)
summary(model_Chow)
