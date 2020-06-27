# Two Variable Linear Model - useful for examining or modelling the relationship between 2 numeric variables.

# Model the relationship between Age and Lung Capacity

plot(Age,LungCap,main = "Scatterplot")


# Calculate the correlation

cor(Age,LungCap)

# Fit a Linear Model

mod <- lm(LungCap ~ Age)

# Model Evaluation
# Function that returns Root Mean Squared Error

rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error

mae <- function(error)
{
  mean(abs(error))
}

rmse(mod$residuals)/100

mae(mod$residuals)/100

summary(mod)

# Predict the Model

predict(mod, data.frame(Age = 16))

# Multiple Linear Regression - useful for modelling the relationship between more than 2 numeric variables.
# Fit model

model1 <- lm(LungCap ~ Age + Height)

# Summary

summary(model1)

# Calculate Pearson's correlation between Age and Height

cor(Age,Height,method = "pearson")

# ask for confidence intervals for the model coefficients

confint(model1,conf.level=0.95)

# MODEL DIAGNOSTIC

# Detect Outliers
library(car)
outlierTest(model1)
qqPlot(model1,main="QQPLOT") 
leveragePlots(model1)

# Detect the Multicollinarity 

vif(model1)  # Variable value should be less than 10 then we can conclude there is no Multicollinarity issue.

# Detect Heteroscadasticity

ncvTest(model1)  # P-value should be less than 0.05 then we can conclude there is no heteroscadasticity

library(lmtest)
bptest(model1)

# Detect Non-Linearity
library(corrplot)
plot(model1)

# Detect Autocorrelation

dwtest(model1) # p-value is < 2.0 hence we accept Ha, can conclude there is no autocorrelation in this model

durbinWatsonTest(model1)

acf(model1$residuals)

# we see all the vertical lines are within significance bounce except 0 and more or less line 4
# is crossing the significance bounce but not so high. wc can conclude there is no autocorrelation in the model.

bgtest(model1)  # p value is < 2.0 hence we accept Ha, can conclude there is no autocorrelation in this model

bgtest(model1,order = 2) # p value is < 2.0 hence we accept Ha, can conclude there is no autocorrelation in this model


# REGRESSION ASSUMPTIONS

# 1 Assumption : The Y-values (or the errors, "e") are independent!*
# 2 Assumption : The Y-values can be expressed as a linear function of the X variables*
# 3 Assumption : Variation of observations around the regression line (the residual SE is constant (homoscedasticity)*
# 4 Assumption : for given values of X,Y values (or the error) are Normally distributed*

plot(model1)


# LOGISTIC REGRESSION - is used to describe data and to explain the relationship between one dependent binary 
# variable and one or more nominal, ordinal, interval or ratio-level independent variables.

# Read Data

data <- read.csv("LungCapData.txt",header = T,sep = "\t")
str(data)

data <- data[c(1:4)]

# Normalize Data

data$LungCap <- scale(data$LungCap)
data$Age <- scale(data$Age)
data$Height <- scale(data$Height)

# Convert data to binary

levels(data$Smoke)=0:1
head(data)

# Check the class imbalance

table(data$Smoke)

# Data Partition

set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]


# Handling Class Imbalance

library(ROSE)

both <- ovun.sample(Smoke ~., data = train,method = 'both',p=0.5,seed=222,N=573)$data
table(both$Smoke)

over <- ovun.sample(Smoke ~.,data = train,method = 'over',N=1296)$data
table(over$Smoke)

under <- ovun.sample(Smoke ~., data = train,method = 'under',N=154)$data
table(under$Smoke)

# Build a Model

mymodel <- glm(Smoke ~ LungCap + Age + Height,data = both, family = 'binomial')

# Predict Model on Training Dataset

p <- predict(mymodel,both,type = 'response')
pred <- ifelse(p>0.5,1,0)
tab <- table(Predicted=pred,Actual=both$Smoke)
1-sum(diag(tab))/sum(tab)

# Predict model on test data

p1 <- predict(mymodel,test,type = 'response')
pred1 <- ifelse(p1 >0.5,1,0)
tab1 <- table(Predicted= pred1, Actual=test$Smoke)
1-sum(diag(tab1))/sum(tab1)

