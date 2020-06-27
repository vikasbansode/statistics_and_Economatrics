# Set working Directory

setwd("C:\\Users\\VB\\Desktop\\PROJECTS")

# Read Data

LungCapData <- read.csv("LungCapData.txt",header = T,sep = "\t")

# Attach data to R memory

attach(LungCapData)

# Explore Data

str(LungCapData)

# Check dimension of data

dim(LungCapData)

# Check lenght of Data

length(LungCapData)

# check head of data

head(LungCapData)

# Check tail of data

tail(LungCapData)

# Check column names

names(LungCapData)

# Check levels of categorical variables

levels(LungCapData$Smoke)

# Check class of variables

class(LungCapData$LungCap)

# Data Cleaning
# Trailing and Leadning spaces

LungCapData$Smoke <- trimws(LungCapData$Smoke,which = c("right"))
LungCapData$Smoke <- trimws(LungCapData$Smoke,which = c("left"))

# Handling punctuation marks
LungCapData$smoke <- gsub("[[:punct:]]|[[:digit:]]|(http[[:alpha:]]*:\\/\\/)","",LungCapData$Smoke)

# Detecting Missing values
library(mice)
md.pattern(LungCapData)

library(VIM)
md.pairs(LungCapData)

# Detect missing values using custom function
p <- function(x){sum(is.na(x))/length(x)*100}
apply(LungCapData,2,p)

# Detect missing values using apply function

apply(is.na(LungCapData),2,which)

# Exploratory Data Analysis
# 1. Univariate Analysis -to check the summary of the data to get rough idea about Data.
summary(LungCapData)

# Frequency Distribution -> to examine the outliers and significant trends are the relative abundance of each particular target data within dataset.

breaks <- seq(from=min(Age),to=max(Age),by=3)
pop <- cut(Age,breaks = breaks,right = TRUE,include.lowest = FALSE)
title <- (cbind(table(pop)))
colnames(title) <- c("frequency");title

# Create a table for proportion -> to examine the number of observations for a variables.

table(Smoke)

table(Smoke)/length(Smoke)

# Create two way table or contigency table

table(Smoke,Gender)

# Measures of Central Tendency -> Calculate the count of variable which is going to analyze

length(LungCap)
sum(LungCap)
mean(LungCap)
median(LungCap)

mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

mode(LungCap)

# Measures of Dispersions -> Calculate the Range of the LungCap

range(LungCap)
quantile(LungCap)
IQR(LungCap)
min(LungCap)
max(LungCap)
var(LungCap)
sd(LungCap)
sqrt(var(LungCap))
quantile(LungCap,probs = c(0.20,0.5,0.9,1))

# Measures of Shapes -> Measures of Shapes -** *Describe the distribution of the data within dataset.
# Calculate the skewness
library(e1071)
skewness(LungCap)

# Calculate the shapiro test to check normality

shapiro.test(LungCap)

# Plot histogram to check normality

hist(LungCap)

# Calculate the Kurtosis

kurtosis(LungCap)

# Plot Density plot

plot(density(LungCap))

# BIVARIATE ANALYSIS -> ivariate Analysis Deals with two sets of data. this paired data come from related sources or samples
# Correlation - is a parametric measure of the linear association between 2 numeric variables.

cor(LungCap,Height)
cor(LungCap,Age)

# **Covariance -** *measure of how much two random variables vary together.*

cov(LungCap,Age)/(sd(LungCap)*sd(Age))

# Correlation Matrix

cor(LungCapData[,1:3])

# Correlation matrix plot
library(corrplot)
corrplot(cor(LungCapData[,1:3]),method = 'pie')

# Pair Plot - way to visualize relationship between each variables. It produces a matrix of relationship between each variable for an instant examination of our data.
library(psych)
library(ggplot2)
pairs.panels(LungCapData[c(1:3)],gap=0,bg=c("red","yellow","blue")[LungCapData$Smoke],pch=21)

# Multivariate Analysis -> You try to understand a sense of relationship of all variables with one another.

aggregate(data.frame("LungCap"=LungCapData$LungCap,"Age"=LungCapData$Age,"Height(cm)"=LungCapData$Height),
          by=list(Smoke=LungCapData$Smoke), FUN=mean)

# VISUALIZATION
# Histogram -is a quick way to get information about a sample distribution without detaild statistical Analysis.

hist(LungCapData$LungCap)

# Barplot is appropriate for summarizing the distribution of a categorical variables.

count <- table(Gender); count

barplot(count,main = "Gender",xlab = "Gender",ylab = "%")

percentage <- table(Gender)/length(Gender)

# Adding titles to plot
barplot(percentage,main = "TITLE",xlab = "Gender",ylab = "%")

# Pie chart - is appropriate for summarizing the distribution of a categorical variables.

pie(count, main = "Gender")

# Boxplot - is appropriate for summarizing the distribution of a numerical variables.

boxplot(LungCap,main='Boxplot',ylab='Lung Capacity',ylim=c(0,16),las=1)

# Density plot - is appropriate for summarizing the distribution of a numerical variables.

d <- density(LungCap)
plot(d, main = 'Density')

# Stratified Boxplot- s useful for examining the relationship between a categorical variable and numerica variable with strata or groupe.

AgeGroups <- cut(Age,breaks = c(0,13,15,17,25),labels = c("<13","14/15","15/17","18"))
Age[1:5]
AgeGroups[1:5]

boxplot(LungCap,ylab='Lung Capacity',main="Boxplot of LungCap",las = 1)
boxplot(LungCap ~ Smoke,ylab = "Lung Capacity",main="LungCap vs Smoke",las = 1)
boxplot(LungCap[Age >= 18] ~ Smoke[Age >= 18],ylab = "Lung Capacity",main = "LungCap vs smoke, for 18+",las=1)
boxplot(LungCap ~ Smoke * AgeGroups,ylab = "Lung Capacity", main = "LungCap vs Smoke, by AgeGroup",las = 2)

# Steam and Leaf Plot - is appropriate for summarizing the distribution of a numberic variables and are most appropriate for smaller datasets.

femaleLungCap <- LungCap[Gender == "female"]
stem(femaleLungCap,scale = 2)
stem(femaleLungCap,scale = 2)

# Scatterplot - is appropriate for examining the relationship between 2 numerica variable.

plot(Age,Height,main = 'Scatterplot',xlab='AGE',ylab = 'HEIGHT',las=1,xlim = c(0,25),pch=8,col=2)



