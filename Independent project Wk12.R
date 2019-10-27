#Loading the imports
install.packages("Hmisc",repos = "http://cran.us.r-project.org")

install.packages("pacman")
library(pacman)
pacman::p_load(pacman, dplyr, ggplot2, rio, gridExtra, scales, ggcorrplot, caret, e1071)

#Load the dataset
adclick <- read.csv("./advertising.csv")


#Carrying out Exploratory Data Analysis

#Check the head of the dataset
head(adclick)

#Check the tail of the dataset
tail(adclick)

#check the dataframe
class(adclick)

#Check the length of the datasets
length(adclick)

#check the  variable types
str(adclick)

#Returning the number of row


#checking out the variables in the data 
see <- names(adclick)
see


# checking the summary
summary(adclick)

# Checking for Missing values
check <- sum(is.na(adclick))
check
#noted that there where no missing values identified

#Check for the number of duplicates
dups <- sum(duplicated(adclick))
dups
# reported that there were no duplicates found

#check for uniqueness and consistency
consistent <- unique(adclick)
consistent

#histogram
adclick <- rnorm(n=1000,m=80,sd=20)
hist(adclick,col = "orange")

#installing ggplot
install.packages("ggplot2")

library(ggplot2)
# Carrying out Univariate Analysis
ggplot(adclick, aes(x=Clicked.on.Ad)) + geom_bar()

#checking the percentage of ad clicks
prop.table(table(adclick$Clicked.on.Ad))
# this means shows that ourdata is balanced
#there is no form of bias

#Checking the adclick as per the gender
ggplot(adclick,aes(x=male,fill=Clicked.on.Ad)) +
theme_bw() +
goem_bar() +
lab(y="number of clicks",
title="clicks based on gender")

#Developing a boxplot for the age
boxplot(adclick$Area.Income,main="boxplot",ylab="male",col=5)
#Outliers where detected in the age column

#Univariate

#Age vs income
ggplot(adclick, aes(Age,Area.Income )) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Age") + ylab("Area Income")+ggtitle("Age vs Area Income")
# age 31 has the highest Area Income will age 60 has the lowest

#Visualizing the Age vs Internet usage
ggplot(adclick,color="red", aes(Age,Daily.Internet.Usage),color="red") + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "green")) + xlab("Age") + ylab("Daily Internet Usage")+ggtitle("Age vs Internet usage")
#People from ages 27 to 37 reported to be having the most internet usage



#Viewing the relationship between variables
ggplot(adclick,aes(Age,Clicked.on.Ad)) +
geom_point() +
theme_minimal() +
labs(title = "Relationship between Age and Gender")

#ChECKNG CORRELATIONS
cor(cars$speed, cars$dist)

#corr between age and clicked ad
cor(adclick$Age, adclick$Clicked.on.Ad)

#corr btw age and Daily Internet Usage
cor(adclick$Age,adclick$Daily.Internet.Usage)
# .....

#Imports
library(caret)
library(dplyr)
library(tidyverse)

Modelling
# round 1 
studentModel <- adclick(Clicked.on.Ad ~ ., data=adclick, method = "knn")
studentTestPred <- predict(studentModel, studentTest) 
confusionMatrix(studentTestPred,
studentTest$Clicked.on.Ad)$overall['Accuracy'] 
