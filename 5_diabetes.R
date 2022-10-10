#Shivang Bajaj
#Roll No 5
#IT B-11
#This is a dataset that contains data from various diabetes reports which depends on various factors such as glucose,insulin,Blood pressure,BMI,etc. 
#The data was collected and made available by "National Institute of Diabetes and Digestive and Kidney Diseases" as part of the Pima Indians Diabetes Database.

#Importing required libraries
library(tidyverse)  #group of packages including ggplot2 and tidyr
library(ggplot2)    #creating graphics,mao variables to aesthetics
library(caret)      #Classification And REgression Training
library(corrplot)   #corrplot package is a graphical display of a correlation matrix
library(Amelia)     #to impute the missing values.
library(dplyr)      #makes data manipulation easier
library(tidyr)      #checks that the data is tidy
library(plot3D)     #for 2d and 3d plotting
library(GGally)     #It contains templates for different plots to be combined into a plot matrix
#import the data file using read.csv
df <- read.csv("./5_diabetes.csv")
#EDA stands for exploratory data analysis
#The class function returns class of data 
class(df)
#The dim function returns dimensions of the data i.e. no. of rows and cols
dim(df)
#we can also use individual nrow() and ncol() function to get no. of rows and cols
nrow(df)
ncol(df)
#The name() function is used to return the name of columns
names(df)
#head prints the top 6 rows of the dataset
head(df)
#displays last few rows of data using tail()
tail(df)
#str gives the detailed structure of the dataset
str(df)
#summary gives the in-depth view of each column.It prints the quantile of each column
summary(df)
#as.facctor is used to encode the vector as a factor indicationg categorical data
df$Outcome <- as.factor(df$Outcome)
str(df)
# Selecting all Numeric Variables
df_Numeric_Variable <- select_if(df, is.numeric)
df_Numeric_Variable


#check for any missing values 
any(is.na(df))  #checks if there is any NA value
any(is.null(df))   #checks if there is any missing value
#check for incorrect values
#gather() function will take multiple columns and collapse them into key-value pairs
#facet_wrap to make a quick set of histograms of each variable in data.frame.
ggplot(gather(df[,-9]),aes(value)) + geom_histogram() + facet_wrap(key~.,scales="free_x")

#Bloodpressure can't have zero value
#normal people
ind_n <-which(df$BloodPressure==0 & df$Outcome==0)
#diabetic people
ind_d <-which(df$BloodPressure==0 & df$Outcome==1)
#replace normal people  and diabetic person with respective means
v_n <- mean(df$BloodPressure[!df$BloodPressure==0 & df$Outcome==0])
v_d <- mean(df$BloodPressure[!df$BloodPressure==0 & df$Outcome==1])
df$BloodPressure[ind_n] <- v_n
df$BloodPressure[ind_d] <- v_d

#same goes for skin thickness ,BMI and insuline,glucose
#SkinThickness
ind_n <-which(df$SkinThickness==0 & df$Outcome==0)
ind_d <-which(df$SkinThickness==0 & df$Outcome==1)
v_n <- mean(df$SkinThickness[!df$SkinThickness==0 & df$Outcome==0])
v_d <- mean(df$SkinThickness[!df$SkinThickness==0 & df$Outcome==1])
df$SkinThickness[ind_n] <- v_n
df$SkinThickness[ind_d] <- v_d

#BMI
ind_n <-which(df$BMI==0 & df$Outcome==0)
ind_d <-which(df$BMI==0 & df$Outcome==1)
v_n <- mean(df$BMI[!df$BMI==0 & df$Outcome==0])
v_d <- mean(df$BMI[!df$BMI==0 & df$Outcome==1])
df$BMI[ind_n] <- v_n
df$BMI[ind_d] <- v_d

#insuline
ind_n <-which(df$Insulin==0 & df$Outcome==0)
ind_d <-which(df$Insulin==0 & df$Outcome==1)
v_n <- mean(df$Insulin[!df$Insulin==0 & df$Outcome==0])
v_d <- mean(df$Insulin[!df$Insulin==0 & df$Outcome==1])
df$Insulin[ind_n] <- v_n
df$Insulin[ind_d] <- v_d

#glucose
#ind_n <-which(df$Glucose==0 & df$Outcome==0)
#ind_d <-which(df$Glucose==0 & df$Outcome==1)
#v_n <- mean(df$Glucose[!df$Glucose==0 & df$Outcome==0])
#v_d <- mean(df$Glucose[!df$Glucose==0 & df$Outcome==1])
#df$Glucose[ind_n] <- v_n
#df$Glucose[ind_d] <- v_d
df$Glucose=ifelse(df$Glucose==0 & df$Outcome==0,mean(df$Glucose[!df$Glucose==0 & df$Outcome==0]),df$Glucose)
df$Glucose=ifelse(df$Glucose==0 & df$Outcome==1,mean(df$Glucose[!df$Glucose==0 & df$Outcome==1]),df$Glucose)

pairs(df[,-9],col=df$Outcome)
#ggplot2 library is used for plotting graphs from data to visualise it
#lets plot some graphs and see relations or patterns
#aes() is a quoting function. This means that its inputs are quoted to be evaluated in the context of the data.we pass the 2 attributes in aes
ggplot(df,aes(x=Age,y=Insulin))  + geom_bar(stat="identity")
ggplot(df,aes(x=Age,fill=Outcome))+geom_density(alpha=0.4)+scale_fill_manual(values=c("red", "blue"))+labs(title="Distribution of Age")
ggplot(df,aes(x=Glucose))+geom_histogram(fill="white",colour="black")+facet_grid(Outcome~.)
ggplot(df,aes(x=Pregnancies,fill=factor(Outcome)))+geom_bar(position="Dodge")+scale_fill_manual(values=c("red","blue"))+scale_x_continuous(limits=c(0,16))+labs(title="Pregnancies Vs Outcome")
ggplot(df,aes(x=Outcome,y=DiabetesPedigreeFunction,color=Outcome)) + geom_boxplot()
#At young age Diabetes pedigree function was high, as age goes on its get reduced
ggplot(df,aes(x=cut(Age,breaks=5),y=DiabetesPedigreeFunction,fill=cut(Age,breaks=5)))+geom_boxplot()


#Now the data should be ready for LR
#lets see the corrplot.corrplot is used for Correlation plotting
corrplot(cor(df[,-9]),method="number")
#Scatterplots of each pair of numeric variable are drawn on the left part of the figure. 
#Pearson correlation is displayed on the right. 
#Variable distribution is available on the diagonal.
ggpairs(df)



#test train split of the data
ind <- sample(2,nrow(df),replace=T,prob=c(0.75,0.25))
train <- df[ind==1,]
test <- df[ind==2,]
#Building a model from train data
#glm() is the function that tells R to run a generalized linear model. Inside the parentheses we give R important information about the model.
my_model <- glm(Outcome~.,data=train,family="binomial")
summary(my_model)
plot(my_model)
#Evaluation of the model
#Predicts values based on linear model object.
p1 <- predict(my_model,test,type="response")
p1 <- ifelse(p1>0.5,1,0)
#table() performs categorical tabulation of data with the variable and its frequency
tab1 <- table(predicted=p1,actual=test$Outcome)
tab1
acc <- (sum(diag(tab1))/sum(tab1))*100
q <- paste("Accuracy is ",acc)
print(q)
#the geom_smooth() function adds confidence bands on the smooth
ggplot(df,aes(x=DiabetesPedigreeFunction,y=Insulin)) + geom_smooth(method = glm) + geom_point(aes(color=Outcome))
ggplot(my_model,aes(x=BloodPressure,y=Insulin,color=BMI,size=Age)) + geom_point(alpha=0.5) + facet_wrap(Outcome~.) + ggtitle("Overall Report of our model Based on different factors")
