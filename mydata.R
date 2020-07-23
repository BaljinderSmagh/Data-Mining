getwd()
setwd("/Users/baljindersmagh/Desktop/Data Mining/HW1")
getwd()
#reading data file#
cleveland.data<-read.csv("processed.cleveland .data")
cleveland.data<-read.table("processed.cleveland .data",header = TRUE,sep = ",")
#calculating no. of people with heart disease#
count_heart_problem<-length(which(cleveland.data$heart_problem==1))
count_heart_problem
#calculating mean#
average_chol<-tapply(cleveland.data$chol,cleveland.data$heart_problem,mean)
average_chol
#calculating standard deviation#
standard_deviation<-tapply(cleveland.data$chol,cleveland.data$heart_problem,sd)
standard_deviation
#creating data with cholestrol more than 240#
mydata<-cleveland.data[cleveland.data$chol >240,]
mydata
#calculating mean of age for people with cholestrol >240#
mean_age<-mean(mydata$age)
mean_age
#caculating means of age for people with heart disease(or without disease) and cholestrol>240#
mean_heartproblem<-tapply(mydata$age,mydata$heart_problem,mean)
mean_heartproblem
#calculating median age for cholestrol higher than 240#
median_age<-median(mydata$age)
median_age
#calculating median of age for people with heartproblems and cholestrol higher than 240#
median_heartproblem<-tapply(mydata$age,mydata$heart_problem,median)
median_heartproblem
#histogram showing resting blood pressure
hist(cleveland.data$trestbps,main = paste("Histogram of Resting Blood Pressure"))

#box plot base on sex and cholestrol 
boxplot(cleveland.data$chol~cleveland.data$sex,col=c("blue","black"),xlab="Sex",ylab="Cholestrol",main="Plot of sex and chol")
#boxplot based on sex and maximum heart rate
boxplot(cleveland.data$thalach~cleveland.data$sex,xlab="sex",ylab="heartrate",main="Plot of sex and heart rate")
#H -Spread of cholestrol for male and female
H_Spread<-tapply(cleveland.data$chol,cleveland.data$sex,quantile)
H_Spread
#Lower Hinge and Upper Hinge values for maximum heart rate for male and female
Hinges<-tapply(cleveland.data$thalach,cleveland.data$sex,quantile,na.rm=TRUE)
Hinges
#Creating new data file "Data_1" for people with heart disease#
Data_1<-cleveland.data[cleveland.data$heart_problem==1,]

#creating datafile "Data_2" for people without heart disease#
Data_2<-cleveland.data[cleveland.data$heart_problem==0,]

#Plot of age and resting blood pressure for people with heart disease.
plot(Data_1$age~Data_1$thalach,ylab="Age",xlab="B.P",main="With Heart Disease")

#Plot of age and resting blood pressure for people without heart disease.
plot(Data_2$age~Data_2$thalach,ylab="Age",xlab="B.P",main="Without heart disease")

#visual correlation
abline(lm(Data_1$age~Data_1$thalach),col="red")
abline(lm(Data_2$age~Data_2$thalach),col="blue")

#Average resting blood pressure of each age for people with heart disease.
mean_bp<-tapply(Data_1$trestbps,Data_1$age,mean)
mean_bp

#Average resting blood pressure of each age for people without heart disease.
mean_bp_nh<-tapply(Data_2$trestbps,na.rm=TRUE,Data_2$age,mean)
mean_bp_nh

#scatter plot for Average Blood pressure and age for peolpe with heart disease
plot(mean_bp,xlab = "Age",ylab="Average B.P",main="Plot for heart disease")

#scatter plot for Average Blood pressure and age for peolpe with heart disease
plot(mean_bp_nh,xlab = "Age",ylab="Average B.P",main="Plot for no heart disease")


#Comparing the resting blood pressure of people with heart disease and without using summary parameters
compare<-tapply(cleveland.data$trestbps,is.na=TRUE,cleveland.data$heart_problem,summary)
compare
  
  
  
  
  
  



