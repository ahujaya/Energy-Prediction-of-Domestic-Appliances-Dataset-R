#1 (ii)
the.data <- as.matrix(read.table("Energy20.txt "))
#(iii)
set.seed(12)
my.data<-the.data[sample(1:671,320),c(1:6)]
View(my.data)

backup_data<-my.data
View(backup_data)

install.packages("rcompanion")
library(rcompanion)

#(iv) 
#Histograms

par(mfrow=c(2,3))

plotNormalHistogram(my.data[,1], main = "Histogram of Temperature in kitchen area",
                    xlab= "Temperature (in Celsius)", breaks=5, las=1, xlim=c(14,26), col="yellow")

plotNormalHistogram(my.data[,2], main = "Histogram of Humidity in kitchen area",
                    xlab= "Humidity (in Percentage)", breaks=5, las=1, xlim=c(25,55), col="yellow")

plotNormalHistogram(my.data[,3], main = "Histogram of Temperature outside (from weather station)",
                    xlab= "Temperature (in Celsius)", breaks=5, las=1, xlim=c(0,7),col="yellow")

plotNormalHistogram(my.data[,4], main = "Histogram of Humidity outside (from weather station)",
                    xlab= "Humidity (in Percentage)", breaks=5, las=1, xlim=c(60,100),col="yellow")

plotNormalHistogram(my.data[,5], main = "Histogram of Visibility (from weather station)",
                    xlab= "Visibility (in km)", breaks=5, las=1, xlim=c(10,70), col="yellow")

plotNormalHistogram(my.data[,6], main = "Histogram of Energy use of appliances",
                    xlab= "Energy (in Wh)", breaks=5, las=1,xlim=c(20,200),col="yellow")


#Scatterplots

par(mfrow=c(2,3))


plot(my.data[,6], my.data[,1], main="Scatter plot between Energy use and
     Temperature in kitchen area", xlab="Energy Use (in Wh)", 
     ylab="Temperature (in Celsius)", col=" dark red")


plot(my.data[,6], my.data[,2], main="Scatter plot between Energy use and
     Humidity in kitchen area", xlab="Energy Use (in Wh)", 
     ylab="Humidity (in Percentage)", col="dark red")

plot(my.data[,6], my.data[,3], main="Scatter plot between Energy use and
     Temperature outside", xlab="Energy Use (in Wh)", 
     ylab="Temperature (in Celsius)", col="dark red")

plot(my.data[,6], my.data[,4], main="Scatter plot between Energy use and
     Humidity outisde", xlab="Energy Use (in Wh)", 
     ylab="Humidity (in Percentage)", col="dark red")

plot(my.data[,6], my.data[,5], main="Scatter plot between Energy use and
     Visibility", xlab="Energy Use (in Wh)", 
     ylab="Visibility (in km)", col="dark red")

#2 i)

#Test for normality

for( i in 1:6) {
  pvalue_variables<-ks.test(jitter(my.data[,i]), "pnorm", mean=mean(my.data[,i]), sd=sd(my.data[,i]))
  print(pvalue_variables)
}

#One-sample Kolmogorov-Smirnov test 

#X1 p-value = 0.3659
#X2 p-value = 0.261
#X3 p-value = 0.06447
#X4 p-value = 0.6452
#X5 p-value = 0.04292
#Y p-value = 3.031e-13


#K-S test as sample size>50
#Since p-value for all variables except X5,Y is greater than 0.05, 
#we can say that there is no significant
#difference between the normal distribution and distribution of variables 
#X1,X2,X3,X4

#Since X1,X2,X3,X4 all are approximately normally distributed
#so need of log or polynomial transformation
#But X5 and Y variables are not approximately normally distributed as per K-S test
#so we will check their skewness and apply the transformation accordingly

install.packages("moments")
library(moments)

#Skewness has to be zero to be a perfect normal distribution
#between -0.5 and 0.5 we can approximate to a normal distribution
# if it's >0.5 --> positive skewed
#if it's <-0.5 --> negative skewed

S2<-skewness(my.data[,5])
S3<-skewness(my.data[,6])

print(S2) #0.7780151
print(S3) #1.574265

#Since skewness for X5 and Y variable is coming out to be >0.5,
#they are positively skewed
#Hence we will apply log transformation on X5 and Y variables


#Checking for negation transformation

pearson_results<-array(0,5)

for(i in 1:5){
  pearson_results[i]<-cor(my.data[,6], my.data[,i])
}

pearson_results

#0.29786526 0.14526831 0.43124855 0.05751533 0.34086755

puser1<-which(pearson_results==max(pearson_results))

puser1 

cat("The variable", puser1, "i.e. Temperature outside (from weather station) has 
    strongest relationship with variable of interest i.e. energy consumption
    based on pearson coefficient")


spearman_results<-array(0,5)

for(i in 1:5){
  spearman_results[i]<-cor(my.data[,6], my.data[,i])
}

spearman_results

#0.29786526 0.14526831 0.43124855 0.05751533 0.34086755

puser2<-which(spearman_results==max(spearman_results))

puser2 

cat("The variable", puser2, "i.e. Temperature outside (from weather station) has 
    strongest relationship with variable of interest i.e. energy consumption
    based on spearman coefficient")



#All positive so need of negation transformation

#Choosing 4 variables based on pearson and spearman results
#X1, X2, X3, X5 are chosen as X4 has weakest relationship with Y variable out of all the 5 variables

#Now applying Min-Max transformation on X1,X2,X3 
#and applying log and then Min-Max transformation on X5 and Y
#To make all these variables in the same range [0,1] 
#and approximately in same distribution

#applying log transformation on x5 and Y

log.fun<-function(x){
  result=log10(x)
  return(result)
}

my.data[,5]<-log.fun(my.data[,5])

log_transform_5<-my.data[,5]

View(log_transform_5)

my.data[,6]<-log.fun(my.data[,6])

log_transform_6<-my.data[,6]

View(log_transform_6)

#applying min-max transformation on all variables (X1,X2,X3,X5,Y)

minmax<-function(x){
  result=(x-min(x))/(max(x)-min(x))
  return(result)
}

my.data[,1]<-minmax(my.data[,1])

my.data[,2]<-minmax(my.data[,2])

my.data[,3]<-minmax(my.data[,3])

my.data[,5]<-minmax(my.data[,5])

my.data[,6]<-minmax(my.data[,6])

par(mfrow=c(2,3)) 

plotNormalHistogram(my.data[,1], main = "Histogram of Temperature in kitchen after Min-Max transformation",
                    xlab= "Temperature (in Celsius)",las=1,col="green")

plotNormalHistogram(my.data[,2], main = "Histogram of Humidity in kitchen area after Min-Max transformation",
                    xlab= "Humidity (in Percentage)", las=1,col="green")

plotNormalHistogram(my.data[,3], main = "Histogram of Temperature outside after Min-Max transformation",
                    xlab= "Temperature (in Celsius)", las=1, col="green")

plotNormalHistogram(my.data[,5], main = "Histogram of Visibility after Log and Min-Max transformation",
                    xlab= "Visibility (in km)", las=1,col="green")

plotNormalHistogram(my.data[,6], main = "Histogram of Energy use after Log and Min-Max Transformation",
                    xlab= "Energy (in Wh)", las=1, col="green")


View(my.data)

my.data<-my.data[,-4]

View(my.data)

write.table(my.data, "yash-transformed.txt")


#3 
#i)

install.packages("lpSolve")
library(lpSolve)

source("AggWaFit718.R")

#ii)

transformed.data<-as.matrix(read.table("yash-transformed.txt"))
View(transformed.data)

#WAM

fit.QAM(transformed.data, "WAMoutput.txt","WAMstats.txt")

#WPM 

PM01 <- function(x) {x^0.1}
invPM01 <-function(x) {x^(1/0.1)}

fit.QAM(transformed.data, "WPM(p=0.1)output.txt", "WPM(p=0.1)stats.txt", g=PM01, g.inv =invPM01)

PM10 <- function(x) {x^10}
invPM10 <-function(x) {x^(1/10)}

fit.QAM(transformed.data, "WPM(p=10)output.txt", "WPM(p=10)stats.txt", g=PM10, g.inv =invPM10)

#OWA

fit.OWA(transformed.data, "OWAoutput.txt", "OWAstats.txt")

#Choquet Integral

fit.choquet(transformed.data, "Choquetoutput.txt", "Choquetstats.txt")


#4
#i)

#Best fitting model is choquet integral as it has the lowest RMSE, Average Absolute error
#and highest pearson and spearman correlation coefficients

#Applying the choquet integral model on new data

#choosing X1, X2, X3 and X5 variables as chosen while training the model

new.data<-c(16,38,4,31)

#transforming the variables in the same way as in question 2

transform.new.data<-new.data

transform.new.data[1]<-(new.data[1]-min(backup_data[,1]))/(max(backup_data[,1])-min(backup_data[,1]))
transform.new.data[2]<-(new.data[2]-min(backup_data[,2]))/(max(backup_data[,2])-min(backup_data[,2]))
transform.new.data[3]<-(new.data[3]-min(backup_data[,3]))/(max(backup_data[,3])-min(backup_data[,3]))

transform.new.data[4]<-log.fun(new.data[4])

transform.new.data[4]<-(transform.new.data[4]-min(log_transform_5))/(max(log_transform_5)-min(log_transform_5))


transform.new.data

#applying the transformed variables on choquet integral model

Choquetweights<-c(0.3291697852352, 0, 0.59125895199465, 0.671701714812161, 0.763996068196678,
0.671701714812161, 0.76399606819668, 0.38553538041704, 0.591258951994704, 0.503344126141423, 
0.591258951994659, 0.671701714812135, 1, 0.671701714812729,1.00000000000057)

output<-choquet(transform.new.data, Choquetweights)

output

transform_output<-output*(max(log_transform_6)-min(log_transform_6))+min(log_transform_6)

transform_output

predicted_output<-10^(transform_output)

predicted_output #51.21951

#5 i)

#Applying linear regression model on the dataset used in question 3

View(transformed.data)

transformed.data.frame<-as.data.frame(transformed.data)

View(transformed.data.frame)

multi.fit<-lm(transformed.data.frame[,5]~transformed.data.frame[,1]+transformed.data.frame[,2]+transformed.data.frame[,3]+transformed.data.frame[,4],
              data = transformed.data.frame)

summary(multi.fit) 

#Visualizing the predicted Y values of both models on the 320 data 
#and compare them with the true Y values

#linear model

View(multi.fit)

linear.model.data<-as.matrix(multi.fit[["model"]])
View(linear.model.data)
linear.true.values<-linear.model.data[,1]
View(linear.true.values)

linear.predicted.values<-multi.fit[["fitted.values"]]
View(linear.predicted.values)

par(mfrow=c(1,1))

plot(linear.true.values, linear.predicted.values, main="Scatter plot between actual values and
     predicted values for linear model", xlab="Actual Values", 
     ylab="Predicted Values", col="dark blue" )

abline(lm(linear.true.values~linear.predicted.values), 
       col="black", lty=1)

#Fitting Choquet Integral Model

choquet.model.data<-as.matrix(read.table("Choquetoutput.txt"))
View(choquet.model.data)

choquet.true.values<-choquet.model.data[,5]
choquet.predicted.values<-choquet.model.data[,6]

par(mfrow=c(1,1))

plot(choquet.true.values, choquet.predicted.values , main="Scatter plot between actual values and
     predicted values for best fitting model (Choquet Integral)", xlab="Actual Values", 
     ylab="Predicted Values", col="dark blue" )

abline(lm(choquet.true.values~choquet.predicted.values), 
       col="black", lty=1)

#Visualizing the predicted values and actual values 
#for both the models together

par(mfrow=c(1,2))

plot(linear.true.values, linear.predicted.values, main="Scatter plot between actual values and
     predicted values for linear model", xlab="Actual Values", 
     ylab="Predicted Values", col="dark blue" )

abline(lm(linear.true.values~linear.predicted.values), 
       col="black", lty=1)

plot(choquet.true.values, choquet.predicted.values , main="Scatter plot between actual values and
     predicted values for best fitting model (Choquet Integral)", xlab="Actual Values", 
     ylab="Predicted Values", col="dark blue" )

abline(lm(choquet.true.values~choquet.predicted.values), 
       col="black", lty=1)



#Comparing the predicted values for both the models

par(mfrow=c(1,1))


plot(choquet.predicted.values, linear.predicted.values , 
main="Scatter plot between predicted values for both models (Choquet Integral and Linear Model)", xlab="Choquet Predicted Values", 
ylab="Linear Predicted Values", col="dark blue" )

abline(lm(choquet.predicted.values~linear.predicted.values), 
       col="black", lty=1)




