rm(list=ls())
setwd("C:/Users/nevin/Downloads/Georgia Tech/Courses/RA_6414/Project/Data")

ridedata <- read.table(file = "rides_data.csv", sep=",",header=TRUE);
names(ridedata)
head(ridedata)
n=dim(ridedata)[1]

ridedata[,c(2,3,4,5,6,9)]=data.frame(lapply(ridedata[,c(2,3,4,5,6,9)], function(X) approxfun(1:n, X)(1:n)))
nrides.ts=ts(ridedata$nrides)
date = as.Date(ridedata$date, format="%m/%d/%Y")
month = as.factor(format(date,"%b"))
month=factor(month,levels = month.abb)
ridedata['month']=month
ridedata$day <- ordered(ridedata$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                        "Friday", "Saturday", "Sunday"))
ridedata['timept']=(ridedata['timept'] - min(ridedata['timept']))/max(ridedata['timept'])


### EDA

plot(ridedata$nrides)
plot(ts(ridedata[,c(2,3,4,5,6,9)]))
#splom
library(lattice)
splom(ridedata[,-c(1,7,8,10,11)])
#correlation
cor(ridedata[,-c(1,7,8,10,11)],use = "complete.obs")

# Box plots
# Weekday/weekend and monthly seasonality is evident from these
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
boxplot(nrides ~ weekend, data = ridedata, xlab = "Weekend Flag",
        ylab = "No of rides", main = "Rides Data")
boxplot(ridedata$nrides ~ ridedata$day, xlab = "Weekday",
       ylab = "No of rides", main = "Rides Data")
boxplot(ridedata$nrides ~ month, xlab = "Month",
        ylab = "No of rides", main = "Rides Data")

## train test split
ridetrain=ridedata[1:(n-365),]
ridetest=ridedata[(n-364):n,]


### Modelling

# Linear Regression
lr1<-lm(nrides~.,data=ridetrain[,-c(1)])
summary(lr1)
# Residual Analysis
par(mfrow = c(2,2))
plot(lr1) ## 333 is an outlier because of the tornado event

ridetrain<-ridetrain[-333,] #removing the outlier
lr1<-lm(nrides~.,data=ridetrain[,-c(1)])
summary(lr1)

# ANOVA
# including weekday instead of weekend flag is found to have given better results
# lr2<-lm(nrides~.,data=ridetrain[,-c(1,8)])
# summary(lr2)
# anova(lr1,lr2)

lr3<-lm(nrides~.,data=ridetrain[,-c(1,8,9)])
summary(lr3) # Adjusted R-squared:  0.8258 
anova(lr3,lr1)
# The p-value greater than 0.05 indicates that LD+weekend doesn't 
# add significant predictive power

# Forecasting
dev.off()
pred_rides = predict(lr3,ridetest[,-c(1)])
ts.plot(nrides.ts,ylab="No of rides")
lines(c((n-364):n),pred_rides,lwd=2,col="green")
err=sqrt(mean((pred_rides-ridetest$nrides)^2)) ##rmse: 3203.249
print(err)


### Linear regression with the best subset model 
library(leaps);
lr.leaps <- regsubsets(nrides ~ ., data=ridetrain[,-c(1,8,9)], nbest= 100, really.big= TRUE); 
lr.leaps.models <- summary(lr.leaps)$which;
lr.leaps.models.size <- as.numeric(attr(lr.leaps.models, "dimnames")[[1]]);
lr.leaps.models.rss <- summary(lr.leaps)$rss;
plot(lr.leaps.models.size, lr.leaps.models.rss); 
#model with all the 8 variableas are the best

### Ridge regression
library(MASS)
lr.ridge <- lm.ridge(nrides ~ ., data=ridetrain[,-c(1,8,9)], lambda= seq(0.0,1,10000));
plot(lr.ridge) 
select(lr.ridge)
lambdaopt <- which.min(lr.ridge$GCV); 
# tried different values but the best performance was for 0

## Using Box-Cox Transformation 
library(MASS)
boxcox(lr3)
boxcox(lr3, lambda=seq(0,1,0.01))

lr3.tfmd<-lm(sqrt(nrides)~.,data=ridetrain[,-c(1,8,9)])
summary(lr3.tfmd) # Adjusted R-squared:  0.8395 
par(mfrow=c(2,2))
plot(lr3.tfmd)
err=sqrt(mean((predict(lr3.tfmd,ridetest[,-c(1,8,9)])^2-ridetest$nrides)^2))
print(err)  ##rmse: 2850.121 # reduction in rmse observed
# forecast
dev.off()
ts.plot(nrides.ts,ylab="No of rides")
lines(c((n-364):n),predict(lr3.tfmd,ridetest[,-c(1,8,9)])^2,lwd=2,col="green")

# lr3.tfmd2<-lm(nrides^0.6~.,data=ridetrain[,-c(1,8,9)])
# summary(lr3.tfmd2) # Adjusted R-squared:  0.8395
# err=sqrt(mean((predict(lr3.tfmd2,ridetest[,-c(1,8,9)])^(1/0.6)-ridetest$nrides)^2))
# print(err) ##rmse: 2887.57


## Poisson Regression
pr.model <- glm(nrides ~. , family = poisson(), data=ridetest[,-c(1,8,9)]); 
summary(pr.model)
err=sqrt(mean((predict(pr.model,ridetest[,-c(1,8,9)])-ridetest$nrides)^2))
print(err)  ##rmse: 2


## Time Series Modelling with nrides at (t-1) and (t-7) as variables

# Trend+Seasonality Estimation
lr.ts<-lm(nrides~timept+day+month, data=ridetrain)
summary(lr.ts)
nrides_res=resid(lr.ts)

library(tseries)
adf.test(nrides_res) #checking stationarity
# ndiffs(nrides_res)

# Arima modelling on residuals
library(forecast)
arima1<-arima(nrides_res, order=c(1,0,0), xreg=ridetrain[,-c(1,2,6,8,9,7,10,11)], 
              seasonal=list(order=c(1,0,0), period=7))
summary(arima1)
pred_rides=predict(arima1,n.ahead=365,newxreg=ridetest[,-c(1,2,6,8,9,7,10,11)])$pred
pred_rides=pred_rides+predict(lr.ts,ridetest)
# dev.off()
ts.plot(nrides.ts,ylab="No of rides")
lines(c((n-364):n),pred_rides,lwd=2,col="green")
err=sqrt(mean((pred_rides-ridetest$nrides)^2))
print(err)  ##rmse (AR 1): 3154.85, (AR 7): 3063.76

