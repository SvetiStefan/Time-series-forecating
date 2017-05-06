#load the required libraries
library(lubridate)
library(zoo)
library(dplyr)
library(plyr)
library(raster)
library(forecast)
library(stats)
library(graphics)

#load the raw data 
globalsuperstore <- read.csv("Global Superstore.csv",stringsAsFactors = FALSE)
#check for correct structure and change as per requirement
globalsuperstore$Market <- as.factor(globalsuperstore$Market)
globalsuperstore$Segment <- as.factor(globalsuperstore$Segment)
globalsuperstore$Order.Date<- as.Date(globalsuperstore$Order.Date,format="%d-%m-%Y")
globalsuperstore$Order.Date <- as.yearmon(globalsuperstore$Order.Date)
globalsuperstore$Order.Date<- as.POSIXct(globalsuperstore$Order.Date,format="%d-%m-%Y")

#Function for calculating number of months from start to end of Time series
elapsed_months <- function(start_date, end_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  - (12 * (sd$year - ed$year) + (sd$mon - ed$mon))
}

#calculate the number of months from the start of analysis and 
#include it as a new attribute
globalsuperstore$Order.month <- as.numeric(elapsed_months("2010-12-01",globalsuperstore$Order.Date))
#Check the range of the new attribute
range(globalsuperstore$Order.month)
#Check the structure again for correctness
str(globalsuperstore)

################### (subset the data into different Markets and Segments)##########
#Subset the data into different segments
subset <- split(globalsuperstore,globalsuperstore$Segment,drop=TRUE)
Y <- lapply(seq_along(subset), function(x) as.data.frame(subset[[x]])[,-8]) 
names(Y) <- c("Consumer","Corporate","Home Office")
list2env(Y, envir = .GlobalEnv)

#subset the different segments into different markets
subset <- split(Consumer,Consumer$Market,drop=TRUE)
Y <- lapply(seq_along(subset), function(x) as.data.frame(subset[[x]])[,-12]) 
names(Y) <- c("Africa_consumer","APAC_consumer","Canada_consumer",
              "EMEA_consumer","EU_consumer","LATAM_consumer",
              "US_consumer")
list2env(Y, envir = .GlobalEnv)


subset <- split(Corporate,Corporate$Market,drop=TRUE)
Y <- lapply(seq_along(subset), function(x) as.data.frame(subset[[x]])[,-12]) 
names(Y) <- c("Africa_corporate","APAC_corporate","Canada_corporate",
              "EMEA_corporate","EU_corporate","LATAM_corporate",
              "US_corporate")
list2env(Y, envir = .GlobalEnv)


subset <- split(`Home Office`,`Home Office`$Market,drop=TRUE)
Y <- lapply(seq_along(subset), function(x) as.data.frame(subset[[x]])[,-12]) 
names(Y) <- c("Africa_HO","APAC_HO","Canada_HO",
              "EMEA_HO","EU_HO","LATAM_HO",
              "US_HO")
list2env(Y, envir = .GlobalEnv)

###############################################################################
#Aggregate the profits, sales and quantity for every month from 2011 to 2014
#This is done for all the 21 Market segments
#Calculate the coefficienct of variance of profits for all 21 Market segments

Africa_consumer_stats <- ddply(Africa_consumer,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                            "Quantity")]))

Africa_consumer_cv <- cv(Africa_consumer_stats$Profit)

Africa_corporate_stats <- ddply(Africa_corporate,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))

Africa_corporate_cv <- cv(Africa_corporate_stats$Profit)

Africa_HO_stats <- ddply(Africa_HO,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
Africa_HO_cv <- cv(Africa_HO_stats$Profit)

APAC_consumer_stats <- ddply(APAC_consumer,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
APAC_consumer_cv <- cv(APAC_consumer_stats$Profit)

APAC_corporate_stats <- ddply(APAC_corporate,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
APAC_corporate_cv <- cv(APAC_corporate_stats$Profit)

APAC_HO_stats <- ddply(APAC_HO,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
APAC_HO_cv <- cv(APAC_HO_stats$Profit)

Canada_consumer_stats <- ddply(Canada_consumer,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
Canada_consumer_cv <- cv(Canada_consumer_stats$Profit)

Canada_corporate_stats <- ddply(Canada_corporate,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
Canada_corporate_cv <- cv(Canada_corporate_stats$Profit)

Canada_HO_stats <- ddply(Canada_HO,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
Canada_HO_cv <- cv(Canada_HO_stats$Profit)


EMEA_consumer_stats <- ddply(EMEA_consumer,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
EMEA_consumer_cv <- cv(EMEA_consumer_stats$Profit)

EMEA_corporate_stats <- ddply(EMEA_corporate,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
EMEA_corporate_cv <- cv(EMEA_corporate_stats$Profit)

EMEA_HO_stats <- ddply(EMEA_HO,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
EMEA_HO_cv <- cv(EMEA_HO_stats$Profit)

EU_consumer_stats <- ddply(EU_consumer,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
EU_consumer_cv <- cv(EU_consumer_stats$Profit)

EU_corporate_stats <- ddply(EU_corporate,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
EU_corporate_cv <- cv(EU_corporate_stats$Profit)

EU_HO_stats <- ddply(EU_HO,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
EU_HO_cv <- cv(EU_HO_stats$Profit)

LATAM_consumer_stats <- ddply(LATAM_consumer,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
LATAM_consumer_cv <- cv(LATAM_consumer_stats$Profit)

LATAM_corporate_stats <- ddply(LATAM_corporate,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
LATAM_corporate_cv <- cv(LATAM_corporate_stats$Profit)

LATAM_HO_stats <- ddply(LATAM_HO,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
LATAM_HO_cv <- cv(LATAM_HO_stats$Profit)

US_consumer_stats <- ddply(US_consumer,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
US_consumer_cv <- cv(US_consumer_stats$Profit)

US_corporate_stats <- ddply(US_corporate,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                                    "Quantity")]))
US_corporate_cv <- cv(US_corporate_stats$Profit)

US_HO_stats <- ddply(US_HO,"Order.month", function(x) colSums(x[c("Profit", "Sales",
                                                                      "Quantity")]))
US_HO_cv <- cv(US_HO_stats$Profit)

#High CV means high volatility. So five datasets with low cv for profits is
#selected for analysis
# Five datasets with least co-efficient of variation of profits is as follows:-
#EU_consumer,APAC_consumer, APAC_corporate,
#LATAM_consumer,EU_corporate

#########################################################################################


#EU_consumer_sales

#Create a Time series for EU consumer sales
#plot the time series
EU_consumer_sales_ts <- ts(EU_consumer_stats$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_consumer_sales_ts, xlab="Time",ylab="Sales",bty="l")
#Smoothen the time series
Smoothed_EU_consumer_sales <- stats::filter(EU_consumer_sales_ts,
                                            filter=rep(1/4,4,method='convolution',
                                                       sides=2))
#create the dataframe from smoothed series and order month
EU_consumer_sales_df <- data.frame(cbind(EU_consumer_stats$Order.month,
                                           Smoothed_EU_consumer_sales))
#change the column names
colnames(EU_consumer_sales_df) <- c("month","sales")

#calculate the blank cell values 
diff_1 <- EU_consumer_sales_df$sales[3] - EU_consumer_sales_df$sales[2]
EU_consumer_sales_df$sales[1] <- EU_consumer_sales_df$sales[2]-diff_1

diff_2 <- EU_consumer_sales_df$sales[46] - EU_consumer_sales_df$sales[45]
EU_consumer_sales_df$sales[47] <- EU_consumer_sales_df$sales[46]+ diff_2

diff_3 <- EU_consumer_sales_df$sales[47] - EU_consumer_sales_df$sales[46]
EU_consumer_sales_df$sales[48] <- EU_consumer_sales_df$sales[47]+ diff_3

#plot the smoothed sales curve
lines(Smoothed_EU_consumer_sales,col='red',lwd=2)

#convert the smoothed curve into time series
EU_consumer_sales <- ts(EU_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                        end=c(2014,12))

#create windows for train and validation
nValid <- 6
nTrain <- length(EU_consumer_sales)-nValid
train.ts <- window(EU_consumer_sales,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_consumer_sales,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_EU_consumer_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

#Calculate MAPE and other performance metrics
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 12.74   
                                 
#Plot acf                                  
acf(train.lm.forecast$residuals,lag.max = 12)

#Model AR and plot it
train.res.ar <- arima(train.lm.forecast$residuals,order=c(4,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")
#check the summary of the AR model
summary(train.res.ar)

#plot the ACF of AR(4)
acf(train.res.arima.pred$residuals,lag.max = 12) #AR(4) is suitable
#calculate the accuracy of combined linear and AR model
EU_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
EU_consumer_combined_accuracy
#Accuracy of the model decreased. Mape is 12.21625

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=nValid)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 10.454
#ARIMA is better than manual model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#autoarima and plot it
autoarima_combined <- auto.arima(EU_consumer_sales)
future_forecast_EU_sales <- forecast(autoarima_combined,h=6,level=c(0.2,0.4,0.6,0.8))
plot(future_forecast_EU_sales,col="blue")
future_forecast_EU_sales

#Follow the same procedure for 
#EU_consumer_quantity

EU_consumer_quantity_ts <- ts(EU_consumer_stats$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_consumer_quantity_ts, xlab="Time",ylab="Quantity",bty="l")

Smoothed_EU_consumer_quantity <- stats::filter(EU_consumer_quantity_ts,
                                            filter=rep(1/3,3,method='convolution',
                                                       sides=2))

lines(Smoothed_EU_consumer_quantity,col='red',lwd=2)

EU_consumer_quantity_df <- data.frame(cbind(EU_consumer_stats$Order.month,
                                         Smoothed_EU_consumer_quantity))

colnames(EU_consumer_quantity_df) <- c("month","quantity")

diff_1 <- EU_consumer_quantity_df$quantity[3] - EU_consumer_quantity_df$quantity[2]
EU_consumer_quantity_df$quantity[1] <- EU_consumer_quantity_df$quantity[2]-diff_1

diff_2 <- EU_consumer_quantity_df$quantity[47] - EU_consumer_quantity_df$quantity[46]
EU_consumer_quantity_df$quantity[48] <- EU_consumer_quantity_df$quantity[47]+ diff_2


EU_consumer_quantity <- ts(EU_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                        end=c(2014,12))

nValid <- 6
nTrain <- length(EU_consumer_quantity)-nValid
train.ts <- window(EU_consumer_quantity,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_consumer_quantity,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_EU_consumer_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")



EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 16.515  


acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")

summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(2) is suitable
EU_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
EU_consumer_combined_accuracy
#Accuracy of the model decreased. Mape is 16.77275

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=nValid)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 19.288
#Manual model is better than autoarima model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#linear time series
train.lm.model <- tslm(EU_consumer_quantity~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="blue")

#Make a dataframe from forecasted sales and quantity
#Change the column names

EU_consumer_final_df <- data.frame(train.lm.total.forecast$mean,future_forecast_EU_sales$mean)
colnames(EU_consumer_final_df) <- c("quantity","sales")


###################################################################################
#Follow the above procedure for the 04 other market segments
####################################################################################

#APAC consumer

APAC_consumer_sales_ts <- ts(APAC_consumer_stats$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_consumer_sales_ts, xlab="Time",ylab="Sales",bty="l")

Smoothed_APAC_consumer_sales <- stats::filter(APAC_consumer_sales_ts,
                                            filter=rep(1/3,3,method='convolution',
                                                       sides=2))
lines(Smoothed_APAC_consumer_sales,col='red',lwd=2)

APAC_consumer_sales_df <- data.frame(cbind(APAC_consumer_stats$Order.month,
                                         Smoothed_APAC_consumer_sales))

colnames(APAC_consumer_sales_df) <- c("month","sales")

View(APAC_consumer_sales_df)

diff_1 <- APAC_consumer_sales_df$sales[3] - APAC_consumer_sales_df$sales[2]
APAC_consumer_sales_df$sales[1] <- APAC_consumer_sales_df$sales[2]-diff_1

diff_2 <- APAC_consumer_sales_df$sales[47] - APAC_consumer_sales_df$sales[46]
APAC_consumer_sales_df$sales[48] <- APAC_consumer_sales_df$sales[47]+ diff_2


lines(Smoothed_EU_consumer_sales,col='red',lwd=2)

APAC_consumer_sales <- ts(APAC_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                        end=c(2014,12))

nValid <- 6
nTrain <- length(APAC_consumer_sales)-nValid
train.ts <- window(APAC_consumer_sales,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_consumer_sales,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+I(sin(pi*trend))+I(cos(pi*trend))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_APAC_consumer_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 10.27
APAC_consumer_accuracy


acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")

summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(2) is suitable
APAC_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
APAC_consumer_combined_accuracy
#Accuracy of the model decreased. Mape is 10.37
#train.lm is better than combined model

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=nValid)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 25.373
#Manual model is better than ARIMA model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#Linear time series

train.lm.model <- tslm(APAC_consumer_sales~trend+I(sin(pi*trend))+I(cos(pi*trend))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="blue")


#APAC_consumer_quantity

APAC_consumer_quantity_ts <- ts(APAC_consumer_stats$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_consumer_quantity_ts, xlab="Time",ylab="Quantity",bty="l")

Smoothed_APAC_consumer_quantity <- stats::filter(APAC_consumer_quantity_ts,
                                               filter=rep(1/3,3,method='convolution',
                                                          sides=2))

lines(Smoothed_APAC_consumer_quantity,col='red',lwd=2)

APAC_consumer_quantity_df <- data.frame(cbind(APAC_consumer_stats$Order.month,
                                            Smoothed_APAC_consumer_quantity))

colnames(APAC_consumer_quantity_df) <- c("month","quantity")

View(APAC_consumer_quantity_df)

diff_1 <- APAC_consumer_quantity_df$quantity[3] - APAC_consumer_quantity_df$quantity[2]
APAC_consumer_quantity_df$quantity[1] <- APAC_consumer_quantity_df$quantity[2]-diff_1

diff_2 <- APAC_consumer_quantity_df$quantity[47] - APAC_consumer_quantity_df$quantity[46]
APAC_consumer_quantity_df$quantity[48] <- APAC_consumer_quantity_df$quantity[47]+ diff_2


APAC_consumer_quantity <- ts(APAC_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                           end=c(2014,12))

nValid <- 6
nTrain <- length(APAC_consumer_quantity)-nValid
train.ts <- window(APAC_consumer_quantity,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_consumer_quantity,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_APAC_consumer_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")


APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 15.678  
APAC_consumer_accuracy

acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")

summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(2) is suitable
APAC_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
APAC_consumer_combined_accuracy
#Accuracy of the model decreased. Mape is 16
#Linear model is better than combined model

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=nValid)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 16.783
#Manual model is better than autoarima model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#combined time series
train.lm.model <- tslm(APAC_consumer_quantity~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="blue")


APAC_consumer_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(APAC_consumer_final_df) <- c("sales","quantity")

################################################################################################################

#APAC corporate

APAC_corporate_sales_ts <- ts(APAC_corporate_stats$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_corporate_sales_ts, xlab="Time",ylab="Sales",bty="l")

Smoothed_APAC_corporate_sales <- stats::filter(APAC_corporate_sales_ts,
                                              filter=rep(1/3,3,method='convolution',
                                                         sides=2))
lines(Smoothed_APAC_corporate_sales,col='red',lwd=2)

APAC_corporate_sales_df <- data.frame(cbind(APAC_corporate_stats$Order.month,
                                           Smoothed_APAC_corporate_sales))

colnames(APAC_corporate_sales_df) <- c("month","sales")

View(APAC_corporate_sales_df)

diff_1 <- APAC_corporate_sales_df$sales[3] - APAC_corporate_sales_df$sales[2]
APAC_corporate_sales_df$sales[1] <- APAC_corporate_sales_df$sales[2]-diff_1

diff_2 <- APAC_corporate_sales_df$sales[47] - APAC_corporate_sales_df$sales[46]
APAC_corporate_sales_df$sales[48] <- APAC_corporate_sales_df$sales[47]+ diff_2


lines(Smoothed_APAC_corporate_sales,col='blue',lwd=2)

APAC_corporate_sales <- ts(APAC_corporate_sales_df$sales,frequency=12,start=c(2011,1),
                          end=c(2014,12))

nValid <- 6
nTrain <- length(APAC_corporate_sales)-nValid
train.ts <- window(APAC_corporate_sales,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_corporate_sales,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+season+I(sin(pi/18*trend)^2)+I(sin(pi/18*trend)^4))
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_APAC_corporate_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")

APAC_corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 6.4
APAC_corporate_accuracy


acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")

summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(2) is suitable
APAC_corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
APAC_corporate_combined_accuracy
#Accuracy of the model slightly increased. Mape is 7.931


autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_ts
autoarima_forecast <- forecast(autoarima_ts,h=6)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 15.672
#Manual model is better than ARIMA

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#manual model

train.lm.model <- tslm(APAC_corporate_sales~trend+season+I(sin(pi/18*trend)^2)+I(sin(pi/18*trend)^4))
summary(train.lm.model)
train.lm.total.forecast_sales <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_sales
plot(train.lm.total.forecast_sales,col="blue")


#APAC_corporate_quantity

APAC_corporate_quantity_ts <- ts(APAC_corporate_stats$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_corporate_quantity_ts, xlab="Time",ylab="Quantity",bty="l")

Smoothed_APAC_corporate_quantity <- stats::filter(APAC_corporate_quantity_ts,
                                                  filter=rep(1/3,3,method='convolution',
                                                             sides=2))

lines(Smoothed_APAC_corporate_quantity,col='red',lwd=2)

APAC_corporate_quantity_df <- data.frame(cbind(APAC_corporate_stats$Order.month,
                                               Smoothed_APAC_corporate_quantity))

colnames(APAC_corporate_quantity_df) <- c("month","quantity")

View(APAC_corporate_quantity_df)

diff_1 <- APAC_corporate_quantity_df$quantity[3] - APAC_corporate_quantity_df$quantity[2]
APAC_corporate_quantity_df$quantity[1] <- APAC_corporate_quantity_df$quantity[2]-diff_1

diff_2 <- APAC_corporate_quantity_df$quantity[47] - APAC_corporate_quantity_df$quantity[46]
APAC_corporate_quantity_df$quantity[48] <- APAC_corporate_quantity_df$quantity[47]+ diff_2


APAC_corporate_quantity <- ts(APAC_corporate_quantity_df$quantity,frequency=12,start=c(2011,1),
                            end=c(2014,12))

nValid <- 6
nTrain <- length(APAC_corporate_quantity)-nValid
train.ts <- window(APAC_corporate_quantity,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_corporate_quantity,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+I(sin(pi*trend/15))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_APAC_corporate_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")


APAC_corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 12.51 
APAC_corporate_accuracy

acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(1,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")
summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(1) is suitable
APAC_corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
APAC_corporate_combined_accuracy
#Accuracy of the model decreased. Mape is 13.01
#Linear model is better than combined model

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=nValid)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 10.746
#Autoarima is better than manual model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#Autoarima model
autoarima_model <- auto.arima(APAC_corporate_quantity)
APAC.corporate.total.forecast <- forecast(autoarima_model,h=6,level=c(0.2,0.4,0.6,0.8))
plot(APAC.corporate.total.forecast,col="blue")
APAC.corporate.total.forecast

APAC_corporate_final_df <- data.frame(APAC.corporate.total.forecast$mean,train.lm.total.forecast_sales$mean)
colnames(APAC_corporate_final_df) <- c("quantity","sales")


#########################################################################################

#LATAM Consumer
LATAM_consumer_sales_ts <- ts(LATAM_consumer_stats$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(LATAM_consumer_sales_ts, xlab="Time",ylab="Sales",bty="l")

Smoothed_LATAM_consumer_sales <- stats::filter(LATAM_consumer_sales_ts,
                                            filter=rep(1/3,3,method='convolution',
                                                       sides=2))

lines(Smoothed_LATAM_consumer_sales,col='red',lwd=2)

LATAM_consumer_sales_df <- data.frame(cbind(LATAM_consumer_stats$Order.month,
                                         Smoothed_LATAM_consumer_sales))

colnames(LATAM_consumer_sales_df) <- c("month","sales")

View(LATAM_consumer_sales_df)

diff_1 <- LATAM_consumer_sales_df$sales[3] - LATAM_consumer_sales_df$sales[2]
LATAM_consumer_sales_df$sales[1] <- LATAM_consumer_sales_df$sales[2]-diff_1

diff_2 <- LATAM_consumer_sales_df$sales[47] - LATAM_consumer_sales_df$sales[46]
LATAM_consumer_sales_df$sales[48] <- LATAM_consumer_sales_df$sales[47]+ diff_2


lines(Smoothed_LATAM_consumer_sales,col='red',lwd=2)

LATAM_consumer_sales <- ts(LATAM_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                        end=c(2014,12))

nValid <- 6
nTrain <- length(LATAM_consumer_sales)-nValid
train.ts <- window(LATAM_consumer_sales,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(LATAM_consumer_sales,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/3))+I(cos(2*pi*trend/3))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_LATAM_consumer_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

LATAM_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 21.87 
LATAM_consumer_accuracy


acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(4,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")

summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(4) is suitable
LATAM_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
LATAM_consumer_combined_accuracy
#Accuracy of the model increased. Mape is 20.8668

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=nValid)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 22.930
#Manual combined model is better than ARIMA model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#autoarima
train.lm.model <- tslm(LATAM_consumer_sales~trend+I(sin(2*pi*trend/3))+I(cos(2*pi*trend/3))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="blue")


#LATAM_consumer_quantity

LATAM_consumer_quantity_ts <- ts(LATAM_consumer_stats$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(LATAM_consumer_quantity_ts, xlab="Time",ylab="Quantity",bty="l")

Smoothed_LATAM_consumer_quantity <- stats::filter(LATAM_consumer_quantity_ts,
                                               filter=rep(1/3,3,method='convolution',
                                                          sides=2))

lines(Smoothed_LATAM_consumer_quantity,col='red',lwd=2)

LATAM_consumer_quantity_df <- data.frame(cbind(LATAM_consumer_stats$Order.month,
                                            Smoothed_LATAM_consumer_quantity))

colnames(LATAM_consumer_quantity_df) <- c("month","quantity")

diff_1 <- LATAM_consumer_quantity_df$quantity[3] - LATAM_consumer_quantity_df$quantity[2]
LATAM_consumer_quantity_df$quantity[1] <- LATAM_consumer_quantity_df$quantity[2]-diff_1

diff_2 <- LATAM_consumer_quantity_df$quantity[47] - LATAM_consumer_quantity_df$quantity[46]
LATAM_consumer_quantity_df$quantity[48] <- LATAM_consumer_quantity_df$quantity[47]+ diff_2

View(LATAM_consumer_quantity_df)

LATAM_consumer_quantity <- ts(LATAM_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                           end=c(2014,12))

nValid <- 6
nTrain <- length(LATAM_consumer_quantity)-nValid
train.ts <- window(LATAM_consumer_quantity,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(LATAM_consumer_quantity,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+I(sin(pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_LATAM_consumer_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")
LATAM_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 14.034  
LATAM_consumer_accuracy

acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(3,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")

summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(3) is suitable
LATAM_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
LATAM_consumer_combined_accuracy
#Accuracy of the model decreased. Mape is 14.07472

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=nValid)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 9.74
#ARIMA is better than manual model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#ARIMA time series
autoarima_model <- auto.arima(LATAM_consumer_quantity)
LATAM.consumer.total.forecast <- forecast(autoarima_model,h=6,level=c(0.2,0.4,0.6,0.8))
plot(LATAM.consumer.total.forecast,col="blue")
LATAM.consumer.total.forecast

LATAM_consumer_final_df <- data.frame(train.lm.total.forecast$mean,LATAM.consumer.total.forecast$mean)
colnames(LATAM_consumer_final_df) <- c("sales","quantity")

#####################################################################################

#EU corporate

EU_corporate_sales_ts <- ts(EU_corporate_stats$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_corporate_sales_ts, xlab="Time",ylab="Sales",bty="l")

Smoothed_EU_corporate_sales <- stats::filter(EU_corporate_sales_ts,
                                             filter=rep(1/3,3,method='convolution',
                                                        sides=2))
lines(Smoothed_EU_corporate_sales,col='red',lwd=2)

EU_corporate_sales_df <- data.frame(cbind(EU_corporate_stats$Order.month,
                                          Smoothed_EU_corporate_sales))

colnames(EU_corporate_sales_df) <- c("month","sales")

View(EU_corporate_sales_df)

diff_1 <- EU_corporate_sales_df$sales[3] - EU_corporate_sales_df$sales[2]
EU_corporate_sales_df$sales[1] <- EU_corporate_sales_df$sales[2]-diff_1

diff_2 <- EU_corporate_sales_df$sales[47] - EU_corporate_sales_df$sales[46]
EU_corporate_sales_df$sales[48] <- EU_corporate_sales_df$sales[47]+ diff_2


lines(Smoothed_EU_corporate_sales,col='blue',lwd=2)

EU_corporate_sales <- ts(EU_corporate_sales_df$sales,frequency=12,start=c(2011,1),
                         end=c(2014,12))

nValid <- 6
nTrain <- length(EU_corporate_sales)-nValid
train.ts <- window(EU_corporate_sales,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_corporate_sales,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+I(2*sin(pi/18*trend)^2)+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_EU_corporate_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")

EU_corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 20.93
EU_corporate_accuracy


acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(3,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")

summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(3) is suitable
EU_corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
EU_corporate_combined_accuracy
#Accuracy of the model slightly decreased. Mape is 21.133


autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_ts
autoarima_forecast <- forecast(autoarima_ts,h=6)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 18.574
#ARIMA is better than Manual model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#ARIMA model
autoarima_model <- auto.arima(EU_corporate_sales)
EU.corporate.total.forecast <- forecast(autoarima_model,h=6,level=c(0.2,0.4,0.6,0.8))
plot(EU.corporate.total.forecast,col="blue")
EU.corporate.total.forecast

#EU_corporate_quantity

EU_corporate_quantity_ts <- ts(EU_corporate_stats$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_corporate_quantity_ts, xlab="Time",ylab="Quantity",bty="l")

Smoothed_EU_corporate_quantity <- stats::filter(EU_corporate_quantity_ts,
                                                filter=rep(1/3,3,method='convolution',
                                                           sides=2))

lines(Smoothed_EU_corporate_quantity,col='red',lwd=2)

EU_corporate_quantity_df <- data.frame(cbind(EU_corporate_stats$Order.month,
                                             Smoothed_EU_corporate_quantity))

colnames(EU_corporate_quantity_df) <- c("month","quantity")

View(EU_corporate_quantity_df)

diff_1 <- EU_corporate_quantity_df$quantity[3] - EU_corporate_quantity_df$quantity[2]
EU_corporate_quantity_df$quantity[1] <- EU_corporate_quantity_df$quantity[2]-diff_1

diff_2 <- EU_corporate_quantity_df$quantity[47] - EU_corporate_quantity_df$quantity[46]
EU_corporate_quantity_df$quantity[48] <- EU_corporate_quantity_df$quantity[47]+ diff_2


EU_corporate_quantity <- ts(EU_corporate_quantity_df$quantity,frequency=12,start=c(2011,1),
                            end=c(2014,12))

nValid <- 6
nTrain <- length(EU_corporate_quantity)-nValid
train.ts <- window(EU_corporate_quantity,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_corporate_quantity,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+nValid))


train.lm <- tslm(train.ts~trend+I(2*sin(pi*trend/9))+I(2*sin(pi*trend/9)^2)+
                   +season)
summary(train.lm)

train.lm.forecast <- forecast(train.lm,h=nValid,level=0)
plot(Smoothed_EU_corporate_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")

EU_corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) #MAPE 18.576  
EU_corporate_accuracy

acf(train.lm.forecast$residuals,lag.max = 12)

train.res.ar <- arima(train.lm.forecast$residuals,order=c(1,0,0))
train.res.arima.pred <- forecast(train.res.ar,h=nValid)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred$fitted,lwd=2,col="blue")
summary(train.res.ar)

acf(train.res.arima.pred$residuals,lag.max = 12) #AR(2) is suitable
EU_corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred$mean),valid.ts)
EU_corporate_combined_accuracy
#Accuracy of the model increased. Mape is 19.642
#Linear model is better than combined model

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=nValid)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is 12.129
#Autoarima model is better than linear model

#Forecasting for the next six months.
#Forecast for the next 6 months will be made on the 
#time series

autoarima_model_quantity <- auto.arima(EU_corporate_quantity)
EU.corporate.quantity.forecast <- forecast(autoarima_model_quantity,h=6,level=c(0.2,0.4,0.6,0.8))
plot(EU.corporate.quantity.forecast,col="blue")
EU.corporate.quantity.forecast

EU_corporate_final_df <- data.frame(EU.corporate.total.forecast$mean,EU.corporate.quantity.forecast$mean)
colnames(EU_corporate_final_df) <- c("sales","quantity")













































