###Loading all the relevant libraries 
library(dplyr)
library(tidyr)
library(reshape) # used for cast function
require(graphics)
library(forecast)
library(tseries)
library(Amelia)
library(ggplot2)

##Loading the data file in the R environment 
superstoredata <- read.csv("Global Superstore.csv", header = T, stringsAsFactors = F)

## UNDERSTANDING THE DATA 
str(superstoredata)
summary(superstoredata)

##Data at a glance, tells that Order and Shipping date are character - should be as date format

superstoredata$Order.Date <- as.Date(superstoredata$Order.Date, format = "%d-%m-%Y")
superstoredata$Ship.Date <- as.Date(superstoredata$Ship.Date, format = "%d-%m-%Y")

######## Deriving additional date columns for further Granularity and data processing #########

superstoredata$month_year<-format(superstoredata$Order.Date,"%Y-%m")
superstoredata$year<-format(superstoredata$Order.Date,"%Y")

View(superstoredata)


##### Calculating No of days between order placed and shipped, as it may be a useful metric #####
superstoredata$ship_days <- as.numeric(superstoredata$Ship.Date - superstoredata$Order.Date)

##### checking for duplicate rows #####
sum(duplicated(superstoredata)) #no duplicate found  


##### CHECKING FOR MISSING VALUES #####
missmap(superstoredata, main = "Missing values in DF vs observed")

## Postal Code seems to have all missing values compared to the rest of the data frame
## A more granular check can help determine the number

sapply(superstoredata, function(x) sum(is.na(x)))

## Total number of NA's in Postal Code = 41296

(41296 / nrow(superstoredata))*100

## Close to 80% of observations in postal code variable are missing Hence Postal code cannot be helpful with the Analysis.
## We will proceed with removing this variable before moving on with the Analysis.

superstoredata$Postal.Code <- NULL

## Checking and confirming once more if missing values are taken care of.

sapply(superstoredata, function(x) sum(is.na(x)))

###Since the time series is to be built using Order Date. Removing shipping date will not affect the analysis.
## Also Removing Customer name and product name too as Customer ID and Product ID would be sufficient for any further analysis if required.

superstoredata <- superstoredata[, -c(4,7,17)]
str(superstoredata)
View(superstoredata)


################################################################################################3

############## GRANULAR DATA UNDERSTANDING #################

ggplot(superstoredata, aes(x=Segment))+ geom_bar()
#Consumer os biggest segment followed by corporate and then home office.
ggplot(superstoredata, aes(x=Market,fill=Segment))+ geom_bar()
#Canada is very small market in terms of number of orders
ggplot(superstoredata, aes(x=Category,fill=Sub.Category))+ geom_bar()
#More number of orders are from category Office supplies

#########################################################

##### PREPARING ADDITIONAL DATA FERAMES FOR FURTHER ANALYSIS #####

store_sales <- superstoredata[,-c(3,16,18)]
store_quantity <- superstoredata[,-c(3,15,18)]
store_profit <- superstoredata[,-c(3,15,16)]
str(store_sales)
str(store_quantity)
str(store_profit)



#### Preparation of Data Sets for Calculation of Coeffcient of Variation by Market and Segment for Monthly Profit ####

monthlyprofit_sum <- aggregate(store_profit$Profit,list( store_profit$Market, store_profit$Segment, store_profit$month_year), FUN =(sum))
View(monthlyprofit_sum)

monthlyprofit_mean <- aggregate(store_profit$Profit,list( store_profit$Market, store_profit$Segment, store_profit$month_year), FUN =(mean))
View(monthlyprofit_mean)

monthlyprofit_sd <- aggregate(store_profit$Profit,list( store_profit$Market, store_profit$Segment,store_profit$month_year), FUN =(sd))
View(monthlyprofit_sd)

### It may be noticed that for Canada Market several month-year combinations have NA values. This is only happening because 
### for that combination of Market, Month-year & Consumer segment there was no more than ONLY 1 Sale.
### which leads to NA values in respective derived SD values.
### For the purpose of our Analysis we will impute these missing values in SD column with "0" later

### Checking the number of missing values before imputation.

sapply(monthlyprofit_sd, function(x) sum(is.na(x)))  ### 25 missing / NA values are observed.
monthlyprofit_sd[is.na(monthlyprofit_sd)] <- 0

### Checking if all missing values have been imputed
sapply(monthlyprofit_sd, function(x) sum(is.na(x)))

### All values are "0" hence no further action required and moving on.

market_segment_group <- group_by(store_profit, Market, Segment, month_year)
sd <- summarise(market_segment_group, sd(Profit, na.rm = T))

### Similar imputation of 0 value required for this new database.

sd[is.na(sd)] <- 0
View(sd)


####### MERGING DERIVED DATA SETS TO COMPILE A FINAL DATA SET FOR ANALYSIS #######

monthlyprofit <- merge(monthlyprofit_sum, monthlyprofit_mean, by = c("Group.1","Group.2","Group.3"))
monthlyprofit <- merge(monthlyprofit, monthlyprofit_sd, by = c("Group.1","Group.2","Group.3"))

### Renaming Columns values for Better readibility ###

names(monthlyprofit) <- c("Market", "Segment","Month_Year" ,"Total profit","Mean Profit", "SD")
View(monthlyprofit)

### Calculating Coefficient of variance ###

monthlyprofit$CV <- monthlyprofit$SD/monthlyprofit$`Mean Profit`

View(monthlyprofit)
monthlyprofit$Market_segment <- paste(monthlyprofit$Market,monthlyprofit$Segment,sep="_")
monthlyprofit <- monthlyprofit[,-c(1,2)]

#plotting to know best performance
monthlyprofit$Market_segment <- as.factor(monthlyprofit$Market_segment)
levels(monthlyprofit$Market_segment)

View(monthlyprofit)
str(monthlyprofit)

### This structure shows that there are total 21 buckets / factors within which all observations lie.


### Plotting to know the best market-segment combination by total profit

monthlyprofit_ts <- monthlyprofit[,c(1,6,2)]
View(monthlyprofit_ts)
monthlyprofit_ts <- cast(monthlyprofit_ts, Month_Year ~ Market_segment)
View(monthlyprofit_ts)
summary(monthlyprofit_ts)
boxplot(monthlyprofit_ts[,-1], las = 2, par(mar = c(9.5, 3.5, 2, 0.5)+ 0.01))

### Plotting to know the best market-segment combination by Coefficient of Variance
monthlycv_ts <- monthlyprofit[,c(1,6,5)]
View(monthlycv_ts)
monthlycv_ts <- cast(monthlycv_ts, Month_Year ~ Market_segment)
View(monthlycv_ts)
summary(monthlycv_ts)
boxplot(monthlycv_ts[,-1], ylim = c(-50,50), las = 2)

#chosing box plot as we want to find out most consistent performing segments
#after looking at both the boxplot graphs APAC consumer and EU consumer seems to be performing consistently.
#They might not be the best but the results are consistent and stable.

# Verifying the result little differently
# Identifying the market segments which has earned maximum profits with least deviation/ CV
# Arrange descending order of profit and ascending order of cv

monthlyprofit_order <- monthlyprofit[order(-monthlyprofit[,2],monthlyprofit[,5]),]
View(monthlyprofit_order)
table(monthlyprofit_order[c(1:100),]$Market_segment) 
### checking what's appearing most in top 100 rows
### Result is same as box plot approach - APAC Consumer and EU consumer.
### Based on these values we can be sure that our chosen Market Segment combination is the best out of all.

#********************************************************
# Forecasting the sales and quantity for the next 6 months for 2 most profitable segments

#********************************************************
#Subsetting sales and quantity data for APAC and EU market and Consumer Segment

monthlysale <- aggregate(store_sales$Sales,list( store_sales$month_year,store_sales$Market, store_sales$Segment), FUN =(sum))
names(monthlysale) <- c("Month_Year", "Market", "Segment","Total_sale")
View(monthlysale)
Sale_APAC_consumer <- subset(monthlysale, Market == "APAC" & Segment == "Consumer")
View(Sale_APAC_consumer)
Sale_EU_consumer <- subset(monthlysale, Market == "EU" & Segment == "Consumer")
View(Sale_EU_consumer)

monthlyquantity <- aggregate(store_quantity$Quantity,list(store_quantity$month_year ,store_quantity$Market, store_quantity$Segment), FUN =(sum))
names(monthlyquantity) <- c("Month_Year", "Market", "Segment","Total_quantity")
View(monthlyquantity)
Quant_APAC_consumer <- subset(monthlyquantity, Market == "APAC" & Segment == "Consumer")
View(Quant_APAC_consumer)
Quant_EU_consumer <- subset(monthlyquantity, Market == "EU" & Segment == "Consumer")
View(Quant_EU_consumer)

###################################################################################################
##### After obtaining 4 sets for Forecasting, creating test and training data for timeseries


Test_APAC_Sale <- subset(Sale_APAC_consumer, Month_Year < "2014-07")[,c(1,4)]
Train_APAC_Sale <- subset(Sale_APAC_consumer, Month_Year > "2014-06")[,c(1,4)]
Test_EU_Sale <- subset(Sale_EU_consumer, Month_Year < "2014-07")[,c(1,4)]
Train_EU_Sale <- subset(Sale_EU_consumer, Month_Year > "2014-06")[,c(1,4)]

Test_APAC_Qty <- subset(Quant_APAC_consumer, Month_Year < "2014-07")[,c(1,4)]
Train_APAC_Qty <- subset(Quant_APAC_consumer, Month_Year > "2014-06")[,c(1,4)]
Test_EU_Qty <- subset(Quant_EU_consumer, Month_Year < "2014-07")[,c(1,4)]
Train_EU_Qty <- subset(Quant_EU_consumer, Month_Year > "2014-06")[,c(1,4)]

nrow(Test_APAC_Qty)
nrow(Test_APAC_Sale)
nrow(Test_EU_Qty)
nrow(Test_EU_Sale)

###################### Modeling for APAC Sales ############################

Test_APAC_Sale$Month_Year <- c(1:nrow(Test_APAC_Sale))
View(Test_APAC_Sale)
Train_APAC_Sale$Month_Year <- c((nrow(Test_APAC_Sale)+1):(nrow(Test_APAC_Sale)+nrow(Train_APAC_Sale)))
View(Train_APAC_Sale)
Sale_APAC_consumer_ts<- Sale_APAC_consumer
Sale_APAC_consumer_ts$Month_Year <- c(1:nrow(Sale_APAC_consumer))


nrow(Test_APAC_Qty)
nrow(Train_APAC_Sale)
nrow(Test_APAC_Sale)
nrow(Test_EU_Qty)
nrow(Test_EU_Sale)

timeser <- ts(Test_APAC_Sale$Total_sale)
total_timeser <- ts(Sale_APAC_consumer_ts$Total_sale)
plot(timeser)

##### Smoothing the series - Moving Average Smoothing

w <- 1
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


##### Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

##### Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

##### Plot the smoothed time series

timevals_in <- Test_APAC_Sale$Month_Year
lines(smoothedseries, col="blue", lwd=2)

##### Building a model on the smoothed time series using classical decomposition
##### First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')
View(smootheddf)
nrow(smootheddf)
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
global_pred
lines(timevals_in, global_pred, col='red', lwd=2)

##### Now, let's look at the locally predictable series
##### We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

##### We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi) # p-value = .1, thus the null hyphothesis that the series is stationary is true

### Now, let's evaluate the model using MAPE
### First, let's make a prediction for the last 6 months

outdata <- Train_APAC_Sale
View(outdata)
names(outdata) <- c("Month", "Total_sale")
timevals_out <- outdata$Month
timevals_out

global_pred_out <- predict(lmfit, data.frame(Month = timevals_out))


fcast <- global_pred_out
fcast

### Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec ### 31.07429

### Let's also plot the predictions along with original values, to
### get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
class_dec_pred
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

### So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

### Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

### Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima ### 27.68952

### Lastly, let's plot the predictions along with original values, to
### get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")


######################## Modeling for APAC Quantity##########################

Test_APAC_Qty$Month_Year <- c(1:nrow(Test_APAC_Qty))
View(Test_APAC_Qty)
Train_APAC_Qty$Month_Year <- c((nrow(Test_APAC_Qty)+1):(nrow(Test_APAC_Qty)+nrow(Train_APAC_Qty)))
View(Train_APAC_Qty)
Quant_APAC_consumer_ts<- Quant_APAC_consumer
Quant_APAC_consumer_ts$Month_Year <- c(1:nrow(Quant_APAC_consumer))


nrow(Test_APAC_Qty)
nrow(Train_APAC_Qty)



timeser <- ts(Test_APAC_Qty$Total_quantity)
total_timeser <- ts(Quant_APAC_consumer_ts$Total_quantity)
plot(timeser)

### Smoothing the series - Moving Average Smoothing

w <- 1
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


### Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

### Smoothing right end of the time series
n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

### Plot the smoothed time series
timevals_in <- Test_APAC_Qty$Month_Year
lines(smoothedseries, col="blue", lwd=2)

### Building a model on the smoothed time series using classical decomposition
### First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Qty')
View(smootheddf)
nrow(smootheddf)
lmfit <- lm(Qty ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
global_pred
lines(timevals_in, global_pred, col='red', lwd=2)

### Now, let's look at the locally predictable series
### We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

### We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi) # p-value = .1, thus the null hyphothesis that the series is stationary is true

### Now, let's evaluate the model using MAPE
### First, let's make a prediction for the last 6 months

outdata <- Train_APAC_Qty
View(outdata)
names(outdata) <- c("Month", "Total_qty")
timevals_out <- outdata$Month
timevals_out

global_pred_out <- predict(lmfit, data.frame(Month = timevals_out))


fcast <- global_pred_out
fcast

### Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec ### 62.10289

### Let's also plot the predictions along with original values, to
### get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
class_dec_pred
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

### So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

### Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

### Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima ### 26.24458

### Lastly, let's plot the predictions along with original values, to
### get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")




###################### Modeling for EU Sales ################################

Test_EU_Sale$Month_Year <- c(1:nrow(Test_EU_Sale))
View(Test_EU_Sale)
Train_EU_Sale$Month_Year <- c((nrow(Test_EU_Sale)+1):(nrow(Test_EU_Sale)+nrow(Train_EU_Sale)))
View(Train_EU_Sale)
Sale_EU_consumer_ts<- Sale_EU_consumer
Sale_EU_consumer_ts$Month_Year <- c(1:nrow(Sale_EU_consumer))


nrow(Test_EU_Qty)
nrow(Train_EU_Sale)
nrow(Test_EU_Sale)
nrow(Test_EU_Qty)
nrow(Test_EU_Sale)

### Modeling for EU Sales
timeser <- ts(Test_EU_Sale$Total_sale)
total_timeser <- ts(Sale_EU_consumer_ts$Total_sale)
plot(timeser)

### Smoothing the series - Moving Average Smoothing

w <- 1
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


### Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

### Smoothing right end of the time series
n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

### Plot the smoothed time series
timevals_in <- Test_EU_Sale$Month_Year
lines(smoothedseries, col="blue", lwd=2)

### Building a model on the smoothed time series using classical decomposition
### First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')
View(smootheddf)
nrow(smootheddf)
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
global_pred
lines(timevals_in, global_pred, col='red', lwd=2)

### Now, let's look at the locally predictable series
### We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

### We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi) # p-value = .1, thus the null hyphothesis that the series is stationary is true

### Now, let's evaluate the model using MAPE
### First, let's make a prediction for the last 6 months

outdata <- Train_EU_Sale
View(outdata)
names(outdata) <- c("Month", "Total_sale")
timevals_out <- outdata$Month
timevals_out

global_pred_out <- predict(lmfit, data.frame(Month = timevals_out))


fcast <- global_pred_out
fcast

### Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec ### 92.95788

### Let's also plot the predictions along with original values, to
### get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
class_dec_pred
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

### So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

### Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

### Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima ### 28.9226

### Lastly, let's plot the predictions along with original values, to
### get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")



###################### Modeling for EU Quantity ################################

Test_EU_Qty$Month_Year <- c(1:nrow(Test_EU_Qty))
View(Test_EU_Qty)
Train_EU_Qty$Month_Year <- c((nrow(Test_EU_Qty)+1):(nrow(Test_EU_Qty)+nrow(Train_EU_Qty)))
View(Train_EU_Sale)
Quant_EU_consumer_ts<- Quant_EU_consumer
Quant_EU_consumer_ts$Month_Year <- c(1:nrow(Quant_EU_consumer))


nrow(Test_EU_Qty)
nrow(Train_EU_Qty)



timeser <- ts(Test_EU_Qty$Total_quantity)
total_timeser <- ts(Quant_EU_consumer_ts$Total_quantity)
plot(timeser)

### Smoothing the series - Moving Average Smoothing

w <- 1
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


### Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

### Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

### Plot the smoothed time series

timevals_in <- Test_EU_Qty$Month_Year
lines(smoothedseries, col="blue", lwd=2)

### Building a model on the smoothed time series using classical decomposition
### First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Qty')
View(smootheddf)
nrow(smootheddf)
lmfit <- lm(Qty ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
global_pred
lines(timevals_in, global_pred, col='red', lwd=2)

### Now, let's look at the locally predictable series
### We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

### We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi) # p-value = .1, thus the null hyphothesis that the series is stationary is true

### Now, let's evaluate the model using MAPE
### First, let's make a prediction for the last 6 months

outdata <- Train_EU_Qty
View(outdata)
names(outdata) <- c("Month", "Total_qty")
timevals_out <- outdata$Month
timevals_out

global_pred_out <- predict(lmfit, data.frame(Month = timevals_out))


fcast <- global_pred_out 
fcast

### Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec ### 30.39741

### Let's also plot the predictions along with original values, to
### get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
class_dec_pred
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

### So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

### Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

### Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima ### 30.13319

### Lastly, let's plot the predictions along with original values, to
### get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")




