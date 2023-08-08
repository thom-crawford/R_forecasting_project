############################################

#Script for forecasting the total sales, and average price
#in Texas & New Mexico Regions (Dallas, Houston, WestTexNewMexico)
#Will split forecasting based on conventional or organic avocados

#Created by Thomas Crawford 10/10/22
############################################

#Load Packages
library(dplyr)
library(forecast)
library(ggplot2)
library(reshape2)

#load data
data.df = read.csv("avocado.csv")
#Group data by Region
data.df = arrange(data.df, region)
data.df$Total.Sales = data.df$AveragePrice*data.df$Total.Volume
#Change Date Column from character to Date format
data.df$Date = as.Date(data.df$Date, "%Y-%m-%d")
class(data.df$Date)
head(data.df)
#Organize data in ascending Date (currently lists end of time2018 at top)
data.df = data.df[order(data.df$Date),]
View(data.df)
#Filter for Texas Regions (DallasFtWorth, Houston, WestTexNewMexico)
avo.df = filter(data.df, region %in% c("DallasFtWorth", "Houston", "WestTexNewMexico"))
View(avo.df)

#Separate main dataframe into 1 for Conventional and 1 for organic
avo.conv.df = filter(avo.df, type == 'conventional')
head(avo.conv.df)
avo.org.df = filter(avo.df, type == 'organic')
head(avo.org.df)
#Create time series for Average Price, Total Volume, and Total Sales for our 3 regions

#Conventional Avocado TS
conv.price.ts = ts(avo.conv.df$AveragePrice, start = c(2015,1), end = c(2018,4), frequency = 52)
conv.volume.ts = ts(avo.conv.df$Total.Volume, start = c(2015,1), end = c(2018,4), frequency = 52)
conv.sales.ts = ts(avo.conv.df$Total.Sales, start = c(2015,1), end = c(2018,4), frequency = 52)

#organic Avocado TS
org.price.ts = ts(avo.org.df$AveragePrice, start = c(2015,1), end = c(2018,4), frequency = 52)
org.volume.ts = ts(avo.org.df$Total.Volume, start = c(2015,1), end = c(2018,4), frequency = 52)
org.sales.ts = ts(avo.org.df$Total.Sales, start = c(2015,1), end = c(2018,4), frequency = 52)

#####################################################
#Create initial time plots for AVERAGE PRICE CONVENTIONAL ts object
#####################################################
#Conventional
p1 = autoplot(conv.price.ts) + 
  ggtitle("Average Price of Conventional Avocados") +
  ylab("Average Price ($)") + 
  xlab("Date") + theme_bw()

p2 = ggplot(avo.conv.df, aes(Date, AveragePrice, group = region, color = region)) +
  geom_line() +
  labs(title = "Average Price of Conventional Avocados", y = "Average Price per ($)") + theme_bw() +
  theme(legend.position = "top") 

#################################################
#Modeling - which model performs best
################################################

#Take the first difference of data, removing any trends to allow for best forecasting
d.conv.price.ts = diff(conv.price.ts)
autoplot(d.conv.price.ts) + ggtitle("Change of Average Price of Conventional Avocados") + ylab("Average Price ($)")

#Check for seasonality and remove seasonality w/ stationary trend
ggseasonplot(d.conv.price.ts) + ggtitle("Seasonal Plot: Change in Average Price") + ylab("Average Price ($)")

#Benchmark method for eval model improvements
#Using Naive as benchmark, trend is random, very little seasonality
conv.price.naive = naive(d.conv.price.ts) 
summary(conv.price.naive)
checkresiduals(conv.price.naive)
#Best Naive Benchmark Model
#Frequency = 52 - sigma: 0.0988

conv.price.snaive = snaive(d.conv.price.ts) #performs better than naive (We have seasonality, this is our benchmark)
summary(conv.price.snaive)
checkresiduals(conv.price.snaive)
#Best SNaive Benchmark Model
#Frequency = 52 - sigma: 0.0964

#Find better model than benchmark
#Using ets (Exponential smoothing) model (Allows for trend)
conv.price.ets = ets(conv.price.ts) 
print(summary(conv.price.ets))
checkresiduals(conv.price.ets)
#Best ETS Model
#Frequency = 52 - sigma: 0.0626

#Try ARIMA model (data has to be stationary - i.e. no trend/seasonality)
#Giving regular ts, and telling take 1st difference (d=1) removing , remove seasonality (D=1)
#Per above benchmark, snaive performs better than naive, data is seasonal, need to compensate for it ARIMA model
conv.price.arima = auto.arima(conv.price.ts, d=1,D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(conv.price.arima)) #sigma/residual sd = 0.04624932 using model ARIMA(2,1,0)(1,0,0)
checkresiduals(conv.price.arima)
#Best ARIMA Model
#Frequency = 52 - sigma: 0.0685 using ARIMA(2,1,3)(0,1,0)[52]


######################################
#Forecasting!! - Next 12 months
#Using ARIMA model, posted lowest residuals/error rate and best ACF plot

conv.price.fcst = forecast::forecast(conv.price.arima, h=52)
f1 = autoplot(conv.price.ts) +
  autolayer(conv.price.fcst, series = "ARIMA", color = "#347235") +
  labs(title="Forecasting Average Price of Conventional Avocados \n in Texas & New Mexico using an ARIMA model", x="Date", y="Price ($)") +
  theme_bw()
summary(conv.price.fcst)

#Evaluate forecast Model
#Create function that returns a forecast for us in tsCV command
conv.price.fx = function(x, h){forecast::forecast(auto.arima(x, d=1, D=1), h = h)}

conv.price.eval = tsCV(conv.price.ts, forecastfunction = conv.price.fx, h=1)
conv.price.eval
sqrt(mean(conv.price.eval^2, na.rm = TRUE)) #RMSE of 0.079697 = model performance is good, overall low error rate beats benchmark

####################################################################################################

#Create initial plots, benchmarks, and forecasting + evaluation for ORGANIC AVERAGE PRICE

####################################################################################################
#Organic
p3 = autoplot(org.price.ts) +
  ggtitle("Average Price of Organic Avocados") +
  ylab("Average Price ($)") +
  xlab("Date") + theme_bw()

p4 = ggplot(avo.org.df, aes(Date, AveragePrice, group = region, color = region)) +
  geom_line() +
  labs(title = "Average Price of Organic Avocados", y = "Average Price per ($)") + theme_bw() +
  theme(legend.position = "top")

#Use plot_grid() to combine organic/conventional into single window

#################################################
#Modeling - which model performs best
################################################

#Take the first difference of data, removing any trends to allow for best forecasting
d.org.price.ts = diff(org.price.ts)
autoplot(d.org.price.ts) + ggtitle("Change of Average Price of Conventional Avocados") + ylab("Average Price ($)")

#Check for seasonality and remove seasonality w/ stationary trend
ggseasonplot(d.org.price.ts) + ggtitle("Seasonal Plot: Change in Average Price") + ylab("Average Price ($)")

#Benchmark method for eval model improvements
#Using Naive as benchmark, trend is random, very little seasonality
org.price.naive = naive(d.org.price.ts) 
summary(org.price.naive)
checkresiduals(org.price.naive)
#Best Naive Benchmark Model
#Frequency = 52 - sigma: 0.6395 - MAE 0.5558 - MAPE Inf

org.price.snaive = snaive(d.org.price.ts) #performs better than naive (We have seasonality, this is our benchmark)
summary(org.price.snaive)
checkresiduals(org.price.snaive)
#Best SNaive Benchmark Model
#Frequency = 52 - sigma: 0.6148 - MAE 0.5348 - MAPE Inf

#Find better model than benchmark
#Using ets (Exponential smoothing) model (Allows for trend)
org.price.ets = ets(org.price.ts) 
print(summary(org.price.ets))
checkresiduals(org.price.ets)
#Best ETS Model
#Frequency = 52 - sigma: 0.1525 - MAE 0.1908 - MAPE 12.6045

#Try ARIMA model (data has to be stationary - i.e. no trend/seasonality)
#Giving regular ts, and telling take 1st difference (d=1) removing , remove seasonality (D=1)
#Per above benchmark, snaive performs better than naive, data is seasonal, need to compensate for it ARIMA model
org.price.arima = auto.arima(org.price.ts, d=1,D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(org.price.arima))
checkresiduals(org.price.arima)
#Best ARIMA Model
#Frequency = 52 - sigma: 0.1413 - MAE 0.06289 - MAPE 4.1408 using ARIMA(2,1,0)(1,1,0)[52]


######################################
#Forecasting!! - Next 12 months
#Using ARIMA model, posted lowest residuals/error rate and best ACF plot

org.price.fcst = forecast::forecast(org.price.arima, h=52)
f2 = autoplot(org.price.ts) +
  autolayer(org.price.fcst, series = "ARIMA", color = "#667C26") +
  labs(title="Forecasting Average Price of Organic Avocados \n in Texas & New Mexico using an ARIMA model", x="Date", y="Price ($)") +
  theme_bw()
summary(org.price.fcst)

#Evaluate forecast Model
#Create function that returns a forecast for us in tsCV command
org.price.fx = function(x, h){forecast::forecast(auto.arima(x, d=1, D=1), h = h)}

org.price.eval = tsCV(org.price.ts, forecastfunction = org.price.fx, h=1)
org.price.eval
sqrt(mean(org.price.eval^2, na.rm = TRUE)) #RMSE of 0.30013 = model performance is good, overall low error rate beats benchmark

###############################################################

#Create initial plots, benchmarks, and forecasting + evaluation for CONVENTIONAL TOTAL VOLUME

###############################################################
#Conventional
p5 = autoplot(conv.volume.ts) +
  ggtitle("Total Volume of Conventional Avocados") +
  ylab("Total Volume") +
  xlab("Date") + theme_bw()

p6 = ggplot(avo.conv.df, aes(Date, Total.Volume, group = region, color = region)) +
  geom_line() + 
  labs(title = "Total Volume of Conventional Avocados", y = "Total Volume") + theme_bw() +
  theme(legend.position = "top") 

#Take the first difference of data, removing any trends to allow for best forecasting
d.conv.volume.ts = diff(conv.volume.ts)
autoplot(d.conv.volume.ts) + ggtitle("Change in Total Volume of Conventional Avocados") + ylab("Total Volume")

#Check for seasonality and remove seasonality w/ stationary trend
ggseasonplot(d.conv.volume.ts) + ggtitle("Seasonal Plot: Change in Total Volume") + ylab("Total Volume")


#Benchmark method for eval model improvements
#Using Naive as benchmark, trend is random, very little seasonality
conv.volume.naive = naive(d.conv.volume.ts) 
summary(conv.volume.naive)
checkresiduals(conv.volume.naive)
#Best Naive Benchmark Model
#Frequency = 52 - sigma: 480388.95 - MAE 424038.7 - MAPE 354.3107

conv.volume.snaive = snaive(d.conv.volume.ts) #performs better than naive (We have seasonality, this is our benchmark)
summary(conv.volume.snaive)
checkresiduals(conv.volume.snaive)
#Best SNaive Benchmark Model
#Frequency = 52 - sigma: 462839.22 - MAE 407004.2 - MAPE 332.53

#Find better model than benchmark
#Using ets (Exponential smoothing) model (Allows for trend)
conv.volume.ets = ets(conv.volume.ts) 
print(summary(conv.volume.ets))
checkresiduals(conv.volume.ets)
#Best ETS Model
#Frequency = 52 - sigma: 187325.6 - RMSE 186151.1 -  MAE 155706.3 - MAPE 17.10

#Try ARIMA model (data has to be stationary - i.e. no trend/seasonality)
#Giving regular ts, and telling take 1st difference (d=1) removing , remove seasonality (D=1)
#Per above benchmark, snaive performs better than naive, data is seasonal, need to compensate for it ARIMA model
conv.volume.arima = auto.arima(conv.volume.ts, d=1,D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(conv.volume.arima))
checkresiduals(conv.volume.arima)
#Best ARIMA Model
#Frequency = 52 - sigma: 137767.9 - RMSE 109989.2 - MAPE 7.7734 using ARIMA(4,1,1)(0,1,0)[52]

######################################
#Forecasting!! - Next 12 months
#Using ARIMA model, posted lowest residuals/error rate and best ACF plot

conv.volume.fcst = forecast::forecast(conv.volume.arima, h=52)
f3 = autoplot(conv.volume.ts) +
  autolayer(conv.volume.fcst, series = "ARIMA", color = "#347235") +
  labs(title="Forecasting Total Volume of Conventional Avocados \n in Texas & New Mexico using an ARIMA model", x="Date", y="Volume Sold") +
  theme_bw()
summary(conv.volume.fcst)

#Evaluate forecast Model
#Create function that returns a forecast for us in tsCV command
conv.volume.fx = function(x, h){forecast::forecast(auto.arima(x, d=1, D=1), h = h)}

conv.volume.eval = tsCV(conv.volume.ts, forecastfunction = conv.volume.fx, h=1)
conv.volume.eval
sqrt(mean(conv.volume.eval^2, na.rm = TRUE)) #RMSE of 256455.7 = model performance is okay, higher RMSE than ARIMA model but beats benchmark


###############################################################

#Create initial plots, benchmarks, and forecasting + evaluation for ORGANIC TOTAL VOLUME

###############################################################
#Organic
p7 = autoplot(org.volume.ts) +
  ggtitle("Total Volume of Organic Avocados") +
  ylab("Total Volume") +
  xlab("Date") + theme_bw()

p8 = ggplot(avo.org.df, aes(Date, Total.Volume, group = region, color = region)) +
  geom_line() +
  labs(title = "Total Volume of Organic Avocados", y = "Total Volume")  + theme_bw() +
  theme(legend.position = "top")

#Use plot_grid() to combine organic/conventional into single window
#Take the first difference of data, removing any trends to allow for best forecasting
d.org.volume.ts = diff(org.volume.ts)
autoplot(d.org.volume.ts) + ggtitle("Change in Total Volume of Organic Avocados") + ylab("Total Volume")

#Check for seasonality and remove seasonality w/ stationary trend
ggseasonplot(d.org.volume.ts) + ggtitle("Seasonal Plot: Change in Total Volume") + ylab("Total Volume")

#Benchmark method for eval model improvements
#Using Naive as benchmark, trend is random, very little seasonality
org.volume.naive = naive(d.org.volume.ts) 
summary(org.volume.naive)
checkresiduals(org.volume.naive)
#Best Naive Benchmark Model
#Frequency = 52 - sigma: 6225.1931 - RMSE 6225.193 - MAE 51558.493 - MAPE 1202.527

org.volume.snaive = snaive(d.org.volume.ts) #performs better than naive (We have seasonality, this is our benchmark)
summary(org.volume.snaive)
checkresiduals(org.volume.snaive)
#Best SNaive Benchmark Model
#Frequency = 52 - sigma: 5343.44 - RMSE 5343.44 - MAE 4411.68 - MAPE 1185.96

#Find better model than benchmark
#Using ets (Exponential smoothing) model (Allows for trend)
org.volume.ets = ets(org.volume.ts) 
print(summary(org.volume.ets))
checkresiduals(org.volume.ets)
#Best ETS Model
#Frequency = 52 - sigma: 2470.06 - RMSE 2454.57 -  MAE 1907.12 - MAPE 20.14

#Try ARIMA model (data has to be stationary - i.e. no trend/seasonality)
#Giving regular ts, and telling take 1st difference (d=1) removing , remove seasonality (D=1)
#Per above benchmark, snaive performs better than naive, data is seasonal, need to compensate for it ARIMA model
org.volume.arima = auto.arima(org.volume.ts, d=1,D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(org.volume.arima))
checkresiduals(org.volume.arima)
#Best ARIMA Model
#Frequency = 52 - sigma: 2696.82 - RMSE 2153.24 - MAE 1396.38 - MAPE 14.44 using ARIMA(3,1,1)(1,1,0)[52]

######################################
#Forecasting!! - Next 12 months
#Using ARIMA model, posted lowest residuals/error rate and best ACF plot

org.volume.fcst = forecast::forecast(org.volume.arima, h=52)
f4 = autoplot(org.volume.ts) +
  autolayer(org.volume.fcst, series = "ARIMA", color = "#667C26") +
  labs(title="Forecasting Total Volume of Organic Avocados \n in Texas & New Mexico using an ARIMA model", x="Date", y="Volume Sold") +
  theme_bw()
summary(org.volume.fcst)

#Evaluate forecast Model
#Create function that returns a forecast for us in tsCV command
org.volume.fx = function(x, h){forecast::forecast(auto.arima(x, d=1, D=1), h = h)}

org.volume.eval = tsCV(org.volume.ts, forecastfunction = org.volume.fx, h=1)
org.volume.eval
sqrt(mean(org.volume.eval^2, na.rm = TRUE)) #RMSE of 3619.791 = model performance is okay, higher RMSE than ARIMA model but beats benchmark


###############################################################

#Create initial plots, benchmarks, and forecasting + evaluation for CONVENTIONAL TOTAL SALES

###############################################################

#Time series for Total Sales
#Conventional
p9 = autoplot(conv.sales.ts) +
  ggtitle("Total Sales of Conventional Avocados") +
  ylab("Total Sales ($)") +
  xlab("Date") + theme_bw()

p10 = ggplot(avo.conv.df, aes(Date, Total.Sales, group = region, color = region)) +
  geom_line() +
  labs(title = "Total Sales of Conventional Avocados", y = "Total Sales ($)") + theme_bw() +
  theme(legend.position = "top") 

#Take the first difference of data, removing any trends to allow for best forecasting
d.conv.sales.ts = diff(conv.sales.ts)
autoplot(d.conv.sales.ts) + ggtitle("Change in Total Sales of Conventional Avocados") + ylab("Total Sales ($)")

#Check for seasonality and remove seasonality w/ stationary trend
ggseasonplot(d.conv.sales.ts) + ggtitle("Seasonal Plot: Change in Total Sales") + ylab("Total Sales($)")

#Benchmark method for eval model improvements
#Using Naive as benchmark, trend is random, very little seasonality
conv.sales.naive = naive(d.conv.sales.ts) 
summary(conv.sales.naive)
checkresiduals(conv.sales.naive)
#Best Naive Benchmark Model
#Frequency = 52 - sigma: 385108.31 - RMSE 385108.3 - MAE 347386.5 - MAPE 346.60

conv.sales.snaive = snaive(d.conv.sales.ts) #performs better than naive (We have seasonality, this is our benchmark)
summary(conv.sales.snaive)
checkresiduals(conv.sales.snaive)
#Best SNaive Benchmark Model
#Frequency = 52 - sigma: 386157.12 - RMSE 386157.1 - MAE 349872.3 - MAPE 279.89

#Find better model than benchmark
#Using ets (Exponential smoothing) model (Allows for trend)
conv.sales.ets = ets(conv.sales.ts) 
print(summary(conv.sales.ets))
checkresiduals(conv.sales.ets)
#Best ETS Model
#Frequency = 52 - sigma: 144357.9 - RMSE 143452.8 -  MAE 122429.8 - MAPE 17.26

#Try ARIMA model (data has to be stationary - i.e. no trend/seasonality)
#Giving regular ts, and telling take 1st difference (d=1) removing , remove seasonality (D=1)
#Per above benchmark, snaive performs better than naive, data is seasonal, need to compensate for it ARIMA model
conv.sales.arima = auto.arima(conv.sales.ts, d=1,D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(conv.sales.arima))
checkresiduals(conv.sales.arima)
#Obtained best model (ARIMA)

#Best ARIMA Model
#Frequency = 52 - sigma: 74545.29 - RMSE 60100.39 - MAE 41061.6 - MAPE 5.594 using ARIMA(2,1,0)(1,1,0)[52]

######################################
#Forecasting!! - Next 12 months
#Using ARIMA model, posted lowest residuals/error rate and best ACF plot
#####################################

conv.sales.fcst = forecast::forecast(conv.sales.arima, h=52)
f5 = autoplot(conv.sales.ts) +
  autolayer(conv.sales.fcst, series = "ARIMA", color = "#347235") +
  labs(title="Forecasting Total Sales of Conventional Avocados \n in Texas & New Mexico using an ARIMA model", x="Date", y="Sales ($)") +
  theme_bw()
summary(conv.sales.fcst)

#Evaluate forecast Model
#Create function that returns a forecast for us in tsCV command
conv.sales.fx = function(x, h){forecast::forecast(auto.arima(x, d=1, D=1), h = h)}

conv.sales.eval = tsCV(conv.sales.ts, forecastfunction = conv.sales.fx, h=1)
conv.sales.eval
sqrt(mean(conv.sales.eval^2, na.rm = TRUE)) #RMSE of 226362.2 = model performance is okay, higher RMSE than ARIMA model but lower than benchmark


###############################################################

#Create initial plots, benchmarks, and forecasting + evaluation for ORGANIC TOTAL SALES

###############################################################
#Organic
p11 = autoplot(org.sales.ts) +
  ggtitle("Total Sales of Organic Avocados") +
  ylab("Total Sales($)") +
  xlab("Date") + theme_bw()

p12 = ggplot(avo.org.df, aes(Date, Total.Sales, group = region, color = region)) +
  geom_line() +
  labs(title = "Total Sales of Organic Avocados", y = "Total Sales ($)")  + theme_bw() +
  theme(legend.position = "top")

#Use plot_grid() to combine organic/conventional into single window

#Take the first difference of data, removing any trends to allow for best forecasting
d.org.sales.ts = diff(org.sales.ts)
autoplot(d.org.sales.ts) + ggtitle("Change in Total Sales of Organic Avocados") + ylab("Total Sales ($)")

#Check for seasonality and remove seasonality w/ stationary trend
ggseasonplot(d.org.sales.ts) + ggtitle("Seasonal Plot: Change in Total Sales") + ylab("Total Sales($)")

#Benchmark method for eval model improvements
#Using Naive as benchmark, trend is random, very little seasonality
org.sales.naive = naive(d.org.sales.ts) 
summary(org.sales.naive)
checkresiduals(org.sales.naive)
#Best Naive Benchmark Model
#Frequency = 52 - sigma: 8204.94 - RMSE 8204.94 - MAE 5703.87 - MAPE 713.58

org.sales.snaive = snaive(d.org.sales.ts) #performs better than naive (We have seasonality, this is our benchmark)
summary(org.sales.snaive)
checkresiduals(org.sales.snaive)
#Best SNaive Benchmark Model
#Frequency = 52 - sigma: 6816.32 - RMSE 6816.32 - MAE 5270.00 - MAPE 1147.55

#Find better model than benchmark
#Using ets (Exponential smoothing) model (Allows for trend)
org.sales.ets = ets(org.sales.ts) 
print(summary(org.sales.ets))
checkresiduals(org.sales.ets)
#Best ETS Model
#Frequency = 52 - sigma: 0.2057 - RMSE 3349.27 -  MAE 2409.594 - MAPE 16.44 (Don't know how/why sigma value is so low)

#Try ARIMA model (data has to be stationary - i.e. no trend/seasonality)
#Giving regular ts, and telling take 1st difference (d=1) removing , remove seasonality (D=1)
#Per above benchmark, snaive performs better than naive, data is seasonal, need to compensate for it ARIMA model
org.sales.arima = auto.arima(org.sales.ts, d=1,D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(org.sales.arima))
checkresiduals(org.sales.arima)
#Obtained best model (ARIMA)

#Best ARIMA Model
#Frequency = 52 - sigma: 3921.04 - RMSE 3130.70 - MAE 1899.753 - MAPE 12.69 using ARIMA(3,1,1)(1,1,0)[52]

######################################
#Forecasting!! - Next 12 months
#Using ARIMA model, posted lowest residuals/error rate and best ACF plot
#####################################

org.sales.fcst = forecast::forecast(org.sales.arima, h=52)
f6 = autoplot(org.sales.ts) +
  autolayer(org.sales.fcst, series = "ARIMA", color = "#667C26") +
  labs(title="Forecasting Total Sales of Organic Avocados \n in Texas & New Mexico using an ARIMA model", x="Date", y="Price ($)") +
  theme_bw()
summary(org.sales.fcst)

#Evaluate forecast Model
#Create function that returns a forecast for us in tsCV command
org.sales.fx = function(x, h){forecast::forecast(auto.arima(x, d=1, D=1), h = h)}

org.sales.eval = tsCV(org.sales.ts, forecastfunction = org.sales.fx, h=1)
org.sales.eval
sqrt(mean(org.sales.eval^2, na.rm = TRUE)) #RMSE of 5476.40 = model performance is decent, higher RMSE than ARIMA model but lower than benchmark

###############################################################################
###############################################################################
###############################################################################

#Creating Grouped Visuals for presentation
library(cowplot)
#Grouping of Average Price (Conv & Organic) - combined and by region breakdown
plot_grid(p1, p2, p3, p4)
plot_grid(p1,p2) #Just Conventional
plot_grid(p3,p4) #Just organic

#Grouping of Total Volume
plot_grid(p5,p6,p7,p8)
plot_grid(p5,p6) #Just conventional
plot_grid(p7,p8) #Just organic

#Grouping of Total Sales
plot_grid(p9,p10,p11,p12)
plot_grid(p9,p10) #just conventional
plot_grid(p11,p12) #Just organic

#Forecasting Groupings
#Average Price
plot_grid(f1,f2)

#Total Volume
plot_grid(f3,f4)

#Total Sales
plot_grid(f5,f6)
