################################################
#Avocado Sales/prices analysis
#Analyzing the Total US region only
#Examining Total Sales (Average Price x Total Volume)
#Dividing analysis by the avocado type (Conventional or Organic)
#Use of ggplot to create multi variate time series
###############################################

#load data
library(dplyr)
library(forecast)
avocado.df = read.csv("avocado.csv")
avocado.df = arrange(avocado.df, region)
avocado.df$Total.Sales = avocado.df$AveragePrice*avocado.df$Total.Volume
#Change Date Column from character to Date format
avocado.df$Date = as.Date(avocado.df$Date, "%Y-%m-%d")
class(avocado.df$Date)
avocado.whole.df = avocado.df[order(avocado.df$Date),]
View(avocado.whole.df)

#Create dataframe for just Total US sales
avocado.totalus = filter(avocado.whole.df[order(avocado.whole.df$Date),], region=="TotalUS")
View(avocado.totalus)
#Separate Conventional and Organic
conv.totalus = filter(avocado.totalus, type == "conventional")
head(conv.totalus)

org.totalus = filter(avocado.totalus, type == "organic")
head(org.totalus)

#Time Series for Total US - Total Sales 
avocado.totalus.ts = ts(avocado.totalus$Total.Sales, start = c(2015,1), end = c(2018,12), freq = 52)
plot(avocado.totalus.ts, xlab = "Time", ylab = "Total Sales ($)", main = "Total Sales Across US")
#^^^^^Not too helpful, too dense to analyze


#Create Time Series for Total Sales of conventional avocados
conv.totalus.ts = ts(conv.totalus$Total.Sales, start = c(2014,12), end = c(2018,4), freq = 52)
#Fit linear regression to time series
conv.totalsales.lm = tslm(conv.totalus.ts ~ trend + I(trend^2))
#Created cleaner looking version w/ ggplot below
plot(conv.totalus.ts, xlab = "Time", ylab = "Total Sales($)", main = "Total Sales Conventional")
lines(conv.totalsales.lm$fitted.values)

#Create Time Series for Total Sales of organic avocados
org.totalus.ts = ts(org.totalus$Total.Sales, start = c(2014,12), end = c(2018,4), freq = 52)
#Fit linear regression 
org.totalsales.lm = tslm(org.totalus.ts ~ trend + I(trend^2))

#Created cleaner looking version w/ ggplot below
plot(org.totalus.ts, xlab = "Time", ylab = "Total Sales ($)", main = "Total")
lines(org.totalsales.lm$fitted.values)


####################################################
#Use ggplot to create graphs
library(ggplot2)
library(ggfortify)
library(reshape2)

#Total Sales multi series graph for conventional/organic in total US
#Create overview of data, to then be broken into individual plots
sales_trend = avocado.totalus %>% select(Date, Total.Sales, type) %>% 
  ggplot(aes(x=Date, y=Total.Sales)) + geom_area(aes(color = type, fill = type), alpha = 0.3) +
  labs(title = "Total Sales of Avocados in US", y = "Total Sales ($)") + theme_bw()
sales_trend

#Individual graphs for Conventional and Organic

#Autoplot to create time series plots
#Creating individual plots for conventional and organic avocados
conv.totalsales = autoplot(conv.totalus.ts) + labs(title = "Total Sales of Conventional Avocados", y = "Total Sales ($)") + theme_bw()
conv.totalsales

org.totalsales = autoplot(org.totalus.ts) + labs(title = "TOtal Sales of Organic Avocados", y = "Total Sales") + theme_bw()
org.totalsales

#Multi-variate Time series

#Time series of Total Sales of Conventional & Organic
ggplot(avocado.totalus, aes(Date, Total.Sales, group = type, color = type)) +
  geom_line() + labs(title = "Total Sales of Avocados in US", y = "Total Sales ($)") + theme_bw()

#Time Series of Average Price of Conventional & ORganic
ggplot(avocado.totalus, aes(Date, AveragePrice, group = type, color = type)) +
  geom_line() + labs(title = "Average Price of Avocados by type", y = "Average Price per ($)") + theme_bw()

