# Install packages and load libraries
install.packages("ggplot2")
install.packages("colorspace")
install.packages("lubridate")
install.packages("dplyr")
install.packages("stringi")
install.packages("prophet")
library(tseries)
library(dplyr)
library(prophet)
library(lubridate)
library(colorspace)
library(ggplot2)

# Read the data from csv file and load it
data = read.csv("C:/Users/Namrata/Desktop/intermediate/crypto-markets.csv")
#Structure of data
str(data)
head(data)
tail(data)

# changing the date format
data$date = as.Date(data$date, format = "%m/%d/%y")



#Seperating the bitcoin data from rest
data_bitcoin = subset(data,data$name == 'Bitcoin')
data_bitcoin

#time series 
data_bitcoin_series <- ts(data_bitcoin$close, frequency=12, start=2013, end = 2019)
class(data_bitcoin_series)
plot(data_bitcoin_series)

#decompose time series
decomp_data_bitcoin_series <- decompose(data_bitcoin_series)
plot(decomp_data_bitcoin_series)

#Including the date and closing value of the day in the dataset
newdata <- data_bitcoin[c(4,9)]
tail(newdata)



#Plotting the data
colnames(newdata) <- c("ds", "y")
ggplot(newdata, aes(ds, y )) + geom_line(col="blue") + scale_x_date('Year')  +
  ylab("Closing Price") + xlab("")

tail(newdata)


#To forecast 180 days further using prophet model
m <- prophet(newdata, daily.seasonality = TRUE)
m
future <- make_future_dataframe(m, periods = 180)
tail(future)

forecast <- predict(m, future)
tail(forecast)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Plot forecast
plot(m, forecast)

#components plot
prophet_plot_components(m, forecast)




