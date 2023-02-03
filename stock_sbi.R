Sbi_stock <- read.csv("C:/Users/AMBUJ SHUKLA/Downloads/SBIN.NS (1).csv")
View(Sbi_stock)

str(Sbi_stock)


sum(as.null(Sbi_stock))
Sbi_stock <- na.omit(Sbi_stock)

Sbi_stock$Date<- as.Date(Sbi_stock$Date, format  = "%Y-%m-%d")
str(Sbi_stock$Date)

#Datatypes from charater vector to numeric vector

Sbi_stock$Open <- as.numeric(Sbi_stock$Open)
Sbi_stock$High <- as.numeric(Sbi_stock$High)
Sbi_stock$Low <- as.numeric(Sbi_stock$Low)
Sbi_stock$Close <- as.numeric(Sbi_stock$Close)
Sbi_stock$Adj.Close <- as.numeric(Sbi_stock$Adj.Close)
Sbi_stock$Volume <- as.numeric(Sbi_stock$Volume)

str(Sbi_stock)

#Data Exploration 

library(ggplot2)
ggplot(data = Sbi_stock, aes(x = Date, y = Close)) +
  geom_line(color = "#2E8B57") +
  xlab("Date") +
  ylab("Stock Price (in Rupees)") +
  ggtitle("SBI Stock Price Trend") +
  geom_ribbon(data = Sbi_stock, aes(ymin = 0, ymax = Close), alpha = 0.5, fill = "#90EE90")

Sbi_stock <- na.omit(Sbi_stock)
summary(Sbi_stock)
df<-Sbi_stock
# Time Series Decomposition: Decompose the time series data

Sbi_stock_ts <- ts(Sbi_stock$Close, frequency = 365) # convert Close to time series object with daily frequency
Sbi_stock_decomposed <- decompose(Sbi_stock_ts) # perform time series decomposition
trend <- Sbi_stock_decomposed$trend # extract trend component
seasonality <- Sbi_stock_decomposed$seasonal # extract seasonality component
residuals <- Sbi_stock_decomposed$random # extract residuals component


library(tsibble)

# create a time series object
ts_data <- as_tsibble(Sbi_stock, index = Date)

library(forecast)

# Fit ARIMA model to the time series data
fit <- auto.arima(ts_data$Adj.Close)

# Make predictions using the fitted ARIMA model
predictions <- forecast(fit, h = 30)

# Plot the predictions along with the original time series data
plot(predictions, main = "ARIMA Model Forecast")
lines(ts_data$Adj.Close, col = "blue")



library(forecast)

# Load data and convert to time series object
df<- Sbi_stock
current_date <- as.Date("2023-02-01")
ts_data <- ts(df$Adj.Close, start = c(1996, 1), end = current_date, frequency = 1000)
fit <- auto.arima(ts_data)
pred <- forecast(fit, h = 30)
plot(pred)




