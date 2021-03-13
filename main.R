t <- c(5,7,12,16,20)
xt <- c(4,12,18,21,24)

# xt depends on t
ts_model <- lm(xt~t)
summary(ts_model)

# Residual Analysis - Check assumptions of linear model
# 1. Normality check on residuals - histogram, qq plot
# 2. Homoscedasticity - Residuals should have constant variance
# 3. Independence of residuals

# Fitted model: yt=1.15+1.22
# Residuals = Observed - fitted

ts_model$residuals

hist(ts_model$residuals)

# Normal QQ Plot
plot(ts_model, 2)

# Homoscedasticity / Homogeneity of variance
plot(ts_model, 3)

# independence of residuals
library(car)

durbinWatsonTest(ts_model)

###################
# Air Passengers Data-set

# Load Packages
library(ggfortify)
library(tseries)
library(forecast)

# Load Data
data("AirPassengers")
AP <- AirPassengers

class(AP)

# AP <- ts(AP) - If data-set is not of class "ts"

#############################
# Part 1
# Perform Exploratory Data analysis
AP

# Check for missing values
sum(is.na(AP))

# Check Frequency of the time series
frequency(AP)

# Check cycle of the Time series
cycle(AP)

# Review the table summary
summary(AP)

# Plot the raw data using the base plot function
plot(AP,xlab="Date", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961")

# Alternative plotting function from "ggfortify" package
autoplot(AP) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961")

# Using boxplot function to see Seasonal Effects
boxplot(AP~cycle(AP),xlab="Date", ylab = "Passenger Numbers (1000's)" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")

# Part II
# Time Series Decomposition

# We saw an indication of a multiplicative model

decomposeAP <- decompose(AP, "multiplicative")
autoplot(decomposeAP)