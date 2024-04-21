# Load required libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(rugarch)

# Function to download financial data
download_financial_data <- function(symbol, start_date, end_date) {
  data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  return(data)
}

# Function to calculate log-returns
calculate_log_returns <- function(data) {
  log_returns <- diff(log(Cl(data)))
  return(log_returns)
}

# Function to plot time series data
plot_time_series <- function(data, title) {
  ggplot(data, aes(x = index(data), y = Cl(data))) +
    geom_line() +
    labs(title = title, x = "Date", y = "Price") +
    theme_minimal()
}

# Function to plot time series data for returns
plot_returns_time_series <- function(data, title) {
  ggplot(data, aes(x = Date, y = Returns)) +
    geom_line() +
    labs(title = title, x = "Date", y = "Returns") +
    theme_minimal()
}

# Function to plot histogram of returns
plot_returns_histogram <- function(returns, title) {
  ggplot() +
    geom_histogram(aes(x = returns), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = title, x = "Returns", y = "Frequency") +
    theme_minimal()
}

# Function to plot histogram of log returns
plot_log_returns_histogram <- function(log_returns, title) {
  ggplot() +
    geom_histogram(aes(x = log_returns), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = title, x = "Log Returns", y = "Frequency") +
    theme_minimal()
}

# Function to plot ACF
plot_acf <- function(returns, main) {
  acf_obj <- acf(returns, plot = FALSE)
  plot(acf_obj, main = main)
}

# Function to calculate Value at Risk (VaR)
calculate_var <- function(returns, confidence_level) {
  var <- quantile(returns, 1 - confidence_level, na.rm = TRUE)
  return(var)
}

# Function to plot time series data for log returns
plot_log_returns_time_series <- function(data, title) {
  ggplot(data, aes(x = Date, y = `Log Returns`)) +
    geom_line() +
    labs(title = title, x = "Date", y = "Log Returns") +
    theme_minimal()
}

# Confidence levels
confidence_levels <- c(0.50, 0.75, 0.90, 0.95, 0.975, 0.99)

# Calculate VaR at different confidence levels
var_values <- sapply(confidence_levels, function(confidence_level) {
  calculate_var(log_returns, confidence_level)
})

# Print VaR values
cat("Value at Risk (VaR):\n")
for (i in seq_along(confidence_levels)) {
  cat(paste("Confidence Level:", confidence_levels[i], ", VaR:", sprintf("%.4f", var_values[i]), "\n"))
}

# Main analysis
start_date <- Sys.Date() - 2000
end_date <- Sys.Date()

# Download financial data
data <- download_financial_data("AMZN", start_date, end_date)

# Remove missing values
data <- na.omit(data)

# Calculate log-returns
log_returns <- calculate_log_returns(data)

# Check for missing values in log returns
if (any(is.na(log_returns))) {
  cat("There are missing values in the log returns. Removing them...\n")
  log_returns <- log_returns[!is.na(log_returns)]
}

# Plot time series data
plot_time_series(data, title = "Amazon Stock Prices")

# Plot time series data for returns
plot_returns_time_series(returns_df, title = "Amazon Stock Returns")

# Plot histograms of returns and log returns separately
plot_returns_histogram(diff(log(Cl(data))), title = "Histogram of Returns")
plot_log_returns_histogram(log_returns, title = "Histogram of Log Returns")

# Plot time series data for log returns
plot_log_returns_time_series(log_returns_df, title = "Amazon Log Returns")

# Plot ACF of log returns if there are no missing values
if (!any(is.na(log_returns))) {
  plot_acf(log_returns, main = "Autocorrelation Function of Log Returns")
} 

# Fit ARIMA model
tryCatch({
  arima_model <- arima(log_returns, order = c(1, 3, 5))
}, error = function(e) {
  print("Error occurred while fitting ARIMA model:")
  print(e)
  arima_model <- NULL
})

if (!is.null(arima_model)) {
  # Check model summary
  print(summary(arima_model))
  
  # Plot ACF and PACF of residuals
  acf(residuals(arima_model))
  pacf(residuals(arima_model))
}

# Fit GARCH model
garch_spec <- ugarchspec(mean.model = list(armaOrder = c(1, 1)), variance.model = list(garchOrder = c(1, 1)))
garch_fit <- ugarchfit(spec = garch_spec, data = log_returns)

# Check model summary
summary(garch_fit)

# Fit GARCH model with ARIMA residuals
if (!is.null(arima_model)) {
  garch_spec <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = FALSE), 
                           variance.model = list(garchOrder = c(1, 1)))
  garch_fit <- ugarchfit(spec = garch_spec, data = residuals(arima_model))
  
  # Print model coefficients
  cat("GARCH Model Coefficients:\n")
  print(coef(garch_fit))
  
  # Calculate log-likelihood
  log_likelihood <- -sum(dnorm(residuals(arima_model), 
                               mean = 0, 
                               sd = sqrt(sigma(garch_fit)^2)))
  cat("Log-Likelihood:", log_likelihood, "\n")
  
  # Calculate AIC
  aic <- -2 * log_likelihood + 2 * length(coef(garch_fit))
  cat("AIC:", aic, "\n")
  
  
  #diagnostic plots
  plot(garch_fit)
  
} else {
  print("GARCH model fitting failed due to missing ARIMA model.")
}

# Forecast volatility for the next 10 periods
forecast_horizon <- 10
garch_forecast <- ugarchforecast(garch_fit, n.ahead = forecast_horizon)

# Extract volatility predictions
volatility_predictions <- sigma(garch_forecast)
cat("Volatility Predictions for the Next 10 Periods:\n")
print(volatility_predictions)

