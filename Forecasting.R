###############################################################################
# Forecasting.R
# AquaGuard: Intelligent Leak Detection Automation
# Part of the Engineering Graduation Project at LYDEC
###############################################################################

# Load the necessary library for time series forecasting
library(forecast)

# Read the input CSV containing the time series data
# Replace "YOUR_DATA_FILE.csv" with the path to your dataset.
data <- read.csv2("YOUR_DATA_FILE.csv", header = TRUE)

# Convert the first column (time strings) to POSIXct format
time_str <- data[, 1]
date_time <- as.POSIXct(time_str, tz = "GMT", format = "%d/%m/%Y %H:%M")

# Define the start and end dates based on the data
date_debut <- date_time[1]
date_fin   <- date_time[length(date_time)]

# Create a complete time sequence with 5-minute intervals
time_seq <- seq(from = date_debut, to = date_fin, by = "5 min")

# Align the observed data with the complete time sequence
# This ensures that the time series is continuous.
Detection_NA <- data[, 2]
Detection_NA <- Detection_NA[match(time_seq, date_time)]

# Initialize vectors for predictions and forecast errors
n_pred <- 2015  # Number of prediction points (adjust as needed)
Prediction <- rep(0, n_pred)
Erreur     <- rep(0, n_pred)
k <- 0

# Loop over a sliding window to generate forecasts using auto.arima.
# Here we assume that 4 blocks of 2016 observations each form the history for each forecast.
for(i in (2016*4 + 1):(2016*5)) {
  k <- k + 1
  
  # Collect history from the previous 4 blocks
  history <- c(Detection_NA[i - 2016*4],
               Detection_NA[i - 2016*3],
               Detection_NA[i - 2016*2],
               Detection_NA[i - 2016*1])
  
  # Create a time series object from the history
  ts_history <- ts(history, start = 1, end = 4, frequency = 1)
  
  # Forecast the next value using auto.arima
  forecast_result <- predict(auto.arima(ts_history), h = 1)
  Prediction[k] <- forecast_result$pred
  
  # Compute a simple error metric based on the standard deviation of the history
  Erreur[k] <- 2.75 * sd(history) / sqrt(4)
}

# Extract the corresponding forecast dates from the time sequence
date_prev <- time_seq[(2016*4 + 1):(2016*5)]

# Combine the predictions and error metrics into one output object
output <- cbind(Prediction, Erreur)

# Write the forecast results to a CSV file.
# Replace "YOUR_OUTPUT_FILE.csv" with your desired output file path.
write.csv2(output, file = "YOUR_OUTPUT_FILE.csv", row.names = date_prev)
