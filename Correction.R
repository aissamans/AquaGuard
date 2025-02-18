###############################################################################
# Correction.R
# AquaGuard: Intelligent Leak Detection Automation
# Part of the Engineering Graduation Project at LYDEC
###############################################################################

##########################
# 1. Data Preparation
##########################

# Load necessary libraries
library(forecast)
library(qcc)

# Read the input CSV file (replace with your actual data file)
data <- read.csv2("YOUR_DATA_FILE.csv", header = TRUE)

# Convert the first column (time strings) to POSIXct format
time_str <- data[, 1]
date_time <- as.POSIXct(time_str, tz = "GMT", format = "%d/%m/%Y %H:%M")

# Define a complete time sequence with 5-minute intervals
date_debut <- date_time[1]
date_fin   <- date_time[length(date_time)]
time_seq   <- seq(from = date_debut, to = date_fin, by = "5 min")

# Align measured data with the complete time sequence
Debit_corrige <- data[, 2]
Detection_NA  <- Debit_corrige[match(time_seq, date_time)]
Debit_corrige <- Detection_NA

##########################
# 2. Correct Small Missing/Negative Values
##########################
# (Basic corrections/interpolations for isolated missing or negative values)
SeqNegative <- which(Debit_corrige < 0)
if (length(SeqNegative) != 0) {
  for (j in SeqNegative) {
    # If both neighbors exist and are non-missing, replace negative value with their mean
    if (!is.na(Debit_corrige[j+1]) && !is.na(Debit_corrige[j-1])) {
      if (Debit_corrige[j+1] >= 0 && Debit_corrige[j-1] >= 0) {
        Debit_corrige[j] <- mean(c(Debit_corrige[j+1], Debit_corrige[j-1]))
      }
      # Additional conditions for using further neighbors can be implemented here.
      # For instance, if there are 2 successive missing values, you can replace them 
      # with a (60%, 40%) weighting based on the nearest existing values for up to 5 consecutive
      # missing entries (equivalent to about 30 minutes of data interruption).
    }
  }
}

SeqNA <- which(is.na(Debit_corrige))
for (i in SeqNA) {
  if (!is.na(Debit_corrige[i+1]) && !is.na(Debit_corrige[i-1])) {
    if (Debit_corrige[i+1] >= 0 && Debit_corrige[i-1] >= 0) {
      Debit_corrige[i] <- mean(c(Debit_corrige[i+1], Debit_corrige[i-1]))
    } else if (Debit_corrige[i+1] >= 0 && Debit_corrige[i-1] < 0) {
      if (!is.na(Debit_corrige[i-2]) && Debit_corrige[i-2] >= 0) {
        Debit_corrige[i] <- 0.4 * Debit_corrige[i-2] + 0.6 * Debit_corrige[i+1]
      } else if (!is.na(Debit_corrige[i-3]) && Debit_corrige[i-3] >= 0) {
        Debit_corrige[i] <- 0.35 * Debit_corrige[i-3] + 0.65 * Debit_corrige[i+1]
      }
    } else if (Debit_corrige[i+1] < 0 && Debit_corrige[i-1] >= 0) {
      if (!is.na(Debit_corrige[i+2]) && Debit_corrige[i+2] >= 0) {
        Debit_corrige[i] <- 0.4 * Debit_corrige[i-1] + 0.6 * Debit_corrige[i+2]
      } else if (!is.na(Debit_corrige[i+3]) && Debit_corrige[i+3] >= 0) {
        Debit_corrige[i] <- 0.35 * Debit_corrige[i-1] + 0.65 * Debit_corrige[i+3]
      }
    }
    # Extend logic as needed for more complex scenarios
  }
}

###############################################################################
# 3. Correct Large Missing Sequences Using auto.arima
###############################################################################
# For cases with more than 5 consecutive missing values, we use a block approach.
# Example: Assume 4 blocks of 2016 observations each.

NewSeqNA <- which(is.na(Debit_corrige))
NewSeqNA_threshold_indices <- which(NewSeqNA > 8064)  # Adjust threshold as needed
Delta <- rep(0, length(Debit_corrige))

if (length(NewSeqNA[NewSeqNA_threshold_indices]) != 0) {
  for (missing_idx in NewSeqNA[NewSeqNA_threshold_indices]) {
    # Ensure the previous 4 blocks of 2016 observations (1 week = 7 days * 24 hours * 12 observations-each-hour) are available and non-missing
    if (!is.na(Debit_corrige[missing_idx - 2016])    &&
        !is.na(Debit_corrige[missing_idx - 2016*2]) &&
        !is.na(Debit_corrige[missing_idx - 2016*3]) &&
        !is.na(Debit_corrige[missing_idx - 2016*4])) {
      
      # Construct a time series using the same day's data point at the same time from the previous 4 weeks (roughly one month of observations)
      history_ts <- ts(
        c(Debit_corrige[missing_idx - 2016*4],
          Debit_corrige[missing_idx - 2016*3],
          Debit_corrige[missing_idx - 2016*2],
          Debit_corrige[missing_idx - 2016]),
        start = 1, end = 4, frequency = 1
      )
      
      # Predict the missing value using auto.arima
      Debit_corrige[missing_idx] <- predict(auto.arima(history_ts), h = 1)$pred
      
      # Compute an error metric (Delta)
      Delta[missing_idx] <- 2.75 * sd(history_ts) / sqrt(4)
    }
  }
}

###############################################################################
# 4. Anomaly Detection and Correction Using qcc
###############################################################################
# Compute the difference in flow measurements and apply an x-bar chart to detect anomalies.

diff_debit <- rep(0, length(data[, 2]))
for (idx in 2:length(data[, 2])) {
  diff_debit[idx] <- data[idx, 2] - data[idx - 1, 2]
}

# Define segments of 288 points for a rolling x-bar chart
qcc_seq <- seq(289, length(diff_debit) - 288, 288)
correction_indices <- rep(0, length(qcc_seq))
k <- 0

for (start_idx in qcc_seq) {
  k <- k + 1
  # Create a qcc object for the segment [start_idx - 288, start_idx - 1]
  qcc_diff_debit <- qcc(
    data = diff_debit[(start_idx - 288):(start_idx - 1)],
    type = "xbar.one"
  )
  
  qcc_lims <- qcc_diff_debit$limits
  
  # Check the next 286 points for out-of-control conditions
  for (j in 1:286) {
    if (diff_debit[start_idx + j] >= qcc_lims[2] && diff_debit[start_idx + j + 1] <= qcc_lims[1]) {
      data[start_idx + j, 2] <- mean(c(data[start_idx + j + 1, 2], data[start_idx + j - 1, 2]))
      correction_indices[k] <- start_idx + j
    } else if (diff_debit[start_idx + j] <= qcc_lims[1] && diff_debit[start_idx + j + 1] >= qcc_lims[2]) {
      data[start_idx + j, 2] <- mean(c(data[start_idx + j + 2, 2], data[start_idx + j - 1, 2]))
      correction_indices[k] <- start_idx + j
    }
  }
}

###############################################################################
# 5. Write the Corrected Data and Delta Values
###############################################################################
# Replace "YOUR_OUTPUT_FILE.csv" with your desired output file path.
write.csv2(
  cbind(Debit_corrige, Delta),
  file = "YOUR_OUTPUT_FILE.csv",
  row.names = time_seq
)
