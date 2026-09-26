# Download PurpleAir sensor data from API
# Load packages
library(tidyverse)
library(PurpleAir)

# Create functions to calculate US EPA AQI, from PurpleAir Community
aqiFromPM <- function(pm) {
  #                                     AQI         RAW PM2.5
  # Good                               0 - 50   |   0.0 – 12.0
  # Moderate                          51 - 100  |  12.1 – 35.4
  # Unhealthy for Sensitive Groups   101 – 150  |  35.5 – 55.4
  # Unhealthy                        151 – 200  |  55.5 – 150.4
  # Very Unhealthy                   201 – 300  |  150.5 – 250.4
  # Hazardous                        301 – 400  |  250.5 – 350.4
  # Hazardous                        401 – 500  |  350.5 – 500.4
  if (pm > 350.5) {
    return(calcAQI(pm, 500, 401, 500.4, 350.5))  # Hazardous
  } else if (pm > 250.5) {
    return(calcAQI(pm, 400, 301, 350.4, 250.5))  # Hazardous
  } else if (pm > 150.5) {
    return(calcAQI(pm, 300, 201, 250.4, 150.5))  # Very Unhealthy
  } else if (pm > 55.5) {
    return(calcAQI(pm, 200, 151, 150.4, 55.5))   # Unhealthy
  } else if (pm > 35.5) {
    return(calcAQI(pm, 150, 101, 55.4, 35.5))    # Unhealthy for Sensitive Groups
  } else if (pm > 12.1) {
    return(calcAQI(pm, 100, 51, 35.4, 12.1))     # Moderate
  } else if (pm >= 0) {
    return(calcAQI(pm, 50, 0, 12, 0))            # Good
  } else {
    return(undefined)
  }
}
calcAQI <- function(Cp, Ih, Il, BPh, BPl) {
  a <- (Ih - Il)
  b <- (BPh - BPl)
  c <- (Cp - BPl)
  return(round((a/b) * c + Il))
}

# Add PurpleAir API key to environment
Sys.setenv(PURPLE_AIR_API_KEY = "800BD95C-6DB2-11F1-B596-4201AC1DC123")

# Check which API key is in the environment
# check_api_key()

# Set start/end time points
start <- "2026-07-23"
end <- "2026-08-01"

# Set sensor index number
index <- 159355

# Get data about the sensor
sensor_data <- as.data.frame(get_sensor_data(sensor_index = index,
                fields = c("name",
                           "latitude",
                           "longitude",
                           "last_seen")
                )
)

# Download historical data between two time points for both Channel A and Channel B
sensor_history <-
  get_sensor_history(
    sensor_index = index,
    fields = c("humidity",
               "temperature",
               "pressure",
               "pm2.5_atm_a",
               "pm2.5_atm_b"),
    average = "10min",
    start_timestamp = as.POSIXct(start),
    end_timestamp = as.POSIXct(end)
    )

# Combine sensor data and sensor history
data <- cbind(sensor_data[rep(1, nrow(sensor_history)), , drop = FALSE], 
                sensor_history)

# Add column calculating the difference between the two channel readings
data$difference <- data$pm2.5_atm_a - data$pm2.5_atm_b

# Add column averaging the readings between Channel A and B
data$pm2.5_average = ((data$pm2.5_atm_a + data$pm2.5_atm_b)/2)

# Add calculated AQI and arrange columns
data <- data %>%
  mutate(AQI = sapply(pm2.5_average, aqiFromPM)) %>% 
  select(name, everything()) %>% 
  arrange(time_stamp)

# Write to CSV
file_name <- paste0(index, "_", start, "_to_", end, ".csv")
write.csv(data, file = file_name, row.names = FALSE)

# QA review of sensor data - portions of below code written with use of Claude Haiku 4.5
# Convert timestamp to datetime, accounting for midnight on each day
data$time_stamp <- as.POSIXct(ifelse(
  nchar(data$time_stamp) > 10,
  data$time_stamp,
  paste(data$time_stamp, "00:00:00")),
  format = "%Y-%m-%d %H:%M:%S")

# Sort by timestamp
data <- data %>% arrange(time_stamp)

# Create function to compare expected time intervals to actual time intervals
check_data_gaps <- function(data, timestamp_col = "time_stamp", interval_minutes = 10) {
  
  # Get date range
  start_time <- min(data[[timestamp_col]], na.rm = TRUE)
  end_time <- max(data[[timestamp_col]], na.rm = TRUE)
  
  # Create expected sequence of timestamps
  expected_times <- seq(start_time, end_time, by = paste(interval_minutes, "mins"))
  
  # Find which expected times are missing
  missing_times <- setdiff(expected_times, data[[timestamp_col]])
  
  # Calculate statistics
  total_expected <- length(expected_times)
  total_observed <- nrow(data)
  total_missing <- length(missing_times)
  percent_complete <- round((total_observed / total_expected) * 100, 2)
  
  # Return summary and details
  list(
    summary = data.frame(
      start_time = start_time,
      end_time = end_time,
      total_expected = total_expected,
      total_observed = total_observed,
      total_missing = total_missing,
      percent_complete = percent_complete
    ),
    missing_times = as.data.frame(missing_times) %>% 
      rename(timestamp = missing_times) %>%
      mutate(gap_duration_mins = interval_minutes)
  )
}

# Create function to identify continuous data gaps (aka downtime)
identify_downtime_periods <- function(missing_times_df, interval_minutes = 10) {
  
  if (nrow(missing_times_df) == 0) {
    return(data.frame(start = NA, end = NA, duration_hours = NA)[FALSE, ])
  }
  
  missing_times_df <- missing_times_df %>%
    arrange(timestamp) %>%
    mutate(
      # Calculate time difference from previous missing observation
      time_diff = as.numeric(difftime(timestamp, lag(timestamp), units = "mins")),
      # Mark when a new gap period starts (when diff > interval)
      gap_group = cumsum(is.na(time_diff) | time_diff > interval_minutes)
    ) %>%
    group_by(gap_group) %>%
    summarise(
      start = first(timestamp),
      end = last(timestamp),
      duration_hours = round(as.numeric(difftime(end, start, units = "hours")), 2),
      .groups = "drop"
    ) %>%
    select(start, end, duration_hours) %>%
    arrange(start)
  
  return(missing_times_df)
}

# Create function to present a user friendly summery of any data gaps
qa_purpleair_monthly <- function(data, timestamp_col = "time_stamp", 
                                 interval_minutes = 10, export_file = NULL) {
  
  # If export_file is provided, redirect output to file
  if (!is.null(export_file)) {
    sink(export_file)
  }
  
  cat("=== PurpleAir Monthly QA Report ===\n\n")
  
  gaps <- check_data_gaps(data, timestamp_col, interval_minutes)
  
  cat("DATA COMPLETENESS:\n")
  print(gaps$summary)
  cat("\n")
  
  if (nrow(gaps$missing_times) == 0) {
    cat("No gaps detected - data is complete.\n\n")
  } else {
    cat("DOWNTIME PERIODS DETECTED:\n")
    downtime <- identify_downtime_periods(gaps$missing_times)
    print(downtime)
    cat("\n")
  }
  
  channels_disagree <- data[data$difference > 2.5 | data$difference < -2.5, ]
  
  cat("ADDITIONAL CHECKS:\n")
  cat("- Null/NA values in PM2.5_atm_a:", sum(is.na(data$pm2.5_atm_a)), 
      "| PM2.5_atm_b:", sum(is.na(data$pm2.5_atm_b)), "\n")
  cat("- Duplicate timestamps:", sum(duplicated(data[[timestamp_col]])), "\n")
  cat("- Date range:", 
      format(min(data[[timestamp_col]]), "%Y-%m-%d %H:%M:%S"), 
      "to", 
      format(max(data[[timestamp_col]]), "%Y-%m-%d %H:%M:%S"), 
      "\n")
  cat("- Channel A and B readings deviate +/- 2.5 ug/m:", nrow(channels_disagree),
      "instances\n")
  cat("- Average deivation Channel A and B readings:", sum(data$difference)/nrow(data))
  
  # Close the file connection if one was opened
  if (!is.null(export_file)) {
    sink()
  }
  
}

# Run monthly QA
qa_purpleair_monthly(data, export_file = paste0(start, "_to_", end, "_qa_report.txt"))
