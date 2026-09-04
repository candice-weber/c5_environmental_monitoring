# QA sensor data - code written with use of Claude Haiku 4.5

library(tidyverse)

# Load in the data you are reviewing
data <- read.csv("159355_2026-08-01_to_2026-09-01.csv")

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

# Create function to identify continuous data gaps (akak downtime)
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
qa_purpleair_monthly <- function(data, timestamp_col = "time_stamp", interval_minutes = 10) {
  
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
  
  cat("ADDITIONAL CHECKS:\n")
  cat("- Null/NA values in PM2.5:", sum(is.na(data$pm2.5_atm)), "\n")
  cat("- Duplicate timestamps:", sum(duplicated(data[[timestamp_col]])), "\n")
  cat("- Date range:", 
      format(min(data[[timestamp_col]]), "%Y-%m-%d %H:%M:%S"), 
      "to", 
      format(max(data[[timestamp_col]]), "%Y-%m-%d %H:%M:%S"), 
      "\n")
  
  invisible(list(gaps = gaps, downtime = downtime))
}

# Run monthly QA
qa_results <- qa_purpleair_monthly(data)
