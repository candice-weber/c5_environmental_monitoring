# Download PurpleAir sensor data from API
library(tidyverse)
library(PurpleAir)

# Create functions to calculate US EPA AQI
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
start <- "2026-08-01"
end <- "2026-09-01"

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

# Download historical data between two time points
sensor_history <-
  get_sensor_history(
    sensor_index = index,
    fields = c("humidity",
               "temperature",
               "pressure",
               "pm2.5_atm"),
    average = "10min",
    start_timestamp = as.POSIXct(start),
    end_timestamp = as.POSIXct(end)
    )

# Combine sensor data and sensor history
sensor_combined <- cbind(sensor_data[rep(1, nrow(sensor_history)), , drop = FALSE], 
                sensor_history)

# Add calculated AQI and arrange columns
sensor_combined <- sensor_combined %>% 
  mutate(AQI = sapply(pm2.5_atm, aqiFromPM)) %>% 
  select(name, everything()) %>% 
  arrange(time_stamp)
  

# Write to CSV
file_name <- paste0(index, "_", start, "_to_", end, ".csv")
write.csv(sensor_combined, file = file_name, row.names = FALSE)
