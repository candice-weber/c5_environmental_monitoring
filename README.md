# c5_environmental_monitoring
Scripts for managing environmental monitoring data for C5 (Concerned Citizens of Charles City County)

## Air Sensor Data
*Archive_and_QA_Sensor_Data.R*   

This code will download sensor data using the PurpleAir API.  You will need to get an API key first by registering for a PurpleAir developer account.  PurpleAir issues tokens/credits that are used up each time you use your API key to download data.  If you are downloading data for C5's own sensors you can email PurpleAir and ask them to give you more tokens/credits if you run out.  
  
Set the start date and end date of the time period you want to download in lines 48 and 49.  
Set the index number of the sensor you want to download data for in line 52. You can find the index number for your sensor on the website https://www.purpleair.com/my-sensors.  
  
This code reviews the downloaded data and produces a QA report for the following:  
- Data completeneess (identifies any missing timestamps)
- Downtime (lists any gaps in the data greater than 10 minutes)
- Missing PM2.5 values
- Duplicate timestamps
- Deviations between Channel A and Channel B PM2.5 readings greater than 2.5 ug/m3
- Average deviation between Channel A and Channel B PM2.5 readings


