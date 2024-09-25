# Load necessary libraries
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)


file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

# Empty list to store data for each year
all_years_data <- list()

# Loop through the years from 1985 to 2023
for (year in 1985:2023) {
  # Construct the file path for the current year
  path <- paste0(file_root, year, tail)
  
  # Read the header
  header <- scan(path, what = 'character', nlines = 1)
  
  # Read the buoy data, skipping the appropriate number of rows (2 for data + units row)
  buoy <- fread(path, header = FALSE, skip = 2, fill = TRUE)
  
  # Adjust header if it has more columns than the data
  if (length(header) > ncol(buoy)) {
    header <- header[1:ncol(buoy)]  # Truncate the header to match the data
  } else if (ncol(buoy) > length(header)) {
    buoy[, extra_col := NA]  # Add a placeholder column if data has more columns
  }
  
  # Set the column names
  colnames(buoy) <- header
  
  # Combine year (YY or YYYY) into a single 'YEAR' column
  # Logic: For years after 2006, #YY represents years starting from 2007
  buoy$YEAR <- ifelse(year >= 2007, as.numeric(buoy$`#YY`), 
                      ifelse(as.numeric(buoy$YY) < 100, as.numeric(buoy$YY) + 1900, as.numeric(buoy$YY)))
  
  # Add a proper date column using lubridate
  buoy$date <- ymd_hms(paste(buoy$YEAR, buoy$MM, buoy$DD, buoy$hh, buoy$mm))
  
  # Append the data to the list for all years
  all_years_data[[as.character(year)]] <- buoy
}

# Combine all years' data into one data.table
all_data <- rbindlist(all_years_data, fill = TRUE)

# Replace all occurrences of 99/999/9999 with NA (excluding the YEAR column)
# Define missing value placeholders for relevant columns
missing_values <- c(999, 999.0, 9999, 99.0, 99.00)

# Columns to check for missing values
columns_to_check <- setdiff(colnames(all_data), "YEAR")  # All except 'YEAR'

# Loop through columns and replace 99, 999, 9999 with NA
for (col in columns_to_check) {
  all_data[get(col) %in% missing_values, (col) := NA]
}

# Ensure that NA's were applied correctly
summary(all_data)

# Summarize the number of NAs for WDIR by year and plot the number of NAs over time
na_summary_by_year <- all_data %>%
  group_by(YEAR) %>%
  summarise(WDIR_NA_count = sum(is.na(WDIR)),
            total_count = n())

# Plot the number of NAs over time
ggplot(na_summary_by_year, aes(x = YEAR, y = WDIR_NA_count)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Number of Missing WDIR Values Over Time", 
       x = "Year", 
       y = "Count of NAs") +
  theme_minimal()

# View the unique years to confirm all years are present
unique(all_data$YEAR)

#(c)
# key climate indicators (ATMP: Air Temperature, WTMP: Water Temperature)

# Summarize the annual average air and water temperatures
temp_summary_by_year <- all_data %>%
  group_by(YEAR) %>%
  summarise(
    avg_air_temp = mean(ATMP, na.rm = TRUE),
    avg_water_temp = mean(WTMP, na.rm = TRUE)
  )

# Plot the trends over time for both air and water temperatures
ggplot(temp_summary_by_year, aes(x = YEAR)) +
  geom_line(aes(y = avg_air_temp, color = "Air Temperature")) +
  geom_line(aes(y = avg_water_temp, color = "Water Temperature")) +
  labs(title = "Annual Average Air and Water Temperature (1985–2023)",
       x = "Year", 
       y = "Temperature (°C)",
       color = "Legend") +
  theme_minimal()

#  Fit linear regression models to check for temperature trends over time 
# Air Temperature
air_temp_lm <- lm(avg_air_temp ~ YEAR, data = temp_summary_by_year)
summary(air_temp_lm)

# Water Temperature
water_temp_lm <- lm(avg_water_temp ~ YEAR, data = temp_summary_by_year)
summary(water_temp_lm)

# Display regression lines on the plots
ggplot(temp_summary_by_year, aes(x = YEAR)) +
  geom_point(aes(y = avg_air_temp, color = "Air Temperature")) +
  geom_smooth(aes(y = avg_air_temp, color = "Air Temperature"), method = "lm", se = FALSE) +
  geom_point(aes(y = avg_water_temp, color = "Water Temperature")) +
  geom_smooth(aes(y = avg_water_temp, color = "Water Temperature"), method = "lm", se = FALSE) +
  labs(title = "Trends in Air and Water Temperature (1985–2023)",
       x = "Year", 
       y = "Temperature (°C)",
       color = "Legend") +
  theme_minimal()

# Calculate the annual temperature anomalies (difference from the mean) for further analysis
# This can help highlight any abnormal years or extreme events
temp_summary_by_year <- temp_summary_by_year %>%
  mutate(
    air_temp_anomaly = avg_air_temp - mean(avg_air_temp, na.rm = TRUE),
    water_temp_anomaly = avg_water_temp - mean(avg_water_temp, na.rm = TRUE)
  )

# Plot the anomalies over time
ggplot(temp_summary_by_year, aes(x = YEAR)) +
  geom_bar(aes(y = air_temp_anomaly, fill = "Air Temperature"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = water_temp_anomaly, fill = "Water Temperature"), stat = "identity", position = "dodge") +
  labs(title = "Temperature Anomalies (1985–2023)",
       x = "Year", 
       y = "Temperature Anomaly (°C)",
       fill = "Legend") +
  theme_minimal()


#(d)


# Load the rainfall data
rainfall_data <- read.csv("Rainfall.csv")

head(rainfall_data$DATE)

# Step 1: Parse the DATE column using ymd_hm
rainfall_data$DATE <- ymd_hm(rainfall_data$DATE)  # Parse the date using the format 'YYYYMMDD HH:MM'

# Step 2: Extract the year from the parsed DATE column
rainfall_data$YEAR <- year(rainfall_data$DATE)

# Step 3: Summarize total rainfall by year
rainfall_by_year <- rainfall_data %>%
  group_by(YEAR) %>%
  summarise(total_rainfall = sum(HPCP, na.rm = TRUE))  # Sum the total rainfall for each year

# Check the summary to ensure it worked
print(head(rainfall_by_year))


buoy_data_by_year <- all_data %>%
  filter(YEAR >= 1985 & YEAR <= 2013) %>%  # Filter buoy data to match rainfall data's year range
  group_by(YEAR) %>%
  summarise(
    avg_air_temp = mean(ATMP, na.rm = TRUE),      # Average Air Temperature
    avg_water_temp = mean(WTMP, na.rm = TRUE),    # Average Water Temperature
    avg_wind_speed = mean(WSPD, na.rm = TRUE)     # Average Wind Speed
  )

combined_data <- left_join(rainfall_by_year, buoy_data_by_year, by = "YEAR")

# Plot 1: Total Rainfall vs Air Temperature (Yearly Data)
ggplot(combined_data, aes(x = avg_air_temp, y = total_rainfall)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Total Rainfall vs Air Temperature (Yearly, 1985–2013)",
       x = "Average Air Temperature (°C)",
       y = "Total Rainfall (inches)") +
  theme_minimal()

# Plot 2: Total Rainfall vs Water Temperature (Yearly Data)
ggplot(combined_data, aes(x = avg_water_temp, y = total_rainfall)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Total Rainfall vs Water Temperature (Yearly, 1985–2013)",
       x = "Average Water Temperature (°C)",
       y = "Total Rainfall (inches)") +
  theme_minimal()

# Plot 3: Total Rainfall vs Wind Speed (Yearly Data)
ggplot(combined_data, aes(x = avg_wind_speed, y = total_rainfall)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Total Rainfall vs Wind Speed (Yearly, 1985–2013)",
       x = "Average Wind Speed (m/s)",
       y = "Total Rainfall (inches)") +
  theme_minimal()

# Step 4: Trend of Rainfall and Air Temperature over Years
ggplot(combined_data, aes(x = YEAR)) +
  geom_line(aes(y = total_rainfall, color = "Rainfall")) +
  geom_line(aes(y = avg_air_temp * 10, color = "Air Temperature (x10)")) + # Scaling air temp for comparison
  labs(title = "Yearly Trend of Rainfall and Air Temperature (1985–2013)",
       x = "Year",
       y = "Value") +
  scale_color_manual(values = c("Rainfall" = "blue", "Air Temperature (x10)" = "red")) +
  theme_minimal()

# Step 5: Trend of Rainfall and Water Temperature over Years
ggplot(combined_data, aes(x = YEAR)) +
  geom_line(aes(y = total_rainfall, color = "Rainfall")) +
  geom_line(aes(y = avg_water_temp * 10, color = "Water Temperature (x10)")) + # Scaling water temp for comparison
  labs(title = "Yearly Trend of Rainfall and Water Temperature (1985–2013)",
       x = "Year",
       y = "Value") +
  scale_color_manual(values = c("Rainfall" = "blue", "Water Temperature (x10)" = "green")) +
  theme_minimal()