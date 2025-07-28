
#rm()

library(dplyr)
library(lubridate)

#getting data from each area of ontario

#south
setwd("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/South_ON")  # Replace with the actual path

south_files <- list.files(pattern = "\\.csv$")


#north

setwd("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/North_ON")  # Replace with the actual path

north_files <- list.files(pattern = "\\.csv$")

#east 

setwd("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/East_ON")  # Replace with the actual path

east_files <- list.files(pattern = "\\.csv$")

#west 

setwd("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/West_ON")  # Replace with the actual path

west_files <- list.files(pattern = "\\.csv$")

#central 

setwd("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/Central_ON")  # Replace with the actual path

central_files <- list.files(pattern = "\\.csv$")

#extra 
setwd("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/Extra")  # Replace with the actual path

extra_files <- list.files(pattern = "\\.csv$")

list(north_files)
list(south_files)
list(east_files)
list(west_files)
list(central_files)
list(extra_files)


# Read files from each region with full paths
south_data <- do.call(rbind, lapply(south_files, function(f) {
  read.csv(file.path("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/South_ON", f))
}))

north_data <- do.call(rbind, lapply(north_files, function(f) {
  read.csv(file.path("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/North_ON", f))
}))

east_data <- do.call(rbind, lapply(east_files, function(f) {
  read.csv(file.path("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/East_ON", f))
}))

west_data <- do.call(rbind, lapply(west_files, function(f) {
  read.csv(file.path("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/West_ON", f))
}))

central_data <- do.call(rbind, lapply(central_files, function(f) {
  read.csv(file.path("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/Central_ON", f))
}))

extra_data <- do.call(rbind, lapply(extra_files, function(f) {
  read.csv(file.path("C:/Users/mckay/OneDrive - University of Guelph/Masters/DATA 6500/Final Project/Weather Data/Extra", f))
}))

weather_data <- rbind(south_data, north_data, east_data, west_data, central_data, extra_data)

head(weather_data)

#cleaning

#removing duplicates 
weather_data <- weather_data[!duplicated(weather_data), ]
sum(duplicated(weather_data))


#cleaning column namesand missing data
names(weather_data) <- tolower(names(weather_data))
missing_prop <- colMeans(is.na(weather_data))
weather_data <- weather_data[, missing_prop < 0.9]

numeric_cols <- c(
  "x", "y",
  "mean_temperature", "min_temperature", "max_temperature",
  "total_precipitation", "total_rain", "total_snow", "snow_on_ground",
  "direction_max_gust", "speed_max_gust",
  "cooling_degree_days", "heating_degree_days",
  "min_rel_humidity", "max_rel_humidity"
)

# Convert selected columns to numeric
weather_data <- weather_data %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(.)))

#weather_data <- weather_data %>%
  #select(-ends_with("_flag"))

#interpolating any missing values 
weather_data <- weather_data %>%
  mutate(across(where(is.numeric), ~ zoo::na.approx(., na.rm = FALSE)))

str(weather_data)
head(weather_data)

#aggregating the weather data by month per year
#Ensure your date column is a proper Date type
weather_data <- weather_data %>%
  mutate(local_date = as.Date(local_date))  # or use ymd() if needed

# Add year and month columns
weather_data <- weather_data %>%
  mutate(
    year = year(local_date),
    month = month(local_date)
  )

# Aggregate by station, year, and month
monthly_weather <- weather_data %>%
  group_by(station_name, x, y, year, month) %>%
  summarise(
    mean_temp = mean(mean_temperature, na.rm = TRUE),
    max_temp = mean(max_temperature, na.rm = TRUE),
    min_temp = mean(min_temperature, na.rm = TRUE),
    total_precip = mean(total_precipitation, na.rm = TRUE),
    min_humidity = mean(min_rel_humidity, na.rm = TRUE),
    max_humidity = mean(max_rel_humidity, na.rm = TRUE),
    .groups = "drop"
  )

head(monthly_weather)

#write.csv(weather_data, "weather_data_full.csv", row.names = FALSE)
write.csv(monthly_weather, "monthly_weather.csv", row.names = FALSE)