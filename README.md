# RUDRA-ASSIGNMENNT-WEEK-4
City Weather Analysis using R Programming
# Introduction:
The given R program performs a comprehensive analysis of weather data for three cities: Bhimavaram, Undi, and Akiveedu. The dataset includes temperature, humidity, and rainfall values recorded daily. The program follows a structured workflow:

Data Import & Inspection – Reads the dataset, checks its structure, and handles missing values.

Data Cleaning & Manipulation – Filters for the selected cities, converts temperature to Fahrenheit, formats dates, and computes weekly averages.

Data Analysis – Identifies the hottest and coldest cities, finds the days with maximum rainfall in each city, and compares average humidity levels.

Data Visualization – Uses line graphs, bar charts, and boxplots to display trends and variations across cities for temperature, rainfall, and humidity.

This systematic approach allows us to understand both daily variations and weekly aggregated patterns across different climatic parameters.

# Conclusion:
From the analysis and visualizations:

The program successfully highlights which city is the hottest and coldest during the selected period.

It identifies the specific days with extreme rainfall, which can be useful for weather forecasting and disaster management.

Average humidity comparisons and boxplots show how atmospheric conditions vary between cities.

The line graph reveals daily temperature trends, the bar plot emphasizes weekly rainfall differences, and the boxplot summarizes humidity distributions.

Overall, this R program demonstrates how data manipulation, statistical summarization, and visualization techniques can be combined to gain meaningful insights from weather data.

# code of weather analysis

# Task 1: Data Import & Inspection
weather <- read.csv("C:/Users/rudra/OneDrive/wheather.csv/wheather.csv.csv")
head(weather)
colnames(weather)
sum(is.na(weather))
# Task 2: Data Cleaning & Manipulation
cities <- c("Bhimavaram", "Undi", "Akiveedu")
weather <- subset(weather, CITY %in% cities)
# Convert Temperature to Fahrenheit
weather$TEMP_F <- weather$TEMPERATURE * 9/5 + 32
# Convert Date to Date format
weather$DATE<- as.Date(weather$DATE)
# Load dplyr for manipulation
library(dplyr)
weekly_avg <- weather %>%
  mutate(Week = format(DATE, "%Y-%U")) %>%
  group_by(CITY, Week) %>%
  summarise(
    Avg_Temp = mean(TEMPERATURE),
    Avg_HUMMIDITY = mean(HUMMIDITY),
    Avg_RAINFALL = mean(RAINFALL)
  )
head(weekly_avg)
# Task 3: Data Analysis
#Hottest and coldest city
avg_CITY_temp <- weather %>%
  group_by(CITY) %>%
  summarise(Mean_Temp = mean(TEMPERATURE))
hottest <- avg_CITY_temp[which.max(avg_CITY_temp$Mean_Temp),]
coldest <- avg_CITY_temp[which.min(avg_CITY_temp$Mean_Temp),]
hottest
coldest
# Day with highest rainfall in each city
max_rain <- weather %>%
  group_by(CITY) %>%
  filter(RAINFALL == max(RAINFALL))
max_rain
# Compare average humidity
avg_HUMMIDITY <- weather %>%
  group_by(CITY) %>%
  summarise(Mean_HUMMIDITY = mean(HUMMIDITY))
avg_HUMMIDITY
# Task 4: Visualization
# 1. Line graph of temperature trends
  library(ggplot2)
  ggplot(weather, aes(x = DATE, y = TEMPERATURE, color = CITY, group = CITY)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(title = "Daily Temperature Trends",
         x = "Date",
         y = "Temperature (°C)") +
    theme_minimal()
# 2. Bar plot of weekly average rainfall
ggplot(weekly_avg, aes(x=Week, y=Avg_RAINFALL, fill=CITY)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Weekly Average Rainfall", y="RAINFALL (mm)")
# 3. Boxplot for humidity distribution
ggplot(weather, aes(x=CITY, y=HUMMIDITY, fill=CITY)) +
  geom_boxplot() +
  labs(title="Humidity Distribution Across Cities", y="HUMMIDITY (%)")

