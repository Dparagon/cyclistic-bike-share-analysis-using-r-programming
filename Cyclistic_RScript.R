                    # COURSERA Case Study (Cyclistic Bike_Share) #

#Loading Libraries
library(tidyverse)
library(janitor)
library(skimr)

# 1. DATA IMPORTATION and PREPARATION

#- Data location
setwd("D:/R Activity/COURSERA Case Study")
#- Data Importation
tripdata1 <- read_csv("202101-divvy-tripdata.csv")
tripdata2 <- read_csv("202102-divvy-tripdata.csv")
tripdata3 <- read_csv("202103-divvy-tripdata.csv")
tripdata4 <- read_csv("202104-divvy-tripdata.csv")
tripdata5 <- read_csv("202105-divvy-tripdata.csv")
tripdata6 <- read_csv("202106-divvy-tripdata.csv")
tripdata7 <- read_csv("202107-divvy-tripdata.csv")
tripdata8 <- read_csv("202108-divvy-tripdata.csv")
tripdata9 <- read_csv("202109-divvy-tripdata.csv")
tripdata10 <- read_csv("202110-divvy-tripdata.csv")
tripdata11 <- read_csv("202111-divvy-tripdata.csv")
tripdata12 <- read_csv("202112-divvy-tripdata.csv")
#- Data Merging
cyclistic_tripdata <- rbind(tripdata1,
                            tripdata2,
                            tripdata3,
                            tripdata4,
                            tripdata5,
                            tripdata6,
                            tripdata7,
                            tripdata8,
                            tripdata9,
                            tripdata10,
                            tripdata11,
                            tripdata12)
#- Data Overview and Structure
count(cyclistic_tripdata) 
View(cyclistic_tripdata)
str(cyclistic_tripdata)
skim_without_charts(cyclistic_tripdata)

# DATA CLEANING

#- Removing unnecessary columns
cyclistic_new_tripdata <- cyclistic_tripdata %>% 
                          select(-start_lat, -start_lng, -end_lat, -end_lng)
colnames(cyclistic_new_tripdata)
#- Deleting null values
cyclistic_new_tripdata <- cyclistic_new_tripdata %>% 
                          drop_na()

count(cyclistic_new_tripdata) 
#- Renaming columns header
cyclistic_new_tripdata <- cyclistic_new_tripdata %>% 
      rename(bike_type = rideable_type,
             user_type = member_casual)

View(cyclistic_new_tripdata)
#- Renaming 'docked_bike' to 'classic_bike' in bike_type column
cyclistic_new_tripdata$bike_type <- str_replace(cyclistic_new_tripdata$bike_type,
                                                "docked_bike", "classic_bike")

skim_without_charts(cyclistic_new_tripdata)

# DATA MANIPULATION
View(cyclistic_new_tripdata)

#- Creating new columns
minutes <- interval(cyclistic_new_tripdata$started_at, cyclistic_new_tripdata$ended_at)
ride_min <- time_length(minutes, "minute") #The minutes used for ride
print(ride_min)

ride_time <- hour(cyclistic_new_tripdata$started_at) #Time of the day for ride
print(ride_time)

ride_day <- wday(cyclistic_new_tripdata$started_at, label = T) #The week name of ride
print(ride_day)

ride_month <- month(cyclistic_new_tripdata$started_at , label = T) #The month name of ride
print(ride_month)

ride_quarter <- quarter(cyclistic_new_tripdata$started_at) #Quarterly period of ride
print(ride_quarter)
#- Joining new columns to data
cyclistic_clean_data <- cyclistic_new_tripdata %>% 
                        cbind(ride_min, ride_time, ride_day, ride_month, ride_quarter)
print(cyclistic_clean_data)
#- Checking for time errors
time_error <- cyclistic_clean_data %>% 
              filter(ride_min <= 1 | ride_min >= 1440)
count(time_error)  

time_error2 <- cyclistic_clean_data %>% 
               filter(started_at > ended_at) 
count(time_error2) 
#- Deleting time errors
cyclistic_clean_data <- cyclistic_clean_data[!(cyclistic_clean_data$ride_min <= 1 | 
                                               cyclistic_clean_data$ride_min >= 1440),]
cyclistic_clean_data <- cyclistic_clean_data[!(cyclistic_clean_data$started_at > 
                                               cyclistic_clean_data$ended_at),]

count(cyclistic_clean_data) 

# DATA OVERVIEW (After Cleaning and Manipulation)

View(cyclistic_clean_data)
skim_without_charts(cyclistic_clean_data)

# SAVING DATA

write_csv(cyclistic_clean_data, "cyclistic_clean_data")