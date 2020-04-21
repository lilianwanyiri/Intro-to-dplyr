#Main functions 
#group_by
#summarise
#mutate
#filter
#arrange
#select 

##load required libraries
#install.packages("dplyr")
library(dplyr)
library(readr)

##Read in the data
guinea_data <-  read_csv("data/guinea_two_stations.csv")

glimpse(guinea_data)
str(guinea_data)
summary(guinea_data)

##Filter to remain with only one station filter()
levels(factor(guinea_data$station))

#data from kankan station
kankan_data <- guinea_data %>%  
  filter(station == "Kankan")

#data from koundara station 
koundara_data <- guinea_data   %>% 
  filter(station == "Koundara")


##Filter to remain with one station for a specific year 

kankan_1950 <-  guinea_data %>% 
  filter(station == "Kankan" & year == 1950)

##Filter to remain with one station with only rainy days (rain>=1mm)

guinea_rainydays <- guinea_data %>% 
  filter(rain>=1)

##Select few columns, station, year,month,day, rain : select()

rain_data <-  guinea_data %>% 
  select(station, year, month, day, rain)

##Exercise 2: Select station, year,month,day, tmin and tmax
temp_data <- guinea_data %>% 
  select(station, year,month,day, tmax,tmin)

##Select a dataframe without rel_hum (-)
rain_temp <- guinea_data %>% 
  select(-rel_hum)

rain_temp_kankan <- guinea_data %>%
  filter(station == "Kankan") %>% 
  select(-rel_hum)

##sort rain in ascending order : arrange()
guinea_data_asc <- guinea_data %>% 
  arrange(rain)

## Exercise 2: sort rain in descending order
guinea_data_desc <- guinea_data %>% 
  arrange(desc(rain))

##Add a new column defining a rainy day as binary (1,0)  WMO 1mm: mutate()

guinea_data <- guinea_data %>% 
  mutate(rain_day = ifelse(rain>=1, 1, 0))

## Exercise 3: Create new date using lubridate::make_date()

guinea_data <-  guinea_data %>% 
  mutate(date = lubridate::make_date(year, month, day))

## Obtain Annual summaries
annual_summaries <-  guinea_data %>% 
  group_by(station, year) %>% 
  summarise(total_rain = sum(rain), mean_rain = mean(rain), sd_rain = sd(rain), mean_tmax = mean(tmax))

##Exercise 4: what about monthly summaries?

monthly_summaries <- guinea_data %>% 
  group_by(station,year,month) %>% 
  summarise(total_rain = sum(rain) , mean_tmax = mean(tmax)  , mean_tmin = mean(tmin) )


##conclusion


