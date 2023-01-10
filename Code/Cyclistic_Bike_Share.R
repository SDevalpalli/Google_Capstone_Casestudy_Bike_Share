## Install and load the necessary packages and libraries

install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
library(readxl)
install.packages("dplyr")
library(dplyr)
library(scales)
library(lubridate)
install.packages("ggplot2")
library(ggplot2)



## Reading the excel files

dfFeb <- read_excel("ProcessedData/Feb2022.xlsx")
dfMar <- read_excel("ProcessedData/Mar2022.xlsx")
dfApr <- read_excel("ProcessedData/Apr2022.xlsx")
dfMay <- read_excel("ProcessedData/May2022.xlsx")
dfJun <- read_excel("ProcessedData/Jun2022.xlsx")
dfJul <- read_excel("ProcessedData/Jul2022.xlsx")
dfAug <- read_excel("ProcessedData/Aug2022.xlsx")
dfSep <- read_excel("ProcessedData/Sep2022.xlsx")
dfOct <- read_excel("ProcessedData/Oct2022.xlsx")
dfNov <- read_excel("ProcessedData/Nov2022.xlsx")
dfDec <- read_excel("ProcessedData/Dec2022.xlsx")

## Getting the column names of all the datasets

colnames(dfFeb)
colnames(dfMar)
colnames(dfApr)
colnames(dfJun)
colnames(dfJul)
colnames(dfAug)
colnames(dfSep)
colnames(dfOct)
colnames(dfNov)
colnames(dfDec)

## inspect the dataframes and check for any inconguencies

str(dfFeb)
str(dfMar)
str(dfApr)
str(dfMay)
str(dfJun)
str(dfJul)
str(dfAug)
str(dfSep)
str(dfOct)
str(dfNov)
str(dfDec)

## çonvert end_station_id in dfSep to char from num

dfSep <- mutate(dfSep, end_station_id = as.character(end_station_id))

## Nopw merging all dataframes into one dataframe

all_rides <- bind_rows(dfFeb,dfMar,dfApr,dfMay,dfJun,dfJul,dfAug,dfSep,dfOct,dfNov,dfDec)

## Remove Start_lat, start_lng,end_lat,end_lng columns from the dataframe 
## as I don't need this data for this analysis and wanted a simplied view

all_rides <- all_rides %>% 
  select(-c(start_lat,start_lng,end_lat, end_lng))

##################################################
## Clean up the data and make ready for analysis##
##################################################

## Now inspect the new table that has been created

colnames(all_rides) ## column names
nrow(all_rides)     ## no. of rows
dim(all_rides)      ## dimensions of the dataframe
head(all_rides)     ## diaplay the first 6 rows
str(all_rides)     ## list of the columns and datatypes
summary(all_rides) ## Statistical summary of the data
table(all_rides$member_casual) ## to check if the number of rows matches the results.

## Now add new columns with date, day, month ,year and day_of_week

all_rides$date <- as.Date(all_rides$started_at)
all_rides$day <- format(as.Date(all_rides$date),"%d")
all_rides$month <- format(as.Date(all_rides$date), "%m")
all_rides$year <- format(as.Date(all_rides$date),"%Y")
all_rides$day_of_week <- format(as.Date(all_rides$date), "%A")
all_rides$month_name <- month.name[as.numeric(all_rides$month)]
all_rides$month_name <- factor(all_rides$month_name, levels=month.name)

## add ride_length to the dataframe, calculated as ended_at - started_at

all_rides$ride_length <- difftime(all_rides$ended_at,all_rides$started_at)

## inspect the structure of the column

str(all_rides)

## Create a new dataframe where the ride_length is not negative as it is saved as version 2 all_rides_v2

all_rides_v2 <- all_rides[!(all_rides$ride_length<0)]

## Check if ride_length is a factor or numeric

is.factor(all_rides_v2$ride_length)
is.numeric(all_rides_v2$ride_length)

## Convert to numeric as it is not numeric

all_rides_v2$ride_length <- as.numeric(as.character(all_rides_v2$ride_length))
is.numeric(all_rides_v2$ride_length)

## summarise the datframe

summary(all_rides_v2$ride_length)
aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual, FUN = mean)
aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual, FUN = median)
aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual, FUN = max)
aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual, FUN = min)

aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$day_of_week, FUN = mean)
aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$day_of_week, FUN = median)
aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$day_of_week, FUN = max)
aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$day_of_week, FUN = min)

## sort and add the weekday column, group by member_casual and weekday,soretd by member_casual and weekday

all_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	%>%       # sorts
  
  ####################################################################################
##                                 Visualization                                  ##
####################################################################################

##    Observations by week

## Plot 1 : Analysis and Visualization for average duration on the days in every week of the year
#################################################################################################

all_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)	%>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Average Duration of the Rides by Day of the Week",
    subtitle = "Average duration of rides for every week",
    caption = "Fig 1",
    x = "day of the week",
    y = "average duration of rides"
  )+
  theme()

## Output File 1: Creating a csv file to view in excel or Tableau for Average Ride Length on days of the week

average_ride_length_week<- aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$day_of_week, FUN = mean)
write.csv(average_ride_length_week, file = "Documentation/avg_ride_length_week.csv")

##  Plot 2 : Analysis and Visualization of no. of trips by weekday
##################################################################

all_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()	) %>%						
  arrange(member_casual, weekday)	%>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Number of Rides by Day of the Week Segmented by Users",
    subtitle = "No.. of trips for every week ",
    caption = "Fig 2",
    x = "day of the week",
    y = "total rides"
  )+
  theme()+
  
  
  ## Output file 2: Creating a csv file to view in excel or Tableau for Average Ride Length on days of the week
  
  total_rides_week <- all_rides_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(cnt = n()) %>% 
  as.data.frame() 
write.csv(total_rides_week, file = "Documentation/total_rides_week.csv")

## Plot 3: Visualization for total duration on the days in every week of the year
#################################################################################

all_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							
            ,total_duration = sum(ride_length)) %>% 		
  arrange(member_casual, weekday)	%>% 
  ggplot(aes(x = weekday, y = total_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Total Duration of the Rides by Day of the Week",
    subtitle = "Total duration of rides for every week",
    caption = "fig 3",
    x = "day of the week",
    y = "total duration of rides"
  )+
  theme()

## Output File 3: Creating a csv file to view in excel or Tableau for Average Ride Length on days of the week

total_ride_length_week <- aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$day_of_week, FUN = sum)
write.csv(total_ride_length_week, file = "Documentation/total_ride_length_week.csv")

##       Observations  by Month

## Plot 4: Analysis and Visualization num of rides by month
############################################################

all_rides_v2 %>% 
  
  mutate(mon = month(started_at, label = FALSE,abbr = TRUE )) %>% 
  group_by(member_casual,mon) %>% 
  summarise(number_of_rides_month = n()
            ,average_duration_month  = mean(ride_length)) %>% 
  arrange(member_casual, mon) %>%
  ggplot(aes(x = mon, y = number_of_rides_month, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(limits = month.abb)+
  labs(
    title = "Number of Rides by Month and Segmented by Users",
    subtitle = " Number of Rides for every month",
    caption = "Fig4",
    x = "month",
    y = " number of rides"
  )+
  theme()  

## Output File 4: Creating a csv file to view in excel or Tableau for number of rides by month 

total_rides_month <- all_rides_v2 %>%
  group_by(member_casual, month_name) %>%
  summarise(cnt = n()) %>% 
  as.data.frame() 
write.csv(total_rides_month, file = "Documentation/total_rides_month.csv")

## Plot 5:  Analysis and Visualization average duration of rides by month
#########################################################################

all_rides_v2 %>% 
  
  mutate(mon = month(started_at, label = FALSE,abbr = TRUE )) %>% 
  group_by(member_casual, mon) %>% 
  summarise(number_of_rides_month = n()
            ,average_duration_month  = mean(ride_length)) %>% 
  arrange(member_casual, mon) %>%
  ggplot(aes(x = mon, y = average_duration_month, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(limits = month.abb)+
  labs(
    title = "Average Duration of Rides by Month and Segmented by Users",
    subtitle = " Average duration for every month",
    caption = "Fig5",
    x = "month",
    y = " average duration of rides"
  )+
  theme()  

## Output File 5: Creating a csv file to view in excel or Tableau for average ride length by month 

average_ride_length_month <- aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$month_name, FUN = mean)
write.csv(average_ride_length_month, file = "Documentation/average_ride_length_month.csv")

## Plot 6 : Analysis and Visualization total duration of rides by month
#######################################################################

all_rides_v2 %>% 
  
  mutate(mon = month(started_at, label = FALSE,abbr = TRUE )) %>% 
  group_by(member_casual, mon) %>% 
  summarise(number_of_rides_month = n()
            ,total_duration_month  = sum(ride_length)) %>% 
  arrange(member_casual, mon) %>%
  ggplot(aes(x = mon, y = total_duration_month, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(limits = month.abb)+
  labs(
    title = "Total Duration of Rides by Month and Segmented by Users",
    subtitle = " Total duration for every month",
    caption = "Fig6",
    x = "month",
    y = "total duration of rides"
  )+
  theme()  

## Output File 7: Creating a csv file to view in excel or Tableau for total duration of by month 

total_ride_length_month <- aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$month_name, FUN = sum)
write.csv(total_ride_length_month, file = "Documentation/total_ride_length_month.csv")

## Observations by Day

## Plot 7: Analysis and visualizaton of Number of Rides by Day
##############################################################

all_rides_v2 %>% 
  mutate(day_hour = hour(started_at)) %>% 
  group_by(member_casual,day_hour) %>% 
  summarise(number_of_rides_hour = n()
            ,average_duration_hour  = mean(ride_length)) %>% 
  arrange(member_casual, day_hour) %>%
  ggplot(aes(x = day_hour, y = number_of_rides_hour, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(0, 24, 2))+
  labs(
    title = "Number of Rides by Hour and Segmented by Users",
    subtitle = " Number of rides for every hour",
    caption = "Fig7",
    x = "days",
    y = " No.of rides"
  )+
  theme()  

## Output File 7: Creating a csv file to view in excel or Tableau for number of rides by day 

total_rides_day <- all_rides_v2 %>%
  group_by(member_casual, day ) %>%
  summarise(cnt = n()) %>% 
  as.data.frame() 
write.csv(total_rides_day, file = "Documentation/total_rides_day.csv")

##  Plot 8: Analysis and Visualization Average Duration of Rides by Day
#######################################################################

all_rides_v2 %>% 
  
  mutate(day_hour = hour(started_at)) %>% 
  group_by(member_casual,day_hour) %>% 
  summarise(number_of_rides_hour = n()
            ,average_duration_hour  = mean(ride_length)) %>% 
  arrange(member_casual, day_hour) %>%
  ggplot(aes(x = day_hour, y = average_duration_hour, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(0, 24, 2))+
  labs(
    title = "Average Duration of Rides by Day and Segmented by Users",
    subtitle = " Average duration for every day",
    caption = "Fig8",
    x = "day",
    y = " average duration of rides"
  )+
  theme()  

## Output File 8: Creating a csv file to view in excel or Tableau for average duration of rides by day

average_ride_length_day <- aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$day, FUN = mean) %>%
  write.csv(average_ride_length_day, file = "Documentation/average_ride_length_day.csv")

## Plot 9 : Analysis and Visualization Total Duration of Rides by Day
#####################################################################

all_rides_v2 %>% 
  
  mutate(day_hour = hour(started_at)) %>% 
  group_by(member_casual,day_hour) %>% 
  summarise(number_of_rides_hour = n()
            ,total_duration_hour  = sum(ride_length)) %>% 
  arrange(member_casual, day_hour) %>%
  ggplot(aes(x = day_hour, y = total_duration_hour, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(0, 24, 2))+
  labs(
    title = "Total Duration of Rides by Day and Segmented by Users",
    subtitle = " Total duration for every day",
    caption = "Fig9",
    x = "day",
    y = " total duration of rides"
  )+
  theme()  

## Output File 9 :  Creating a csv file to view in excel or Tableau for total duration of rides by day

Total_ride_length_day <- aggregate(all_rides_v2$ride_length ~ all_rides_v2$member_casual + all_rides_v2$day, FUN = sum)
write.csv(Total_ride_length_day, file = "Documentation/total_ride_length_day.csv")
