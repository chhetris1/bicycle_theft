## Load tidyverse package
library(tidyverse)
## Read `bike_data`
bike_data <- read_csv("data/Cleaned_Bicycle_Thefts_Open_Data.csv")
## Take a glance of the `bike_data`
head(bike_data)

#1. Goal: Which quarter of the year are bike thefts most and least frequent? 
bike_data %>%   
  group_by(quarter) %>%  
  # Summarize the data by counting the number of rows in each quarter group  
  summarize(  
    total_per_quarter = n()  
  ) %>%  
  # Initialize the ggplot function and map the 'quarter' to the x-axis and 'n' (count) to the y-axis  
  ggplot(aes(x = quarter, y = total_per_quarter)) +  
  # Add points to the plot for each quarter  
  geom_point() +  
  # Add a smooth line to the plot with a span of 0.1 and disable the confidence interval shading  
  geom_smooth(span = 0.1, se = FALSE) +   
  # Add labels for the y-axis, title, and caption of the plot  
  labs(y = "The Number of Stolen Bikes",  
       title = "Quarterly Trends in Bike Thefts")

location_summary <- bike_data1 %>% 
  mutate(location = as.factor(location))%>%
  group_by(location) %>% 
  summarise(count = n()) %>%
  mutate(proportion = round(count/sum(count), 1))
location_summary
ggplot(location_summary, aes(x = location, y = count))+
  geom_col(fill = "blue")
location <- "Residential Structures"
percentage <- 0.5

#3 which region of Toronto is the median value of stolen bike highest?
stolen_bike_price <- bike_data %>% 
  group_by(neighborhood) %>%
  summarise(long = mean(long), 
            lat = mean(lat), 
            median_value = round(median(bike_cost, na.rm = TRUE)))%>%
  arrange(desc(median_value))
stolen_bike_price
stolen_bike_price$neighborhood[1]
region <- '41'
action <- "Beware in the third and fourth quarters of the year, in area 41, in residential structures."

#this will be a new experiment

