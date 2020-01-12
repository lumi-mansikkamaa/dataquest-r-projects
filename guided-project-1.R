# title: Guided Project #1: Analyzing Forest Fire Data
# author: Beste Erdem
# date: January 12, 2020

library(readr)
library(dplyr)
library(ggplot2)

# TODO add metadata 
data <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv")

# During which months are forest fires most common?
fires_by_month <- data %>% group_by(month) %>% summarize(total_fires=n())
ggplot(data = fires_by_month) +
  aes(x=month, y=total_fires) +
  geom_bar(stat="identity") +
  labs(title="Fires by Month")

# On which days are forest fires most common?
fires_by_weekday <- data %>% group_by(day) %>% summarize(total_fires=n())
ggplot(data = fires_by_weekday) +
  aes(x=day, y=total_fires) +
  geom_bar(stat="identity") +
  labs(title="Fires by Day of the Week")

# Change the order of variables, month & day in chronological order 
fires_by_month <- fires_by_month %>% 
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

fires_by_weekday <- fires_by_weekday %>%
  mutate(day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")))

# Create the bar charts using the reordered month and day variables

# Visualize the distribution of some variables (temperature, rain, wind speed, etc.) by month & day
fire_dist_by_month <- function(x,y){
  ggplot(data = data) +
    aes_string(x = x, y = y) +
    geom_boxplot()
}
  
fire_dist_by_day <- function(x,y){
  ggplot(data = data) +
    aes_string(x = x, y = y) +
    geom_boxplot()
}

x_var <- names(data)[3:4]
y_var <- names(data)[5:12]

dist_by_month <- purrr::map2(x_var[1], y_var, fire_dist_by_month)
dist_by_day <- purrr::map2(x_var[2], y_var, fire_dist_by_day)

dist_by_day

dist_by_month

# Which variables are related to forest fire severity?

# ggplot(data = data) +
#   aes(x = area) +
#   geom_histogram()

area_filter1 <- data %>% filter(area < 400 & area > 0)
area_filter2 <- data %>% filter(area != 0)

create_scatter <- function(x,y){
  ggplot(data = area_filter2) +
    aes_string(x = x, y = y) +
    geom_point()
}

area <- names(data)[13]

dist_by_area <- purrr::map2(names(data)[5:12], area, create_scatter)

dist_by_area

