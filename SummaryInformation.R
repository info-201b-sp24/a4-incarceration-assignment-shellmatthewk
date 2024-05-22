library(dplyr)
library(tidyverse)
library(knitr)
file1 <- '/Users/matthewkshell/Documents/Info201Code/a4-incarceration-assignment-shellmatthewk/us-prison-jail-rates.csv'
data1 <- read.csv(file1)
rows1 <- nrow(data1)
cols1 <- ncol(data1)
rows1
cols1

file2 <- '/Users/matthewkshell/Documents/Info201Code/a4-incarceration-assignment-shellmatthewk/us-prison-jail-rates-1990.csv'
data2 <- read.csv(file2)
rows2 <- nrow(data2)
cols2 <- ncol(data2)
rows2
cols2

#summary information: 

recent_year <- max(data1$year) 
current_incarceration_for_race <- data1 %>%
  filter(year == recent_year) %>%
  summarize(
    AAPI_Current = mean(aapi_prison_pop_rate, na.rm = TRUE),
    BLACK_Current = mean(black_prison_pop_rate, na.rm = TRUE),
    LATINX_Current = mean(latinx_prison_pop_rate, na.rm = TRUE),
    NATIVE_Current = mean(native_prison_pop_rate, na.rm = TRUE),
    WHITE_Current = mean(white_prison_pop_rate, na.rm = TRUE),
  )

