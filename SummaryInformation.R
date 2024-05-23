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
oldest_year <- min(data2$year) 
recent <- max(data2$year) 

recent_year <- max(data1$year) 
## current mean values right now for prison populations
current_incarceration_for_race <- data1 %>%
  filter(year == recent_year) %>%
  summarize(
    AAPI_Current = round(mean(aapi_jail_pop_rate, na.rm = TRUE), digits = 2),
    BLACK_Current = round(mean(black_jail_pop_rate, na.rm = TRUE), digits = 2),
    LATINX_Current = round(mean(latinx_jail_pop_rate, na.rm = TRUE), digits = 2),
    NATIVE_Current = round(mean(native_jail_pop_rate, na.rm = TRUE), digits = 2),
    WHITE_Current = round(mean(white_jail_pop_rate, na.rm = TRUE), digits = 2)
  )

## highest and lowest jail population rates across counties in 1990
highest_and_lowest_first <- data2 %>% ## since 1990
  filter(year == oldest_year) %>%
  summarize(
    AAPI_first_high = ifelse(all(is.na(aapi_jail_pop_rate)), NA, max(aapi_jail_pop_rate, na.rm = TRUE)),
    AAPI_first_low = ifelse(all(is.na(aapi_jail_pop_rate)), NA, min(aapi_jail_pop_rate, na.rm = TRUE)),
    BLACK_first_high = ifelse(all(is.na(black_jail_pop_rate)), NA, max(black_jail_pop_rate, na.rm = TRUE)),
    BLACK_first_low = ifelse(all(is.na(black_jail_pop_rate)), NA, min(black_jail_pop_rate, na.rm = TRUE)),
    LATINX_first_high = ifelse(all(is.na(latinx_jail_pop_rate)), NA, max(latinx_jail_pop_rate, na.rm = TRUE)),
    LATINX_first_low = ifelse(all(is.na(latinx_jail_pop_rate)), NA, min(latinx_jail_pop_rate, na.rm = TRUE)),
    NATIVE_first_high = ifelse(all(is.na(native_jail_pop_rate)), NA, max(native_jail_pop_rate, na.rm = TRUE)),
    NATIVE_first_low = ifelse(all(is.na(native_jail_pop_rate)), NA, min(native_jail_pop_rate, na.rm = TRUE)),
    WHITE_first_high = ifelse(all(is.na(white_jail_pop_rate)), NA, max(white_jail_pop_rate, na.rm = TRUE)),
    WHITE_first_low = ifelse(all(is.na(white_jail_pop_rate)), NA, min(white_jail_pop_rate, na.rm = TRUE))
  )

## highest and lowest jail population rates across counties in most recent 
highest_and_lowest_now <- data2 %>% ## since 1990
  filter(year == recent) %>%
  summarize(
    AAPI_first_high = ifelse(all(is.na(aapi_jail_pop_rate)), NA, max(aapi_jail_pop_rate, na.rm = TRUE)),
    AAPI_first_low = ifelse(all(is.na(aapi_jail_pop_rate)), NA, min(aapi_jail_pop_rate, na.rm = TRUE)),
    BLACK_first_high = ifelse(all(is.na(black_jail_pop_rate)), NA, max(black_jail_pop_rate, na.rm = TRUE)),
    BLACK_first_low = ifelse(all(is.na(black_jail_pop_rate)), NA, min(black_jail_pop_rate, na.rm = TRUE)),
    LATINX_first_high = ifelse(all(is.na(latinx_jail_pop_rate)), NA, max(latinx_jail_pop_rate, na.rm = TRUE)),
    LATINX_first_low = ifelse(all(is.na(latinx_jail_pop_rate)), NA, min(latinx_jail_pop_rate, na.rm = TRUE)),
    NATIVE_first_high = ifelse(all(is.na(native_jail_pop_rate)), NA, max(native_jail_pop_rate, na.rm = TRUE)),
    NATIVE_first_low = ifelse(all(is.na(native_jail_pop_rate)), NA, min(native_jail_pop_rate, na.rm = TRUE)),
    WHITE_first_high = ifelse(all(is.na(white_jail_pop_rate)), NA, max(white_jail_pop_rate, na.rm = TRUE)),
    WHITE_first_low = ifelse(all(is.na(white_jail_pop_rate)), NA, min(white_jail_pop_rate, na.rm = TRUE))
  )

oldest_year <- min(data2$year) 
recent <- max(data2$year) 

pop_change_recent <- data2 %>%
  filter(year == recent) %>%
  group_by(year) %>%
  summarise(
    total_AAPI_recent = sum(aapi_jail_pop_rate, na.rm = TRUE),
    total_Black_recent = sum(black_jail_pop_rate, na.rm = TRUE),
    total_Latinx_recent = sum(latinx_jail_pop_rate, na.rm = TRUE),
    total_Native_recent = sum(native_jail_pop_rate, na.rm = TRUE),
    total_White_recent = sum(white_jail_pop_rate, na.rm = TRUE)
  )

pop_change_oldest <- data2 %>%
  filter(year == oldest_year) %>%
  group_by(year) %>%
  summarise(
    total_AAPI_then = sum(aapi_jail_pop_rate, na.rm = TRUE),
    total_Black_then = sum(black_jail_pop_rate, na.rm = TRUE),
    total_Latinx_then = sum(latinx_jail_pop_rate, na.rm = TRUE),
    total_Native_then = sum(native_jail_pop_rate, na.rm = TRUE),
    total_White_then = sum(white_jail_pop_rate, na.rm = TRUE)
  )


pop_change <- data.frame(
  change_AAPI = round(pop_change_recent$total_AAPI_recent - pop_change_oldest$total_AAPI_then, digits = 2),
  change_BLACK = round(pop_change_recent$total_Black_recent - pop_change_oldest$total_Black_then, digits = 2),
  change_Latinx = round(pop_change_recent$total_Latinx_recent - pop_change_oldest$total_Latinx_then, digits = 2),
  change_Native = round(pop_change_recent$total_Native_recent - pop_change_oldest$total_Native_then, digits = 2),
  change_White = round(pop_change_recent$total_White_recent - pop_change_oldest$total_White_then, digits = 2)
)
