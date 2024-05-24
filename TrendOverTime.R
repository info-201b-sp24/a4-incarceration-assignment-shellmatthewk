library(dplyr)
library(tidyverse)
library(ggplot2)

file <- '/Users/matthewkshell/Documents/Info201Code/a4-incarceration-assignment-shellmatthewk/us-prison-jail-rates-1990.csv'
data2 <- read.csv(file)

population_totals_year <- data2 %>%
  group_by(year) %>%
  summarise(
    total_AAPI = sum(aapi_jail_pop_rate, na.rm = TRUE),
    total_Black = sum(black_jail_pop_rate, na.rm = TRUE),
    total_Latinx = sum(latinx_jail_pop_rate, na.rm = TRUE),
    total_Native = sum(native_jail_pop_rate, na.rm = TRUE),
    total_White = sum(white_jail_pop_rate, na.rm = TRUE)
  )

population_totals_year$year <- as.factor(population_totals_year$year)

total_population_ready <- population_totals_year %>%
  pivot_longer(cols = c(total_AAPI, total_Black, total_Latinx, total_Native, total_White),
              names_to = "racial_group", 
              values_to = "total_population")

population_totals_year <- total_population_ready %>%
  group_by(year, racial_group = factor(racial_group, levels = unique(total_population_ready$racial_group))) %>%
  summarise(total_population = sum(total_population, na.rm = TRUE))

ggplot(population_totals_year, aes(x = year, y = total_population, color = racial_group, group = racial_group)) +
  geom_line() +
  ggtitle("Sum of Jail Population Races Over Time") +
  labs(x = "Year", y = "Total Population Rate", color = "Racial Group:") +
  scale_color_manual(values = c("total_AAPI" = "blue",
                                "total_Black" = "red",
                                "total_Latinx" = "green",
                                "total_Native" = "orange",
                                "total_White" = "purple"),
                     labels = c("total_AAPI" = "Asian American and Pacific Islander",
                                "total_Black" = "Black",
                                "total_Latinx" = "Latinx",
                                "total_Native" = "Native American",
                                "total_White" = "White")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top",
    title.position = "top"
  )
  