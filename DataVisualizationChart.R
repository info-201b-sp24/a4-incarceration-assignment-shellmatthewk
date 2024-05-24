library(dplyr)
library(tidyverse)
library(ggplot2)

file <- '/Users/matthewkshell/Documents/Info201Code/a4-incarceration-assignment-shellmatthewk/us-prison-jail-rates-1990.csv'
data2 <- read.csv(file)

rates <- data2 %>%
  group_by(year) %>%
  summarise(
    total_Population = mean(total_jail_pop_rate, na.rm = TRUE),
    total_Latinx = mean(latinx_jail_pop_rate, na.rm = TRUE),
    total_Native = mean(native_jail_pop_rate, na.rm = TRUE)
  )

proportion_rates <- rates %>%
  mutate(
    latin_prop_to_total = round((total_Population / total_Latinx), digits = 2),
    native_prop_to_total = round((total_Population / total_Native), digits = 2)
  )
View(proportion_rates)
ggplot(proportion_rates, aes(x = latin_prop_to_total)) +
  geom_line(aes(y = native_prop_to_total)) +  
  ggtitle("Native Jail Population Rate vs Latinx Jail Population Rate Proportionally") +
  labs(x = "Total Population Rate / Latinx Population Rate", y = "Total Population Rate / Total Native Population Rate") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
  ) 
  
