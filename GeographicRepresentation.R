library(dplyr)
library(maps)
library(ggplot2)

file <- '/Users/matthewkshell/Documents/Info201Code/a4-incarceration-assignment-shellmatthewk/us-prison-jail-rates-1990.csv'
data1 <- read.csv(file)

current_year <- max(data1$year)
state_names <- c(
  "al" = "alabama", "ak" = "alaska", "az" = "arizona", "ar" = "arkansas", "ca" = "california",
  "co" = "colorado", "ct" = "connecticut", "dc" = "district of columbia", "de" = "delaware", "fl" = "florida", "ga" = "georgia",
  "hi" = "hawaii", "id" = "idaho", "il" = "illinois", "in" = "indiana", "ia" = "iowa",
  "ks" = "kansas", "ky" = "kentucky", "la" = "louisiana", "me" = "maine", "md" = "maryland",
  "ma" = "massachusetts", "mi" = "michigan", "mn" = "minnesota", "ms" = "mississippi", "mo" = "missouri",
  "mt" = "montana", "ne" = "nebraska", "nv" = "nevada", "nh" = "new hampshire", "nj" = "new jersey",
  "nm" = "new mexico", "ny" = "new york", "nc" = "north carolina", "nd" = "north dakota", "oh" = "ohio",
  "ok" = "oklahoma", "or" = "oregon", "pa" = "pennsylvania", "ri" = "rhode island", "sc" = "south carolina",
  "sd" = "south dakota", "tn" = "tennessee", "tx" = "texas", "ut" = "utah", "vt" = "vermont",
  "va" = "virginia", "wa" = "washington", "wv" = "west virginia", "wi" = "wisconsin", "wy" = "wyoming"
)

black_jail_data <- data1 %>%
  filter(year == current_year) %>%
  select(state, black_jail_pop_rate) %>%
  group_by(state) %>%
  summarise(total_black_jail_rate = mean(black_jail_pop_rate, na.rm = TRUE)) %>%
  mutate(region = tolower(state_names[tolower(state)]))

state <- map_data("state")

state_data <- left_join(state, black_jail_data, by = "region")

state_data2 <- state_data %>% filter(!is.na(total_black_jail_rate))

ggplot(state_data2, aes(x = long, y = lat, group = group, fill = total_black_jail_rate)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_gradient(name = "African American Jail Pop Rate", na.value = "grey50", low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = paste("African American Jail Population Rate by State -", current_year),
       caption = "Source: US Prison Jail Rates 1990",
       x = "longitude",
       y = "latitude") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )
