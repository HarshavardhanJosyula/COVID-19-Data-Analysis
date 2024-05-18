# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

# Load the data
covid_data <- read_csv("data/covid19_data.csv")

# Data Preprocessing
covid_data <- covid_data %>%
  mutate(date = ymd(date)) %>%
  filter(date >= "2020-01-01")

# Summary statistics
summary_stats <- covid_data %>%
  group_by(location) %>%
  summarise(
    total_cases = max(total_cases, na.rm = TRUE),
    total_deaths = max(total_deaths, na.rm = TRUE),
    total_vaccinations = max(total_vaccinations, na.rm = TRUE)
  )

# Print summary statistics
print(summary_stats)

# Visualizations
# Plotting total cases over time for selected countries
selected_countries <- c("United States", "India", "Brazil", "Russia", "United Kingdom")
covid_data %>%
  filter(location %in% selected_countries) %>%
  ggplot(aes(x = date, y = total_cases, color = location)) +
  geom_line() +
  labs(title = "Total COVID-19 Cases Over Time",
       x = "Date",
       y = "Total Cases",
       color = "Country") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("figures/total_cases_over_time.png")
