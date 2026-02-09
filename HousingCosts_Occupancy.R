rm(list=ls())
install.packages(c("tidyverse","skimr","naniar"))
library(tidyverse)
library(skimr)
library(naniar)

# 1) Load data
df <- read_csv("~/Downloads/homelessness_shelter_data.csv")

# 2) Quick overview
glimpse(df)         # structure
skim(df)            # summary stats (mean, sd, n_missing, etc.)

install.packages("lubridate")
library(lubridate)

# Make sure date is in Date format
df <- df %>%
  mutate(date = as.Date(date))

# 1) Find top 8 cities by row count
city_counts <- df %>%
  count(city, sort = TRUE)

top8_cities <- city_counts %>%
  slice_head(n = 8) %>%
  pull(city)

top_city_name <- city_counts %>%
  slice_head(n = 1) %>%
  pull(city)   ## Los Angeles is top 1 for occupancy rate

# 2) Compute monthly average occupancy for those cities
monthly_occ <- df %>%
  filter(city %in% top8_cities) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(city, month) %>%
  summarise(avg_occupancy = mean(occupancy_rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(is_top = city == top_city_name)

# 3) Build a color palette that emphasizes the top city
base_cols <- rep("grey70", length(top8_cities))
names(base_cols) <- top8_cities
base_cols[top_city_name] <- "#1f78b4"  # highlight color for LA/top city

# 4) Plot: single chart, top city emphasized
p <- ggplot(monthly_occ, aes(x = month, y = avg_occupancy, group = city, color = city)) +
  geom_line(aes(linewidth = ifelse(is_top, 1.5, 0.6)), alpha = 0.9) +
  scale_color_manual(values = base_cols) +
  scale_linewidth_identity() +
  labs(
    title = paste0("Monthly Average Occupancy — Top 8 Cities (Top: ", top_city_name, ")"),
    x = "Month",
    y = "Average Occupancy Rate (%)",
    color = "City"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

library(tidyverse)
library(lubridate)

city_name <- "Los Angeles"

# Compute yearly average occupancy by shelter
la_ranked <- df %>%
  filter(str_to_lower(city) == str_to_lower(city_name)) %>%
  mutate(year = year(date)) %>%
  group_by(year, shelter_name) %>%
  summarise(avg_occupancy = mean(occupancy_rate, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, desc(avg_occupancy)) %>%
  group_by(year) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# View the ranking
print(la_ranked)


##---------------------------
library(dplyr)
library(readr)

# Rent burden dataset
rb <- read_csv("~/Downloads/LA_City_Rent_Burdened_Households.csv") 

#Citywide % rent-burdened households (≥30% of income on rent).)
rent_city <- rb %>%
  summarise(
    mean_rent_burden = mean(B25070_calc_pctGE30pctE, na.rm = TRUE),
    median_rent_burden = median(B25070_calc_pctGE30pctE, na.rm = TRUE),
    mean_mortgage_burden = mean(B25091_calc_pctMortGE30pctE, na.rm = TRUE)
  )

rent_city

# Check occupancy rate stats (from Kaggle dataset)
df %>%
  filter(city == "Los Angeles") %>%
  summarise(
    avg_occupancy = mean(occupancy_rate, na.rm = TRUE),
    max_occupancy = max(occupancy_rate, na.rm = TRUE),
    min_occupancy = min(occupancy_rate, na.rm = TRUE)
  )

#Compare
tibble(
  mean_rent_burden = rent_city$mean_rent_burden,
  mean_shelter_occupancy = mean(df$occupancy_rate, na.rm = TRUE)
)

#Comparison chart
library(ggplot2)
library(tibble)

comparison <- tibble(
  metric = c("Rent Burden (≥30%)", "Shelter Occupancy"),
  value = c(55.9, 51.2)
)

ggplot(comparison, aes(x = metric, y = value, fill = metric)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(value,1), "%")), vjust = -0.5, size = 5) +
  labs(
    title = "Comparison: Rent Burden vs Shelter Occupancy in Los Angeles",
    y = "Percentage", x = ""
  ) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")




