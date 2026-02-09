#----------------------------------------------------------------------
#RQ1: What demographic groups are most affected by homelessness?
#How does Los Angeles compare to other cities?
#----------------------------------------------------------------------

#load necessary libraries
library(tidyverse) #we will use for data manipulation and plotting
library(ggplot2) #we will use for plotting the data

#load the dataset
homeless_shelter_data <- read.csv('C:/Temp/homelessness_shelter_data.csv')

#do initial data check
glimpse(homeless_shelter_data)

#first we will change the season column - the original dataset has off categorization of seasons
#for example a date falling in july is being categorized as Spring
homeless_shelter_data <- homeless_shelter_data %>%
  mutate(
    season = case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer",
      month(date) %in% c(9, 10, 11) ~ "Autumn"
    )
  )

#summarize by gender - grab the actual count not just the percentage
homeless_shelter_data <- homeless_shelter_data %>%
  mutate(
    male_count = occupied_beds * male_percentage / 100,
    female_count = occupied_beds * female_percentage / 100
  )

#create columns so we can compare average age of homeless individuals in LA versus other cities
homeless_shelter_data <- homeless_shelter_data %>%
  mutate(Location = ifelse(city == "Los Angeles", "Los Angeles", "Other City"))

#aggregate data for comparison
#gender totals by location
gender_summary <- homeless_shelter_data %>%
  group_by(Location) %>%
  summarise(
    total_males = sum(male_count, na.rm = TRUE),
    total_females = sum(female_count, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(total_males, total_females),
               names_to = "Gender",
               values_to = "Total")

#average age by location
age_summary <- homeless_shelter_data %>%
  group_by(Location) %>%
  summarise(avg_age = mean(average_age, na.rm = TRUE))

#visualize
#gender comparison
ggplot(gender_summary, aes(x = Gender, y = Total, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender Distribution of Homeless Individuals: LA vs Other States",
       x = "Gender", y = "Total Homeless Individuals") +
  theme_minimal()

#average age comparison
ggplot(age_summary, aes(x = Location, y = avg_age, fill = Location)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Age of Homeless Individuals: LA vs Other States",
       x = "", y = "Average Age") +
  theme_minimal()

#now we drill down and look at LA versus individual cities
#summarize total males and females by city
city_gender_summary <- homeless_shelter_data %>%
  group_by(city) %>%
  summarise(
    total_males = sum(male_count, na.rm = TRUE),
    total_females = sum(female_count, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(total_males, total_females),
    names_to = "Gender",
    values_to = "Total"
  )

#quick check
head(city_gender_summary)

#visualize gender by city
ggplot(city_gender_summary, aes(x = reorder(city, -Total), y = Total, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender Distribution of Homeless Individuals by City",
       x = "City", y = "Total Homeless Individuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#summarize avg age by city 
city_age_summary <- homeless_shelter_data %>%
  group_by(city) %>%
  summarise(avg_age = mean(average_age, na.rm = TRUE))

#visualize age by city
ggplot(city_age_summary, aes(x = reorder(city, -avg_age), y = avg_age, fill = city == "Los Angeles")) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("gray", "steelblue"), labels = c("Other Cities", "Los Angeles")) +
  labs(title = "Average Age of Homeless Individuals by City",
       x = "City", y = "Average Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

#lets now look at both gender and age in comparison to occupancy rates in each city
city_summary <- homeless_shelter_data %>%
  group_by(city) %>%
  summarise(
    total_males = sum(male_count, na.rm = TRUE),
    total_females = sum(female_count, na.rm = TRUE),
    avg_age = mean(average_age, na.rm = TRUE),
    avg_occupancy_rate = mean(occupancy_rate, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols =c(total_males, total_females),
    names_to = "Gender",
    values_to = "Total"
  )

#create a combined plot of gender and occupancy
ggplot(city_summary, aes(x = reorder(city, -Total))) +
  #gender bars
  geom_bar(aes(y = Total, fill = Gender), stat = "identity", position = "dodge") +
  #occupancy rate line
  geom_line(aes(y = avg_occupancy_rate * max(Total)/100, group = 1), color = "black", size = 1)+
  geom_point(aes(y = avg_occupancy_rate * max(Total)/100), color = "black", size = 2) +
  scale_y_continuous(
    name = "Total Homeless Individuals",
    sec.axis = sec_axis(~ . * 100 / max(city_summary$Total), name = "Average Occupancy Rate (%)")
  ) +
  labs(title = "Homelessness by Gender and Shelter Occupancy by City",
       x = "City") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust =1))

#create a combined plot of age and occupancy
#summarize by city and age
city_age_occupancy <- homeless_shelter_data %>%
  group_by(city) %>%
  summarise (
    avg_age = mean(average_age, na.rm = TRUE),
    avg_occupancy = mean(occupancy_rate, na.rm = TRUE)
  )

#plot avg age vs occupancy rate
ggplot(
  city_age_occupancy %>%
    mutate(Location = ifelse(city == "Los Angeles", "Los Angeles", "Other Cities")),
  aes(x = reorder(city, -avg_age))
) +
  # Bars for average age
  geom_bar(aes(y = avg_age, fill = Location), stat = "identity") +
  # Line for occupancy rate
  geom_line(aes(y = avg_occupancy * max(avg_age)/100, group = 1), color = "black", size = 1) +
  geom_point(aes(y = avg_occupancy * max(avg_age)/100), color = "black", size = 2) +
  # Manual color mapping
  scale_fill_manual(values = c("Los Angeles" = "steelblue", "Other Cities" = "gray70")) +
  scale_y_continuous(
    name = "Average Age of Homeless Individuals",
    sec.axis = sec_axis(~ . * 100 / max(city_age_occupancy$avg_age),
                        name = "Average Occupancy Rate (%)")
  ) +
  labs(title = "Average Age and Shelter Occupancy Rate by City",
       x = "City") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )

#now we explore statisitcally quantifying the relationship between gender, age, and occupancy rates
#we will examine this from both a city lense (Los Angeles) and an overall lense
#1: filter to los angeles vs all cities
la_data <- homeless_shelter_data %>%
  filter(city == "Los Angeles")

all_cities_data <-homeless_shelter_data

#2a: age vs occupancy rate - LA only
#correlation (strength and direction of relationship)
cor_la_age_occ <- cor(la_data$average_age, la_data$occupancy_rate, use = "complete.obs")
cor_la_age_occ

#regression model
model_la_age_occ <-lm(occupancy_rate ~ average_age, data = la_data)
summary(model_la_age_occ)

#scatterplot
ggplot(la_data, aes(x = average_age, y = occupancy_rate)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = paste0("Age vs. Occupancy Rate in Los Angeles (r = ", round(cor_la_age_occ, 2),")"),
       x = "Average Age", y = "Occupancy Rate (%)") +
  theme_minimal()

#question we can answer: Do older populations have higher or lower occupancy in shelters in LA?

#2b: age vs occupancy rate - all cities
#correlation (strength and direction of relationship)
cor_all_age_occ <- cor(all_cities_data$average_age, all_cities_data$occupancy_rate, use = "complete.obs")
cor_all_age_occ

#regression model
model_all_age_occ <- lm(occupancy_rate ~ average_age, data = all_cities_data)
summary(model_all_age_occ)

#scatterplot
ggplot(all_cities_data, aes(x = average_age, y = occupancy_rate)) +
  geom_point(aes(color = city == "Los Angeles")) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = c("gray70", "steelblue"), labels = c("Other Cities", "Los Angeles")) +
  labs(title = paste0("Age vs. Occupancy Rate Across All Cities (r = ", round(cor_all_age_occ, 2), ")"),
       x = "Average Age", y = "Occupancy Rate (%)") +
  theme_minimal() +
  theme(legend.title = element_blank())

#question we can answer: Are age patterns in LA unique?

#3a: gender vs occupancy rate - LA only
#correlation (strength and direction of relationship)
cor_la_male <- cor(la_data$male_percentage, la_data$occupancy_rate, use = "complete.obs")
cor_la_male
cor_la_female <- cor(la_data$female_percentage, la_data$occupancy_rate, use = "complete.obs")
cor_la_female

#regression model
model_la_gender <- lm(occupancy_rate ~ male_percentage + female_percentage, data = la_data)
summary(model_la_gender)

#question we can answer: Does gender distribution affect shelter crowding in LA?
#3b: gender vs occupancy rate - all cities
#correlation (strength and direction of relationship)
cor_all_male <- cor(all_cities_data$male_percentage, all_cities_data$occupancy_rate, use = "complete.obs")
cor_all_male
cor_all_female <- cor(all_cities_data$female_percentage, all_cities_data$occupancy_rate, use = "complete.obs")
cor_all_female

#regression model
model_all_gender <- lm(occupancy_rate ~ male_percentage + female_percentage, data = all_cities_data)
summary(model_all_gender)
#question we can answer: Are LA's gender dynamics typical or not?

#4: visual gender vs occupancy
#male %
ggplot(all_cities_data, aes(x = male_percentage, y = occupancy_rate)) +
  geom_point(aes(color = city == "Los Angeles")) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = c("gray70", "steelblue")) +
  labs(title = paste0("Males % vs. Occupancy Rate (r = ", round(cor_all_male, 2), ")"),
       x = "Male Percentage", y = "Occupancy Rate (%)") +
  theme_minimal()

#female %
ggplot(all_cities_data, aes(x = female_percentage, y = occupancy_rate)) +
  geom_point(aes(color = city == "Los Angeles")) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = c("gray70", "steelblue")) +
  labs(title = paste0("Female % vs. Occupancy Rate (r = ", round(cor_all_female, 2), ")"),
       x = "Female Percentage", y = "Occupancy Rate (%)") +
         theme_minimal()

#lastly, lets statistically validate whether Los Angeles truly differs from other cities in how demographics (age and gender) relate to homelessness (occupancy rates)
#1: add a grouping variable to easily compare the two samples
homeless_shelter_data$region <- ifelse(homeless_shelter_data$city == "Los Angeles", "Los Angeles", "Other Cities")

#2: compare avg age and occupancy rate relationships using correlation  tests and interaction-based regression to check if LA differs significantly
#2a: correlation in each group
cor_la_age_occ <- cor(homeless_shelter_data$average_age[homeless_shelter_data$region == "Los Angeles"],
                      homeless_shelter_data$occupancy_rate[homeless_shelter_data$region == "Los Angeles"],
                      use = "complete.obs")

cor_other_age_occ <- cor(homeless_shelter_data$average_age[homeless_shelter_data$region == "Other Cities"],
                         homeless_shelter_data$occupancy_rate[homeless_shelter_data$region == "Other Cities"],
                         use = "complete.obs")

cor_la_age_occ
cor_other_age_occ

#2b: test if these correlations differ significantly
#using Fisher's r to z transformation to test whether two correlation coefficients (LA vs other) differ

fisher_r_to_z <- function(r) { 0.5 * log((1+r) / (1-r)) }

z1 <- fisher_r_to_z(cor_la_age_occ)
z2 <- fisher_r_to_z(cor_other_age_occ)

n1 <- sum(!is.na(homeless_shelter_data$average_age[homeless_shelter_data$region == "Los Angeles"]))
n2 <- sum(!is.na(homeless_shelter_data$average_age[homeless_shelter_data$region == "Other Cities"]))

z_diff <- (z1 - z2) / sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
p_value <- 2 * (1-pnorm(abs(z_diff)))

z_diff
p_value

#interpretation: a p-value  < 0.05 suggest that the relationship between average age and occupancy rate differs significantly between LA and other cities

#3: t-test on average metrics
#this let's us compare the mean occupancy rates and average ages directly between LA and other cities
#compare average age
t.test(average_age ~ region, data = homeless_shelter_data)

#compare occupancy rate
t.test(occupancy_rate ~ region, data = homeless_shelter_data)

#4: include gender percentages to test if gender composition is significantly different too:
t.test(male_percentage ~ region, data = homeless_shelter_data)
t.test(female_percentage ~ region, data = homeless_shelter_data)

#5: explore interaction effects (regression) - using a regression model to see whether the effect of age on occupancy changes depending on region (LA vs. other)
model <- lm(occupancy_rate ~ average_age * region, data = homeless_shelter_data)
summary(model)

#to best interpret we look at the p_value of the interaction coefficient (average_age:regionOther Cities)

#5b: explore interaction effection of gender
model_male <-lm(occupancy_rate ~ male_percentage * region, data = homeless_shelter_data)
summary(model_male)

model_female <- lm(occupancy_rate ~ female_percentage * region, data = homeless_shelter_data)
summary(model_female)

#6: visualize the interaction
ggplot(homeless_shelter_data, aes(x = male_percentage, y = occupancy_rate, color = region)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("Los Angeles" = "steelblue", "Other Cities" = "gray60")) +
  labs(
    title = "Relationship Between Male Percentage and Occupancy Rate",
    x = "Male Percentage",
    y = "Occupancy Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

ggplot(homeless_shelter_data, aes(x = female_percentage, y = occupancy_rate, color = region)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("Los Angeles" = "steelblue", "Other Cities" = "gray60")) +
  labs(
    title = "Relationship Between Female Percentage and Occupancy Rate",
    x = "Female Percentage",
    y = "Occupancy Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

#----------------------------------------------------------------------
#RQ1.5: How does other city's occupancy rates compare to Los Angeles? 
#Is Los Angeles more impacted than other city's? How so?
#----------------------------------------------------------------------
library(dplyr)

#1: summarize central tendencies and spread of occupancy rates
occupancy_summary <- homeless_shelter_data %>%
  group_by(region) %>%
  summarise(
    mean_occupancy = mean(occupancy_rate, na.rm = TRUE),
    median_occupancy = median(occupancy_rate, na.rm = TRUE),
    sd_occupancy = sd(occupancy_rate, na.rm = TRUE),
    n = n()
  )

occupancy_summary

#2: run statistical comparison (t-test) to see if LA's occupancy rate is significantly different from other cities
t_test_occupancy <- t.test(occupancy_rate ~ region, data = homeless_shelter_data)
t_test_occupancy

#3: visualization: show this visually with a boxplot and mean line overlay
ggplot(homeless_shelter_data, aes(x = region, y = occupancy_rate, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "black") +
  scale_fill_manual(values = c("Los Angeles" = "steelblue", "Other Cities" = "gray70")) +
  labs(
    title = "Comparison of Shelter Occupancy Rates: Los Angeles vs. Other Cities",
    x = "",
    y = "Occupancy Rate (%)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#4: lets drill down to see which specific cities have higher or lower occupancy than LA:
city_summary <- homeless_shelter_data %>%
  group_by(city) %>%
  summarise(mean_occupancy = mean(occupancy_rate, na.rm = TRUE)) %>%
  arrange(desc(mean_occupancy))

head(city_summary, 10)  # top 10 cities by occupancy

#visualuze this
ggplot(city_summary, aes(x = reorder(city, mean_occupancy), y = mean_occupancy,
                         fill = city == "Los Angeles")) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70")) +
  labs(
    title = "Average Shelter Occupancy by City (Highlighting Los Angeles)",
    x = "City",
    y = "Average Occupancy Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#5: add time dimension to show whether LA's occupancy pressure has been increasing or decreasing relative to other cities
library(dplyr)
library(lubridate)

#convert date column
homeless_shelter_data$date <- as.Date(homeless_shelter_data$date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))

#calculate average occupancy rate per month for Los Angeles vs other cities
occupancy_over_time <- homeless_shelter_data %>%
  mutate(month = floor_date(date, "month")) %>% #round to month
  group_by(month, region) %>%
  summarise(
    avg_occupancy = mean(occupancy_rate, na.rm = TRUE),
    .groups = "drop"
  )

#plot occupancy rate trends for both LA and other cities over time
ggplot(occupancy_over_time, aes(x = month, y = avg_occupancy, color = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("Los Angeles" = "steelblue", "Other Cities" = "gray60")) +
  labs(
    title = "Shelter Occupancy Trends Over Time: Los Angeles vs. Other Cities",
    x = "Month",
    y = "Average Occupancy Rate (%)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

#look at occupancy rates differences by each city
# 1) Find top 8 cities by row count
city_counts <- homeless_shelter_data %>%
  count(city, sort = TRUE)

top8_cities <- city_counts %>%
  slice_head(n = 8) %>%
  pull(city)

top_city_name <- city_counts %>%
  slice_head(n = 1) %>%
  pull(city)   ## Los Angeles is top 1 for occupancy rate

# 2) Compute monthly average occupancy for those cities
monthly_occ <- homeless_shelter_data %>%
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
ggplot(monthly_occ, aes(x = month, y = avg_occupancy, group = city, color = city)) +
  geom_line(aes(linewidth = ifelse(is_top, 1.5, 0.6)), alpha = 0.9) +
  scale_color_manual(values = base_cols) +
  scale_linewidth_identity() +
  labs(
    title = paste0("Monthly Average Occupancy — Top 8 Cities"),
    x = "Month",
    y = "Average Occupancy Rate (%)",
    color = "City"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

#fit a simple trend model (linear regression by time) to see whether LA's occupancy rate is increasing faster than others
#add numeric time index
occupancy_over_time <- occupancy_over_time %>%
  arrange(month) %>%
  mutate(time_index = as.numeric(month - min(month)))

#fit model with interaction between time and region
trend_model <-lm(avg_occupancy ~ time_index * region, data = occupancy_over_time)
summary(trend_model)

#----------------------------------------------------------------------------------------------
#RQ2: How do seasonal trends and temperature fluctutations influence shelter occupancy rates?
#----------------------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

#1: extract all temp files
temp_path <- "C:/Temp/577/Phase1Deliverables/data/Temp Data"

#read all CSV files and create city column
temperature_data <- list.files(path = temp_path, pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(function(file) {
    city_name <- str_extract(basename(file), "^[^_]+") #extract city 
    read_csv(file) %>%
      mutate(
        city = city_name
      )
  }) %>%
  bind_rows()

#2:clean df (get rid of NAME and TEMP_ATTRIBUTES column)
temperature_data <- temperature_data %>%
  dplyr::select(-NAME, -TEMP_ATTRIBUTES)

#3: prepare and merge with shelter data
#convert temperature data DATE column to Date type
temperature_data <- temperature_data %>%
  mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))

#join daily temperature to shelter data on city and date
shelter_data_with_temperature <- homeless_shelter_data %>%
  left_join(temperature_data, by = c("city" = "city", "date" = "DATE"))

#4:aggregate by month and city
library(lubridate)

combined_monthly <- shelter_data_with_temperature %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month, city, season) %>%
  summarise(
    avg_occupancy = mean(occupancy_rate, na.rm = TRUE),
    avg_temp = mean(TEMP, na.rm = TRUE),
    avg_max = mean(MAX, na.rm = TRUE),
    avg_min = mean(MIN, na.rm = TRUE),
    .grups = "drop"
  )

#5 add region for LA vs other cities
combined_monthly <- combined_monthly %>%
  mutate(region = if_else(city == "Los Angeles", "Los Angeles", "Other Cities"))

#6: visual monthly temperature and occupancy rate trends for LA and other cities
#the solid line is occupancy and the dashed line is temperature (scaled for visualization)
ggplot(combined_monthly, aes(x = month)) +
  geom_line(aes(y = avg_occupancy, color = "Occupancy Rate"), size = 1.1) +
  geom_line(aes(y = avg_temp * 2, color = "Avg Temp (scaled)"), linetype = "dashed") +
  facet_wrap(~region, scales = "free_y") +
  scale_color_manual(values = c("Occupancy Rate" = "steelblue", "Avg Temp (scaled)" = "tomato")) +
  labs(
    title = "Monthly Shelter Occupancy vs. Temperature (2023 - 2025)",
    x = "Month",
    y = "Occupancy Rate / Temperature (scaled)",
    color = "Variable"
  ) +
  theme_minimal()

#7: regression analysis - does temperature affect occupancy differently in LA?
temp_model <- lm(avg_occupancy ~ avg_temp * region, data = combined_monthly)
summary(temp_model)

#if our interaction term is significant, it shows LA reacts differently to temperature compared to other cities

#8: add seasonality for additional insight
ggplot(combined_monthly, aes(x = season, y = avg_occupancy, fill = region)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Los Angeles" = "steelblue", "Other Cities" = "gray60")) +
  labs(
    title = "Seasonal Shelter Occupancy by Region",
    x = "Season",
    y = "Average Occupancy Rate (%)",
    fill = "Region"
  ) +
  theme_minimal()

#exploring statistical significance of seasonality
#1: using an ANOVA test to see if occupancy differs across seasons in LA at a statistically significant level
la_monthly <- combined_monthly %>%
  filter(region == "Los Angeles")

#2: ANOVA test:
#null hypothesis (H0): mean occupancy is the same in all seasons
#alternative (H1): at least one season differs
la_season_anova <- aov(avg_occupancy ~ season, data = la_monthly)
summary(la_season_anova)
#if p_value is <0.05 then there is significant seasonal variation

#3: conduct post-hoc testing to see which specific seasons differ
TukeyHSD(la_season_anova)

#4: two-way ANOVA test to see if (1) season affects occupancy (2) region affects occupancy and (3) the effect of season differs between LA and other cities
season_region_anova <- aov(avg_occupancy ~ season * region, data = combined_monthly)
summary(season_region_anova)

#5: since the interaction is significant, we do a post-hoc test for pairwise season differences within each region
install.packages('emmeans')
library(emmeans)

emmeans(season_region_anova, pairwise ~ season | region)

# ADVANCED ANALYSIS SECTION IN PHASE 2 #

#--------------------------------------------------------------------------------------------------------------
#RQ3: How do local economic factors, such as rental costs or poverty rates, relate to shelter occupancy rates?
#--------------------------------------------------------------------------------------------------------------

#load data
rent_burden_df <- read_csv("C:/Temp/577/Phase1Deliverables/data/LA_City_Rent_Burdened_Households.csv")

#load zip data api
library(httr)
library(jsonlite)

#define the API URL
url <- "https://www.huduser.gov/hudapi/public/usps"


# Create all year-quarter combinations
params_df <- expand.grid(
  year = 2023:2025,
  quarter = 1:4
)

# Add type and query
params_df$type <- 1
params_df$query <- "CA"

# Initialize empty list to store results
all_data <- list()

# Loop through each combination
for(i in 1:nrow(params_df)) {
  params <- as.list(params_df[i, ])
  
  # Make the GET request
  response <- GET(
    url, 
    query = params, 
    add_headers(Authorization = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiI2IiwianRpIjoiNzczZWYyODBiYmNkNzc3ZDBjYjA4ZTNjNTg1ZGQxMmM5Y2UxY2FkZTRhZTNhZjM2MjQ3ZGZmYzk5ODhjZTE5NTFjZjY1YTU2MGU0NzQ5YWIiLCJpYXQiOjE3NjE2ODM0MTUuMDQzODI4LCJuYmYiOjE3NjE2ODM0MTUuMDQzODMsImV4cCI6MjA3NzIxNjIxNS4wMzk2NDYsInN1YiI6IjExMjAzMyIsInNjb3BlcyI6W119.DZuRjWQH35VEiDjznBbzaCr_CEf3mor2cGFWDQl0u7V4x1Lm9g_YUIieM-Ak1q0RtlsLDcJe75rpcN_ZqB84aA")
  )
  
  # Parse JSON
  hud_data <- fromJSON(content(response, "text"))
  
  # Convert to data frame and store in list
  all_data[[i]] <- as.data.frame(hud_data)
  
  # pause between requests 
  Sys.sleep(0.5)
}

# Combine all results into a single data frame
hud_data_df <- bind_rows(all_data)

# View first few rows
head(hud_data_df)

names(hud_data_df)


# flatten the nested results list into a proper data frame
hud_data_clean <- hud_data_df %>%  # extract the list of results
  as_tibble() %>%
  rename(
    FIPS = data.results.geoid,
    Zip = data.results.zip,
    city = data.results.city,
    state = data.results.state,
    year = data.year,
    quarter = data.quarter
  )

# check the result
head(hud_data_clean)
str(hud_data_clean)


#load LA poverty data
la_poverty_stats <- read_csv("C:/Temp/577/Phase1Deliverables/data/LA_Poverty_Stats.csv")

# take a quick look
glimpse(la_poverty_stats)

#select + rename key columns
poverty_data_clean <- la_poverty_stats %>%
  dplyr::select(
    `Geographic Identifier - FIPS Code`,
    `Total Population For Whom Poverty Status is Determined`,
    `Population whose income in the past 12 months is below federal poverty level`
  ) %>%
  rename(
    FIPS = `Geographic Identifier - FIPS Code`,
    total_population_in_poverty = `Total Population For Whom Poverty Status is Determined`,
    population_below_federal_poverty = `Population whose income in the past 12 months is below federal poverty level`
  )

# Assign 2024 to every record
poverty_data_clean <- poverty_data_clean %>%
  mutate(year = "2024")

#quick checks
head(poverty_data_clean)
summary(poverty_data_clean$total_population_in_poverty)
summary(poverty_data_clean$population_below_federal_poverty)

# read 2023 population/poverty data
pop_2023 <- read_csv("C:/Temp/577/Phase1Deliverables/data/2023_Population.csv") %>%
  filter(CITY == "Los Angeles") %>%
  summarise(
    total_poverty = sum(POV23_TOTAL, na.rm = TRUE),
    total_population = sum(POP23_TOTAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    city = "Los Angeles",
    year = "2023",
    poverty_rate = round(total_poverty / total_population * 100, 2)
  )

#2024 poverty data
poverty_2024 <- poverty_data_clean %>%
  mutate(year = 2024) %>%
  left_join(
    dplyr::select(hud_data_clean, FIPS, city, state) %>% dplyr::distinct(),
    by = "FIPS"
  ) %>%
  # normalize city names to title case and remove extra spaces
  mutate(city = str_to_title(str_trim(city))) %>%
  filter(city == "Los Angeles") %>%        # keep only LA
  group_by(FIPS, city, state, year) %>%
  summarise(
    total_in_poverty = sum(population_below_federal_poverty, na.rm = TRUE),
    .groups = "drop"
  )


# Aggregate total_in_poverty to city level
poverty_2024_city <- poverty_2024 %>%
  group_by(city, year) %>%
  summarise(
    total_in_poverty = sum(total_in_poverty, na.rm = TRUE),
    .groups = "drop"
  )

#2024 population data
pop_2024 <- read_csv("C:/Temp/577/Phase1Deliverables/data/2024_Total_Population.csv")

#select and rename columns
pop_2024_la <- pop_2024 %>%
  dplyr::select(Name, `Total Population Number`) %>%       # adjust column name if needed
  rename(
    city = Name,
    total_population = `Total Population Number`
  ) %>%
  # Standardize Los Angeles name
  mutate(
    city = ifelse(city == "City of Los Angeles", "Los Angeles", city),
    year = 2024
  ) %>%
  filter(city == "Los Angeles")             # keep only LA

#rename poverty
poverty_2024_city <- poverty_2024_city %>%
  rename(total_poverty = total_in_poverty)

# Combine total population and population in poverty for 2024
la_2024_combined <- poverty_2024_city %>%
  left_join(pop_2024_la, by = c("city", "year")) %>%
  # Optional: compute poverty rate
  mutate(
    poverty_rate = round(total_poverty / total_population * 100, 2)
  )

# Check result
la_2024_combined

# Make  numeric
# Convert year to numeric in both tables
pop_2023 <- pop_2023 %>%
  mutate(year = as.numeric(year))  # 2023 is likely already numeric

la_2024_combined <- la_2024_combined %>%
  mutate(year = as.numeric(year))  # convert "2024" character to numeric

# Now combine
la_poverty_all <- bind_rows(pop_2023, la_2024_combined)

# Check result
la_poverty_all

# Compute growth rates from 2023 → 2024
pop_growth <- (la_poverty_all$total_population[la_poverty_all$year == 2024] -
                 la_poverty_all$total_population[la_poverty_all$year == 2023]) /
  la_poverty_all$total_population[la_poverty_all$year == 2023]

poverty_growth <- (la_poverty_all$total_poverty[la_poverty_all$year == 2024] -
                     la_poverty_all$total_poverty[la_poverty_all$year == 2023]) /
  la_poverty_all$total_poverty[la_poverty_all$year == 2023]

# Extrapolate 2025
la_2025 <- tibble(
  city = "Los Angeles",
  year = 2025,
  total_population = round(la_poverty_all$total_population[la_poverty_all$year == 2024] * (1 + pop_growth)),
  total_poverty = round(la_poverty_all$total_poverty[la_poverty_all$year == 2024] * (1 + poverty_growth))
) %>%
  mutate(
    poverty_rate = round(total_poverty / total_population * 100, 2)
  )

# Combine with existing 2023–2024 data
la_poverty_all <- bind_rows(la_poverty_all, la_2025)

# View final table
la_poverty_all


# Ensure year is treated as a factor for discrete x-axis
la_poverty_totals <- la_poverty_all %>%
  mutate(year = factor(year))

# Bar chart
ggplot(la_poverty_totals, aes(x = year, y = total_poverty, fill = year)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = format(total_poverty, big.mark = ",")), 
            vjust = -0.5, size = 5) +
  labs(title = "Total Population in Poverty in Los Angeles (2023–2025)",
       y = "Population in Poverty", x = "Year") +
  scale_fill_manual(values = c("steelblue", "forestgreen", "orange")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#rent burden cont.
# Identify all duplicated column pairs
dup_cols <- names(rent_burden_df)[str_detect(names(rent_burden_df), "_1$")]

# Remove the _1 columns
rent_burden_df <- rent_burden_df %>% 
  dplyr::select(-all_of(dup_cols))

#we will look at this data as a single "snapshot" of LA's economic situation
#1: compute overall average for rent burden data
rent_summary <- rent_burden_df %>%
  summarise(
    mean_rent_burden = mean(B25070_calc_pctGE30pctE, na.rm = TRUE),
    median_rent_burden = median(B25070_calc_pctGE30pctE, na.rm = TRUE),
    mean_mortage_burden = mean(B25091_calc_pctMortGE30pctE, na.rm = TRUE)
  )

#2: compute shelter occupancy (LA only)
la_monthly_summary <- homeless_shelter_data %>%
  filter(city == "Los Angeles") %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%
  group_by(month) %>%
  summarise(
    mean_occupancy = mean(occupancy_rate, na.rm = TRUE),
    max_occupancy = max(occupancy_rate, na.rm = TRUE),
    min_occupancy = min(occupancy_rate, na.rm = TRUE),
    .groups = 'drop'
  )

#3:compute overall mean occupancy to match single rent burden value
overall_mean_occupancy <- mean(la_monthly_summary$mean_occupancy, na.rm = TRUE)

#4:combine into single table
la_econ_combined <- tibble(
  metric = c("Rent Burden (≥30%)", "Shelter Occupancy"),
  value = c(rent_summary$mean_rent_burden, overall_mean_occupancy)
)

#5:visualize the comparison
ggplot(la_econ_combined, aes(x = metric, y = value, fill = metric)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(value,1), "%")), vust = -0.5, size = 5) +
  labs(
    title = "Comparison: Rent Burden vs Shelter Occupancy in Los Angeles",
    y = "Percentage", x =""
  ) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#6:MIGHT GET RID OF- overall monthly occupancy trend
ggplot(la_monthly_summary, aes(x = month, y = mean_occupancy)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_hline(yintercept = rent_summary$mean_rent_burden, color = "darkorange", linetype = "dashed") +
  labs(
    title = "Monthly Shelter Occupancy in LA vs. Rent Burden",
    x = "Month",
    y = "Average Shelter Occupancy (%)",
    caption = "Dashed line = mean rent burden (%)"
  ) +
  theme_minimal(base_size = 14)

#7:mortgage burden
mortgage_summary <- rent_burden_df %>%
  summarise(
    mean_mortgage_burden = mean(B25091_calc_numMortGE30pctE, na.rm = TRUE)
  )

#single snapshot comparison of mortgage burden and shelter occupancy
mortgage_econ_combine <- tibble(
  metric = c("Mortgage Burden (≥30%)", "Shelter Occupancy"),
  value = c(mortgage_summary$mean_mortgage_burden, overall_mean_occupancy)
)

#plot it
ggplot(mortgage_econ_combine, aes(x = metric, y = value, fill = metric)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(value,1), "%")), vjust = -0.5, size = 5) +
  labs(
    title = "Comparison: Mortgage Burden vs Shelter Occupancy in Los Angeles",
    y = "Percentage", x = ""
  ) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#8:combine rent, mortgage, and shelter occupancy
combined_econ <- tibble(
  metric = c("Rent Burden (≥30%)", "Mortgage Burden (≥30%)", "Shelter Occupancy"),
  value = c(rent_summary$mean_rent_burden, mortgage_summary$mean_mortgage_burden, overall_mean_occupancy)
)

#plot it
ggplot(combined_econ, aes(x = metric, y = value, fill = metric)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(value,1), "%")), vjust = -0.5, size = 5) +
  labs(
    title = "Comparison: Rent, Mortgage Burden & Shelter Occupancy in Los Angeles",
    y = "Percentage", x = ""
  ) +
  scale_fill_manual(values = c("steelblue", "forestgreen", "tomato")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#how are individual shelters affected by economic burdens?
#1: compute average occupancy by shelter in LA
la_shelter_summary <- homeless_shelter_data %>%
  filter(city == "Los Angeles") %>%
  group_by(shelter_name) %>%
  summarise(
    avg_occupancy = mean(occupancy_rate, na.rm = TRUE),
    max_occupancy = max(occupancy_rate, na.rm = TRUE),
    min_occupancy = min(occupancy_rate, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_occupancy))

#view top shelters by occupancy
head(la_shelter_summary, 5)

#visualize shelter-level variability
ggplot(la_shelter_summary, aes(x = reorder(shelter_name, avg_occupancy), y = avg_occupancy)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = rent_summary$mean_rent_burden, color = "orange", linetype = "dashed") +
  geom_hline(yintercept = mortgage_summary$mean_mortgage_burden, color = "green", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "LA Shelters by Average Occupancy vs Rent & Mortgage Burden",
    y = "Average Occupancy (%)",
    x = "Shelter Name",
    caption = "Dashed lines = Citywide Rent (orange) & Mortgage (green) burden (%)"
  ) +
  theme_minimal(base_size = 12)

#-------------------------------------------------------------------------------
#RQ3.5: Scenario Analysis - simulate where economic conditions worsen and its estimated impact on LA shelter occupancy
#-------------------------------------------------------------------------------
library(tidyverse)

#  Base values from datasets - use 2024 as the baseline
base_rent <- rent_summary$mean_rent_burden                  # % of households rent burdened ≥30%
base_mortgage <- mortgage_summary$mean_mortgage_burden      # mortgage burden
base_poverty <- la_poverty_all$poverty_rate[la_poverty_all$year == 2024][1]  # 2024 poverty rate
base_occupancy <- overall_mean_occupancy                    # avg LA shelter occupancy (%)

# Define stress scenarios (relative % changes)
scenarios <- tibble(
  scenario = c("Baseline (2024)", "Mild Stress", "Moderate Stress", "Severe Stress"),
  rent_change = c(0, 0.05, 0.10, 0.15),        # +5%, +10%, +15% rent burden shocks
  mortgage_change = c(0, 0.03, 0.05, 0.07),    # +3%, +5%, +7% mortgage burden
  poverty_change = c(0, 0.02, 0.04, 0.08)      # +2%, +4%, +8% poverty
)

# Elasticity assumptions (based on literature: Pew, Enterprise, BLS)
elasticity_rent <- 1.2       # 1% ↑ rent burden → +1.2% shelter occupancy
elasticity_mortgage <- 0.6   # 1% ↑ mortgage burden → +0.6% shelter occupancy
elasticity_poverty <- 0.8    # 1% ↑ poverty rate → +0.8% shelter occupancy

# Apply scenario impacts
scenarios <- scenarios %>%
  mutate(
    rent_burden = base_rent * (1 + rent_change),
    mortgage_burden = base_mortgage * (1 + mortgage_change),
    poverty_rate = base_poverty * (1 + poverty_change),
    predicted_occupancy = base_occupancy * (1 + 
                                              elasticity_rent * rent_change +
                                              elasticity_mortgage * mortgage_change +
                                              elasticity_poverty * poverty_change)
  )

# View scenario table
print(scenarios %>% 
        select(scenario, rent_burden, mortgage_burden, poverty_rate, predicted_occupancy))

# Visualization
ggplot(scenarios, aes(x = scenario, y = predicted_occupancy, fill = scenario)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(predicted_occupancy, 1), "%")), 
            vjust = -0.5, size = 5) +
  labs(
    title = "Projected Shelter Occupancy under Economic Stress Scenarios (Los Angeles)",
    subtitle = "Based on literature-informed elasticities linking rent, mortgage, and poverty to homelessness",
    y = "Predicted Shelter Occupancy (%)",
    x = ""
  ) +
  scale_fill_manual(values = c("steelblue", "goldenrod", "orange", "red")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



#-------------------------------------------------------------------------------
#RQ4: How does government funding or program support relate to occupancy rates?
#-------------------------------------------------------------------------------

#load data
gov_assistance<-read_csv("C:/Temp/577/Phase1Deliverables/data/2025-hhap-annual-report.csv") %>%
  rename(
    funding_round       = `Funding Round`,
    activity_description = `Activity Description`,
    youth_set_aside     = `Funded as part of Youth Set-Aside?`,
    eligible_use        = `Eligible Use`,
    address             = `Address or General Location (if applicable)`,
    payment_to          = `Payment to / Contracted with`,
    subcontracted       = `Was this activity subcontracted? (Y/N)`,
    amount_obligated    = `Amount Obligated`,
    amount_expended     = `Amount Expended`,
    activity_status     = `Activity Status`,
    completion_date     = `Completion Date or Estimated Completion Date`,
    sbms_targeted       = `SPMs Targeted for Improvement (all that apply)`
  )

#drop unnecessary columns
gov_assistance <- gov_assistance %>%
  dplyr::select(-...13, -...14, -...15, -...16, -...17)

#clean payment_to
gov_assistance <- gov_assistance %>%
  # Ensure character type and replace NAs
  mutate(payment_to = as.character(payment_to),
         payment_to = replace_na(payment_to, "")) %>%
  # Standardize "City of Los Angeles" to "Los Angeles"
  mutate(payment_to = ifelse(str_detect(tolower(payment_to), "city of los angeles"),
                             "Los Angeles",
                             payment_to))

#clean numeric columns
gov_assistance <- gov_assistance %>%
  mutate(
    amount_obligated = as.numeric(gsub("[\\$,]", "", amount_obligated)),
    amount_expended  = as.numeric(gsub("[\\$,]", "", amount_expended))
  )

#standardize categorical columns 
gov_assistance <- gov_assistance %>%
  mutate(
    subcontracted = ifelse(tolower(subcontracted) %in% c("yes","y"), "Yes", "No"),
    youth_set_aside = ifelse(tolower(youth_set_aside) %in% c("yes","y"), "Yes", "No"),
    activity_status = str_to_title(activity_status)
  )

#convert date
gov_assistance <- gov_assistance %>%
  mutate(completion_date = as.Date(completion_date, format = "%m/%d/%Y"))

#filter to just city of LA
gov_assistance_clean <- gov_assistance %>%
  filter(payment_to != "LAHSA") %>%  # remove county-level rows
  mutate(payment_to = case_when(
    payment_to == "Los Angeles" ~ "Los Angeles",
    TRUE ~ payment_to
  )) %>%
  # Replace empty strings with NA and remove
  mutate(payment_to = na_if(payment_to, "")) %>%
  filter(!is.na(payment_to))

#2:aggregate by year/month to compare with shelter occupancy rates
la_funding_summary <- gov_assistance_clean %>%
  filter(payment_to == "Los Angeles") %>%
  mutate(year = year(completion_date)) %>%
  group_by(year) %>%
  summarise(
    total_obligated = sum(amount_obligated, na.rm = TRUE),
    total_expended = sum(amount_expended, na.rm = TRUE),
    .groups = "drop"
  )

la_funding_monthly <- gov_assistance_clean %>%
  filter(payment_to == "Los Angeles") %>%
  mutate(month = floor_date(completion_date, "month")) %>%
  group_by(month) %>%
  summarise(
    total_obligated = sum(amount_obligated, na.rm = TRUE),
    total_expended  = sum(amount_expended, na.rm = TRUE),
    .groups = "drop"
  )

#join funding with occupancy
la_funding_occupancy <- la_monthly_summary %>%
  left_join(la_funding_monthly, by = "month")

#normalize both funding and occupancy so trends are visually comparable
install.packages("scales")   # only if not already installed
library(scales)
library(ggplot2)

la_funding_occupancy <- la_funding_occupancy %>%
  mutate(
    total_obligated_scaled = rescale(total_obligated),   # scales to 0-1
    mean_occupancy_scaled  = rescale(mean_occupancy)      # scales to 0-1
  )

#plot it
ggplot(la_funding_occupancy, aes(x = month)) +
  geom_col(aes(y = total_obligated_scaled), fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = mean_occupancy_scaled), color = "tomato", size = 1.2) +
  labs(
    title = "LA Funding vs Shelter Occupancy (Normalized)",
    x = "Month",
    y = "Scaled Values (0-1)"
  ) +
  theme_minimal(base_size = 12)

#exploring statistical relationship between scaled occupancy and scaled obligated dollars
cor_obligated <- cor(
  la_funding_occupancy$mean_occupancy,
  la_funding_occupancy$total_obligated,
  use = "complete.obs"
)

#explore statistical relationship between scaled occupancy and scaled expended funding
cor_expended <- cor(
  la_funding_occupancy$mean_occupancy,
  la_funding_occupancy$total_expended,
  use = "complete.obs"
)

cor_obligated
cor_expended

#explore simple linear regression of occupancy ~ obligated funding
lm_obligated <- lm(mean_occupancy ~ total_obligated, data = la_funding_occupancy)
summary(lm_obligated)

#explore simple linear regression of occupancy ~ expended funding
lm_expended <- lm(mean_occupancy ~ total_expended, data = la_funding_occupancy)
summary(lm_expended)

#multiple regression
lm_both <- lm(mean_occupancy ~ total_obligated + total_expended, data = la_funding_occupancy)
summary(lm_both)

#visualize obligated funding
ggplot(la_funding_occupancy, aes(x = total_obligated, y = mean_occupancy)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", color = "tomato", se = TRUE) +
  labs(
    title = "LA Shelter Occupancy vs Total Obligated Funding",
    x = "Total Obligated Funding (Scaled 0-1)",
    y = "Average Occupancy (Scaled 0-1)"
  ) +
  theme_minimal(base_size = 12)

#visualize expended funding
ggplot(la_funding_occupancy, aes(x = total_expended, y = mean_occupancy)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(
    title = "LA Shelter Occupancy vs Total Expended Funding",
    x = "Total Expended Funding (Scaled 0-1)",
    y = "Average Occupancy (Scaled 0-1)"
  ) +
  theme_minimal(base_size = 12)

#general context to funding data
#1:summarize total obligated and expended by funding round
funding_round_summary <- gov_assistance_clean  %>%
  group_by(funding_round) %>%
  summarise(
    total_obligated = sum(amount_obligated, na.rm = TRUE),
    total_expended = sum(amount_expended, na.rm = TRUE),
    n_activities = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_obligated))

funding_round_summary

#2: summarize funding by eligible use
eligible_use_summary <- gov_assistance_clean %>%
  group_by(eligible_use) %>%
  summarise(
    total_obligated = sum(amount_obligated, na.rm = TRUE),
    total_expended  = sum(amount_expended, na.rm = TRUE),
    n_activities    = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_obligated))

#visualize funding distributions
# Top 5 eligible use categories by total obligated
top_eligible_use <- eligible_use_summary %>%
  slice_max(total_obligated, n = 5)

ggplot(top_eligible_use, aes(x = reorder(eligible_use, total_obligated), y = total_obligated, fill = eligible_use)) +
  geom_col() +
  geom_text(aes(label = scales::dollar(total_obligated)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Funding Uses in Los Angeles HHAP Program",
    x = "Eligible Use",
    y = "Total Amount Obligated ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

#visualize total obligated vs total expended by eligibility use type
eligible_use_long <- eligible_use_summary %>%
  pivot_longer(
    cols = c(total_obligated, total_expended),
    names_to = "funding_type",
    values_to = "amount"
  )

ggplot(eligible_use_long %>% slice_max(amount, n = 10, by = amount), 
       aes(x = reorder(eligible_use, amount), y = amount, fill = funding_type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::dollar(amount)), 
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Funding Obligated vs Expended by Eligible Use (Top 10)",
    x = "Eligible Use",
    y = "Amount ($)",
    fill = "Funding Type"
  ) +
  theme_minimal(base_size = 12)

# ADVANCED ANALYSIS SECTION IN PHASE 2 #

# Save a data frame as CSV
#write.csv(homeless_shelter_data, "C:/Temp/577/Phase1Deliverables/data/cleaned_shelter_data.csv", row.names = FALSE)

# Save a data frame as CSV
#write.csv(rent_burden_df, "C:/Temp/577/Phase1Deliverables/data/cleaned_economic_factors.csv", row.names = FALSE)

# Save a data frame as CSV
#write.csv(shelter_data_with_temperature, "C:/Temp/577/Phase1Deliverables/data/cleaned_shelter_data_and_temperature.csv", row.names = FALSE)

# Save a data frame as CSV
#write.csv(gov_assistance_clean, "C:/Temp/577/Phase1Deliverables/data/cleaned_government_assistance.csv", row.names = FALSE)

# Save a data frame as CSV
#write.csv(temperature_data, "C:/Temp/577/Phase1Deliverables/data/cleaned_temperature_data.csv", row.names = FALSE)


#-------------------------------
#Phase 2: More advanced analysis
#-------------------------------

#-----------------------------------------------
#Step 1: Cleanse and merge all data together
#-----------------------------------------------
#rename payment_to field to align with shelter data
gov_assistance_clean <- gov_assistance_clean %>%
  rename(city = payment_to)

# Clean up the County column to create a city column
head(rent_burden_df$County)
rent_burden_df <- rent_burden_df %>%
  mutate(city = str_remove(County, regex(" County$", ignore_case = TRUE)),
         city = str_trim(city))
rent_summary <- rent_summary %>%
  mutate(city = "Los Angeles")


#Check result
head(rent_burden_df$city)


#clean government funding data
gov_assistance_clean <- gov_assistance %>%
  # Standardize numeric fields
  mutate(
    amount_obligated = as.numeric(gsub("[\\$,]", "", amount_obligated)),
    amount_expended  = as.numeric(gsub("[\\$,]", "", amount_expended)),
    payment_to = as.character(payment_to),
    payment_to = ifelse(str_detect(tolower(payment_to), "city of los angeles"),
                        "Los Angeles", payment_to),
    completion_date = as.Date(completion_date, format = "%m/%d/%Y")
  ) %>%
  filter(payment_to == "Los Angeles") %>%
  dplyr::select(payment_to, completion_date, amount_obligated, amount_expended)

# aggregate to monthly level
la_funding_monthly <- gov_assistance_clean %>%
  mutate(month = floor_date(completion_date, "month"),
         year = year(month),
         month_num = month(month)) %>%
  group_by(year, month_num, month) %>%
  summarise(
    total_obligated = sum(amount_obligated, na.rm = TRUE),
    total_expended  = sum(amount_expended, na.rm = TRUE),
    .groups = "drop"
  )

# spread funding over 12 months
expanded_funding <- la_funding_monthly %>%
  mutate(
    year = lubridate::year(month)  # extract year from month
  ) %>%
  group_by(year) %>%
  summarise(
    total_obligated = sum(total_obligated, na.rm = TRUE),
    total_expended  = sum(total_expended, na.rm = TRUE)
  ) %>%
  rowwise() %>%   # <-- THIS FIXES THE ERROR
  mutate(
    month = list(seq(as.Date(paste0(year, "-01-01")), by = "month", length.out = 12)),
    monthly_obligated = total_obligated / 12,
    monthly_expended  = total_expended / 12
  ) %>%
  unnest(month) %>%
  mutate(month = as.Date(month)) %>%   # ← THIS FIXES WHAT YOU'RE SEEING
  dplyr::select(month, monthly_obligated, monthly_expended)

# aggregate monthly shelter occupancy
la_monthly_summary <- homeless_shelter_data %>%
  filter(city == "Los Angeles") %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%
  group_by(month) %>%
  summarise(
    occupancy_rate = mean(occupancy_rate, na.rm = TRUE),
    .groups = "drop"
  )

# create merged data
merged_data <- shelter_data_with_temperature %>% 
  mutate( year = year(date), month_num = month(date) )
#-------------------------------------------------------------------------------
 #Step 2: RQ2 Advanced Analysis
 #Goals: Test lagged temperature effects, nonlinear responses, mixed models,
 #Granger causality, ARIMAX forecasting with exogeneous weather, and validation.
#-------------------------------------------------------------------------------

#load required packages
packages <- c("mgcv","lme4","broom","forecast","vars","tsibble","feasts","tidyr","rsample","yardstick","purrr")
invisible(lapply(packages, function(p) if(!requireNamespace(p, quietly=TRUE)) install.packages(p)))
library(dplyr); library(lubridate); library(ggplot2); library(mgcv)
library(lme4); library(broom); library(forecast); library(vars)
library(tsibble); library(feasts); library(tidyr); library(rsample); library(yardstick); library(purrr)

# standardize + rename columns

combined_monthly <- combined_monthly %>%
  mutate(
    date = as.Date(month),        # Convert date column to Date type
    city = as.character(city),   # Ensure city is character for grouping
    region = as.character(region), # Ensure region is character
    season = as.factor(season)     # Season as factor for plotting / ANOVA
  ) %>%
  arrange(city, month)            # Sort by city and chronological order

#drop month column
combined_monthly <- combined_monthly %>%
  dplyr::select(-month)

#densify time series per city
combined_monthly <- combined_monthly %>%
  group_by(city) %>%
  complete(
    date = seq(min(date), max(date), by = "month")  # Create rows for any missing months
  ) %>%
  # Fill forward/backward any missing temperature, occupancy, season
  fill(avg_temp, avg_occupancy, season, region, .direction = "downup") %>%
  ungroup()

#create lagged temp variables
combined_monthly <- combined_monthly %>%
  group_by(city) %>%        # Lag separately per city
  arrange(date) %>%         # Ensure chronological order
  mutate(
    temp_lag1 = lag(avg_temp, 1),  # 1-month lag
    temp_lag2 = lag(avg_temp, 2),  # 2-month lag
    temp_lag3 = lag(avg_temp, 3)   # 3-month lag
  ) %>%
  ungroup()

# How many NAs are in the lag columns? (first few months will be NA)
sum(is.na(combined_monthly$temp_lag1))
sum(is.na(combined_monthly$temp_lag2))
sum(is.na(combined_monthly$temp_lag3))

#distributed-lag regression (linear)
#model occupancy as a function of current and previous3 months' temperatures
#add season as factor and region interaction (LA vs Other)
# Model occupancy as a function of current and previous 3 months' temperatures
# Add season as a factor and region interaction (LA vs Other)
temp_lag_model <- lm(
  avg_occupancy ~ avg_temp + temp_lag1 + temp_lag2 + temp_lag3 + season * region,
  data = combined_monthly
)

summary(temp_lag_model)

#nonlinear temperature effect
#using a natural spline for temperature to capture non-linear impact
library(splines)
temp_spline_model <- lm(
  avg_occupancy ~ ns(avg_temp, df = 4) + ns(temp_lag1, df = 3) + season * region,
  data = combined_monthly
)

summary(temp_spline_model)

#visualize actual vs predicted occupancy
combined_monthly <- combined_monthly %>%
  mutate(
    predicted_occupancy = predict(temp_lag_model, newdata = combined_monthly)
  )

ggplot(combined_monthly, aes(x = date, y = avg_occupancy, color = region)) +
  geom_line(size = 1) +
  geom_line(aes(y = predicted_occupancy), linetype = "dashed", size = 1) +
  facet_wrap(~region, scales = "free_y") +
  labs(
    title = "Observed vs Predicted Shelter Occupancy by Region",
    x = "Month",
    y = "Occupancy Rate"
  ) +
  theme_minimal()

#check for distributed lag significance
library(car)  # for linear hypothesis testing

# Are lagged temperature coefficients jointly significant?
linearHypothesis(temp_lag_model, c("temp_lag1 = 0", "temp_lag2 = 0", "temp_lag3 = 0"))

#partial dependence plots for temperature
install.packages("ggpubr")
library(ggpubr)

# Visualize effect of temp on occupancy while holding other lags/seasons constant
ggplot(combined_monthly, aes(x = avg_temp, y = avg_occupancy)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ ns(x, df = 4), color = "tomato") +
  facet_wrap(~region) +
  labs(
    title = "Nonlinear Temperature Effect on Shelter Occupancy",
    x = "Average Monthly Temperature",
    y = "Occupancy Rate"
  ) +
  theme_minimal()

#--------------------------------------------------------------------------------------------
#Step 3: RQ3 Advanced Analysis
#Goal: Due to the data being only available at the city-level and
#having no access to census data, we will only perform scenario analysis to answer this question
#--------------------------------------------------------------------------------------------

#step 1: input baseline values (2024)
library(dplyr)

baseline_poverty_rate <- la_poverty_all %>%
  filter(year == 2024) %>%
  mutate(poverty_rate = total_poverty / total_population * 100) %>%
  pull(poverty_rate)

# step 2: overall mean shelter occupancy in 2024
overall_mean_occupancy_2024 <- la_monthly_summary %>%
  filter(lubridate::year(month) == 2024) %>%
  summarise(mean_occ = mean(occupancy_rate, na.rm = TRUE)) %>%
  pull(mean_occ)

#step 3: set up baseline table
# Baseline table
baseline <- tibble(
  metric = c("Rent Burden", "Mortgage Burden", "Poverty Rate"),
  value  = c(rent_summary$mean_rent_burden,
             mortgage_summary$mean_mortgage_burden,
             baseline_poverty_rate),
  occupancy = overall_mean_occupancy_2024
)

#step 4: define scenario changes (+/- 10% and 20%)
scenarios <- tibble(
  change = c(-20, -10, 0, 10, 20) / 100
)

# step 5: create function to generate new occupancy based on economic change
# assume a simple proportional effect: +1% rent burden -> +0.2% occupancy
simulate_occupancy <- function(baseline_value, change_pct, effect_size = 0.2){
  new_value <- baseline_value * (1 + change_pct)
  new_occupancy <- overall_mean_occupancy_2024  * (1 + change_pct * effect_size)
  tibble(new_value = new_value, new_occupancy = new_occupancy)
}

#step 6: rent burden scenario analysis
library(tidyr)
rent_scenarios <- scenarios %>%
  rowwise() %>%
  mutate(sim = list(simulate_occupancy(baseline$value[baseline$metric == "Rent Burden"], change))) %>%
  unnest(sim) %>%
  mutate(scenario = paste0(ifelse(change < 0, "", "+"), change*100, "%"))

#step 7: plot results
ggplot(rent_scenarios, aes(x = scenario, y = new_occupancy, fill = scenario)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(new_occupancy,1)), vjust = -0.5) +
  labs(
    title = "Scenario Analysis: Rent Burden Impact on Shelter Occupancy (Baseline 2024)",
    x = "Scenario Change in Rent Burden",
    y = "Predicted Occupancy (%)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)

#Someone still needs to do the following: 
# (1) mortgage burden scenario chart
# (2) poverty rate scenario chart
# (3) combined scenario (rent + mortgage + poverty together)
# to save the plot results hit the export button and choose save as image
# then either upload to github to phase 2 figures or send them to me and i can push them through!

#-----------------------------------------------------------------------------------------------------
#Step 4: RQ4 Advanced Analysis
#Goals: Explore how daily/lagged government funding (obligated and expended) affects shelter occupancy
# Test for delayed effects using distributed log regression
# Capture potential nonlinear effects with Generalized Additive Models (GAMs)
# Visualize funding trends alongside occupancy rates
# Use ARIMAX to forecast occupancy while incorporating funding as an exogenous predictor
#-----------------------------------------------------------------------------------------------------

#load all required packages
packages <- c("dplyr","lubridate","ggplot2","mgcv","forecast","scales","tidyr")
invisible(lapply(packages, function(p) if(!requireNamespace(p, quietly=TRUE)) install.packages(p)))
library(dplyr); library(lubridate); library(ggplot2)
library(mgcv); library(forecast); library(scales); library(tidyr)

# merge funding and occupancy
merged_monthly <- la_monthly_summary %>%
  left_join(expanded_funding, by = "month") %>%
  replace_na(list(monthly_obligated = 0, monthly_expended = 0))

#create lagged funding variables
merged_monthly <- merged_monthly %>%
  arrange(month) %>%
  mutate(
    lag1_obligated = lag(monthly_obligated, 1, default = 0),
    lag2_obligated = lag(monthly_obligated, 2, default = 0),
    lag3_obligated = lag(monthly_obligated, 3, default = 0),
    lag1_expended  = lag(monthly_expended, 1, default = 0),
    lag2_expended  = lag(monthly_expended, 2, default = 0),
    lag3_expended  = lag(monthly_expended, 3, default = 0)
  )
#standardize for visualization
merged_monthly <- merged_monthly %>%
  mutate(
    occupancy_scaled = rescale(occupancy_rate),
    obligated_scaled = rescale(monthly_obligated),
    expended_scaled  = rescale(monthly_expended)
  )

#visualize trends
ggplot(merged_monthly, aes(x = month)) +
  geom_col(aes(y = obligated_scaled), fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = occupancy_scaled), color = "tomato", size = 1.2) +
  labs(
    title = "LA Shelter Occupancy vs Obligated Funding (Normalized)",
    x = "Month",
    y = "Scaled Values (0-1)"
  ) +
  theme_minimal(base_size = 12)

#prep for modeling
# Ensure occupancy dataset has a month column to join with funding
merged_data <- merged_data %>%
  mutate(month = as.Date(format(date, "%Y-%m-01")))

# create 1-month lagged funding terms
merged_monthly <- merged_monthly %>%
  #group_by(city) %>%
  arrange(month) %>%
  mutate(
    lag1month_obligated = lag(monthly_obligated, 1, default = 0),
    lag1month_expended  = lag(monthly_expended, 1, default = 0)
  ) %>%
  ungroup()

# Replace any remaining NAs with 0 for modeling
merged_monthly <- merged_monthly %>%
  mutate(
    monthly_obligated = replace_na(monthly_obligated, 0),
    monthly_expended  = replace_na(monthly_expended, 0),
    lag1month_obligated    = replace_na(lag1month_obligated, 0),
    lag1month_expended     = replace_na(lag1month_expended, 0)
  )

#re-explore correlations
corphase2_obligated <- cor(merged_monthly$occupancy_rate, merged_monthly$monthly_obligated, use = "complete.obs")
corphase2_expended  <- cor(merged_monthly$occupancy_rate, merged_monthly$monthly_expended,  use = "complete.obs")

cor_obligated
corphase2_obligated
cor_expended
corphase2_expended

# Count unique values for each variable - determine k
unique_monthly_obligated <- length(unique(merged_monthly$monthly_obligated))
unique_monthly_expended  <- length(unique(merged_monthly$monthly_expended))
unique_lag1month_obligated    <- length(unique(merged_monthly$lag1month_obligated))
unique_lag1month_expended     <- length(unique(merged_monthly$lag1month_expended))

unique_monthly_obligated
unique_monthly_expended
unique_lag1month_obligated
unique_lag1month_expended

#fit GAM model for 1 month lag
gam_model <- gam(occupancy_rate ~ 
                   s(monthly_obligated, k = 3) + 
                   s(monthly_expended, k = 3) + 
                   s(lag1month_obligated, k = 3) + 
                   s(lag1month_expended, k = 3),
                 data = merged_monthly)

summary(gam_model)

#visualize funding vs occupancy
#plot occupied vs monthly obligated
ggplot(merged_monthly, aes(x = monthly_obligated, y = occupancy_rate)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k =3), color = "tomato", se = TRUE) +
  labs(
    title = "Occupancy Rate vs Monthly Obligated Funding",
    x = "Monthly Obligated Funding",
    y = "Occupancy Rate"
  ) +
  theme_minimal(base_size = 12)

# explore GAM of longer-term lagged effect (2-3 months back)
gam_model_3month <- gam(
  occupancy_rate ~ 
    s(monthly_obligated, k = 3) + 
    s(lag1_obligated, k = 3) +
    s(lag2_obligated, k = 3) +
    s(lag3_obligated, k = 3) +
    s(monthly_expended, k = 3) + 
    s(lag1_expended, k = 3) +
    s(lag2_expended, k = 3) +
    s(lag3_expended, k = 3),
  data = merged_monthly
)
summary(gam_model_3month)
plot(gam_model_3month, pages = 1, shade = TRUE)

#gam model for 3 month lag

#ARIMAX Forecasting with Funding as Exogenous Variables
# we will convert occupancy rates to a time series
# and use funding (current + lag1) as exogenous predictors
library(forecast)

xreg_matrix <- merged_monthly %>%
  dplyr::select(monthly_obligated, monthly_expended, lag1_obligated, lag1_expended) %>%
  as.matrix()

occupancy_ts <- ts(
  merged_monthly$occupancy_rate, 
  start = c(year(min(merged_monthly$month)), month(min(merged_monthly$month))),
  frequency = 12
)

arimax_model <- auto.arima(occupancy_ts, xreg = xreg_matrix)
summary(arimax_model)

checkresiduals(arimax_model)

# Forecast next 6 months (replace zeros with realistic estimates if available)
future_xreg <- matrix(0, nrow = 6, ncol = ncol(xreg_matrix))
future_xreg
colnames(future_xreg) <- colnames(xreg_matrix)

occupancy_forecast <- forecast(arimax_model, xreg = future_xreg, h = 6)
plot(occupancy_forecast)


