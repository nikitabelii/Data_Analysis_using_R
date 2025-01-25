# Homework 5 by Nikita Belii

# Loading necessary libraries
library(tidyverse)
library(tidyr)
library(readr)
library(writexl)

# Loading the WHO TB dataset
who <- tidyr::who
who

# Part 1: Tidying up the data

# Tidying the dataset
who_tidy <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65,  # Selecting columns to pivot
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>%
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

who_tidy

# Saving the tidy data to an Excel file
write_xlsx(who_tidy, path = "tidy_who_data.xlsx")

# Part 2: EDA on the Flights Dataset

# QUESTION 1: Which of the three New York airports in your dataset is the busiest?

# Loading the nycflights13 dataset
library(nycflights13)
data("flights")

# Counting the number of flights from each airport
busiest_airport <- flights %>%
  group_by(origin) %>%
  summarise(total_flights = n()) %>%
  arrange(desc(total_flights))

busiest_airport


# QUESTION 2: How many different destination airports does your dataset contain and what is the most popular destination (airport code)?

# Counting unique destination airports and finding the most popular destination
destination_info <- flights %>%
  group_by(dest) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

num_destinations <- n_distinct(flights$dest)
most_popular_destination <- destination_info[1, ]

list(num_destinations, most_popular_destination)

# QUESTION 3: How many flights departed from LGA on Thanksgiving Day, 2013?

# Counting flights from LGA on Thanksgiving Day, 2013
thanksgiving_flights <- flights %>%
  filter(origin == "LGA", year == 2013, month == 11, day == 28) %>%
  tally()

thanksgiving_flights

# QUESTION 4: What was the busiest day of the year for JFK?

# Identifying the busiest day of the year for JFK
busiest_day_jfk <- flights %>%
  filter(origin == "JFK") %>%
  group_by(year, month, day) %>%
  summarise(total_flights = n()) %>%
  arrange(desc(total_flights)) %>%
  slice(1)

busiest_day_jfk

# QUESTION 5: What was the busiest month of the year for LGA?

# Identifying the busiest month of the year for LGA
busiest_month_lga <- flights %>%
  filter(origin == "LGA") %>%
  group_by(month) %>%
  summarise(total_flights = n()) %>%
  arrange(desc(total_flights)) %>%
  slice(1)

busiest_month_lga

# QUESTION 6: What is the longest flight during the month of June in the dataset?

# Finding the longest flight in June
longest_flight_june <- flights %>%
  filter(month == 6) %>%
  arrange(desc(distance)) %>%
  slice(1)

longest_flight_june

# QUESTION 7: What is the shortest flight during the month of May in the dataset?

# Finding the shortest flight in May
shortest_flight_may <- flights %>%
  filter(month == 5) %>%
  arrange(distance) %>%
  slice(1)

shortest_flight_may

# QUESTION 8: Which carrier had the smallest number of flights?

# Finding the carrier with the smallest number of flights
smallest_carrier <- flights %>%
  group_by(carrier) %>%
  summarise(total_flights = n()) %>%
  arrange(total_flights) %>%
  slice(1)

smallest_carrier

# QUESTION 9: Which airline had the shortest average delay per flight departing from JFK?

# Calculating the shortest average delay per flight for airlines departing from JFK
shortest_delay_jfk <- flights %>%
  filter(origin == "JFK") %>%
  group_by(carrier) %>%
  summarise(average_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(average_delay) %>%
  slice(1)

shortest_delay_jfk

# QUESTION 10: Which airline had the longest average delay per flight departing from LGA?

# Calculating the longest average delay per flight for airlines departing from LGA
longest_delay_lga <- flights %>%
  filter(origin == "LGA") %>%
  group_by(carrier) %>%
  summarise(average_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(average_delay)) %>%
  slice(1)

longest_delay_lga

# QUESTION 11: What was the worst day of the year to catch a flight from LGA?

# Identifying the worst day of the year to catch a flight from LGA
worst_day_lga <- flights %>%
  filter(origin == "LGA") %>%
  group_by(year, month, day) %>%
  summarise(average_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(average_delay)) %>%
  slice(1)

worst_day_lga

# QUESTION 12: What percentage of flights departing from JFK had a delay of less than 5% of the total flight time?

# Calculating the percentage of flights with delay less than 5% of total flight time
percentage_delay_jfk <- flights %>%
  filter(origin == "JFK", !is.na(dep_delay), !is.na(air_time)) %>%
  mutate(delay_percentage = dep_delay / (air_time * 60) * 100) %>%
  filter(delay_percentage < 5) %>%
  summarise(percentage = (n() / nrow(flights[flights$origin == "JFK",])) * 100)

percentage_delay_jfk

# QUESTION 13: Which airline had the shortest number of flights delayed by more than 3 hours between October and December?

# Identifying the airline with the fewest delays over 3 hours from Oct to Dec
fewest_long_delays_airline <- flights %>%
  filter(month %in% 10:12, dep_delay > 180) %>%
  group_by(carrier) %>%
  summarise(total_delays = n()) %>%
  arrange(total_delays) %>%
  slice(1)

fewest_long_delays_airline

# Part 3: Creating Plots for the Flights Dataset

# PLOT 1: Total number of departures per month per departure airport (“origin”)

# Required library for plotting
library(ggplot2)

# Plotting total number of departures per month per departure airport
plot1 <- flights %>%
  group_by(origin, month) %>%
  summarise(total_departures = n()) %>%
  ggplot(aes(x = month, y = total_departures, fill = origin)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Number of Departures per Month per Airport",
       x = "Month",
       y = "Total Departures")

plot1

# PLOT 2: Average departure delay for flights departing from JFK per month

# Plotting average departure delay for JFK per month
plot2 <- flights %>%
  filter(origin == "JFK") %>%
  group_by(month) %>%
  summarise(average_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = average_delay)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Departure Delay for JFK per Month",
       x = "Month",
       y = "Average Delay (minutes)")

plot2

# PLOT 3: Total number of flights departing from JFK per airline/carrier

# Plotting total number of flights from JFK per airline
plot3 <- flights %>%
  filter(origin == "JFK") %>%
  group_by(carrier) %>%
  summarise(total_flights = n()) %>%
  ggplot(aes(x = reorder(carrier, total_flights), y = total_flights)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Number of Flights from JFK per Airline",
       x = "Airline",
       y = "Total Flights")

plot3

# PLOT 4: Statistical distribution of departure delays for the 5 busiest carriers

# Identifying the 5 busiest carriers
top_5_carriers <- flights %>%
  group_by(carrier) %>%
  summarise(total_flights = n()) %>%
  top_n(5, total_flights)

# Plotting departure delays for these carriers
plot4 <- flights %>%
  filter(carrier %in% top_5_carriers$carrier) %>%
  ggplot(aes(x = dep_delay, fill = carrier)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  labs(title = "Departure Delays Distribution for the 5 Busiest Carriers",
       x = "Departure Delay (minutes)",
       y = "Frequency")

plot4

# PLOT 5: Total number of flights departing from LGA per month

# Plotting total number of flights from LGA per month
plot5 <- flights %>%
  filter(origin == "LGA") %>%
  group_by(month) %>%
  summarise(total_flights = n()) %>%
  ggplot(aes(x = month, y = total_flights)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Number of Flights from LGA per Month",
       x = "Month",
       y = "Total Flights")

plot5

# Part 4: EDA on the who_tidy dataset

# QUESTION 1: Which countries had the largest and smallest number of TB cases in 2012?

# Finding countries with the largest and smallest number of TB cases in 2012
tb_2012 <- who_tidy %>%
  filter(year == 2012) %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases))

largest_cases_country <- tb_2012[1, ]
smallest_cases_country <- tb_2012[nrow(tb_2012), ]

list(largest_cases_country, smallest_cases_country)

# QUESTION 2: Plot the number of cases per gender for Australia over time, for the period covered in the dataset for that country.

# Plotting TB cases per gender for Australia over time
plot_australia_cases <- who_tidy %>%
  filter(country == "Australia") %>%
  group_by(year, sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_cases, color = sex)) +
  geom_line() +
  labs(title = "TB Cases per Gender in Australia Over Time",
       x = "Year",
       y = "Total Cases")

plot_australia_cases

# QUESTION 3: Plot the total number of cases per gender for Afghanistan over time, for the period between 2000 and 2013.

# Plotting TB cases per gender for Afghanistan from 2000 to 2013
plot_afghanistan_cases <- who_tidy %>%
  filter(country == "Afghanistan", year >= 2000, year <= 2013) %>%
  group_by(year, sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_cases, color = sex)) +
  geom_line() +
  labs(title = "TB Cases per Gender in Afghanistan (2000-2013)",
       x = "Year",
       y = "Total Cases")

plot_afghanistan_cases

# QUESTION 4: Compute and plot (pie chart?) the percentage of cases per age group for the 3 North American countries of largest population (USA, Canada and Mexico) in the longest possible period covered by the dataset for all those countries.

# Computing and plotting cases per age group for USA, Canada, and Mexico
north_american_cases <- who_tidy %>%
  filter(country %in% c("United States of America", "Canada", "Mexico")) %>%
  group_by(country, age) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100)

plot_na_cases <- ggplot(north_american_cases, aes(x = "", y = percentage, fill = age)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ country) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of TB Cases per Age Group in North America",
       x = "",
       y = "Percentage")

plot_na_cases

# QUESTION 5: Repeat question 4 for the 3 South American countries of largest population.

# South American countries: Brazil, Colombia, Argentina
south_american_cases <- who_tidy %>%
  filter(country %in% c("Brazil", "Colombia", "Argentina")) %>%
  group_by(country, age) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100)

plot_sa_cases <- ggplot(south_american_cases, aes(x = "", y = percentage, fill = age)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ country) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of TB Cases per Age Group in South America",
       x = "",
       y = "Percentage")

plot_sa_cases

# QUESTION 6: Repeat question 4 for the 3 European countries of largest population.

# European countries: Russia, Germany, United Kingdom
european_cases <- who_tidy %>%
  filter(country %in% c("Russian Federation", "Germany", "United Kingdom")) %>%
  group_by(country, age) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100)

plot_eu_cases <- ggplot(european_cases, aes(x = "", y = percentage, fill = age)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ country) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of TB Cases per Age Group in Europe",
       x = "",
       y = "Percentage")

plot_eu_cases

# QUESTION 7: Repeat question 4 for the 3 African countries of largest population.

# African countries: Nigeria, Ethiopia, Egypt
african_cases <- who_tidy %>%
  filter(country %in% c("Nigeria", "Ethiopia", "Egypt")) %>%
  group_by(country, age) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100)

plot_af_cases <- ggplot(african_cases, aes(x = "", y = percentage, fill = age)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ country) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of TB Cases per Age Group in Africa",
       x = "",
       y = "Percentage")

plot_af_cases

# QUESTION 8: Repeat question 4 for the 3 Asian countries of largest population.

# Asian countries: China, India, Indonesia
asian_cases <- who_tidy %>%
  filter(country %in% c("China", "India", "Indonesia")) %>%
  group_by(country, age) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100)

plot_as_cases <- ggplot(asian_cases, aes(x = "", y = percentage, fill = age)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ country) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of TB Cases per Age Group in Asia",
       x = "",
       y = "Percentage")

plot_as_cases

# QUESTION 9: Plot the total number of cases worldwide over time, per age group.

# Plotting total number of cases worldwide over time, per age group
world_cases <- who_tidy %>%
  group_by(year, age) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_cases, color = age)) +
  geom_line() +
  labs(title = "Total Number of TB Cases Worldwide Over Time, Per Age Group",
       x = "Year",
       y = "Total Cases")

world_cases

# QUESTION 10: Which country had the sharpest reduction (in % terms) in the total number of cases between 1997 and 2010?

# Calculating percentage reduction in TB cases between 1997 and 2010
case_reduction <- who_tidy %>%
  filter(year %in% c(1997, 2010)) %>%
  group_by(country) %>%
  summarise(cases_1997 = sum(ifelse(year == 1997, cases, NA), na.rm = TRUE),
            cases_2010 = sum(ifelse(year == 2010, cases, NA), na.rm = TRUE)) %>%
  mutate(reduction = (cases_1997 - cases_2010) / cases_1997 * 100) %>%
  filter(cases_1997 > 0) %>%
  arrange(reduction)

sharpest_reduction_country <- case_reduction[1, ]

sharpest_reduction_country

