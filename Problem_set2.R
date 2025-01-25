# Homework 2 by Nikita Belii

# PROBLEM 1: Analyzing NBA Salaries

# QUESTION 1: Who is the highest paid NBA player and what is his team, draft number, minutes played (MP), and true shooting percentage (TS%)?
salaries = read.csv("/Users/Nikita/Downloads/2017-18_NBA_salary.csv") # Load the file into 'salaries' variable
player = which.max(salaries$"Salary") # Find the player with the highest salary
salaries[player,] # Show the player's characteristics

# QUESTION 2: What is the minimum, maximum, and average salaries of NBA players?
min_salary = min(salaries$"Salary")
max_salary = max(salaries$"Salary")
ave_salary = mean(salaries$"Salary")
min_salary = paste("The minimum salary is", min_salary) # Create a formatted string
max_salary = paste("The maximum salary is", max_salary) # Create a formatted string
ave_salary = paste("The average salary is", floor(ave_salary)) # Create a formatted string
print(min_salary) # Print the minimum salary
print(max_salary) # Print the maximum salary
print(ave_salary) # Print the average salary

# QUESTION 3: What is the minimum, maximum, and average ages of all the NBA players from USA?
min_age = min(salaries$"Age")
max_age = max(salaries$"Age")
ave_age = mean(salaries$"Age")
min_age = paste("The minimum age is", min_age) # Create a formatted string
max_age = paste("The maximum age is", max_age) # Create a formatted string
ave_age = paste("The average age is", floor(ave_age)) # Create a formatted string
print(min_age) # Print the minimum age
print(max_age) # Print the maximum age
print(ave_age) # Print the average age

# QUESTION 4: What is the minimum, maximum, and average ages of all the NBA non-USA players?
non_usa_min_age = min(subset(salaries, NBA_Country != "USA")$"Age")
non_usa_max_age = max(subset(salaries, NBA_Country != "USA")$"Age")
non_usa_ave_age = mean(subset(salaries, NBA_Country != "USA")$"Age")
non_usa_min_age = paste("The minimum age of non-USA players is", non_usa_min_age) # Create a formatted string
non_usa_max_age = paste("The maximum age of non-USA players is", non_usa_max_age) # Create a formatted string
non_usa_ave_age = paste("The average age of non-USA players is", floor(non_usa_ave_age)) # Create a formatted string
print(non_usa_min_age) # Print the minimum age of non-USA players
print(non_usa_max_age) # Print the maximum age of non-USA players
print(non_usa_ave_age) # Print the average age of non-USA players

# PROBLEM 2: Analyzing NBA Teams

# QUESTION 1: What is the average salary and average true shooting percentage (TS%) of the first NBA team?
miami = filter(salaries, NBA_Country == "USA", Tm == "MIA")
miami
mia_ave_salary = mean(miami$"Salary")
mia_ave_shoot = mean(miami$"TS.")
mia_ave_salary = paste("Average salary of the Miami Heat players is ", floor(mia_ave_salary)) # Create a formatted string
mia_ave_shoot = paste("Average True Shooting % of the Miami Heat players is ", mia_ave_shoot) # Create a formatted string
print(mia_ave_salary) # Print the average salary of the Miami Heat players
print(mia_ave_shoot) # Print the average TS% of the Miami Heat players

# QUESTION 2: What is the average salary and average true shooting percentage (TS%) of the second NBA team?
orlando = filter(salaries, NBA_Country == "USA", Tm == "ORL")
orlando
orl_ave_salary = mean(orlando$"Salary")
orl_ave_shoot = mean(orlando$"TS.")
orl_ave_salary = paste("Average salary of the Orlando Magic players is ", floor(orl_ave_salary)) # Create a formatted string
orl_ave_shoot = paste("Average True Shooting % of the Orlando Magic players is ", orl_ave_shoot) # Create a formatted string
print(orl_ave_salary) # Print the average salary of the Orlando Magic players
print(orl_ave_shoot) # Print the average TS% of the Orlando Magic players

# QUESTION 3: Print a table that compares the average age, salaries, and true shooting percentages (TS%) of the two teams in tabular form
mia_ave_age = mean(miami$"Age")
orl_ave_age = mean(orlando$"Age")
mia_ave_salary = mean(miami$"Salary")
orl_ave_salary = mean(orlando$"Salary")
mia_ave_shoot = mean(miami$"TS.")
orl_ave_shoot = mean(orlando$"TS.")
teams_table = tibble(
  Teams = c("Miami Heat", "Orlando Magic"),
  "Average Age" = c(mia_ave_age, orl_ave_age),
  "Average Salary" = c(mia_ave_salary, orl_ave_salary),
  "TS%" = c(mia_ave_shoot, orl_ave_shoot)
)
print(teams_table)    # Print the table

# PROBLEM 3: Analyzing Flight Data

install.packages("nycflights13")    # Install and load packages
library(nycflights13)
library(tidyverse)
flights

# QUESTION 1: Number of Flights from New York to Fort Lauderdale (FLL) in 2013
flights_table = table(flights$origin, flights$dest)
flights_count = flights_table["JFK", "FLL"] + flights_table["LGA", "FLL"] + flights_table["EWR", "FLL"]
flights_count = paste("Number of flights from New York to Fort Lauderdale (FLL) in 2013:", flights_count)
print(flights_count)

# QUESTION 2: How many flights went from New York (JFK) to Fort Lauderdale (FLL) in 2013?
flights_count = flights_table["JFK", "FLL"]
flights_count = paste("Number of flights from New York (JFK) to Fort Lauderdale (FLL) in 2013:", flights_count)
print(flights_count)

# QUESTION 3: How many flights went from New York (LGA) to Fort Lauderdale (FLL) in 2013?
flights_count = flights_table["LGA", "FLL"]
flights_count = paste("Number of flights from New York (LGA) to Fort Lauderdale (FLL) in 2013:", flights_count)
print(flights_count)

# QUESTION 4: How many flights went from New York (EWR) to Fort Lauderdale (FLL) in 2013?
flights_count = flights_table["EWR", "FLL"]
flights_count = paste("Number of flights from New York (EWR) to Fort Lauderdale (FLL) in 2013:", flights_count)
print(flights_count)

# PROBLEM 4 (OPTIONAL): Analyzing NBA Countries
library(dplyr)
library(tidyr)

# QUESTION 1: Number of Different Countries Represented by NBA Players
number_of_countries = salaries %>%
  summarise(Unique_Countries = n_distinct(NBA_Country))
number_of_countries = number_of_countries$Unique_Countries
number_of_countries = paste("Number of different countries represented by NBA players: ", num_countries$Unique_Countries)
print(number_of_countries)    # Print the number of different countries

# QUESTION 2: Print a list of all the countries represented by NBA players and the number of players from each country. Generate a horizontal geom-bar plot of countries and number of NBA players.
country_summary = salaries %>%         
  group_by(NBA_Country) %>%        # Create a summary table of countries and the number of players from each country
  summarise(Number_of_Players = n())
print(country_summary)    # Print the summary table

library(ggplot2)
ggplot(country_summary, aes(y = reorder(NBA_Country, Number_of_Players), x = Number_of_Players)) +  # Generate a bar plot of countries and number of NBA players
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Number of NBA Players", y = "Country") +
  ggtitle("Number of NBA Players by Country")
#QUESTION 3 : Print a list of all the NBA teams and the average player salary of each team that year.
team_salary_summary <- salaries %>%
  group_by(Tm) %>%    # Group the data by team and calculate the average salary for each team
  summarise(Average_Salary = mean(Salary))
print(team_salary_summary)  # Print the list of NBA teams and their average player salaries

# QUESTION 4 : Print a list of all the NBA teams and the average true shooting percentage (TS%) of each team that year. team_ts_summary <- salaries %>%
  group_by(Tm) %>%    # Group the data by team and calculate the average TS% for each team
  summarise(Average_TS = mean(TS.))
print(team_ts_summary)  # Print the list of NBA teams and their average true shooting percentages
