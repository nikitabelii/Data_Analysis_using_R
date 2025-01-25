#Homework 4 by Nikita Belii

library(tidyverse)
library(dplyr)
library(ggplot2)

#1. Load the dataset as a new dataframe called caffeine

caffeine <- read.table("https://raw.githubusercontent.com/ndphillips/ThePiratesGuideToR/master/data/caffeinestudy.txt")

#2. 
# a. the mean age for each gender.

mean_age_gender <- caffeine %>%
  group_by(gender) %>%
  summarise(mean_age = mean(age))
print(mean_age_gender)

# b. the mean age for each drink.

mean_age_drink <- caffeine %>%
  group_by(drink) %>%
  summarise(mean_age = mean(age))
print(mean_age_drink)

# c. the mean age for each combined level of both gender and drink

mean_age_drink_gender <- caffeine %>%
  group_by(drink, gender) %>%
  summarise(mean_age = mean(age))
print(mean_age_drink_gender)

# d. the median score for each age

median_score_age <- caffeine %>%
  group_by(age) %>%
  summarise(median_score = median(score))
print(median_score_age)

# 3. For men only, write R code to calculate and print the maximum score for each age.

max_score_men <- caffeine %>%
  filter(gender == "male") %>%
  group_by(age) %>%
  summarise(max_score = max(score))
print(max_score_men)

# 4. Create a dataframe showing, for each level of drink, the mean, median, maximum, and standard deviation of scores.

stats_drink <- caffeine %>%
  group_by(drink) %>%
  summarise(mean_score = mean(score),
            median_score = median(score),
            max_score = max(score),
            sd_score = sd(score))
print(stats_drink)

# 5. Write R code to plot the contents of the dataframe created in the previous step in a visually pleasant and informative way

stats_drink_long <- stats_drink %>%
  # Transform 'stats_drink' dataframe to a longer format
  pivot_longer(cols = -drink, names_to = "statistic", values_to = "value")

ggplot(stats_drink_long, aes(x = drink, y = value, fill = statistic)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Score Statistics by Drink", x = "Drink", y = "Value")

# 6. Only for females above the age of 20, create a table showing, for each combined level of drink and cups, the mean, median, maximum, and standard deviation of scores. Also include a column showing how many people were in each group.

female_stats <- caffeine %>%
  filter(gender == "female", age > 20) %>%
  group_by(drink, cups) %>%
  summarise(mean_score = mean(score),
            median_score = median(score),
            max_score = max(score),
            sd_score = sd(score),
            count = n())
print(female_stats)

# 7. Write R code to plot the contents of the table created in the previous step in a visually pleasant and informative way.

female_stats_long <- female_stats %>%
  # Transform 'female_stats' dataframe to a longer format
  pivot_longer(cols = -c(drink, cups), names_to = "statistic", values_to = "value")

ggplot(female_stats_long, aes(x = interaction(drink, cups), y = value, fill = statistic)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Score Statistics for Females > 20 by Drink and Cups", x = "Drink & Cups", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjusting the text angle for x-axis labels

# 8. 
# QUESTION 1: What is the correlation between test scores and type of drink?

# Check the structure of the dataframe
str(caffeine)

# Calculating correlation
correlation_drink <- cor(caffeine$score, caffeine$drink_numeric)
print(correlation_drink)

ggplot(caffeine, aes(x = drink, y = score)) +
  geom_boxplot() +
  labs(title = "Score by Type of Drink", x = "Type of Drink", y = "Score")

# The negative correlation coefficient of -0.1341913 suggests a slight inverse relationship between test scores and the type of drink
# According to the plot, no definitive strong conclusion can be made. Score of coffee appears to be more spread out and only slightly higher. 

# QUESTION 2: What is the correlation between test scores and the amount of drink?

# Calculating correlation
correlation_cups <- cor(caffeine$score, caffeine$cups)
print(correlation_cups)

ggplot(caffeine, aes(x = cups, y = score)) +
  geom_point(aes(color = drink), alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(title = "Score by Amount of Drink", x = "Cups of Drink", y = "Score")

# The correlation coefficient of 0.8693923 is high, which indicates a strong positive relationship between the amount of drink consumed and the test scores.

# QUESTION 3: What is the correlation between test scores and age?

# Calculating correlation
correlation_age <- cor(caffeine$score, caffeine$age)
print(correlation_age)

ggplot(caffeine, aes(x = age, y = score)) +
  geom_point(aes(color = drink), alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(title = "Score by Age", x = "Age", y = "Score")

# The negative correlation coefficient of -0.07245816 indicates a weak inverse relationship between age and test scores

# QUESTION 4: What is the correlation between test scores and gender?

# Convert gender to numeric
caffeine$gender_numeric <- ifelse(caffeine$gender == "male", 1, ifelse(caffeine$gender == "female", 0, NA))

# Calculating correlation
correlation_gender <- cor(caffeine$score, caffeine$gender_numeric)
print(correlation_gender)

ggplot(caffeine, aes(x = gender, y = score)) +
  geom_boxplot() +
  labs(title = "Score by Gender", x = "Gender", y = "Score")

# The correlation coefficient of 0.07995989 indicates a weak positive linear relationship between gender and test scores

# QUESTION 5: What can we conclude from this study (and how does the data support such conclusion)?

# What data says:

# Type of Drink vs. Scores (-0.1341913): The type of drink doesn't seem to make much of a difference in how people score on the tests. The number shows a very small link, suggesting that what you drink isn't that important for your test score.

# Amount of Drink vs. Scores (0.8693923): It looks like the more someone drinks, the higher their test score tends to be. But we can't say for sure that drinking more causes better scores; it could be that people who score well also just like to drink more.

# Age vs. Scores (-0.07245816): Age doesn't seem to affect test scores much at all. The number is really small, so whether you're older or younger doesn't say much about how well you might do on a test.

# Gender vs. Scores (0.07995989): Whether someone is male or female doesn't give us a good guess on how they'll score. There's an insignificant number that says males might score a little higher, but it's so small that it doesn't mean much.

# Conclusion: 

# The main indicator we noticed is that people who drink more caffeine tend to have higher scores! Other factors like age and gender do not have a significant impact on the score. In terms of a type of drink, score of coffee appears to be more spread out and only slightly higher than green tea, which does not show a real contrast. 



