# Nikita Belii
# HW1

install.packages("tidyverse")
library(tidyverse)

# Read the mpg dataset and store it in a new variable.
mpg
a = mpg

# Plot the relationship between engine size (displ) and fuel efficiency (hwy) with vehicles labeled by class, in an aesthetically pleasant and meaningful way.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
    labs(x = "Engine Size (displ)", y = "Highway MPG (hwy)") +
      ggtitle("Engine Size vs. Highway MPG by Vehicle Class")

# Also generate a scatterplot of hwy vs cyl in the mpg dataset.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl, color = class)) +
   labs(x = "Cylinders (cyl)", y = "Highway MPG (hwy)") +
    ggtitle("Cylinders vs. Highway MPG")

# Compute and display the total number of 4-, 5-, 6- and 8-cylinder vehicles in the dataset.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl, color = displ < 5))

# Compute and display the total number of 4-, 5-, 6- and 8-cylinder vehicles in the dataset.
table(mpg$cyl)

#Compute and display the details (manufacturer, model, etc.) of the most fuel-efficient vehicles in the dataset
most = which.max(mpg$cty)
mpg[most,]

#Compute and display the details (manufacturer, model, etc.) of the least fuel-efficient vehicles in the dataset.
least = which.min(mpg$cty)
mpg[least,]


