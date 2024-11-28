# Creating new tutorial about NMDS
# Data Science in EES 2024
# Auther: Siya-Qin
# 27th November 2024

# Install necessary packages (if not already installed)
install.packages("vegan")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")

# Load the libraries
library(vegan)
library(ggplot2)
library(tidyr)
library(dplyr)

peat_bog_data <- read_csv("peat bog data.csv")


# Check the data structure
View(peat_bog_data)
head(peat_bog_data)
str(peat_bog_data)

