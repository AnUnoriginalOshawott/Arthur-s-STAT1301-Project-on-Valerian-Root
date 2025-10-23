#Install required packages
install.packages(c("readxl", "dplyr", "tidyr", "broom", "ggplot2"))

#Load Packages
library(readxl)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)

#Import excel data 
data <- read_excel("Research Project Data File.xlsx", sheet = 1, range = "G3:L64")

#reformat data to make it more readable
data_long <- data %>%
  pivot_longer(cols = matches("^[0-9]+$"), #numeric night columns 
               names_to = "Night",
               values_to = "WakingMinutes") %>%
  mutate(Night = as.numeric(Night))

