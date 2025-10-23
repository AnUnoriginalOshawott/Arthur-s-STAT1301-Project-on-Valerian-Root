install.package(c("readxl", "dplyr", "tidir", "broom", "ggplot2"))
library(readxl)
library(dplyr)
library(tidir)
library(broom)
library(ggplot2)
data <- read_excel("Research Project Data File.xlsx", sheet = 1, range = "G3:L64")
data_long <- data %>%
  pivot_longer(cols = matches("^[0-9]+$"), #numeric night columns 
               names_to = "Night",
               vaules_to = "WakingMinutes") %>%
  mutate(Night = as.numeric(Night))

