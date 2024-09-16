#CHL8010 In-Class Activity Week 2
#Name: Syeda Aiman Fatima
#Date: September 16, 2024

library(here)
library(tidyverse)

#read in external data
rawdat <- read.csv(here("Original", "disaster.csv"), 
                   header = TRUE)

filtered_data <- rawdat %>%
  filter(Year >= 2000 & Year <= 2019, 
         Disaster.Type %in% c("Earthquake", "Drought"))

subset_data <- filtered_data %>%
  select(Year, ISO, Disaster.Type)

data_with_dummies <- subset_data %>%
  mutate(
    drought = ifelse(Disaster.Type == "Drought", 1, 0),
    earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0)
  )

summarized_data <- data_with_dummies %>%
  group_by(Year, ISO) %>%
  summarize(
    drought = max(drought, na.rm = TRUE),
    earthquake = max(earthquake, na.rm = TRUE),
    .groups = 'drop'
  )
