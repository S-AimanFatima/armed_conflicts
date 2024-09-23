#CHL8010 In-Class Activity Week 2
#Name: Syeda Aiman Fatima
#Date: September 16, 2024

library(here)
library(tidyverse)

#read in external data
rawdat <- read.csv(here("Original", "maternalmortality.csv"), 
                   header = TRUE)

#create subset
subset <- rawdat %>%
  select(Country.Name, X2000:X2019)

#change data format from wide to long
df_long <- subset %>%
  pivot_longer(cols = X2000:X2019,  # Select the year columns
               names_to = "Year",   # Rename the column to 'Year'
               names_prefix = "X",  # Remove the prefix 'X'
               values_to = "MatMor") %>%  # Rename the values to 'MatMor'
  mutate(Year = as.numeric(Year))  # Convert 'Year' to numeric

#save output
write.csv(df_long, here("Data", "maternalmortality_cleandata.csv"), row.names = FALSE)

#Introduce Git

install.packages("usethis")
library(usethis) 

#usethis::use_git_config(user.name = "S-AimanFatima", user.email = "syedaaiman.fatima@mail.utoronto.ca")

# to confirm, generate a git situation-report, your user name and email should appear under Git config (global)
#usethis::git_sitrep()
#usethis::use_git()
#usethis::create_github_token()
#gitcreds::gitcreds_set()
#usethis::use_github()

library(here)
library(tidyverse)

#read in external data
disaster <- read.csv(here("Original", "disaster.csv"), 
                   header = TRUE)

filtered_data <- disaster %>%
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


#CHL8010 In-Class Activity Week 3
#Name: Syeda Aiman Fatima
#Date: September 23, 2024

library(purrr)
library(dplyr)
library(tidyr)

#read in following datasets
raw_infant <- read.csv(here("Original", "infantmortality.csv"), 
                   header = TRUE)
raw_neonatal <- read.csv(here("Original", "neonatalmortality.csv"), 
                   header = TRUE)
raw_under5 <- read.csv(here("Original", "under5mortality.csv"), 
                   header = TRUE)
raw_maternal <- read.csv(here("Original", "maternalmortality.csv"), 
                         header = TRUE)

#Create function (where data is subset & change data from wide to long format)

cleandata <- function(x, mortality_type){
  subset<- x %>%
    select(Country.Name, X2000:X2019) %>%
  pivot_longer(cols = X2000:X2019,  # Select the year columns
               names_to = "Year",   # Rename the column to 'Year'
               names_prefix = "X",  # Remove the prefix 'X'
               values_to = "Mortality") %>%  # Rename the values to 'Mortality'
  mutate(Year = as.numeric(Year)) # Convert 'Year' to numeric %>%
  
  colnames(subset)[colnames(subset) == "Mortality"] <- paste0(mortality_type, "_Mortality")
  
  return(subset)
}


#apply to infant, neonatal, maternal and under5 datasets

clean_infant <- cleandata(raw_infant, "Infant")
clean_neonatal <- cleandata(raw_neonatal, "Neonatal")
clean_under5 <- cleandata(raw_under5, "Under5")
clean_maternal <- cleandata(raw_maternal, "Maternal")

#Merge all 4 datasets

merged_data <- list(clean_infant, clean_neonatal, clean_under5, clean_maternal) %>%
  reduce(full_join, by = c("Country.Name", "Year"))

#add the ISO-3 country code variable to the merged dataset

library(countrycode)
merged_data$ISO <- countrycode(merged_data$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")


