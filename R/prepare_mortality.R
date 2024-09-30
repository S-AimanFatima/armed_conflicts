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

mortdata <- list(clean_infant, clean_neonatal, clean_under5, clean_maternal) %>%
  reduce(full_join, by = c("Country.Name", "Year"))

#add the ISO-3 country code variable to the merged dataset

library(countrycode)
mortdata$ISO <- countrycode(mortdata$Country.Name,
                               origin = "country.name",
                               destination = "iso3c")

#remove country.name variable
mortdata <- mortdata %>%
  dplyr::select(-Country.Name)