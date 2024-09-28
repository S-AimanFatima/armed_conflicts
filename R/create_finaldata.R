#CHL8010 In-Class Activity Week 3
#Name: Syeda Aiman Fatima
#Date: September 23, 2024

covs <- read.csv(here("original", "covariates.csv"), header = TRUE)

covs <- covs %>% rename(Year = year)

source(here("R", "prepare_mortality.R"))
source(here("R", "prepare_disaster.R"))
source(here("R", "prepare_conflict.R"))

#put all data frames into list
alllist <- list(confdata, mortdata, disastersdata)

#merge all data frames in list
finaldata0 <- alllist %>% reduce(full_join, by = c('ISO', 'Year'))

finaldata <- covs %>%
  left_join(finaldata0, by = c('ISO', 'Year'))

# need to fill in NAs with 0's for armconf1, drought, earthquake
finaldata <- finaldata %>%
  mutate(armconf1 = replace_na(armconf1, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         totdeath = replace_na(totdeath, 0))

write.csv(finaldata, file = here("data", "finaldata.csv"), row.names = FALSE)
