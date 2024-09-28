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


