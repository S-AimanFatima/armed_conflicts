##Derive armed conflict data

raw_conflict <- read.csv(here("Original", "conflictdata.csv"), 
                         header = TRUE)

armed_conflict <- raw_conflict %>%
  # Create a binary indicator: 1 if there were any conflicts in that year, 0 otherwise
  group_by(ISO, year, conflict_id) %>%
  summarize(armed_conflict = as.integer(any(best > 0, na.rm = TRUE)), .groups = 'drop') %>%
  # Adjust the year to account for the lag
  mutate(year = year + 1)
