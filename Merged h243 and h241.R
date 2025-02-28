source ("Loading and cleaning h243.R")

library(tidyverse)
library(readr)
library(dplyr)

#load h241 data 
pos_start_241 <- c(
  1, 8, 11, 21, 23, 36, 38, 39, 42, 44, 
  46, 48, 50, 52, 54, 56, 58, 60, 61, 64, 
  72, 78, 84, 90, 96, 97, 98, 99, 100, 101, 
  102, 115, 119)


pos_end_241 <- c(
  7, 10, 20, 22, 35, 37, 38, 41, 43, 45, 
  47, 49, 51, 53, 55, 57, 59, 60, 63, 71, 
  77, 83, 89, 95, 96, 97, 98, 99, 100, 101, 
  114, 118, 119)


var_names_241 <- c("DUID", "PID", "DUPERSID", "CONDN", "CONDIDX", "PANEL", "CONDRN", "AGEDIAG", "CRND1", "CRND2", "CRND3", "CRND4", "CRND5", "CRND6", "CRND7", "CRND8", "CRND9", "INJURY", "ACCDNWRK", "ICD10CDX", "CCSR1X", "CCSR2X", "CCSR3X", "CCSR4X", "HHCOND", "IPCOND", "OPCOND", "OBCOND", "ERCOND", "RXCOND", "PERWT22F", "VARSTR", "VARPSU")


var_types_241 <- c(
  "c", "n", "c", "n", "c", "n", "n", "n", "n", "n", 
  "n", "n", "n", "n", "n", "n", "n", "n", "n", "c", 
  "c", "c", "c", "c", "n", "n", "n", "n", "n", "n", 
  "n", "n", "n")

var_types_241 <- setNames(var_types_241, var_names_241)

h241 <- read_fwf("h241.dat", col_positions = fwf_positions(start = pos_start_241, end = pos_end_241, col_names = var_names_241), col_types = var_types_241) 

# Load h241.dat and select relevant columns

h241_selected <- h241 %>% 
  select(DUPERSID, ICD10CDX, RXCOND) %>% 
  filter(RXCOND == 1) # Keep only rows where RXCOND (Any prescribed medicine associated with condition) is 1

# Define psychiatric illness codes to filter
psych_codes <- c("F20", "F31", "F32", "F34", "F39", "F41", "F42", "F43", "F90", "F99")

# Filter h241 for individuals with psychiatric illness
h241_filtered <- h241_selected %>%
  filter(ICD10CDX %in% psych_codes) %>% 
  select(DUPERSID, ICD10CDX)


#Merge duplicate DUPERSID and make column that concatenates all psychiatric conditions  
h241_summary <- h241_filtered %>%
  group_by(DUPERSID) %>%
  summarise(
    psych_conditions_count = n(),
    psych_conditions = paste(unique(ICD10CDX), collapse = ", ")  # Concatenate unique conditions
  )

h241_summary

#This summary concatenates all conditions and includes all conditions, not just psychiatric 
h241_summary2 <- h241_selected %>%
  group_by(DUPERSID) %>%
  summarise(all_conditions = paste(unique(ICD10CDX), collapse = ", ")) # Concatenate unique conditions

#Add new column psych_count that counts how many psychiatric illnesses one has
h241_summary2 <- h241_summary2 %>%
  mutate(psych_count = str_count(all_conditions, paste(psych_codes, collapse = "|")))

#Checking current status of dataframes
h241_summary
h241_summary2
h243_categorized

duplicates <- h243_categorized %>%
  count(DUPERSID) %>%
  filter (n>1)

# Count how many DUPERSID values are common between both dataframes
matching_count <- h241_summary2 %>%
  inner_join(h243_categorized, by = "DUPERSID") %>%
  nrow()

# Print result
print(matching_count)

# Merge h241 and h243 datasets on DUPERSID
h241h243_merged <- h241_summary2 %>%
  inner_join(h243_categorized, by = "DUPERSID")
print(h241h243_merged)

write_csv(h241h243_merged, "h241h243_merged.csv")