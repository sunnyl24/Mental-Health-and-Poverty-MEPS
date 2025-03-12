#Loading and cleaning hc239A: Prescribed Medications File 

source(file.path("data-wrangling","Loading and cleaning h243.R"))
source(file.path("data-wrangling","Merged h243 and h241.R"))

library(readr) 
pos_start_239a <- c(
  1, 8, 11, 21, 34, 53, 69, 71, 72, 75, 
  79, 129, 189, 200, 208, 258, 308, 358, 408, 411, 
  414, 416, 418, 420, 422, 424, 426, 428, 430, 432, 
  434, 435, 436, 437, 438, 439, 442, 445, 448, 451, 
  454, 457, 459, 461, 464, 467, 470, 473, 476, 479, 
  482, 485, 492, 500, 509, 517, 524, 532, 539, 547, 
  554, 561, 570, 583, 587)


pos_end_239a <- c(
  7, 10, 20, 33, 52, 68, 70, 71, 74, 78, 
  128, 188, 199, 207, 257, 307, 357, 407, 410, 413, 
  415, 417, 419, 421, 423, 425, 427, 429, 431, 433, 
  434, 435, 436, 437, 438, 441, 444, 447, 450, 453, 
  456, 458, 460, 463, 466, 469, 472, 475, 478, 481, 
  484, 491, 499, 508, 516, 523, 531, 538, 546, 553, 
  560, 569, 582, 586, 587)


var_names_239a <- c(
  "DUID", "PID", "DUPERSID", "DRUGIDX", "RXRECIDX", "LINKIDX", "PANEL", "PURCHRD", "RXBEGMM", "RXBEGYRX", 
  "RXNAME", "RXDRGNAM", "RXNDC", "RXQUANTY", "RXFORM", "RXFRMUNT", "RXSTRENG", "RXSTRUNT", "RXDAYSUP", "PHARTP1", 
  "PHARTP2", "PHARTP3", "PHARTP4", "PHARTP5", "PHARTP6", "PHARTP7", "PHARTP8", "PHARTP9", "PHARTP10", "PHARTP11", 
  "RXFLG", "IMPFLAG", "PCIMPFLG", "DIABEQUIP", "INPCFLG", "TC1", "TC1S1", "TC1S1_1", "TC1S1_2", "TC1S2", 
  "TC1S2_1", "TC1S3", "TC1S3_1", "TC2", "TC2S1", "TC2S1_1", "TC2S1_2", "TC2S2", "TC3", "TC3S1", 
  "TC3S1_1", "RXSF22X", "RXMR22X", "RXMD22X", "RXPV22X", "RXVA22X", "RXTR22X", "RXOF22X", "RXSL22X", "RXWC22X", 
  "RXOT22X", "RXXP22X", "PERWT22F", "VARSTR", "VARPSU")


var_types_239a <- c(
  "n", "n", "c", "c", "c", "c", "n", "n", "n", "n", 
  "c", "c", "c", "n", "c", "c", "c", "c", "n", "n", 
  "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
  "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
  "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
  "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
  "n", "n", "n", "n", "n")

var_types_239a <- setNames(var_types_239a, var_names_239a)

#Read the file
h239a <- read_fwf("raw-data/h239a.dat", col_positions = fwf_positions(start = pos_start_239a, end  = pos_end_239a, col_names = var_names_239a),col_types = var_types_239a) 

#Save the file as .Rdata so it's easier to load next time
save(h239a, file ="h239a.Rdata")

library(dplyr)

# Select only the variables of interest
variables_h239a <- c("DUPERSID", "RXXP22X", "RXSF22X", "TC1", "TC1S1", "TC1S1_1", "TC1S1_2", "TC1S2", "TC1S2_1", "TC1S3", "TC1S3_1", "TC2", "TC2S1", "TC2S1_1", "TC2S1_2", "TC2S2", "TC3", "TC3S1", "TC3S1_1")

# Create a new dataframe with only the selected columns
h239a_selected <- h239a %>% select(all_of(variables_h239a))
write_csv(h239a_selected, "h239a_selected.csv")

#Create new column Psych_Drug that returns TRUE if individual is prescribed with psychiatric drug and FALSE if not
psych_columns <- c("TC1", "TC1S1", "TC1S1_1", "TC1S1_2", "TC1S2", "TC1S2_1", "TC1S3", "TC1S3_1", "TC2", "TC2S1", "TC2S1_1", "TC2S1_2", "TC2S2", "TC3", "TC3S1", "TC3S1_1")

psych_codes <- c(67, 68, 69, 70, 71, 76, 77, 79, 208, 209, 210, 249, 250, 251, 280, 306, 307, 308, 341, 504, 516)

h239a_coladded <- h239a_selected %>%
  mutate(psych_drug = rowSums(across(all_of(psych_columns), ~ . %in% psych_codes, .names = "check_{.col}")) > 0)

head(h239a_coladded)
colnames(h239a_coladded)

# Create a person-level dataset by summarizing prescriptions
h239a_person_level <- h239a_coladded %>%
  group_by(DUPERSID) %>%
  summarise(
    # Concatenate all unique psychiatric drugs per person
    all_drugs = paste(unique(unlist(across(all_of(psych_columns)))), collapse = ", "),
    # Count unique psychiatric drug codes
    psych_drug_count = length(unique(unlist(across(all_of(psych_columns)))[unlist(across(all_of(psych_columns))) %in% psych_codes])))

h239a_person_level
    
all_merged <- h239a_person_level %>%
  inner_join(h241h243_merged, by = "DUPERSID") #Join by unique DUPERSID

colnames(all_merged)

all_merged$psych_drug_exposure <- ifelse(all_merged$psych_drug_count > 0, "Yes", "No")
all_merged$psych_cond_exist <- ifelse(all_merged$psych_conditions_count > 0, "Yes", "No")

all_merged

clean_data <- all_merged %>% 
  select(DUPERSID, psych_drug_count, psych_conditions_count, AGE31X, RACETHX, SEX, INSURC22, POVCAT22) %>%
  mutate(SEX = factor(str_to_title(as.character(SEX)))) #Change all values in SEX to capitalize first letter 

print(all_merged)
print(clean_data)

#Save the all_merged file 
write_csv(all_merged, "cleaned-data/all_merged.csv")  

clean_data <- clean_data %>%
  mutate(exposure_psych_drugs = factor(ifelse(psych_drug_count > 0, "Yes", "No"))) #Make new column that returns Yes, No for exposure to psychiatric drugs 

print(clean_data)

