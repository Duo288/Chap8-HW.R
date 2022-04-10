#### Wenhan Zhao

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)

linelist_raw <- import("linelist_raw.xlsx")

skimr::skim(linelist_raw)
names(linelist_raw)
##  [1] "case_id"         "generation"      "infection date"  "date onset"      "hosp date"       "date_of_outcome" "outcome"         "gender"         
##  [9] "hospital"        "lon"             "lat"             "infector"        "source"          "age"             "age_unit"        "row_num"        
## [17] "wt_kg"           "ht_cm"           "ct_blood"        "fever"           "chills"          "cough"           "aches"           "vomit"          
## [25] "temp"            "time_admission"  "merged_header"   "...28"

# pipe the raw dataset through the function clean_names(), assign result as "linelist"  
linelist <- linelist_raw %>% 
  janitor::clean_names()

# see the new column names
names(linelist)
# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome)
##  [1] "case_id"              "generation"           "date_infection"       "date_onset"           "date_hospitalisation" "date_outcome"        
##  [7] "outcome"              "gender"               "hospital"             "lon"                  "lat"                  "infector"            
## [13] "source"               "age"                  "age_unit"             "row_num"              "wt_kg"                "ht_cm"               
## [19] "ct_blood"             "fever"                "chills"               "cough"                "aches"                "vomit"               
## [25] "temp"                 "time_admission"       "merged_header"        "x28"

rename(newNameForFirstColumn  = 1,
       newNameForSecondColumn = 2)
linelist_raw %>% 
  select(# NEW name             # OLD name
    date_infection       = `infection date`,    # rename and KEEP ONLY these columns
    date_hospitalisation = `hosp date`)
linelist_raw <- openxlsx::readWorkbook("linelist_raw.xlsx", fillMergedCells = TRUE)
names(linelist)
## [1] "case_id"              "date_onset"           "date_hospitalisation" "fever"
# move date_onset and date_hospitalisation to beginning
linelist %>% 
  select(date_onset, date_hospitalisation, everything()) %>% 
  names()
# select columns that are class Numeric
linelist %>% 
  select(where(is.numeric)) %>% 
  names()
## [1] "generation" "lon"        "lat"        "row_num"    "wt_kg"      "ht_cm"      "ct_blood"   "temp"

# searched for multiple character matches
linelist %>% 
  select(matches("onset|hosp|fev")) %>%   # note the OR symbol "|"
  names()

# HIDDEN FROM READER
# removes new demo columns created above
# linelist <- linelist %>% 
#   select(-contains("new_var"))
## [1] "character"
linelist <- linelist %>% 
  mutate(across(.cols = c(temp, ht_cm, wt_kg), .fns = as.character))

village_detection <- c("a", "b", NA,  NA)
village_residence <- c("a", "c", "a", "d")

village <- coalesce(village_detection, village_residence)
village    # print
##    date_onset n cumulative_cases
## 1  2012-04-15 1                1
## 2  2012-05-05 1                2
## 3  2012-05-08 1                3
## 4  2012-05-31 1                4
## 5  2012-06-02 1                5
## 6  2012-06-07 1                6
## 7  2012-06-14 1                7
## 8  2012-06-21 1                8
## 9  2012-06-24 1                9
## 10 2012-06-25 1               10

linelist$bmi = linelist$wt_kg / (linelist$ht_cm / 100) ^ 2)

# fix incorrect values                   # old value       # new value
linelist <- linelist %>% 
  mutate(date_onset = recode(date_onset, "2014-14-15" = "2014-04-15"))

## 
##                      Central Hopital                     Central Hospital                           Hospital A                           Hospital B 
##                                   11                                  457                                  290                                  289 
##                     Military Hopital                    Military Hospital                     Mitylira Hopital                    Mitylira Hospital 
##                                   32                                  798                                    1                                   79 
##                                Other                         Port Hopital                        Port Hospital St. Mark's Maternity Hospital (SMMH) 
##                                  907                                   48                                 1756                                  417 
##   St. Marks Maternity Hopital (SMMH)                                 <NA> 
##                                   11                                 1512

# Example: change gender of one specific observation to "Female" 
linelist <- linelist %>% 
  mutate(gender = replace(gender, case_id == "2195", "Female"))

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  
  # add column
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>%     
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) %>% 
  
  # add column: delay to hospitalisation
  mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  ###################################################

# clean values of hospital column
mutate(hospital = recode(hospital,
                         # OLD = NEW
                         "Mitylira Hopital"  = "Military Hospital",
                         "Mitylira Hospital" = "Military Hospital",
                         "Military Hopital"  = "Military Hospital",
                         "Port Hopital"      = "Port Hospital",
                         "Central Hopital"   = "Central Hospital",
                         "other"             = "Other",
                         "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
)) %>% 
  
  mutate(hospital = replace_na(hospital, "Missing")) %>% 
  
  # create age_years column (from age and age_unit)
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age,
    TRUE ~ NA_real_))
linelist %>%
  rowwise() %>%
  mutate(latest_date = max(c_across(contains("date")), na.rm=T)) %>% 
  ungroup() %>% 
  select(latest_date, contains("date"))  # for display
## # A tibble: 5,888 x 5
##    latest_date date_infection date_onset date_hospitalisation date_outcome
##    <date>      <date>         <date>     <date>               <date>      
##  1 2014-05-15  2014-05-08     2014-05-13 2014-05-15           NA          
##  2 2014-05-18  NA             2014-05-13 2014-05-14           2014-05-18  
##  3 2014-05-30  NA             2014-05-16 2014-05-18           2014-05-30  
##  4 2014-05-20  2014-05-04     2014-05-18 2014-05-20           NA          
##  5 2014-05-29  2014-05-18     2014-05-21 2014-05-22           2014-05-29  
##  6 2014-05-24  2014-05-03     2014-05-22 2014-05-23           2014-05-24  
##  7 2014-06-01  2014-05-22     2014-05-27 2014-05-29           2014-06-01  
##  8 2014-06-07  2014-05-28     2014-06-02 2014-06-03           2014-06-07  
##  9 2014-06-18  NA             2014-06-05 2014-06-06           2014-06-18  
## 10 2014-06-09  NA             2014-06-05 2014-06-07           2014-06-09  
## # ... with 5,878 more rows
linelist %>% 
  arrange(hospital, desc(date_onset))
