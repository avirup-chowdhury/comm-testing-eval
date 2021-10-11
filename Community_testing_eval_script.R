# Read in library
setwd("D:/Community testing evaluation")

library(tidyverse)
library(lubridate)

# Introductory information: sites ----
site_codes <- read_csv("Community Test Site List.csv") %>%
  select(site_name = `Site Name`, 
         district = `District`,
         subtype = `Site Sub Type`)

site_capacity <- read_csv("Site Capacity.csv") %>%
  
  # Clean up date and select useful cols
  mutate(opening_date = dmy(`Date site opened`)) %>%
  dplyr::select(site_name = `Site name`, 
                ID = `Site ID`, 
                opening_date, 
                capacity = `Max Daily Capacity`) %>%
  
  # Append site subtype information
  left_join(., site_codes, by = "site_name")

rm(site_codes)

# Counts by subtype, location, capacity
site_capacity %>%
  count(subtype)

site_capacity %>%
  count(district)

site_capacity %>%
  count(capacity)

# Introductory information: testing ----
positives <- read_csv("Test Data - Row Level Pos for Community Sites.csv") %>%
  
  # Rename cols
  select(site_code = `Site Code`,
         test_ID = ID,
         patient_ID = `Person Key`,
         age = Age_Years,
         gender = Patient_Gender,
         ethnicity = Ethnicity,
         occupation = Patient_Occupation,
         symptomatic = Asymptomatic,
         msoa = MSOA_Name,
         test_date = Specimen_Date,
         test_reason = Reason_For_Test,
         result = `Test Result`) %>%
  
  # Reverse coding of symptomatic column to reflect renaming, test_ID class
  mutate(symptomatic = case_when(symptomatic == "N" ~ TRUE,
                                 symptomatic == "Y" ~ FALSE),
         test_ID = as.character(test_ID),
         test_date = dmy(test_date))

negatives <- read_csv("Test Data - Row Level Neg for Community Sites.csv") %>%
  
  # Rename cols
  select(site_code = `Site Code`,
         test_ID = Id,
         patient_ID = `Person Key`,
         age = Age_in_Years,
         gender = Patient_Sex,
         ethnicity = Ethnicity_Description,
         occupation = Patient_Occupation,
         symptomatic = Covid_Symptomatic,
         msoa = Middle_Super_Output_Area_Desc,
         test_date = `Specimen Date`,
         test_reason = Reason_For_Test,
         result = Test_Result) %>%
  
  # Fix date
  mutate(test_date = dmy_hm(test_date))


test_results <- bind_rows(positives, negatives) %>%
  
  # Recode values for uniformity
  mutate(test_reason = recode(test_reason, "do-not-know" = "none"),
         result = recode(result, "Void-LFT" = "Void"),
         
         # Recode ethnicity in line with census codes (from Gov.uk)
         ethnicity = recode(ethnicity,
                            "Prefer not to say" = "No ethnicity information",
                            "Another ethnic group" = "Any other ethnic group",
                            "Another Asian background" = "Any other Asian background",
                            "Another Black background" = "Any other Black, African or Caribbean background",
                            "Another ethnic background" = "Any other ethnic group",
                            "Another Mixed background" = "Any other Mixed or Multiple ethnic background",
                            "Another White background" = "Any other White background",
                            "Any other Mixed / Multiple ethnic background" = "Any other Mixed or Multiple ethnic background",
                            "Asian and White" = "White and Asian",
                            "Black African and White" = "White and Black African",
                            "Black Caribbean and White" = "White and Black Caribbean",
                            "Any other Black / African / Caribbean background" = "Any other Black, African or Caribbean background",
                            "British" = "English, Welsh, Scottish, Northern Irish or British",
                            "British~ English~ Northern Irish~ Scottish~ or Welsh" = "English, Welsh, Scottish, Northern Irish or British",
                            "Black~ African~ Black British or Caribbean" = "Black, African, Caribbean or Black British - unspecified",
                            "Mixed or multiple ethnic groups" = "Mixed or multiple ethnic groups - unspecified",
                            "Irish Traveller or Gypsy" = "Gypsy or Irish Traveller",
                            "Asian or Asian British" = "Asian or Asian British - unspecified",
                            "White" = "White - unspecified"),

         # Derive LTLA from MSOA names
         ltla = stringr::word(msoa, start = 1, end = -2))

rm(negatives, positives)

# Descriptive stats by testing
test_results %>%
  count(result)

test_results %>%
  count(test_reason)

test_results %>%
  count(symptomatic)

# Testing uptake by demographics
table(test_results$patient_ID == "No NHS Number") # Caveat de-duplication issues

summary(test_results$age)

test_results %>%
  count(gender)

ltla <- test_results %>%
  count(ltla)

ethnicity <- test_results %>%
  count(ethnicity) %>%
  mutate(percentage = round(n/nrow(test_results) * 100, 1))

write.csv(ethnicity, "ethnicity.csv")

rm(ltla, ethnicity)

# Tests per person
perperson <- test_results %>%
  count(patient_ID) %>%
  filter(!str_detect(patient_ID, "NHS"))

summary(perperson$n)
hist(perperson$n)

rm(perperson)

# Occupations/deprivation work (TBD)----
occupations <- test_results %>%
  count(occupation)

rm(occupations)

# Capacity analyses
daily_tests <- test_results %>%
  count(test_date, site_code) %>%
  
  # Add daily capacity by site
  left_join(site_capacity[, c(2,4)], by = c("site_code" = "ID")) %>%
  drop_na(capacity) %>%
  
  # Calculate percentage capacity daily per site
  mutate(percent_usage = n/capacity * 100)

usage <- daily_tests %>%
  left_join(., site_capacity[, c(1, 2)], by = c("site_code" = "ID")) %>%
  group_by(site_name) %>%
  summarise(mean = mean(percent_usage, na.rm = TRUE),
            max = max(percent_usage, na.rm = TRUE))

write.csv(usage, "site_usage.csv")

rm(usage)

utla_usage <- daily_tests %>%
  count(test_date) %>%
  mutate(percent = n/sum(site_capacity$capacity) * 100)

ggplot(utla_usage, aes(x = test_date, y = percent)) + 
  geom_line()

rm(daily_tests, utla_usage)

# Calculate incidence information----
utla_incidence <- read_csv("UTLA_incidence.csv") %>%
  
  # Select only Suffolk
  dplyr::filter(areaName == "Suffolk") %>%
  dplyr::select(-areaCode, -areaType) %>%
  mutate(date = lubridate::dmy(date)) %>%
  
  # Trim data for end date 4th June 2021 plus one week
  filter(date <= lubridate::dmy("11/06/2021"))

  # Plot incidence of new cases
ggplot(utla_incidence, aes(x = date, y = newCasesBySpecimenDateRollingRate)) +
  geom_point() +
  geom_vline(xintercept = site_capacity$opening_date) + # Site opening dates
  geom_vline(xintercept = lubridate::dmy("04/06/2021")) # Site closing date
  # Need one to indicate national lockdowns

# Service user views ----
users <- read_csv("Site_user_survey_responses.csv", guess_max = 1800)

responses_by_site <- users %>%
  count(`Test Site`)

rm(responses_by_site)

users %>% count()
