# Read in library
setwd("D:/Community testing evaluation")

library(tidyverse)

# Calcuating incidence ----
# Read in data
utla_incidence <- read_csv("UTLA_incidence.csv") %>%
  dplyr::filter(areaName == "Suffolk") %>%
  dplyr::select(-areaCode, -areaType) %>%
  mutate(date = lubridate::dmy(date))

# Plot incidence of new cases
ggplot(utla_incidence, aes(x = date, y = newCasesBySpecimenDateRollingRate)) +
  geom_point()

# Service user views ----
users <- read_csv("Site_user_survey_responses.csv", guess_max = 1800)

responses_by_site <- users %>%
  count(`Test Site`)

rm(responses_by_site)

users %>% count()
