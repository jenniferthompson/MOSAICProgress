################################################################################
## Data management to create MOSAIC study progress dashboard
################################################################################

library(RCurl)
library(tidyverse)

## -- Import each data set from REDCap (exclusions, in-hospital, follow-up) ----
## All tokens are stored in .Renviron

## We'll be doing the same thing for each, so write some functions
## 1. Function to create postForm() object given a database token
get_pF <- function(rctoken){
  postForm(
    "https://redcap.vanderbilt.edu/api/", ## URL for REDCap instance
    token = Sys.getenv(rctoken),                ## token for specific database
    content = "record",                         ## export records
    format = "csv",                             ## export as CSV
    rawOrLabel = "label",                       ## exp. factor labels vs numbers
    exportCheckboxLabel = TRUE,                 ## exp. checkbox labels vs U/C
    exportDataAccessGroups = FALSE              ## don't need data access grps
  )
}

get_csv <- function(pF){
  read.csv(file = textConnection(pF), na.strings = "", stringsAsFactors = FALSE)
}

import_df <- function(rctoken){
  tmp_pF <- get_pF(rctoken)
  tmp_csv <- get_csv(tmp_pF)

  ## REDCap loves to use so many underscores; one per instance seems like plenty
  names(tmp_csv) <- gsub("_+", "_", names(tmp_csv))

  tmp_csv
}

inhosp_df <- import_df("MOSAIC_IH_TOKEN")
exc_df <- import_df("MOSAIC_EXC_TOKEN")

## Remove test patients from each database
inhosp_df <- inhosp_df[grep("test", tolower(inhosp_df$id), invert = TRUE),]
exc_df <- exc_df[grep("test", tolower(exc_df$exc_id), invert = TRUE),]

################################################################################
## Screening and Exclusions
################################################################################

## -- Barchart for screening and enrollment by month ---------------------------
## We want to plot the number of patients screened, approached, and enrolled by
## month. Need a list of all unique IDs (exclusions + enrolled).

## Screened: Everyone recorded
## Approached: Enrolled + refusals
## Refused: exclusion #14 checked
##      (Inability to obtain informed consent: Patient and/or surrogate refusal)
## Enrolled: Included in in-hospital database

exc_combine <- exc_df %>%
  separate(exc_date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(myear = paste(month, year, sep = "-"),
         myear_char = paste(month.abb[as.numeric(month)], year),
         Screened = TRUE,
         Approached = !is.na(exc_rsn_14),
         Refused = !is.na(exc_rsn_14),
         Enrolled = FALSE) %>%
  rename(id = exc_id) %>%
  dplyr::select(id, year, month, Screened, Approached, Refused, Enrolled)

inhosp_combine <- inhosp_df %>%
  filter(redcap_event_name == 'Enrollment /Trial Day 1') %>%
  separate(enroll_dttm, into = c("year", "month", "day", "time"), sep = "-| ") %>%
  mutate(Screened = TRUE,
         Approached = TRUE,
         Refused = FALSE,
         Enrolled = TRUE) %>%
  dplyr::select(id, year, month, Screened, Approached, Refused, Enrolled)

screening_combine <- bind_rows(exc_combine, inhosp_combine) %>%
  mutate(mabb = month.abb[as.numeric(month)],
         myear = paste(month, year, sep = "-"),
         myear_char = ifelse(mabb == "Mar", paste(mabb, year), mabb))

screening_summary <- screening_combine %>%
  group_by(myear, myear_char) %>%
  summarise_each(funs(sum), Screened, Approached, Refused, Enrolled)

## How many patients have been enrolled so far? What is our enrollment goal?
n_screened <- sum(screening_combine$Screened)
pct_approached <- mean(screening_combine$Approached)
pct_excluded <- 1 - pct_approached
pct_refused <- mean(subset(screening_combine, Approached)$Refused)
n_enrolled <- sum(screening_combine$Enrolled)
pct_enrolled <- mean(subset(screening_combine, Approached)$Enrolled)
n_goal <- 312

## -- Line chart for exclusion percentages over time ---------------------------
## Create long-format data set of all exclusions, one row each
exc_df_long <- exc_df %>%
  gather(key = exc_reason, value = was_excluded, exc_rsn_1:exc_rsn_99) %>%
  separate(exc_date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(was_excluded = !is.na(was_excluded),
         mabb = month.abb[as.numeric(month)],
         myear = paste(month, year, sep = "-"),
         myear_char = ifelse(mabb == "Mar", paste(mabb, year), mabb),
         Reason = ifelse(exc_reason == "exc_rsn_1", "Rapidly resolving organ failure",
                  ifelse(exc_reason == "exc_rsn_2", ">5 hospital days in last 30",
                  ifelse(exc_reason == "exc_rsn_3", "Inability to live independently",
                  ifelse(exc_reason == "exc_rsn_4", "Severe neurologic injury",
                  ifelse(exc_reason == "exc_rsn_5", "BMI > 50",
                  ifelse(exc_reason == "exc_rsn_6", "Substance abuse, etc",
                  ifelse(exc_reason == "exc_rsn_7", "Blind, deaf, English",
                  ifelse(exc_reason == "exc_rsn_8", "Death within 24h/hospice",
                  ifelse(exc_reason == "exc_rsn_9", "Prisoner",
                  ifelse(exc_reason == "exc_rsn_10", "Lives >150 miles from VUMC",
                  ifelse(exc_reason == "exc_rsn_11", "Homeless",
                  ifelse(exc_reason == "exc_rsn_12", "Study with no co-enrollment",
                  ifelse(exc_reason == "exc_rsn_13", "Attending refusal",
                  ifelse(exc_reason == "exc_rsn_14", "Patient/surrogate refusal",
                  ifelse(exc_reason == "exc_rsn_15", "No surrogate within 72h",
                  ifelse(exc_reason == "exc_rsn_99", "Other",
                         NA))))))))))))))))) %>%
  filter(was_excluded)

## Data set for exclusions over time: Proportion of each exclusion each month
## How many exclusions total per month?
exc_per_month <- exc_df_long %>%
  dplyr::select(exc_id, myear, was_excluded) %>%
  unique() %>%
  group_by(myear) %>%
  summarise(n_all_exclusions = sum(was_excluded))

exc_over_time <- exc_df_long %>%
  group_by(myear, myear_char, Reason) %>%
  summarise(n_this_exclusion = sum(was_excluded)) %>%
  left_join(exc_per_month, by = "myear") %>%
  mutate(Percent = round((n_this_exclusion / n_all_exclusions)*100))

## -- Treemap for cumulative exclusions ----------------------------------------
exc_cumul <- exc_df_long %>%
  group_by(Reason) %>%
  summarise(n_reason = n()) %>%
  mutate(n_patients_exc = nrow(exc_df),
         reason_type = case_when(
           .$Reason %in% c(
             "Inability to live independently",
             "Homeless",
             "BMI > 50",
             "Blind, deaf, English",
             "Substance abuse, etc"
            ) ~ "Patient characteristics",
           .$Reason %in% c(
             ">5 hospital days in last 30",
             "Death within 24h/hospice",
             "Rapidly resolving organ failure",
             "Severe neurologic injury"
           ) ~ "Medical exclusions",
           .$Reason %in% c("Lives >150 miles from VUMC") ~ "Geography",
           .$Reason %in% c(
             "Attending refusal",
             "No surrogate within 72h",
             "Patient/surrogate refusal"
           ) ~ "Informed consent",
           TRUE ~ "Other"
         ))
