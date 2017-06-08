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
n_goal <- 500

## -- Line chart for exclusion percentages over time ---------------------------
exc_df <- exc_df %>%
  separate(exc_date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(mabb = month.abb[as.numeric(month)],
         myear = paste(month, year, sep = "-"),
         myear_char = ifelse(mabb == "Mar", paste(mabb, year), mabb))

calc_pct_notmissing <- function(x){ mean(!is.na(x)) * 100 }

exc_over_time <- exc_df %>%
  group_by(myear, myear_char) %>%
  summarise_each(funs(calc_pct_notmissing), exc_rsn_1:exc_rsn_99) %>%
  gather(key = Reason, value = Percent, exc_rsn_1:exc_rsn_99)
