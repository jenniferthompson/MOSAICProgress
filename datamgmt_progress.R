################################################################################
## Data management to create MOSAIC study progress dashboard
################################################################################

library(RCurl)
library(tidyverse)
library(lubridate)

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

## Data management prep: Create POSIXct versions of most relevant date/times
dtvars <- c("enroll_dttm", "death_dttm", "hospdis_dttm")

inhosp_df <- inhosp_df %>%
  mutate_at(dtvars, "ymd_hm") %>%
  mutate_at(dtvars, funs(date = "as_date")) %>%
  rename_at(dtvars, ~ gsub("tm$", "", .)) %>%
  rename_at(paste0(dtvars, "_date"), ~ gsub("_dttm", "", ., fixed = TRUE)) %>%
  mutate(studywd_date = ymd(studywd_dttm)) %>%
  select(-studywd_dttm)

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

## Get list of any patients with no exclusion date entered, then remove them
exc_id_nodate <- exc_df %>%
  filter(is.na(exc_date)) %>%
  pull(exc_id)

exc_combine <- exc_df %>%
  filter(!is.na(exc_date)) %>%
  separate(exc_date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(Screened = TRUE,
         Approached = !is.na(exc_rsn_14),
         Refused = !is.na(exc_rsn_14),
         Enrolled = FALSE) %>%
  rename(id = exc_id) %>%
  dplyr::select(id, year, month, Screened, Approached, Refused, Enrolled)

inhosp_combine <- inhosp_df %>%
  filter(redcap_event_name == 'Enrollment /Trial Day 1') %>%
  separate(enroll_dt, into = c("year", "month", "day", "time"), sep = "-| ") %>%
  mutate(Screened = TRUE,
         Approached = TRUE,
         Refused = FALSE,
         Enrolled = TRUE) %>%
  dplyr::select(id, year, month, Screened, Approached, Refused, Enrolled)

screening_combine <- bind_rows(exc_combine, inhosp_combine) %>%
  mutate(mabb = month.abb[as.numeric(month)],
         myear = paste(year, month, sep = "-"),
         myear_char = ifelse(mabb == "Mar", paste(mabb, year), mabb))

screening_summary <- screening_combine %>%
  group_by(myear, myear_char) %>%
  summarise_at(c("Screened", "Approached", "Refused", "Enrolled"), sum) %>%
  arrange(myear)

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
         myear = paste(year, month, sep = "-"),
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
                  ifelse(exc_reason == "exc_rsn_16", ">72h eligibility prior to screening",
                  ifelse(exc_reason == "exc_rsn_99", "Other",
                         NA)))))))))))))))))) %>%
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
  mutate(Percent = round((n_this_exclusion / n_all_exclusions)*100)) %>%
  ungroup() %>%
  arrange(myear)

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
             "Patient/surrogate refusal",
             ">72h eligibility prior to screening"
           ) ~ "Informed consent",
           TRUE ~ "Other exclusions"
         ))

################################################################################
## Phase I (In-Hospital)
################################################################################

## -- Currently: died/withdrew in hospital, discharged, still in hospital ------
## Get IDs for anyone with no enrollment date entered
enroll_id_nodate <- inhosp_df %>%
  filter(redcap_event_name == "Enrollment /Trial Day 1" & is.na(enroll_date)) %>%
  pull(id)

all_enrolled <- inhosp_df %>%
  ## Restrict to patients with an enrollment date entered
  filter(
    redcap_event_name == "Enrollment /Trial Day 1" & !is.na(enroll_date)
  ) %>%
  mutate(inhosp_status = factor(ifelse(!is.na(hospdis_date), 1,
                                ifelse(!is.na(death_date), 2,
                                ifelse(!is.na(studywd_date), 3, 4))),
                                levels = 1:4,
                                labels = c("Discharged alive",
                                           "Died in hospital",
                                           "Withdrew in hospital",
                                           "Still in hospital")))
  
status_count <- all_enrolled %>%
  group_by(inhosp_status) %>%
  summarise(n_status = n())

## -- Completion of pre-hospital surrogate, caregiver batteries ----------------
## Surrogate battery: General questions, PASE, basic/IADLs, life space,
##   employment questionnaire, AUDIT, IQCODE
## Caregiver battery: Zarit, memory/behavior checklist
## "Complete" = every section fully or partially completed
surrogate_compvars <- paste0(
  c("gq", "pase", "adl", "ls", "emp", "audit", "iqcode"),
  "_comp_ph"
)
caregiver_compvars <- paste0(c("zarit", "memory"), "_comp_ph")

all_enrolled <- all_enrolled %>%
  mutate_at(
    vars(one_of(c(surrogate_compvars, caregiver_compvars))),
    funs(!is.na(.) & . %in% c("Yes, completely", "Yes, partially"))
  ) %>%
  mutate(
    ph_surrogate_comp =
      rowSums(.[, surrogate_compvars]) == length(surrogate_compvars),
    ph_caregiver_comp =
      rowSums(.[, caregiver_compvars]) == length(caregiver_compvars)
  )

## -- Specimen log: compliance = >0 tubes drawn on days 1, 3, 5, discharge -----
## Get "proper" study *dates* for each ID
study_dates <- tibble(
  study_date =
    map(pull(all_enrolled, enroll_date), ~ seq(., by = 1, length.out = 30)) %>%
    flatten_int() %>%
    as.Date(origin = "1970-1-1")
)

## Create "dummy" data frame with ID, study event, study day, study date up to
## day 30 for each patient
timeline_df <- tibble(
  id = rep(sort(unique(all_enrolled$id)), each = 30),
  study_day = rep(1:30, length(unique(all_enrolled$id)))
) %>%
  left_join(subset(all_enrolled,
                   select = c(id, enroll_date, death_date, hospdis_date,
                              studywd_date)),
            by = "id") %>%
  bind_cols(study_dates) %>%
  ## Add "status" for each day:
  ##  - deceased
  ##  - discharged
  ##  - withdrawn
  ##  - in hospital
  ## With additional indicator for "transition day", or days on which patients
  ## died, were discharged, or withdrew. These days may or may not have data
  ## collected (eg, if patient died in evening, data may have been collected,
  ## but if patient died in morning, likely that no data was collected).
  mutate(redcap_event_name = ifelse(study_day == 1, "Enrollment /Trial Day 1",
                                    paste("Trial Day", study_day)),
         transition_day = (!is.na(death_date) & study_date == death_date) |
           (!is.na(studywd_date) & study_date == studywd_date) |
           (!is.na(hospdis_date) & study_date == hospdis_date),
         study_status = factor(
           ifelse(!is.na(death_date) & study_date >= death_date, 4,
           ifelse(!is.na(hospdis_date) & study_date >= hospdis_date, 3,
           ifelse(!is.na(studywd_date) & study_date >= studywd_date, 2, 1))),
           levels = 1:4,
           labels = c("In hospital",
                      "Withdrawn",
                      "Discharged",
                      "Deceased")))
  
## Data set for specimens: merge specimen variables onto study days 1, 3, 5, 30
## (for specimen log, 30 = day of discharge, even if patient was discharged
## earlier)
specimen_df <- timeline_df %>%
  select(id, redcap_event_name, study_day, study_date, study_status,
         transition_day) %>%
  left_join(select(inhosp_df, id, redcap_event_name, study_day_specimen,
                   blue_drawn, blue_rsn,
                   purple_drawn, purple_rsn),
            by = c("id", "redcap_event_name")) %>%
  filter((study_day %in% c(1, 3, 5) &
            (study_status == "In hospital" | transition_day)) |
           study_day == 30)

## -- Specimen log: compliance = >0 tubes drawn at discharge ("day 30") --------
## Combine levels for patient discharged, died or withdrew before blood draw
spec_unavail_levels <- paste(
  "Patient",
  c("discharged from hospital", "died", "withdrew"),
  "before blood draw"
)

specimen_df <- inhosp_df %>%
  filter(redcap_event_name == "Discharge Day") %>%
  dplyr::select(id, blue_drawn, blue_rsn, purple_drawn, purple_rsn) %>%
  gather(key = org_var, value = org_value, blue_drawn:purple_rsn) %>%
  separate(org_var, into = c("Color", "var"), sep = "_") %>%
  spread(key = var, value = org_value) %>%
  mutate(drawn = as.numeric(drawn),
         compliant = drawn > 0,
         ## "Insufficient documentation" if drawn is missing, or if patient is
         ## noncompliant and reason is missing
         Reason = factor(ifelse(is.na(drawn), 0,
                         ifelse(compliant, 7,
                         ifelse(is.na(rsn), 0,
                         ifelse(rsn == "Low volume", 6,
                         ifelse(rsn == "No access", 5,
                         ifelse(rsn == "Patient/surrogate refused blood draw", 4,
                         ifelse(rsn %in% spec_unavail_levels, 3,
                         ifelse(rsn == "Not randomized", 2, 1)))))))),
                         levels = 0:7,
                         labels = c("Incomplete<br>documentation",
                                    "Other",
                                    "Not randomized",
                                    "Pt unavailable",
                                    "Refusal",
                                    "No access",
                                    "Low volume",
                                    "Compliant")))
  
specimen_rsns <- specimen_df %>%
  group_by(Color, Reason) %>%
  summarise(n_reason = n()) %>%
  ungroup() %>%
  mutate(Percent = (n_reason / n_enrolled) * 100)

## -- Accelerometer info -------------------------------------------------------

## Patient-days
## On what percentage of patient-days has the accelerometer been removed?
n_hosp_days <- sum(!is.na(inhosp_df$daily_date))

## Get number of days with accelerometer info
n_accel_days <- with(inhosp_df, sum(!is.na(bed_device_num) & !is.na(daily_date)))

## Get number of days accelerometer was removed at least once
n_accel_rm <- sum(inhosp_df$bed_device_num > 0, na.rm = TRUE)

## Patients with device permanently removed
pts_accel_rm <- inhosp_df %>%
  dplyr::select(id, starts_with("bed_remove_why")) %>%
  gather(key = time, value = reason, bed_remove_why_1:bed_remove_why_8) %>%
  filter(!is.na(reason)) %>%
  unique()

n_accel_permrm <- sum(pts_accel_rm$reason == "Permanent discontinuation")

## Summarize reasons for device removal
sum_accel_rm <- pts_accel_rm %>%
  group_by(reason) %>%
  count() %>%
  arrange(desc(n))

## -- Number of times/day accelerometer was removed ----------------------------
accel_rm_df <- inhosp_df %>%
  filter(!is.na(daily_date)) %>%
  dplyr::select(id, daily_date, bed_device_num) %>%
  separate(daily_date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(mabb = month.abb[as.numeric(month)],
         myear = paste(year, month, sep = "-"),
         myear_char = ifelse(mabb == "Mar", paste(mabb, year), mabb))

