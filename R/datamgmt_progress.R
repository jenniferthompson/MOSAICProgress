################################################################################
## Data management to create MOSAIC study progress dashboard
################################################################################

library(httr)
library(tidyverse)
library(lubridate)

## -- Import each data set from REDCap (exclusions, in-hospital, follow-up) ----
## All tokens are stored in .Renviron

## We'll be doing the same thing for each, so write some functions
## 1. Function to create postForm() object given a database token
get_pF <- function(rctoken){
  httr::POST(
    url = "https://redcap.vanderbilt.edu/api/",
    body = list(
      token = Sys.getenv(rctoken),   ## API token gives you permission
      content = "record",            ## export *records*
      format = "csv",                ## export as *CSV*
      rawOrLabel = "label",          ## export factor *labels* v codes
      exportCheckboxLabel = TRUE,    ## exp. checkbox labels vs U/C
      exportDataAccessGroups = FALSE ## don't need data access grps
    )
  )
}

get_csv <- function(pF){
  read.csv(text = as.character(pF), na.strings = "", stringsAsFactors = FALSE)
}

import_df <- function(rctoken){
  tmp_pF <- get_pF(rctoken)
  tmp_csv <- get_csv(tmp_pF)

  ## REDCap loves to use so many underscores; one per instance seems like plenty
  names(tmp_csv) <- gsub("_+", "_", names(tmp_csv))

  tmp_csv
}

## Comment out while building dashboard to save time
inhosp_df <- import_df("MOSAIC_IH_TOKEN")
exc_df <- import_df("MOSAIC_EXC_TOKEN")
fu_df <- import_df("MOSAIC_FU_TOKEN")
# save(inhosp_df, exc_df, fu_df, file = "testdata/testdata.Rdata")
# load("../testdata/testdata.Rdata")

## Remove test patients from each database
inhosp_df <- inhosp_df[grep("test", tolower(inhosp_df$id), invert = TRUE),]
exc_df <- exc_df[grep("test", tolower(exc_df$exc_id), invert = TRUE),]

## Data management prep: Create POSIXct versions of most relevant date/times
dtvars <- c("enroll_dttm", "death_dttm", "hospdis_dttm")
datevars <- c("daily_date")

inhosp_df <- inhosp_df %>%
  mutate_at(dtvars, "ymd_hm") %>%
  mutate_at(dtvars, funs(date = "as_date")) %>%
  rename_at(dtvars, ~ gsub("tm$", "", .)) %>%
  rename_at(paste0(dtvars, "_date"), ~ gsub("_dttm", "", ., fixed = TRUE)) %>%
  mutate(studywd_date = ymd(studywd_dttm)) %>%
  mutate_at(datevars, ymd) %>%
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
##   employment questionnaire, AUDIT, IQCODE; BDI, if enrolled >= 6/19/2018
## Caregiver battery: Zarit, memory/behavior checklist
## "Complete" = every section fully or partially completed
surrogate_compvars <- paste0(
  c("gq", "pase", "adl", "ls", "emp", "audit", "iqcode", "bdi"),
  "_comp_ph"
)
caregiver_compvars <- paste0(c("zarit", "memory"), "_comp_ph")

all_enrolled <- all_enrolled %>%
  mutate_at(
    vars(one_of(c(surrogate_compvars, caregiver_compvars))),
    funs(!is.na(.) & str_detect(., "^Yes"))
  ) %>%
  ## BDI was not included in the battery until June 19, 2018; set these to
  ##  missing, rather than FALSE
  mutate(
    bdi_comp_ph = if_else(enroll_date < as.Date("2018-06-19"), NA, bdi_comp_ph)
  ) %>%
  mutate(
    ph_surrogate_comp = case_when(
      enroll_date < as.Date("2018-06-19") ~
        rowSums(.[, setdiff(surrogate_compvars, "bdi_comp_ph")]) ==
          length(surrogate_compvars) - 1,
      TRUE ~ rowSums(.[, surrogate_compvars]) == length(surrogate_compvars)
    ),
    ph_caregiver_comp =
      rowSums(.[, caregiver_compvars]) == length(caregiver_compvars)
  )

## -- Specimen log: compliance = >0 tubes drawn on days 1, 3, 5, discharge -----
## Get "proper" study *dates* for each ID
study_dates <- tibble(
  study_date =
    map(pull(all_enrolled, enroll_date), ~ seq(., by = 1, length.out = 29)) %>%
    flatten_int() %>%
    as.Date(origin = "1970-1-1")
)

## Create "dummy" data frame with ID, study event, study day, study date up to
## day 30 for each patient
timeline_df <- tibble(
  id = rep(sort(unique(all_enrolled$id)), each = 29),
  study_day = rep(1:29, length(unique(all_enrolled$id)))
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
  mutate(
    redcap_event_name = case_when(
      study_day == 1  ~ "Enrollment /Trial Day 1",
      study_day == 29 ~ "Discharge Day",
      TRUE            ~ paste("Trial Day", study_day)
    ),
    transition_day = (!is.na(death_date) & study_date == death_date) |
      (!is.na(studywd_date) & study_date == studywd_date) |
      (!is.na(hospdis_date) & study_date == hospdis_date),
    study_status = factor(
      ifelse(!is.na(death_date) & study_date >= death_date, 4,
      ifelse(!is.na(hospdis_date) & study_date >= hospdis_date, 3,
      ifelse(!is.na(studywd_date) & study_date >= studywd_date, 2, 1))),
      levels = 1:4,
      labels = c("In hospital", "Withdrawn", "Discharged", "Deceased"))
  )
  
## Data set for specimens: merge specimen variables onto study days 1, 3, 5, 29
## (for specimen log, 29 = day of discharge, even if patient was discharged
## earlier)
specimen_df <- timeline_df %>%
  dplyr::select(
    id, redcap_event_name, study_day, study_status, transition_day
  ) %>%
  left_join(
    dplyr::select(inhosp_df, id, redcap_event_name, blue_drawn, purple_drawn),
    by = c("id", "redcap_event_name")
  ) %>%
  ## Restrict to:
  ## - Days 1, 3, 5, if patient was still hospitalized at least part of the day
  ## - Day 29 (d/c event) if pt still hospitalized or had been discharged alive
  ## If patient was discharged on day 7, still had "discharge" specimens entered
  ## on discharge event.
  filter(
    (study_day %in% c(1, 3, 5) &
       (study_status == "In hospital" | transition_day)) |
      (study_day == 29 & study_status %in% c("In hospital", "Discharged"))
  ) %>%
  ## Reshape to long format, with one record per day/tube color
  dplyr::select(id, study_day, blue_drawn, purple_drawn) %>%
  gather(key = Color, value = drawn, blue_drawn:purple_drawn) %>%
  mutate(
    Color = str_replace(Color, "\\_drawn", ""),
    ## Compliance: At least one tube drawn
    compliant = !is.na(drawn) & drawn > 0,
    ## Factor version of study_day
    Day = factor(
      case_when(
        study_day == 1 ~ 1,
        study_day == 3 ~ 2,
        study_day == 5 ~ 3,
        TRUE           ~ 4
      ),
      levels = 1:4,
      labels = c("Day 1", "Day 3", "Day 5", "Discharge")
    )
  ) %>%
  ## Blue tubes are not drawn on days 3/5; keeping them in was causing
  ##  tooltip issues
  filter(!(Color == "blue" & study_day %in% c(3, 5))) %>%
  ## Summarize % compliance by study day, tube color
  group_by(Day, Color) %>%
  summarise(
    Compliance = mean(compliant, na.rm = TRUE)
  ) %>%
  ungroup() 

## -- Accelerometer info -------------------------------------------------------

## Patient-days
## On what percentage of patient-days has the accelerometer been removed?
n_hosp_days <- sum(!is.na(inhosp_df$daily_date))

## Get number of days accelerometer was worn
n_accel_days <- with(inhosp_df, sum(coord_ever == "Yes", na.rm = TRUE))

## Get number of days accelerometer was removed at least once
n_accel_rm <- sum(inhosp_df$bed_device_num > 0, na.rm = TRUE)

## Patients with device permanently removed *prior to 48h before discharge*
pts_accel_rm <- inhosp_df %>%
  dplyr::select(id, daily_date, starts_with("bed_remove_why")) %>%
  right_join(subset(all_enrolled, select = c(id, hospdis_date))) %>%
  gather(key = time, value = reason, bed_remove_why_1:bed_remove_why_8) %>%
  filter(!is.na(reason)) %>%
  ## Indicator for whether device was permanently removed on day of or just
  ## prior to discharge (should not count for study monitoring purposes)
  mutate(
    days_before_discharge =
      as.numeric(difftime(hospdis_date, daily_date, units = "days")),
    prep_discharge =
      reason == "Permanent discontinuation" & days_before_discharge %in% 0:1,
    reason_mod = case_when(
      reason == "Permanent discontinuation" & prep_discharge ~
        "Removed within a day of hospital discharge",
      TRUE ~ reason
    )
  ) %>%
  unique()

n_accel_permrm <- sum(pts_accel_rm$reason_mod == "Permanent discontinuation")

## Summarize reasons for device removal
sum_accel_rm <- pts_accel_rm %>%
  group_by(reason_mod) %>%
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

################################################################################
## Follow-Up Phase
################################################################################

## Note: No date field for PASE

## -- Create dummy df: One record per enrolled patient per f/u time point ------
fu_dummy <- cross_df(
  list(
    id = unique(all_enrolled$id),
    redcap_event_name = unique(fu_df$redcap_event_name)
  )
)

## List of assessments done at each time point
asmts_phone <- c("ls", "ph_biadl")
asmts_full <- c(
  "gq", "biadl", "sppb", "hand", "rbans", "trails", "social", "eq5d", "pase",
  "emp", "hus", "bpi", "audit", "zarit", "membehav", "ls"
)
asmts_all <- unique(c(asmts_phone, asmts_full))
asmts_withdate <- setdiff(asmts_all, "pase") ## No date variable for PASE

## -- Function to turn missing assessment indicators to FALSE ------------------
## This happens if (eg) the patient has not yet been reached for an assessment
##  at a given time point; the "test_complete" variable has not yet been filled
##  out, but for monitoring purposes, patient should be counted as not assessed
turn_na_false <- function(x, phone_asmt, df){
  if(phone_asmt){
    ifelse(is.na(x) & df$phone_only & df$fu_elig, FALSE, x)
  } else{
    ifelse(is.na(x) & !df$phone_only & df$fu_elig, FALSE, x)
  }
}

## -- Combine in-hospital dates with follow-up data ----------------------------
fu_df2 <- fu_dummy %>%
  ## Merge in-hospital info onto dummy records
  left_join(
    all_enrolled %>%
      select(id, hospdis_date, studywd_date, death_date, inhosp_status),
    by = "id"
  ) %>%
  left_join(
    fu_df %>%
      ## Select only variables needed for status, completion at time point
      dplyr::select(
        id, redcap_event_name, ends_with("complete_yn"), gq_rsn, rbans_completed,
        trails_completed, pase_comp_ph, emp_complete, hus_complete, bpi_complete,
        ends_with("datecomp"), ends_with("date"), ends_with("date_complete"),
        ends_with("date_compl")
      ),
    by = c("id", "redcap_event_name")
  ) %>%
  ## Rename all completion, date variables for consistency
  rename_at(
    vars(matches("\\_complete.*$"), pase_comp_ph),
    ~ str_replace(., "\\_comp.+$", "_complete")
  ) %>%
  rename_at(
    vars(matches("\\_date.+")), ~ str_replace(., "\\_date.+", "_date")
  ) %>%
  ## Convert dates to Date
  mutate_at(paste0(asmts_withdate, "_date"), ymd) %>%
  ## Was each assessment completed at this time point?
  mutate_at(
    paste0(unique(c(asmts_phone, asmts_full)), "_complete"),
    ~ str_detect(., "^Yes")
  ) %>%
  mutate(
    ## Is this a phone assessment or a full assessment?
    phone_only = str_detect(redcap_event_name, "Phone Call"),
    ## How many assessments were done at each?
    ##  If time point involved a phone assessment, info for full assessment is
    ##  missing, and vice versa
    n_asmts_phone = ifelse(
      phone_only,
      rowSums(.[, paste0(asmts_phone, "_complete")], na.rm = TRUE),
      NA
    ),
    n_asmts_full = ifelse(
      phone_only,
      NA,
      rowSums(.[, paste0(asmts_full, "_complete")], na.rm = TRUE)
    ),
    any_phone = n_asmts_phone > 0,
    any_full = n_asmts_full > 0,
    all_phone = n_asmts_phone == length(asmts_phone),
    all_full = n_asmts_full == length(asmts_full)
  )

## -- Figure out patient's status at each time point ---------------------------
## Get first, last asssessment at each time point (these will often, but not
##  always, be the same; sometimes the assessment was broken up into 2+ calls or
##  visits due to time/fatigue)
asmt_minmax <- fu_df2 %>%
  dplyr::select(id, redcap_event_name, paste0(asmts_withdate, "_date")) %>%
  gather(key = "asmt_type", value = "asmt_date", ends_with("_date")) %>%
  ## What is the earliest, latest followup date at this assessment?
  group_by(id, redcap_event_name) %>%
  summarise(
    ## Necessary to redo ymd(); otherwise it thinks none of them are NA?
    first_asmt = ymd(min(asmt_date, na.rm = TRUE)),
    last_asmt = ymd(max(asmt_date, na.rm = TRUE))
  ) %>%
  ungroup()

fu_long <- fu_df2 %>%
  left_join(asmt_minmax, by = c("id", "redcap_event_name")) %>%
  ## Don't need dates anymore
  dplyr::select(-one_of(paste0(asmts_withdate, "_date"))) %>%
  ## Determine status at each time point
  mutate(
    fu_month = as.numeric(str_extract(redcap_event_name, "^\\d+(?= )")),
    daysto_window = case_when(
      fu_month == 1  ~ 30,
      fu_month == 2  ~ 60,
      fu_month == 3  ~ 83,
      fu_month == 6  ~ 180,
      fu_month == 12 ~ 335,
      TRUE           ~ as.numeric(NA)
    ),
    enter_window = as.Date(hospdis_date + daysto_window),
    exit_window = as.Date(
      case_when(
        fu_month %in% c(1, 2) ~ enter_window + 14,
        fu_month == 3         ~ enter_window + 56,
        fu_month == 6         ~ enter_window + 30,
        fu_month == 12        ~ enter_window + 90,
        TRUE                  ~ as.Date(NA)
      )
    ),
    in_window = ifelse(is.na(hospdis_date), NA, enter_window <= Sys.Date()),
    
    ## Indicator for whether patient refused assessment (but didn't withdraw)
    ## Currently relies on general questions only; checking with Julie
    refused_gq = !is.na(gq_rsn) & gq_rsn == "Patient refusal",
    
    ## Followup status:
    ## - Had >1 assessment: Assessed
    ## - Died prior to end of followup window: Died
    ## - Withdrew prior to end of followup window: Withdrew
    ## - Not yet in the follow-up window: Currently ineligible
    ## - VMO-001-7: consent did not include phone assessments (1, 2, 6m)
    ## - None of the above: Currently lost to follow-up
    fu_status = factor(
      case_when(
        (phone_only & any_phone) | (!phone_only & any_full) ~ 1,
        !is.na(death_date) &
          (inhosp_status == "Died in hospital" |
             death_date < exit_window)                      ~ 2,
        !is.na(studywd_date) &
          (inhosp_status == "Withdrew in hospital" |
             studywd_date < exit_window)                    ~ 3,
        Sys.Date() < enter_window                           ~ 4,
        inhosp_status == "Still in hospital"                ~ as.numeric(NA),
        phone_only & id %in% paste0("VMO-00", 1:7)          ~ 5,
        refused_gq                                          ~ 6,
        TRUE                                                ~ 7
      ),
      levels = 1:7,
      labels = c(
        "Assessment fully or partially completed",
        "Died before follow-up window ended",
        "Withdrew before follow-up window ended",
        "Not yet eligible for follow-up",
        "Consent did not include phone assessment",
        "Refused assessment (but did not withdraw)",
        "Eligible, but not yet assessed"
      )
    ),
    
    ## Indicators for whether patient is eligible for followup (included in
    ##  denominator) and has been assessed (included in numerator)
    fu_elig = fu_status %in% c(
      "Assessment fully or partially completed",
      "Refused assessment (but did not withdraw)",
      "Eligible, but not yet assessed"
    ),
    fu_comp = ifelse(
      !fu_elig, NA, fu_status == "Assessment fully or partially completed"
    )
  ) %>%
  ## Set asmt indicators to FALSE if pt eligible but no data yet entered
  ## Phone only
  mutate_at(
    vars(paste0(asmts_phone, "_complete")),
    funs(ifelse(is.na(.) & phone_only & fu_elig, FALSE, .))
  ) %>%
  ## Full batteries
  mutate_at(
    vars(paste0(asmts_full, "_complete")),
    funs(ifelse(is.na(.) & !phone_only & fu_elig, FALSE, .))
  )

# ## -- Check patients without followup for JV -----------------------------------
# fu_long %>%
#   filter(fu_status == "Eligible, but not yet assessed") %>%
#   dplyr::select(
#     id, redcap_event_name, hospdis_date, enter_window, exit_window
#   ) %>%
#   arrange(redcap_event_name) %>%
#   write_csv(path = "testdata/eligible_nofu.csv", na = "", col_names = TRUE)

## -- Summary statistics for dashboard -----------------------------------------
## Overall % complete at each time point
fu_totals <- fu_long %>%
  dplyr::select(redcap_event_name, fu_elig, fu_comp) %>%
  filter(fu_elig) %>%
  group_by(redcap_event_name) %>%
  summarise(
    n_elig = sum(fu_elig),
    n_comp = sum(fu_comp),
    prop_comp = mean(fu_comp)
  )

fu_asmts <- fu_long %>%
  dplyr::select(redcap_event_name, fu_elig, ends_with("_complete")) %>%
  filter(fu_elig) %>%
  gather(key = asmt_type, value = asmt_done, ends_with("_complete")) %>%
  ## Only include assessments that "match" the time point
  filter(
    (redcap_event_name %in% paste(c(3, 12), "Month Assessment") &
      asmt_type %in% paste0(asmts_full, "_complete")) |
    (redcap_event_name %in% paste(c(1, 2, 6), "Month Phone Call") &
       asmt_type %in% paste0(asmts_phone, "_complete"))
  ) %>%
  group_by(redcap_event_name, asmt_type) %>%
  summarise(
    n_elig = sum(fu_elig),
    n_comp = sum(asmt_done),
    prop_comp = mean(asmt_done)
  )

## -- Rearrange data for Sankey plot -------------------------------------------
## source = enrollment; target = end of hospitalization
sankey_hospital <- all_enrolled %>%
  dplyr::select(id, inhosp_status) %>%
  distinct() %>%
  set_names(c("id", "target")) %>%
  mutate(
    source = "Enrolled",
    target = case_when(
      target == "Still in hospital" ~ "Hospitalized",
      target == "Discharged alive" ~ "Discharged",
      TRUE ~ stringr::str_replace(target, " in ", ", ")
    )
  )

## source = status after illness; target = status at 3m
sankey_3m <- fu_long %>%
  filter(
    redcap_event_name == "3 Month Assessment",
    inhosp_status != "Still in hospital"
  ) %>%
  dplyr::select(id, inhosp_status, fu_status) %>%
  set_names(c("id", "source", "target")) %>%
  mutate(
    source = case_when(
      source == "Died in hospital"     ~ "Died, hospital",
      source == "Withdrew in hospital" ~ "Withdrew, hospital",
      source == "Still in hospital"    ~ "Hospitalized",
      TRUE                             ~ "Discharged"
    ),
    target = case_when(
      source == "Died, hospital" |
        target == "Died before follow-up window ended" ~ "Died, 3m",
      source == "Withdrew, hospital" |
        target == "Withdrew before follow-up window ended" ~ "Withdrew, 3m",
      source == "Hospitalized" ~ "Hospitalized",
      target == "Assessment fully or partially completed" ~ "Assessed, 3m",
      target %in% c(
        "Eligible, but not yet assessed",
        "Refused assessment (but did not withdraw)"
      ) ~ "Not assessed, 3m",
      target == "Not yet eligible for follow-up" ~ "Not yet eligible, 3m",
      TRUE ~ "Missing"
    )
  )

## source = status at 3m; target = status at 12m
sankey_12m <- fu_long %>%
  filter(
    redcap_event_name == "12 Month Assessment",
    inhosp_status != "Still in hospital"
  ) %>%
  dplyr::select(id, fu_status) %>%
  left_join(dplyr::select(sankey_3m, id, target)) %>%
  ## target at 3m is now source at 12m
  set_names(c("id", "target", "source")) %>%
  mutate(
    target = case_when(
      source == "Hospitalized" ~ "Hospitalized",
      target == "Died before follow-up window ended" ~ "Died, 12m",
      target == "Withdrew before follow-up window ended" ~ "Withdrew, 12m",
      target == "Assessment fully or partially completed" ~ "Assessed, 12m",
      target %in% c(
        "Eligible, but not yet assessed",
        "Refused assessment (but did not withdraw)"
      ) ~ "Not assessed, 12m",
      target == "Not yet eligible for follow-up" ~ "Not yet eligible, 12m",
      TRUE ~ "Missing"
    )
  )

## Calculate final weights for each edge (# patients with each source/target combo)
sankey_edges <- bind_rows(sankey_hospital, sankey_3m, sankey_12m) %>%
  dplyr::select(-id) %>%
  group_by(source, target) %>%
  summarise(weight = n()) %>%
  ungroup()
