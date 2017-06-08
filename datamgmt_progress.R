################################################################################
## Data management to create MOSAIC study progress dashboard
################################################################################

library(RCurl)

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
  tmp_csv
  
  ## REDCap loves to use so many underscores; one per instance seems like plenty
  names(tmp_csv) <- gsub('_+', '_', names(tmp_csv))
}

inhosp_df <- import_df("MOSAIC_IH_TOKEN")
exc_df <- import_df("MOSAIC_EXC_TOKEN")
