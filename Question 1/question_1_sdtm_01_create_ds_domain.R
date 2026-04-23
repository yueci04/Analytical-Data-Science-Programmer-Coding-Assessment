log_file <- "logfile2.log"

log_con <- file(log_file, open = "wt")

sink(log_con, split = TRUE)
sink(log_con, type = "message")

library(pharmaversesdtm)
library(admiral)
library(dplyr)
library(stringr)
library(sdtm.oak)
library(pharmaverseraw)

ds_raw <- pharmaverseraw::ds_raw

study_ct <-
  data.frame(
    stringsAsFactors = FALSE,
    codelist_code = c("C66727","C66727",
                      "C66727","C66727","C66727","C66727","C66727","C66727",
                      "C66727","C66727"),
    term_code = c("C41331","C25250",
                  "C28554","C48226","C48227","C48250","C142185","C49628",
                  "C49632","C49634"),
    term_value = c("ADVERSE EVENT",
                   "COMPLETED","DEATH","LACK OF EFFICACY","LOST TO FOLLOW-UP",
                   "PHYSICIAN DECISION","PROTOCOL VIOLATION",
                   "SCREEN FAILURE","STUDY TERMINATED BY SPONSOR",
                   "WITHDRAWAL BY SUBJECT"),
    collected_value = c("Adverse Event",
                        "Complete","Dead","Lack of Efficacy","Lost To Follow-Up",
                        "Physician Decision","Protocol Violation",
                        "Trial Screen Failure","Study Terminated By Sponsor",
                        "Withdrawal by Subject"),
    term_preferred_term = c("AE","Completed","Died",
                            NA,NA,NA,"Violation",
                            "Failure to Meet Inclusion/Exclusion Criteria",NA,"Dropout"),
    term_synonyms = c("ADVERSE EVENT",
                      "Completed","Death",NA,"Lost to Follow-Up",NA,NA,"Screen Failure","Study Terminated by Sponsor",
                      "Discontinued Participation")
  )

ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  ) %>%
  mutate(
    IT.DSDECOD = na_if(str_squish(IT.DSDECOD), ""),
    OTHERSP    = na_if(str_squish(OTHERSP), ""),
    
    DSCAT = case_when(
      IT.DSDECOD == "Randomized" ~ "PROTOCOL MILESTONE",
      !is.na(OTHERSP)            ~ "OTHER EVENT",
      TRUE                       ~ "DISPOSITION EVENT"
    ),
    
    DSTERM_SRC = if_else(!is.na(OTHERSP), OTHERSP, IT.DSDECOD),
    
    DSDECOD_NOCT_SRC = if_else(!is.na(OTHERSP), OTHERSP, NA_character_),
    
    IT.DSDECOD_DISP = case_when(
      IT.DSDECOD == "Randomized" ~ NA_character_,
      !is.na(OTHERSP)            ~ NA_character_,
      TRUE                       ~ IT.DSDECOD
    )
  )

ds <- ds_raw %>%
  mutate(
    STUDYID = STUDY,
    DOMAIN  = "DS",
    USUBJID = paste0("01-", PATNUM)
  ) %>%
  select(oak_id, raw_source, patient_number, STUDYID, DOMAIN, USUBJID, DSCAT) %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("mm-dd-yyyy"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL","DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("mm-dd-yyyy", "H:M"),
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "DSTERM_SRC",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "DSDECOD_NOCT_SRC",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD_DISP",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  )

cat("\nProcessing complete\n")

cat("\n=== END:", format(Sys.time()), "===\n")

sink(type = "message")

sink()

close(log_con)