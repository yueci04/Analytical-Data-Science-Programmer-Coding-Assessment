log_file <- "run.log"

log_con <- file(log_file, open = "wt")

sink(log_con, split = TRUE)
sink(log_con, type = "message")

library(dplyr)
library(rtables)
library(rtables.officer)
library(gtsummary)
library(pharmaversesdtm)

dm <- pharmaversesdtm::dm

adsl <- dm %>%
  select(-DOMAIN)

adae <- pharmaverseadam::adae
adae <- adae |>
  filter(
    # Treatment-emergent AE records
    TRTEMFL == "Y")

tbl <- adae |>
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Any TEAE"
  ) |>
  add_overall()

sort_hierarchical(tbl)

# Export to docx file
count_pct <- function(x, .N_col) {
  x <- x[!is.na(x)]
  n <- length(unique(x))
  
  pct <- ifelse(
    is.na(.N_col) || .N_col == 0,
    0,
    100 * n / .N_col
  )
  
  in_rows(
    "n (%)" = sprintf("%d (%.1f%%)", n, pct)
  )
}
lyt <- basic_table() %>%
  split_cols_by("ACTARM", show_colcounts = TRUE) %>%
  split_rows_by("AESOC",
                split_fun = drop_split_levels, label_pos = "topleft",
                split_label = obj_label(adae$AESOC), page_by = TRUE
  ) %>%
  split_rows_by("AETERM",
                split_fun = drop_split_levels, label_pos = "topleft",
                split_label = obj_label(adae$AETERM)
  ) %>%
  analyze("USUBJID", afun = count_pct)

adae <- bind_rows(
  adae,
  adae %>% mutate(ACTARM = "Total")
)

adsl <- bind_rows(
  adsl,
  adsl %>% mutate(ACTARM = "Total")
)

soc_order <- adae %>%
  filter(TRTEMFL == "Y") %>%
  distinct(USUBJID, AESOC) %>%
  count(AESOC, sort = TRUE) %>%
  pull(AESOC)

pt_order <- adae %>%
  filter(TRTEMFL == "Y") %>%
  distinct(USUBJID, AESOC, AETERM) %>%
  count(AESOC, AETERM, sort = TRUE) %>%
  arrange(AESOC, desc(n)) %>%
  pull(AETERM)

adae_sorted <- adae %>%
  mutate(
    AESOC = factor(AESOC, levels = soc_order),
    AETERM = factor(AETERM, levels = pt_order),
    ACTARM = factor(ACTARM, levels = c(setdiff(unique(ACTARM), "Total"), "Total"))
  )
result <- build_table(lyt, adae_sorted, alt_counts_df = adsl)

tf <- "table.docx"
flx_res <- tt_to_flextable(result)
export_as_docx(flx_res,
               file = tf,
               section_properties = section_properties_default(orientation = "landscape")
)
flx_res

cat("\nProcessing complete\n")

cat("\n=== END:", format(Sys.time()), "===\n")

sink(type = "message")

sink()

close(log_con)