log_file <- "run2.log"

log_con <- file(log_file, open = "wt")

sink(log_con, split = TRUE)
sink(log_con, type = "message")

library(dplyr)
library(pharmaverseadam)
library(ggplot2)
library(binom)

adae <- pharmaverseadam::adae
plot_data <- adae %>%
  filter(!is.na(TRT01A), !is.na(AESEV)) %>%
  count(TRT01A, AESEV) %>%
  group_by(TRT01A) %>%
  mutate(prop = n / sum(n))

ggplot(plot_data, aes(x = TRT01A, y = n, fill = AESEV)) +
  geom_bar(stat = "identity") +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "Treatment",
    y = "Number of AEs",
    fill = "Severity"
  ) +
  theme_minimal()

adae <- pharmaverseadam::adae

# Total unique subjects (denominator)
n_subj <- adae %>%
  distinct(USUBJID) %>%
  nrow()

# Subject-level AE occurrence
ae_subj <- adae %>%
  filter(!is.na(AETERM)) %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "x") %>%
  mutate(
    ci = binom.confint(x, n_subj, methods = "exact")  # Clopper-Pearson
  ) %>%
  mutate(
    prop = x / n_subj,
    lcl = ci$lower,
    ucl = ci$upper
  ) %>%
  select(-ci)

top10 <- ae_subj %>%
  arrange(desc(x)) %>%
  slice_head(n = 10)

top10 <- top10 %>%
  mutate(
    prop = prop * 100,
    lcl = lcl * 100,
    ucl = ucl * 100
  )

n_subj <- adae %>%
  distinct(USUBJID) %>%
  nrow()

ci_label <- "95% Clopper-Pearson CI"

ggplot(top10, aes(x = reorder(AETERM, prop), y = prop)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2) +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("Number of patients: ", n_subj, "; ", ci_label),
    x = NULL,
    y = paste0("Percentage of Patients (%)")
  )

cat("\nProcessing complete\n")

cat("\n=== END:", format(Sys.time()), "===\n")

sink(type = "message")

sink()

close(log_con)