# 01_clean.R -- Hotel booking ADR project: profile, clean, and build the analytic file
#
# Input : Data/raw/hotel_bookings.csv  (Kaggle copy of Antonio, Almeida & Nunes, 2019;
#         download it manually and drop it in Data/raw/. Never edit this file.)
# Output: Data/clean/hotel_analytic.csv          final analytic file
#         Data/clean/data_dictionary_draft.csv   starter dictionary (fill in descriptions)
#         Outputs/raw_profile.csv                per-variable missing counts, raw file
#         Outputs/cleaning_log.csv               rows/cols after each cleaning step
# Run from the project root:  Rscript Scripts/01_clean.R
#
# NOTE: written from the documented column list; not yet run against the real file.
# Read the printed checks first, then adjust the team decisions below.

library(tidyverse)
library(here)

set.seed(2026)

# ---- Team decisions (edit here and re-run; every choice is logged) ----------
KEEP_CANCELED     <- FALSE  # FALSE = analyze realized stays only
DROP_DUPLICATES   <- FALSE  # no booking ID, so identical rows may be real bookings
ADR_MAX_PLAUSIBLE <- 1000   # REVIEW against the top ADR values printed below
DROP_POST_BOOKING <- TRUE   # drop fields not known when the rate is set
MISSING_DROP_PCT  <- 10     # drop variables missing more than this share
TOP_COUNTRIES     <- 10     # keep the N most frequent countries, lump the rest
SAMPLE_N          <- 8000   # rows in the analytic file (NULL keeps everything)

raw_path <- here("Data", "raw", "hotel_bookings.csv")
if (!file.exists(raw_path)) {
  stop("Missing ", raw_path, ". Download hotel_bookings.csv from Kaggle ",
       "(jessemostipak/hotel-booking-demand) and place it in Data/raw/.")
}
dir.create(here("Data", "clean"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Outputs"), recursive = TRUE, showWarnings = FALSE)

# 'NULL' is how the source marks missing text values
raw <- read_csv(raw_path, na = c("", "NA", "NULL"), show_col_types = FALSE)

log <- tibble(step = character(), rows = integer(), cols = integer(),
              note = character())
log_step <- function(df, step, note = "") {
  log <<- add_row(log, step = step, rows = nrow(df), cols = ncol(df), note = note)
  invisible(df)
}
hot_deck <- function(x) {  # fill NAs with random draws from observed values
  idx <- is.na(x)
  x[idx] <- sample(x[!idx], sum(idx), replace = TRUE)
  x
}

# ---- Checks on the raw file (read these before trusting the rules below) ----
message("Raw dimensions: ", nrow(raw), " rows x ", ncol(raw), " columns")
message("Exact duplicate rows: ", sum(duplicated(raw)))
message("ADR <= 0: ", sum(raw$adr <= 0, na.rm = TRUE))
message("Arrival years: ", paste(sort(unique(raw$arrival_date_year)), collapse = ", "))
profile <- tibble(variable = names(raw),
                  type = map_chr(raw, ~ class(.x)[1]),
                  n_missing = map_int(raw, ~ sum(is.na(.x))),
                  pct_missing = round(n_missing / nrow(raw) * 100, 2),
                  n_distinct = map_int(raw, n_distinct))
write_csv(profile, here("Outputs", "raw_profile.csv"))
print(profile, n = Inf)
message("Ten highest ADR values:")
print(raw %>% arrange(desc(adr)) %>%
        select(hotel, arrival_date_year, arrival_date_month, adr) %>% head(10))
message("Meal categories:"); print(count(raw, meal))

# ---- Cleaning rules ---------------------------------------------------------
df <- log_step(raw, "0 raw file", "as downloaded; 'NULL' read as NA")

# 1. Duplicates (optional)
if (DROP_DUPLICATES) {
  df <- df %>% distinct() %>% log_step("1 drop exact duplicates")
}

# 2. Variables missing more than the threshold are dropped
too_missing <- profile %>% filter(pct_missing > MISSING_DROP_PCT) %>% pull(variable)
df <- df %>% select(-any_of(too_missing)) %>%
  log_step("2 drop variables over missing threshold",
           paste0(">", MISSING_DROP_PCT, "% missing: ", paste(too_missing, collapse = ", ")))

# 3. Fields only known after the booking outcome (leakage)
df <- df %>% select(-any_of(c("reservation_status", "reservation_status_date"))) %>%
  log_step("3 drop outcome-leakage fields", "reservation_status, reservation_status_date")

# 4. Cancellation scope; the flag itself is not known at booking, so it is dropped
if (!KEEP_CANCELED) df <- df %>% filter(is_canceled == 0)
df <- df %>% select(-is_canceled) %>%
  log_step("4 cancellation scope", paste("KEEP_CANCELED =", KEEP_CANCELED))

# 5. Remaining missing values: hot-deck for country, listwise delete otherwise
n_country_na <- sum(is.na(df$country))
df <- df %>% mutate(country = hot_deck(country))
before <- nrow(df)
df <- df %>% drop_na() %>%
  log_step("5 missing values",
           paste0("hot-deck country (", n_country_na, " values); listwise deleted ",
                  before - nrow(df), " rows"))

# 6. Invalid ADR
before <- nrow(df)
df <- df %>% filter(adr > 0, adr <= ADR_MAX_PLAUSIBLE) %>%
  log_step("6 invalid ADR", paste0("kept 0 < adr <= ", ADR_MAX_PLAUSIBLE,
                                   "; removed ", before - nrow(df)))

# 7. Impossible bookings: no guests or no nights
before <- nrow(df)
df <- df %>%
  filter(adults + children + babies > 0,
         stays_in_weekend_nights + stays_in_week_nights > 0) %>%
  log_step("7 zero guests or zero nights", paste("removed", before - nrow(df)))

# 8. Inconsistent labels: 'Undefined' and 'SC' both mean no meal package
df <- df %>% mutate(meal = if_else(meal == "Undefined", "SC", meal)) %>%
  log_step("8 recode meal", "Undefined -> SC")

# 9. Fields not known when the rate is set (team decision)
post_booking <- c("assigned_room_type", "booking_changes", "days_in_waiting_list",
                  "required_car_parking_spaces", "total_of_special_requests")
if (DROP_POST_BOOKING) {
  df <- df %>% select(-any_of(post_booking)) %>%
    log_step("9 drop post-booking fields", paste(post_booking, collapse = ", "))
}

# 10. Transformations: derived country_grp (antecedent: country), month as factor,
#     text columns as factors. Set reference levels in the modeling script.
df <- df %>%
  mutate(country_grp = fct_lump_n(factor(country), n = TOP_COUNTRIES,
                                  other_level = "Other"),
         arrival_date_month = factor(arrival_date_month, levels = month.name)) %>%
  select(-country) %>%
  mutate(across(where(is.character), as.factor)) %>%
  log_step("10 transformations",
           paste0("country -> country_grp (top ", TOP_COUNTRIES,
                  " + Other); month as factor; text -> factor"))

# 11. Seeded random sample so stepwise/VIF work stays manageable
if (!is.null(SAMPLE_N) && nrow(df) > SAMPLE_N) {
  df <- slice_sample(df, n = SAMPLE_N) %>%
    log_step("11 random sample", paste("n =", SAMPLE_N, "seed = 2026"))
}

# ---- Save -------------------------------------------------------------------
log <- log %>% mutate(rows_removed = lag(rows) - rows)
write_csv(log, here("Outputs", "cleaning_log.csv"))
write_csv(df, here("Data", "clean", "hotel_analytic.csv"))

# Starter data dictionary: fill in the description column by hand
dict <- tibble(
  variable = names(df),
  type = map_chr(df, ~ class(.x)[1]),
  n_distinct = map_int(df, n_distinct),
  values = map_chr(df, function(x) {
    if (is.numeric(x)) paste0(min(x), " to ", max(x))
    else if (n_distinct(x) <= 8) paste(sort(unique(as.character(x))), collapse = " | ")
    else paste(n_distinct(x), "levels")
  }),
  antecedent = if_else(variable == "country_grp", "country", ""),
  target = if_else(variable == "adr", "Y", ""),
  description = ""
)
write_csv(dict, here("Data", "clean", "data_dictionary_draft.csv"))

message("Final analytic file: ", nrow(df), " x ", ncol(df))
print(log)
glimpse(df)
