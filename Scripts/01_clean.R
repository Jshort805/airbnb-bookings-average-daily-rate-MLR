#01_clean.R -Inside Airbnb Los Angeles listings: profile, clean, build analytic file
#
#Input : Data/raw/los_angeles_airbnb_listings.csv.gz  (Inside Airbnb, LA, scrape
#         2026-09-14)
#Output: Data/clean/airbnb_analytic.csv          final analytic file
#         Data/clean/data_dictionary_draft.csv    starter dictionary (fill in descriptions)
#         Outputs/raw_profile.csv                 per-variable missing counts, raw file
#         Outputs/cleaning_log.csv                rows/cols after each cleaning step
#Run from the project root:  Rscript Scripts/01_clean.R
#
#Target (Y): price, nightly price in USD. Model log(price) in 03_model.R.
#Read the printed checks first, then adjust the team decisions below.

library(tidyverse)
library(here)

set.seed(2026)

#---- Team decisions (edit here and re-run; every choice is logged in cleaning_log.csv) ----------
PRICE_MIN          <- 20     # REVIEW against the price quantiles printed below
PRICE_MAX          <- 1000   # REVIEW: ~5% of listings are above $1,000
INCLUDE_REVIEWS    <- FALSE  # review fields are only known after bookings happen
MISSING_DROP_PCT   <- 10     # drop variables missing more than this share
TOP_NEIGHBOURHOODS <- 15     # keep the N biggest neighbourhoods, lump the rest
TOP_PROPERTY_TYPES <- 8      # keep the N most common property types, lump the rest
SAMPLE_N           <- NULL   # e.g. 10000 for a seeded random sample; NULL keeps all
LAX                <- c(lat = 33.9416, lon = -118.4085)  # airport, for dist_lax_mi

#The project is pinned to this quarter's snapshot. Inside Airbnb refreshes its data and moves
#older scrapes to an archive, so the copy committed in Data/raw/ is the file used for this analysis.
#See README.md for the source URL and checksum.

EXPECTED_ROWS <- 43781
EXPECTED_COLS <- 90

raw_path <- here("Data", "raw", c("los_angeles_airbnb_listings.csv.gz",
                                  "los_angeles_airbnb_listings.csv"))
raw_path <- raw_path[file.exists(raw_path)][1]
if (is.na(raw_path)) {
  stop("Missing raw file in Data/raw/. Expected los_angeles_airbnb_listings.csv.gz ",
       "(the Inside Airbnb Los Angeles listings.csv.gz, scrape 2026-09-14).")
}
dir.create(here("Data", "clean"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Outputs"), recursive = TRUE, showWarnings = FALSE)

raw <- read_csv(raw_path, guess_max = Inf, show_col_types = FALSE)
if (nrow(raw) != EXPECTED_ROWS || ncol(raw) != EXPECTED_COLS) {
  stop("Raw file is ", nrow(raw), " x ", ncol(raw), " but this project was built on ",
       EXPECTED_ROWS, " x ", EXPECTED_COLS, ". Use the committed 2026-09-14 snapshot.")
}
message("Raw file: ", basename(raw_path), "; last_scraped ",
        min(raw$last_scraped), " to ", max(raw$last_scraped))

log <- tibble(step = character(), rows = integer(), cols = integer(),
              note = character())
log_step <- function(df, step, note = "") {
  log <<- add_row(log, step = step, rows = nrow(df), cols = ncol(df), note = note)
  invisible(df)
}
hot_deck <- function(x) {  # fill NAs with random draws from observed values
  idx <- is.na(x)
  if (any(idx)) x[idx] <- sample(x[!idx], sum(idx), replace = TRUE)
  x
}
haversine_mi <- function(lat1, lon1, lat2, lon2) {  # great-circle distance, miles
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 3958.8 * asin(sqrt(a))
}

# ---- Checks on the raw file (read these before trusting the rules below) ----
message("Raw dimensions: ", nrow(raw), " rows x ", ncol(raw), " columns")
message("Duplicate listing ids: ", sum(duplicated(raw$id)))
profile <- tibble(variable = names(raw),
                  type = map_chr(raw, ~ class(.x)[1]),
                  n_missing = map_int(raw, ~ sum(is.na(.x))),
                  pct_missing = round(n_missing / nrow(raw) * 100, 2),
                  n_distinct = map_int(raw, n_distinct))
write_csv(profile, here("Outputs", "raw_profile.csv"))
price_raw <- parse_number(raw$price)
message("Price quantiles (raw, USD):")
print(round(quantile(price_raw, c(0, .01, .05, .5, .95, .99, 1), na.rm = TRUE)))
message("Missing price by scrape source:")
print(count(raw, source, price_missing = is.na(price_raw)))
message("Room types:"); print(count(raw, room_type))

# ---- EDA 1: raw file (what needs cleaning, and why) -------------------------
message("Variables over the missing threshold (raw file):")
print(profile %>% filter(pct_missing > MISSING_DROP_PCT) %>%
        select(variable, type, pct_missing), n = Inf)
message("Price by room type (raw, USD):")
raw_by_room <- raw %>% mutate(price = price_raw) %>% group_by(room_type) %>%
  summarise(n = n(), n_price = sum(!is.na(price)),
            median = median(price, na.rm = TRUE), mean = round(mean(price, na.rm = TRUE)),
            p99 = quantile(price, .99, na.rm = TRUE), max = max(price, na.rm = TRUE))
print(raw_by_room)
write_csv(raw_by_room, here("Outputs", "eda_raw_price_by_room_type.csv"))
message("Listings priced at or above $1,000: ", sum(price_raw >= 1000, na.rm = TRUE),
        " (", round(mean(price_raw >= 1000, na.rm = TRUE) * 100, 1), "% of priced listings)")

# ---- Cleaning rules ---------------------------------------------------------
df <- log_step(raw, "0 raw file", "as downloaded")

# Record every team decision in the log, with the values used in this run
decisions <- list(PRICE_MIN = PRICE_MIN, PRICE_MAX = PRICE_MAX,
                  INCLUDE_REVIEWS = INCLUDE_REVIEWS, MISSING_DROP_PCT = MISSING_DROP_PCT,
                  TOP_NEIGHBOURHOODS = TOP_NEIGHBOURHOODS,
                  TOP_PROPERTY_TYPES = TOP_PROPERTY_TYPES,
                  SAMPLE_N = if (is.null(SAMPLE_N)) "NULL (keep all rows)" else SAMPLE_N,
                  LAX = paste(names(LAX), LAX, collapse = ", "), SEED = 2026,
                  RAW_FILE = basename(raw_path))
for (nm in names(decisions)) {
  df <- log_step(df, paste("setting", nm), paste(decisions[[nm]], collapse = ", "))
}

# 1. Keep only the columns needed. Dropped on purpose: URLs, text descriptions, host
#    fields that are 100% empty in this scrape, outcome/demand fields (availability,
#    estimated occupancy and revenue) and the price_quote_* copies of the target.
keep <- c("id", "price", "room_type", "property_type", "accommodates", "bedrooms",
          "beds", "bathrooms_text", "amenities", "license", "minimum_nights",
          "neighbourhood_cleansed", "latitude", "longitude", "host_is_superhost",
          "host_identity_verified", "hosts_time_as_host_years",
          "calculated_host_listings_count")
review_cols <- c("number_of_reviews", "review_scores_rating")
if (INCLUDE_REVIEWS) keep <- c(keep, review_cols)
df <- df %>% select(all_of(keep)) %>%
  log_step("1 select columns", paste(length(keep), "of", ncol(raw), "kept"))

# 2. Duplicate listings (the file should have one row per id)
df <- df %>% distinct(id, .keep_all = TRUE) %>% log_step("2 one row per listing id")

# 3. Target: parse "$1,234.00" to a number; listings with no price cannot be modeled
df <- df %>% mutate(price = parse_number(price))
before <- nrow(df)
df <- df %>% filter(!is.na(price)) %>%
  log_step("3 missing price", paste("removed", before - nrow(df), "listings with no price"))

# 4. Implausible prices
before <- nrow(df)
df <- df %>% filter(price >= PRICE_MIN, price <= PRICE_MAX) %>%
  log_step("4 price range", paste0("kept $", PRICE_MIN, "-$", PRICE_MAX,
                                   "; removed ", before - nrow(df)))

# 5. Variables over the missing threshold are dropped. Exempt: price (handled above),
#    license (turned into a has_license flag in step 7), and review fields if included.
exempt <- c("price", "license", if (INCLUDE_REVIEWS) review_cols)
too_missing <- profile %>%
  filter(pct_missing > MISSING_DROP_PCT, variable %in% names(df),
         !variable %in% exempt) %>% pull(variable)
df <- df %>% select(-any_of(too_missing)) %>%
  log_step("5 drop variables over missing threshold",
           paste0(">", MISSING_DROP_PCT, "% missing: ",
                  if (length(too_missing)) paste(too_missing, collapse = ", ") else "none"))

# 6. Bathrooms arrive as text ("1.5 baths", "1 shared bath", "Half-bath")
df <- df %>%
  mutate(bathrooms_text = tolower(bathrooms_text),
         shared_bath = coalesce(str_detect(bathrooms_text, "shared"), FALSE),
         bathrooms = if_else(str_detect(bathrooms_text, "half"), 0.5,
                             parse_number(bathrooms_text))) %>%
  log_step("6 parse bathrooms", "bathrooms_text -> bathrooms (number), shared_bath (flag)")

# 7. Derived variables (antecedents are recorded in the data dictionary)
df <- df %>%
  mutate(n_amenities = if_else(amenities == "[]", 0L,
                               str_count(amenities, '",\\s*"') + 1L),
         has_license = !is.na(license),
         dist_lax_mi = haversine_mi(latitude, longitude, LAX["lat"], LAX["lon"])) %>%
  select(-amenities, -license, -bathrooms_text) %>%
  log_step("7 derived variables",
           "n_amenities, has_license, dist_lax_mi (miles from LAX)")

# 8. Remaining missing values: hot-deck the few gaps in size fields, delete the rest
fill_cols <- c("bedrooms", "beds", "bathrooms")
n_filled <- map_int(df[fill_cols], ~ sum(is.na(.x)))
df <- df %>% mutate(across(all_of(fill_cols), hot_deck))
before <- nrow(df)
df <- df %>% drop_na() %>%
  log_step("8 missing values",
           paste0("hot-deck ", paste(fill_cols, n_filled, sep = "=", collapse = ", "),
                  "; listwise deleted ", before - nrow(df), " rows"))

# 9. Transformations: lump rare categories, text -> factor, drop helper columns
df <- df %>%
  mutate(neighbourhood_grp = fct_lump_n(factor(neighbourhood_cleansed),
                                        n = TOP_NEIGHBOURHOODS, other_level = "Other"),
         property_type_grp = fct_lump_n(factor(property_type),
                                        n = TOP_PROPERTY_TYPES, other_level = "Other"),
         room_type = factor(room_type)) %>%
  select(-neighbourhood_cleansed, -property_type) %>%
  log_step("9 transformations",
           paste0("neighbourhood_grp (top ", TOP_NEIGHBOURHOODS, " + Other), ",
                  "property_type_grp (top ", TOP_PROPERTY_TYPES, " + Other), ",
                  "room_type as factor"))

# 10. Optional seeded random sample
if (!is.null(SAMPLE_N) && nrow(df) > SAMPLE_N) {
  df <- slice_sample(df, n = SAMPLE_N) %>%
    log_step("10 random sample", paste("n =", SAMPLE_N, "seed = 2026"))
}

# ---- Save -------------------------------------------------------------------
log <- log %>% mutate(rows_removed = lag(rows) - rows)
write_csv(log, here("Outputs", "cleaning_log.csv"))
write_csv(df, here("Data", "clean", "airbnb_analytic.csv"))

# Starter data dictionary: fill in the description column by hand
antecedents <- c(bathrooms = "bathrooms_text", shared_bath = "bathrooms_text",
                 n_amenities = "amenities", has_license = "license",
                 dist_lax_mi = "latitude, longitude",
                 neighbourhood_grp = "neighbourhood_cleansed",
                 property_type_grp = "property_type")
dict <- tibble(
  variable = names(df),
  role = case_when(variable == "price" ~ "Y (target)",
                   variable == "id" ~ "ID (not a predictor)",
                   variable %in% c("latitude", "longitude") ~ "maps only (use dist_lax_mi)",
                   TRUE ~ "X (predictor)"),
  type = map_chr(df, ~ class(.x)[1]),
  n_distinct = map_int(df, n_distinct),
  values = map_chr(df, function(x) {
    if (is.numeric(x)) paste0(round(min(x), 2), " to ", round(max(x), 2))
    else if (n_distinct(x) <= 8) paste(sort(unique(as.character(x))), collapse = " | ")
    else paste(n_distinct(x), "levels")
  }),
  antecedent = coalesce(unname(antecedents[variable]), ""),
  description = ""
)
write_csv(dict, here("Data", "clean", "data_dictionary_draft.csv"))

# ---- EDA 2: cleaned file (tables saved to Outputs/; charts go in 02_explore.R) ----
num_cols <- df %>% select(where(is.numeric), -id, -latitude, -longitude)
eda_numeric <- num_cols %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarise(mean = mean(value), sd = sd(value), min = min(value),
            median = median(value), max = max(value), .groups = "drop")
message("Numeric variables, cleaned file:"); print(eda_numeric, n = Inf)
write_csv(eda_numeric, here("Outputs", "eda_numeric_summary.csv"))

eda_cor <- num_cols %>% mutate(log_price = log(price)) %>%
  summarise(across(-c(price, log_price), ~ cor(.x, log_price))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "cor_with_log_price") %>%
  arrange(desc(abs(cor_with_log_price)))
message("Correlation with log(price), cleaned file:"); print(eda_cor, n = Inf)
write_csv(eda_cor, here("Outputs", "eda_correlation_with_log_price.csv"))

eda_room <- df %>% group_by(room_type) %>%
  summarise(n = n(), median_price = median(price), mean_price = round(mean(price)),
            .groups = "drop")
message("Price by room type, cleaned file:"); print(eda_room)
write_csv(eda_room, here("Outputs", "eda_price_by_room_type.csv"))

eda_area <- df %>% group_by(neighbourhood_grp) %>%
  summarise(n = n(), median_price = median(price), .groups = "drop") %>%
  arrange(desc(median_price))
message("Median price by neighbourhood group, cleaned file:"); print(eda_area, n = Inf)
write_csv(eda_area, here("Outputs", "eda_price_by_neighbourhood.csv"))
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
message("Skewness of price: ", round(skew(df$price), 2),
        "; of log(price): ", round(skew(log(df$price)), 2))

message("Final analytic file: ", nrow(df), " x ", ncol(df))
print(select(log, step, rows, cols, rows_removed), n = Inf)
glimpse(df)
