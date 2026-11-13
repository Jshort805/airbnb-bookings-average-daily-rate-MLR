# 01_clean.R -- Clean the Inside Airbnb Los Angeles listings (scrape 2026-09-14)
# Input : Data/raw/los_angeles_airbnb_listings.csv.gz  (never edit this file)
# Output: Data/clean/airbnb_analytic.csv  (final analytic file)
#         Data/clean/data_dictionary_draft.csv  (variable names and types)
# Run from the project root:  Rscript Scripts/01_clean.R
# Each step prints how many rows it removed, so the console output is our cleaning log.

set.seed(2026)

## Part 1: Settings (change here and re-run)
PRICE_MIN <- 20
PRICE_MAX <- 1000
TOP_NEIGHBORHOODS <- 15
TOP_PROPERTY_TYPES <- 8
lax_lat <- 33.9416
lax_lon <- -118.4085

## Part 2: Read the raw file
raw <- read.csv("Data/raw/los_angeles_airbnb_listings.csv.gz", na.strings = c("", "NA"))
cat("Raw file:", nrow(raw), "rows x", ncol(raw), "columns\n")
if (nrow(raw) != 43781 || ncol(raw) != 90) stop("Use the committed 2026-09-14 snapshot")

## Part 3: Clean
# Step 1: keep only the columns we need
keep <- c("id", "price", "room_type", "property_type", "accommodates", "bedrooms",
          "beds", "bathrooms_text", "amenities", "license", "minimum_nights",
          "neighbourhood_cleansed", "latitude", "longitude", "host_is_superhost",
          "host_identity_verified", "hosts_time_as_host_years",
          "calculated_host_listings_count")
df <- raw[, keep]
cat("Step 1: kept", ncol(df), "of", ncol(raw), "columns\n")

# Step 2: one row per listing
n <- nrow(df)
df <- df[!duplicated(df$id), ]
cat("Step 2: removed", n - nrow(df), "duplicate rows;", nrow(df), "left\n")

# Step 3: price is text like "$1,234.00"; make it a number and drop listings with no price
df$price <- as.numeric(gsub("[$,]", "", df$price))
n <- nrow(df)
df <- df[!is.na(df$price), ]
cat("Step 3: removed", n - nrow(df), "rows with no price;", nrow(df), "left\n")

# Step 4: remove extreme outliers
n <- nrow(df)
df <- df[df$price >= PRICE_MIN & df$price <= PRICE_MAX, ]
cat("Step 4: removed", n - nrow(df), "rows outside $", PRICE_MIN, "-", PRICE_MAX, ";",
    nrow(df), "left\n")

# Step 5: check missing values (license is mostly empty, so we turn it into a flag below)
cat("Step 5: percent missing by column (only columns with missing values):\n")
pct_missing <- round(colMeans(is.na(df)) * 100, 1)
print(pct_missing[pct_missing > 0])

# Step 6: yes/no columns are "t"/"f" text; bathrooms is text like "1.5 baths"
df$host_is_superhost <- df$host_is_superhost == "t"
df$host_identity_verified <- df$host_identity_verified == "t"
bath_text <- tolower(df$bathrooms_text)
df$shared_bath <- grepl("shared", bath_text)
df$bathrooms <- as.numeric(gsub("[^0-9.]", "", bath_text))
df$bathrooms[grepl("half", bath_text)] <- 0.5

# Step 7: new variables
df$n_amenities <- lengths(regmatches(df$amenities, gregexpr('",\\s*"', df$amenities))) + 1
df$n_amenities[which(df$amenities == "[]")] <- 0
df$has_license <- !is.na(df$license)
# miles from LAX (1 degree of latitude = 69 miles, 1 degree of longitude = 57.5 miles in LA)
df$dist_lax_mi <- sqrt(((df$latitude - lax_lat) * 69)^2 + ((df$longitude - lax_lon) * 57.5)^2)

# Step 8: fill the few missing sizes with random existing values (hot deck), delete the rest
for (col in c("bedrooms", "beds", "bathrooms")) {
  miss <- is.na(df[[col]])
  cat("Step 8: filling", sum(miss), "missing values in", col, "\n")
  df[[col]][miss] <- sample(df[[col]][!miss], sum(miss), replace = TRUE)
}
drop_cols <- c("amenities", "license", "bathrooms_text")
df <- df[, !(names(df) %in% drop_cols)]
n <- nrow(df)
df <- na.omit(df)
cat("Step 8: removed", n - nrow(df), "rows with other missing values;", nrow(df), "left\n")

# Step 9: keep the biggest neighborhoods and property types, call the rest "Other"
top_groups <- function(x, n) {
  common <- names(sort(table(x), decreasing = TRUE))[1:n]
  ifelse(x %in% common, x, "Other")
}
df$neighborhood_grp <- top_groups(df$neighbourhood_cleansed, TOP_NEIGHBORHOODS)
df$property_type_grp <- top_groups(df$property_type, TOP_PROPERTY_TYPES)
df <- df[, !(names(df) %in% c("neighbourhood_cleansed", "property_type"))]

## Part 4: Save and check
dir.create("Data/clean", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "Data/clean/airbnb_analytic.csv", row.names = FALSE)
dict <- data.frame(variable = names(df), type = sapply(df, class),
                   n_distinct = sapply(df, function(x) length(unique(x))))
write.csv(dict, "Data/clean/data_dictionary_draft.csv", row.names = FALSE)

cat("Final analytic file:", nrow(df), "rows x", ncol(df), "columns\n")
