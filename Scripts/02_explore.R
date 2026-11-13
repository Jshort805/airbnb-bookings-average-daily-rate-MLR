# 02_explore.R -- Summaries and charts for the cleaned Airbnb Los Angeles data
# Input : Data/clean/airbnb_analytic.csv  (made by 01_clean.R)
# Output: Outputs/summary_numeric.csv, Outputs/summary_categorical.csv,
#         Outputs/fig1 to fig6 (.png charts)
# Run from the project root:  Rscript Scripts/02_explore.R

## Part 1: Read the cleaned data
df <- read.csv("Data/clean/airbnb_analytic.csv")
dir.create("Outputs", showWarnings = FALSE)
n <- nrow(df)
cat("Cleaned file:", n, "rows x", ncol(df), "columns\n")
df$log_price <- log(df$price)

## Part 2: Descriptive summaries
# Numeric variables: sample size, missing count, mean, median, sd, range
num_vars <- c("price", "log_price", "accommodates", "bedrooms", "beds", "bathrooms",
              "n_amenities", "minimum_nights", "dist_lax_mi",
              "hosts_time_as_host_years", "calculated_host_listings_count")
summary_numeric <- data.frame(
  variable = num_vars,
  n = sapply(df[num_vars], function(x) sum(!is.na(x))),
  n_missing = sapply(df[num_vars], function(x) sum(is.na(x))),
  mean = sapply(df[num_vars], mean),
  median = sapply(df[num_vars], median),
  sd = sapply(df[num_vars], sd),
  min = sapply(df[num_vars], min),
  max = sapply(df[num_vars], max),
  row.names = NULL
)
summary_numeric[, -1] <- round(summary_numeric[, -1], 2)
write.csv(summary_numeric, "Outputs/summary_numeric.csv", row.names = FALSE)
print(summary_numeric)

# Categorical variables: counts and percent of all listings
cat_vars <- c("room_type", "property_type_grp", "neighborhood_grp",
              "host_is_superhost", "host_identity_verified", "shared_bath", "has_license")
summary_categorical <- NULL
for (v in cat_vars) {
  counts <- table(df[[v]])
  summary_categorical <- rbind(summary_categorical,
    data.frame(variable = v, level = names(counts), n = as.vector(counts),
               percent = round(as.vector(counts) / n * 100, 1)))
}
write.csv(summary_categorical, "Outputs/summary_categorical.csv", row.names = FALSE)
print(summary_categorical)

# Numbers to quote in the report captions
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
cat("Skewness of price:", round(skew(df$price), 2),
    "; of log(price):", round(skew(df$log_price), 2), "\n")
cat("Median price by room type:\n")
print(tapply(df$price, df$room_type, median))
cor_vars <- setdiff(num_vars, c("price", "log_price"))
cors <- sort(sapply(df[cor_vars], function(x) cor(x, df$log_price)))
cat("Correlation with log(price):\n")
print(round(cors, 2))


# Data issue to explain in the report: many listings are long-stay (monthly) rentals
long_stay <- df$minimum_nights >= 30
cat("Listings with a 30+ night minimum:", round(mean(long_stay) * 100, 1), "%; median price",
    median(df$price[long_stay]), "vs", median(df$price[!long_stay]), "for shorter stays\n")
# Takeaway: 44% of listings require 30+ nights and their median asking price is about half
# ($132 vs $268). minimum_nights is a real pricing signal (correlation -0.30) and the team
# should decide whether to keep long stays, or run a sensitivity check without them.
## Part 3: Charts (each saved as a PNG in Outputs/)
blue <- "steelblue"
faint <- rgb(0, 0, 0, 0.08)  # see-through points so overlapping dots stay readable

# Fig 1: outcome distribution, raw and logged
# What it shows: price is strongly right-skewed (mean $255 vs median $202, skewness 1.44).
#   A few expensive listings stretch the right tail. After taking logs the shape is much more
#   symmetric (skewness -0.14).
# Why it matters: a log-price outcome fits MLR assumptions better (more even residuals), so
#   model log(price) and report effects as approximate percent changes.
png("Outputs/fig1_price_distribution.png", width = 1400, height = 600, res = 130)
par(mfrow = c(1, 2))
hist(df$price, breaks = 40, col = blue, border = "white",
     main = paste0("Nightly price (n = ", n, ")"), xlab = "Price per night (USD)")
hist(df$log_price, breaks = 40, col = blue, border = "white",
     main = paste0("Log of nightly price (n = ", n, ")"), xlab = "log(price per night)")
invisible(dev.off())

# Fig 2: group comparison, price by room type
# What it shows: median price is $250 for entire homes (73% of listings) and $272 for hotel
#   rooms (1%), versus $86 for private rooms (25%) and $36 for shared rooms (0.8%). Boxes
#   overlap little between private rooms and entire homes. The y-axis is log scale.
# Why it matters: room type separates prices clearly, so it belongs in the model. Hotel and
#   shared rooms are tiny groups, so their estimates will be uncertain.
png("Outputs/fig2_price_by_room_type.png", width = 1000, height = 700, res = 130)
boxplot(price ~ room_type, data = df, log = "y", col = blue, las = 1,
        main = paste0("Price by room type (n = ", n, ")"),
        xlab = "Room type", ylab = "Price per night (USD, log scale)")
invisible(dev.off())

# Fig 3: relationship, guests accommodated vs log price
# What it shows: price rises with the number of guests (correlation 0.64 with log price) but
#   flattens above about 8 guests. At any group size prices vary a lot, which is why the
#   other variables are still needed. Points are jittered and see-through to limit overplotting.
# Why it matters: accommodates is the strongest single predictor, but it overlaps with bedrooms
#   and beds (all measure size), so check VIF in 04_diagnostics.R. The curve bends, so a
#   transformation of accommodates may be worth testing. This is association, not causation.
png("Outputs/fig3_accommodates_vs_price.png", width = 1000, height = 700, res = 130)
plot(jitter(df$accommodates, amount = 0.3), df$log_price, pch = 16, col = faint,
     main = paste0("Guests accommodated vs price (n = ", n, ")"),
     xlab = "Guests accommodated", ylab = "log(price per night)")
lines(lowess(df$accommodates, df$log_price), col = "red", lwd = 2)
invisible(dev.off())

# Fig 4: group comparison, median price by neighborhood group
# What it shows: median price ranges from about $293 in Beverly Hills and $294 in Santa Monica
#   (also West Hollywood $285, Culver City $276, Venice $275) down to $127 in Mid-City and $46
#   in Exposition Park. The "Other" group is 22,445 listings (65%) with a median of $189.
# Why it matters: location clearly matters, so keep the neighborhood group in the model. Because
#   65% of listings fall in "Other", consider raising TOP_NEIGHBORHOODS in 01_clean.R, and check
#   the odd Exposition Park value before relying on it.
med <- sort(tapply(df$price, df$neighborhood_grp, median))
counts <- as.vector(table(df$neighborhood_grp)[names(med)])
png("Outputs/fig4_price_by_neighborhood.png", width = 1100, height = 800, res = 130)
par(mar = c(5, 14, 4, 2))
barplot(med, horiz = TRUE, las = 1, col = blue,
        names.arg = paste0(names(med), " (n=", counts, ")"),
        main = "Median price by neighborhood group",
        xlab = "Median price per night (USD)")
invisible(dev.off())

# Fig 5: relationship, distance from LAX vs log price
# What it shows: only a slight downward slope (correlation -0.05): listings farther from LAX
#   are a little cheaper on average, with wide spread at every distance. The vertical band at
#   about 41 miles is a group of listings at nearly the same distance.
# Why it matters: distance to LAX has little explanatory power on its own. It is a weak stand-in
#   for the airline data idea, and neighborhood probably captures most of the location effect.
png("Outputs/fig5_lax_distance_vs_price.png", width = 1000, height = 700, res = 130)
plot(df$dist_lax_mi, df$log_price, pch = 16, col = faint,
     main = paste0("Distance from LAX vs price (n = ", n, ")"),
     xlab = "Distance from LAX (miles)", ylab = "log(price per night)")
lines(lowess(df$dist_lax_mi, df$log_price), col = "red", lwd = 2)
invisible(dev.off())

# Fig 6: which numeric variables move with price
# What it shows: size variables are most strongly tied to price: accommodates 0.64, bedrooms
#   0.57, beds 0.52, bathrooms 0.42, amenities 0.36. Minimum nights (-0.30) and the host's number
#   of listings (-0.21) go the other way. Distance to LAX (-0.05) and years as a host (-0.02) are
#   near zero.
# Why it matters: start the model with the size variables, minimum nights and amenities. The size
#   variables are correlated with each other, so expect to drop one or two after the VIF check.
#   These are descriptive associations, not causal effects (prices are the host's asking price).
png("Outputs/fig6_correlation_with_price.png", width = 1100, height = 700, res = 130)
par(mar = c(5, 14, 4, 2))
barplot(cors, horiz = TRUE, las = 1, col = blue,
        main = "Correlation of each numeric variable with log(price)",
        xlab = "Correlation coefficient")
invisible(dev.off())

cat("Saved 6 charts and 2 summary tables to Outputs/\n")
