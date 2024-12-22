# Load necessary data
load("Bond_all.RData")

# Libraries
library(dplyr)       # For data manipulation
library(lubridate)   # For date handling
library(xts)         # For working with time-series data
library(tibble)      # For tibble manipulation (e.g., adding rownames)

# Fix column names for consistency across datasets
colnames(Bonds_prices_1Y) <- gsub(" 1Y", "", colnames(Bonds_prices_1Y))  # Remove "1Y" from column names
colnames(Bonds_prices_3M) <- gsub(" 3M", "", colnames(Bonds_prices_3M))  # Remove "3M" from column names
colnames(Exchange_rates) <- gsub(" Curncy", "", colnames(Exchange_rates))  # Remove "Curncy" from column names

# Ensure the "date" column is properly formatted as Date in all datasets
Bonds_prices_1Y <- Bonds_prices_1Y %>%
  rename(Date = date) %>%  # Rename "date" column to "Date"
  mutate(Date = as.Date(Date))  # Convert to Date type

Bonds_prices_3M <- Bonds_prices_3M %>%
  rename(Date = date) %>%
  mutate(Date = as.Date(Date))

Exchange_rates <- Exchange_rates %>%
  rename(Date = date) %>%
  mutate(Date = as.Date(Date))

# Deduplicate the tickers by removing redundant labels
unique_B_1Y_Tickers <- unique(gsub(" 1Y", "", B_1Y_Tickers))
unique_B_3M_Tickers <- unique(gsub(" 3M", "", B_3M_Tickers))

# Map FX tickers to bond tickers (e.g., "AUDUSD" -> "AUSTRALIA")
fx_mapping <- c(
  "AUDUSD" = "AUSTRALIA",
  "CADUSD" = "CANADA",
  "EURUSD" = "GERMANY",
  "JPYUSD" = "JAPAN",
  "NZDUSD" = "NEW ZEALAND",
  "NOKUSD" = "NORWAY",
  "SEKUSD" = "SWEDEN",
  "CHFUSD" = "SWITZERLAND",
  "GBPUSD" = "BRITIAN",
  "USDUSD" = "UNITED STATES"
)

# Function to calculate bond excess returns
# This implements Equation (6) for excess returns
calculate_excess_returns <- function(long_bond_price_t, short_bond_price_t, fx_rate_t, long_bond_price_t1, fx_rate_t1) {
  # Calculate excess return using the formula
  excess_return <- ((long_bond_price_t1 / long_bond_price_t) - 1) -
    (1 / short_bond_price_t) * (fx_rate_t1 / fx_rate_t)
  return(excess_return)
}

# Initialize a DataFrame to store excess returns
excess_returns_df <- data.frame(Date = Bonds_prices_1Y$Date)

# Loop through unique tickers to calculate excess returns for each country
for (ticker in unique_B_1Y_Tickers) {
  # Extract long-term bond price, short-term bond price, and FX rate for the current ticker
  long_bond_col <- Bonds_prices_1Y[[ticker]]
  short_bond_col <- Bonds_prices_3M[[ticker]]
  fx_ticker <- names(fx_mapping[fx_mapping == ticker])  # Find the FX ticker matching the country
  fx_rate_col <- Exchange_rates[[fx_ticker]]
  
  # Ensure all columns are valid and have the same length
  if (is.null(long_bond_col) || is.null(short_bond_col) || is.null(fx_rate_col)) {
    warning(paste("Missing data for ticker:", ticker))
    next
  }
  
  if (!(length(long_bond_col) == length(short_bond_col) && length(short_bond_col) == length(fx_rate_col))) {
    warning(paste("Mismatched lengths for ticker:", ticker))
    next
  }
  
  # Calculate excess returns using the provided formula
  excess_returns <- calculate_excess_returns(
    long_bond_price_t = long_bond_col[-length(long_bond_col)],       # Current long bond price
    long_bond_price_t1 = long_bond_col[-1],                         # Next month's long bond price
    short_bond_price_t = short_bond_col[-length(short_bond_col)],   # Current short bond price
    fx_rate_t = fx_rate_col[-length(fx_rate_col)],                  # Current exchange rate
    fx_rate_t1 = fx_rate_col[-1]                                    # Next month's exchange rate
  )
  
  # Align lengths by appending NA for the last row
  excess_returns <- c(excess_returns, NA)
  
  # Add calculated excess returns to the DataFrame
  excess_returns_df[[ticker]] <- excess_returns
}

# Check the results of excess return calculations
print(head(excess_returns_df))

# Load ggplot2 and tidyr libraries for visualization
library(ggplot2)
library(tidyr)

# Reshape the excess returns data to long format for plotting
excess_returns_long <- excess_returns_df %>%
  pivot_longer(cols = -Date, names_to = "Country", values_to = "ExcessReturn") %>%
  drop_na()  # Remove rows with NA values for clean plots

# Create subplots for each country showing excess returns over time
ggplot(excess_returns_long, aes(x = Date, y = ExcessReturn)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y") +
  labs(
    title = "Excess Returns for Bonds by Country",
    x = "Date",
    y = "Excess Return"
  )

# Function to calculate log excess returns
# This accounts for potential invalid data (e.g., negative or zero values)
calculate_log_excess_returns <- function(long_bond_price_t, short_bond_price_t, fx_rate_t, long_bond_price_t1, fx_rate_t1) {
  # Identify valid data points (greater than 0 for all inputs)
  valid <- long_bond_price_t > 0 & short_bond_price_t > 0 & fx_rate_t > 0 & 
    long_bond_price_t1 > 0 & fx_rate_t1 > 0
  
  # Replace invalid values with NA
  if (any(!valid)) {
    long_bond_price_t[!valid] <- NA
    short_bond_price_t[!valid] <- NA
    fx_rate_t[!valid] <- NA
    long_bond_price_t1[!valid] <- NA
    fx_rate_t1[!valid] <- NA
  }
  
  # Calculate log excess return using the formula
  excess_return <- log(long_bond_price_t1 / long_bond_price_t) - 
    log(short_bond_price_t) - 
    log(fx_rate_t1 / fx_rate_t)
  return(excess_return)
}

# Re-initialize a DataFrame for log excess returns
log_excess_returns_df <- data.frame(Date = Bonds_prices_1Y$Date)

# Loop through unique tickers to calculate log excess returns
for (ticker in unique_B_1Y_Tickers) {
  # Extract data for the current ticker
  long_bond_col <- Bonds_prices_1Y[[ticker]]
  short_bond_col <- Bonds_prices_3M[[ticker]]
  fx_ticker <- names(fx_mapping[fx_mapping == ticker])
  fx_rate_col <- Exchange_rates[[fx_ticker]]
  
  # Ensure all columns are valid and have the same length
  if (is.null(long_bond_col) || is.null(short_bond_col) || is.null(fx_rate_col)) {
    warning(paste("Missing data for ticker:", ticker))
    next
  }
  
  # Calculate log excess returns
  log_excess_returns <- calculate_log_excess_returns(
    long_bond_price_t = long_bond_col[-length(long_bond_col)],
    long_bond_price_t1 = long_bond_col[-1],
    short_bond_price_t = short_bond_col[-length(short_bond_col)],
    fx_rate_t = fx_rate_col[-length(fx_rate_col)],
    fx_rate_t1 = fx_rate_col[-1]
  )
  
  # Align lengths by appending NA for the last row
  log_excess_returns_df[[ticker]] <- c(log_excess_returns, NA)
}

# Check the summary of log excess returns
summary(log_excess_returns_df)

# Reshape the log excess returns data for plotting
log_excess_returns_long <- log_excess_returns_df %>%
  pivot_longer(cols = -Date, names_to = "Country", values_to = "LogExcessReturn") %>%
  drop_na()  # Remove rows with NA values

# Create subplots for each country showing log excess returns over time
ggplot(log_excess_returns_long, aes(x = Date, y = LogExcessReturn)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y") +
  labs(
    title = "Log Excess Returns for Bonds by Country",
    x = "Date",
    y = "Log Excess Return"
  )
