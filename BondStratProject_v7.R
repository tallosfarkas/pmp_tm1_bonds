load("BBG_BondTicker.RData")

install.packages("corrplot", dependencies = TRUE)
install.packages("gt", dependencies = TRUE)

library(openxlsx)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(rlang)
library(gridExtra)
library(corrplot)
library(lubridate)
library(gt)
library(RColorBrewer)





# connect to BBG (Terminal must be open)
library(Rblpapi)
blpConnect()

# Only look at the G10 Countries:
unique(DT$country)

G10 <- c("AUSTRALIA","CANADA","GERMANY","JAPAN","NEW ZEALAND","NORWAY","SWEDEN","SWITZERLAND","BRITAIN","UNITED STATES")

Tickers <- DT$ticker[DT$country %in% G10]

Tickers_9Y <- c("I00109Y Index", "I00709Y Index",
                "I01609Y Index", "I01809Y Index",
                "I04909Y Index", "I07809Y Index",
                "I02109Y Index", "I08209Y Index",
                "I02209Y Index", "I02509Y Index")

# Extracting Bond Yields

Tickers <- Tickers[grepl("3M|10Y",Tickers)]

Tickers_3m_10Y_9Y <- c(Tickers, Tickers_9Y)


opt <- c("periodicitySelection"="DAILY")
Gov_Bonds <- bdh(Tickers_3m_10Y_9Y, c("PX_LAST"),
                 as.Date("1995-01-01"), as.Date("2023-01-01"), options=opt)


exchange_tickers <- c("AUDUSD Curncy", # Australia
                      "CADUSD Curncy", # Canada
                      "EURUSD Curncy", # Germany (Eurozone)
                      "JPYUSD Curncy", # Japan
                      "NZDUSD Curncy", # New Zealand
                      "NOKUSD Curncy", # Norway
                      "SEKUSD Curncy", # Sweden
                      "CHFUSD Curncy", # Switzerland
                      "GBPUSD Curncy" # Britain
) 

Exchange_rate <- bdh(exchange_tickers, c("PX_LAST"),
                     as.Date("1995-01-01"), as.Date("2023-01-01"), options=opt)

##################################################################################

# Create a new workbook
wb <- createWorkbook()

# Add data for Exchange Rates
addWorksheet(wb, "Exchange_Rates")
writeData(wb, "Exchange_Rates", Exchange_rate)

# Add data for Government Bonds
addWorksheet(wb, "Government_Bonds")
writeData(wb, "Government_Bonds", Gov_Bonds)

# Save the Excel file
saveWorkbook(wb, "Exchange_and_Bonds_Data.xlsx", overwrite = TRUE)

save(Gov_Bonds, Exchange_rate, Tickers, file="Bond_Project_v2.RData")

str(Gov_Bonds)
str(Exchange_rate)

# For Exchange Rates (e.g., first ticker)
diff_dates_exchange <- diff(as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]]))
table(diff_dates_exchange)

# For Government Bonds (e.g., first ticker)
diff_dates_bonds <- diff(as.Date(Gov_Bonds[["I02203M Index"]][["date"]]))
table(diff_dates_bonds)


# Expected daily sequence
start_date <- min(as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]]))
end_date <- max(as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]]))
expected_dates <- seq.Date(start_date, end_date, by = "day")

# Check for missing dates
actual_dates <- as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]])
missing_dates <- setdiff(expected_dates, actual_dates)

missing_dates


# Range of dates
range(as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]]))
range(as.Date(Gov_Bonds[["I02203M Index"]][["date"]]))

# Plot all Government Bonds
par(mfrow = c(2, 2)) # Arrange plots in a grid
for (ticker in names(Gov_Bonds)[1:4]) { # Plot first 4 as an example
  plot(as.Date(Gov_Bonds[[ticker]]$date),
       Gov_Bonds[[ticker]]$PX_LAST,
       type = "l",
       main = paste("Price Over Time:", ticker),
       xlab = "Date",
       ylab = "Price")
}

# Plot all Exchange Rates
par(mfrow = c(2, 2)) # Arrange plots in a grid
for (ticker in names(Exchange_rate)[1:4]) { # Plot first 4 as an example
  plot(as.Date(Exchange_rate[[ticker]]$date),
       Exchange_rate[[ticker]]$PX_LAST,
       type = "l",
       main = paste("Exchange Rate:", ticker),
       xlab = "Date",
       ylab = "Price")
}

# Government Bonds: Check date ranges
for (ticker in names(Gov_Bonds)) {
  cat("\nTicker:", ticker)
  cat("\nDate Range:", range(as.Date(Gov_Bonds[[ticker]]$date)), "\n")
}

# Exchange Rates: Check date ranges
for (ticker in names(Exchange_rate)) {
  cat("\nTicker:", ticker)
  cat("\nDate Range:", range(as.Date(Exchange_rate[[ticker]]$date)), "\n")
}

# Loop through all tickers in Exchange_rate
for (ticker in names(Exchange_rate)) {
  # Extract dates
  dates <- as.Date(Exchange_rate[[ticker]]$date)
  
  # Create expected daily date sequence
  expected_dates <- seq.Date(min(dates), max(dates), by = "day")
  
  # Check for missing dates
  missing_dates <- setdiff(expected_dates, dates)
  
  # Print results for the ticker
  cat("\nTicker:", ticker)
  cat("\nMissing Dates:", length(missing_dates), "\n")
  print(missing_dates)
}

# Plot all exchange rate data
par(mfrow = c(3, 3)) # Arrange plots in a grid (3x3 for 9 currencies)

for (ticker in names(Exchange_rate)) {
  plot(as.Date(Exchange_rate[[ticker]]$date),
       Exchange_rate[[ticker]]$PX_LAST,
       type = "l",
       col = "blue",
       main = paste("Exchange Rate:", ticker),
       xlab = "Date",
       ylab = "Price")
}

# Reset plotting layout
par(mfrow = c(1, 1))
# Plot all government bond data
par(mfrow = c(4, 5)) # Arrange plots in a grid (4x5 for 20 bonds)

for (ticker in names(Gov_Bonds)) {
  plot(as.Date(Gov_Bonds[[ticker]]$date),
       Gov_Bonds[[ticker]]$PX_LAST,
       type = "l",
       col = "red",
       main = paste("Gov Bond:", ticker),
       xlab = "Date",
       ylab = "Price")
}

# Reset plotting layout
par(mfrow = c(1, 1))



#Data Preparation
################################################################################

#Setting the names from the Bloomberg Output
Countries <- c("AUSTRALIA","CANADA","GERMANY","JAPAN","SWEDEN","BRITAIN","UNITED STATES","NEW ZEALAND","NORWAY","SWITZERLAND")
Bond_names_3M <- rep("3M",10)
Bond_names_9Y <-rep("9Y",10)
Bond_names_10Y <-rep("10Y",10)
Bond_names_3M <- paste(Countries,Bond_names_3M)
Bond_names_9Y <- paste(Countries,Bond_names_9Y)
Bond_names_10Y <- paste(Countries,Bond_names_10Y)
FX_names <- c("AUDUSD Curncy",
              "CADUSD Curncy",
              "EURUSD Curncy",
              "JPYUSD Curncy",
              "SEKUSD Curncy",
              "GBPUSD Curncy",
              "USDUSD Curncy",
              "NZDUSD Curncy",
              "NOKUSD Curncy",
              "CHFUSD Curncy")

#Create the Ticker sequence for the Loop

Ticker_seq_extra <- seq(from = 1, to = 28, by = 3)
Ticker_seq_extra
Tickers_extra <- sort(names(Gov_Bonds))
Tickers_extra
FX_Tickers_extra <- c(rep(FX_names,each = 3))
length(FX_Tickers)
B_3M_Tickers <- c(rep(Bond_names_3M,each = 3))
typeof(B_3M_Tickers)
B_9Y_Tickers <- c(rep(Bond_names_9Y,each = 3))
typeof(B_9Y_Tickers)
B_10Y_Tickers <- c(rep(Bond_names_10Y,each = 3))
typeof(B_10Y_Tickers)


#Setting the time frame
#United States: largest data range
time_frame_raw <- Gov_Bonds[[Tickers_extra[13]]][1]
time_frame <- as.Date(time_frame_raw[[1]])
time_frame
length(time_frame)
length(unique(time_frame))

#Add USD currency list to exchange rate
USDUSD_Curncy <- data.frame(date = time_frame, PX_LAST = rep(1, length(time_frame)))
exchange_rate_complete_old <- append(Exchange_rate, list(USDUSD_Curncy))
names(exchange_rate_complete_old)[length(exchange_rate_complete_old)] <- "USDUSD Curncy"

exchange_rate_complete <- exchange_rate_complete_old[FX_names]
names(exchange_rate_complete)

#Create data frames for end result:
Bonds_yields_3M <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Bonds_yields_3M) <- c(Bond_names_3M)
Bonds_yields_9Y <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Bonds_yields_9Y) <-c(Bond_names_9Y)
Bonds_yields_10Y <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Bonds_yields_10Y) <-c(Bond_names_10Y)
Exchange_rates <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Exchange_rates) <- c(FX_names)
rownames(Bonds_yields_3M) <- time_frame
rownames(Bonds_yields_9Y) <- time_frame
rownames(Bonds_yields_10Y) <- time_frame
rownames(Exchange_rates) <- time_frame


#Setting the same time frame for every data and interpolate missing values
for(i in Ticker_seq_extra){
  #Data for each Country
  df1 <- data.frame(date = as.Date(Gov_Bonds[[Tickers_extra[i]]][[1]]),data = Gov_Bonds[[Tickers_extra[i]]][[2]])
  df2 <- data.frame(date = as.Date(Gov_Bonds[[Tickers_extra[i+1]]][[1]]),data = Gov_Bonds[[Tickers_extra[i+1]]][[2]])
  df3 <- data.frame(date = as.Date(Gov_Bonds[[Tickers_extra[i+2]]][[1]]),data = Gov_Bonds[[Tickers_extra[i+2]]][[2]])
  df4 <- data.frame(date = as.Date(exchange_rate_complete[[FX_Tickers_extra[i]]][[1]]),data = exchange_rate_complete[[FX_Tickers_extra[i]]][[2]])
  
  zoo_df1 <- zoo(df1$data, order.by = df1$date)
  zoo_df2 <- zoo(df2$data, order.by = df2$date)
  zoo_df3 <- zoo(df3$data, order.by = df3$date)
  zoo_df4 <- zoo(df4$data, order.by = df4$date)
  
  # Reindex each zoo object to match the target date vector and set missing values to NA
  zoo_reindexed_1 <- merge(zoo_df1, zoo(, time_frame), all = TRUE)
  zoo_reindexed_2 <- merge(zoo_df2, zoo(, time_frame), all = TRUE)
  zoo_reindexed_3 <- merge(zoo_df3, zoo(, time_frame), all = TRUE)
  zoo_reindexed_4 <- merge(zoo_df4, zoo(, time_frame), all = TRUE)
  
  zoo_interpolated_1 <- na.approx(zoo_reindexed_1, xout = time_frame, na.rm = FALSE)
  zoo_interpolated_2 <- na.approx(zoo_reindexed_2, xout = time_frame, na.rm = FALSE)
  zoo_interpolated_3 <- na.approx(zoo_reindexed_3, xout = time_frame, na.rm = FALSE)
  zoo_interpolated_4 <- na.approx(zoo_reindexed_4, xout = time_frame, na.rm = FALSE)
  
  # Perform linear interpolation for missing values for each sequence
  Bonds_yields_3M[[B_3M_Tickers[i]]] <- coredata(zoo_interpolated_1)
  Bonds_yields_9Y[[B_9Y_Tickers[i]]] <- coredata(zoo_interpolated_2)
  Bonds_yields_10Y[[B_10Y_Tickers[i]]] <- coredata(zoo_interpolated_3)
  Exchange_rates[[FX_Tickers_extra[i]]] <- coredata(zoo_interpolated_4)
}

#Create synthetic prices from Yields and add date column

Bonds_prices_3M <- 1 / ((1+ Bonds_yields_3M/100))^(1/4) 

Bonds_prices_3M <- Bonds_prices_3M   %>% 
  mutate( 
    date=time_frame
  )

Bonds_prices_10Y <-1 / ((1+ Bonds_yields_10Y/100))^(10)

Bonds_prices_10Y <- Bonds_prices_10Y   %>% 
  mutate( 
    date=time_frame
  )

Bonds_prices_9Y <- 1 / ((1+ Bonds_yields_9Y/100))^(9)

Bonds_prices_9Y <- Bonds_prices_9Y  %>% 
  mutate( 
    date=time_frame
  )

Bonds_prices_10Y3M <- 1 / ( 1 + (Bonds_yields_9Y/100 + (Bonds_yields_10Y/100 - Bonds_yields_9Y/100) * (1 - 1/12)))^(9 + 11/12)

Bonds_prices_10Y3M <- Bonds_prices_10Y3M   %>% 
  mutate( 
    date=time_frame
  )

#Add a date column to the Exchange Rates
Exchange_rates <- Exchange_rates   %>% 
  mutate( 
    date=time_frame
  )


#Plotting and Observing the Data
###############################################################################


##Plots with corrected values:
plot_list_3M <- list()
plot_list_10Y <- list()
plot_list_FX <- list()

for (i in 1:10) {
  column_name_3M <- Bond_names_3M[i]
  column_name_10Y <- Bond_names_10Y[i]
  column_name_FX <- FX_names[i]
  plot_list_3M[[i]] <- ggplot(Bonds_prices_3M, aes(x = date, y = !!sym(column_name_3M))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Price", title = paste( column_name_3M))+
    theme(
      axis.title.x = element_text(size = 5),  
      axis.title.y = element_text(size = 5),  
      axis.text.x = element_text(size = 4),    
      axis.text.y = element_text(size = 4),    
      plot.title = element_text(size = 7)     
    )
  plot_list_10Y[[i]] <- ggplot(Bonds_prices_10Y, aes(x = date, y = !!sym(column_name_10Y))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Price", title = paste( column_name_10Y))+
    theme(
      axis.title.x = element_text(size = 5),  
      axis.title.y = element_text(size = 5),  
      axis.text.x = element_text(size = 4),    
      axis.text.y = element_text(size = 4),    
      plot.title = element_text(size = 7)     
    )
  plot_list_FX[[i]] <- ggplot(Exchange_rates, aes(x = date, y = !!sym(column_name_FX))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Price", title = paste( column_name_FX))+
    theme(
      axis.title.x = element_text(size = 5),  
      axis.title.y = element_text(size = 5),  
      axis.text.x = element_text(size = 4),    
      axis.text.y = element_text(size = 4),    
      plot.title = element_text(size = 7)     
    )
}
grid.arrange(grobs = plot_list_3M, ncol = 2)
grid.arrange(grobs = plot_list_10Y, ncol = 2)
grid.arrange(grobs = plot_list_FX, ncol = 5)


##Correlation Matrix:

correlation_matrix_3M <- cor(Bonds_prices_3M[,1:10])
correlation_matrix_10Y <- cor(Bonds_prices_10Y[,1:10])

par(mar = c(1, 5, 2, 2))
corrplot(correlation_matrix_3M, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         tl.cex = 0.7, # Adjust font size of text labels
         cl.cex = 0.6, # Adjust font size of color legend
         number.cex = 0.6) # Adjust font size of correlation coefficients
title("Correlation Matrix of 3M Bond Prices", cex.main = 1)

corrplot(correlation_matrix_10Y, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         tl.cex = 0.7, # Adjust font size of text labels
         cl.cex = 0.6, # Adjust font size of color legend
         number.cex = 0.6) # Adjust font size of correlation coefficients

title("Correlation Matrix of 10Y Bond Prices", cex.main = 1)

#Saving the data:

save(Bonds_prices_3M, Bonds_prices_10Y,Exchange_rates,B_10Y_Tickers,B_3M_Tickers,FX_names,Tickers, file = "Bond_PX_FX.RData")
save(plot_list_3M,plot_list_10Y,plot_list_FX, file ="Bond_PX_FX_Plots.RData")

# Creating the Bond Excess Returns
################################################################################

# "We calculate time series of excess returns as defined in Equation (6) at monthly frequency,
# using data from the last day of each month"

## SORT THE FX NAMES AND COLUMNS TO MATCH THE BONDS

# 1. Filter the last day of each month from the data

eomonth_data <- function(data){
  data %>%
    mutate(date = as.Date(date)) %>%            
    group_by(month = floor_date(date, "month")) %>% 
    filter(date == max(date)) %>%               
    ungroup() %>%
    select(-month)   
}


Bonds_3M_eom <- eomonth_data(Bonds_prices_3M)
Bonds_10Y_eom <- eomonth_data(Bonds_prices_10Y)
Bonds_10Y3M_eom <- eomonth_data(Bonds_prices_10Y3M)
FX_eom <- eomonth_data(Exchange_rates)

colnames(Bonds_3M_eom) <- c(Countries,"date")
colnames(Bonds_10Y_eom) <- c(Countries,"date")
colnames(Bonds_10Y3M_eom) <- c(Countries,"date")
colnames(FX_eom) <- c(Countries,"date")


#2. Reshape data into long format and merge by data and country

Bonds_3M_eom_long <- Bonds_3M_eom %>% pivot_longer(-date, names_to = "Country", values_to = "P_3M")
Bonds_10Y_eom_long <- Bonds_10Y_eom %>% pivot_longer(-date, names_to = "Country", values_to = "P_10Y")
Bonds_10Y3M_eom_long <- Bonds_10Y3M_eom %>% pivot_longer(-date, names_to = "Country", values_to = "P_10Y3M")
FX_eom_long <- FX_eom %>% pivot_longer(-date, names_to = "Country", values_to = "FX")


#3. Merge

Merged_Bonds_FX <- Bonds_3M_eom_long %>%
  inner_join(Bonds_10Y_eom_long, by = c("date", "Country")) %>%
  inner_join(Bonds_10Y3M_eom_long, by = c("date", "Country")) %>%
  inner_join(FX_eom_long, by = c("date", "Country"))


#3. Lagging and Sorting by Country

Merged_Bonds_FX <- Merged_Bonds_FX %>%
  arrange(Country, date) %>%  # Sort by Country and Date
  group_by(Country) %>%       # Group by Country for lagging
  mutate(
    P_10Y_prev = lag(P_10Y), # 10-year bond price at t-1
    FX_prev = lag(FX),
    P_3M_prev = lag(P_3M)# Exchange rate at t-1
  ) %>%
  ungroup()

#4. 

# Isolate Returns from Total Excess Return

Merged_Bonds_FX <- Merged_Bonds_FX %>%
  mutate(
    excess_return = ((((P_10Y3M) / P_10Y_prev) - (1 / P_3M_prev)) * (FX / FX_prev))
  )

Bonds_ex_ret_long <- Merged_Bonds_FX %>%
  select(
    Country, date, excess_return
  )

Bonds_ex_ret_wide <- Bonds_ex_ret_long %>%
  pivot_wider(names_from = Country, values_from = excess_return)

annualized_returns <- Bonds_ex_ret_wide %>%
  mutate(across(-date, ~ exp(. * 12) - 1))

#Descriptive Statistics

col_Means <- colMeans(annualized_returns[,-1], na.rm = TRUE)*100
col_variance <- sapply(annualized_returns[-1,-1]*100, var)
col_Medians <-  sapply(annualized_returns[-1,-1]*100, median)
Sharpe_Ratio <- col_Means / sqrt(col_variance)

des_stats <- as.data.frame(cbind(col_Means,sqrt(col_variance),col_Medians,Sharpe_Ratio))

des_stats <- data.frame(Country = rownames(des_stats), des_stats)

colnames(des_stats) <- c("Country","Mean","Volatility","Median","Sharpe-Ratio")


des_stats %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics Bond Excess Returns"
  ) %>%
  fmt_number(
    columns = c("Mean","Volatility","Median","Sharpe-Ratio"),
decimals = 3
)

#5. Plot Excess Returns

plot_list_ex_ret<- list()

for (i in 1:10) {
  column_name <- Countries[i]
  plot_list_ex_ret[[i]] <- ggplot(annualized_returns, aes(x = date, y = !!sym(column_name))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Excess Return", title = paste(column_name))+
    theme(
      axis.title.x = element_text(size = 5),  
      axis.title.y = element_text(size = 5),  
      axis.text.x = element_text(size = 4),    
      axis.text.y = element_text(size = 4),    
      plot.title = element_text(size = 7) 
    )
}

grid.arrange(grobs = plot_list_ex_ret, ncol = 3)

correlation_matrix_returns <- cor(annualized_returns[-1,-1])

par(mar = c(1, 5, 2, 2))
corrplot(correlation_matrix_returns, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         tl.cex = 0.7, # Adjust font size of text labels
         cl.cex = 0.6, # Adjust font size of color legend
         number.cex = 0.6) # Adjust font size of correlation coefficients

########################################################################

Merged_Bonds_FX <- Merged_Bonds_FX %>%
  mutate(
    fwspread = ((((P_10Y3M) / P_10Y_prev) - (1 / P_3M_prev)))
  )



# Creating the portfolio weights and the forward spread
################################################################################

### Constructing the fwspread dataset
fwspread_country = read_excel("C:/Users/Student1.AzureAD/ZZ Vermögensverwaltung GmbH/ISK-Wien - General/ZZ Gruppe/2024/Personal/Tomas/tm1_bonds/Merged_Bonds_FX_p.xlsx")
fwspread_country_c = fwspread_country[-c(1:11),]
fwspread_country_c = fwspread_country_c[-c(1:60),]
Value_c = Value[-c(1:60),]
DT.betas_c = DT.betas[-c(1:59),]

### Constructing the fwspread dataset
beta_fsw = DT.betas_c$betaFS10Y
beta_val = DT.betas_c$betaval10Y

fwspread_country_c = fwspread_country_c[,c(1:10)]
Value_c = Value_c[,c(1:10)]


### Expected excess returns
e_e_ret = beta_fsw*fwspread_country_c + beta_val*Value_c


### Inverting the Variance covariance matrix
ewmaVar2_c <- ewmaVar2[names(ewmaVar2) >= "2000-12"]
# ewmaVar2_c_i <- lapply(ewmaVar2_c, function(mat) {
#   # Check if the matrix is invertible
#   if (is.matrix(mat) && det(mat) != 0) {
#     return(solve(mat))  # Invert the matrix
#   } else {
#     return(NULL)  # Return NULL for non-invertible matrices
#   }
# })

### Calculating portfolio weights
e_e_ret_m = as.matrix(e_e_ret)

compute_weights <- function(varcov_matrices, expected_excess_returns) {
  # Initialize a list to store weights for each t
  p_weights <- list()
  
  # Loop over each time step
  for (t in seq_along(varcov_matrices)) {
    # Extract the variance-covariance matrix for time t
    V_t <- varcov_matrices[[t]]
    
    # Extract the expected excess returns for time t
    E_rX_t <- expected_excess_returns[t, ]
    
    # Compute the numerator: V_t^(-1) * E_rX_t
    numerator <- solve(V_t) %*% E_rX_t
    
    # Compute the denominator: 1 + E_rX_t' * V_t^(-1) * E_rX_t
    denominator <- 1 + t(E_rX_t) %*% solve(V_t) %*% E_rX_t
    
    # Compute the weights for time t
    w_t <- numerator / as.numeric(denominator)
    
    # Store the weights for time t
    p_weights[[t]] <- w_t
  }
  
  return(p_weights)
}

ewmaVar2_c_good <- ewmaVar2_c[-length(ewmaVar2_c)]
umve_weights = compute_weights(ewmaVar2_c_good, e_e_ret_m)
umve_weights
sum(umve_weights[[1]])

### Descriptive statistics on UMVE weights
lapply(umve_weights, summary)

umve_weights_unlist = unlist(umve_weights)

mean_umve = mean(umve_weights_unlist)
median_umve = median(umve_weights_unlist)
std_umve = sd(umve_weights_unlist)
# plot the UMW weights
hist(umve_weights_unlist, breaks = 50, col = "blue", xlab = "UMVE Weights", main = "UMVE Weights Distribution")
plot(umve_weights_unlist, type = "l", col = "blue", xlab = "Time", ylab = "UMVE Weights", main = "UMVE Weights over Time")



# Load necessary libraries
library(ggplot2)
library(reshape2)

# Assuming the data `umve_weights` is a list where each element is a vector of weights for a given time period.

# Convert the list of weights into a data frame
weights_matrix <- matrix(unlist(umve_weights), ncol = 10, byrow = TRUE)
colnames(weights_matrix) <- c("AUSTRALIA", "BRITAIN", "CANADA", "GERMANY", "JAPAN", 
                          "NEW ZEALAND", "NORWAY", "SWEDEN", "SWITZERLAND", "UNITED STATES")

# Add a date column (assuming you have a sequence of dates corresponding to the weights)
weights_matrix <- as.data.frame(weights_matrix)
weights_matrix$Date <- seq.Date(from = as.Date("2000-12-29"), by = "month", length.out = nrow(weights_matrix))  # Adjust start date and frequency as needed

# Melt the data for ggplot
weights_melted <- melt(weights_matrix, id.vars = "Date", variable.name = "Country", value.name = "Weight")

# Create the stacked area plot
ggplot(weights_melted, aes(x = Date, y = Weight, fill = Country)) +
  geom_area(alpha = 0.8, size = 0.2, colour = "black") +
  scale_fill_manual(values = rainbow(length(unique(weights_melted$Country)))) +  # Assign distinct colors to each country
  labs(
    title = "Times Series of Portfolio Weights",
    x = "Date",
    y = "UMVE Portfolio Weights",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )



library(ggplot2)
library(RColorBrewer)

# Choose a softer color palette (e.g., "Set3")
palette_colors <- brewer.pal(n = length(unique(weights_melted$Country)), "Set3")

# Create the plot with the new palette
ggplot(weights_melted, aes(x = Date, y = Weight, fill = Country)) +
  geom_area(alpha = 0.8, size = 0.2, colour = "black") +
  scale_fill_manual(values = palette_colors) +  # Use the softer "Set3" palette
  labs(
    title = "Time Series of Portfolio Weights",
    x = "Date",
    y = "UMVE Portfolio Weights",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )





################## dynamic portfolio calculation ###################

# Function to compute portfolio returns
compute_portfolio_returns <- function(weights_list, excess_returns_matrix) {
  # Initialize a vector to store portfolio returns
  portfolio_returns <- numeric(length(weights_list))
  
  # Loop over each time t
  for (t in seq_along(weights_list)) {
    # Extract weights for time t
    w_t <- weights_list[[t]]
    
    # Extract excess returns for time t+1
    rX_t1 <- excess_returns_matrix[t + 1, -1]  # Exclude the date column
    
    # Ensure dimensions align
    if (length(w_t) != length(rX_t1)) {
      stop(sprintf("Dimension mismatch at t = %d: weights and returns", t))
    }
    
    # Compute the portfolio return
    portfolio_returns[t] <- sum(w_t * rX_t1)
  }
  
  return(portfolio_returns)
}

length(umve_weights)
dim(annualized_returns)
# remove the rows of the annualized returns that are before 2000-12-29
annualized_returns_aligned <- annualized_returns[annualized_returns$date >= as.Date("2000-12-29"), ]

UMVE_port <- compute_portfolio_returns(umve_weights, annualized_returns_aligned)



print(UMVE_port)
mean(UMVE_port)
plot(UMVE_port, type = "l", col = "blue", xlab = "Time", ylab = "Portfolio Return", main = "UMVE Portfolio Returns")
# cumsum returns plotted
plot(cumsum(UMVE_port), type = "l", col = "blue", xlab = "Time", ylab = "Cumulative Portfolio Return", main = "UMVE Cumulative Portfolio Returns")



################### Descriptive Statistics ####################




# Portfolio weights unlisted for analysis
umve_weights_unlist <- unlist(umve_weights)

# Descriptive statistics for weights
mean_weights <- mean(umve_weights_unlist) * 100  # Convert to %
median_weights <- median(umve_weights_unlist) * 100  # Convert to %
std_weights <- sd(umve_weights_unlist) * 100  # Convert to %
min_weights <- min(umve_weights_unlist) * 100
max_weights <- max(umve_weights_unlist) * 100
positive_weights <- sum(umve_weights_unlist > 0) / length(umve_weights_unlist) * 100  # Percentage of positive weights
negative_weights <- sum(umve_weights_unlist < 0) / length(umve_weights_unlist) * 100  # Percentage of negative weights
weight_turnover <- mean(abs(diff(umve_weights_unlist))) * 100  # Turnover: change in weights over time

# Load the stargazer package
library(stargazer)

# Create the weights_summary table
weights_summary <- data.frame(
  Metric = c("Mean (in %)", "Median (in %)", "Std. Dev. (in %)", 
             "Min (in %)", "Max (in %)", 
             "Positive Weights (in %)", "Negative Weights (in %)", 
             "Average Turnover (in %)"),
  Value = c(mean_weights, median_weights, std_weights, 
            min_weights, max_weights, 
            positive_weights, negative_weights, 
            weight_turnover)
)

# Format the table using stargazer
stargazer(weights_summary, type = "text", summary = FALSE, rownames = FALSE,
          title = "Descriptive Statistics for Portfolio Weights",
          digits = 2)




# Descriptive statistics for portfolio returns
mean_returns <- mean(UMVE_port) * 100  # Convert to %
median_returns <- median(UMVE_port) * 100  # Convert to %
std_returns <- sd(UMVE_port) * 100  # Convert to %
sharpe_ratio <- mean(UMVE_port) / sd(UMVE_port)  # Assuming risk-free rate is 0
max_drawdown <- max(cummax(cumsum(UMVE_port)) - cumsum(UMVE_port)) * 100  # Maximum drawdown

# Turnover for portfolio returns
portfolio_turnover <- mean(abs(diff(UMVE_port))) * 100  # Average turnover in returns

# Create the returns_summary table
returns_summary <- data.frame(
  Metric = c("Mean (in %)", "Median (in %)", "Std. Dev. (in %)", 
             "Sharpe Ratio", "Maximum Drawdown (in %)", 
             "Average Turnover (in %)"),
  Value = c(mean_returns, median_returns, std_returns, 
            sharpe_ratio, max_drawdown, portfolio_turnover)
)

# Format the table using stargazer
stargazer(returns_summary, type = "text", summary = FALSE, rownames = FALSE,
          title = "Descriptive Statistics for Portfolio Returns",
          digits = 2)












# Portfolio Weights Distribution
hist(umve_weights_unlist, breaks = 50, col = "blue", 
     xlab = "Portfolio Weights (in %)", main = "Distribution of Portfolio Weights")

# Generate the sequence of dates
dates <- seq.Date(from = as.Date("2001-01-01"), by = "month", length.out = length(UMVE_port))

# Portfolio Returns over Time
plot(dates, UMVE_port, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Portfolio Return (in %)", main = "UMVE Portfolio Returns Over Time",
     xaxt = "n")  # Suppress default x-axis
axis(1, at = seq(from = as.Date("2000-12-31"), to = as.Date("2022-12-31"), by = "2 years"), 
     labels = format(seq(from = as.Date("2000-12-31"), to = as.Date("2022-12-31"), by = "2 years"), "%Y"))
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")  # Add grid lines

# Cumulative Portfolio Returns
plot(dates, cumsum(UMVE_port), type = "l", col = "darkblue", lwd = 2,
     xlab = "Date", ylab = "Cumulative Return (in %)", main = "UMVE Cumulative Portfolio Returns",
     xaxt = "n")  # Suppress default x-axis
axis(1, at = seq(from = as.Date("2000-12-31"), to = as.Date("2022-12-31"), by = "2 years"), 
     labels = format(seq(from = as.Date("2000-12-31"), to = as.Date("2022-12-31"), by = "2 years"), "%Y"))
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")  # Add grid lines




library(ggplot2)


# Define the dimensions for Full HD (16:9 aspect ratio)
width <- 1920
height <- 1080

# Save the Time Series Plot of Portfolio Weights
png("Time_Series_Portfolio_Weights.png", width = width, height = height, res = 150)
ggplot(weights_melted, aes(x = Date, y = Weight, fill = Country)) +
  geom_area(alpha = 0.8, size = 0.2, colour = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(weights_melted$Country)), "Set3")) +
  labs(
    title = "Time Series of Portfolio Weights",
    x = "Date",
    y = "UMVE Portfolio Weights",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
dev.off()

# Save the Portfolio Weights Distribution Plot
png("Portfolio_Weights_Distribution.png", width = width, height = height, res = 150)
hist(umve_weights_unlist, breaks = 50, col = "blue", 
     xlab = "Portfolio Weights (in %)", main = "Distribution of Portfolio Weights")
dev.off()

# Define the time range for the x-axis
dates <- seq.Date(from = as.Date("2001-01-01"), to = as.Date("2022-12-31"), by = "month")

# Enhanced Portfolio Returns Over Time Plot
png("Enhanced_Portfolio_Returns_Over_Time.png", width = width, height = height, res = 150)
plot(dates, UMVE_port, type = "l", col = "blue", lwd = 2,
     xlab = "Year", ylab = "Portfolio Return (in %)", main = "UMVE Portfolio Returns Over Time",
     xaxt = "n")  # Suppress default x-axis
axis(1, at = seq(from = as.Date("2000-12-31"), to = as.Date("2022-12-31"), by = "2 years"), 
     labels = format(seq(from = as.Date("2000-12-31"), to = as.Date("2022-12-31"), by = "2 years"), "%Y"))
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")  # Add grid lines
dev.off()

# Enhanced Cumulative Portfolio Returns Plot
png("Enhanced_Cumulative_Portfolio_Returns.png", width = width, height = height, res = 150)
plot(dates, cumsum(UMVE_port), type = "l", col = "darkblue", lwd = 2,
     xlab = "Year", ylab = "Cumulative Return (in %)", main = "UMVE Cumulative Portfolio Returns",
     xaxt = "n")  # Suppress default x-axis
axis(1, at = seq(from = as.Date("2000-12-31"), to = as.Date("2022-12-31"), by = "2 years"), 
     labels = format(seq(from = as.Date("2000-12-31"), to = as.Date("2022-12-31"), by = "2 years"), "%Y"))
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")  # Add grid lines
dev.off()

# Save the Descriptive Statistics Tables
library(gridExtra)
library(grid)

# Convert weights_summary and returns_summary into tables for plotting
weights_table <- tableGrob(weights_summary, rows = NULL)
returns_table <- tableGrob(returns_summary, rows = NULL)

# Save weights_summary to a PNG
png("Weights_Descriptive_Statistics.png", width = width, height = height, res = 150)
grid.newpage()
grid.draw(weights_table)
dev.off()

# Save returns_summary to a PNG
png("Returns_Descriptive_Statistics.png", width = width, height = height, res = 150)
grid.newpage()
grid.draw(returns_table)
dev.off()


