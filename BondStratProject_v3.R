setwd("C:/Users/Student2.AzureAD/Documents/R")

getwd()

load("BBG_BondTicker.RData")

library(openxlsx)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(rlang)
library(gridExtra)



# connect to BBG (Terminal must be open)
library(Rblpapi)
blpConnect()

# Only look at the G10 Countries:
unique(DT$country)

G10 <- c("AUSTRALIA","CANADA","GERMANY","JAPAN","NEW ZEALAND","NORWAY","SWEDEN","SWITZERLAND","BRITAIN","UNITED STATES")

Tickers <- DT$ticker[DT$country %in% G10]

Tickers

# Only look at Gov. Bond 1-Month, and 10-Y

Tickers <- Tickers[grepl("3M|10Y",Tickers)]

Tickers

opt <- c("periodicitySelection"="DAILY")
Gov_Bonds <- bdh(Tickers, c("PX_LAST"),
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
Countries <- c("BRITIAN","UNITED STATES","AUSTRALIA","CANADA","GERMANY","JAPAN","NEW ZEALAND","NORWAY","SWEDEN","SWITZERLAND")
Bond_names_3M <- rep("3M",10)
Bond_names_1Y <-rep("1Y",10)
Bond_names_3M <- paste(Countries,Bond_names_3M)
Bond_names_1Y <- paste(Countries,Bond_names_1Y)
FX_names <- c(exchange_tickers, "USDUSD Curncy")

#Create the Ticker sequence for the Loop
Ticker_seq <- seq(from = 1, to = 19, by = 2)
FX_Tickers <- c(rep(FX_names,each = 2))
length(FX_Tickers)
B_3M_Tickers <- c(rep(Bond_names_3M,each = 2))
typeof(B_3M_Tickers)
B_1Y_Tickers <- c(rep(Bond_names_1Y,each = 2))
typeof(B_1Y_Tickers)


#Setting the time frame
#United States: largest data range
time_frame_raw <- Gov_Bonds[[Tickers[19]]][1]
time_frame <- as.Date(time_frame_raw[[1]])
time_frame
length(time_frame)
length(unique(time_frame))

#Add USD currency list to exchange rate
USDUSD_Curncy <- data.frame(date = time_frame,PX_LAST = rep(1,length(time_frame)))
exchange_rate_complete <- append(Exchange_rate,list(USDUSD_Curncy))
names(exchange_rate_complete)[length(exchange_rate_complete)] <- "USDUSD Curncy"

#Create data frames for end result:
Bonds_prices_3M <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Bonds_prices_3M) <- c(Bond_names_3M)
Bonds_prices_1Y <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Bonds_prices_1Y) <-c(Bond_names_1Y)
Exchange_rates <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Exchange_rates) <- c(FX_names)
rownames(Bonds_prices_3M) <- time_frame
rownames(Bonds_prices_1Y) <- time_frame
rownames(Exchange_rates) <- time_frame

#Setting the same time frame for every data and interpolate missing values
for(i in Ticker_seq){
  #Data for each Country
  df1 <- data.frame(date = as.Date(Gov_Bonds[[Tickers[i]]][[1]]),data = Gov_Bonds[[Tickers[i]]][[2]])
  df2 <- data.frame(date = as.Date(Gov_Bonds[[Tickers[i+1]]][[1]]),data = Gov_Bonds[[Tickers[i+1]]][[2]])
  df3 <- data.frame(date = as.Date(exchange_rate_complete[[FX_Tickers[i]]][[1]]),data = exchange_rate_complete[[FX_Tickers[i]]][[2]])
  
  zoo_df1 <- zoo(df1$data, order.by = df1$date)
  zoo_df2 <- zoo(df2$data, order.by = df2$date)
  zoo_df3 <- zoo(df3$data, order.by = df3$date)
  
  # Reindex each zoo object to match the target date vector and set missing values to NA
  zoo_reindexed_1 <- merge(zoo_df1, zoo(, time_frame), all = TRUE)
  zoo_reindexed_2 <- merge(zoo_df2, zoo(, time_frame), all = TRUE)
  zoo_reindexed_3 <- merge(zoo_df3, zoo(, time_frame), all = TRUE)
  
  zoo_interpolated_1 <- na.approx(zoo_reindexed_1, xout = time_frame, na.rm = FALSE)
  zoo_interpolated_2 <- na.approx(zoo_reindexed_2, xout = time_frame, na.rm = FALSE)
  zoo_interpolated_3 <- na.approx(zoo_reindexed_3, xout = time_frame, na.rm = FALSE)
  
  # Perform linear interpolation for missing values for each sequence
  Bonds_prices_3M[[B_3M_Tickers[i]]] <- coredata(zoo_interpolated_1)
  Bonds_prices_1Y[[B_1Y_Tickers[i]]] <- coredata(zoo_interpolated_2)
  Exchange_rates[[FX_Tickers[i]]] <- coredata(zoo_interpolated_3)
}

#Plotting and Observing the Data
################################################################################

#Add new column for the date
Bonds_prices_3M$date <- as.Date(rownames(Bonds_prices_3M))
Bonds_prices_1Y$date <- as.Date(rownames(Bonds_prices_1Y))
Exchange_rates$date <-  as.Date(rownames(Exchange_rates))

##Correcting Outliers:
#Japan One Year Bond Price --> Possible Missing Data 
outlier_Jap_date <- Bonds_prices_1Y$date[Bonds_prices_1Y$`JAPAN 1Y`== 0]
#Use the mean of the last 10 days to replace the outlier/missing data
index <- which(Bonds_prices_1Y$date == outlier_Jap_date)
Replacement <- mean(Bonds_prices_1Y$`JAPAN 1Y`[(index-10):(index-1)])
Bonds_prices_1Y$`JAPAN 1Y`[index] <- Replacement
  
##Plots with corrected values:
plot_list_3M <- list()
plot_list_1Y <- list()
plot_list_FX <- list()

for (i in 1:10) {
  column_name_3M <- Bond_names_3M[i]
  column_name_1Y <- Bond_names_1Y[i]
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
  plot_list_1Y[[i]] <- ggplot(Bonds_prices_1Y, aes(x = date, y = !!sym(column_name_1Y))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Price", title = paste( column_name_1Y))+
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
grid.arrange(grobs = plot_list_1Y, ncol = 2)
grid.arrange(grobs = plot_list_FX, ncol = 5)

#Saving the data:

save(Bonds_prices_3M, Bonds_prices_1Y,Exchange_rates,B_1Y_Tickers,B_3M_Tickers,FX_names,Tickers, file = "Bond_PX_FX.RData")
save(plot_list_3M,plot_list_1Y,plot_list_FX, file ="Bond_PX_FX_Plots.RData")

# Creating the Bond Excess Returns
################################################################################

