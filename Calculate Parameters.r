library(quantmod)
library(dplyr)

# Downloading ETF data
tickers <- c('FXE', 'EWJ', 'GLD', 'QQQ', 'SPY', 'SHV', 'DBA', 'USO', 'XBI', 'ILF', 'EPP', 'FEZ')
getSymbols(tickers, from = '2007-02-28', to = '2022-11-01')

# Reading Fama French data
ff <- read.csv('F-F_Research_Data_Factors_daily.CSV')
colnames(ff)[1] <- 'Date'
ff[,1] <- as.Date(ff[,1], format = '%Y%m%d')
ff <- (dplyr::filter(ff, Date >= '2007-03-01', Date <= '2022-11-01'))


# Compiling adjusted close column of all stocks into one df
ETFs <- FXE$FXE.Adjusted
ETFs <- as.matrix(ETFs)
for (i in 2:length(tickers)) {
  temp_stock <- as.matrix(mget(tickers)[[i]][,6])
  rownames(temp_stock) <- NULL
  try(ETFs <- cbind(ETFs, temp_stock)) #Only gets stocks with data for full time frame
}
colnames(ETFs) <- tickers
ETFs <- as.data.frame(ETFs)

# Annualized Returns
ETFs_rtn <- (ETFs[2:nrow(ETFs),] - ETFs[1:(nrow(ETFs) - 1),]) / ETFs[1:(nrow(ETFs) - 1),] * 252

# Function to get params for investment strategies
get_strategy_params <- function(returns, ff_data, benchmark, start='2007-03-01', end='2022-11-01') {
  benchmark <- as.data.frame(cbind(rownames(returns), benchmark))
  returns <- filter(cbind(rownames(returns), returns), rownames(returns) >= start, rownames(returns) <= end)[, 2:(ncol(returns)+1)]
  ff_data <- filter(ff_data, Date >= start, Date <= end)
  benchmark <- as.numeric(filter(benchmark, V1 >= start, V1 <= end)[, 2])
  
  reg_info <- data.frame(matrix(ncol = 5, nrow = 0))
  for (i in 1:ncol(returns)) {
    reg <- lm(returns[,i]~ff_data[,2]+ff_data[,3]+ff_data[,4])
    reg_info <- rbind(reg_info, c(reg$coefficients[1], reg$coefficients[2], reg$coefficients[3], reg$coefficients[4], sigma(reg)))
  }
  colnames(reg_info) <- c("Intercept", "Beta_Momentum", "Beta_SMB", "Beta_HML", "Idiosyncratic SD")
  rownames(reg_info) <- colnames(ETFs_rtn)
  
  beta <- cov(returns, benchmark)/var(benchmark)
  
  ff_means <- colMeans(ff_data[,2:ncol(ff_data)])
  expected_rtn <- c()
  for (i in 1:nrow(reg_info)) {
    expected_rtn <- c(expected_rtn, ff_means[4] + reg_info[i,2]*ff_means[1] + reg_info[i,3]*ff_means[2] + reg_info[i,4]*ff_means[3] + reg_info[i,1])
  }
  names(expected_rtn) <- colnames(ETFs_rtn)
  
  cov_matrix <- as.matrix(reg_info[, 2:4]) %*% cov(ff_data[, 2:4]) %*% t(as.matrix(reg_info[, 2:4]))
  
  params <- list(reg_info, beta, expected_rtn, cov_matrix)
  names(params) <- c("Regression_Info", "Beta", "Expected_Return", "Covariance_Matrix")
  return(params)
}
params <- get_strategy_params(ETFs_rtn, ff, ETFs_rtn[,5], start = '2010-03-01', end = '2022-11-01')
params$Beta
params$Expected_Return
params$Covariance_Matrix







