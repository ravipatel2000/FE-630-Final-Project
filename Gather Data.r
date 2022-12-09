library(quantmod)
library(dplyr)

tickers <- c('FXE', 'EWJ', 'GLD', 'QQQ', 'SPY', 'SHV', 'DBA', 'USO', 'XBI', 'ILF', 'EPP', 'FEZ')

getSymbols(tickers, from = '2007-03-01', to = '2022-11-01')

View(FXE)

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
