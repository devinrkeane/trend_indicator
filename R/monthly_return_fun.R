library(stringr);library(dplyr);library(readxl);library(quantmod);library(PerformanceAnalytics);library(Hmisc);library(RColorBrewer);library(readxl);library(scales);
library(data.table);library(purrr);library(astsa);library(ggvis);library(tidyr);library(readr)


monthly.return.fun <- function(symbol, from="1962-01-01", to = Sys.Date()){
    symbol.ohlc <- na.omit(getSymbols(symbol, from = from,
                                      to = to, auto.assign=F))
    splits <- getSplits(symbol, from = from, to = to)
    
    dividends <- getDividends(symbol, from = from, to = to)
    
    ratios <- adjRatios(splits = splits, dividends = dividends, close = Cl(symbol.ohlc))
    
    adjusted.symbol <- Cl(symbol.ohlc) * ratios[, "Split"] * ratios[, "Div"]
    
    monthlyReturn(adjusted.symbol)
    
}