library(stringr);library(dplyr);library(readxl);library(quantmod);library(PerformanceAnalytics);library(Hmisc);library(RColorBrewer);library(readxl);library(scales);
library(data.table);library(purrr);library(astsa);library(ggvis);library(tidyr);library(readr)


get_stock_prices <- function(symbol, from="1950-01-01", to = Sys.Date(), adjusted = TRUE){
    symbol.ohlc <- na.omit(getSymbols(symbol, from = from,
                                        to = to, auto.assign=F))
    
    if(adjusted == TRUE){
    splits <- getSplits(symbol, from = from, to = to)
    dividends <- getDividends(symbol, from = from, to = to)
    ratios <- adjRatios(splits = splits, dividends = dividends, close = Cl(symbol.ohlc))
    adjusted.symbol <- Cl(symbol.ohlc) * ratios[, "Split"] * ratios[, "Div"]
    assign("prices", adjusted.symbol, envir = globalenv())
    assign(symbol, adjusted.symbol, envir = globalenv())
    } else{adjusted.symbol <- Cl(symbol.ohlc)
    assign("prices", adjusted.symbol, envir = globalenv())
    assign(symbol, adjusted.symbol, envir = globalenv())}

}