library(PerformanceAnalytics);library(quantmod);library(ggplot2);library(tidyr);library(reshape2);library(lubridate); library(purrr)
library(dplyr);library(RColorBrewer);library(plotly);library(readr);library(stringr);library(data.table)

    
treasury_returns <- function(freq = "months", write.excel = FALSE, inflation = FALSE){

    treasury10yr <- getSymbols.FRED("DGS10", env = globalenv(), auto.assign = FALSE)[,1]
    
    treasury10yr_monthly <- na.locf(treasury10yr) %>% 
        split(., freq) %>% 
        lapply(., function(x) first(x)) %>%
        do.call(rbind, .) 
    
    pce_PI <- getSymbols.FRED("PCEPI", env = globalenv(), from = start(treasury10yr), to = end(treasury10yr),
                              auto.assign = FALSE)[,1]
    pce_PI$inflation_rate <-  monthlyReturn(pce_PI$PCEPI)
  
    pce_PI2 <- pce_PI["1962/2017"]
    treasury10yr_monthly <- treasury10yr_monthly["1962/2017-04-03"]
    index(treasury10yr_monthly) <- index(pce_PI2)
    
    #line up dates with inflation and yield
    treasury10yr_monthly <- merge.xts(treasury10yr_monthly, pce_PI2)
    
    colnames(treasury10yr_monthly) <- c("yield", "price.level", "inflation.rate")
    treasury10yr_monthly$yield <- treasury10yr_monthly$yield/100


    treasury10yr_monthly$price_t <- 1000/(1+treasury10yr_monthly$yield)^(120/12)
    treasury10yr_monthly$price_t1 <- 1000/(1+lag.xts(treasury10yr_monthly$yield, k = -1))^(119/12)
    treasury10yr_monthly$accrued_int <- lag.xts((lag.xts(treasury10yr_monthly$yield)/12*1000), k = -1)

    yield_t1 <- lag.xts(treasury10yr_monthly$yield, k = -1)
    
    #create semi-annual coupons at end of year based on yield at beginning of year
    coupon_index <- seq(1,nrow(treasury10yr_monthly), 12)
    coupons <- (treasury10yr_monthly$yield[coupon_annual]/2)*1000
    treasury10yr_monthly <- merge.xts(treasury10yr_monthly, coupons)
    
    names(treasury10yr_monthly)[7] <- "coupon"
    treasury10yr_monthly$coupon <- lag.xts(treasury10yr_monthly$coupon, 6)
    treasury10yr_monthly$coupon <- na.fill(treasury10yr_monthly$coupon, 0)
    
    #use first semi annual coupons to populate second semi annual coupon
    semi_annual.1 <- which(treasury10yr_monthly$coupon != 0)
    semi_annual.2 <- semi_annual.1 + 6
    treasury10yr_monthly$coupon[semi_annual.2] <- treasury10yr_monthly$coupon[semi_annual.1]
    
    #calculate return of bond based on price change and coupon issued semi-annually
    treasury10yr_monthly$return <- table.CalendarReturns((treasury10yr_monthly$price_t1 - treasury10yr_monthly$price_t + treasury10yr_monthly$coupon)/
        treasury10yr_monthly$price_t)
    
    (treasury10yr_monthly$yield*(1-(1/(1+yield_t1)^10)))/yield_t1 + (1+yield_t1)^-10 - 1
    
    
    index(treasury10yr_monthly) <- as.yearmon(index(treasury10yr_monthly))
    
    
    
    sp500 <- monthlyReturn(gspc$gspc_adjusted)[paste(start(treasury10yr), "/", end(treasury10yr), sep = "")]

    
    
    stock_bond_returns <- merge.xts(treasury10yr_monthly$DGS10, gspc)
    names(stock_bond_returns) <- c("treasury10yr", "sp500")
    stock_bond_returns$treasury10yr <- na.locf(stock_bond_returns$treasury10yr)
    stock_bond_returns_complete <- stock_bond_returns[complete.cases(stock_bond_returns)]
    
    table.AnnualizedReturns(stock_bond_returns_complete)    
    charts.PerformanceSummary(stock_bond_returns_complete)
    
}
    
    
#Rolling Returns
    trend_analysis_returns <- as.xts(read.csv.zoo("data/trend_buy_hold_returns_1962_2017.csv", format = "%m/%d/%Y"))
    table.AnnualizedReturns(trend_analysis_returns["2007/2017-06-29"], Rf = .03/12)
    rolling_10_year <- rollapply(trend_buy_and_hold, FUN = Return.annualized, width = 40)
    write.zoo(rolling_10_year, "rolling_10_year.csv", sep = ",")
    
    
    
    
    
    
    