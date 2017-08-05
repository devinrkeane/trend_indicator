
library(quantmod);library(ggplot2);library(tidyr);library(reshape2);library(lubridate); library(purrr); library(XLConnect)
library(dplyr);library(RColorBrewer);library(plotly);library(readr);library(stringr); library(ggthemes);library(PerformanceAnalytics)


trend_indicator <- function(weight_list = list(wt30 = 0.4, wt60 = 0.1, wt120 = 0.1, wt252 = 0.4), span = 30, index.ticker = "^RUA",
                                 start.date = "1960-01-01", end.date = Sys.Date(), colorset = "Set1", write.excel = FALSE, monthly = FALSE){
    
    if(!exists("params", where = globalenv())){
        params <<- list(weight_list = list(wt30 = weight_list[[1]], wt60 = weight_list[[2]], wt120 = weight_list[[3]], wt252 = weight_list[[4]]), 
                              span = span, index.ticker = index.ticker, start.date = start.date, end.date = end.date, 
                              colorset = colorset, write.excel = write.excel, monthly = monthly) 
    }
    
    params.check <<- list(weight_list = list(wt30 = weight_list[[1]], wt60 = weight_list[[2]], wt120 = weight_list[[3]], wt252 = weight_list[[4]]), 
                          span = span, index.ticker = index.ticker, start.date = start.date, end.date = end.date, 
                          colorset = colorset, write.excel = write.excel, monthly = monthly)
    
    if(!identical(params.check, params) | !exists("prices", where = globalenv())){  
        
        params <<- params.check
        
        ticker <<- index.ticker
        
        weights <- weight_list
        
        if(!exists(index.ticker, where = globalenv()) | !exists("prices", where = globalenv())){
            get_stock_prices(symbol = index.ticker, from = start.date)
            colnames(prices) <- str_replace(ticker, "^", "")
        }
        
        prices$MA_30 <- rollapply(prices[,1], width = 30, FUN = mean)
        prices$MA_60 <- rollapply(prices[,1], width = 60, FUN = mean)
        prices$MA_120 <- rollapply(prices[,1], width = 120, FUN = mean)
        prices$MA_252 <- rollapply(prices[,1], width = 253, FUN = mean)
        prices$weighted <- weights[[1]]*prices$MA_30 + weights[[2]]*prices$MA_60 + 
        weights[[3]]*prices$MA_120 + weights[[4]]*prices$MA_252
    
        assign("prices", prices, globalenv())
    
    }

    prices_df <- as_data_frame(prices) %>% 
                       mutate(Date = index(prices)) %>%
                       select(Date, 1, MA_30, MA_60, MA_120, MA_252, weighted) %>%
                       arrange(desc(Date)) 
                       
    prices_df <- prices_df[complete.cases(prices_df),]
        
    #generate best fit lines for last n weighted moving average points (n = the 'span' argument)
    best_fit_slope <- NULL
    not_na <- which(!is.na(prices_df$weighted))
    
    for(i in not_na){
            reg <- lm(weighted[i:(i+span)]~Date[i:(i+span)], data = prices_df)
            best_fit_slope[i] <- reg$coefficients[[2]]
        }
        
    prices_df$best_fit_slope <- best_fit_slope
    prices_df$daily_trend <- ifelse(prices_df$best_fit_slope > 0, "MAX", "MIN")
    
    prices_df$quarter <- lubridate::quarter(prices_df$Date, with_year = TRUE)
                                                #delete first four rows of NAs
    prices_return <- periodReturn(prices[,1], period = "quarterly")[paste(start.date,"/",end.date, sep = "")][-c(1:4)]
        
    trend_df_quarterly <<- prices_df %>% 
        xts(., order.by = prices_df$Date) %>% 
        subset(., from = "1962-01-02", to = end.date) %>%
        split.xts(., "quarters") %>%
        lapply(., function(x) x[1,]) %>%  
        do.call(rbind,.) %>% 
        as_data_frame(.) %>%  
        select(Date, daily_trend) %>% 
        mutate(Date = as.Date(Date)) %>%
        inner_join(as.data.frame(prices_df, by = "Date")) %>%
        bind_cols(as.data.frame(prices_return)) %>%
        mutate(quarter = lubridate::quarter(prices_return, with_year = T), 
               pos_neg = ifelse(quarterly.returns > 0, "Positive", "Negative")) %>%
        rename(quarter_trend = daily_trend)
    
    prices_df <- left_join(prices_df, select(trend_df_quarterly, quarter_trend, quarter), by = "quarter")
    
    assign("prices_df", prices_df, envir = globalenv())
    
    if(monthly == TRUE){
        
        prices_df_monthly <<- prices_df

        prices_return_monthly <- periodReturn(prices[,1], period = "monthly")[paste(start.date,"/",end.date, sep = "")][-c(1:12)]
        
        trend_df_monthly <<- prices_df_monthly %>% 
            xts(., order.by = prices_df$Date) %>% 
            split.xts(., "months") %>%
            lapply(., function(x) x[1,]) %>%  
            do.call(rbind,.) %>% 
            as_data_frame(.) %>%  
            select(., Date, quarter_trend) %>% 
            mutate(Date = as.Date(Date), return = round(as.numeric(prices_return_monthly), 4)) 
        if(write.excel == TRUE){  
            excel_file <- loadWorkbook("data/trend_quarterly_analysis_1950-2017.xlsx")
            writeWorksheet(excel_file, trend_df_monthly, sheet = "monthly_trend_data", startRow = 2, startCol = 1, header = FALSE)
            saveWorkbook(excel_file, "data/trend_quarterly_analysis_1950-2017.xlsx")
        }
     }
  

    trend_quarterly_tidy <<- trend_df_quarterly %>% filter(!is.na(quarter_trend)) %>%
                             gather(., ma_length, ma_value, -Date, -quarter_trend,
                                    -best_fit_slope, -quarter, -quarterly.returns, -pos_neg)
        
    trend_summary <- table(trend_df_quarterly$quarter_trend, trend_df_quarterly$pos_neg)
    
    trend_by_pct <- table(trend_df_quarterly$quarter_trend, round(trend_df_quarterly$quarterly.returns, 2))
    
    print(trend_summary)
    print(trend_by_pct)

    
    if(write.excel == TRUE){
        to_excel <- list(trend_summary, trend_by_pct)
        excel_file <- loadWorkbook("data/trend_quarterly_analysis_1950-2017.xlsx")
        writeWorksheet(excel_file, select(trend_df_quarterly, 1, 2, 10:12), sheet = "quarterly_trend_data", startRow = 2, header = FALSE)
        writeWorksheet(excel_file, trend_summary, sheet = "R_trend_summary", startRow = 1, startCol = 1)
        writeWorksheet(excel_file, trend_by_pct, sheet = "R_trend_summary", startRow = 1, startCol = 5)
        saveWorkbook(excel_file, "data/trend_quarterly_analysis_1950-2017.xlsx")
    }
    
    #moving average trendlines and raw price data        
     trend.plot <<-  ggplot(trend_quarterly_tidy, aes(x=Date, y=ma_value, col=factor(ma_length))) + 
                    geom_line() + 
                    #geom_rect(data = min_periods, aes(x=Date, ymin=Inf, xmin=Inf)) +
                    scale_color_brewer(name = paste(names(prices)[1], "Series"),
                                       palette = colorset, 
                                       labels = c("120 day MA", "240 day MA", 
                                                  "30 day MA", "60 day MA", 
                                                  names(prices)[1], "Weighted MA")) +
                    labs(title = "Raw and Moving Average (MA) Lengths",
                         x = "Year", 
                         y = paste(names(prices)[1], "Close Value")) +
                    scale_x_date(limits = c(as.Date(start.date), as.Date(end.date))) +
                    theme_classic() 
       
       ggplot(trend_quarterly_tidy, aes(x=quarterly.returns, col= factor(pos_neg), fill=factor(quarter_trend))) + 
           geom_histogram(binwidth = 0.02, alpha = 0.6, position = "stack") +
           scale_x_continuous(labels = scales::percent) +
           scale_fill_manual(values = c("green", "red"), name = "Trend Status") +
           labs(title = format(paste("Range of Quarterly Returns and Position of Trend Indicator\n", start.date, "-", end.date),
                               justify = "centre"),
                        x = "Quarterly Return",
                        y = "Proportion of Trend MIN/MAX Positions") +
           guides(col = "none") +
           theme_minimal() +
           theme(legend.position = "top")
       
}


plot.zoo(prices["1963/1966"], plot.type = "single",
         col = brewer.pal(6, "PuBu"), lwd = .85, 
         main = "Moving Averages", xlab = "Date", ylab = paste(names(trend_df_quarterly)[3], "Index Value", sep = " "))





#rolling 10 year returns

plot.xts(na.omit(apply.rolling((portfolio.return*12), width = 120, Return.annualized(scale = .1))), main="Rolling 10 Year Returns")
