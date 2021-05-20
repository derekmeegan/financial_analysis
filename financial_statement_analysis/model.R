library(quantmod)
library(tidyverse)
library(ggdark)
library(scales)
library(PerformanceAnalytics)
library(zoo)
library(httr)
library(stringr)
library(jsonlite)
library(reshape)
library(tidyquant)
library(readxl)
cloudexApiKey <- "pk_c21de93fbdea4be89ca216aabb42e4ca"


getBalanceSheet <- function(symbol) {
  str_glue(
    "https://cloud.iexapis.com/stable/stock/",
    symbol,
    "/balance-sheet/?token=",
    cloudexApiKey
  ) %>% 
    GET() %>% 
    content("text") %>% 
    fromJSON() %>% 
    .$balancesheet %>% 
    melt() %>% 
    .[-c(29:30),-c(1:8)]
}

getCashFlow <- function(symbol) {
  str_glue(
    "https://cloud.iexapis.com/stable/stock/",
    symbol,
    "/cash-flow/?token=",
    cloudexApiKey
  ) %>% 
    GET() %>% 
    content("text") %>% 
    fromJSON() %>% 
    .$cashflow %>% 
    melt() %>% 
    .[-c(18:19),-c(1:8)]
}

getIncomeStatement <- function(symbol) {
  str_glue(
    "https://cloud.iexapis.com/stable/stock/",
    symbol,
    "/income/?token=",
    cloudexApiKey
  ) %>% 
    GET() %>% 
    content("text") %>% 
    fromJSON() %>% 
    .$income %>% 
    melt() %>% 
    .[-c(18:19),-c(1:8)]
}

getKeyStats <- function(symbol) {
  str_glue(
    "https://cloud.iexapis.com/stable/stock/",
    symbol,
    "/stats/?token=",
    cloudexApiKey
  ) %>% 
    GET() %>% 
    content("text") %>% 
    fromJSON() %>% 
    melt %>% 
    .[,order(ncol(.):1)]
}

getAdvancedStats <- function(symbol) {
  str_glue(
    "https://cloud.iexapis.com/stable/stock/",
    symbol,
    "/advanced-stats/?token=",
    cloudexApiKey
  ) %>% 
    GET() %>% 
    content("text") %>% 
    fromJSON() 
}

getCompanyInfo <- function(symbol) {
  str_glue(
    "https://cloud.iexapis.com/stable/stock/",
    symbol,
    "/company/?token=",
    cloudexApiKey
  ) %>% 
    GET() %>% 
    content("text") %>% 
    fromJSON() 
}

chart <- function(symbol, subset) {
  getSymbols(symbol, auto.assign = FALSE) %>% 
    chartSeries(
      subset=subset,
      name = paste(toupper(symbol), subset, "Performance"),
      TA = "addVo();addMACD();addRSI();addEMA(50);addEMA(200)",
      theme = chartTheme("white")
    )
}

#compare peRatio and market cap
getPE <- function(symbols) {
  lst <- c()
  for(symbol in symbols) {
    pe <- getKeyStats(symbol) %>% 
      filter(L1 == "peRatio") %>% 
      .[,2] %>% 
      as.numeric
    lst = c(lst, pe)
  }
  return(lst)
}

pe_df <- tq_exchange("nyse") %>% 
  arrange(desc(market.cap)) %>% 
  head(75) %>% 
  .[, c(1,2,4,7)] %>% 
  mutate(peRatio = getPE(symbol))

pe_df <- read_excel("/Users/d/desktop/fs_analysis/rename.xlsx")

pe_df %>% 
  ggplot(aes(x = peRatio, y = market.cap, color = industry)) +
  geom_point() + 
  geom_text(label = pe_df$symbol) +
  dark_mode() +
  ggtitle("Top 75 Stocks by Market Cap Compared to PE") 

#compare peRatio and 3 month performance
getDE <- function(symbols) {
  lst <- c()
  for(symbol in symbols) {
    de <- str_glue(
      "https://cloud.iexapis.com/stable/stock/",
      symbol,
      "/advanced-stats/?token=",
      cloudexApiKey
    ) %>% 
      GET() %>% 
      content("text") %>% 
      fromJSON() %>% 
      .$debtToEquity
    if(is.null(de)) de = 0
    lst <- c(lst, de)
  }
  return(lst)
}

getTM <- function(symbols) {
  lst <- c()
  for(symbol in symbols) {
    tm <- getKeyStats(symbol) %>% 
      filter(L1 == "month3ChangePercent") %>% 
      .[,2] %>% 
      as.numeric
    lst = c(lst, tm)
  }
  return(lst)
}

tm_df <- pe_df %>% 
  mutate(threeMoP = getTM(symbol),
         de = getDE(symbol)) %>% 
  filter(de != 0)

tm_df %>% 
  ggplot(aes(x = threeMoP, y = de, color = industry)) +
  geom_point() + 
  geom_text(label = tm_df$symbol) +
  dark_mode() +
  ggtitle("Top 70 Stocks by Market Cap Compared to D/E") 

#Compare Div Yield to 3 month Performance
getDivYield <- function(symbols) {
  lst <- c()
  for(symbol in symbols) {
    tm <- getKeyStats(symbol) %>% 
      filter(L1 == "dividendYield") %>% 
      .[,2] %>% 
      as.numeric
    lst = c(lst, tm)
  }
  return(lst)
}
 
dy_df <- tm_df %>% 
  mutate(divYield = getDivYield(symbol))

dy_df %>% 
  ggplot(aes(x = threeMoP, y = divYield)) +
  geom_point() + 
  geom_smooth(method = lm) +
  geom_text(label = tm_df$symbol) +
  dark_mode() +
  ggtitle("Top 70 Stocks by Market Cap Compared to Div Yield") 

getSymbols("UPS", auto.assign = FALSE) %>% 
  chartSeries(
    subset="2021",
    name = "UPS 2021 Performance"
  )

#Compare Avg 10 Day Vol to Div Yield for Positive Performers
getAvgVol <- function(symbols) {
  lst <- c()
  for(symbol in symbols) {
    tm <- getKeyStats(symbol) %>% 
      filter(L1 == "avg10Volume") %>% 
      .[,2] %>% 
      as.numeric
    lst = c(lst, tm)
  }
  return(lst)
}

getMonthChange <- function(symbols) {
  lst <- c()
  for(symbol in symbols) {
    tm <- getKeyStats(symbol) %>% 
      filter(L1 == "month1ChangePercent") %>% 
      .[,2] %>% 
      as.numeric
    lst = c(lst, tm)
  }
  return(lst)
}

vol_df <- dy_df %>% 
  mutate(avg10Vol = getAvgVol(symbol),
         month1change = getMonthChange(symbol)) %>% 
  filter(month1change>0)

vol_df


vol_df %>% 
  ggplot(aes(x = avg10Vol, y = divYield)) +
  geom_point() + 
  geom_smooth(method = lm) +
  geom_text(label = vol_df$symbol) +
  dark_mode() +
  ggtitle("Avg 10 Day Volume vs Dividend Yield") 


#Get Balance Sheet Accounts
getCA <- function(balanceSheet) {
  currentAssets <- c("currentCash", 
                     "receivables", 
                     "inventory",
                     "shortTermInvestments",
                     "currentAssets")
  totalCA <- balanceSheet %>% 
    filter(variable == "currentAssets") %>% 
    .[,2] 
  balanceSheet %>% 
    filter(variable %in% currentAssets) %>% 
    arrange(value) %>% 
    as.tibble %>% 
    mutate(ca_ratio = label_percent() (value/totalCA))
}

getFA <- function(balanceSheet) {
  fixedAssets <- c("goodwill", 
                    "intangibleAssets",
                    "otherAssets",
                   "propertyPlantEquipment", 
                   "netTangibleAssets",
                   "longTermInvestments")
  totalFA <- balanceSheet %>% 
    filter(variable == "longTermInvestments") %>% 
    .[,2] 
  balanceSheet %>% 
    filter(variable %in% fixedAssets) %>% 
    arrange(value) %>% 
    as.tibble %>% 
    mutate(fa_ratio = label_percent() (value/totalFA))
}


