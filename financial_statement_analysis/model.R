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
  geom_text(label = df$symbol) +
  dark_mode() +
  ggtitle("Top 75 Stocks by Market Cap Compared to PE") 

#compare peRatio and 3month performance
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

