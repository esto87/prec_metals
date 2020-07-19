rm(list=ls())
library(tidyverse)
library(rmgarch)
source('lib/helpers.R')

GD <- read.csv( "input_data/XAUUSD_15 Mins_Bid_2017.07.13_2020.07.19.csv",header = T, sep = ',', stringsAsFactors = F)
SR <- read.csv("input_data/XAGUSD_15 Mins_Bid_2017.07.13_2020.07.19.csv",header = T, sep = ',', stringsAsFactors = F)

par(mfrow=c(2,1))
# plot(GD$Close, type='l')
# plot(SR$Close, type='l')

LAG <- 1

df <- GD %>% left_join(SR, by = 'Time..UTC.') %>% 
  mutate(GD_close_ROR = log(Close.x/lag(Close.x,LAG,Close.x[1]))
         ,GD_high_ROR = log(High.x/lag(Close.x,LAG,Close.x[1]))
         ,GD_low_ROR = log(Low.x/lag(Close.x,LAG,Close.x[1]))
         ,SR_close_ROR = log(Close.y/lag(Close.y,LAG,Close.y[1]))
         ,SR_high_ROR = log(High.y/lag(Close.y,LAG,Close.y[1]))
         ,SR_low_ROR = log(Low.y/lag(Close.y,LAG,Close.y[1]))
  ) %>% 
  select(Time = Time..UTC.
         ,GD_close = Close.x
         ,GD_high = High.x
         ,GD_low = Low.x
         ,SR_close = Close.y
         ,SR_high = High.y
         ,SR_low = Low.y
         ,GD_close_ROR
         ,GD_high_ROR
         ,GD_low_ROR
         ,SR_close_ROR
         ,SR_high_ROR
         ,SR_low_ROR
  )

cor(df$GD_close_ROR, df$SR_close_ROR) %>% print

# weights = data.frame(w1=1:100/100)
# v1 <- df$GD_close_ROR
# v2 <-  df$SR_close_ROR
# SRs <- apply(weights, 1,get_sr)
# SRs %>% plot

# DCCtest(select(df,GD_close_ROR,SR_close_ROR), garchOrder = c(1,1), n.lags = 1, solver = "solnp")

mspec <- multispec(
  speclist = c(
    ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(1, 1))
               ,distribution.model='sstd')
    # ,ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(1, 1))
    #             ,distribution.model='sstd')
    # ,ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(1, 1))
    #             ,distribution.model='sstd')
    ,ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(1, 1))
                ,distribution.model='sstd')
    # ,ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(1, 1))
    #             ,distribution.model='sstd')
    # ,ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(1, 1))
    #             ,distribution.model='sstd')
  )
)

spec_m <- dccspec(uspec = mspec
                  ,VAR = F
                  ,robust = F
                  ,lag = 1, 
                  lag.criterion = c("AIC"), 
                  robust.control = list("gamma"=0.25
                                        ,"delta"=0.01
                                        ,"nc"=10
                                        ,"ns"=500),
                  dccOrder = c(1,1)
                  ,model = c("DCC")
                  ,groups = rep(1, length(uspec@spec)))

df_m <- xts::xts(x=select(df
                          ,GD_close_ROR
                          # ,GD_high_ROR
                          # ,GD_low_ROR
                          ,SR_close_ROR
                          # ,SR_high_ROR
                          # ,SR_low_ROR
)
,order.by=as.Date(str_replace_all(df$Time,'[.]','-'))
)

dccroll_m <- dccroll(spec=spec_m
                     ,data=df_m[65e3:nrow(df_m),]
                     ,save.wdir='output/workings'
                     ,n.ahead=1
                     ,forecast.length=960
                     ,refit.every=24
                     ,refit.window=c('recursive')
                     ,solver='solnp'
                     ,fit.control=list(eval.se=T
                                       ,stationarity=T
                                       ,scale=T)
                     ,save.fit = T)

v1 <- df$GD_close_ROR
v2 <-  df$SR_close_ROR

r_p <- get_p(c(0.5,0.5)) %>% smooth %>% smooth %>% smooth %>% smooth

plot(r_p[(length(r_p)-1026):length(r_p)],type='l')
abline(h=0, col='red')

plot(dccroll_m)
