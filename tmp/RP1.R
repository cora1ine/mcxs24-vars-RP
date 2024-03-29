##### RP1

# install.packages("readabs")
# install.packages("readrba")
library(readabs)
library(readrba)
library(xts)
library(fUnitRoots)   # ADF test - adfTest
library(tidyverse)    # for table
library(kableExtra)   # for print table
install.packages("corrplot") # for corr
library(corrplot)

### Data downloading

# 1.Inflation / CPI 
# 6401.0 Consumer Price Index, Australia
# series_id = "A2325846C": Index Numbers ;  All groups CPI ;  Australia ;
cpi_download  = read_abs(series_id = "A2325846C")     
cpi_data      = xts(cpi_download$value, cpi_download$date)

# 2.GDP
# 5206.0 Australian National Accounts: National Income, Expenditure and Product
# series_id = "A2304404C": GDP per capita: Chain volume measures ;
gdp_download  = read_abs(series_id = "A2304404C")     
gdp_data      = xts(gdp_download$value, gdp_download$date)

# 3.Cash rate target

crt_download   = read_rba(series_id = "FIRMMCRTD")   
crt_data       = xts(crt_download$value, crt_download$date)
crt_data       = apply.quarterly(crt_data, mean)     # change daily to quarterly
crt_data       = xts(crt_data, seq(as.Date("1990-03-01"), by = "quarter", length.out = length(crt_data)))

# 4.Unemployment rate
# 6202.0 Labour Force, Australia
# series_id = "A84423050A": Unemployment rate ;  Persons ; seasonal adjust
unemp_download = read_abs(series_id = "A84423050A")     
unemp_data     = xts(unemp_download$value, unemp_download$date)
unemp_data     = apply.quarterly(unemp_data, mean) # change daily to quarterly
unemp_data     = xts(unemp_data, seq(as.Date("1978-03-01"), by = "quarter", length.out = length(unemp_data)))

# 5.Export
# 5368.0 International Trade in Goods
# series_id = "A2718603V": Debits, Total goods ;
export_download = read_abs(series_id = "A2718603V")     
export_data     = xts(export_download$value, export_download$date)
export_data     = abs(export_data)
export_data     = apply.quarterly(export_data, mean)
 
# 6.Import
# 5368.0 International Trade in Goods
# series_id = "A2718577A": Credits, Total goods ;
import_download  = read_abs(series_id = "A2718577A")     
import_data      = xts(import_download$value, import_download$date)
import_data      = apply.quarterly(import_data, mean)

# 7.New loan
# 5601.0 Lending Indicators
# series_id = "A108296973X"： Households ;  Housing Finance ;  Total housing excluding refinancing ;  New loan commitments ;  Value ; seasonal adjust
nloan_download   = read_abs(series_id = "A108296973X")     
nloan_data       = xts(nloan_download$value, nloan_download$date)
nloan_data       = apply.quarterly(nloan_data, mean)


# 8.Gold price
gold_link       = "https://query1.finance.yahoo.com/v7/finance/download/GC%3DF?period1=1262304000&period2=1703980800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true"
gold_download   = read.csv(gold_link)
gold_data       = gold_download[,6]
gold_data       = data.frame(gold_download[,1], gold_data)
colnames(gold_data) = c('date', 'gol')
gold_data$date  = as.Date(as.character(gold_data$date),format="%Y-%m-%d") 
gold_data       = xts(gold_data$gol, gold_data$date)
gold_data       = apply.quarterly(gold_data, mean)

# 9.AORD
aord_link       = "https://query1.finance.yahoo.com/v7/finance/download/%5EAORD?period1=1262304000&period2=1703980800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true"
aord_download   = read.csv(aord_link)
aord_data       = aord_download[,6]
aord_data       = data.frame(aord_download[,1], aord_data)
colnames(aord_data) = c('date', 'aord')
aord_data$date  = as.Date(as.character(aord_data$date),format="%Y-%m-%d") 
aord_data       = xts(aord_data$aord, aord_data$date)
aord_data       = apply.quarterly(aord_data, mean)

# 10. AUD/USD
exr_link        = "https://query1.finance.yahoo.com/v7/finance/download/AUDUSD%3DX?period1=1262304000&period2=1703980800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true"
exr_download    = read.csv(exr_link)
exr_data        = exr_download[,6]
exr_data        = data.frame(exr_download[,1], exr_data)
colnames(exr_data) = c('date', 'exr')
exr_data$date   = as.Date(as.character(exr_data$date),format="%Y-%m-%d") 
exr_data        = xts(exr_data$exr, exr_data$date)
exr_data        = apply.quarterly(exr_data, mean)

### Data plot
# All Variables
all_data             = na.omit(merge(cpi_data, gdp_data, crt_data, unemp_data, export_data, import_data, nloan_data,  gold_data, aord_data, exr_data ))
colnames(all_data)   = c("cpi_data", "gdp_data", "crt_data", "unemp_data", "export_data", "import_data","nloan_data", "gold_data", "aord_data", "exr_data")

## plot corr table
cor_matrix <- round(cor(all_data), 4)

cor_first_row <- as_tibble(t(cor_matrix[1, , drop = FALSE]))

cor_results_table <- add_column(cor_first_row, Variable = colnames(cor_matrix), .before = 1)

kable(cor_results_table, align = "c") %>% 
  kable_styling(font_size = 8, 
                fixed_thead = TRUE, 
                full_width = FALSE, 
                position = "center",
                latex_options = c("HOLD_position"),
                bootstrap_options = c("striped", "hover", "bordered", "responsive", "dark"))


## Line plot
par(mfcol = c(5, 2), mar = c(2, 2, 2, 2))

for (i in 1:10) {
  ts.plot(all_data[, i], main = colnames(all_data)[i], 
          ylab = "", xlab = "", col = "darkblue")
}

## log transformation for exp data

lcpi_data  =  log(cpi_data)
lexport_data= log(export_data)
limport_data=  log(import_data)
lexr_data = log(exr_data)


# All Variables after log
all_data             = na.omit(merge(lcpi_data,    gold_data, 
                                     gdp_data,     crt_data, 
                                     unemp_data,   nloan_data,  
                                     lexport_data, limport_data, 
                                     aord_data,    lexr_data ))

colnames(all_data)   = c("lcpi_data",     "gold_data", 
                         "gdp_data",      "crt_data", 
                         "unemp_data",    "nloan_data", 
                         "lexport_data",  "limport_data",
                         "aord_data",     "lexr_data")


## ACF plot
par(mfcol = c(5, 2), mar=c(2,2,2,2))
for (i in 1:10){
  acf = acf(all_data[,i], plot = FALSE)[1:20]
  plot(acf, main = "")
  title(main = paste(colnames(all_data)[i]), line = 0.5)
}


## AR

#check the optimal lag 
ar_results <- list()

for (i in 1:ncol(all_data)) {
  ol.aic.ar <- ar(all_data[,i], order.max=20, aic=TRUE, method="ols")
  
  ar_results[[colnames(all_data)[i]]] <- ol.aic.ar$order
}

## ADF test

# ol.cpi.aic.ar$order
adf.cpi   = adfTest(all_data[,1], lags=17, type="c")              # don't reject -> non-stationary
dadf.cpi  = adfTest(diff(all_data[,1]), lags=16, type="nc")        # don't reject -> non-stationary
d2adf.cpi = adfTest(diff(diff(all_data[,1])), lags=15, type="nc")  # reject -> (I2 is stationary)
# adf.cpi@test$p.value
#-> integration order = 

# ol.gold.aic.ar$order
adf.gold   = adfTest(all_data[,2], lags=18, type="c")               # don't reject -> non-stationary
dadf.gold  = adfTest(diff(all_data[,2]), lags=17, type="nc")        # don't reject -> non-stationary
d2adf.gold = adfTest(diff(diff(all_data[,2])), lags=16, type="nc")  # reject -> (I2 is stationary)
# adf.gold@test$p.value
#-> integration order = 2

# ol.gdp.aic.ar$order
adf.gdp   = adfTest(all_data[,3], lags=1, type="c")              # don't reject -> non-stationary
dadf.gdp  = adfTest(diff(all_data[,3]), lags=0, type="nc")       # reject -> (I1 is stationary)
# adf.gdp@test$p.value
#-> integration order = 1

# ol.crt.aic.ar$order
adf.crt  = adfTest(all_data[,4], lags=20, type="c")               # don't reject -> non-stationary
dadf.crt = adfTest(diff(all_data[,4]), lags=19, type="nc")        # don't reject -> non-stationary
d2adf.crt = adfTest(diff(diff(all_data[,4])), lags=18, type="nc") # don't reject -> non-stationary
# d2adf.crt@test$p.value
#-> integration order = 

# ol.unemp.aic.ar$order
adf.unemp  = adfTest(all_data[,5], lags=20, type="c")           # don't reject -> non-stationary
dadf.unemp = adfTest(diff(all_data[,5]), lags=19, type="nc")    # don't reject -> non-stationary
d2adf.unemp = adfTest(diff(diff(all_data[,5])), lags=18, type="nc")    # don't reject -> non-stationary
# adf.unemp@test$p.value
#-> integration order = 2

# ol.nloan.aic.ar$order
adf.nloan  = adfTest(all_data[,6], lags=20, type="c")           # don't reject -> non-stationary
dadf.nloan = adfTest(diff(all_data[,6]), lags=19, type="nc")    # don't reject -> non-stationary
d2adf.nloan = adfTest(diff(diff(all_data[,6])), lags=18, type="nc")    # don't reject -> non-stationary
# adf.nloan@test$p.value
#-> integration order = 

# ol.export.aic.ar$order
adf.export  = adfTest(all_data[,7], lags=20, type="c")           # don't reject -> non-stationary
dadf.export = adfTest(diff(all_data[,7]), lags=19, type="nc")    # reject -> (I1 is stationary)
d2adf.export = adfTest(diff(diff(all_data[,7])), lags=18, type="nc")    # don't reject -> non-stationary
# adf.export@test$p.value
#-> integration order = 2

# ol.import.aic.ar$order
adf.import  = adfTest(all_data[,8], lags=20, type="c")           # don't reject -> non-stationary
dadf.import = adfTest(diff(all_data[,8]), lags=19, type="nc")    # don't reject -> non-stationary
d2adf.import = adfTest(diff(diff(all_data[,8])), lags=18, type="nc")    # don't reject -> non-stationary
# d2adf.import@test$p.value
#-> integration order = 

# ol.aord.aic.ar$order
adf.aord   = adfTest(all_data[,9], lags=7, type="c")               # don't reject -> non-stationary
dadf.aord  = adfTest(diff(all_data[,9]), lags=6, type="nc")        # reject -> (I1 is stationary)
# adf.aord@test$p.value
#-> integration order = 1

# ol.exr.aic.ar$order
adf.exr   = adfTest(all_data[,10], lags=20, type="c")               # don't reject -> non-stationary
dadf.exr  = adfTest(diff(all_data[,10]), lags=19, type="nc")        # reject -> (I1 is stationary)
# adf.exr@test$p.value
# #-> integration order = 1

Unit_Root_Test_table <- 
  tibble( " " = c("lcpi", "gold", "gdp", "crt", "unemp", "nloan", "lexport","limport", "aord", "lexr"),
          "p value of ADF test of AR" 
          = round(c(adf.cpi@test$p.value,    adf.gold@test$p.value,
                    adf.gdp@test$p.value,    adf.crt@test$p.value,    
                    adf.unemp@test$p.value,  adf.nloan@test$p.value,
                    adf.export@test$p.value, adf.import@test$p.value,
                    adf.aord@test$p.value,   adf.exr@test$p.value),4),
          "p value of ADF test of diff-AR" 
          = round(c(dadf.cpi@test$p.value,    dadf.gold@test$p.value,
                    dadf.gdp@test$p.value,    dadf.crt@test$p.value,    
                    dadf.unemp@test$p.value,  dadf.nloan@test$p.value,
                    dadf.export@test$p.value, dadf.import@test$p.value,   
                    dadf.aord@test$p.value,   dadf.exr@test$p.value),4),
          "p value of ADF test of diff-diff-AR" 
          = round(c(d2adf.cpi@test$p.value,   d2adf.gold@test$p.value,
                    NA,                       d2adf.crt@test$p.value,
                    d2adf.unemp@test$p.value, d2adf.nloan@test$p.value, 
                    d2adf.export@test$p.value,d2adf.import@test$p.value,
                    NA, NA
                    ),4),
          "conclusion" 
          = c("lcpi~I(n)",    "gold~I(2)", 
              "gdp~I(1)",    "crt~I(n)",    
              "unemp~I(2)",  "nloan~I(n)", 
              "lexport~I(2)", "limport~I(n)", 
              "aord~I(1)",   "lexr~I(1)"
          ),
  )

# use 10% los, n>2

kable(Unit_Root_Test_table, align = "c") %>% 
  kable_styling(font_size = 8, 
                fixed_thead = TRUE, 
                full_width = FALSE, 
                position = "center",
                latex_options = c("HOLD_position"),
                bootstrap_options = c("striped", "hover", "bordered", "responsive", "dark"))

# all are first order diff
all_data         = na.omit(diff(all_data))

### Data plot after transformation

## Line plot for after transformation data
par(mfcol = c(5, 2), mar = c(2, 2, 2, 2))

for (i in 1:10) {
  ts.plot(all_data[, i], main = colnames(all_data)[i], 
          ylab = "", xlab = "", col = "darkgreen")
}

## compute CPI

inflation_table <-
  tibble( " " = c("Inflation rate"),
          "2023/03" = round((all_data[nrow(all_data) - 3, 1][[1]]/all_data[nrow(all_data) - 7, 1][[1]] -1 )*100, 2),
          "2023/06" = round((all_data[nrow(all_data) - 2, 1][[1]]/all_data[nrow(all_data) - 6, 1][[1]] -1 )*100 ,2),
          "2023/09" = round((all_data[nrow(all_data) - 1, 1][[1]]/all_data[nrow(all_data) - 5, 1][[1]] -1 )*100 ,2),
          "2023/12" = round((all_data[nrow(all_data), 1][[1]]/all_data[nrow(all_data) - 4, 1][[1]] -1 )*100 ,2)
          )
 
kable(inflation_table, align = "c") %>% 
  kable_styling(font_size = 8, 
                fixed_thead = TRUE, 
                full_width = FALSE, 
                position = "center",
                latex_options = c("HOLD_position"),
                bootstrap_options = c("striped", "hover", "bordered", "responsive", "dark"))


## gold data
cpi_last_12 <- tail(cpi_data, 12)
gold_last_12 <- tail(gold_data, 12)

par(mfcol = c(2, 1), mar=c(2,2,2,2))

plot(cpi_last_12, type="l", col="blue", xlab="Date", ylab="CPI Value", xlim=range(common_time_index))
title(main = "CPI last 3yrs", line = 0.5)

plot(gold_last_12, type="l", col="red", xlab="Date", ylab="Gold Price", xlim=range(common_time_index))
title(main = "Gold price 3yrs", line = 0.5)

## cash rate data

final2021.inf = (all_data[nrow(all_data) - 8, 1][[1]]/all_data[nrow(all_data) - 12, 1][[1]] -1 )*100
final2022.inf = (all_data[nrow(all_data) - 4, 1][[1]]/all_data[nrow(all_data) - 8, 1][[1]] -1 )*100
final2023.inf = (all_data[nrow(all_data), 1][[1]]/all_data[nrow(all_data) - 4, 1][[1]] -1 )*100

final2021.crt = (all_data[nrow(all_data) - 8, 3][[1]])
final2022.crt = (all_data[nrow(all_data) - 4, 3][[1]])
final2023.crt = (all_data[nrow(all_data), 3][[1]])

cash_rate_table <-
  tibble( " " = c("Inflation rate","Cash rate target"),
          "2021/12" = c(round((all_data[nrow(all_data) - 8, 1][[1]]/all_data[nrow(all_data) - 12, 1][[1]] -1 )*100, 2),
                        round((all_data[nrow(all_data) - 8, 3][[1]]), 2)),
          "2022/03" = c(round((all_data[nrow(all_data) - 7, 1][[1]]/all_data[nrow(all_data) - 11, 1][[1]] -1 )*100, 2),
                        round((all_data[nrow(all_data) - 7, 3][[1]]), 2)),
          "2022/06" = c(round((all_data[nrow(all_data) - 6, 1][[1]]/all_data[nrow(all_data) - 10, 1][[1]] -1 )*100 ,2),
                        round((all_data[nrow(all_data) - 6, 3][[1]]), 2)),
          "2022/09" = c(round((all_data[nrow(all_data) - 5, 1][[1]]/all_data[nrow(all_data) - 9, 1][[1]] -1 )*100 ,2),
                        round((all_data[nrow(all_data) - 5, 3][[1]]), 2)),
          "2022/12" = c(round((all_data[nrow(all_data) - 4, 1][[1]]/all_data[nrow(all_data) - 8, 1][[1]] -1 )*100 ,2),
                        round((all_data[nrow(all_data) - 4, 3][[1]]), 2)),
          "2022 annual change" = c(round((final2022.inf - final2021.inf)/final2021.inf ,2),
                                   round((final2022.crt - final2021.crt)/final2021.crt ,2)),
          "2023/03" = c(round((all_data[nrow(all_data) - 3, 1][[1]]/all_data[nrow(all_data) - 7, 1][[1]] -1 )*100, 2),
                        round((all_data[nrow(all_data) - 3, 3][[1]]), 2)),
          "2023/06" = c(round((all_data[nrow(all_data) - 2, 1][[1]]/all_data[nrow(all_data) - 6, 1][[1]] -1 )*100 ,2),
                        round((all_data[nrow(all_data) - 2, 3][[1]]), 2)),
          "2023/09" = c(round((all_data[nrow(all_data) - 1, 1][[1]]/all_data[nrow(all_data) - 5, 1][[1]] -1 )*100 ,2),
                        round((all_data[nrow(all_data) - 1, 3][[1]]), 2)),
          "2023/12" = c(round((all_data[nrow(all_data), 1][[1]]/all_data[nrow(all_data) - 4, 1][[1]] -1 )*100 ,2),
                        round((all_data[nrow(all_data), 3][[1]]), 2)),
          "2023 annual change" = c(round((final2023.inf - final2022.inf)/final2022.inf ,2),
                                   round((final2023.crt - final2022.crt)/final2022.crt ,2)),
  )

kable(cash_rate_table, align = "c") %>% 
  kable_styling(font_size = 8, 
                fixed_thead = TRUE, 
                full_width = FALSE, 
                position = "center",
                latex_options = c("HOLD_position"),
                bootstrap_options = c("striped", "hover", "bordered", "responsive", "dark"))
