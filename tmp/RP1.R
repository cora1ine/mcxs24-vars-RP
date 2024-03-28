##### RP1

# install.packages("readabs")
# install.packages("readrba")
library(readabs)
library(readrba)
library(xts)

### Data downloading

# 1.Inflation / CPI 
# 6401.0 Consumer Price Index, Australia
# series_id = "A2325846C": Index Numbers ;  All groups CPI ;  Australia ;
cpi_download = read_abs(series_id = "A2325846C")     
cpi_data     = xts(cpi_download$value, cpi_download$date)

# 2.GDP
# 5206.0 Australian National Accounts: National Income, Expenditure and Product
# series_id = "A2304370T": Gross domestic product: Chain volume measures - Percentage changes
gdp_download = read_abs(series_id = "A2304370T")     
gdp_data     = xts(cpi_download$value, cpi_download$date)

# 3.Cash rate target

crt_download = read_rba(series_id = "FIRMMCRTD")   
crt_data     = xts(crt_download$value, crt_download$date)
crt_data     = apply.quarterly(crt_data, mean)     # change daily to quarterly
crt_data     = xts(crt_data, seq(as.Date("1990-03-01"), by = "quarter", length.out = length(crt_data)))

# 4.Wages (private sector wages)
# 5676.0 Business Indicators, Australia
# series_id = "A3531262C": Wages ;  Total (State) ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ; seasonal adjust
wages_download   = read_abs(series_id = "A3531262C")     
wages_data    = xts(wages_download$value, wages_download$date)

# 5.Unemployment rate
# 6202.0 Labour Force, Australia
# series_id = "A84423050A": Unemployment rate ;  Persons ; seasonal adjust
unemp_download = read_abs(series_id = "A84423050A")     
unemp_data     = xts(unemp_download$value, unemp_download$date)
unemp_data     = apply.quarterly(unemp_data, mean) # change daily to quarterly
unemp_data     = xts(unemp_data, seq(as.Date("1978-03-01"), by = "quarter", length.out = length(unemp_data)))

# 6.Export
# 5368.0 International Trade in Goods
# series_id = "A2718603V": Debits, Total goods ;
export_download   = read_abs(series_id = "A2718603V")     
export_data    = xts(export_download$value, export_download$date)
export_data   = abs(export_data)
export_data    = apply.quarterly(export_data, mean)
 
# 7.Import
# 5368.0 International Trade in Goods
# series_id = "A2718603V": Credits, Total goods ;
import_download   = read_abs(series_id = "A2718603V")     
import_data    = xts(import_download$value, import_download$date)
import_data    = apply.quarterly(import_data, mean)

# 8.New loan
# 5601.0 Lending Indicators
# series_id = "A108296973X"： Households ;  Housing Finance ;  Total housing excluding refinancing ;  New loan commitments ;  Value ; seasonal adjust
nloan_download   = read_abs(series_id = "A108296973X")     
nloan_data    = xts(nloan_download$value, nloan_download$date)
nloan_data    = apply.quarterly(nloan_data, mean)


# 9.Gold price
gold_link = "https://query1.finance.yahoo.com/v7/finance/download/GC%3DF?period1=1262304000&period2=1703980800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true"
gold_download = read.csv(gold_link)
gold_data = gold_download[,6]
gold_data <- data.frame(gold_download[,1], gold_data)
colnames(gold_data) <- c('date', 'gol')
gold_data$date <- as.Date(as.character(gold_data$date),format="%Y-%m-%d") 
gold_data    = xts(gold_data$gol, gold_data$date)
gold_data    = apply.quarterly(gold_data, mean)

# 10.AORD
aord_link = "https://query1.finance.yahoo.com/v7/finance/download/%5EAORD?period1=1262304000&period2=1703980800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true"
aord_download = read.csv(aord_link)
aord_data = aord_download[,6]
aord_data <- data.frame(aord_download[,1], aord_data)
colnames(aord_data) <- c('date', 'aord')
aord_data$date <- as.Date(as.character(aord_data$date),format="%Y-%m-%d") 
aord_data    = xts(aord_data$aord, aord_data$date)
aord_data    = apply.quarterly(aord_data, mean)

# 11. AUD/USD
exr_link = "https://query1.finance.yahoo.com/v7/finance/download/AUDUSD%3DX?period1=1262304000&period2=1703980800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true"
exr_download = read.csv(exr_link)
exr_data = exr_download[,6]
exr_data <- data.frame(exr_download[,1], exr_data)
colnames(exr_data) <- c('date', 'exr')
exr_data$date <- as.Date(as.character(exr_data$date),format="%Y-%m-%d") 
exr_data    = xts(exr_data$exr, exr_data$date)
exr_data    = apply.quarterly(exr_data, mean)
