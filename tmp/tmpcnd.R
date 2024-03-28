##### RP1

# install.packages("readabs")
# install.packages("readrba")
library(readabs)
library(readrba)
library(xts)

### Data downloading

# Inflation / CPI 
# 6401.0 Consumer Price Index, Australia
# series_id = "A2325846C": Index Numbers ;  All groups CPI ;  Australia ;
cpi_download = read_abs(series_id = "A2325846C")     
cpi_data     = xts(cpi_download$value, cpi_download$date)

# GDP
# 5206.0 Australian National Accounts: National Income, Expenditure and Product
# series_id = "A2304370T": Gross domestic product: Chain volume measures - Percentage changes
gdp_download = read_abs(series_id = "A2304370T")     
gdp_data     = xts(cpi_download$value, cpi_download$date)

# Cash rate target

crt_download = read_rba(series_id = "FIRMMCRTD")   
crt_data     = xts(crt_download$value, crt_download$date)
crt_data     = apply.quarterly(crt_data, mean)     # change daily to quarterly
crt_data     = xts(crt_data, seq(as.Date("1990-03-01"), by = "quarter", length.out = length(crt_data)))

# Unemployment rate
# 6202.0 Labour Force, Australia
# series_id = "A84423050A": Unemployment rate ;  Persons ; seasonal adjust
unemp_download = read_abs(series_id = "A84423050A")     
unemp_data     = xts(unemp_download$value, unemp_download$date)
unemp_data     = apply.quarterly(unemp_data, mean) # change daily to quarterly
unemp_data     = xts(unemp_data, seq(as.Date("1978-03-01"), by = "quarter", length.out = length(unemp_data)))

# Export
# 5368.0 International Trade in Goods
# series_id = "A2718603V": Debits, Total goods ;
export_download   = read_abs(series_id = "A2718603V")     
export_data    = xts(export_download$value, export_download$date)
export_data   = abs(export_data)
export_data    = apply.quarterly(export_data, mean)
 
# Import
# 5368.0 International Trade in Goods
# series_id = "A2718603V": Credits, Total goods ;
import_download   = read_abs(series_id = "A2718603V")     
import_data    = xts(import_download$value, import_download$date)
import_data    = apply.quarterly(import_data, mean)

#
# 5601.0 Lending Indicators
# series_id = "A108296973X"： Households ;  Housing Finance ;  Total housing excluding refinancing ;  New loan commitments ;  Value ; seasonal adjust
nloan_download   = read_abs(series_id = "A108296973X")     
nloan_data    = xts(nloan_download$value, nloan_download$date)
nloan_data    = apply.quarterly(nloan_data, mean)
