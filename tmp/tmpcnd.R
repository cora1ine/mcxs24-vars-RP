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
crt_data     = apply.quarterly(crt_data,mean)     # change daily to quarterly



