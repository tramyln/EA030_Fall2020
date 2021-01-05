read.csv("/home/CAMPUS/tlna2018/Air_Quality/Air_Quality/10312020_airquality_data.csv")

# READ CSV
filepath.csv = "/home/CAMPUS/tlna2018/Air_Quality/Air_Quality/10312020_airquality_data.csv"
rawdata = read.csv(filepath.csv)
names(rawdata)= c("X1", "X2", "Month", "Day", "Hour",
                  "Minute", "Second", "X3", "X4", "pm1_cf", "X5", "pm25_cf", "X6",
                  "pm10_cf", "X7", "pm1", "X8", "pm25", "pm10.", "X9")
rawdata$pm1_cf = as.numeric(gsub('[)]','', rawdata$pm1_cf))
## Warning: NAs introduced by coercion
rawdata$pm25_cf = as.numeric(gsub('[)]','', rawdata$pm25_cf))
## Warning: NAs introduced by coercion
rawdata$pm10_cf = as.numeric(gsub('[)]','', rawdata$pm10_cf))
## Warning: NAs introduced by coercion
as.Date(with(rawdata, paste("2020", Month, Day,sep="-")), "%Y-%m-%d")[1]
## [1] "2020-10-31"
library(lubridate)
##
## Attaching package: 'lubridate'
## The following object is masked from 'package:base':
##
## date
rawdata$DateTime = with(rawdata, ymd_hms(paste("2020", Month, Day, Hour, Minute, Second, sep= '-')))
## Warning: 11 failed to parse.

# CHECK DATA!
rawdata[sample(1:nrow(rawdata), 5),] # random 10 rows, all columns
##Remove Variables
cleandata = subset(rawdata, select=c(DateTime, pm1_cf, pm25_cf, pm10_cf))

# PLOT DATA!
## View patterns via histogram; what are the trends?
hist(cleandata$pm25_cf, xlab="PM2.5 concentration", main="Histogram of PM2.5 in Bellingham", las=1, xlim=c(0,400))
## Dive deeper by plotting a time series
plot(pm25_cf~DateTime, data=cleandata, pch=20, cex=.5)
## Add times to the plot with some rectancles for "nighttime" and see how different size particles behave by making three graphs
Ylab = expression(paste("PM (", mu,"g/m","^3",")"))
par(mfrow=c(3,1), mar=c(5,5,2,0)+.1, cex.lab=1.5)
plot(pm1_cf~DateTime, data=cleandata, pch=20, 
     cex=.5, las=1, ylab=Ylab, xlab="Day", 
     ylim=c(0,320), main="PM1.0")
#abline(h=35, col="red")

night.fun <- function(offset){
  offset=offset*24*60*60
  rect(as.POSIXct("2020-10-31 18:00:00 UTC")+offset, 
       -20, as.POSIXct("2020-10-31 6:00:00 UTC")+offset, 
       330, border = NA, col= rgb(0,0,1.0,alpha=0.1))
}

night.fun(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)) 

plot(pm25_cf~DateTime, data=cleandata, pch=20, 
     cex=.5, las=1, ylab=Ylab, xlab="Day", ylim=c(0,320), main="PM 2.5")
abline(h=35, col="red")
night.fun(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))  

plot(pm10_cf~DateTime, data=cleandata, pch=20, 
     cex=.5, las=1, ylab="PM2.5", xlab="Time", 
     ylim=c(0,320), main="PM 10")
abline(h=50, col="red")
night.fun(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)) 
