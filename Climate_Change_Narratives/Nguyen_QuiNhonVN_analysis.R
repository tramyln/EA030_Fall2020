read.csv("/home/CAMPUS/tlna2018/Climate_Change_Narratives/Student_Folders/Nguyen/Tramy_QuiNhonVN_data.csv")
##should produce a list of values
climate_data <- read.csv("/home/CAMPUS/tlna2018/Climate_Change_Narratives/Student_Folders/Nguyen/Tramy_QuiNhonVN_data.csv")
##puts data into a data frame

#Path: "/home/CAMPUS/tlna2018/Climate_Change_Narratives/Student_Folders/Nguyen/Tramy_QuiNhonVN_data.csv"
##object should show up in environment under 'data'

head(climate_data)
##to confirm readings by listing first 6 observations; data placed in columns
str(climate_data)
##gives you structure of data (more info to evaluate data set)

names(climate_data)
##to confirm column names
##nameofdataframe$columnname --> to access data in one of the columns specifically
##ex. climate_data$TMAX --> R will spit out data for just this column

plot(TAVG~DATE, climate_data)
##to check data by plotting it
###originally for TMAX, but too many missing values
min(climate_data$TAVG)
##will show "NA" if data is missing
min(climate_data$TAVG, na.rm=T)
##gets function to cooperate if there are missing values

#FOR TMIN AND TMAX
climate_data$TMAX[climate_data$TMAX==-9999] = NA
climate_data$TMIN[climate_data$TMIN==-9999] = NA
##these two replace missing values values with NA

#FOR TAVG
climate_data$TAVG[climate_data$TAVG==-9999] = NA

strDates <- as.character(climate_data$DATE)
climate_data$NewDate <- as.Date(strDates, "%Y-%m-%d")
##these two reformat dates to the yyyy-mm-dd format

plot(TAVG~NewDate, climate_data[1:21146,], ty='l')
##to check new dates

#Creating a line of best fit
lm(TAVG ~ NewDate, data=climate_data)
##calls linear model
TAVG.lm = lm(TAVG ~ NewDate, data= climate_data)
##puts linear model into an object called "TVG.lm"
slopeintercept = coef(TAVG.lm)
##puts coefficients of the linear model into an object called "slope intercept"
plot(TAVG ~ NewDate, data= climate_data, las=1)
## las=1 positions the text horizontally
abline(slopeintercept, col="red", lwd=3)
## lwd denotes line width

##SCRAPPED BESTFITLINE CODE##
#bestfitline.lm = lm(TAVG~NewDate, climate_data[1:21146])
##the numbers in brackets represents the desired oberservations (in this case, there are 21146 observations in the data)
#summary(bestfitline.lm)
#coef(bestfitline.lm)
#abline(coef(bestfitline.lm))

#Interpreting the Results
(TAVG.lm.sum = summary(lm(TAVG ~ NewDate, data=climate_data)))
##spits out statistical values in ANOVA format

#Creating Monthly Means
climate_data$Month = format(as.Date(climate_data$NewDate), format = "%m")
climate_data$Year = format(climate_data$NewDate, format="%Y")
##Disagragates the NewDate variable into a month and year variables
MonthlyTAVGMean = aggregate(TAVG ~ Month + Year, climate_data, mean)
MonthlyTAVGMean$YEAR = as.numeric(MonthlyTAVGMean$Year)
MonthlyTAVGMean$MONTH = as.numeric(MonthlyTAVGMean$Month)
## aggregate() calculates the mean
str(MonthlyTAVGMean)
##year and month should each have two vectors
plot(MonthlyTAVGMean$TAVG, ty=l)
##Reminder: When we define a plot type, (ty = ), we are defining it as a line which is abreviated as the letter 'l', not the number '1'.

#SELECTING FOR 1 MONTH - JANUARY
#plot(MonthlyTAVGMean$TAVG[MonthlyTAVGMean$Month=="05"], ty='l')
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="01",], ty='l', xlim=c(1957, 2020))
January.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="01",])
summary(January.lm)
abline(coef(January.lm), col="red")

#FEBRUARY
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="02",], ty='l', xlim=c(1957, 2020))
February.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="02",])
summary(February.lm)

#MARCH
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="03",], ty='l', xlim=c(1957, 2020))
March.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="03",])
summary(March.lm)

#APRIL
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="04",], ty='l', xlim=c(1957, 2020))
April.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="04",])
summary(April.lm)

#MAY
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="05",], ty='l', xlim=c(1957, 2020))
May.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="05",])
summary(May.lm)

#JUNE
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="06",], ty='l', xlim=c(1957, 2020))
June.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="06",])
summary(June.lm)

#JULY
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="07",], ty='l', xlim=c(1957, 2020))
July.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="07",])
summary(July.lm)

#AUGUST
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="08",], ty='l', xlim=c(1957, 2020))
August.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="08",])
summary(August.lm)

#SEPTEMBER
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="09",], ty='l', xlim=c(1957, 2020))
September.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="09",])
summary(September.lm)

#OCTOBER
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="10",], ty='l', xlim=c(1957, 2020))
October.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="10",])
summary(October.lm)

#NOVEMBER
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="11",], ty='l', xlim=c(1957, 2020))
November.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="11",])
summary(November.lm)

#DECEMBER
plot(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="12",], ty='l', xlim=c(1957, 2020))
December.lm <- lm(TAVG~YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$Month=="12",])
summary(December.lm)

#ALL MONTHS AT ONCE
##use monthly means code and adjust accordingly for max, min, and avg
## First create a vector of months
Months = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
# Then create a panel to see all the figures at once
par(mfrow = c(4, 3), mar = c(5, 4, 3, 2) + 0.1)
TAVGresult <- NA
for (i in 1:12) {
  # plot(MonthlyTMAXMean$TMAX[MonthlyTMAXMean$Month==i], ty='l')
  plot(TAVG ~ YEAR, data = MonthlyTAVGMean[MonthlyTAVGMean$MONTH == i, ], ty = "l", las = 1, xlim = c(1957, 2020), main = Months[i])
  Month.lm <- lm(TAVG ~ YEAR, data = MonthlyTAVGMean[MonthlyTAVGMean$MONTH == i, ])
  summary(Month.lm)
  
  abline(coef(Month.lm), col = "red")
  
  TAVGresult <- rbind(TAVGresult, cbind(Months[i], round(coef(Month.lm)[2], 4), round(summary(Month.lm)$coefficients[2,4], 4), round(summary(Month.lm)$r.squared, 3)))
}

#Table of Estimated Slopes
##First, create a 12 month display
##Then create the table (the table only displays the numeric P-value...)
library(xtable)
Results <- data.frame(Month = TAVGresult[c(2:13),1], TAVGSlope = TAVGresult[c(2:13),2], TAVG_P = as.numeric(TAVGresult[c(2:13),3]), TAVGRsq = TAVGresult[c(2:13),4])
Results$starTAVG = "NS"
Results$starTAVG[Results$TAVG_P <= .05] = "*"
Results$starTAVG[Results$TAVG_P < 0.01] = "**"
Results$starTAVG[Results$TAVG_P < 0.001] = "***"
Results$TAVGSlope=paste(Results$TAVGSlope, Results$starTAVG)
colnames(Results) <- c("Month", "2", "R^2", "4", "Slope TAVG")
print(xtable(Results[c(1, 5, 3)]))

#KATY'S CODE FOR HTMLTABLE [MODIFIED]
Results <- data.frame(Month = TAVGresult[,1],
                      TAVGSlope = TAVGresult[,2],
                      TAVG_P = as.numeric(TAVGresult[,3]),
                      TAVGRsq = TAVGresult[,4])
Results$starTAVG <- ifelse(Results$TAVG_P <= .001, "***", 
                           ifelse(Results$TAVG_P <= .01, "**",
                                  ifelse(Results$TAVG_P <= .05, "*", "NS")))
Results$TAVGSlope = paste(Results$TAVGSlope, Results$starTAVG)
Results <- Results[c(2:13), c(1,2,3,4)]
colnames(Results) = c("Month", "Slope TAVG", "P value", "R^2")
library(htmlTable)
htmlTable(Results)

#XTABLE EXPERIMENT [MODIFIED]
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
Results <- data.frame(Month = TAVGresult[,1],
                      TAVGSlope = TAVGresult[,2],
                      TAVG_P = as.numeric(TAVGresult[,3]),
                      TAVGRsq = TAVGresult[,4])
Results$starTAVG <- ifelse(Results$TAVG_P <= .001, "***", 
                           ifelse(Results$TAVG_P <= .01, "**",
                                  ifelse(Results$TAVG_P <= .05, "*", "NS")))
Results$TAVGSlope = paste(Results$TAVGSlope, Results$starTAVG)
Results <- Results[c(2:13), c(1,2,3,4)]
colnames(Results) = c("Month", "Slope TAVG", "P value", "R^2")
##Codes below don't culminate in an exported graph; rely on xtable(Results) for code output
print.xtable(Results)
align(Results) <- xalign(Results)
digits(Results) <- xdigits(Results)
display(Results) <- xdisplay(Results)

#KABLE EXPERIMENT [MODIFIED]
library(knitr)
Results <- data.frame(Month = TAVGresult[,1],
                      TAVGSlope = TAVGresult[,2],
                      TAVG_P = as.numeric(TAVGresult[,3]),
                      TAVGRsq = TAVGresult[,4])
Results$starTAVG <- ifelse(Results$TAVG_P <= .001, "***", 
                           ifelse(Results$TAVG_P <= .01, "**",
                                  ifelse(Results$TAVG_P <= .05, "*", "NS")))
Results$TAVGSlope = paste(Results$TAVGSlope, Results$starTAVG)
Results <- Results[c(2:13), c(1,2,3,4)]
colnames(Results) = c("Month", "Slope TAVG", "P value", "R^2")
rownames(Results) <- NULL
kable(Results, padding=-1L, caption="A Knitr Table")

#DIAGNOSTIC PLOTS
par(mfrow=c(2,2))
plot(lm(TAVG ~ YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$MONTH==1,]))
##shortcut
diagnostic.lm <- lm(TAVG ~ YEAR, data=MonthlyTAVGMean[MonthlyTAVGMean$MONTH==1,])
plot(diagnostic.lm)
