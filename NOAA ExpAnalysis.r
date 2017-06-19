## File Download, Read-in, and Descriptive Analysis

## Setup initial variables
MyWD <- "/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Intro to Data Science R Files/Reproducible Research - Peer Graded Assessments/RR-WK4-CP-2"
setwd(MyWD)
dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
documentationURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"
documfile <- "StormDataDocumentation.PDF"
compfilename <- "StormData.csv.bz2"
filename <- "StormData.csv"
DataName <- "StormData"

## Install packages
install.packages("R.utils") ## necessary for decompressing a .bz2 file
install.packages("dplyr")
library(R.utils)
library(dplyr)


## Download the Documentation for reference
if (!file.exists(documfile)){
  download.file(documentationURL, documfile)
}

## Download the Data and Read it into the Global Environment
if (!file.exists(filename)){
  if (!file.exists(compfilename)){
    download.file(dataURL, compfilename, method="curl")
  }
  bunzip2(compfilename, filename, remove = FALSE, skip = TRUE)
}
if (!exists(DataName)){
  StormData <- read.csv(filename, header = TRUE)
}

## Descriptive Analysis
names(StormData)
head(StormData)
event_types <- as.data.frame(table(StormData$EVTYPE))
event_types <- event_types[order(event_types$Var1), ]
head(event_types)
rm(event_types)

## Clean the Event Types
StormData$EVTYPE <- as.character(StormData$EVTYPE)
StormData$EVTYPE[grepl("/|&|and", StormData$EVTYPE,ignore.case = TRUE)] <- "Multiple Event"
StormData$EVTYPE[grepl("volc", StormData$EVTYPE,ignore.case = TRUE)] <- "Volcano"
StormData$EVTYPE[grepl("DROUGHT", StormData$EVTYPE,ignore.case = TRUE)] <- "Drought"
StormData$EVTYPE[grepl("tsunami", StormData$EVTYPE,ignore.case = TRUE)] <- "Tsnumai"
StormData$EVTYPE[grepl("LIGHTNING", StormData$EVTYPE,ignore.case = TRUE)] <- "Lightning"
StormData$EVTYPE[grepl("Avalanc*", StormData$EVTYPE,ignore.case = TRUE)] <- "Avalanche"
StormData$EVTYPE[grepl("*Fog", StormData$EVTYPE,ignore.case = TRUE)] <- "Fog"
StormData$EVTYPE[grepl("wind|wnd", StormData$EVTYPE,ignore.case = TRUE)] <- "Wind"
StormData$EVTYPE[grepl("funnel|tornado", StormData$EVTYPE,ignore.case = TRUE)] <- "Tornado"
StormData$EVTYPE[grepl("glaze", StormData$EVTYPE,ignore.case = TRUE)] <- "Glaze"
StormData$EVTYPE[grepl("hail", StormData$EVTYPE,ignore.case = TRUE)] <- "Hail"
StormData$EVTYPE[grepl("dust", StormData$EVTYPE,ignore.case = TRUE)]  <- "Dust"
StormData$EVTYPE[grepl("flood|*rising*|high water", StormData$EVTYPE,ignore.case = TRUE)] <- "Flood"
StormData$EVTYPE[grepl("ic(e|y)", StormData$EVTYPE,ignore.case = TRUE)] <- "Ice"
StormData$EVTYPE[grepl("fire|smoke", StormData$EVTYPE,ignore.case = TRUE)] <- "Fire"
StormData$EVTYPE[grepl("thunder", StormData$EVTYPE,ignore.case = TRUE)] <- "Non-Tropical Storm"
StormData$EVTYPE[grepl("Coastal*|Non-Tropical*|microburst", StormData$EVTYPE,
                       ignore.case = TRUE)] <- "Non-Tropical Storm"
StormData$EVTYPE[grepl("slide|eros", StormData$EVTYPE,ignore.case = TRUE)] <- "Erosion"
StormData$EVTYPE[grepl("rain", StormData$EVTYPE,ignore.case = TRUE)] <- "Rain"
StormData$EVTYPE[grepl("freez|cold|snow|chill|winter|blizzard|sleet|glaze|frost|hypothermia|wintry*|low temp*|mixed precip|ice", 
                      StormData$EVTYPE,ignore.case = TRUE)] <- "Cold Weather"
StormData$EVTYPE[grepl("TROPICAL.Storm|typhoon|*depression", StormData$EVTYPE,ignore.case = TRUE)] <- "Tropical Storm"
StormData$EVTYPE[grepl("(hurri|opal)", StormData$EVTYPE,ignore.case = TRUE)] <- "Tropical Storm"
StormData$EVTYPE[grepl("RIP CURREN(T|TS)", StormData$EVTYPE,ignore.case = TRUE)] <- "Rip Current"
StormData$EVTYPE[grepl("*Current|*SURF|*SEAS|marine*|*swells|*waves|waterspout|*tide", StormData$EVTYPE,ignore.case = TRUE)] <- "Nautical"
StormData$EVTYPE[grepl("heat", StormData$EVTYPE,ignore.case = TRUE)] <- "Heat"
StormData$EVTYPE[grepl("Storm SURGE", StormData$EVTYPE,ignore.case = TRUE)] <- "Storm Surge"

## Clean the Property and Crop Damage Data
unique(StormData$PROPDMGEXP)

## Convert exponents into numeric values
StormData$PROPDMG[StormData$PROPDMGEXP == "K"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "K"] * 1000
StormData$PROPDMG[StormData$PROPDMGEXP == "M"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "M"] * (10^6)
StormData$PROPDMG[StormData$PROPDMGEXP == "H"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "H"] * 100
StormData$PROPDMG[StormData$PROPDMGEXP == "h"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "h"] * 100
StormData$PROPDMG[StormData$PROPDMGEXP == ""] <- StormData$PROPDMG[StormData$PROPDMGEXP == ""] * 1
StormData$PROPDMG[StormData$PROPDMGEXP == "B"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "B"] * (10^9)
StormData$PROPDMG[StormData$PROPDMGEXP == "m"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "m"] * (10^6)
StormData$PROPDMG[StormData$PROPDMGEXP == "0"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "0"] * 1
StormData$PROPDMG[StormData$PROPDMGEXP == "1"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "1"] * 10
StormData$PROPDMG[StormData$PROPDMGEXP == "2"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "2"] * 100
StormData$PROPDMG[StormData$PROPDMGEXP == "3"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "3"] * 1000
StormData$PROPDMG[StormData$PROPDMGEXP == "4"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "4"] * (10^4)
StormData$PROPDMG[StormData$PROPDMGEXP == "5"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "5"] * (10^5)
StormData$PROPDMG[StormData$PROPDMGEXP == "6"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "6"] * (10^6)
StormData$PROPDMG[StormData$PROPDMGEXP == "7"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "7"] * (10^7)
StormData$PROPDMG[StormData$PROPDMGEXP == "8"] <- StormData$PROPDMG[StormData$PROPDMGEXP == "8"] * (10^8)
StormData$PROPDMG[StormData$PROPDMGEXP == "+"] <- 0
StormData$PROPDMG[StormData$PROPDMGEXP == "-"] <- 0
StormData$PROPDMG[StormData$PROPDMGEXP == "?"] <- 0
head(StormData[,c("EVTYPE","PROPDMG","PROPDMGEXP")])

StormData$CROPDMG[StormData$CROPDMGEXP == "M"] <- StormData$CROPDMG[StormData$CROPDMGEXP == "M"] * (10^6)
StormData$CROPDMG[StormData$CROPDMGEXP == "K"] <- StormData$CROPDMG[StormData$CROPDMGEXP == "K"] * 1000
StormData$CROPDMG[StormData$CROPDMGEXP == "m"] <- StormData$CROPDMG[StormData$CROPDMGEXP == "m"] * (10^6)
StormData$CROPDMG[StormData$CROPDMGEXP == "B"] <- StormData$CROPDMG[StormData$CROPDMGEXP == "B"] * (10^9)
StormData$CROPDMG[StormData$CROPDMGEXP == "k"] <- StormData$CROPDMG[StormData$CROPDMGEXP == "k"] * 1000
StormData$CROPDMG[StormData$CROPDMGEXP == "0"] <- StormData$CROPDMG[StormData$CROPDMGEXP == "0"] * 1
StormData$CROPDMG[StormData$CROPDMGEXP == "2"] <- StormData$CROPDMG[StormData$CROPDMGEXP == "2"] * 100
StormData$CROPDMG[StormData$CROPDMGEXP == ""] <- StormData$CROPDMG[StormData$CROPDMGEXP == ""] * 1
StormData$CROPDMG[StormData$CROPDMGEXP == "?"] <- 0
head(StormData[,c("EVTYPE","CROPDMG","CROPDMGEXP")])

## Aggregation and Analysis

## Public Health Consequences
fatal <- aggregate(FATALITIES~EVTYPE,data=StormData,FUN=sum,na.rm=TRUE)
fatal <- fatal[with(fatal,order(-FATALITIES)),]
fatal <- head(fatal, 10)
injury <- aggregate(INJURIES~EVTYPE,data=StormData,FUN=sum,na.rm=TRUE)
injury <- injury[with(injury,order(-INJURIES)),]
injury <- head(injury,10)

## Public Health Results
par(mfrow=c(1,2),mar=c(8,5,4,2))
##plot the graph showing the top 10 fatalities and injuries
barplot(fatal$FATALITIES/(10^3),names.arg=fatal$EVTYPE,las=2,col="black",ylab="Fatalities (Thousands)",main="Top 10 Causes of Fatalities")
barplot(injury$INJURIES/(10^3),names.arg=injury$EVTYPE,las=2,col="red",ylab="Injuries (Thousands)",main="Top 10 Causes of Injuries")

## Economic Consequences
prop <- aggregate(PROPDMG~EVTYPE,data=StormData,FUN=sum,na.rm=TRUE)
prop <- prop[with(prop,order(-PROPDMG)),]
prop <- head(prop,10)
print(prop)
crop <- aggregate(CROPDMG~EVTYPE,data=StormData,FUN=sum,na.rm=TRUE)
crop <- crop[with(crop,order(-CROPDMG)),]
crop <- head(crop,10)
print(crop)

## Economic Results 
par(mfrow=c(1,2),mar=c(8,5,4,3))
##plot the graph showing the top 10 property and crop damages
barplot(prop$PROPDMG/(10^9),names.arg=prop$EVTYPE,las=2,col="green",ylab="Prop.damage(billions)",main="Top 10 Causes of Prop. Damage")
barplot(crop$CROPDMG/(10^9),names.arg=crop$EVTYPE,las=2,col="yellow",ylab="Crop damage(billions)",main="Top 10 Causes of Crop Damage")