library(ggplot2)
library(dplyr)
library(data.table)
#file <- file.path("repdata_data_StormData.zip")

#if (!file.exists(file)){
#  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#  download.file(url, file, method = "curl")
#}

# unzip file into data folder
#if (file.exists(file)){
#  unzip(file, exdir='./data')
#}

data_raw <- fread("repdata_data_StormData.csv", select = c("BGN_DATE", "STATE","EVTYPE","FATALITIES",
                                                           "INJURIES", "PROPDMG", "PROPDMGEXP"))

data <- na.omit(data_raw)

health <- data.frame(as.Date(data$BGN_DATE, format = "%m/%d/%Y"), data$STATE, to.lower(data$EVTYPE), as.numeric(data$FATALITIES), as.numeric(data$INJURIES))
colnames(health) <- c("date", "state", "event", "fatalities", "injuries")
health_fatalities <- health %>%
  select(date, state, fatalities) %>%
  filter(fatalities > 0) %>%
  group_by(event) %>%
  summarise_each(funs(sum))

health_injuries_agg <- aggregate(injuries ~ event, health, sum)
health_sort <- data.frame(apply(health_injuries_agg,2,sort,decreasing=T))

prop_dmg <- data.frame(as.Date(data$BGN_DATE, format = "%m/%d/%Y"), data$STATE, data$EVTYPE, as.numeric(data$PROPDMG), as.character(data$PROPDMGEXP))
colnames(prop_dmg) <- cbind("date", "state", "event", "prop-dmg", "prop-dmg-x")

## 