library(ggplot2)
library(GGally)
library(dplyr)
library(rworldmap)
crime2011 <- read.csv2(file = "crime_incidents_2011_CSV.csv",sep = ",")
crime2011 <- tbl_df(crime2011)
crime2012 <- read.csv2(file = "crime_incidents_2012_CSV.csv",sep = ",")
crime2012 <- tbl_df(crime2012)
dim(crime2011)
dim(crime2012)
g_offense_crime2011<- group_by(crime2011,OFFENSE)
g_offense_crime2012<- group_by(crime2012,OFFENSE)
offense2012 <- summarise(g_offense_crime2012,count = n())
offense2011 <- summarise(g_offense_crime2011,count = n())
offense2011 <- tbl_df(offense2011)
offense2012 <- tbl_df(offense2012)
total_offense <- merge(offense2011,offense2012,by = "OFFENSE")
names(total_offense) <- c("OFFENSE","2011","2012")
total_offense <- mutate(total_offense,difference = total_offense$`2012` - total_offense$`2011`,
                        changePercent = (total_offense$`2012`/total_offense$`2011`-1)*100)

total_offense
g <- ggplot(total_offense, aes(OFFENSE, changePercent))
g + geom_bar(stat = "identity")


gbyshift2011 <- group_by(crime2011,SHIFT,OFFENSE)
shift2011 <- summarise(gbyshift2011,number = n())
gbyshift2012 <- group_by(crime2012,SHIFT,OFFENSE)
shift2012 <- summarise(gbyshift2012,number = n())
shiftmerged <- merge(shift2011,shift2012,by = c('OFFENSE','SHIFT'))
shiftmerged <- mutate(shiftmerged,diff = (shiftmerged$number.y - shiftmerged$number.x),
percent = (diff/shiftmerged$number.x)*100)

ggplot(shiftmerged, aes(shiftmerged$OFFENSE, shiftmerged$percent)) + geom_bar(stat = "identity",aes(fill = shiftmerged$SHIFT))
