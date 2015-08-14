##Download Zip file and access data

setwd("~/RWorkspace")


#Create a temp space for ZIP file
temp<-tempfile()
fileUrl<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,temp)
#ACT <- read.csv(unzip(temp, "activity.csv"),colClasses = "character",header = TRUE, sep = ",")
ACT <- read.csv(unzip(temp, "activity.csv"),header = TRUE, sep = ",")
#unlink(temp)

library(lubridate)
library(sqldf)
library(dplyr)

## Change Colclasses to date
ACT2<-ACT
ACT2[,2]<-as.Date(ACT2[,2])

ACT3<-sqldf("select date, sum(steps) as steps from ACT2 group by date")
ACT4<-aggregate(ACT2$steps, by = list(ACT2$date), FUN=sum, na.rm=TRUE)
ACT5<-aggregate(ACT2$steps, by = list(ACT2$date), FUN=mean, na.rm=TRUE)
ACT6<-aggregate(ACT2$steps, by = list(ACT2$date), FUN=median, na.rm=TRUE)

colnames(ACT5)<-c("date", "steps")
colnames(ACT6)<-c("date", "steps")

#par(mfrow = c(1,3)) #To divide plot area for multiple charts
hist(ACT3$steps,     col="blue",
     main="Histogram of Total Steps per Day",
     xlab="Total Steps per Day")

mean(ACT3$steps, na.rm = TRUE)
median(ACT3$steps, na.rm = TRUE)

#Next question
newdata <- aggregate( steps~interval, data = ACT2, FUN = mean)
with( newdata, plot(interval,steps,
                    type="l",
                    col="blue",
                    main="Average Daily Activity Pattern",
                    xlab="5 min - interval",
                    ylab="Average Steps"))

with(newdata,interval[which.max(steps)])

colnames(newdata)[2]<-c("StepsInterval")

newdata<- inner_join(ACT2, newdata) %>% # every row will contain the average steps for the interval StepsInterval from newdata
    mutate(steps=ifelse(is.na(steps),StepsInterval,steps)) %>%
    select(-StepsInterval) # this will drop the StepsInterval from the dataframe

# summarise and plot data
newdata1 <- newdata %>% group_by(date) %>% summarise(dailyTotal=sum(steps))

hist(newdata1$dailyTotal,
     col="red",
     main="Histogram of Total Steps per Day",
     xlab="Total Steps per Day")

mean(newdata1$dailyTotal)
median(newdata1$dailyTotal)

newdata<-newdata %>% 
    mutate(day=as.factor(ifelse(wday(date) %in% c(1,7),"weekend","weekday")))

library(lattice)
newdata<- newdata %>% group_by(day,interval) %>% summarise(meansteps=mean(steps))
with (newdata, 
      xyplot(meansteps ~ interval|day, type="l", 
             ylab="Number of steps",layout=c(1,2)))

setwd("~/RWorkspace/Reproducible_Research")
render("PA1_template.RMD","md_document")