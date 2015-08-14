Reproducible Research Peer Assessment 1
---------------------------------------

### Libraries used

    library(lubridate)
    library(sqldf)

    ## Loading required package: gsubfn
    ## Loading required package: proto
    ## Loading required package: RSQLite
    ## Loading required package: DBI

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lattice)

### Loading and cleaning data

The code below is used to first download the file from the website and a
temporary sace is created and unlinked after the download is complete.
Formats are then changed to make the date column usable for further
analysis.

    setwd("~/RWorkspace")

    #Create a temp space for ZIP file
    temp<-tempfile()
    fileUrl<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl,temp)
    ACT <- read.csv(unzip(temp, "activity.csv"),header = TRUE, sep = ",")
    unlink(temp)

    ## Change Colclasses to date
    ACT2<-ACT
    ACT2[,2]<-as.Date(ACT2[,2])

### What is mean total number of steps taken per day?

1.  First dataframe is aggregated to right level of granularity required
    to answer the question
2.  Then a histogram is created
3.  Then the mean and median is calculated using the codes below

<!-- -->

    ACT3<-sqldf("select date, sum(steps) as steps from ACT2 group by date")

    ## Loading required package: tcltk

    #Histogram is created based on the summarised data above
    hist(ACT3$steps,col="blue",
         main="Histogram of Total Steps per Day",
         xlab="Total Steps per Day")

![](Activity_PA_1_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    mean(ACT3$steps, na.rm = TRUE)

    ## [1] 10766.19

    median(ACT3$steps, na.rm = TRUE)

    ## [1] 10765

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

2.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    newdata <- aggregate( steps~interval, data = ACT2, FUN = mean)
    with( newdata, plot(interval,steps,
                        type="l",
                        col="blue",
                        main="Average Daily Activity Pattern",
                        xlab="5 min - interval",
                        ylab="Average Steps"))

![](Activity_PA_1_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    #calculate which interval has the maximim number of steps
    with(newdata,interval[which.max(steps)])

    ## [1] 835

### Imputing missing values

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

2.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

3.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

4.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    # get rows with NA's
    df_NA <- ACT2[!complete.cases(ACT2),]

    # number of rows
    nrow(df_NA)

    ## [1] 2304

The number of rows with NAs is **2304**

    # change colname
    colnames(newdata)[2]<-c("StepsInterval")

    newdata<- inner_join(ACT2, newdata) %>% # every row will contain the average steps for the interval StepsInterval from newdata
        mutate(steps=ifelse(is.na(steps),StepsInterval,steps)) %>%
        select(-StepsInterval) # this will drop the StepsInterval from the dataframe

    ## Joining by: "interval"

    # summarise and plot data
    newdata1 <- newdata %>% group_by(date) %>% summarise(dailyTotal=sum(steps))

    hist(newdata1$dailyTotal,
         col="red",
         main="Histogram of Total Steps per Day",
         xlab="Total Steps per Day")

![](Activity_PA_1_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    #calculate mean and median
    mean(newdata1$dailyTotal)

    ## [1] 10766.19

    median(newdata1$dailyTotal)

    ## [1] 10766.19

### Are there differences in activity patterns between weekdays and weekends?

using lattice plotting system, lets compare weekend and weekday activity
to identify any differences in activity

    newdata<-newdata %>% 
        mutate(day=as.factor(ifelse(wday(date) %in% c(1,7),"weekend","weekday")))

    #calculate mean of steps grouped by day and interval in 'newdata' dataset
    newdata<- newdata %>% group_by(day,interval) %>% summarise(meansteps=mean(steps))
    with (newdata, xyplot(meansteps ~ interval|day, type="l", ylab="Number of steps",layout=c(1,2)))

![](Activity_PA_1_files/figure-markdown_strict/unnamed-chunk-7-1.png)

It can therefore be seen that the weekdays activities are more
concentrated while weekend activities are more spread out.
