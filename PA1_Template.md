---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First load the libraries needed in this assessment


```r
library(dplyr)
library(lubridate)
library(lattice)
```

Read in the data set and remove the rows with steps=NA


```r
activity <- read.csv("./activity.csv")
activity2 <- filter(activity,is.na(steps)==FALSE)
```



## What is mean total number of steps taken per day?

Calculate the total number of steps each day and produce a histogram
of the results


```r
activity2 <- group_by(activity2,date)
act_per_day <- summarize(activity2,steps_per_day = sum(steps))
hist(act_per_day$steps_per_day,main="Histogram of Total Steps per Day",xlab = "Total Steps per Day")
```

![plot of chunk first_hist](figure/first_hist-1.png) 

Calculate and report the mean and median of the total number of steps taken per day


```r
mn <- mean(act_per_day$steps_per_day)
med <- median(act_per_day$steps_per_day)
```

The mean steps per day is 1.0766189 &times; 10<sup>4</sup>.
The median steps per day is 10765.

## What is the average daily activity pattern?
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activity3 <- filter(activity,is.na(steps)==FALSE)
activity3 <- group_by(activity3,interval)
act_by_time <- summarize(activity3,steps_by_time = mean(steps))
plot(act_by_time,type="l")
```

![plot of chunk first_time_series](figure/first_time_series-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
max_start <-act_by_time$interval[act_by_time$steps_by_time==max(act_by_time$steps_by_time)]
max_fin <- max_start + 5
```

The period with the maximum number of steps is between 835 and 840 minutes


## Imputing missing values

Calculate and report the total number of missing values in the dataset 


```r
activity_na <- filter(activity,is.na(steps)==TRUE)
activity_good <- filter(activity,is.na(steps)==FALSE)
missing <- nrow(activity_na)
```

There are 2304 rows with missing values in the dataset


Fill in the missing values by setting them equal to the mean for that 5 minute period over all days


```r
activity_na <- left_join(activity_na,act_by_time,by="interval")
activity_na$steps <- round(activity_na$steps_by_time)
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
activity_na <- select(activity_na,-steps_by_time)
activity_na$steps <- as.integer(activity_na$steps)
activity_na$interval <- as.integer(activity_na$interval)
activity_all <- rbind(activity_na,activity_good)
activity_all <- arrange(activity_all,date,interval)
```

Make a histogram of the total number of steps taken each day now that the missing values have been filled in.


```r
activity_all <- group_by(activity_all,date)
act_per_day2 <- summarize(activity_all,steps_per_day = sum(steps))
hist(act_per_day2$steps_per_day,main="Histogram of Total Steps per Day (Missing Values Replaced)",xlab = "Total Steps per Day")
```

![plot of chunk second_hist](figure/second_hist-1.png) 

Calculate and report the mean and median total number of steps taken per day.


```r
mn2 <- mean(act_per_day2$steps_per_day)
med2 <- median(act_per_day2$steps_per_day)
```

The mean steps per day (now that missing values have been filled in) is 1.0765639 &times; 10<sup>4</sup>.
The median steps per day (now that missing values have been filled in) is 10762.


Calculate the impact of imputing missing data on the estimates of the total daily number of steps by comparing with the previous means and medians we calculated.


```r
diffmean <- mn2-mn
diffmed <- med2 - med
```

The difference in the means by filling in missing values is -0.549335. 
The difference in the medians by filling in missing values is -3


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



```r
activity_all$date <- ymd(activity_all$date)
activity_all$day <- weekdays(activity_all$date)
activity_all$weekend[activity_all$day == "Saturday" | activity_all$day == "Sunday"] = "weekend"
activity_all$weekend[is.na(activity_all$weekend) == TRUE] <- "weekday"
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
activity_all <- group_by(activity_all,interval,weekend)
activity_weekend <- summarize(activity_all,steps = mean(steps))
xyplot(steps~interval|weekend,activity_weekend,type="l")
```

![plot of chunk second_time_series](figure/second_time_series-1.png) 

