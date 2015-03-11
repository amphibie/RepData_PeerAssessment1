---
title: "PA1_template.Rmd"
author: "Paul Foellbach"
date: "Tuesday, March 10, 2015"
output: html_document
---

##Reproducible Research: Peer Assessment 1

###Basic settings

```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
```

###Loading and processing the data    
1. Load the data (i.e. read.csv())

```r
unzip("activity_data.zip", exdir = "activity_data")
```
List.files with the argument full.names=TRUE gives the filename and the path there from working directory

```r
file <- list.files("activity_data", full.names=TRUE)
file
```

```
## [1] "activity_data/activity.csv"
```
The following statement is used to load the data using read.csv().

```r
amd <- read.csv(file, header = TRUE, sep = ",",
                colClasses=c("numeric", "character", "numeric"))
```
     
2. Process/transform the data into a format that is suitable for my analysis                 
We convert the date field to Date class.

```r
amd$date <- as.Date(amd$date, format = "%Y-%m-%d")
```
We remove incomplete cases.

```r
amd.ignore.na <- na.omit(amd) 
```

###What is mean total number of steps taken per day?
Here we ignore the missing values.     
1. We calculate the total number of steps per day.

```r
steps_per_day <- aggregate(steps ~ date, amd.ignore.na, sum)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
     
2 We make a histogram of the total number of steps taken per day, plotted with appropriate breaks number.

```r
hist(steps_per_day$steps, main = paste("Total number of steps each day"), col="blue", xlab="Total number of steps", breaks=30)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_per_day$steps) 
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps) 
```

```
## [1] 10765
```
The mean is 10766.19 and median is 10765.

###What is the average daily activity pattern ?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken across all days (y-axis)      

2. Report which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.
Observe and comment the average daily activity pattern

Calculate average steps for each of 5-minute interval aross all days

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.2
```

```r
interval_mean_steps <- ddply(amd.ignore.na,~interval, summarise, mean=mean(steps))
```

Plot time series of the 5-minute interval and the average number of steps taken across all days

```r
plot(interval_mean_steps$interval,interval_mean_steps$mean, type= "l", main="Mean step number taken in each 5-minute-intervall across all days" ,xlab= "5-Minute Interval", ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

Report the 5-min interval, that contains the maximum mean of number of steps:

```r
interval_mean_steps[which.max(interval_mean_steps$mean), ]
```

```
##     interval     mean
## 104      835 206.1698
```

Observation:  
The maximum mean of step number is located at 5-min Interval 835. 

###Imputing missing values

Note that there are a number of days/intervals where there are
missing values (coded as NA). The presence of missing days may 
introduce bias into some calculations or summaries of the data. 
In this section:   
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).       
2. Devise a strategy for filling in all of the missing values in the dataset.     
For this assignment the strategy is to use the 
mean for that 5-minute interval to replace missing values.      
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.     
4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.     
Make following comments: Do these values differ from the 
estimates from the first part of the assignment ? 
What is the impact of imputing missing data on the estimates 
of the total daily number of steps ?

Calculate and report the total number of missing values in 
the dataset (i.e. the total number of rows with NAs)

```r
na_amd <- sum(is.na(amd$steps))
na_amd
```

```
## [1] 2304
```
The total number of missing values are 2304.

Strategy for filling in all of the missing values 
in the dataset to populate missing values, we choose to replace them with 
the mean value at the same interval across all days. In most of the 
cases the median is a better centrality measure than mean, 
but in our case the total median is not much far away from 
total mean.

```r
df_impute <- amd
ndx <- is.na(df_impute$steps)
int_avg <- tapply(amd.ignore.na$steps,amd.ignore.na$interval, mean, na.rm=TRUE, simplify=T)
df_impute$steps[ndx] <- int_avg[as.character(df_impute$interval[ndx])]
```
We check that are there any missing values remaining or not

```r
sum(is.na(df_impute$steps))
```

```
## [1] 0
```
We see that there are no missing values.    
Calculate the total number of steps taken each day with missing data imputed and assign to new_steps_per_day

```r
new_steps_per_day <- aggregate(steps ~ date, df_impute, sum)
head(new_steps_per_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```
Make a histogram of the total number of steps taken each day
with plot

```r
hist(new_steps_per_day$steps, main = paste("Total number of steps each day (with missing data imputed)"), col="blue", xlab="Total number of steps", breaks=30)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 

Report mean and median total number of steps taken per day:

```r
mean(new_steps_per_day$steps) 
```

```
## [1] 10766.19
```

```r
median(new_steps_per_day$steps) 
```

```
## [1] 10766.19
```

Based on the imputed data set the new mean and median is 10766.19. 
Compared with the original mean 10766 and median 10765 the mean doesn't change 
and the median has a small change. In fact, the new median becomes identical to the mean. 
One possible explanation is that when we fill the missing data for the intervals, 
we use means for intervals, so we have more data close or identical to the means,
and median is shifted towards the mean and becomes identical to the mean.
The impact of imputing missing data on the estimates of the total daily number of steps is also clear: 
now we have higher frequency counts in the histogram at the center region (close to the mean).

###Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. We use the dataset with the filled-in missing values for this part.    
1. We create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating 
whether a given date is a weekday or weekend day.

```r
head(df_impute)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
df_impute$weekdays <- factor(format(df_impute$date, "%A"))
levels(df_impute$weekdays)
```

```
## [1] "Dienstag"   "Donnerstag" "Freitag"    "Mittwoch"   "Montag"    
## [6] "Samstag"    "Sonntag"
```

```r
levels(df_impute$weekdays) <- list(weekday = c("Montag", "Dienstag",
                                             "Mittwoch", 
                                             "Donnerstag", "Freitag"),
                                 weekend = c("Samstag", "Sonntag"))
levels(df_impute$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(df_impute$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```
      
2. Make a panel plot containing a time series plot (i.e. type = "l")
of the 5-minute interval
(x-axis) and the average number of steps taken,
averaged across all week- or weekend days (y-axis).

```r
avgSteps <- aggregate(df_impute$steps, 
                      list(interval = df_impute$interval, 
                           weekdays = df_impute$weekdays),
                      FUN = "mean")

names(avgSteps)[3] <- "meanOfSteps"

library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Mean of steps")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png) 
