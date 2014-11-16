---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data



```r
data <- read.csv("activity/activity.csv")

complete <- data[complete.cases(data),]

days <- split(complete, complete$date, drop=TRUE)

stepsPerDay <- sapply(days, function(day) { return (sum(day$steps)) } )

#median(stepsPerDay)
#mean(stepsPerDay)
```


```r
hist(stepsPerDay,
     main="Total steps per day",
     xlab="Steps")
```

![plot of chunk hist](figure/hist-1.png) 

## What is mean total number of steps taken per day?
The mean total number of steps taken per day is
10766.
The median total number of steps taken per day is
10765.


## What is the average daily activity pattern?


```r
intervals <- split(complete, complete$interval)

stepsPerInterval <- sapply(intervals, function(i) { return (mean(i$steps)) } )
```



```r
plot(stepsPerInterval,
     type="l",
     main="Daily Activity Pattern",
     ylab="Steps",
     xlab="Interval")
```

![plot of chunk plot](figure/plot-1.png) 

```r
highestInterval = which.max(stepsPerInterval)
highestValue = stepsPerInterval[highestInterval]
```


The highest mean number of steps (of value 206.1698113) occurs at five-minute interval 104.

## Inputing missing values



## Are there differences in activity patterns between weekdays and weekends?
