---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


## What is mean total number of steps taken per day?

```r
data <- read.csv("activity/activity.csv")

complete <- data[complete.cases(data),]

days <- split(complete, complete$date, drop=TRUE)

totalStepsPerDay <- sapply(days, function(day) { return (sum(day$steps)) } )
```


```r
hist(totalStepsPerDay,
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

meanStepsPerInterval <- sapply(intervals, function(i) { return (mean(i$steps)) } )
```



```r
plot(meanStepsPerInterval,
     type="l",
     main="Daily Activity Pattern",
     ylab="Steps",
     xlab="Interval")
```

![plot of chunk plot](figure/plot-1.png) 

```r
highestInterval = which.max(meanStepsPerInterval)
highestValue = meanStepsPerInterval[highestInterval]
```


The highest mean number of steps (of value 206.1698113) occurs at five-minute interval 104.

## Inputing missing values
The total number of rows with missing values is
2304.


```r
# Estimate the missing values
estimData = data
for (r in 1:nrow(estimData)) {
  if (is.na(estimData[r, 'steps'])) {
    interval = estimData[r, 'interval']
    index = which(names(meanStepsPerInterval) == as.character(interval))
    replacement = meanStepsPerInterval[index]
    estimData[r, 'steps'] = replacement
  }
}
```


```r
estimDays <- split(estimData, estimData$date, drop=TRUE)

estimTotalStepsPerDay <- sapply(estimDays, function(day) { return (sum(day$steps)) } )
```


```r
hist(estimTotalStepsPerDay,
     main="Total steps per day (with missing values estimated)",
     xlab="Steps")
```

![plot of chunk estimHist](figure/estimHist-1.png) 

With missing values filled in with estimates,
the mean total number of steps taken per day is
10766.
and the median total number of steps taken per day is
10766.


```r
# What is the impact of adding estimates for the missing data?
# Returns: the percentage difference (roughly speaking)
Impact <- function (before, after) {
  return (200 * (before - after) / (before + after))
}

impactMean = Impact(mean(totalStepsPerDay), mean(estimTotalStepsPerDay))

impactMedian = Impact(median(totalStepsPerDay), median(estimTotalStepsPerDay))
```

The mean   changed by about 0 percent.
The meidan changed by about -0.0110415 percent.


## Are there differences in activity patterns between weekdays and weekends?
