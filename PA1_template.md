# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activityData <- read.csv("activity.csv", header = TRUE)
date <- as.Date(activityData$date, format = "%Y-%m-%d", na.rm=TRUE)
interval <- as.integer(activityData$interval, na.rm=TRUE)
steps <- as.integer(activityData$steps, na.rm=TRUE)
```

## What is mean total number of steps taken per day?


```r
library(ggplot2)
stepsPerDay <- aggregate(steps~date, activityData, sum)
ggplot(stepsPerDay, aes(steps)) + geom_histogram(binwidth=1000) + labs(title= "Daily Steps", x = "Steps Taken Per Day", y= "Times per Day(Count)")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meanStepsPerDay <- mean(stepsPerDay$steps, na.rm=TRUE)
medianStepsPerDay <- median(stepsPerDay$steps, na.rm=TRUE)
meanStepsPerDay
```

```
## [1] 10766.19
```

```r
medianStepsPerDay
```

```
## [1] 10765
```

The mean number of steps per day is 10766.19. 
The median number of steps per day is 10765.

## What is the average daily activity pattern?


```r
avgSteps <- aggregate(steps~interval, activityData, mean, na.rm=TRUE)
ggplot(avgSteps, aes(interval, steps)) + geom_line() + labs(title="Average Daily Activity Pattern", x = "Interval (5 Minutes)", y = "Steps Taken Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
avgSteps[which.max(avgSteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values


```r
total_na <- sum(is.na(steps))
total_na
```

```
## [1] 2304
```

The total number of missing values is 2304


```r
mean_substitue <- function(steps, interval) {
    substitution <- NA
    if (!is.na(steps))
        substitution <- c(steps)
    else
        substitution <- (avgSteps[avgSteps$interval==interval, "steps"])
    return(substitution)
}
activityCloned <- activityData
activityCloned$steps <- mapply(mean_substitue, steps, interval)

library(ggplot2)
stepsPerDay <- aggregate(steps~date, activityCloned, sum)
ggplot(stepsPerDay, aes(steps)) + geom_histogram(binwidth=1000) + labs(title= "Daily Steps", x = "Steps Taken Per Day", y= "Times per Day(Count)")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
meanStepsPerDay <- mean(stepsPerDay$steps, na.rm=TRUE)
medianStepsPerDay <- median(stepsPerDay$steps, na.rm=TRUE)
meanStepsPerDay
```

```
## [1] 10766.19
```

```r
medianStepsPerDay
```

```
## [1] 10766.19
```

The new mean steps per day is 10766.19.  The new median steps per day is 10766.19.  
Values of total number of steps, mean and median total number of steps taken per day are different from the first part of the assignment. 
Imputing missing values removes a lot of zeros of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?


```r
weekend_weekday <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
activityCloned$date <- as.Date(activityCloned$date)
activityCloned$day <- sapply(activityCloned$date, weekend_weekday)
average_steps <- aggregate(steps~interval+ day, activityCloned, mean, na.rm=TRUE)
ggplot(average_steps, aes(interval, steps)) + geom_line() + facet_grid(day~.) + labs(title="Average Daily Activity Pattern", x = "Interval (5 Minutes)", y = "Steps Taken Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

It seems that in weekdays there is a major peak between 500 and 1000 minutes, 
but in weekend the activity is more evenly spread
