# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
setwd("C:/Users/Divya/Desktop/RepData_PeerAssessment1")
library(ggplot2)
library(scales)
library(Hmisc)
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
Data1 <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?

#### 1.Total number of steps taken per day

```r
steps_per_day <- tapply(Data1$steps, Data1$date, sum, na.rm=TRUE)
```

#### 2.Histogram for total number of steps taken per day

```r
qplot(steps_per_day, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

#### 3.Mean and median of the total number of steps taken per day

```r
steps_per_day_mean <- mean(steps_per_day)
steps_per_day_median <- median(steps_per_day)
```
* Mean: 9354.2295082
* Median:  10395

## What is the average daily activity pattern?


```r
average_steps_per_time <- aggregate(x=list(meanSteps=Data1$steps), by=list(interval=Data1$interval), FUN=mean, na.rm=TRUE)
head(average_steps_per_time)
```

```
##   interval meanSteps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

#### 1.Time Series Plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
ggplot(data=average_steps_per_time, aes(x=interval, y=meanSteps)) + geom_line() 
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

#### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mostSteps <- which.max(average_steps_per_time$meanSteps)
max_steps_time <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", average_steps_per_time[mostSteps,'interval'])
```

*Most Steps at: 8:35

## Imputing missing values

#### 1.Calculate and report the total number of missing values in the dataset

```r
num_miss_values <- length(which(is.na(Data1$steps)))
Data1 <- read.csv('activity.csv')
```

Number of missing values: 2304

#### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.

#### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_data_imput <- Data1
activity_data_imput$steps <- impute(Data1$steps, fun=mean)
```

#### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
steps_day_imput<- tapply(activity_data_imput$steps, activity_data_imput$date, sum)
qplot(steps_per_day_mean, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
steps_data_mena_imput <- mean(steps_day_imput)
steps_data_mena_imput_median <- median(steps_day_imput)
```
* Mean (Imputed): 1.0766189 &times; 10<sup>4</sup>
* Median (Imputed):  1.0766189 &times; 10<sup>4</sup>


## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_data_imput$dateType <-  ifelse(as.POSIXlt(activity_data_imput$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

##### 2. Make a panel plot containing a time series plot


```r
averaged_activity_data <- aggregate(steps ~ interval + dateType, data=activity_data_imput, mean)
ggplot(averaged_activity_data, aes(interval, steps)) +
    geom_line() + 
    facet_grid(dateType ~ .)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
