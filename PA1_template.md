Reproducible Research Peer Assignment 1
==========================================================

# Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

In this report we make use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. Data was provided for the course students for this particular assignment, avaliable to download via this link (avaliable at 2014-05-25).

In the report, we try to answer the following questions:

What is mean total number of steps taken per day?
What is the average daily activity pattern?
Are there differences in activity patterns between weekdays and weekends?

# Loading and preprocessing the data
As a first step we will load the data from "activity.csv"

```r
activity <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```
Now we will look at summary of the activity using "summary" and "str" functions


```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

# What is the mean number of steps taken per day?
Histogram of the total steps per day is given below


```r
steps_per_day <- aggregate(steps ~ date, activity, sum)
hist(steps_per_day$steps, main = paste("Total Steps Per Day"), col="grey",xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Mean of the steps:

```r
mean_steps <- mean(steps_per_day$steps)
mean_steps
```

```
## [1] 10766.19
```

Median of the steps:

```r
median_steps <- median(steps_per_day$steps)
median_steps
```

```
## [1] 10765
```

# What is the average daily activity pattern?
A time series plot of the 5-minute interval and the average number of steps taken (averaged across all days) is shown below

```r
steps_int <- tapply(activity$steps, activity$interval, mean, na.rm=T)
plot(steps_int ~ unique(activity$interval), type="l", xlab = "5-min interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

The 5-min interval that contains the maximum number of steps and the maximum number of steps are respectively given below:

```r
steps_int[which.max(steps_int)]
```

```
##      835 
## 206.1698
```

# Imputing missing values
Total number of missing values in the dataset

```r
Total_NAs <- sum(!complete.cases(activity))
Total_NAs
```

```
## [1] 2304
```

The strategy, which is being is used here that the missing values are replaced by the mean of the day

```r
# creation of the dataset that will have no more NAs
activity_new <- activity  
for (i in 1:nrow(activity)){
    if(is.na(activity$steps[i])){
        activity_new$steps[i]<-steps_int[[as.character(activity[i, "interval"])]]
    }
}
#Creating the histogram
latest <- tapply(activity_new$steps, activity_new$date, sum, na.rm=T)
hist(latest,col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
The new mean is 

```r
mean_latest <- mean (latest)
mean_latest
```

```
## [1] 10766.19
```

The new median is 

```r
median_latest <- median (latest)
median_latest
```

```
## [1] 10766.19
```

So, we can see that mean (10766.19) is same for both with missing and without missing values while median has increased from 10765 to 10766.19 when we removed the missing values

# Are there differences in activity patterns between weekdays and weekends?
The plot below shows the difference in the activity pattern for weekdays and weekends:


```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
activity_new$dow = as.factor(ifelse(is.element(weekdays(as.Date(activity_new$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, activity_new, mean)
library(lattice)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
