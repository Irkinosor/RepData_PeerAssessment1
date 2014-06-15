# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r

unzip(zipfile = "activity.zip", overwrite = TRUE, unzip = "internal")
activity <- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", 
    "Date", "numeric"))
```


## What is mean total number of steps taken per day?
**1.make a histogram of the total number of steps taken each day**

```r
library(ggplot2)
library(plyr)

steps.sum <- ddply(activity, "date", summarise, total = sum(steps, na.rm = T))

fig <- ggplot(steps.sum, aes(x = date, y = total)) + geom_bar(stat = "identity")

datebreaks <- seq(as.Date(min(activity$date)), as.Date(max(activity$date)), 
    by = 5)

fig + scale_x_date(breaks = datebreaks) + theme(axis.text.x = element_text(angle = 30, 
    hjust = 1)) + labs(x = "Days", y = "Total Steps", title = "total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

**2.Calculate and report the mean and median total number of steps taken per day**

```r
mean.for.days <- mean(steps.sum$total, na.rm = TRUE)
median.for.days <- median(steps.sum$total, na.rm = TRUE)
```

mean is 9354.2295 and median is 1.0395 &times; 10<sup>4</sup>

## What is the average daily activity pattern?
**1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```r
steps.sum.day.interval <- ddply(activity, "interval", summarise, average = mean(steps, 
    na.rm = T))

figmean <- ggplot(steps.sum.day.interval, aes(x = interval, y = average)) + 
    geom_line()

figmean + labs(x = "average steps", y = "5 min interval", title = "Averge number of steps per interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

**2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
maxaverage <- which(steps.sum.day.interval$average == max(steps.sum.day.interval$average))
interval.max <- steps.sum.day.interval$interval[maxaverage]
```

the 5 min interval containing the maximum is: 835
## Imputing missing values
**1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

```r
totalmissing <- sum(colSums(is.na(activity)))
```

total numberof missing value: 2304

**2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

```r
impute <- function(x, foo) replace(x, is.na(x), foo(x, na.rm = TRUE))
```

**3.Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
imput.activity <- ddply(activity, "interval", transform, steps = impute(steps, 
    mean))
imput.activity <- imput.activity[order(imput.activity$date), ]
```


**4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

```r
steps.sum2 <- ddply(imput.activity, "date", summarise, total = sum(steps, na.rm = T))

fig2 <- ggplot(steps.sum2, aes(x = date, y = total)) + geom_bar(stat = "identity")

datebreaks <- seq(as.Date(min(imput.activity$date)), as.Date(max(imput.activity$date)), 
    by = 5)

fig2 + scale_x_date(breaks = datebreaks) + theme(axis.text.x = element_text(angle = 30, 
    hjust = 1)) + labs(x = "Days", y = "Total Steps", title = "total number of steps per day on imputed dataset")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

**mean and median on imputed data**

```r
mean.for.days2 <- mean(steps.sum2$total, na.rm = TRUE)
median.for.days2 <- median(steps.sum2$total, na.rm = TRUE)
```

imputed data's mean is 1.0766 &times; 10<sup>4</sup> and median is 1.0766 &times; 10<sup>4</sup>
*Do these values differ from the estimates from the first part of the assignment?*
yes
*What is the impact of imputing missing data on the estimates of the total daily number of steps?*
increase the estimate value
## Are there differences in activity patterns between weekdays and weekends?

**1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**

```r
wdays <- weekdays(imput.activity$date)
imput.activity$day.type <- as.factor(ifelse(wdays == "Saturday" | wdays == "Sunday", 
    "weekend", "weekday"))
```

**2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**

```r
steps.interval <- ddply(imput.activity, .(interval, day.type), summarise, average = mean(steps, 
    na.rm = T))

fig3 <- ggplot(steps.interval, aes(x = interval, y = average)) + geom_line()

fig3 + facet_wrap(~day.type, ncol = 1)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

