# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
dt <- read.table("activity.csv", dec = ".", fill = TRUE, quote = "\"", row.names = NULL, 
    header = TRUE, sep = ",")

dt$date <- as.Date(dt$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
stepsByDay <- aggregate(dt$steps, by = list(day = dt$date), FUN = sum, na.rm = TRUE)
meanStepsByDay <- aggregate(dt$steps, by = list(day = dt$date), FUN = mean, 
    na.rm = TRUE)
plot(stepsByDay$day, stepsByDay$x, type = "h", xlab = "", ylab = "number of steps", 
    main = "The number of steps taken each day")
```

![plot of chunk meanStepsByDay1](figure/meanStepsByDay1.png) 

```r
dev.copy(png, file = "figures/histogramStepsEachDay.png", width = 640, height = 480, 
    bg = "white")
```

```
## quartz_off_screen 
##                 3
```

The mean is:

```r
mean(stepsByDay$x)
```

```
## [1] 9354
```


The median is:

```r
median(stepsByDay$x)
```

```
## [1] 10395
```



## What is the average daily activity pattern?

```r
meanStepsByInterval <- aggregate(dt$steps, by = list(interval = dt$interval), 
    FUN = mean, na.rm = TRUE)
plot(meanStepsByInterval$interval, meanStepsByInterval$x, type = "l", xlab = "interval", 
    ylab = "average number of steps", main = "The average number of steps by interval")
```

![plot of chunk dailyActivityPattern](figure/dailyActivityPattern.png) 

```r
dev.copy(png, file = "figures/averageNumberOfStepsByInterval.png", width = 640, 
    height = 480, bg = "white")
```

```
## quartz_off_screen 
##                 4
```

The 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps is:

```r
subset(meanStepsByInterval$interval, meanStepsByInterval$x == max(meanStepsByInterval$x))
```

```
## [1] 835
```


## Imputing missing values
The total number of missing values in the dataset: 

```r
sum(is.na(dt$steps))
```

```
## [1] 2304
```


### The strategy chosen to fill the missing values is to use the mean for that 5-minute interval, converted to integer. 

```r
x = dt
for (i in 1:nrow(x)) {
    if (is.na(x[i, 1])) {
        interval = x[i, 3]
        x[i, 1] <- as.integer(subset(meanStepsByInterval, meanStepsByInterval$interval == 
            interval)[1, 2])
    }
}
sum(is.na(x$steps))
```

```
## [1] 0
```

```r

stepsXByDay <- aggregate(x$steps, by = list(day = x$date), FUN = sum, na.rm = TRUE)
meanXStepsByDay <- aggregate(x$steps, by = list(day = x$date), FUN = mean, na.rm = TRUE)
plot(stepsXByDay$day, stepsXByDay$x, type = "h", xlab = "", ylab = "number of steps", 
    main = "The number of steps taken each day after filling the missing values")
```

![plot of chunk fillMissingData](figure/fillMissingData.png) 

```r
dev.copy(png, file = "figures/histogramXStepsEachDay.png", width = 640, height = 480, 
    bg = "white")
```

```
## quartz_off_screen 
##                 5
```


The mean after filling the missing values is:

```r
mean(stepsXByDay$x)
```

```
## [1] 9392
```


The median after filling the missing values is:

```r
median(stepsXByDay$x)
```

```
## [1] 10395
```


When comparing the case with the missing values with the one where we filled them with the integer mean for the 5-minute interval, we can see that the median is the same and the mean differs.
The impact of imputing missing data on the estimates of the total daily number of steps can be seen if we do the difference between the daily number of steps after filling the data and the original number of steps:

```r
stepsXByDay$x - stepsByDay$x
```

```
##  [1] 288   0   0   0   0   0   0 288   0   0   0   0   0   0   0   0   0
## [18]   0   0   0   0   0   0   0   0   0   0   0   0   0   0 288   0   0
## [35] 288   0   0   0   0 288 288   0   0   0 288   0   0   0   0   0   0
## [52]   0   0   0   0   0   0   0   0   0 288
```

As expected, we see non null values for the days where missing data was filled in.


## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
dt$date <- factor(as.numeric(weekdays(dt$date) %in% c("Sunday", "Saturday")), 
    labels = c("weekday", "weekend"))
meanStepsByInt <- aggregate(dt$steps, by = list(interval = dt$interval, day = dt$date), 
    FUN = mean, na.rm = TRUE)
xyplot(meanStepsByInt$x ~ meanStepsByInt$interval | meanStepsByInt$day, type = "l", 
    layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk weekdayDifferences](figure/weekdayDifferences.png) 

