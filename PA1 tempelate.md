---
title: "PA template 1"
output: html_document
date: "2023-02-07"
---

#REPRODUCIBLE RESEARCH ASSINGNMENT 1

##Loading and processing the data


```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
library(ggplot2)
library(dplyr)
activity$date <- as.Date(activity$date)
totalsteps <- tapply(activity$steps , activity$date ,FUN = sum , na.rm = T )

hist(totalsteps , main = "Histogram of Total steps " , xlab = "Steps", col= "red" , ylim = c(0,30) )
```

![plot of chunk unnamed-chunk-34](figure/unnamed-chunk-34-1.png)

```r
mean <- mean(totalsteps , na.rm = T)

median <-  median(totalsteps , na.rm = T)
```

## What is the average daily activity pattern?


```r
library(ggplot2)
library(dplyr)

averages <- aggregate(x=list(steps = activity$steps) , by = list(interval = activity$interval), FUN = mean , na.rm = T )

plot(averages$steps ~ averages$interval, type = "l" , col= "red" , xlab= "5 min interval" , ylab = "Average number of Steps" , main = "Steps by Time Interval")
```

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35-1.png)

```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

Straregy to solve missing NAs values = The average of associated interval will be used .


```r
 print(paste("total no of rows with  NA : ", sum(is.na(activity$steps))))
```

```
## [1] "total no of rows with  NA :  2304"
```

these missing values are filled in with mean value for that 5-minute interval.


```r
library(ggplot2)
library(dplyr)
 activitynonas <- activity
 for(i in 1:nrow(activity)){if (is.na(activity$steps[i])){ activitynonas$steps[i] <- averages$steps[activitynonas$interval[i] == averages$interval]} }
 totalfillsteps <- tapply(activitynonas$steps , activitynonas$date , FUN = sum)
 
 hist(totalfillsteps , xlab = "Steps", main = "Histogram of total Steps" , col = "green" )
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36-1.png)

```r
print(paste("The mean is : " , mean(totalfillsteps)))
```

```
## [1] "The mean is :  10766.1886792453"
```

```r
print(paste("The median is : " , median(totalfillsteps)))
```

```
## [1] "The median is :  10766.1886792453"
```

Mean and median values are higher after imputing missing data. The reason is that in the original data, there are some days with `steps` values `NA` for any `interval`. The total number of steps taken in such days are set to 0s by default. However, after replacing missing `steps` values with the mean `steps` of associated `interval` value, these 0 values are removed from the histogram of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?


```r
library(ggplot2)
library(dplyr)

weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}


activitynonas$date <- as.Date(activitynonas$date)
activitynonas$day <- sapply(activitynonas$date , FUN = weekday.or.weekend) 
```

Now makes a plot for containing plots of average number of steps taken on weekdays and weekends.


