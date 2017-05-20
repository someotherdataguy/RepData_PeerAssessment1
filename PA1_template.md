# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data<-read.csv("activity.csv", stringsAsFactors=FALSE)
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
data<-data %>% mutate(date=ymd(date))
```

## What is mean total number of steps taken per day?

```r
byDay<-data %>% group_by(date) %>% summarise(TotalSteps=sum(steps, na.rm = TRUE))
ggplot(data = byDay, aes(TotalSteps)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(byDay$TotalSteps)
```

```
## [1] 9354.23
```

```r
median(byDay$TotalSteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
byInterval<-data %>% group_by(interval) %>% summarise(AverageSteps=median(steps, na.rm=TRUE), meanSteps=mean(steps, na.rm=TRUE))
ggplot(data = byInterval, aes(x=interval, y=AverageSteps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
byInterval[which.max(byInterval$AverageSteps),1]
```

```
## # A tibble: 1 × 1
##   interval
##      <int>
## 1      845
```


## Imputing missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
inputed<-inner_join(x=data, y=byInterval, by=c("interval","interval"))
inputed$extra <- ifelse(!is.na(inputed$steps), inputed$steps, inputed$AverageSteps)
ibyDay<-inputed %>% group_by(date) %>% summarise(TotalSteps=sum(extra, na.rm = TRUE))
ggplot(data = ibyDay, aes(TotalSteps)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(ibyDay$TotalSteps)
```

```
## [1] 9503.869
```

```r
median(ibyDay$TotalSteps)
```

```
## [1] 10395
```


## Are there differences in activity patterns between weekdays and weekends?

```r
library(chron)
```

```
## 
## Attaching package: 'chron'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     days, hours, minutes, seconds, years
```

```r
inputed$isweekend<-is.weekend(inputed$date)

byIntervalw<-inputed %>% group_by(interval, isweekend) %>% summarise(AverageSteps=median(steps, na.rm=TRUE), meanSteps=mean(steps, na.rm=TRUE))
library(lattice)
xyplot(data = byIntervalw, meanSteps ~ interval | isweekend, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
