# Reproducible Research: Peer Assessment 1
Gerardo Santiago Flores  
June 12, 2016  


```r
knitr::opts_chunk$set(echo = TRUE)
library(timeDate)
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.2.4
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

## Loading and preprocessing the data
### 1. Code for reading in the dataset and/or processing the data

```r
read_data <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
head(read_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?
### 2. Histogram of the total number of steps taken each day

```r
total_steps <-ddply(read_data, .(date), summarise, sum=sum(steps))
hist(total_steps$sum, xlab = "Steps", main ="Total number of steps per day ")
```

![](PA1_template_files/figure-html/hist_total_per_day-1.png)<!-- -->

## What is the average daily activity pattern?
### 3. Mean and median number of steps taken each day

```r
mean_median_data <- ddply(read_data, .(date), summarise, mean=mean(steps, na.rm = TRUE), median=median(steps, na.rm = TRUE))
mean_median_data
```

```
##          date       mean median
## 1  2012-10-01        NaN     NA
## 2  2012-10-02  0.4375000      0
## 3  2012-10-03 39.4166667      0
## 4  2012-10-04 42.0694444      0
## 5  2012-10-05 46.1597222      0
## 6  2012-10-06 53.5416667      0
## 7  2012-10-07 38.2465278      0
## 8  2012-10-08        NaN     NA
## 9  2012-10-09 44.4826389      0
## 10 2012-10-10 34.3750000      0
## 11 2012-10-11 35.7777778      0
## 12 2012-10-12 60.3541667      0
## 13 2012-10-13 43.1458333      0
## 14 2012-10-14 52.4236111      0
## 15 2012-10-15 35.2048611      0
## 16 2012-10-16 52.3750000      0
## 17 2012-10-17 46.7083333      0
## 18 2012-10-18 34.9166667      0
## 19 2012-10-19 41.0729167      0
## 20 2012-10-20 36.0937500      0
## 21 2012-10-21 30.6284722      0
## 22 2012-10-22 46.7361111      0
## 23 2012-10-23 30.9652778      0
## 24 2012-10-24 29.0104167      0
## 25 2012-10-25  8.6527778      0
## 26 2012-10-26 23.5347222      0
## 27 2012-10-27 35.1354167      0
## 28 2012-10-28 39.7847222      0
## 29 2012-10-29 17.4236111      0
## 30 2012-10-30 34.0937500      0
## 31 2012-10-31 53.5208333      0
## 32 2012-11-01        NaN     NA
## 33 2012-11-02 36.8055556      0
## 34 2012-11-03 36.7048611      0
## 35 2012-11-04        NaN     NA
## 36 2012-11-05 36.2465278      0
## 37 2012-11-06 28.9375000      0
## 38 2012-11-07 44.7326389      0
## 39 2012-11-08 11.1770833      0
## 40 2012-11-09        NaN     NA
## 41 2012-11-10        NaN     NA
## 42 2012-11-11 43.7777778      0
## 43 2012-11-12 37.3784722      0
## 44 2012-11-13 25.4722222      0
## 45 2012-11-14        NaN     NA
## 46 2012-11-15  0.1423611      0
## 47 2012-11-16 18.8923611      0
## 48 2012-11-17 49.7881944      0
## 49 2012-11-18 52.4652778      0
## 50 2012-11-19 30.6979167      0
## 51 2012-11-20 15.5277778      0
## 52 2012-11-21 44.3993056      0
## 53 2012-11-22 70.9270833      0
## 54 2012-11-23 73.5902778      0
## 55 2012-11-24 50.2708333      0
## 56 2012-11-25 41.0902778      0
## 57 2012-11-26 38.7569444      0
## 58 2012-11-27 47.3819444      0
## 59 2012-11-28 35.3576389      0
## 60 2012-11-29 24.4687500      0
## 61 2012-11-30        NaN     NA
```

### 4. Time series plot of the average number of steps taken


```r
average_steps_per_interval <- ddply(read_data, .(interval), summarise, mean=mean(steps, na.rm = TRUE))
plot(average_steps_per_interval, type = "l", main = "Mean number of steps per interval")
```

![](PA1_template_files/figure-html/plot_average_steps-1.png)<!-- -->

### 5. The 5-minute interval that, on average, contains the maximum number of steps


```r
interval_max_steps <- average_steps_per_interval$interval[which.max(average_steps_per_interval$mean)]
print("interval with max steps")
```

```
## [1] "interval with max steps"
```

```r
print(interval_max_steps)
```

```
## [1] 835
```

## Imputing missing values
### 6. Code to describe and show a strategy for imputing missing data
The strategy used is to replace NA by mean value of that 5-minute interval


```r
print("Total number of rows with NA")
```

```
## [1] "Total number of rows with NA"
```

```r
print(sum(is.na(read_data$steps)))
```

```
## [1] 2304
```


```r
print("Fill NA strategy")
```

```
## [1] "Fill NA strategy"
```

```r
intervals <- unique(read_data$interval)
filled_data <- read_data
for (inter in intervals)
{
    index_to_fill <- which(is.na(read_data$steps) & read_data$interval == inter)
    filled_data[index_to_fill,]$steps <- average_steps_per_interval[average_steps_per_interval$interval == inter,]$mean
}
print(head(filled_data))
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

### 7. Histogram of the total number of steps taken each day after missing values are imputed


```r
filled_total_steps <-ddply(filled_data, .(date), summarise, sum=sum(steps))
hist(filled_total_steps$sum, xlab = "Steps", main ="Total number of steps per day (Filled Data)")
```

![](PA1_template_files/figure-html/hist_total_per_day_filled-1.png)<!-- -->


```r
filled_mean_median_data <- ddply(filled_data, .(date), summarise, mean=mean(steps, na.rm = TRUE), median=median(steps, na.rm = TRUE))
print("Mean Meadian of filled data")
```

```
## [1] "Mean Meadian of filled data"
```

```r
print(filled_mean_median_data)
```

```
##          date       mean   median
## 1  2012-10-01 37.3825996 34.11321
## 2  2012-10-02  0.4375000  0.00000
## 3  2012-10-03 39.4166667  0.00000
## 4  2012-10-04 42.0694444  0.00000
## 5  2012-10-05 46.1597222  0.00000
## 6  2012-10-06 53.5416667  0.00000
## 7  2012-10-07 38.2465278  0.00000
## 8  2012-10-08 37.3825996 34.11321
## 9  2012-10-09 44.4826389  0.00000
## 10 2012-10-10 34.3750000  0.00000
## 11 2012-10-11 35.7777778  0.00000
## 12 2012-10-12 60.3541667  0.00000
## 13 2012-10-13 43.1458333  0.00000
## 14 2012-10-14 52.4236111  0.00000
## 15 2012-10-15 35.2048611  0.00000
## 16 2012-10-16 52.3750000  0.00000
## 17 2012-10-17 46.7083333  0.00000
## 18 2012-10-18 34.9166667  0.00000
## 19 2012-10-19 41.0729167  0.00000
## 20 2012-10-20 36.0937500  0.00000
## 21 2012-10-21 30.6284722  0.00000
## 22 2012-10-22 46.7361111  0.00000
## 23 2012-10-23 30.9652778  0.00000
## 24 2012-10-24 29.0104167  0.00000
## 25 2012-10-25  8.6527778  0.00000
## 26 2012-10-26 23.5347222  0.00000
## 27 2012-10-27 35.1354167  0.00000
## 28 2012-10-28 39.7847222  0.00000
## 29 2012-10-29 17.4236111  0.00000
## 30 2012-10-30 34.0937500  0.00000
## 31 2012-10-31 53.5208333  0.00000
## 32 2012-11-01 37.3825996 34.11321
## 33 2012-11-02 36.8055556  0.00000
## 34 2012-11-03 36.7048611  0.00000
## 35 2012-11-04 37.3825996 34.11321
## 36 2012-11-05 36.2465278  0.00000
## 37 2012-11-06 28.9375000  0.00000
## 38 2012-11-07 44.7326389  0.00000
## 39 2012-11-08 11.1770833  0.00000
## 40 2012-11-09 37.3825996 34.11321
## 41 2012-11-10 37.3825996 34.11321
## 42 2012-11-11 43.7777778  0.00000
## 43 2012-11-12 37.3784722  0.00000
## 44 2012-11-13 25.4722222  0.00000
## 45 2012-11-14 37.3825996 34.11321
## 46 2012-11-15  0.1423611  0.00000
## 47 2012-11-16 18.8923611  0.00000
## 48 2012-11-17 49.7881944  0.00000
## 49 2012-11-18 52.4652778  0.00000
## 50 2012-11-19 30.6979167  0.00000
## 51 2012-11-20 15.5277778  0.00000
## 52 2012-11-21 44.3993056  0.00000
## 53 2012-11-22 70.9270833  0.00000
## 54 2012-11-23 73.5902778  0.00000
## 55 2012-11-24 50.2708333  0.00000
## 56 2012-11-25 41.0902778  0.00000
## 57 2012-11-26 38.7569444  0.00000
## 58 2012-11-27 47.3819444  0.00000
## 59 2012-11-28 35.3576389  0.00000
## 60 2012-11-29 24.4687500  0.00000
## 61 2012-11-30 37.3825996 34.11321
```

## Are there differences in activity patterns between weekdays and weekends?


```r
filled_data$date <- as.Date(filled_data$date)
filled_data$weekday <- isWeekday(filled_data$date)
weekday_average_steps_per_interval <- ddply(filled_data, .(interval, weekday), summarise, mean=mean(steps, na.rm = TRUE))
ggplot(weekday_average_steps_per_interval, aes(x = interval, y = mean, colour = weekday)) + geom_line() + facet_grid(weekday ~ .) + ggtitle("Mean steps per interval") + labs(x="Interval",y="Number of steps") 
```

![](PA1_template_files/figure-html/weekdays_mean-1.png)<!-- -->
