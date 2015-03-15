# Reproducible Research: Peer Assessment 1
Before we dive, we quickly load any supporting libraries.

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data
The data is included with this HTML report and the markdown files used to 
generate the HTML report.

The data starts its life as a zip file, "activity.zip", in the local working
directory. We need to unzip the file, import the contents, and then clean up 
the unzipped messy-ness.

**Note:** This code assumes that your working directory has been set correctly.


```r
# unzip the file
unzip("./activity.zip")
# get the data
act <- read.csv("./activity.csv")
# clean up the csv
unlink("./activity.csv")
```

We then take a quick pause to add another date variable (in case we want to
treat our dates as dates and not as factors.)

```r
act$date_as_date <- strptime(as.character(act$date), "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
Our data has many step measurements for each day.


```r
# first we get the total steps for each unique day
steps_per_day <- aggregate(steps ~ date, act, sum, na.rm = TRUE)

# then we take the mean of that aggregation
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

We may also want to consider the median value...

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

Or just visualize the distribution of steps per day.

```r
ggplot(act, aes(x = date_as_date, y = steps)) + 
    geom_histogram(stat = "identity")
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/hist_steps-1.png) 

## What is the average daily activity pattern?
First we find the average steps per interval (across days).

```r
mean_per_interval <- aggregate(steps ~ interval, act, mean, na.rm = T)
```

Then we make a histogram showing off the interval data.

```r
ggplot(mean_per_interval, aes(x = interval, y = steps)) + 
    geom_histogram(bindwidth = 1, stat = "identity")
```

![](PA1_template_files/figure-html/hist_interval-1.png) 

Final we note which of the intervals had the largest average number of steps.

```r
# sort largest to smallest, return just the first (largest) result
arrange(mean_per_interval, desc(steps))[1,]
```

```
##   interval    steps
## 1      835 206.1698
```

## Imputing missing values
Determine how many NAs are in the data set.

```r
summary(act)
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
##   date_as_date                
##  Min.   :2012-10-01 00:00:00  
##  1st Qu.:2012-10-16 00:00:00  
##  Median :2012-10-31 00:00:00  
##  Mean   :2012-10-31 00:25:34  
##  3rd Qu.:2012-11-15 00:00:00  
##  Max.   :2012-11-30 00:00:00  
## 
```

Replace any NAs with the median for the given day.


Show the updated mean and median per day, along with the updated histogram.

## Are there differences in activity patterns between weekdays and weekends?
Add a new factor specifying weekdays and weekends.

Contrast weekdays and weekends via plot.
