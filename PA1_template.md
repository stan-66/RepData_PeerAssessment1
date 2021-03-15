---
title: "Reproducible research - Course project 1"
author: "stan"
date: "15/03/2021"
output: 
  html_document: 
    keep_md: yes
---




# Course project 1
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
The variables included in this dataset are:
-steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
-date: The date on which the measurement was taken in YYYY-MM-DD format
-interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Preliminary steps: Unzip and read the data
It is assumed that the zip.file was already downloaded into the work directory.

```r
#unzip
unzip("repdata_data_activity.zip")
#read and filter NA values
activity <- read.csv("activity.csv", header=T, sep=',')
```

## 1. Mean total number of steps taken per day
1.1 Plot the histogram of the number of steps taken per day

```r
library(ggplot2)
stepsperday <- aggregate(steps ~ date, data=activity, FUN=sum, na.rm=T) #aggregate
ggplot(data=stepsperday, aes(steps)) + geom_histogram(binwidth = 1000) + xlab('Total steps per day') + ylab('Count of days') + ggtitle('Total number of steps taken per day') #plot
```

![](PA1_template_files/figure-html/compute and plot the steps taken per day-1.png)<!-- -->

```r
dev.copy(png, file="plot1.png") #create file
```

```
## png 
##   3
```

```r
stepsmean <- mean(stepsperday$steps, na.rm = T) #compute mean
stepsmedian <- median(stepsperday$steps, na.rm = T) #compute median
```
1.2 Compute the mean and median number of steps taken per day:
The mean number of steps taken per day is: 1.0766189\times 10^{4}.
The median number of steps taken per day is: 10765.

## 2. Average daily activity pattern
2.1 Prepare the data set for the time series

```r
stepsperintv <- aggregate(steps ~ interval, data=activity, FUN=mean, na.rm=T) #aggregate
plot(stepsperintv, type = "l", xlab = "Time of day, 5-min interval", 
    ylab = "Average number of steps taken", main = "Average daily activity pattern")
```

![](PA1_template_files/figure-html/compute and plot the time series-1.png)<!-- -->

```r
dev.copy(png, file="plot2.png") #create file
```

```
## png 
##   4
```
2.2 Compute the 5-min intervAL with the maximum number of steps

```r
max_steps <- stepsperintv[stepsperintv$steps==max(stepsperintv$steps),]
```
The 5-minute interval with the maximum number of steps is: 835.

## 3. Imputting missing values
3.1 Compute and report the total number of missing values in the dataset.

```r
missingvalues <- sum(is.na(activity))
```
There are 2304 missing values in the source dataset.

3.2 Fill the missing values in the dataset by using the mean for that interval.

```r
completed <- activity #create a new data set by copying the source
#loop through the records and complete where missing with the interval data
for (i in 1:nrow(completed)) {
    if (is.na(completed[i,1])){ completed[i,1] <- stepsperintv[stepsperintv$interval==completed[i,3], 2]}
}
```
3.3 Plot a histogramm based on the completed data

```r
cstepsperday <- aggregate(steps ~ date, data=completed, FUN=sum, na.rm=T) #aggregate
ggplot(data=cstepsperday, aes(steps)) + geom_histogram(binwidth = 1000) + xlab('Total steps per day') + ylab('Count of days') + ggtitle('Total number of steps taken per day (completed data)') #plot
```

![](PA1_template_files/figure-html/compute and plot the steps taken per day based on the completed data-1.png)<!-- -->

```r
dev.copy(png, file="plot3.png") #create file
```

```
## png 
##   5
```

```r
cstepsmean <- mean(cstepsperday$steps, na.rm = T) #compute mean
cstepsmedian <- median(cstepsperday$steps, na.rm = T) #compute median
```
3.4 Compute the mean and median number of steps taken per day, based on the completed data:

The mean number of steps taken per day is: 1.0766189\times 10^{4}.
The median number of steps taken per day is: 1.0766189\times 10^{4}.

Compared to the data set before completing the data, the mean number of steps has remained the same, and there is a small increase in the median.
In the completetd dataset, the mean and the median are equal.

## 4. Differences of activity patterns between weekdays and weekends.
4.1 Compute the weekday and assign a code indicating whether weekday or weekend

```r
activity$weekday <- weekdays(as.Date(activity$date)) #compute the weekday
activity$wpart <- "weekday" #first assign value weekday to wpart code on all
# then loop though dataset and change to weekend where saturday or sunday
# - here in french
for (i in 1:nrow(activity)) {
  if (activity[i,4] %in% c("samedi", "dimanche")) {
    activity$wpart[i] <- "weekend"
  }
}
# and make it a factor
activity$wpart <- as.factor(activity$wpart)
```
4.2 then aggregate and plot

```r
avgsteps <- aggregate(steps ~ interval+wpart, activity, mean) #aggregate data
#then plot
ggplot(data=avgsteps, aes(interval, steps)) + geom_line() +
facet_wrap(~ wpart, ncol=1) + xlab("Timeline - 5-min interval") + ylab("Number of steps") + ggtitle('Difference in activity pattern (weekday / weekend)')
```

![](PA1_template_files/figure-html/aggregate and plot-1.png)<!-- -->

```r
dev.copy(png, file="plot4.png") #create file
```

```
## png 
##   6
```
