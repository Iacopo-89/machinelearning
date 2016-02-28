---
title: "Rep course"
author: "iac"
date: "06 febbraio 2016"
output: html_document
---



```r
# What is mean total number of steps taken per day?
## Load the data 
data <- read.csv("C://Users//User//Documents//activity.csv", header=TRUE, sep=",")

## Omitting NA and create some aggregations to get the hist's data.
newdata <- na.omit(data)
avg <- data.frame(aggregate(newdata$steps, by=list(newdata$date), FUN = mean)) 
sum <- data.frame(aggregate(newdata$steps, by=list(newdata$date), FUN = sum))
median <- data.frame(aggregate(newdata$steps, by=list(newdata$date), FUN = median))
#Histogram, mean and median
hist(sum$x, main="Total number of steps taken each day", xlab= "day", col=c("blue"), breaks=20)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
mean(sum$x)
```

```
## [1] 10766.19
```

```r
median(sum$x)
```

```
## [1] 10765
```

```r
# What is the average daily activity pattern?

## Make a time series plot (i.e.  type = "l" ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avgstepsinterval <- data.frame(aggregate(newdata$steps, by=list(newdata$interval), FUN = mean))
plot(avgstepsinterval$Group.1, avgstepsinterval$x, type="l")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png) 

```r
## Get the max
avgstepsinterval$Group.1[which.max(avgstepsinterval$x)]
```

```
## [1] 835
```

```r
# Imputing missing values

## How many NAs?
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
## Loop to fill NAs
new=data
for (row in 1:length(new$steps)){
  if(is.na(new[row,1])){
    new[row,1]=avgstepsinterval[avgstepsinterval$Group.1==new[row,3],2]
  }
}

## New hist with handled NAs
newagg=aggregate(new$steps,by=list(new$date),FUN=sum)
hist(newagg$x,breaks=20,labels=unique(newagg$x[order(newagg$x)]),main="Histogram of steps by day",xlab="Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png) 

```r
## See new results
mean(newagg$x)
```

```
## [1] 10766.19
```

```r
median(newagg$x)
```

```
## [1] 10766.19
```

```r
# Are there differences in activity patterns between weekdays and weekends?
## Create a factor variable for weekdays vs weekend
new$date <- as.Date(new$date)
new$dayOfWeek=weekdays(new$date)
new$daytype=ifelse(new$dayOfWeek=="sabato"|new$dayOfWeek=="domenica","Weekend","Weekday")
new$daytype=as.factor(new$daytype)
new$interval=as.factor(new$interval)

## Weekend vs Weekdays plot
library(ggplot2)
lastagg=aggregate(new$steps,list(as.factor(new$interval),as.factor(new$daytype)),mean)
plot <- ggplot(lastagg, aes(x = as.integer(Group.1), y=x)) + geom_line()
plot + facet_grid(Group.2~.)+xlab("Interval")+ylab("Average Steps in Interval")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png) 

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
