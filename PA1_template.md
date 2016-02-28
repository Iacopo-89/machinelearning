---
output: 
  html_document: 
    self_contained: no
    fig_caption: yes
---
# Reproducible Research Project


## What is mean total number of steps taken per day?

####Loading Data

```r
data <- read.csv("activity.csv", header=TRUE, sep=",")
```
#### Omitting NA and create some aggregations to get the hist's data.

```r
newdata <- na.omit(data)
avg <- data.frame(aggregate(newdata$steps, by=list(newdata$date), FUN = mean))
sum <- data.frame(aggregate(newdata$steps, by=list(newdata$date), FUN = sum))
median <- data.frame(aggregate(newdata$steps, by=list(newdata$date), FUN = median))
```

```r
hist(sum$x, main="Total number of steps taken each day", xlab= "day", col=c("blue"), breaks=20)
```

![plot of chunk unnamed-chunk-3](PA1_templates_files/figures-htmlunnamed-chunk-3-1.pdf)
#### Mean and Median

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
## What is the average daily activity pattern?

```r
avgstepsinterval <- data.frame(aggregate(newdata$steps, by=list(newdata$interval), FUN = mean))
```
#### Make a ts series plot (i.e.  type = "l" ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(avgstepsinterval$Group.1, avgstepsinterval$x, type="l")
```

![plot of chunk unnamed-chunk-6](PA1_templates_files/figures-htmlunnamed-chunk-6-1.pdf)

```r
avgstepsinterval$Group.1[which.max(avgstepsinterval$x)]
```

```
## [1] 835
```
## Imputing missing values, new histogram and results

```r
new=data
for (row in 1:length(new$steps)){
  if(is.na(new[row,1])){
    new[row,1]=avgstepsinterval[avgstepsinterval$Group.1==new[row,3],2]
  }
}

newagg=aggregate(new$steps,by=list(new$date),FUN=sum)
hist(newagg$x,breaks=20,labels=unique(newagg$x[order(newagg$x)]),main="Histogram of steps by day",xlab="Steps")
```

![plot of chunk unnamed-chunk-7](PA1_templates_files/figures-htmlunnamed-chunk-7-1.pdf)

```r
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
## Are there differences in activity patterns between weekdays and weekends?
#### Create a factor variable for weekdays vs weekend

```r
new$date <- as.Date(new$date)
new$dayOfWeek=weekdays(new$date)
new$daytype=ifelse(new$dayOfWeek=="sabato"|new$dayOfWeek=="domenica","Weekend","Weekday")
new$daytype=as.factor(new$daytype)
new$interval=as.factor(new$interval)
```
## Weekend vs Weekdays plot

```r
library(ggplot2)
lastagg=aggregate(new$steps,list(as.factor(new$interval),as.factor(new$daytype)),mean)
plot <- ggplot(lastagg, aes(x = as.integer(Group.1), y=x)) + geom_line()
plot + facet_grid(Group.2~.)+xlab("Interval")+ylab("Average Steps in Interval")
```

![plot of chunk unnamed-chunk-9](PA1_templates_files/figures-htmlunnamed-chunk-9-1.pdf)
