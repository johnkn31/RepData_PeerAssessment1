---
title: "2020_0913_Repro_Res_Proj_1"
author: "John Nguyen"
date: "9/13/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=TRUE}
data<-read.csv("activity.csv")
head(data)
```

## Loading and preprocessing the data
This will include code cleaning
```{r, echo=TRUE}
library(dplyr)
data<-read.csv("activity.csv")
data$date <- as.Date(data$date, format = "%m/%d/%Y")

```

## What is mean total number of steps taken per day?
```{r,echo=TRUE}
##1) Calculate the total number of steps taken per day
datasumdate<-data%>%
    group_by(date)%>%
    summarise(sumdate<-sum(steps))

datasumdate<-data.frame(datasumdate)
head(datasumdate)

##2) If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
hist(datasumdate$sumdate....sum.steps., main='Histogram', xlab='Total Steps')

##3) Calculate and report the mean and median of the total number of steps taken per day

mean(datasumdate$sumdate....sum.steps., na.rm = TRUE) ##Answer

median(datasumdate$sumdate....sum.steps., na.rm=TRUE) ##Answer

```


## What is the average daily activity pattern?
```{r,echo=TRUE}
##Time series plot of the average number of steps taken
avg_step<-tapply(data$steps,data$interval,FUN = mean,na.rm=TRUE)
avg_step<-data.frame(avg_step)
df2 <- cbind(interval = rownames(avg_step), avg_step)
rownames(df2) <- 1:nrow(df2)
plot(df2$interval,df2$avg_step, type='l',xlab='5 minute Time Interval',ylab='Average Step', col='red',
     main='Average Step vs 5 minute interval')

## Contains the maximum number of steps
highest_interval<-subset(df2, avg_step==max(avg_step))
highest_interval

```


## Imputing missing values
```{r,echo=TRUE}
##1) Calculate and report the total number of missing values in the dataset.
sum(is.na(data))
##2) Devise a strategy for filling in all of the missing values in the dataset. I chose the median for 5 minute interval.
med_step<-aggregate(steps~interval, data=data, median, na.rm=TRUE)
b<-numeric()
for (i in 1:nrow(data))
{
    if(is.na(data[i,]$steps)){##if steps in data is na
        a<-subset(med_step,interval==data[i,]$interval)$steps
    }
    else{
        a<-data[i,]$steps
    }
    b<-c(b,a) ##concat and keep every value of a stored in b
}
b<-data.frame(b)

##3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

new_data<-c(data,b)

new_data<-data.frame(new_data)
new_data2<-new_data[,2:4]
names(new_data2)[names(new_data2)=='b']<-'steps'
str(new_data2)

##4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

datasum2<-new_data2%>%
    group_by(date)%>%
    summarise(sumdate<-sum(steps))
head(datasum2)
datasum2<-data.frame(datasum2)

hist(datasum2$sumdate....sum.steps., main='Histogram', xlab='Total Steps')

mean(datasum2$sumdate....sum.steps.) ##mean
median(datasum2$sumdate....sum.steps.)##median

##Comment: We find that both mean and median drops when we replace missing values in steps with the median of each day.

```


## Are there differences in activity patterns between weekdays and weekends?
```{r,echo=TRUE}
##1)Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
data$days<-weekdays(data$date)

dataweekend<-subset(data,days=='Saturday'| days=='Sunday')
dataweekend$days<-as.factor(dataweekend$days)
avg_weekend<-aggregate(steps~interval, data=dataweekend, mean)

dataweek<-subset(data, days!='Saturday' & days!='Sunday')
dataweek$days<-as.factor(dataweek$days)
avg_week<-aggregate(steps~interval, data=dataweek, mean)


par(mfrow=c(2,1))
plot(avg_weekend$interval,avg_weekend$steps, type='l',xlab='5 minute Time Interval',ylab='Weekend Average Step', main='Weekend')
plot(avg_week$interval,avg_week$steps, type='l',xlab='5 minute Time Interval',ylab='Weekday Average Step', main='Weekday')

```

