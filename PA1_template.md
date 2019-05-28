---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
# Nathan Tran

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


    
    ```r
    library(plyr)
    library(ggplot2)
    library(lattice)
    
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    zip <- "repdata_data_activity.zip"
    file <- "activity.csv"
    
    if(!file.exists(zip)){
        download.file(url, zip, method = "curl")
    }
    
    if(!file.exists(file)){
        unzip(zip)
    }
    
    activityDT <- read.csv(file)
    activityDT$date <- as.Date(activityDT$date, "%Y-%m-%d")
    ```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day
    
    
    ```r
    totalSteps <- ddply(activityDT, ~date, summarise, steps=sum(steps))
    
    ggplot(data = totalSteps, aes(x = steps)) + 
        geom_histogram(binwidth = 700, ) +
        labs(x = "Total Steps", title = "Total Steps Taken Each Day")
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
    
    ```r
    mean.steps <- format(mean(totalSteps$steps, na.rm = TRUE), digits = 5)
    median.steps <- format(median(totalSteps$steps, na.rm = TRUE), digits = 5)
    ```

- The mean total number of steps taken per day is: **10766**
- The median total number of steps taken per day is: **10765**
    
## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    
    ```r
    intervalSteps <- ddply(activityDT, ~ interval, summarise, steps = mean(steps, na.rm = TRUE))
    
    plot(data = intervalSteps, 
         x = intervalSteps$interval, 
         y = intervalSteps$steps, 
         type = "l",
         xlab = "5-minute Interval",
         ylab = "Average Steps",
         main = "Average Steps Taken During 5-minute Intervals Each Day") 
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
    
    ```r
    max <- intervalSteps[which.max(intervalSteps$steps), ]
    interval <- max[,1]
    step <- round(max[,2])
    ```

- The 5-minute interval that contains the maximum number of steps is: **835** with **206** steps

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


    
    ```r
    activityDT2 <- read.csv(file)
    activityDT2$date <- as.Date(activityDT2$date, "%Y-%m-%d")
    
    na <- sum(is.na(activityDT2) == TRUE)
    
    for(i in 1:nrow(activityDT2)){
        if(is.na(activityDT2$steps[i]) == TRUE ){
            matchINT <- activityDT2$interval[i]
            mean5 <- intervalSteps[intervalSteps$interval == matchINT, ]
            activityDT2$steps[i] = mean5$steps
        }
    }
    
    totalSteps2 <- ddply(activityDT2, ~ date, summarise, total.steps = sum(steps))
    
    ggplot(data = totalSteps2, aes(x = total.steps)) +
        geom_histogram(binwidth = 700) + 
        labs(x = "Total Steps", title = "Total Steps Taken Each Day")
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
    
    ```r
    mean.steps2 <- format(mean(totalSteps2$total.steps), digits = 5)
    median.steps2 <- format(median(totalSteps2$total.steps), digits = 5)
    ```

- The total number of missing values in the dataset is: **2304**

- The mean total number of steps taken per day is: **10766**
- The median total number of steps taken per day is: **10766**

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    
    ```r
    activityDT2$week <- ifelse(weekdays(activityDT2$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
    
    intervalSteps2 <- ddply(activityDT2, week ~ interval, summarise, steps = mean(steps))
    
    weeekPlot <- xyplot(steps ~ interval | week, 
                data = intervalSteps2,
                type = "l",
                layout = c(1,2),
                xlab = "Interval",
                ylab = "Number of Steps")
    print(weeekPlot)
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
