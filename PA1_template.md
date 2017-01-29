Step 1: Loading the data
------------------------

    unzip(zipfile="activity.zip")
    data <- read.csv("activity.csv")
    knitr::opts_chunk$set(echo = TRUE)

Step 2:What is the mean total number of steps taken per day?
------------------------------------------------------------

    library(ggplot2)
    total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
    qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    mean(total.steps, na.rm=TRUE)

    ## [1] 9354.23

    median(total.steps, na.rm=TRUE)

    ## [1] 10395

    knitr::opts_chunk$set(echo = TRUE)

Step 3: What is the daily average activity pattern?
---------------------------------------------------

    library(ggplot2)
    averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                FUN=mean, na.rm=TRUE)
    ggplot(data=averages, aes(x=interval, y=steps)) +
      geom_line() +
      xlab("5-minute interval") +
      ylab("average number of steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    knitr::opts_chunk$set(echo = TRUE)

The 5-minute interval that, on average, contains the maximum number of
steps

    averages[which.max(averages$steps),]

    ##     interval    steps
    ## 104      835 206.1698

    knitr::opts_chunk$set(echo = TRUE)

Step 4: Code describing and showing a strategy for imputing missing data
------------------------------------------------------------------------

    missing <- is.na(data$steps)
    # How many missing
    table(missing)

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

All of the missing values are filled in with mean value for that
5-minute interval.

    fill.value <- function(steps, interval) {
      filled <- NA
      if (!is.na(steps))
        filled <- c(steps)
      else
        filled <- (averages[averages$interval==interval, "steps"])
      return(filled)
    }
    filled.data <- data
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

Make a histogram of the total number of steps taken each day and
calculate the mean and median total number of steps.

    library(ggplot2)
    total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
    qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    mean(total.steps)

    ## [1] 10766.19

    median(total.steps)

    ## [1] 10766.19

Step 5:Average number of steps across weekdays and weekends
-----------------------------------------------------------

We will only use the filled cells

    weekday.or.weekend <- function(date) {
      day <- weekdays(date)
      if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
      else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
      else
        stop("invalid date")
    }
    filled.data$date <- as.Date(filled.data$date)
    filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

This will create a panel plot containing plots of average number of
steps taken on weekdays and weekends.

    averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
      xlab("5-minute interval") + ylab("Number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)
