

### 1 - Code for reading in the dataset and/or processing the data

library(dplyr)
library(lubridate)
library(ggplot2)
library(mice)

mydata <- read.csv("activity.csv")
mydata$date <- ymd(mydata$date)


### 2 - Histogram of the total number of steps taken each day

mydata <- group_by(mydata, date)
dailysteps <- summarise(mydata, steps = sum(steps))
mydata <- ungroup(mydata)
dailysteps <- ungroup(dailysteps)

ggplot() + geom_histogram(data = dailysteps, aes(steps, fill = ..x..), bins = 20) +
                theme_dark() +
                scale_y_continuous(breaks = c(0, 2, 4, 6)) +
                labs(title = "Daily Steps Taken", x = "Steps", y = "Count") + 
                scale_fill_gradient(low = "blue", high = "red") + 
                theme(legend.position = "none")
                


### 3 - Mean and median number of steps taken each day

mnsteps <- mean(dailysteps$steps, na.rm = TRUE)
mdsteps <- median(dailysteps$steps, na.rm = TRUE)

mnsteps
mdsteps


### 4 - Time series plot of the average number of steps taken

mydata <- group_by(mydata, interval)
mnbyint <- summarise(mydata, avsteps = mean(steps, na.rm = TRUE))
mydata <- ungroup(mydata)
mnbyint <- ungroup(mnbyint)

ggplot(data = mnbyint, aes(interval, avsteps)) + 
        geom_line(col = "grey20", lwd = 0.7) + 
        theme_minimal() + 
        labs(x = "Time", y = "Average Steps", title = "Average Steps Taken Throughout the Day")


### 5 - The 5-minute interval that, on average, contains the maximum number of steps

maxinterval <- subset(mnbyint, avsteps == max(mnbyint[, 2]))
maxinterval


### 6 - Code to describe and show a strategy for imputing missing data.
###     I decided to use predictive mean matching (pmm) to impute the missing variables and
### used the "mice" package to compute this.

temp <- mice(mydata, m = 5, method = "pmm")
imputed <- complete(temp)

### 7 - Histogram of the total number of steps taken each day after missing values are imputed

imputed <- group_by(imputed, date)
impsteps <- summarise(imputed, steps = sum(steps))
imputed <- ungroup(imputed)
impsteps <- ungroup(impsteps)

ggplot() + geom_histogram(data = impsteps, aes(steps, fill = ..x..), bins = 20) +
        theme_dark() +
        scale_y_continuous(breaks = c(0, 2, 4, 6)) +
        labs(title = "Daily Steps Taken of Imputed Data", x = "Steps", y = "Count") + 
        scale_fill_gradient(low = "blue", high = "red") + 
        theme(legend.position = "none")


### 8 - Panel plot comparing the average number of steps taken per 5-minute 
### interval across weekdays and weekends


z <- weekdays(imputed$date, TRUE)
z[z %in% c("Mon", "Tue", "Wed", "Thu", "Fri")] <- "Weekday"
z[z %in% c("Sat", "Sun")] <- "Weekend"


imputed$dayofweek <- as.factor(z)

imputed <- group_by(imputed, interval, dayofweek)
weeklyint <- summarise(imputed, avsteps = mean(steps))
imputed <- ungroup(imputed)
weeklyint <- ungroup(weeklyint)

ggplot(weeklyint, aes(x = interval, y = avsteps, group = dayofweek)) +
        geom_area(aes(fill = dayofweek)) + 
        facet_wrap(. ~ dayofweek) + 
        labs(x = "Interval (24Hour Time)", y = "Daily Steps", title = "Daily Steps by Weekday and Weekend") +
        theme_minimal(base_family = "serif") + 
        theme(legend.position = "none")
        
        
        
        