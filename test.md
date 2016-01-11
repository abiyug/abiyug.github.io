---
layout: page
title: test
author: "Abiyu Giday"
date: "September 18, 2015"
output: 
         html_document:
             toc: true
---

#### _Table of Content_
1. Overview
2. Q-1 Loading and preprocessing the data
   + Process/transform the data (if necessary) into a format suitable for your analysis
3. Q-2 What is mean total number of steps taken per day?
   + Calculate the total number of steps taken per day
   + Make a histogram of the total number of steps taken each day
   + Calculate and report the mean and median of the total number of steps taken per day
4. Q-3 What is the average daily activity pattern?
   + Make a time series plot on the average number of steps taken
   + Which 5-minute interval contains the maximum number of steps?
5. Q-4 Imputing missing values
   + Calculate and report the total number of missing values in the dataset
   + Impute missing values in the dataset
   + Create a new dataset that is equal to the original dataset but with the missing data filled in.
   + Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total    number of steps taken per day.
6. Q-5 Are there differences in activity patterns between weekdays and weekends?
   + Preparing the data table for weekday/weekend activty pattern comparison
   + Create a new factor variable with two levels – “weekday” and “weekend”
   + Make a time series plot the average number of steps taken, averaged across all weekday days or weekend days
7. Q-6 What are the weekly Distribution of steps taken for the two months?
   + Figure 5: Daily steps bar chart
8. Over all observation

# Overview
<img src="http://www.ericselectronics.com/wp-content/uploads/2015/06/HAMSWAN-Smart-Wrist-Watch-Bluetooth-40-Fitness-Tracker-Health-Smartwatch-with-Heart-Rate-Monitor-Phone-Watches-NFC-Function-for-IOS-Android-Iphone-Samsung-HTC-Smartphones-White-0.jpg" align="right" width="35%" height="35%" />
This analysis makes use of data from a personal activity monitoring device, such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take   measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there 
is a lack of statistical methods and software for processing and interpreting the data. This script Calculate the total number of steps taken per day, calculate and report the mean and median of the total number of steps, plots histogram of the total number of steps, shows average daily activity pattern,  method that impute missing values from the data, and Make a panel plot containing a time series plots. 

## Q-1 Loading and preprocessing the data


```r
# Download and save the file in data directory
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/Factivity.zip", method = "curl")
unzip("./data/Factivity.zip")
```

### Process/transform the data (if necessary) into a format suitable for your analysis

```r
library(dplyr)

actvty <- read.csv("activity.csv") #data frame in active directory
actvty <- tbl_df(actvty) # convert df to dt
actvty1 <- na.omit(actvty)  # remove missing rows from the data frame
actvty1 <- select(actvty1, date, interval, steps)       # rearrange the data columns
actvty1 <- actvty1 %>%
        group_by(date) %>% 
                summarise_each(funs(sum), steps) # use dplyr to sum steps each day
str(actvty1)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	53 obs. of  2 variables:
##  $ date : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 3 4 5 6 7 9 10 11 12 ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...
##   .. ..- attr(*, "names")= chr [1:2304] "1" "2" "3" "4" ...
```

```r
head(actvty1)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
##       (fctr) (int)
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

##Q-2 What is mean total number of steps taken per day?


```r
TotalSteps <- actvty1$steps 
summary(TotalSteps) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```


### Calculate the total number of steps taken per day

```r
sum(actvty1$steps)
```

```
## [1] 570608
```
The total number steps take is 570608.

### Make a histogram of the total number of steps taken each day
#### Figure 1: A histogram for 10/12 & 11/2

```r
actvtyHist <- hist(actvty1$steps,  breaks = 15, freq = T, col = "yellow", 
                   main = "Histogram for total number of Steps taken per day",
                   xlab = "Daily step count", ylab ="Frequency")
rug(actvty1$steps) # rug under the histogram to show step concentration
abline(v = mean(actvty1$steps), col = "red", lwd = 4)
```

![plot of chunk Fig-1dayTotalSteps](figure/Fig-1dayTotalSteps-1.png) 

### Calculate and report the mean and median of the total number of steps taken per day

```r
TotalSteps <- actvty1$steps 
summary(TotalSteps) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

##Q-3 What is the average daily activity pattern?
### Make a time series plot on the average number of steps taken

Loading and preprocessing the data for the time series plot

```r
library(ggplot2)

str(actvty)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
actvty$date <- as.Date(actvty$date)
actvty2sum <- actvty %>% 
        group_by(date) %>% 
                    summarise_each(funs(sum), steps) # sum the steps for each day
actvty2sum <- na.omit(actvty2sum)      #remove rows with unavilable data - NA
head(actvty2sum)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
##       (date) (int)
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

####Figure 2: A Time series plot for the total Steps Across all days

```r
qplot(steps, date, data = actvty2sum, geom = c("line", "smooth"), method ="lm") + 
        ggtitle("Average total steps Across all days") + 
        labs( x = "Steps", y = "Days") +
        theme(plot.title = element_text(lineheight=.8, face="bold")) 
```

![plot of chunk Fig-2TSstepsAcrossDays](figure/Fig-2TSstepsAcrossDays-1.png) 

### Which 5-minute interval contains the maximum number of steps?
 

```r
 summary(actvty)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
 filter(actvty, steps ==806)
```

```
## Source: local data frame [1 x 3]
## 
##   steps       date interval
##   (int)     (date)    (int)
## 1   806 2012-11-27      615
```

##Q-4 Imputing missing values
To impute value for missing data a numberof model/techhniques are available to choose from. For this excersie we are using the **Last value carried forward _(LOCF)_** technique.  The last observed value is used to fill in missing values in subsequent observations, this method assums that the most recent observation is the best guess for subsequent missing values. We are using the R **_ZOO_** package's **na.locf** function.
### Prepare the data 

```r
actvty3 <- read.csv("activity.csv")                #data frame in active directory

actvty3 <- actvty3 %>%
             group_by(date) %>% 
                summarise_each(funs(sum), steps)   # sum of  steps each day
```
### Calculate and report the total number of missing values in the dataset

```r
sum(is.na(actvty3)) # how many days of misseang value
```

```
## [1] 8
```

```r
mean(is.na(actvty3)) 
```

```
## [1] 0.06557377
```
#### Does the presence of missing days introduced a bias?
In order to answer the imapct of bias after imputing data, the following steps in the _zoo_ package na.locf techinque is utlized.

### Impute missing values in the dataset

```r
library(zoo)

actvty3 <- actvty3 %>%
             group_by(date) %>% 
                summarise_each(funs(sum), steps)   # sum of  steps each day

# change the var type back to Date/numeric
actvty3$steps <- as.numeric(actvty3$steps)
actvty3$date <- as.Date(actvty3$date)
summary(actvty3$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

```r
actvty33 <- na.locf(actvty3, na.rm = FALSE) #replace NA with the Last Value from the top down
actvty333 <- na.locf(actvty33, na.rm = FALSE, fromLast = TRUE) # NA replace from the last data point to complete the cricle

# change the var type back to Date/numeric
actvty333$steps <- as.numeric(actvty333$steps)
actvty333$date <- as.Date(actvty333$date)
```
#### Confirming Imputed value remove all NA

```r
sum(is.na(actvty333)) # how many days of misseang value
```

```
## [1] 0
```

```r
mean(is.na(actvty333))
```

```
## [1] 0
```

```r
ImpTotlSteps <- actvty3$steps
summary(ImpTotlSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
actvty333 <- tbl_df(actvty333)
str(actvty333)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	61 obs. of  2 variables:
##  $ date : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps: num  126 126 11352 12116 13294 ...
```

```r
sum(is.na(actvty333))
```

```
## [1] 0
```

```r
head(actvty333)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
##       (date) (dbl)
## 1 2012-10-01   126
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
AddSteps = sum(actvty333$steps) - sum(actvty3$steps, na.rm = TRUE)
T =  AddSteps / sum(actvty3$steps, na.rm = TRUE) * 100
```
The imputed data with locf technique added 5.7947 &times; 10<sup>4</sup> additional steps.Which is 10.155308 % more, and this could add a slight bias in the analysis. 

##### Side by side summary comparison shows in this table

    | Summary  |  Msng NA   | Imputed NA |
    | -------- | ---------- | ---------- | 
    | Min      |   41       |    41      |   
    | Median   |   10760    |    10570   |
    | Mean     |   10770    |    10300   |
    | Max      |   21990    |    21990   |
    | Total    |   570608   |    628555  |

     
The imputed data with locf technique added 5.7947 &times; 10<sup>4</sup> many more steps. Which is 10.155308 % more. That is statstically significant could introduce bias in the analysis. 

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
     
#### Figure 3: Side by side comparison of total steps with missing and imputed data.

```r
par(mfrow = c(1,2))

actvtyHist <- hist(actvty1$steps,  breaks = 15, freq = T, col = "yellow", 
                   main = "Total steps with missing data removd",
                   xlab = "Daily step count", ylab ="Frequency")
                   rug(actvty1$steps)
                   abline(v = mean(actvty1$steps), col = "red", lwd = 4)

actvtyHist3 <- hist(actvty333$steps,  breaks = 15, freq = T, col = "Green", 
                    main = "Total steps with missing data imputed",
                    xlab = "Daily step count", ylab ="Frequency")
                    rug(actvty333$steps)
                    abline(v = mean(actvty333$steps), col = "red", lwd = 4)
```

![plot of chunk Fig-3compareNAvsimputed](figure/Fig-3compareNAvsimputed-1.png) 



##Q-5 Are there differences in activity patterns between weekdays and weekends?
### Preparing the data table for weekday/weekend activty pattern comparison

```r
actvtyWK <- actvty333  %>% mutate( DaysOfWk = weekdays(as.Date(date))) # Add coresponding weekday
str(actvtyWK)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	61 obs. of  3 variables:
##  $ date    : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps   : num  126 126 11352 12116 13294 ...
##  $ DaysOfWk: chr  "Monday" "Tuesday" "Wednesday" "Thursday" ...
```

```r
head(actvtyWK, 10)
```

```
## Source: local data frame [10 x 3]
## 
##          date steps  DaysOfWk
##        (date) (dbl)     (chr)
## 1  2012-10-01   126    Monday
## 2  2012-10-02   126   Tuesday
## 3  2012-10-03 11352 Wednesday
## 4  2012-10-04 12116  Thursday
## 5  2012-10-05 13294    Friday
## 6  2012-10-06 15420  Saturday
## 7  2012-10-07 11015    Sunday
## 8  2012-10-08 11015    Monday
## 9  2012-10-09 12811   Tuesday
## 10 2012-10-10  9900 Wednesday
```

### Create a new factor variable with two levels – “weekday” and “weekend” 

```r
actvtyWKD <- actvtyWK %>%  mutate(TypeOfDay = ifelse(DaysOfWk == "Saturday" | DaysOfWk =="Sunday", "weekend", "weekday" )) # add weekday & weekend variable 

actvtyWKD[actvtyWKD == 0] <- NA  # convert 0 value data to NA
actvtyWKD1 <- na.locf(actvtyWKD, na.rm = FALSE) #replace NA with the Last Value from the top down
actvtyWKD2 <- na.locf(actvtyWKD1, na.rm = FALSE, fromLast = TRUE) # NA replace from the last data point to complete the cricle

# #change variable data types 
actvtyWKD2$TypeOfDay <- as.factor(actvtyWKD2$TypeOfDay)
actvtyWKD2$DaysOfWk <- as.factor(actvtyWKD2$DaysOfWk) 
actvtyWKD2$steps <- as.numeric(actvtyWKD2$steps)
actvtyWKD2$date <- as.Date(actvtyWKD2$date)

str(actvtyWKD2)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	61 obs. of  4 variables:
##  $ date     : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps    : num  126 126 11352 12116 13294 ...
##  $ DaysOfWk : Factor w/ 7 levels "Friday","Monday",..: 2 6 7 5 1 3 4 2 6 7 ...
##  $ TypeOfDay: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 2 2 1 1 1 ...
```

```r
head(actvtyWKD2, 10)
```

```
## Source: local data frame [10 x 4]
## 
##          date steps  DaysOfWk TypeOfDay
##        (date) (dbl)    (fctr)    (fctr)
## 1  2012-10-01   126    Monday   weekday
## 2  2012-10-02   126   Tuesday   weekday
## 3  2012-10-03 11352 Wednesday   weekday
## 4  2012-10-04 12116  Thursday   weekday
## 5  2012-10-05 13294    Friday   weekday
## 6  2012-10-06 15420  Saturday   weekend
## 7  2012-10-07 11015    Sunday   weekend
## 8  2012-10-08 11015    Monday   weekday
## 9  2012-10-09 12811   Tuesday   weekday
## 10 2012-10-10  9900 Wednesday   weekday
```

###


### Make a time series plot the average number of steps taken, averaged across all weekday days or weekend days
#### Figure 4: Weekdays Vs Weekend time series plot 

```r
qplot(steps, date, data = actvtyWKD2, geom = c("line", "smooth"), method ="lm",facets = .~TypeOfDay) + 
        ggtitle("Average total steps on Weekdays and Weekends") + 
        labs( x = "Steps", y = "days") +
        theme(plot.title = element_text(lineheight=.8, face="bold"))
```

![plot of chunk Fig-4weekdayVSweekend](figure/Fig-4weekdayVSweekend-1.png) 

## Q-6 What are the weekly Distribution of steps taken for the two months?

```r
actvtyWKD2
```

```
## Source: local data frame [61 x 4]
## 
##          date steps  DaysOfWk TypeOfDay
##        (date) (dbl)    (fctr)    (fctr)
## 1  2012-10-01   126    Monday   weekday
## 2  2012-10-02   126   Tuesday   weekday
## 3  2012-10-03 11352 Wednesday   weekday
## 4  2012-10-04 12116  Thursday   weekday
## 5  2012-10-05 13294    Friday   weekday
## 6  2012-10-06 15420  Saturday   weekend
## 7  2012-10-07 11015    Sunday   weekend
## 8  2012-10-08 11015    Monday   weekday
## 9  2012-10-09 12811   Tuesday   weekday
## 10 2012-10-10  9900 Wednesday   weekday
## ..        ...   ...       ...       ...
```

```r
actvtyPerDay <- actvtyWKD2 %>%
                  group_by(DaysOfWk) %>% 
                     summarise_each(funs(sum), steps) # sum of  steps each day
Dailyactv <- arrange(actvtyPerDay, desc(steps))
Dailyactv
```

```
## Source: local data frame [7 x 2]
## 
##    DaysOfWk  steps
##      (fctr)  (dbl)
## 1 Wednesday 101662
## 2    Friday  96784
## 3    Sunday  96515
## 4  Saturday  90967
## 5  Thursday  81116
## 6    Monday  80965
## 7   Tuesday  80546
```
### Figure 5: Daily steps bar chart

```r
ggplot(Dailyactv, aes(x = factor(DaysOfWk), y = steps, color = DaysOfWk)) + geom_bar(stat = "identity") + ggtitle("Steps per day distribution") + xlab("Days of the week") 
```

![plot of chunk Fig-5dailyActvty](figure/Fig-5dailyActvty-1.png) 

# Overall observation
-  For the observed activty data, the steps tend to increase from Monday to Friday. On Weekend About the same number of steps taken on saturday and sunday.
- The imputed data with locf technique added 5.7947 &times; 10<sup>4</sup> additional steps. Which is 10.155308 % more. This could add a slight bias in the analysis, however the model factors in somemarigin of error.  
-  November 27, the 615th interval contains the maximum number of steps.
- The total number steps take not including the missing data is 570608.
The subject the activty monitor data analyzed walked less at the beggining of the week, increasing  activity towards the end of the week.  
- The most steps are taken on Wednesdays, with 26% more steps compare to least active day Tuesdays.



