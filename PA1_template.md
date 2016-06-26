# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip('activity.zip')
accel <- read.csv('activity.csv', header=TRUE, sep = ',')
str(accel)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
for(i in 1:length(accel) ) {
  str(accel[i])
  }
```

```
## 'data.frame':	17568 obs. of  1 variable:
##  $ steps: int  NA NA NA NA NA NA NA NA NA NA ...
## 'data.frame':	17568 obs. of  1 variable:
##  $ date: Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
## 'data.frame':	17568 obs. of  1 variable:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

We observe that the **date** variable is of the type **Factor**, we therefore recode it as a **date** variable.


```r
accel$date <- as.Date( accel$date, format = '%Y-%m-%d' )
str(accel$date)
```

```
##  Date[1:17568], format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
```

## What is mean total number of steps taken per day?
We start by loading the packages for data manipulation and plotting.


```r
library(plyr)
library(ggplot2)
```

We now compute the data needed for the plot.


```r
tot.steps <- ddply(accel, .(date),
                  summarize,
                  total.steps = sum(steps)
                  )
head(tot.steps)
```

```
##         date total.steps
## 1 2012-10-01          NA
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

We can now plot the data.


```r
tot.st.plot <- ggplot(tot.steps,
                      aes(x = total.steps)
                      )
tot.st.plot <- tot.st.plot + geom_histogram()
print(tot.st.plot)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk histogram](figure/histogram.png) 

We now report the summary statistics, to see the mean and median.


```r
summary(tot.steps$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8840   10800   10800   13300   21200       8
```

```r
summary(tot.steps$total.steps)['Mean']
```

```
##  Mean 
## 10800
```

```r
summary(tot.steps$total.steps)['Median']
```

```
## Median 
##  10800
```



## What is the average daily activity pattern?
Compute the data needed for the plot.


```r
av.steps <- ddply(accel, .(interval),
                  summarize,
                  average.steps = mean(steps, na.rm=TRUE
                                       )
                  )
head(av.steps)
```

```
##   interval average.steps
## 1        0       1.71698
## 2        5       0.33962
## 3       10       0.13208
## 4       15       0.15094
## 5       20       0.07547
## 6       25       2.09434
```

Plot the computed data.


```r
av.st.plot <- ggplot(av.steps,
                     aes(interval, average.steps)
                     )
av.st.plot <- av.st.plot + geom_line()
print(av.st.plot)
```

![plot of chunk plot](figure/plot.png) 

Find the maximum.


```r
summary(av.steps$average.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    2.49   34.10   37.40   52.80  206.00
```

```r
summary(av.steps$average.steps)['Max.']
```

```
## Max. 
##  206
```



## Imputing missing values

First we count the rows that have NAs and the ones that do not.


```r
table( complete.cases(accel) )
```

```
## 
## FALSE  TRUE 
##  2304 15264
```

Create a new data frame and impute missing values using interval averages.


```r
imp.accel <- accel
imp.accel$steps <- ifelse( is.na(imp.accel$steps),
                     av.steps$average.steps[match(imp.accel$interval, av.steps$interval) ],
                     imp.accel$steps )
table( complete.cases(accel) )
```

```
## 
## FALSE  TRUE 
##  2304 15264
```

```r
table( complete.cases(imp.accel) )
```

```
## 
##  TRUE 
## 17568
```


```r
tot.imp.steps <- ddply(imp.accel, .(date),
                  summarize,
                  total.steps = sum(steps)
                  )
head(tot.imp.steps)
```

```
##         date total.steps
## 1 2012-10-01       10766
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

We can now plot the data.


```r
tot.imp.st.plot <- ggplot(tot.imp.steps,
                      aes(x = total.steps)
                      )
tot.imp.st.plot <- tot.imp.st.plot + geom_histogram()
print(tot.imp.st.plot)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk imputed.histogram](figure/imputed.histogram.png) 

We now report the summary statistics, to see the mean and median.


```r
summary(tot.imp.steps$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9820   10800   10800   12800   21200
```

```r
summary(tot.imp.steps$total.steps)['Mean']
```

```
##  Mean 
## 10800
```

```r
summary(tot.imp.steps$total.steps)['Median']
```

```
## Median 
##  10800
```

The results of the imputing of data seem to be that the histogram is now a closer approximation of the Gaussian distribution, the mean and median appear to remain unchanged.

## Are there differences in activity patterns between weekdays and weekends?
First we create a day of the week variable in the accel data frame, then we create a list of weekdays and weekend days.



```r
accel$day <- weekdays(accel$date)

w.day <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
w.e.day <- c('Saturday', 'Sunday')

accel$weekpart <- ifelse(accel$day %in% w.day, 'weekday', 'weekend')
head(accel)
```

```
##   steps       date interval    day weekpart
## 1    NA 2012-10-01        0 Monday  weekday
## 2    NA 2012-10-01        5 Monday  weekday
## 3    NA 2012-10-01       10 Monday  weekday
## 4    NA 2012-10-01       15 Monday  weekday
## 5    NA 2012-10-01       20 Monday  weekday
## 6    NA 2012-10-01       25 Monday  weekday
```

```r
table(accel$weekpart)
```

```
## 
## weekday weekend 
##   12960    4608
```


Now computer the data for plotting.


```r
av.w.e.steps <- ddply(accel, .(interval, weekpart),
                  summarize,
                  average.steps = mean(steps, na.rm=TRUE
                                       )
                  )
head(av.w.e.steps)
```

```
##   interval weekpart average.steps
## 1        0  weekday        2.3333
## 2        0  weekend        0.0000
## 3        5  weekday        0.4615
## 4        5  weekend        0.0000
## 5       10  weekday        0.1795
## 6       10  weekend        0.0000
```

Now plot in two facets using this factor variable.


```r
av.w.e.st.plot <- ggplot(av.w.e.steps,
                     aes(interval, average.steps)
                     )
av.w.e.st.plot <- av.w.e.st.plot + geom_line()
av.w.e.st.plot <- av.w.e.st.plot + facet_grid(weekpart ~ .)
print(av.w.e.st.plot)
```

![plot of chunk facet.plot](figure/facet.plot.png) 
