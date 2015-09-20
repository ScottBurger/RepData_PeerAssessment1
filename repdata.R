


#Calculate the total number of steps taken per day
data <- read.table("C:/Users/v-scburg/Desktop/repdata/activity.csv", sep=",", header=T)
steps_by_date <- split(data, data$date)
total_steps <- sapply(steps_by_date, function(x){sum(x[, c("steps")])})

date <- unique(data$date)

total_steps_df <- data.frame(total_steps)

#Make a histogram of the total number of steps taken each day
hist(total_steps)

#Calculate and report the mean and median of the total number of steps taken per day
mean(na.omit(total_steps_df$total_steps))
median(na.omit(total_steps_df$total_steps))


#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
fulldata <- na.omit(data)

series <- data.frame(unique(data$interval))

steps_by_interval <- split(fulldata, data$interval)
interval_steps <- sapply(steps_by_interval, function(x){mean(x[, c("steps")])})
interval_steps_df <- data.frame(interval_steps)
interval_steps_df$interval <- row.names(interval_steps_df)
plot(interval_steps_df$interval, interval_steps_df$interval_steps, type="l")

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
table(is.na(data$steps))[2]

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

for(i in 1:length(data$steps)){
  if(is.na(data$steps[i])){
    data$steps[i] <- interval_steps_df$interval_steps[interval_steps_df$interval==data$interval[i]]
    
  }
  
}

data_original <- read.table("C:/Users/v-scburg/Desktop/repdata/activity.csv", sep=",", header=T)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps_by_date <- split(data, data$date)
total_steps <- sapply(steps_by_date, function(x){sum(x[, c("steps")])})
hist(total_steps)
mean(na.omit(total_steps_df$total_steps)) #original: 10766.19
median(na.omit(total_steps_df$total_steps)) #original: 10765
#--the mean and median are the same as before by design. the tradeoff is that the distribution is taller in the middle, since the NA values are resampled with the average step data intervals. this resampling adds more data to the central peak of the histogram, but the overall shape is more or less preserved.


#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
library(chron)
data$isweekend <- is.weekend(data$date)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

fulldata <- na.omit(data)
series <- data.frame(unique(data$interval))
steps_by_interval <- split(fulldata, data$interval)
interval_steps <- sapply(steps_by_interval, function(x){mean(x[, c("steps")])})
interval_steps_df <- data.frame(interval_steps)
interval_steps_df$interval <- row.names(interval_steps_df)


fulldata_original <- na.omit(data_original)
steps_by_interval_original <- split(fulldata_original, data$interval)
interval_steps_original <- sapply(steps_by_interval_original, function(x){mean(x[, c("steps")])})
interval_steps_original_df <- data.frame(interval_steps_original)
interval_steps_original_df$interval <- row.names(interval_steps_original_df)

par(mfrow=c(2,1))
plot(interval_steps_df$interval, interval_steps_df$interval_steps, type="l")
plot(interval_steps_original_df$interval, interval_steps_original_df$interval_steps_original, type="l")







