activity <- read.csv("activity.csv")
day_of_week <- weekdays(as.Date(activity$date))
number_of_steps <- sapply(split(activity$steps, activity$date), sum, na.rm=TRUE)
hist(number_of_steps)
mean_number_of_steps <- mean(number_of_steps)
median_number_of_steps <- median(number_of_steps)
print("mean number of steps per day:")
print(mean_number_of_steps)
print("median number of steps per day:")
print(median_number_of_steps)
y <- sapply(split(activity$steps, activity$interval), mean, na.rm=TRUE)
x <- names(y)
plot(x,y, type="l")
title(main="Average steps per 5min interval",xlab="Interval number",  ylab="Average number of steps")
x[y==max(y)]

NA_index <- is.na(activity$steps)
num_NA <- sum(NA_index)
median_steps <- sapply(split(activity$steps, activity$interval), median, na.rm=TRUE)

for(i in 1:length(activity$steps)){
  if(NA_index[i]){
    activity_noNA[i] <- unname(median_steps[activity$interval[i]==names(min_steps)])
  }else{
    activity_noNA[i] <- activity$steps[i]
  }
}
y <- sapply(split(activity_noNA, activity$date), sum, na.rm=TRUE)
hist(y)
title(main="Histogram of total number of steps taken in a day", xlab="total number of steps", ylab="counts")
mean_steps <- mean(y)
meadian_steps <- median(y)
print("mean number of steps per day:")
print(mean_steps)
print("median number of steps per day:")
print(median_steps)

is.weekday <- function(x){
  y <- NULL
  for(i in 1:length(x)){
  if(x[i]=="Sunday" || x[i]=="Saturday"){
    y <- c(y, FALSE)
  }else{
    y <- c(y,TRUE)
  }
  }
  y
}

z <- is.weekday(day_of_week)

