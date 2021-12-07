## Loading and preprocessing the data
## activity.csv

**Load Libraries**
library(ggplot2)
library(dplyr)
library(data.table)

## Unzip archive 'activity.zip' to current working directory
unzip("activity.zip", exdir=".")

csv_file <- read.csv2(file="activity.csv", sep="," , header=TRUE)
setDT(csv_file)

## Show only non-NA rows
head(csv_file[complete.cases(csv_file), ],10)

############################################################################
## What is mean total number of steps taken per day?
############################################################################
mean_steps <- mean(csv_file$steps, na.rm=TRUE)
print("Mean total number of steps taken per day: ", mean_steps)
## 37.3826

############################################################################
## Imputing missing values
############################################################################
## Total Number of missing values
num_of_NAs_in_rows <- apply(final, 1, function(x){any(is.na(x))})
print("Number of rows with missing values - NAs: ", num_of_NAs_in_rows)

## Update dataset to impute  -- used median values for NA -- is '0'
## To replace missing values in a single column - csv_file$steps
csv_file$steps[is.na(csv_file$steps)] <- median(csv_file$steps, na.rm=TRUE)

## Convert 'date' from 'character' to 'Date'
csv_file$date <- as.Date(csv_file$date, format="%Y-%m-%d")

############################################################################
## What is the average daily activity pattern?
############################################################################
## Group by date to create plot and add steps taken in a day
plot_steps <- csv_file %>% group_by(date) %>% summarize(steps = sum(steps))

## What is mean total number of steps taken per day?
mean_steps <- mean(plot_steps$steps, na.rm=TRUE)
print(mean_steps)
## 37.3826

median_steps <- median(plot_steps$steps, na.rm=TRUE)
print(median_steps)
## 0

ggplot(plot_steps, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Steps taken in a Day", x = "Steps")
ggsave('instructions_fig/steps-taken-in-a-day', width=8, height=7)

############################################################################
### Average Daily Steps
############################################################################
csv_file1$steps[is.na(csv_file1$steps)] <- median(csv_file1$steps, na.rm=TRUE)
# str(csv_file1)
# head(csv_file1,10)
# dim(csv_file)
# csv_file1 <- csv_file
# str(csv_file1)
# csv_file1 <- mutate(csv_file1, DayofWeek = weekdays(csv_file1$date))
# str(csv_file1)
# head(csv_file1,10)
csv_file1 <- set_weekdays_weekend(csv_file1)

avgSteps <- csv_file1
avgSteps1 <- avgSteps %>% group_by(interval) %>% summarize(steps = mean(steps))
str(avgSteps1)

## Plot Line Graph
g <- ggplot(avgSteps1, aes(x=interval, y=steps)) +geom_line(color="blue", size=1)
print(g)

## Save Plot
ggsave('instructions_fig/average-daily-steps.png', width=8, height=7)

############################################################################
## Are there differences in activity patterns between weekdays and weekends?
############################################################################
## Add DayofWeek column
csv_file <- mutate(csv_file, DayofWeek = weekdays(csv_file$date))

## Function to add 'weekday_or_weekend column to plot
## credit: https://stackoverflow.com/questions/38649533/case-when-in-mutate-pipe
## credit: https://www.statology.org/conditional-mutating-r/
set_weekdays_weekend <- function(df) {
    df %>% mutate(weekday_or_weekend = case_when( 
        DayofWeek == "Monday" ~ "weekday", 
        DayofWeek == "Tuesday" ~ "weekday", 
        DayofWeek == "Wednesday" ~ "weekday", 
        DayofWeek == "Thursday" ~ "weekday", 
        DayofWeek == "Friday" ~ "weekday", 
        DayofWeek == "Saturday" ~ "weekend", 
        DayofWeek == "Sunday" ~ "weekend",
    ))
}

## Add two more columns -- weekend and weekday
csv_file <- set_weekdays_weekend(csv_file)
test_weekend <- csv_file[csv_file$weekday_or_weekend == "weekend",]
head(test_weekend,10)

plot_weekend_weekday <- csv_file1 %>% group_by(interval, weekday_or_weekend) %>% summarize(steps = mean(steps))

## Create ggplot
g <- ggplot(plot_weekend_weekday, aes(x=interval, y=steps, color=weekday_or_weekend))
g + geom_line() + labs(x = "xlabtext") + labs(y = "ylabtext") + labs(title = "Activity Patterns between Weekdays & Weekends") + facet_wrap(~weekday_or_weekend, ncol =1, nrow=2)
ggsave('instructions_fig/walking-patterns-weekday-weekend.png', width=8, height=7)



