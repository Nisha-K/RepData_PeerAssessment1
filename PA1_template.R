###Importing the data
mdatatotal<-read.csv("D:/#GoogleDrive/#Nisha/Coursera - DataScience/Reproducible Research/Assignment1/activity.csv")
head(mdatatotal)
summary(mdatatotal)
mdata<-subset(mdatatotal,!is.na(mdatatotal$steps))
head(mdata)

###Histogram of the total number of steps taken each day, mean and median number of steps taken each day
StepsPerDay<-aggregate(mdata$steps~mdata$date,mdata,sum)
StepsPerDay<-data.frame(StepsPerDay)
head(StepsPerDay)
names(StepsPerDay)[names(StepsPerDay)=="mdata.date"] <- "Date"
names(StepsPerDay)[names(StepsPerDay)=="mdata.steps"] <- "TotalSteps"

library("ggplot2")
qplot(StepsPerDay$TotalSteps,geom = "histogram",main="Distibution of steps walked each day"
      ,xlab="No. of steps per day",ylab="Frequency of days",col=I("blue"),fill=I("Green"))

###Below code shows a set of 10 values of mean and median of total no. of steps taken per day per day
StepsCount_Mean<-aggregate(StepsPerDay$Steps~StepsPerDay$Date,StepsPerDay,mean)
StepsCount_Median<-aggregate(StepsPerDay$`mdata$steps`~StepsPerDay$`mdata$date`,StepsPerDay,median)
head(StepsCount_Mean,10)
head(StepsCount_Median,10)

###Time series plot of the average number of steps taken
DailyMean<-aggregate(mdata$steps~mdata$interval,mdata,mean)
DailyMean<-data.frame(DailyMean)
head(DailyMean)
names(DailyMean)[names(DailyMean)=="mdata.interval"] <- "Interval"
names(DailyMean)[names(DailyMean)=="mdata.steps"] <- "MeanOfSteps"
ggplot(DailyMean, aes(Interval, MeanOfSteps)) + geom_line() + xlab("Intervals") + ylab("No. of steps") +
  ggtitle("Time series distribution")

###This code will show the entry that has highest of average steps taken
MaxOfSteps<-subset(DailyMean,DailyMean$MeanOfSteps==max(DailyMean$MeanOfSteps))
MaxOfSteps

###Total data missing regarding no. of steps
sum(is.na(mdatatotal$steps))

###Lets replace missing values with mean value of steps column
newmdatatotal<-mdatatotal
head(newmdatatotal)
a<-is.na(newmdatatotal$steps)
avgdata<- tapply(newmdatatotal$steps, newmdatatotal$interval, mean, na.rm=TRUE, simplify=TRUE)
newmdatatotal$steps[a] <- avgdata
head(newmdatatotal)

StepsPerDay1<-aggregate(newmdatatotal$steps~newmdatatotal$date,newmdatatotal,sum)
StepsPerDay1<-data.frame(StepsPerDay1)
head(StepsPerDay1)
names(StepsPerDay1)[names(StepsPerDay1)=="newmdatatotal.date"] <- "Date"
names(StepsPerDay1)[names(StepsPerDay1)=="newmdatatotal.steps"] <- "TotalSteps"

qplot(StepsPerDay1$TotalSteps,geom = "histogram",main="Distibution of steps walked each day"
      ,xlab="No. of steps per day",ylab="Frequency of days",col=I("blue"),fill=I("Green"))

####Mean and median of new data
mean(StepsPerDay1$TotalSteps)
median(StepsPerDay1$TotalSteps)

###Create factor variable for weekdays and weekend
newmdatatotal$date<-as.Date(as.character(newmdatatotal$date))
newmdatatotal$day<-weekdays(newmdatatotal$date)
newmdatatotal$weekday<- as.factor(c("weekend", "weekday"))
newmdatatotal[newmdatatotal$day == "Sunday" | newmdatatotal$day == "Saturday" ,5]<- factor("weekend")
newmdatatotal[!(newmdatatotal$day == "Sunday" | newmdatatotal$day == "Saturday"),5 ]<- factor("weekday")

###Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
library(lattice)
interval <- aggregate(steps ~ interval + weekday, newmdatatotal, mean)
xyplot(steps ~ interval | weekday, data=interval, layout=c(1,2), type='l')
