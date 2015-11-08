#getwd()
#setwd()

#CODE FOR THE DATA LOAD, DATA MANIPULATION dataset gets loaded, and various data checks are performed (Data formating, Data subsetting)
##Step 1: download the zip file   => Dataset: Electric power consumption [20Mb]
##Step 2. read the csv file, caution NA values

hhpc_data <- read.csv("./household_power_consumption.txt", header=T, sep=';', na.strings="?", 
                      nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')

#check summary
summary(hhpc_data)
#check dimensions
dim(hhpc_data)
#result : [1] 2075259       9  / correct

#validate if dataset consist of columns as stated in the introduction
names(hhpc_data)
##result: [1] "Date"  "Time"  "Global_active_power"  "Global_reactive_power" "Voltage"  "Global_intensity"  "Sub_metering_1"  "Sub_metering_2"  "Sub_metering_3"     
##correct, all fine

#format date
hhpc_data$Date <- as.Date(hhpc_data$Date, format="%d/%m/%Y")  #initial format  16/12/2006  new format 2006-12-16
#check top 
head(hhpc_data)
#check tail
tail(hhpc_data)
#okay, proceed further

## subsetting the dataset to following timerange: 2007-02-01   and 2007-02-02
hhpc_subset <- subset(hhpc_data, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
dim(hhpc_subset) #result , dataset to work with for the assignment [1] 2880    9 

#******************************************************************************************#
# CODE FOR PLOTTING THE SUBSETTED DATASET
## Requirements: Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
##Name each of the plot files as plot1.png, plot2.png ...

## convert date format
dt <- paste(as.Date(hhpc_subset$Date), hhpc_subset$Time)
hhpc_subset$Datetime <- as.POSIXct(dt)
head(hhpc_subset)
str(hhpc_subset)

#*************************************************************************#

# CODE FOR PLOTTING THE SUBSETTED DATASET
## Requirements: Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
##Name each of the plot files as plot1.png, plot2.png ...

## convert date format
dt <- paste(as.Date(hhpc_subset$Date), hhpc_subset$Time)
hhpc_subset$Datetime <- as.POSIXct(dt)
head(hhpc_subset)
str(hhpc_subset)


## Plot 4 "Global Active Power", "Voltage", "Energy sub metering", "Global_reactive_power"
par(mfrow=c(2,2), mar=c(4,4,2,2),oma=c(0,0,2,0))
with(hhpc_subset, {
        plot(Global_active_power~Datetime, type="l", 
             ylab="Global Active Power", xlab="")
        plot(Voltage~Datetime, type="l", 
             ylab="Voltage", xlab="datetime")
        plot(Sub_metering_1~Datetime, type="l", 
             ylab="Energy sub metering", xlab="")
        lines(Sub_metering_1~Datetime,col='black')
        lines(Sub_metering_2~Datetime,col='red')
        lines(Sub_metering_3~Datetime,col='blue')
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Global_reactive_power~Datetime, type="l", 
             ylab="Global_reactive_power",xlab="datetime")
})

## Saving to file
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()
#Comments: Computer system set to German language, hence Do= Thu, Fr=Fri , Sa=Sat


#END CODE#