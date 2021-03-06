## Plot 1

setwd("~/R/ExData_Plotting1")

#Load data
dat <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
# Subset
dat <- subset(dat, Date == "1/2/2007" | Date == "2/2/2007")

dat$Global_active_power <- as.numeric(dat$Global_active_power)
dat$Global_reactive_power <- as.numeric(dat$Global_reactive_power)
dat$Global_intensity<- as.numeric(dat$Global_intensity)
dat$Voltage <- as.numeric(dat$Voltage)
dat$Sub_metering_1 <- as.numeric(dat$Sub_metering_1)
dat$Sub_metering_2 <- as.numeric(dat$Sub_metering_2)
dat$Sub_metering_3 <- as.numeric(dat$Sub_metering_3)

png(filename = "plot1.png", width = 480, height = 480)

hist(dat$Global_active_power, 
     col = "red", 
     xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power",
     ylim = c(0,1200))

dev.off()
