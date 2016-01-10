
makePlot4 <- function() {
    
    # This code makes four plots using the data between 02/01/2007 - 
    # 02/02/2007.
    #
    # Plot_1: global active power vs time
    # Plot_2: Voltage vs time
    # Plot_3: sub_metering 1, 2, and 3 vs time
    # Plot_4: global reactive power vs time
    #
    # The output is a single PNG file named "plot4.png"   
    # Data source: "household_power_consumption.txt"
    
    # LOAD DATA
    # -----------------------------------------------------------------------     
    
        # Read data from file
        PowerData <- read.table("household_power_consumption.txt", na.strings = "?", sep = ";", header = TRUE)
    
        # Make time list
        DateTime <- strptime(paste(PowerData$Date, PowerData$Time), "%d/%m/%Y %H:%M:%S")
    
        # Compute the time differences measured from 2/1/2007
        DiffDateTime <- difftime(DateTime, as.POSIXlt("2007-02-01 00:00:00 CST"), unit = "days")
    
        # Make subset of the data (2/1/2007-2/2/2007)
        SubData <- subset(PowerData, DiffDateTime >= 0 & DiffDateTime < 2)
    
        # Make subset of the time list (2/1/2007-2/2/2007)
        SubDateTime <- subset(DateTime, DiffDateTime >= 0 & DiffDateTime < 2)
        
    # MAKE PNG FILE
    # -----------------------------------------------------------------------        
        
        # Open PNG graphics device for writing 
        png("plot4.png", width = 480, height = 480, units = "px", bg = "transparent")  
  
        # Set figure global parameters 
        par(mfrow = c(2,2), mar = c(4,4,2,1))

        # Add Plot 1
        plot(SubDateTime, SubData$Global_active_power, type = "n", xlab = "", ylab = "Global Active Power")
        lines(SubDateTime, SubData$Global_active_power)

        # Add Plot 2
        plot(SubDateTime,SubData$Voltage,type = "n", xlab = "datetime", ylab = "Voltage")
        lines(SubDateTime,SubData$Voltage)

        # Add Plot 3
        plot(SubDateTime, SubData$Sub_metering_1,type = "n", xlab = "", ylab = "Energy sub metering")
        lines(SubDateTime, SubData$Sub_metering_1,col = "black")
        lines(SubDateTime, SubData$Sub_metering_2,col = "red")
        lines(SubDateTime, SubData$Sub_metering_3,col = "blue")
        legend("topright", bty = "n", lty = c(1,1,1), col = c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

        # Add Plot 4
        plot(SubDateTime, SubData$Global_reactive_power, type = "n", xlab = "datetime", ylab = "Global_reactive_power")
        lines(SubDateTime, SubData$Global_reactive_power)

        # Close graphics device
        dev.off()

    # -----------------------------------------------------------------------
}