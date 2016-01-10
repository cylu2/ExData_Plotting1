
makePlot3 <- function() {
    
    # This code makes a plot of the sub_metering 1,2, and 3 between 02/01/2007 - 
    # 02/02/2007. The output is a PNG file named "plot3.png"    
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
        png("plot3.png", width = 480, height = 480, units = "px", bg = "transparent")        
        
        # Make plot
        plot(SubDateTime, SubData$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
        
        # Add lines
        lines(SubDateTime, SubData$Sub_metering_1, col = "black")
        lines(SubDateTime, SubData$Sub_metering_2,col = "red")
        lines(SubDateTime, SubData$Sub_metering_3,col = "blue")
        
        # Add legend
        legend("topright", lty = c(1,1,1), col = c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        
        # Close graphics device
        dev.off() 
        
    # -----------------------------------------------------------------------
}