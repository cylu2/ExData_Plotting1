
makePlot2 <- function() {
    
    # This code makes a plot of the global active power between 02/01/2007 - 
    # 02/02/2007. The output is a PNG file named "plot2.png"   
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
        png("plot2.png", width = 480, height = 480, units = "px", bg = "transparent")         
        
        # Make plot
        plot(SubDateTime, SubData$Global_active_power, type="n", xlab="", ylab = "Global Active Power (kilowatts)")
        
        # Add line
        lines(SubDateTime, SubData$Global_active_power)

        # Close graphics device
        dev.off()
        
    # -----------------------------------------------------------------------
}