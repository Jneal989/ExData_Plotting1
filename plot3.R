plot3 <- function(){
        ## Loads the Datafile, downloads and unzips it if it isn't in the working dir
        if(!file.exists("exdata-Fdata-Fhousehold_power_consumption.zip")){
                fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                download.file(fileurl, "exdata-Fdata-Fhousehold_power_consumption.zip", 
                              method = "curl")
                unzip("exdata-Fdata-Fhousehold_power_consumption.zip")
        }else if(!file.exists("household_power_consumption.txt")){
                unzip("exdata-Fdata-Fhousehold_power_consumption.zip")
        }
        ## Loads the dates and fines the dates that we want for this data set
        dates <- read.table("household_power_consumption.txt", na.strings = "?", 
                            colClasses = "character", sep = ";")[,1]
        dates.wanted <- grep("^(1|2)/2/2007",dates)
        Energydata <- read.table("household_power_consumption.txt", na.strings = "?", 
                                 sep = ";", header = TRUE )[dates.wanted,]
        rm(dates)
        ## Seeting the right class for Date and Time variables
        datetime <- paste(Energydata[,1], Energydata[,2]) 
        datetime <- strptime(datetime, format ="%d/%m/%Y %H:%M:%S" )
        Energydata$Time <- datetime
        Energydata$Date <- as.Date(Energydata$Date, format= "%d/%m/%Y")
        ## Plot first Submeter over Time as a line graph
        with(Energydata, plot(Time, Sub_metering_1, type = "l", xlab = "", 
                              ylab ="Energy Sub metering" ))
        ## add second and third submeters to the line graph
        lines(Energydata$Time, Energydata$Sub_metering_2, col="red")
        lines(Energydata$Time, Energydata$Sub_metering_3, col="blue")
        ## Add ledgend to graph
        legend("topright", legend = c("Sub_metering_1","Sub_metering_2",
                                      "Sub_metering_3"), lty = c(1,1,1), 
               col = c("black", "red", "blue"))
        ## copy plot to PNG device in working dir
        dev.copy(png, "plot3.png", width = 480, height = 480)
        dev.off()
}