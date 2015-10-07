plot3 <- function() {
  downloadedZipFile = "data.zip"
  downloadFolder = "tmp" 
  # get path for tmp folder
  pTmp <- function(fn) {
    return (paste(downloadFolder, fn, sep = "/"))
  }
  # function to download the zip file from the given URL
  unzipFile <- function(zipFileUrl) {
    
    if (!file.exists(downloadFolder)) {
      
      print("creating tmp folder and downloading the zip file.")
      # create a tmp folder
      dir.create(downloadFolder)
      # download the file file into tmp folder
      download.file(zipFileUrl, pTmp(downloadedZipFile), method = "curl")
      # unzip the file
      unzip(pTmp(downloadedZipFile), exdir=downloadFolder)
    }
  }
  # function to delete tmp folder
  deleteTmpFolder <- function() {
    if (file.exists(downloadFolder)) {
      print("deleting tmp folder")
      unlink(downloadFolder, recursive = TRUE)
    }
  }
  
  print("into plot3 > calling unzipFile")
  # download the zip file from the given URL
  unzipFile("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip");
  
  print("into plot3 > loading powerData")
  # read the file content
  powerData = read.table(
    pTmp("household_power_consumption.txt"), 
    header=TRUE, 
    sep = ";", 
    na.strings = "?",
    colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  #print(head(powerData))
  # subset data for date between 2007-02-01 and 2007-02-02
  subPowerData = powerData[powerData$Date %in% c("1/2/2007","2/2/2007") ,]
  print(head(subPowerData))
  # create a png file
  png("plot3.png", width=480, height=480)
  # plot chart using subPowerData data - sub metering - 1
  plot(subPowerData$Sub_metering_1~as.POSIXct(paste(as.Date(subPowerData$Date, "%d/%m/%Y"), subPowerData$Time)), 
       xlab = "",
       ylab = "Energy sub metering",
       #main = "Global Active Power",
       type="l")
  # plot chart using subPowerData data - sub metering - 2
  points(subPowerData$Sub_metering_2~as.POSIXct(paste(as.Date(subPowerData$Date, "%d/%m/%Y"), subPowerData$Time)), 
       xlab = "",
       ylab = "Energy sub metering",
       #main = "Global Active Power",
       type="l", col="red")
  # plot chart using subPowerData data - sub metering - 3
  points(subPowerData$Sub_metering_3~as.POSIXct(paste(as.Date(subPowerData$Date, "%d/%m/%Y"), subPowerData$Time)), 
         xlab = "",
         ylab = "Energy sub metering",
         #main = "Global Active Power",
         type="l", col="blue")
  legend("topright", cex=0.8, lwd=2, col=c("black","red","blue"),
         legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.off()
  
  # delete tmp folder
  deleteTmpFolder()
}