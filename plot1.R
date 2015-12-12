plot1 <- function () {
print("Checking if household_power_consumption.txt file exists................... [household_power_consumption.txt]");
if (!file.exists('household_power_consumption.txt')) {
  print("The household_power_consumption.txt file was not found!");
  print("Checking if household_power_consumption.zip file exists................... [household_power_consumption.zip]");
  if (!file.exists('household_power_consumption.zip')) {
    print("Starting download......................................................... [household_power_consumption.zip]");
    dataFile <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    download.file(dataFile, 'household_power_consumption.zip', method='curl')
    print(".......................................................................... [Done!]");
  }
  else {
    print(".......................................................................... [Zip file found!]");
  }
  print("Unzipping file............................................................ [household_power_consumption.zip]");
  unzip('household_power_consumption.zip')
  print(".......................................................................... [Done!]");
}
else {
  print(".......................................................................... [Text file found!]");
}

## Read the text file unziped.
print("Reading file to dataframe................................................. [household]");
household <- read.csv2('household_power_consumption.txt');
print(".......................................................................... [Done!]");

##Convert Date, Time and Numerals
print("Converting Date, Time and Numerals format................................. [household$...]");
household$Date <- as.Date(household$Date, "%d/%m/%Y");
household$Time <- strptime(household$Time, "%H:%M:%S");
household$Global_active_power <- as.numeric(levels(household$Global_active_power))[household$Global_active_power];
household$Global_reactive_power <- as.numeric(levels(household$Global_reactive_power))[household$Global_reactive_power];
household$Voltage <- as.numeric(levels(household$Voltage))[household$Voltage];
household$Global_intensity <- as.numeric(levels(household$Global_intensity))[household$Global_intensity];
household$Sub_metering_1 <- as.numeric(levels(household$Sub_metering_1))[household$Sub_metering_1];
household$Sub_metering_2 <- as.numeric(levels(household$Sub_metering_2))[household$Sub_metering_2];
household$Sub_metering_3 <- as.numeric(levels(household$Sub_metering_3))[household$Sub_metering_3];
print(".......................................................................... [Done!]");

## Selecting the target data $Date >= "2007-02-01" & $Date <= "2007-02-02"
print("Selecting the target data................................................. [household$Date >= 2007-02-01 and <= 2007-02-02]");
household <- subset(household, household$Date >= "2007-02-01" & household$Date <= "2007-02-02");
print(".......................................................................... [Done!]");

## Plotting
print("Starting plot............................................................. [plot1.png]");
png("plot1.png", width=480, height=480);
hist(household$Global_active_power, col="orangered", main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency");
dev.off();
print(".......................................................................... [Done!]");

}
