plot4 <- function () {
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

## Identify the days
days <- c(unique(household$Date));
weekday <- weekdays(days, abbreviate = TRUE);
weekday <- c(weekday, weekdays(max(household$Date)+1, abbreviate = TRUE));
nrowdays <- c(1);
for (i in 2:length(days)) {
  nrowdays <- c(nrowdays, min(which(household$Date==days[i])));
}
nrowdays <- c(nrowdays, nrow(household));

## Plotting
print("Starting plot............................................................. [plot4.png]");
png("plot4.png");
par(mfrow=c(2,2), mar=c(4,4,2,1),oma=c(0,0,2,0));

## SUBPLOT1
plot.default(household$Global_active_power, type='l', xaxt='n', xlab = "", ylab = "Global Active power (kilowatts)");
axis(1, at=nrowdays, labels=weekday);

## SUBPLOT2
plot.default(household$Voltage, type='l', xaxt='n', xlab = "datetime", ylab = "Voltage");
axis(1, at=nrowdays, labels=weekday);

## SUBPLOT3
plot(household$Sub_metering_1, col="black", ylab="Energy sub metering", xlab="", type='l', xaxt='n');
lines(household$Sub_metering_2, col="red");
lines(household$Sub_metering_3, col="blue");
axis(1, at=nrowdays, labels=weekday);
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), cex=0.7, lty=1, lwd=1, y.intersp = 0.9, text.width=c(1000));

## SUBPLOT4
plot.default(household$Global_reactive_power, type='l', xaxt='n', xlab = "datetime", ylab = "Global_reactive_power");
axis(1, at=nrowdays, labels=weekday);

dev.off();
print(".......................................................................... [Done!]");
}
