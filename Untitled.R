1. library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Factorize SCC and year (not necessary for plotting here but I like doing it.)
NEI <- transform(NEI, SCC = factor(SCC))
NEI <- transform(NEI, year = factor(year))
#Summing emissions by SCC and year
totals <- aggregate(Emissions ~ year,NEI, sum)

barplot(totals$Emissions, xlab="Year", ylab="PM2.5 Emissions", main = "PM2.5 Emission Totals", names.arg = totals$year)

dev.copy(png, file="Plot1.png", height=480, width=480) 
dev.off()




NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

balt <- NEI[which(NEI$fips == 24510),]
tbal <- aggregate(Emissions ~ year,balt, sum)

barplot(tbal$Emissions, xlab="Year", ylab="PM2.5 Emissions", main = "Baltimore PM2.5 Emission Totals", names.arg = tbal$year)

dev.copy(png, file="Plot2.png", height=480, width=480) 
dev.off()




NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(ggplot2)
baltimore <- subset(NEI, NEI$fips == "24510")
baltimoreType <- aggregate(Emissions ~ year + type, baltimore, sum)

ggplot(baltimoreType, aes(year, Emissions, col = type)) +
  geom_line() +
  geom_point() +
  ggtitle(expression("Total Baltimore " ~ PM[2.5] ~ "Emissions by Type and Year")) +
  ylab(expression("Total Baltimore " ~ PM[2.5] ~ "Emissions")) +
  xlab("Year") +
  scale_colour_discrete(name = "Type of sources") +
  theme(legend.title = element_text(face = "bold"))


dev.copy(png, file="Plot3.png", height=480, width=480) 
dev.off()



NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(ggplot2)


coal_SCC <- SCC[grep("*coal*|*Coal*",SCC$Short.Name),]$SCC
coal_data <- NEI[NEI$SCC %in% coal_SCC,]

suppressWarnings(agg_coal <- aggregate(coal_data, list(coal_data$year), mean))

g2 <- ggplot(agg_coal, aes(x=Group.1, y=Emissions, group =1)) + geom_point(aes(colour = factor(Group.1)), size = 4)+geom_line()
g2

dev.copy(png, file="Plot4.png", height=480, width=480) 
dev.off()



NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(ggplot2)

vehicle_SCC <- SCC[grep("*vehicle*|*Vehicle*",SCC$Short.Name),]$SCC
motor_balt <- balt[balt$SCC %in% vehicle_SCC,]
agg_motor_balt <- aggregate(motor_balt$Emissions, list(motor_balt$year),mean)

g3 <- ggplot(agg_motor_balt, aes(x=Group.1, y=x, group =1)) + geom_point(aes(colour = factor(Group.1)), size = 4)+geom_line()
g3

dev.copy(png, file="Plot5.png", height=480, width=480) 
dev.off()



NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(ggplot2)

LA <- NEI[which(NEI$fips == "06037"),]
motor_LA <- LA[LA$SCC %in% vehicle_SCC,]
agg_motor_LA <- aggregate(motor_LA$Emissions, list(motor_LA$year),mean)
agg_motor_LA$city <- rep("LA",4)
agg_motor_balt$city <- rep("Baltimore",4)

two_cities <- rbind.data.frame(agg_motor_balt,agg_motor_LA)

g4 <- ggplot(two_cities, aes(x=factor(Group.1), y=x, group =city)) + geom_point(aes(colour = city), size = 4)+geom_line(aes(colour=city))
g4
dev.copy(png, file="Plot6.png", height=480, width=480) 
dev.off()


