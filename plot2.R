## Exploratory Data Analysis Class Project 2
## Question 2
## Have total emissions from PM2.5 decreased in Baltimore City,
## Maryland (fips == "24510") from 1999 to 2008? 
 
## Load packages

library(dplyr)
library(grid)

## Read in the data

NEI <- readRDS("summarySCC_PM25.rds")

## Subset the Baltimore City Data

NEI_Balt <- filter(NEI, fips == "24510")

## Total the Emissions by year into a new table
## and create a new variable TonsEmitted

Years <- summarize(group_by(NEI_Balt,year), TonsEmitted = sum(Emissions))

## Open a grphics device

png(file = "plot2.png", width = 480, height = 480, type = c("windows")) 


## Plot total emissions in tons by year

plot(Years$year, Years$TonsEmitted, type="l", cex.axis=0.75, col = "blue",
     xlab = "Years", ylab= "Emissions (tons)" , 
     main = "Total PM 2.5 Emissions - Baltimore City" )

## Sign my plot
mtext("<EAM>", side=1, line=4, font=4, adj=1, cex=.75, col="gray")  


dev.off()
