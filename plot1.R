## Exploratory Data Analysis Class Project 2
## Question 1
## Have total emissions from PM2.5 decreased in the United States
## from 1999 to 2008?
## 

## Load packages

library(dplyr)
library(grid)

## Read in the data

NEI <- readRDS("summarySCC_PM25.rds")

## Total the Emissions by year into a new table
## and create a new variable TonsEmitted

Years <- summarize(group_by(NEI,year), TonsEmitted = sum(Emissions / 1000))

## Open a grphics device

png(file = "plot1.png", width = 480, height = 480, type = c("windows")) 


## Plot total emissions in tons by year

plot(Years$year, Years$TonsEmitted, type="l", cex.axis=0.75, col = "blue",
     xlab = "Years", ylab= "Emissions ('000 of tons)" , 
     main = "Total PM 2.5 Emissions - United States" )

## Sign my plot
mtext("<EAM>", side=1, line=4, font=4, adj=1, cex=.75, col="gray")  


dev.off()
