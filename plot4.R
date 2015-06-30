## Exploratory Data Analysis Class Project 2
## Question 4
## 
## Across the United States, how have emissions from
## coal combustion-related sources changed from 1999–2008?
##
## NOTE: For this purpose any source with the word "Coal" (upper or lower case) in
## the Short Name field of the Source Classification Code (SCC) file
## is considered a "coal combustion-related source"
## This yielded 53,411 rows of data.

## load packages

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

## Read in the data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Find the SSC codes for "coal combustion-related sources"

SCC_coal <- filter(SCC, grepl("Coal", Short.Name, ignore.case = TRUE))

## Subset data for coal emissions

SCC_coal <- mutate(SCC_coal, source = as.character(SCC))


NEI_coal <- filter(NEI, SCC %in% SCC_coal$source ) 

## Total the Emissions by year into a new table
## and create a new variable TonsEmitted

Years <- summarize(group_by(NEI_coal,year), TonsEmitted = sum(Emissions / 1000))

## Open a grphics device

png(file = "plot4.png", width = 480, height = 480, type = c("windows")) 


## Plot Total emissions in tons by year

##plot(Years$year, Years$TonsEmitted, type="b", cex.axis=0.75, col="blue",
##     xlab = "Years", ylab= "Emissions ('000 of tons)" , 
##     main = "PM 2.5 Emissions - United Stes \n Coal Combustion-related")

ggplot(Years, aes(year)) +
      geom_point(aes(y = TonsEmitted)) +
      geom_smooth(aes(y= TonsEmitted), method = "loess", se = FALSE) +
      ggtitle(expression(atop("PM2.5 Emissions - Coal Combustion Related, United States "))) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))  +
      theme(plot.title = element_text(size = 12, vjust = 2)) +
      labs(x= "Year", y = "Emissions ('000 of tons)") 
     
## Sign my plot  
grid.text("<EAM>", just = "right", x = 1, y = .05, gp=gpar(col="grey"))



dev.off()
