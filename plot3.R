## Exploratory Data Analysis Class Project 2
## Question 3
## Of the four types of sources indicated by the type which of these four sources 
## have seen decreases in emissions from 1999–2008 for Baltimore City 
## Which have seen increases in emissions from 1999–2008?
 
## Load packages

library(dplyr)
library(ggplot2)
library(grid)

## Read in the data

NEI <- readRDS("summarySCC_PM25.rds")

## Subset the Baltimore City Data

NEI_Balt <- filter(NEI, fips == "24510")

## Total the Emissions by year into a new table
## and create a new variable TonsEmitted

Years <- summarize(group_by(NEI_Balt,type,year), TonsEmitted = sum(Emissions))

## Open a graphics device

png(file = "plot3.png", width = 480, height = 480, type = c("windows")) 

## Plot total emissions in tons by year by type

ggplot(Years, aes(year, TonsEmitted)) +
      geom_point() +      
      facet_wrap(~ type, ncol = 2, nrow = 2) +
      ggtitle(expression(atop("Emmisions by year - Baltimore City", atop("Broken out by type")))) +
      geom_smooth(method = "lm", se = FALSE) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))  +
      theme(plot.title = element_text(size = 15, vjust = 3))  

## Sign my plot
grid.text("<EAM>", just = "right", x = 1, y = .05, gp=gpar(col="grey"))



dev.off()
