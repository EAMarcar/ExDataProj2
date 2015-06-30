## Exploratory Data Analysis Class Project 2
## Question 5
## How have emissions from motor vehicle sources
## changed from 1999 to 2008 in Baltimore City?

## Note:  I'm using total emissions here.  
## My assumption is we are looking at overall air quality for which totals matter
## I compared means to be sure I wasn't missing something and saw no real difference

## Activate some packages

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

## Read in the data

NEI <- readRDS("summarySCC_PM25.rds")

## Subset the Baltimore City Data

NEI_Balt_mv <- filter(NEI, fips == "24510" & type == "ON-ROAD")

## ON-ROAD contains all mobile on road sources as per NEI documentations

## Break out types of motor vehicles based on the fact that SCC
## values that start with "202" are gasoline vehicles and "230" are Deisel;
## the data set contains only these two types.

NEI_Balt_mv$fuel <- "Unknown"

NEI_Balt_mv$fuel[grepl("220",NEI_Balt_mv$SCC)] <- "Gasoline"
NEI_Balt_mv$fuel[grepl("230",NEI_Balt_mv$SCC)] <- "Deisel"
NEI_Balt_mv$fromGasoline  <- 0
NEI_Balt_mv$fromDeisel  <- 0

obs_tot <- nrow(NEI_Balt_mv)

## Create variables to compare Deisel emissions and Gasoline emissions 
## (had trouble getting mutate to do this; used alternative method)

for (i in 1:obs_tot) {
      
      if ( NEI_Balt_mv$fuel[i] == "Gasoline") 
                  { NEI_Balt_mv$fromGasoline[i] <- NEI_Balt_mv$Emissions[i] }
     
      if ( NEI_Balt_mv$fuel[i] == "Deisel") 
                  { NEI_Balt_mv$fromDeisel[i] <- NEI_Balt_mv$Emissions[i] } 
      }


## Total the Emissions by year into a new table
## and create new variables: TonsEmitted, DeiselTons and GasolineTons

Years_mv <- summarize(group_by(NEI_Balt_mv,year), TonsEmitted = sum(Emissions), 
            DeiselTons = sum(fromDeisel),GasolineTons = sum(fromGasoline))

## Create variables that show emissions as a percent change
## with 1999 as the index  ("YrYr" will be the year over year percentage change)

Years_mv <- arrange(Years_mv, desc(year))   ## reorder first to get the right differences
Years_mv <- mutate (Years_mv, delta = c(diff(Years_mv$TonsEmitted), 0))
Years_mv <- mutate (Years_mv, prevYr = TonsEmitted + delta, YrYr = (delta/prevYr) * -100)


## Open a graphics device

png(file = "plot5.png", width = 1000, height = 480, type = c("windows")) 


## Plot Total emissions in tons by year by type

p1 <- ggplot(Years_mv, aes(year)) +
      geom_point(aes(y = TonsEmitted, col = "Total")) +
     geom_point(aes(y = GasolineTons, col = "Gasoline Sources")) +
      geom_point(aes(y = DeiselTons, col = "Deisel Sources")) +
      geom_smooth(aes(y= TonsEmitted), method = "loess", se = FALSE) +
      geom_smooth(aes(y= GasolineTons), method = "loess", se = FALSE, col ="green") +
      geom_smooth(aes(y= DeiselTons), method = "loess", se = FALSE, col ="orange") +
      ggtitle(expression(atop("PM2.5 Emissions by year - Baltimore City \n Source = Onroad(Motor Vehicle)"))) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))  +
      theme(plot.title = element_text(size = 12, vjust = 2)) +
      theme(legend.title = element_text(colour="black", size=14, face="bold"))+
      labs(x= "Year", y = "Emissions (tons)") +
                  scale_color_discrete(name="Fuel Type")

## Plot percent change year over year

p2 <- ggplot(Years_mv, aes(as.character(year))) +
      geom_bar(aes(y = YrYr), stat="identity", col="blue", fill="blue", width= .5) +
      ggtitle(expression(atop("PM2.5 Emissions by year - Baltimore City \n Source = Onroad(Motor Vehicle) \n Year over Year Decrease (%, Baseline =1999)"))) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))  +
      theme(plot.title = element_text(size = 12, vjust = 1)) +
      labs(x= "Year", y = "Percent")

## Format the plot grid  

grid.arrange(p1, p2, ncol=2)

## Sign my plot  
grid.text("<EAM>", just = "right", x = 1, y = .05, gp=gpar(col="grey"))


dev.off()
