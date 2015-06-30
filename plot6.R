## Exploratory Data Analysis Class Project 2
## Question 6
## Compare emissions from motor vehicle sources in Baltimore City(fips == "24510") with emissions 
## from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?



## Note:  I'm using total emissions here.  
## My assumption is we are looking at overall air quality for which totals matter

## Read in the data

NEI <- readRDS("summarySCC_PM25.rds")

## Activate some packages

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

## Subset the Baltimore City and Los Angeles Data

NEI_B_LA_mv <- filter(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD")

## ON-ROAD contains all mobile on-road sources as per NEI documentation

## Create variables for each city's emissions

NEI_B_LA_mv$city <- "Unknown"

NEI_B_LA_mv$city[ NEI_B_LA_mv$fips == "24510"] <- "Baltimore"
NEI_B_LA_mv$city[ NEI_B_LA_mv$fips == "06037"] <- "Los Angeles"

NEI_B_LA_mv$BaltEmissions  <- 0
NEI_B_LA_mv$LAEmissions  <- 0

obs_tot <- nrow(NEI_B_LA_mv)

## Create variables for emissions by city

for (i in 1:obs_tot) {
      
      if ( NEI_B_LA_mv$city[i] == "Baltimore") 
                  { NEI_B_LA_mv$BaltEmissions[i] <- NEI_B_LA_mv$Emissions[i] }
     
      if ( NEI_B_LA_mv$city[i] == "Los Angeles") 
                  { NEI_B_LA_mv$LAEmissions[i] <- NEI_B_LA_mv$Emissions[i] } 
      }


## Total the Emissions by year into a new table
## and create new variable; TonsEmitted, LATons and BaltTons

Balt_LA_mv <- summarize(group_by(NEI_B_LA_mv,year), TonsEmitted = sum(Emissions), 
            LATons = sum(LAEmissions),BaltTons = sum(BaltEmissions))

## Create variables that show emmsions as a percent change, year over year
## with 1999 as the starting point. YrYr -T, -B, -L,  will be plotted.

## Formula: Change from the previous year (delta) / Previous Year Emissions (prevYr) * -100 to 
## correct the sign of the increase or decrease and put in percentage form.)

Balt_LA_mv <- arrange(Balt_LA_mv, desc(year))   ## reorder first to get the right differences
Balt_LA_mv <- mutate (Balt_LA_mv, deltaT = c(diff(Balt_LA_mv$TonsEmitted), 0))
Balt_LA_mv <- mutate (Balt_LA_mv, prevYrT = TonsEmitted + deltaT, YrYrT = (deltaT/prevYrT) * -100)
Balt_LA_mv <- mutate (Balt_LA_mv, deltaB = c(diff(Balt_LA_mv$BaltTons), 0))
Balt_LA_mv <- mutate (Balt_LA_mv, prevYrB = BaltTons + deltaB, YrYrB = (deltaB/prevYrB) * -100)
Balt_LA_mv <- mutate (Balt_LA_mv, deltaL = c(diff(Balt_LA_mv$LATons), 0))
Balt_LA_mv <- mutate (Balt_LA_mv, prevYrL = LATons + deltaL, YrYrL = (deltaL/prevYrL) * -100)


## Alternative comparision - cumulative percent drop from basline year (1999)
## Create variables - PrcntChng -T, -B, -L  will be plotted.  

Idx <- nrow(Balt_LA_mv)  ## used to get baseline emissions from 1999

Balt_LA_mv$baseTonsTotal = Balt_LA_mv$TonsEmitted[Idx]
Balt_LA_mv$baseTonsBalt = Balt_LA_mv$BaltTons[Idx]
Balt_LA_mv$baseTonsLA = Balt_LA_mv$LATons[Idx]

## Formula: ((Emssions(given year) / Baseline emissions) -1) * 100 
## subtract 1 to creatte a zero baseline for 1999; mutiply by 100 to put in percent form

Balt_LA_mv <- mutate (Balt_LA_mv, PcntChngT = ((TonsEmitted / baseTonsTotal) - 1) * 100)
Balt_LA_mv <- mutate (Balt_LA_mv, PcntChngB = ((BaltTons / baseTonsBalt) - 1) * 100)
Balt_LA_mv <- mutate (Balt_LA_mv, PcntChngL = ((LATons / baseTonsLA) - 1) * 100)

## Open a graphics device

png(file = "plot6.png", width = 800, height = 600, type = c("windows")) 


## Plot total emissions in tons by year by type (ON-ROAD, that is, motor vehicles).

p1 <- ggplot(Balt_LA_mv, aes(year)) +
      geom_point(aes(y = BaltTons, col = "Baltimore")) +
      geom_point(aes(y = LATons, col = "Los Angeles")) +
      geom_smooth(aes(y= BaltTons), method = "loess", se = FALSE, col ="orangered1") +
      geom_smooth(aes(y= LATons), method = "loess", se = FALSE, col ="turquoise4") +
      ggtitle(expression(atop("PM2.5 Emissions by year - Baltimore City \n Source = Onroad(Motor Vehicle)"))) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))  +
      theme(plot.title = element_text(size = 10, vjust = 2)) +
      theme(legend.title = element_text(colour="black", size=14, face="bold"))+
      labs(x= "Year", y = "Emissions (tons)") +
                  scale_color_discrete(name="City")

## Plot cumulative percent change since 1999 for each city

p2 <- ggplot(Balt_LA_mv, aes(year)) +
      geom_point(aes(y = PcntChngB, col = "Baltimore")) +
      geom_point(aes(y = PcntChngL, col = "Los Angeles")) +
      geom_smooth(aes(y= PcntChngB), method = "loess", se = FALSE, col ="orangered1") +
      geom_smooth(aes(y= PcntChngL), method = "loess", se = FALSE, col ="turquoise4") +
      ggtitle(expression(atop("PM2.5 Emissions by year - Baltimore City \n Source = Onroad(Motor Vehicle) \n Cumualtive Change (%, Baseline =1999)"))) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))  +
      theme(plot.title = element_text(size = 10, vjust = 0)) +
      theme(legend.title = element_text(colour="black", size=14, face="bold")) +
      labs(x= "Year", y = "Percent Change") +
      scale_color_discrete(name="City")

## Plot Yer over year change in percentage terms for each city



p3 <- ggplot(Balt_LA_mv, aes(year, ymax=20)) +
      geom_point(aes(y = YrYrL, col = "Los Angeles"), shape = 0, size = 6) +
      geom_point(aes(y = YrYrB, col = "Baltimore"), shape = 19, size =4) +
      geom_hline(yintercept = 0) +
      ggtitle(expression(atop("PM2.5 Emissions by year - Baltimore City \n Source = Onroad(Motor Vehicle) \n Year over Year Change (%)"))) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))  +
      theme(plot.title = element_text(size = 10, vjust = 0)) +
      theme(legend.title = element_text(colour="black", size=14, face="bold")) +
      labs(x= "Year", y = "Percent Change") +
      scale_color_discrete(name="City")

## Format and annotate the plot grid  

grid.arrange(p1, p2, p3, ncol=2, nrow=2)

grid.text("PM2.5 Emisisons Comparison", just = "right", x = .85, y=.40,
            gp=gpar(fontsize = 12))
grid.text("Los Angeles and Baltimore City", just = "right", x = .85, y=.36,
            gp=gpar(fontsize = 12))
## Sign my plot  
grid.text("<EAM> ", just = "right", x = 1, y = .05, gp=gpar(col="grey"))


dev.off()
