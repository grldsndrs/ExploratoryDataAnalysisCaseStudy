# Introduction
#
# Fine particulate matter (PM2.5) is an ambient air pollutant for which there is
# strong evidence that it is harmful to human health. In the United States, the
# Environmental Protection Agency (EPA) is tasked with setting national ambient
# air quality standards for fine PM and for tracking the emissions of this
# pollutant into the atmosphere. Approximatly every 3 years, the EPA releases
# its database on emissions of PM2.5. This database is known as the National
# Emissions Inventory (NEI). You can read more information about the NEI at the
# EPA National Emissions Inventory web site.
#
# For each year and for each type of PM source, the NEI records how many tons of
# PM2.5 were emitted from that source over the course of the entire year. The
# data that you will use for this assignment are for 1999, 2002, 2005, and 2008.
#
# Data
#
# The data for this assignment are available from the course web site as a
# single zip file:
#
# Data for Peer Assessment [29Mb] The zip file contains two files:
#
# PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame
# with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each
# year, the table contains number of tons of PM2.5 emitted from a specific type
# of source for the entire year. Here are the first few rows.
#
#
##     fips      SCC Pollutant Emissions  type year
## 4  09001 10100401 PM25-PRI    15.714 POINT 1999
## 8  09001 10100404  PM25-PRI   234.178 POINT 1999
## 12 09001 10100501  PM25-PRI     0.128 POINT 1999
## 16 09001 10200401  PM25-PRI     2.036 POINT 1999
## 20 09001 10200504  PM25-PRI     0.388 POINT 1999
## 24 09001 10200602  PM25-PRI     1.490 POINT 1999

# fips: A five-digit number (represented as a string) indicating the U.S. county
#
# SCC: The name of the source as indicated by a digit string (see source code
# classification table)
#
# Pollutant: A string indicating the pollutant
#
# Emissions: Amount of PM2.5 emitted, in tons
#
# type: The type of source (point, non-point, on-road, or non-road)
#
# year: The year of emissions recorded
#
# Source Classification Code Table (Source_Classification_Code.rds): This table
# provides a mapping from the SCC digit strings in the Emissions table to the
# actual name of the PM2.5 source. The sources are categorized in a few
# different ways from more general to more specific and you may choose to
# explore whatever categories you think are most useful. For example, source
# “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized
# Coal”.
#
# You can read each of the two files using the readRDS() function in R. For
# example, reading in each file can be done with the following code:
#
library(dplyr)
library(RColorBrewer)
library(scales)
library(outliers)
library(ggplot2)
# ## This first line will likely take a few seconds. Be patient!
NEI <-readRDS("summarySCC_PM25.rds")
SCC <-readRDS("Source_Classification_Code.rds")
# as long as each of those files is in
# your current working directory (check by calling dir() and see if those files
# are in the listing).
#

# Assignment
#
# The overall goal of this assignment is to explore the National Emissions
# Inventory database and see what it say about fine particulate matter pollution
# in the United states over the 10-year period 1999–2008. You may use any R
# package you want to support your analysis.
#
# Questions
#
# You must address the following questions and tasks in your exploratory
# analysis. For each question/task you will need to make a single plot. Unless
# specified, you can use any plotting system in R to make your plot.


# 3.Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable,

# Which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008?

# Use the ggplot2 plotting system to make a plot answer this question.


#create table with fips, SCC, type and Pollutants are factors

NEI[,1]<-as.factor(NEI[,1])
NEI[,2]<-as.factor(NEI[,2])
NEI[,3]<-as.factor(NEI[,3])
NEI[,5]<-as.factor(NEI[,5])
NEI[,6]<-as.factor(NEI[,6])

# filter the data for Baltimore fips
# Get number of points in each year

gyear<-filter(.data = NEI,fips=="24510")%>%group_by(Pollutant,year)
gyearSm<-summarise(gyear,countPerYr=n(),TotalEmissions=sum(Emissions,na.rm = TRUE))
gyear<-merge(gyear,gyearSm,by = intersect(names(gyearSm), names(gyear)))

# Get smaller set of data to work with by building a random
# sample of numbers between year boundries
runningSum0<-0;
# runningSum1<-1;
set.seed<-1
pct <- 1
listSubSetIndices<-floor(unlist(
  sapply(gyearSm$countPerYr, function(cnt) {
    listOfInd<-runif(floor(pct*cnt),runningSum0+1,cnt+runningSum0)
    runningSum0<<-runningSum0+cnt
    # runningSum1<<-runningSum1+cnt
    listOfInd
  },simplify = TRUE))
)
plot(listSubSetIndices)
#t<-listSubSetIndices
# gyear[listSubSetIndices,]

# # Remove outliers
outlier(gyear$Emissions)
gyear$Emissions=rm.outlier(gyear$Emissions,fill =TRUE)
# mrro1=rm.outlier(gyear$Emissions,fill =TRUE)
# outlier(mrro1)
# mrro1=rm.outlier(mrro1,fill =TRUE)
# outlier(mrro1)
# mrro1=rm.outlier(mrro1,fill =TRUE)
# outlier(mrro1)
# mrro1=rm.outlier(mrro1,fill =TRUE)
# outlier(mrro1)
# gyear$Emissions=rm.outlier(mrro1,fill =TRUE)

mr<-gyear[listSubSetIndices,]

# Count the number of distint classiifcations for the PM-25 pollutant for each type in each fips in each year
# group so that type is at the extreme of the group
gall<-group_by(mr,Pollutant,year,fips,type)
# get a count of the distinct Classes per type
sm<-summarise(gall,count=n_distinct(SCC))
# merge counts back to original data set
mr<-merge(gall,sm,by = intersect(names(sm), names(gall)))

#pallette for the line and fill of the symbols
numberOfColsForYrs <- length(levels(mr$year))
emisPalInt<- colorRampPalette(topo.colors(numberOfColsForYrs))
yearPalInt<- colorRampPalette(topo.colors(numberOfColsForYrs))
palSymLnColsInt<- (yearPalInt(numberOfColsForYrs))
palSymFillColsInt<- (emisPalInt(numberOfColsForYrs))


mr$LnColor <- factor(mr$year, levels=levels(mr$year), labels=palSymLnColsInt)
# Warning is becuase there are not enough levels for each indidual data point
mr$BgColor <- factor(mr$year, levels=levels(mr$year), labels=palSymFillColsInt)
mr$Symbol <- factor(mr$type, levels=levels(mr$type), labels=c(21,22,23,24))

# group by year to get mean by year
gtype<-group_by(mr,Pollutant,year,type)
# get a count of the distinct Classes per type
sm<-summarise(gtype,mean=mean(Emissions,na.rm = TRUE),med=median(Emissions,na.rm = TRUE) )
# merge counts back to original data set
mr<-merge(mr,sm,by = intersect(names(sm), names(mr)))

# Scale function used to scale data for graph aestetics
scale01 <- function(v){
  if (max(v,na.rm = TRUE)-min(v,na.rm = TRUE)==0) {
    addt = .0000000001
  }
  else{
    addt= 0
  }
  (v-min(v,na.rm = TRUE))/(max(v,na.rm = TRUE)+addt-min(v,na.rm = TRUE))
}

#mr<-filter(mr,year=="2008")%>%group_by(Pollutant,year)
mr<-group_by(mr,Pollutant,year)


# Making and Submitting Plots
   TotalEmissions<-(scale01(mr$TotalEmissions))
   PercentEmissions<-(scale01(mr$Emissions/mr$TotalEmissions))
   DistinctSourceClassCount<-mr$count
   yr<-as.numeric(levels(mr$year)[mr$year])
#
# For each plot you should
#
# Construct the plot and save it to a PNG file.
# Save it to a PNG file
# Name each of the plot files as plot1.png, plot2.png, etc.
   par(mar=c(5,5,5,4)+.1)

 png(filename = paste0(getwd(),"/plot3.png"), width = 480, height = 480)
 # with a width of 480 pixels and a height of 480 pixels.

   p<-ggplot(data=mr, aes(x=year, y=Emissions, shape=type,colour=year)) +
    scale_colour_manual(name="Year",values=palSymFillColsInt)  +
     scale_fill_gradientn(colours = palSymFillColsInt,guide = FALSE) +
     geom_point(aes(size=TotalEmissions,
                    colour=year,
                    fill=yr,
                    alpha=DistinctSourceClassCount)) +
     scale_alpha(name = "opacity ~ #Distinct Source Classes") +
     theme(legend.position="top") +
     scale_size(name="Total\nEmissions", range=c(5,15)) +
     scale_shape_manual(name="Type",values=c(21,22,23,24)) +
     theme(legend.background = element_rect()) +
     theme(panel.background=element_rect(fill = "white")) +
     scale_x_discrete(name="Baltimaore") +
     scale_y_continuous(name="Emmisions")
print(p)
dev.off()


# Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the
# corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. Your
# code file should include code for reading the data so that the plot can be
# fully reproduced. You must also include the code that creates the PNG file.
# Only include the code for a single plot (i.e. plot1.R should only include code
# for producing plot1.png)
#
# Upload the PNG file on the Assignment submission page
#
# Copy and paste the R code from the corresponding R file into the text box at
# the appropriate point in the peer assessment.
