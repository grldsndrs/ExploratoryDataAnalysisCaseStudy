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
library(gplots)
# Scale function used to scale data for graph aestetics
scale01 <- function(v) {
  if (max(v,na.rm = TRUE) - min(v,na.rm = TRUE) == 0) {
    addt = .0000000001
  }
  else{
    addt = 0
  }
  (v - min(v,na.rm = TRUE)) / (max(v,na.rm = TRUE) + addt - min(v,na.rm = TRUE))
}

# Vectorized function to compute a running difference
runningDiff <- Vectorize(function(x, y) abs((y-x)/x), SIMPLIFY = TRUE)

# ## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
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



# 6.Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").

#Which city has seen greater changes over time in motor vehicle emissions?


#create table with fips, SCC, type and Pollutants are factors

NEI[,1] <- as.factor(NEI[,1])
NEI[,2] <- as.factor(NEI[,2])
NEI[,3] <- as.factor(NEI[,3])
NEI[,5] <- as.factor(NEI[,5])
NEI[,6] <- as.factor(NEI[,6])

# filter the data for Baltimore fips
# Get number of points in each year
# filter the data for coal combustion related sources

fSCC <-
  filter(.data = SCC,grepl(".*\\b[Mm]obile\\b.*",SCC$EI.Sector))
gyear <-
  NEI[NEI$SCC %in% fSCC$SCC,] %>% filter(fips == "24510" |
                                           fips == "06037") %>% group_by(Pollutant,year)
gyearSm <-
  summarise(gyear,countPerYr = n(),Total.Emissions.Yr = sum(Emissions,na.rm = TRUE))
gyear <-
  merge(gyear,gyearSm,by = intersect(names(gyearSm), names(gyear)))

gfips<-group_by(gyear,Pollutant,year,fips)%>%
  summarise(Total.Emissions.yr.fips = sum(Emissions,na.rm = TRUE))

gyear <-
  merge(gyear,gfips,by = intersect(names(gfips), names(gyear)))

# Get smaller set of data to work with by building a random
# sample of numbers between year boundries
runningSum0 <- 0;
# runningSum1<-1;
set.seed <- 1
pct <- 1
listSubSetIndices <- floor(unlist(sapply(gyearSm$countPerYr, function(cnt) {
  listOfInd <- runif(floor(pct * cnt),runningSum0 + 1,cnt + runningSum0)
  runningSum0 <<- runningSum0 + cnt
  # runningSum1<<-runningSum1+cnt
  listOfInd
},simplify = TRUE)))
plot(listSubSetIndices)
#t<-listSubSetIndices
# gyear[listSubSetIndices,]

# # Remove outliers
# outlier(gyear$Emissions)
# gyear$Emissions = rm.outlier(gyear$Emissions,fill = TRUE)
# mrro1=rm.outlier(gyear$Emissions,fill =TRUE)
# outlier(mrro1)
# mrro1=rm.outlier(mrro1,fill =TRUE)
# outlier(mrro1)
# mrro1=rm.outlier(mrro1,fill =TRUE)
# outlier(mrro1)
# mrro1=rm.outlier(mrro1,fill =TRUE)
# outlier(mrro1)
# gyear$Emissions=rm.outlier(mrro1,fill =TRUE)

mr <- gyear[listSubSetIndices,]

# Count the number of distint classiifcations for the PM-25 pollutant for each type in each fips in each year
# group so that type is at the extreme of the group
gall <- group_by(mr,Pollutant,year,fips,type)
# get a count of the distinct Classes per type
sm <- summarise(gall,count = n_distinct(SCC))
# merge counts back to original data set
mr <- merge(gall,sm,by = intersect(names(sm), names(gall)))

mr <- mutate(
  mr,
  Percent.Emissions.fips = (Emissions / Total.Emissions.yr.fips),
  Distinct.Source.Class.Count = (count),
  newFipsFactors = factor(as.character(levels(fips)[fips]))
)
# cast variables
yearLvls <- levels(mr$year)
typeLvls <- levels(mr$type)
fipsLvls <- levels(mr$newFipsFactors)


#pallette for the line and fill of the symbols
numberOfColsForYrs <- length(yearLvls)
numberOfColsForFips <- length(fipsLvls)

emisPalInt <- colorRampPalette(topo.colors(numberOfColsForYrs))
yearPalInt <- colorRampPalette(topo.colors(numberOfColsForYrs))
fipsPalInt <- colorRampPalette(terrain.colors(numberOfColsForFips))

palSymLnColsInt <- (yearPalInt(numberOfColsForYrs))
palSymFillColsInt <- rev(col2hex(c("red","chocolate")))#brewer.pal(3,"Pastel1")[1:2]
palSymbols <- (c(24,22,25,21))

palSymFillColsInt2<-(c(alpha(palSymFillColsInt[1],.001),alpha(palSymFillColsInt[2],1)))

names(palSymLnColsInt) <- yearLvls
names(palSymFillColsInt) <- fipsLvls
names(palSymbols) <- typeLvls

mr$LnColor <-
  factor(mr$year, levels = yearLvls, labels = palSymLnColsInt)
# Warning is becuase there are not enough levels for each indidual data point
mr$BgColor <-
  factor(mr$newFipsFactors, levels = fipsLvls, labels = palSymFillColsInt)#%>%alpha( .01)
mr$Symbol <- factor(mr$type, levels = typeLvls, labels = palSymbols)

# calculate emission deltas by year
statDf<-distinct(select(mr,Total.Emissions.yr.fips,year))[with( distinct(select(mr,Total.Emissions.yr.fips,year)),order(year)),]

totalEmisYr<-statDf["Total.Emissions.yr.fips"]
statDf["Percent.Yearly.Emissions.Change"]<-
  c(0,
    runningDiff(
      totalEmisYr[1:nrow(totalEmisYr)-1,],
      totalEmisYr[2:nrow(totalEmisYr),]
      )
    )

mr <- merge(mr,statDf,by = intersect(names(statDf), names(mr)))

# group by year to get mean by year
gtype <- group_by(mr,Pollutant,year,type)
# get a count of the distinct Classes per type
sm <-
  summarise(gtype,
            mean = mean(Emissions,na.rm = TRUE),
            med = median(Emissions,na.rm = TRUE),
            Total.Emissions.yr.type = sum(Emissions,na.rm = TRUE)
            )
# merge counts back to original data set
mr <- merge(mr,sm,by = intersect(names(sm), names(mr)))

mr <- mutate(
  mr,
  Percent.Emissions.type.fips = (Total.Emissions.yr.type / Total.Emissions.yr.fips)
)

mr <- group_by(mr,Pollutant,year)# Making and Submitting Plots


# For each plot you should
#
# Construct the plot and save it to a PNG file.
# Save it to a PNG file
# Name each of the plot files as plot1.png, plot2.png, etc.
#par(mar=c(2,2,5,4)+.1)

png(
  filename = paste0(getwd(),"/plot6.png"), width = 480, height = 480
)
# with a width of 480 pixels and a height of 480 pixels.

p <-
  ggplot(
    data = mr, aes(x = year,
                   y = Emissions,
                   shape = type,
                   colour = year,
                   fill = newFipsFactors,
                   size = Percent.Yearly.Emissions.Change,
                    #alpha = .1,#Percent.Emissions.type.fips,
                   group = fips,
                   order = Total.Emissions.yr.fips
                   )
    ) +
  geom_point(
    data = mr, aes(x = year,
                   y = Emissions)
    ) +
  scale_shape_manual(
    name = "Type",
    breaks = c(typeLvls[4],typeLvls[2],typeLvls[3],typeLvls[1]),
    values = palSymbols,
    guide = guide_legend(
      title = "Type",
      title.position = "left",
      keywidth = 0,
      label.theme = element_text(angle=0,colour = "black"),
      override.aes = list(
        size=5,
        alpha=.0001
      )
    )
  ) +
# To get a legend guide, specify guide = "legend" and breaks
  scale_colour_manual(
    name = "Year",
    breaks = yearLvls,
    values = palSymLnColsInt,
    # alpha=mr$Distinct.Source.Class.Count,
    guide = guide_legend(
      title = "Year",
      title.position = "left",
      keywidth = 0,
      label.theme = element_text(angle=0,colour = "black"),
      override.aes = list(
        shape = 18,
        size=5

      )
    )  )   +
  # Fill aestitics
  scale_fill_manual(
    name = "City",
    labels= c("Los Angles","Baltimore"),
    breaks = fipsLvls,
    values = palSymFillColsInt2,
    guide = guide_legend(
      title = "City",
      title.position = "left",
      keywidth = 0,
      label.theme = element_text(angle=0,colour = "black"),
      override.aes = list(
        colour= palSymFillColsInt,
        shape = 23,
        size=3
        )
      )
  ) +
  scale_size(
    name = "size ~ % Difference\n of Total Emissions\n from one year to the next",
    range = c(4,32),
    guide = "legend"
    ) +
#   scale_alpha(
#     name = "Opacity ~ % of Emissions by Type for year and fips",
#     range = (c(.01,1)),
#     guide = guide_legend(
#       title = "Opacity ~ % of Emissions by Type for year and fips",
#       title.position = "top",
#       title.hjust = 1,
#       keywidth = 0,
#       label.theme = element_text(angle=0,colour = "black"),
#       override.aes = list(
#         shape = 18,
#         size=5
#         )
#       )
#     ) +
  scale_x_discrete(
    name = "Baltimaore ~ Los Ageles motor vehicle sources"
    ) +
  scale_y_continuous(
    name = "Emmisions"
    ) +
  theme(
    legend.position = "top",
    legend.key = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    #legend.text = element_text( face = "bold"),
    panel.background = element_rect(fill = "white")
  )
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
