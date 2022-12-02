setwd("/Users/viktort/Desktop/Data Processing Software/")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#You must address the following questions and tasks in your exploratory analysis. 
#For each question/task you will need to make a single **bar** plot. 
#You can use any plotting system in R to make your plot.

#1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
sum_emissions <- aggregate(NEI$Emissions, list(NEI$year), function(x) sum(x))
barplot(avg_emissions$x ~ sum_emissions$Group.1, main="PM2.5 Emissions in US, 1999-2008", xlab="Year", ylab="Emissions")

### Yes, Emissions decreased in US from 1999 to 2008

#2. Have total emissions from PM2.5 decreased in the **Baltimore City**, Maryland (`fips == "24510"`) from 1999 to 2008?
nei_baltimore <- NEI[NEI$fips == "24510",]
sum_emissions <- aggregate(nei_baltimore$Emissions, list(nei_baltimore$year), function(x) sum(x))
barplot(sum_emissions$x ~ sum_emissions$Group.1, main="PM2.5 Emissions in Baltimore City, 1999-2008", xlab="Year", ylab="Emissions")

### Yes, Emissions decreased in Baltimore City from 1999 to 2008

#3. Of the four types of sources indicated by the `type` (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999–2008 for **Baltimore City**? 
#Which have seen increases in emissions from 1999–2008?

sum_emissions <- aggregate(nei_baltimore$Emissions, list(nei_baltimore$year, nei_baltimore$type), function(x) sum(x))
barplot(sum_emissions$x ~ sum_emissions$Group.1 + sum_emissions$Group.2, 
        main="PM2.5 Emissions in Baltimore City, 1999-2008, by source type", xlab="Year", ylab="Emissions",
        legend=c('1999', '2002', '2005', '2008'), beside=TRUE)

### All types, excpect Point decreased.

# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
scc_coal <- NEI[NEI$SCC %in% SCC[grepl("coal", SCC$Short.Name),]$SCC ,]
sum_emissions <- aggregate(scc_coal$Emissions, list(scc_coal$year), function(x) sum(x))
barplot(sum_emissions$x ~ sum_emissions$Group.1, main="Emissions from coal combustion-related sources, 1999-2008", xlab="Year", ylab="Emissions")

### Decreased from 5936.154 to 2116.185

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in **Baltimore City** (EI.Sector starts from "Mobile")?
scc_mot <- NEI[NEI$fips == "24510" & NEI$SCC %in% SCC[grepl("Mobile", SCC$EI.Sector),]$SCC ,]
sum_emissions <- aggregate(scc_mot$Emissions, list(scc_mot$year), function(x) sum(x))
barplot(sum_emissions$x ~ sum_emissions$Group.1, main="Emissions from motor vehicle sources in Baltimore, 1999-2008", xlab="Year", ylab="Emissions")

### Decreased from 869.76 to 518.7066


# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in **Los Angeles County**, 
# California (`fips == "06037"`). Which city has seen greater changes over time in motor vehicle emissions?

scc_mot <- NEI[NEI$fips %in% c("24510", "06037") & NEI$SCC %in% SCC[grepl("Mobile", SCC$EI.Sector),]$SCC ,]
sum_emissions <- aggregate(scc_mot$Emissions, list(scc_mot$year, scc_mot$fips), function(x) sum(x))
barplot(sum_emissions$x ~ sum_emissions$Group.1 + sum_emissions$Group.2, main="Emissions from motor vehicle sources in Baltimore VS LA, 1999-2008"
        , xlab="Year", ylab="Emissions", legend = c('1999', '2002', '2005', '2008'), beside=TRUE)


# In LA emissions from motor vehicle sources decreased 7517.37 to 8740.7552,
# in Baltimore from 869.76 to 518.7066
  
  
  
  