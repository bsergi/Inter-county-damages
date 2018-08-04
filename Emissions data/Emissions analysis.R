# NEI Emissions Analysis
# Created: January 5, 2018
# Author: Brian Sergi

## Notes ####

# Need to laod data from Emissions breakdown V3.R
load("NEI emissions data.RData")

## Libraries ####

library(readstata13)
library(reshape2)
library(plyr)
library(data.table)
library(openxlsx)
library(tidyr)
library(ggplot2)

library(maps)
library(mapproj)
library(ggplot2)
library(gridExtra)


## Totals ####

## Raw totals
rawTotal <- function(df, pollutant){
  return(sum(df[df$pollutant_cd == pollutant, "total_emissions"]) ) 
}

rawTotals <- data.frame(year = rep(c(2008, 2011, 2014), each=2), stack=rep(c('area', 'point'),3))

for (pollutant in c("NOX", "NH3", "SO2", "PM25-PRI", "VOC")){
  rawTotals[rawTotals$stack == "area", pollutant] <- ldply(area.sources, rawTotal, pollutant=pollutant) 
  rawTotals[rawTotals$stack == "point", pollutant] <- ldply(point, rawTotal, pollutant=pollutant)
}

setwd(paste(NEI.wd, "Outputs", sep="/"))
write.csv(rawTotals, paste("Raw emissions totals - ", NEI.version, ".csv", sep=""), row.names=F)


## Totals for CONUS (used in the model)
point.totals <- ldply(point.sums, colSums)[,c("NOX", "PM25", "SO2", "VOC", "NH3")]
area.totals <- ldply(area.sums, colSums)[,c("NOX", "PM25-PRI", "SO2", "VOC_A", "VOC_B", "NH3")]

point.totals$year <- years; area.totals$year <- years
point.totals$stack <- "point"; area.totals$stack <- "area"
point.totals <- point.totals[, c("year", "stack", "NOX", "NH3", "SO2", "PM25", "VOC")]
area.totals <- area.totals[, c("year", "stack", "NOX", "NH3", "SO2", "PM25-PRI", "VOC_A", "VOC_B")]
colnames(area.totals)[colnames(area.totals) == "PM25-PRI"] <- "PM25"
colnames(point.totals)[colnames(point.totals) == "VOC"] <- "VOC_A"
point.totals$VOC_B <- 0

national.totals <- rbind(area.totals, point.totals)
national.totals <- national.totals[order(national.totals$year),]

# convert to thousand tons
national.totals[,c("NOX", "NH3", "SO2", "PM25", "VOC_A", "VOC_B")] <- national.totals[,c("NOX", "NH3", "SO2", "PM25", "VOC_A", "VOC_B")] / 1E3

setwd(paste(NEI.wd, "Outputs", sep="/"))
write.csv(national.totals, file="Total CONUS emissions.csv", row.names=F)


## Total emissions by stack height
point.stack.totals <- ldply(point.sums, colSums)[,c("NOX_tall", "PM25_tall", "SO2_tall", "VOC_tall", "NH3_tall",
                                                    "NOX_med", "PM25_med", "SO2_med", "VOC_med", "NH3_med",
                                                    "NOX_low", "PM25_low", "SO2_low", "VOC_low", "NH3_low")]

point.stack.totals$year <- years      
point.stack.totals <- melt(point.stack.totals, id.vars="year")
point.stack.totals$height <- gsub("[[:alnum:]]+_", "",  point.stack.totals$variable)
point.stack.totals$variable <- gsub("_[[:alnum:]]+", "",  point.stack.totals$variable)

point.stack.totals <- reshape(point.stack.totals, idvar = c("year", "height"),
                              timevar = "variable", direction = "wide")

colnames(point.stack.totals) <- gsub("value\\.", "", colnames(point.stack.totals))
colnames(point.stack.totals)[colnames(point.stack.totals) == "VOC"] <- "VOC_A"
colnames(point.stack.totals)[colnames(point.stack.totals) == "height"] <- "stack"
point.stack.totals$VOC_B <- 0
point.stack.totals <- point.stack.totals[,c("year", "stack", "NOX", "NH3", "SO2", "PM25", "VOC_A", "VOC_B")]

stack.totals <- rbind(area.totals, point.stack.totals)
stack.totals <- stack.totals[order(stack.totals$stack),]

# convert to thousands 
# stack.totals[,c("NOX", "NH3", "SO2", "PM25", "VOC")] <- stack.totals[,c("NOX", "NH3", "SO2", "PM25", "VOC")] 

setwd(paste(NEI.wd, "Outputs", sep="/"))
write.csv(stack.totals, row.names = F, "Total CONUS emissions by stack.csv")

stack.totals.2014 <- stack.totals[stack.totals$year == 2014,]
sum2014 <- colSums(stack.totals.2014[,c("NOX", "NH3", "SO2", "PM25", "VOC_A", "VOC_B")])
stack.totals.2014 <- rbind(stack.totals.2014, data.frame(year=2014, stack="total", t(sum2014)))
stack.totals.2014 <- stack.totals.2014[,c("year", "stack", "NH3", "NOX", "PM25", "SO2", "VOC_A", "VOC_B")]
setwd(paste(NEI.wd, "Outputs", sep="/"))
write.csv(stack.totals.2014, row.names = F, "Total CONUS emissions by stack - 2014.csv")

## Check CONUS emissions
match_and_drop <- function(emissions.list){
  
  emissions.list <- llply(emissions.list, subset, st_usps_cd %in% c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", 
                                                                    "MD", "DC", "DE", "PA", "WV", "VA", "NC", "SC", 
                                                                    "GA", "FL", "AL", "MS", "TN", "KY", "LA", "AR", 
                                                                    "TX", "OK", "NM", "OH", "IN", "IL", "WI", "MI", 
                                                                    "MN", "KS", "MO", "IA", "NE", "ND", "SD", "CO", 
                                                                    "UT", "WY", "MT", "AZ", "NV", "CA", "WA", "OR", "ID"))
  
  for(i in 1:3){
    interim <- ddply(emissions.list[[i]], ~ pollutant_cd, summarize, emissions = sum(total_emissions, na.rm=T))
    if(i == 1){
      final <- interim
    } else {
      final <- merge(final, interim, by = "pollutant_cd")
    }
    
  }
  
  final <- t(final[,-1])
  rownames(final) <- years
  
  colnames(final) <- c("NH3", "NOX", "PM25-PRI", "SO2", "VOC")
  final <- final[,c("NOX", "PM25-PRI", "SO2", "VOC", "NH3")] 
  
  return(as.data.frame(final))
}

# original data (remove AK, HI, US territories, tribal lands)
final.area <- match_and_drop(area.sources)
final.point <- match_and_drop(point)

final.point$year <- years; final.area$year <- years
final.point$stack <- "point"; final.area$stack <- "area"
final.point <- final.point[, c("year", "stack", "NOX", "NH3", "SO2", "PM25-PRI", "VOC")]
final.area <- final.area[, c("year", "stack", "NOX", "NH3", "SO2", "PM25-PRI", "VOC")]
colnames(final.area)[colnames(final.area) == "PM25-PRI"] <- "PM25"
colnames(final.point)[colnames(final.point) == "PM25-PRI"] <- "PM25"

national.totals.raw <- rbind(final.area, final.point); rm(final.area); rm(final.point)
national.totals.raw <- national.totals.raw[order(national.totals.raw$year),]

# convert to thousand tons
national.totals.raw[,c("NOX", "NH3", "SO2", "PM25", "VOC")] <- national.totals.raw[,c("NOX", "NH3", "SO2", "PM25", "VOC")] / 1E3
# Raw totals slightly under processed values on account of adding in power plants on tribal lands, emissions from eGrid


## Emissions by state
stateSums <- function(sum, type, fips){
  
  # adjustment for Miami-Dade
  fips[fips$fips == 12086 & !is.na(fips$fips), "fips"] <- 12025
  
  if (type=="point"){
    sum <- sum[,c("fips", "NOX", "NH3", "SO2", "PM25", "VOC")]
    sum[,"Biogenic VOC"] <- 0
  } else {
    colnames(sum) <- c("fips", "NH3", "NOX", "PM25", "SO2", "VOC", "Biogenic VOC", "PM10")
    sum <- sum[,c("fips", "NH3", "NOX", "PM25", "SO2", "VOC", "Biogenic VOC")]
  }
  sum <- merge(sum, fips[,c("fips", "state")], all.x=T)
  
  sum <- melt(sum, id.vars = c("fips", "state"))
  sum <- ddply(sum, ~ state + variable, summarize, emissions=sum(value, na.rm=T))
  
  # convert to thousand tons
  sum$emissions <- sum$emissions / 1000
  sum <- dcast(sum, state ~ variable, value.var = "emissions")
  sum$height <- type
  
  return(sum)
  
}

# separate FIPS data for name of each county
setwd(NEI.wd)
census.fips <- read.csv("Census FIPS codes.txt", header=FALSE, colClasses = "character")
colnames(census.fips) <- c("state", "state.num", "county.num", "county.name", "fips.class")

# create new column with full fips format (state + county)
census.fips$fips <- as.numeric(with(census.fips, paste(state.num, county.num, sep = "")))
census.fips$county.num <- as.numeric(census.fips$county.num)

statePointSums <- llply(point.sums, stateSums, type="point", fips=census.fips)
stateAreaSums <- llply(area.sums, stateSums, type="area", fips=census.fips)

for (i in 1:3){
  statePointSums[[i]]$year <- c(2008, 2011, 2014)[i]
  stateAreaSums[[i]]$year <- c(2008, 2011, 2014)[i]
}

superStates <- rbind(statePointSums[[1]], statePointSums[[2]], statePointSums[[3]],
                     stateAreaSums[[1]], stateAreaSums[[2]], stateAreaSums[[3]])

superStates <- superStates[,c("state", "year", "height", "NH3", "NOX", "PM25", "SO2", "VOC", "Biogenic VOC")]
superStates <- superStates[order(superStates$state, superStates$year, superStates$height), ]

superStates <- melt(superStates, id.vars = c("state", "year", "height"))
superStates <- dcast(superStates, state + year ~ height + variable, value.var = "value")
colnames(superStates) <- c("state", "year", "NOx","NH3", "SO2", "PM25", "VOCs", "Biogenic VOCs","NOx","NH3", "SO2", "PM25", "VOCs", "Biogenic VOCs" )

setwd(paste(NEI.wd, "Outputs", sep="/"))
write.csv(superStates, "State emissions totals.csv", row.names=F)

## Check tall stacks

# point check
flag = TRUE
for (i in 1:length(years)) {
  for(poll in c("NOX", "PM25", "SO2", "VOC", "NH3"))
    if (sum(point.sums[[i]][,poll]) != sum(point.sums[[i]][,paste(poll, "tall", sep = "_")] + 
                                           point.sums[[i]][,paste(poll, "med", sep = "_")]  + point.sums[[i]][,paste(poll, "low", sep = "_")])){
      print(paste("Error:", poll, years[i]))
      flag = FALSE
    }
}

if(flag){print("Point stacks match.")}


## SCC codes ####

area.scc <- llply(area.sources, scc.grouping) 
point.scc <- llply(process, scc.grouping) 

scc.totals <- list()
scc.super.totals <- list()
for(i in 1:length(years)){
  scc.totals[[i]] <- rbind(area.scc[[i]], point.scc[[i]])
  names(scc.totals[[i]]) <- c("sector", "NH3", "NOX", "PM25", "SO2", "VOC")
  
  scc.totals[[i]]$sector <- mapvalues(scc.totals[[i]]$sector, 
                                      from = levels(sort(unique(scc$sector.text))),
                                      to = c(rep("Agriculture", 3), "Biogenics", "Transportation - infrastructure", "Cooking", rep("Dust", 3),
                                             rep("Fires", 3), rep("Other fuel combustion", 5), rep("Electricity generation", 5),
                                             rep("Industrial boilers", 5), rep("Residential fuel consumption", 4), "Transportation - infrastructure",
                                             "Cement manufacturing", "Chemical manufacturing", "Industrial processes - Other", "Mining", 
                                             rep("Industrial processes - Other", 2), "Industrial processes - Oil & gas production", 
                                             "Industrial processes - Petroleum refineries", 
                                             "Industrial processes - Pulp & paper mills", "Industrial processes - Other", 
                                             "Other", "Transportation - Aircraft", 
                                             "Transportation - Marine vessels", "Transportation - Trains",
                                             rep("Transportation - other mobile sources", 3), "Transportation - Heavy duty vehicles", 
                                             "Transportation - Light duty vehicles", "Transportation - Heavy duty vehicles",
                                             "Transportation - Light duty vehicles", rep("Solvents", 6), "Waste disposal"))
  
  scc.totals[[i]] <- ddply(scc.totals[[i]], ~ sector, summarize, NH3 = sum(NH3, na.rm=T),
                           NOX = sum(NOX, na.rm=T),
                           PM25 = sum(PM25, na.rm=T),
                           SO2 = sum(SO2, na.rm=T),
                           VOC = sum(VOC, na.rm=T))
  
  scc.totals[[i]]$super.sector <- mapvalues(scc.totals[[i]]$sector,
                                            from = c("Agriculture", "Biogenics", "Transportation - infrastructure", "Cooking", "Dust",
                                                     "Fires", "Other fuel combustion", "Electricity generation",
                                                     "Industrial boilers", "Residential fuel consumption",
                                                     "Cement manufacturing", "Chemical manufacturing", "Mining",
                                                     "Industrial processes - Other", "Industrial processes - Oil & gas production",
                                                     "Industrial processes - Petroleum refineries",
                                                     "Industrial processes - Pulp & paper mills",
                                                     "Other", "Transportation - Aircraft",
                                                     "Transportation - Marine vessels", "Transportation - Trains",
                                                     "Transportation - other mobile sources", "Transportation - Heavy duty vehicles",
                                                     "Transportation - Light duty vehicles", "Solvents", "Waste disposal"),
                                            to = c("Agriculture", "Biogenic sources", "Transportation", "Cooking", "Dust", "Fires", "Residential & other fuel consumption",
                                                   "Electricity generation", "Industrial processes", "Residential & other fuel consumption", "Cement manufacturing",
                                                   "Chemical manufacturing", "Mining", rep("Industrial processes", 4), 
                                                   "Other", rep("Transportation", 6), rep("Other", 2)))
  
  scc.super.totals[[i]] <- ddply(scc.totals[[i]], ~ super.sector, summarize, NH3 = sum(NH3, na.rm=T),
                                 NOX = sum(NOX, na.rm=T),
                                 PM25 = sum(PM25, na.rm=T),
                                 SO2 = sum(SO2, na.rm=T),
                                 VOC = sum(VOC, na.rm=T))
  
}

percentChangeCalc <- function(sectorEmissions, sectorName, pollutant, region=NULL){
  if(!is.null(region)){
    start <- sectorEmissions[[1]][sectorEmissions[[1]]$super.sector == sectorName & sectorEmissions[[1]]$region == region, pollutant]
    end <- sectorEmissions[[3]][sectorEmissions[[3]]$super.sector == sectorName & sectorEmissions[[3]]$region == region, pollutant]
  } else{
    start <- sectorEmissions[[1]][sectorEmissions[[1]]$super.sector == sectorName, pollutant]
    end <- sectorEmissions[[3]][sectorEmissions[[3]]$super.sector == sectorName, pollutant]
  }
  return( (end-start)/start*100)
}

percentChangeCalc(scc.super.totals, "Electricity generation", "PM25")
percentChangeCalc(scc.super.totals, "Electricity generation", "SO2")

# Summary bar plot

scc.long <- llply(scc.super.totals, melt, id.vars = "super.sector", measure.vars = c("NH3", "NOX", "PM25", "SO2", "VOC"))
for(i in 1:length(years)){
  colnames(scc.long[[i]]) <- c("Sector", "pollutant", "tons")
  scc.long[[i]]$Year <- factor(years[i])
}

scc.long <- ldply(scc.long)
scc.long$tons <- scc.long$tons / 1E6  # convert to millions

scc.long$Sector <- factor(scc.long$Sector, levels = sort(unique(c("Agriculture", "Biogenic sources", "Transportation", "Cooking", 
                                                                  "Dust", "Fires", "Residential & other fuel consumption",
                                                                  "Electricity generation", "Industrial processes", 
                                                                  "Residential & other fuel consumption", "Cement manufacturing",
                                                                  "Chemical manufacturing", "Mining", rep("Industrial processes", 4), 
                                                                  "Other", rep("Transportation", 6), rep("Other", 2)))))

ggplot(scc.long, aes(x = Year, y = tons, fill = Sector))+
  geom_bar(stat = "identity", color="black") + facet_wrap( ~ pollutant, ncol=2, scales = "free") +
  ylab("Total emissions\n(million tons)") + xlab("Year") + theme_bw() + 
  theme(legend.text=element_text(size=8))

setwd(paste(NEI.wd, "Outputs", sep="/"))
ggsave("Sector emissions breakdown.pdf", width = 8, height = 6)

subSector <- scc.long[scc.long$Sector == "Cooking",]
#subSector <- scc.long[scc.long$Sector == "Dust",]
ggplot(subSector, aes(x = Year, y = tons, fill = pollutant))+
  geom_bar(stat = "identity", color="black", position="dodge") +
  ylab("Total emissions\n(million tons)") + xlab("Year") + theme_bw() + 
  theme(legend.text=element_text(size=8))

subPollutant <- scc.long[scc.long$pollutant == "PM25",]
#subPollutant <- scc.long[scc.long$pollutant == "NH3",]
#subPollutant <- scc.long[scc.long$pollutant == "NOX",]
#subPollutant <- scc.long[scc.long$pollutant == "VOC",]
#subPollutant <- scc.long[scc.long$pollutant == "SO2",]

ggplot(subPollutant, aes(x = Year, y = tons, fill = Sector))+
  geom_bar(stat = "identity", color="black", position="dodge") +
  ylab("Total emissions\n(million tons)") + xlab("Year") + theme_bw() + 
  theme(legend.text=element_text(size=8))


## Differences by sector and year
scc.percent <- list()

# percent of total

percent.scc <- function(df){
  scc.percent <- sweep(as.matrix(df[,-c(1,7)]), 2, colSums(df[,-c(1,7)]), "/")
  scc.percent <- data.frame(round(scc.percent * 100, 1))
  print(colSums(scc.percent))
  
  scc.percent$sub.sector <- df[,1]
  scc.percent$Sector <- df[,7]
  scc.percent <- scc.percent[,c("Sector", "sub.sector", "NH3", "PM25", "NOX", "SO2", "VOC")]
}

scc.percent <- ldply(scc.totals, percent.scc)
scc.percent$Year <- factor(rep(years, each=26))

scc.percent.long <- melt(scc.percent, id.vars = c("Sector", "sub.sector", "Year"))
scc.percent.long$Sector <- factor(scc.percent.long$Sector, levels = sort(unique(c("Agriculture", "Biogenic sources", "Transportation", "Cooking", 
                                                                  "Dust", "Fires", "Residential & other fuel consumption",
                                                                  "Electricity generation", "Industrial processes", 
                                                                  "Residential & other fuel consumption", "Cement manufacturing",
                                                                  "Chemical manufacturing", "Mining", rep("Industrial processes", 4), 
                                                                  "Other", rep("Transportation", 6), rep("Other", 2)))))
colnames(scc.percent.long) <- c("Sector", "sub.sector", "Year", "Pollutant", "Percent")

scc.percent.long <- ddply(scc.percent.long, ~ Sector + Year + Pollutant, summarize, Percent=sum(Percent))
scc.percent.long$Pollutant <- factor(scc.percent.long$Pollutant, levels=c("NH3", "NOX", "PM25", "SO2", "VOC"))

ggplot(scc.percent.long, aes(x = Year, y = Percent, fill = Sector))+
  geom_bar(stat = "identity", color="black") + facet_wrap( ~ Pollutant, ncol=2, scales = "free") +
  ylab("Percent of total emissions") + xlab("Year") + theme_bw() + 
  theme(legend.text=element_text(size=8))

setwd(paste(NEI.wd, "Outputs", sep="/"))
ggsave("Sector emissions breakdown by percent.pdf", width = 8, height = 6)


## SCC Codes - state level analysis ####

area.scc.state <- llply(area.sources, scc.grouping, state=T) 
point.scc.state <- llply(process, scc.grouping, state=T) 

state.totals <- list()
super.state.totals <- list()
super.region.totals <- list()
for(i in 1:length(years)){
  state.totals[[i]] <- rbind(area.scc.state[[i]], point.scc.state[[i]])
  names(state.totals[[i]]) <- c("state", "sector", "NH3", "NOX", "PM25", "SO2", "VOC")
  
  state.totals[[i]]$sector <- mapvalues(state.totals[[i]]$sector, 
                                        from = levels(sort(unique(scc$sector.text))),
                                        to = c(rep("Agriculture", 3), "Biogenics", "Transportation - infrastructure", "Cooking", rep("Dust", 3),
                                               rep("Fires", 3), rep("Other fuel combustion", 5), rep("Electricity generation", 5),
                                               rep("Industrial boilers", 5), rep("Residential fuel consumption", 4), "Transportation - infrastructure",
                                               "Cement manufacturing", "Chemical manufacturing", "Industrial processes - Other", "Mining", 
                                               rep("Industrial processes - Other", 2), "Industrial processes - Oil & gas production", 
                                               "Industrial processes - Petroleum refineries", 
                                               "Industrial processes - Pulp & paper mills", "Industrial processes - Other", 
                                               "Other", "Transportation - Aircraft", 
                                               "Transportation - Marine vessels", "Transportation - Trains",
                                               rep("Transportation - other mobile sources", 3), "Transportation - Heavy duty vehicles", 
                                               "Transportation - Light duty vehicles", "Transportation - Heavy duty vehicles",
                                               "Transportation - Light duty vehicles", rep("Solvents", 6), "Waste disposal"))
  
  state.totals[[i]] <- ddply(state.totals[[i]], ~ state + sector, summarize, NH3 = sum(NH3, na.rm=T),
                             NOX = sum(NOX, na.rm=T),
                             PM25 = sum(PM25, na.rm=T),
                             SO2 = sum(SO2, na.rm=T),
                             VOC = sum(VOC, na.rm=T))
  
  state.totals[[i]]$super.sector <- mapvalues(state.totals[[i]]$sector,
                                              from = c("Agriculture", "Biogenics", "Transportation - infrastructure", "Cooking", "Dust",
                                                       "Fires", "Other fuel combustion", "Electricity generation",
                                                       "Industrial boilers", "Residential fuel consumption",
                                                       "Cement manufacturing", "Chemical manufacturing", "Mining",
                                                       "Industrial processes - Other", "Industrial processes - Oil & gas production",
                                                       "Industrial processes - Petroleum refineries",
                                                       "Industrial processes - Pulp & paper mills",
                                                       "Other", "Transportation - Aircraft",
                                                       "Transportation - Marine vessels", "Transportation - Trains",
                                                       "Transportation - other mobile sources", "Transportation - Heavy duty vehicles",
                                                       "Transportation - Light duty vehicles", "Solvents", "Waste disposal"),
                                              to = c("Agriculture", "Biogenic sources", "Transportation", "Cooking", "Dust", "Fires", "Residential & other fuel consumption",
                                                     "Electricity generation", "Industrial processes", "Residential & other fuel consumption", "Cement manufacturing",
                                                     "Chemical manufacturing", "Mining", rep("Industrial processes", 4), 
                                                     "Other", rep("Transportation", 6), rep("Other", 2)))
  
  super.state.totals[[i]] <- ddply(state.totals[[i]], ~ state + super.sector, summarize, NH3 = sum(NH3, na.rm=T),
                                   NOX = sum(NOX, na.rm=T),
                                   PM25 = sum(PM25, na.rm=T),
                                   SO2 = sum(SO2, na.rm=T),
                                   VOC = sum(VOC, na.rm=T))
  
  super.region.totals[[i]] <- super.state.totals[[i]]
  super.region.totals[[i]]$region <- mapvalues(super.region.totals[[i]]$state,
                                        from = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "MD", "DC", "DE", "PA",
                                                 "WV", "VA", "NC", "SC", "GA", "FL", "AL", "MS", "TN", "KY", "LA","AR",
                                                 "OH", "IN", "IL", "WI", "MI", "MN", "KS", "MO", "IA", "NE", "ND", "SD",
                                                 "TX",  "OK", "NM",  "AZ",
                                                 "NV","CA", "WA", "OR", "ID", "CO", "UT", "WY", "MT"),
                                        to = c(rep("NE", 12),
                                               rep("SE", 12),
                                               rep("MW", 12),
                                               rep("SW", 4),
                                               rep("W", 9)))
  
  super.region.totals[[i]] <- ddply(super.region.totals[[i]], ~ region + super.sector, summarize,  NH3 = sum(NH3, na.rm=T),
                                                                                             NOX = sum(NOX, na.rm=T),
                                                                                             PM25 = sum(PM25, na.rm=T),
                                                                                             SO2 = sum(SO2, na.rm=T),
                                                                                             VOC = sum(VOC, na.rm=T))
                       
  
}

percentChangeCalc(super.region.totals, "Electricity generation", "SO2", "NE")
percentChangeCalc(super.region.totals, "Electricity generation", "SO2", "MW")



stateChange <- function(year1, year2, years){
  
  pollutants <- c("NH3", "NOX", "PM25", "SO2", "VOC")
  colnames(year1) <- c("state", "sector", paste(pollutants, years[1], sep="_"))
  colnames(year2) <- c("state", "sector", paste(pollutants, years[2], sep="_"))
  
  yearTotals <- merge(year1, year2, by=c("state", "sector"), all=T)
  
  for (pollutant in pollutants){
    var1 <- paste(pollutant, years[1],sep="_")
    var2 <- paste(pollutant, years[2],sep="_")
    
    yearTotals[,paste(pollutant, "change", sep="_")] <- round((yearTotals[,var2] - yearTotals[,var1]) / yearTotals[,var1] * 100, 1)
    
  }
  
  return(yearTotals)
  
}

stateChanges <- stateChange(super.state.totals[[1]], super.state.totals[[3]], years=c(2008, 2014))
stateChanges[stateChanges$state == "MS", c("state", "sector", "NH3_change", "NOX_change", "PM25_change", "SO2_change", "VOC_change")]
stateChanges[stateChanges$state == "FL", ]


# county analysis
area.scc.county <- llply(area.sources, scc.grouping, county=T) 
point.scc.county <- llply(process, scc.grouping,  county=T) 

county.totals <- list()
super.county.totals <- list()
for(i in 1:length(years)){
  county.totals[[i]] <- rbind(area.scc.county[[i]], point.scc.county[[i]])
  names(county.totals[[i]]) <- c("state", "sector", "NH3", "NOX", "PM25", "SO2", "VOC")
  
  county.totals[[i]]$sector <- mapvalues(county.totals[[i]]$sector, 
                                        from = levels(sort(unique(scc$sector.text))),
                                        to = c(rep("Agriculture", 3), "Biogenics", "Transportation - infrastructure", "Cooking", rep("Dust", 3),
                                               rep("Fires", 3), rep("Other fuel combustion", 5), rep("Electricity generation", 5),
                                               rep("Industrial boilers", 5), rep("Residential fuel consumption", 4), "Transportation - infrastructure",
                                               "Cement manufacturing", "Chemical manufacturing", "Industrial processes - Other", "Mining", 
                                               rep("Industrial processes - Other", 2), "Industrial processes - Oil & gas production", 
                                               "Industrial processes - Petroleum refineries", 
                                               "Industrial processes - Pulp & paper mills", "Industrial processes - Other", 
                                               "Other", "Transportation - Aircraft", 
                                               "Transportation - Marine vessels", "Transportation - Trains",
                                               rep("Transportation - other mobile sources", 3), "Transportation - Heavy duty vehicles", 
                                               "Transportation - Light duty vehicles", "Transportation - Heavy duty vehicles",
                                               "Transportation - Light duty vehicles", rep("Solvents", 6), "Waste disposal"))
  
  county.totals[[i]] <- ddply(county.totals[[i]], ~ state + sector, summarize, NH3 = sum(NH3, na.rm=T),
                             NOX = sum(NOX, na.rm=T),
                             PM25 = sum(PM25, na.rm=T),
                             SO2 = sum(SO2, na.rm=T),
                             VOC = sum(VOC, na.rm=T))
  
  county.totals[[i]]$super.sector <- mapvalues(county.totals[[i]]$sector,
                                              from = c("Agriculture", "Biogenics", "Transportation - infrastructure", "Cooking", "Dust",
                                                       "Fires", "Other fuel combustion", "Electricity generation",
                                                       "Industrial boilers", "Residential fuel consumption",
                                                       "Cement manufacturing", "Chemical manufacturing", "Mining",
                                                       "Industrial processes - Other", "Industrial processes - Oil & gas production",
                                                       "Industrial processes - Petroleum refineries",
                                                       "Industrial processes - Pulp & paper mills",
                                                       "Other", "Transportation - Aircraft",
                                                       "Transportation - Marine vessels", "Transportation - Trains",
                                                       "Transportation - other mobile sources", "Transportation - Heavy duty vehicles",
                                                       "Transportation - Light duty vehicles", "Solvents", "Waste disposal"),
                                              to = c("Agriculture", "Biogenic sources", "Transportation", "Cooking", "Dust", "Fires", "Residential & other fuel consumption",
                                                     "Electricity generation", "Industrial processes", "Residential & other fuel consumption", "Cement manufacturing",
                                                     "Chemical manufacturing", "Mining", rep("Industrial processes", 4), 
                                                     "Other", rep("Transportation", 6), rep("Other", 2)))
  
  super.county.totals[[i]] <- ddply(county.totals[[i]], ~ state + super.sector, summarize, NH3 = sum(NH3, na.rm=T),
                                   NOX = sum(NOX, na.rm=T),
                                   PM25 = sum(PM25, na.rm=T),
                                   SO2 = sum(SO2, na.rm=T),
                                   VOC = sum(VOC, na.rm=T))
  
}

countyChanges <- stateChange(super.county.totals[[1]], super.county.totals[[3]], years=c(2008, 2014))
countyChangesSector <- stateChange(county.totals[[1]][,-8], county.totals[[3]][,-8], years=c(2008, 2014))

colnames(countyChanges)[colnames(countyChanges) == "state"] <- "fips"
## Old code ####

# area.scc <- llply(area.sources, scc.grouping, state=T) 
# point.scc <- llply(process, scc.grouping, state=T) 

# 
# scc.state.totals <- list()
# for(i in 1:3){
#   scc.state.totals[[i]] <- rbind(area.scc[[i]], point.scc[[i]])
#   names(scc.state.totals[[i]]) <- c("state", "sector", "NH3", "NOX", "PM25", "SO2", "VOC")
#   
#   scc.state.totals[[i]]$sector <- mapSectors(scc.state.totals[[i]]$sector, scc)
#   
#   scc.state.totals[[i]] <- ddply(scc.state.totals[[i]], ~ state + sector, summarize, NH3 = sum(NH3, na.rm=T),
#                                  NOX = sum(NOX, na.rm=T),
#                                  PM25 = sum(PM25, na.rm=T),
#                                  SO2 = sum(SO2, na.rm=T),
#                                  VOC = sum(VOC, na.rm=T))
# }
# 
# 
# percent changes by state

scc.state.percent <- merge(scc.state.totals[[1]], scc.state.totals[[3]], by = c("state", "sector"), all = T)

scc.state.percent$NOX <- (scc.state.percent$NOX.y - scc.state.percent$NOX.x) / scc.state.percent$NOX.x
scc.state.percent$NH3 <- (scc.state.percent$NH3.y - scc.state.percent$NH3.x) / scc.state.percent$NH3.x
scc.state.percent$SO2 <- (scc.state.percent$SO2.y - scc.state.percent$SO2.x) / scc.state.percent$SO2.x
scc.state.percent$VOC <- (scc.state.percent$VOC.y - scc.state.percent$VOC.x) / scc.state.percent$VOC.x
scc.state.percent$PM25 <- (scc.state.percent$PM25.y - scc.state.percent$PM25.x) / scc.state.percent$PM25.x


# regions

# 
# 
# scc.region.percent$NOX <- (scc.region.percent$NOX.y - scc.region.percent$NOX.x) / scc.region.percent$NOX.x
# scc.region.percent$NH3 <- (scc.region.percent$NH3.y - scc.region.percent$NH3.x) / scc.region.percent$NH3.x
# scc.region.percent$SO2 <- (scc.region.percent$SO2.y - scc.region.percent$SO2.x) / scc.region.percent$SO2.x
# scc.region.percent$VOC <- (scc.region.percent$VOC.y - scc.region.percent$VOC.x) / scc.region.percent$VOC.x
# scc.region.percent$PM25 <- (scc.region.percent$PM25.y - scc.region.percent$PM25.x) / scc.region.percent$PM25.x
# 
# scc.state.percent <- ddply(scc.state.percent, ~ state, transform,  NH3.percent = NH3.y / sum(NH3.y, na.rm=T),
#                            NOX.percent = NOX.y / sum(NOX.y, na.rm=T),
#                            SO2.percent = SO2.y / sum(SO2.y, na.rm=T),
#                            VOC.percent = VOC.y / sum(VOC.y, na.rm=T),
#                            PM25.percent = PM25.y / sum(PM25.y, na.rm=T))
# 
# scc.region.percent <- ddply(scc.region.percent, ~ region, transform,  NH3.percent = NH3.y / sum(NH3.y, na.rm=T),
#                             NOX.percent = NOX.y / sum(NOX.y, na.rm=T),
#                             SO2.percent = SO2.y / sum(SO2.y, na.rm=T),
#                             VOC.percent = VOC.y / sum(VOC.y, na.rm=T),
#                             PM25.percent = PM25.y / sum(PM25.y, na.rm=T))
# 
# scc.state.percent <- scc.state.percent[, c("state", "sector", "NOX", "NOX.y", "NOX.percent",
#                                            "SO2", "SO2.y", "SO2.percent",
#                                            "VOC", "VOC.y", "VOC.percent",
#                                            "PM25", "PM25.y", "PM25.percent",
#                                            "NH3", "NH3.y", "NH3.percent")]
# 
# scc.region.percent <- scc.region.percent[, c("region", "sector", "NOX", "NOX.y", "NOX.percent",
#                                              "SO2", "SO2.y", "SO2.percent",
#                                              "VOC", "VOC.y", "VOC.percent",
#                                              "PM25", "PM25.y", "PM25.percent",
#                                              "NH3", "NH3.y", "NH3.percent")]
# 
# # function to explore change in emissions for a given state, pollutant, and sector
# state.plant.look <- function(state, poll, process.name, merge=T){
#   
#   state.2008 <- process[[1]][process[[1]]$st_usps_cd == state & process[[1]]$pollutant_cd == poll & process[[1]]$sector.text == process.name,]
#   state.2014 <- process[[3]][process[[3]]$st_usps_cd == state & process[[3]]$pollutant_cd == poll & process[[3]]$sector.text == process.name,]
#   
#   state.2014$eis_facility_site_id <- as.integer(state.2014$eis_facility_site_id)
#   state.2014$eis_emissions_process_id <- as.integer(state.2014$eis_emissions_process_id)
#   
#   if(merge){
#     state.plants <- merge(state.2008[,c("eis_facility_site_id", "eis_emissions_process_id","total_emissions")], 
#                           state.2014[,c("eis_facility_site_id", "eis_emissions_process_id","total_emissions")],
#                           by = c("eis_facility_site_id", "eis_emissions_process_id"), all = T)
#     
#     return(state.plants)
#     
#   } else {
#     return(list(state.2008, state.2014))
#   }
#   
#   
# }
# 
# # oil and gas extraction
# gas.2008 <- scc.totals[[1]][scc.totals[[1]]$sector == "Industrial processes - Oil & gas production",]
# gas.2014 <- scc.totals[[3]][scc.totals[[3]]$sector == "Industrial processes - Oil & gas production",]
# gas.2008[,-c(1,7)] / colSums(scc.totals[[1]][,-c(1,7)])
