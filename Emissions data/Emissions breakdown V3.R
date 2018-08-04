# Emissions breakdown
# Created: January 5, 2018
# Author: Brian Sergi

## Notes ####

# Description: Breakdown of changes to emissions over the 2008-2014 period, including categories and types

# NEI data in short tons. NEI column names changed starting with V2 in 2014. 

# Require the following emissions files:
# 1. Nonroad
# 2. Onroad
# 3. Nonpoint
# 4. Biogenic VOCs
# 5. Point sources (facility level)
# 6. Point sources (process level)

## Settings ####

#NEI.wd <- "/Volumes/Seagate Backup Plus Drive/AP2/NEI"     # home directory (use for relative paths)
NEI.wd <- getwd()
NEI.version <- "NEI July 2018"                             # date NEI files were last downloaded
years <- c(2008, 2011, 2014)                               # years of emissions data (note: can't change without updating tall stacks code)
save <- TRUE                                               # control output to csv files / saved workspace
changeColumns2014 <- TRUE                                  # manually adjust column names for 2014 data

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


## Select and read emissions functions  ####

# function to subset to 5 pollutants of interest
select.emissions <- function(emissions.df){
  if("description" %in% colnames(emissions.df)){
    emissions.df <- subset(emissions.df, description %in% c("Sulfur Dioxide",
                                                            "Nitrogen Oxides",
                                                            "PM2.5 Primary (Filt + Cond)",
                                                            "Volatile Organic Compounds",
                                                            "Ammonia"))
  } else{
    emissions.df <- subset(emissions.df, pollutant_desc %in% c("Sulfur Dioxide",
                                                            "Nitrogen Oxides",
                                                            "PM2.5 Primary (Filt + Cond)",
                                                            "Volatile Organic Compounds",
                                                            "Ammonia"))
  }

  
  emissions.df$total_emissions <- as.numeric(emissions.df$total_emissions)
  emissions.df$state_and_county_fips_code <- as.numeric(emissions.df$state_and_county_fips_code)
  
  return(emissions.df)
}

# function to read area source emissions data sets
readEmissions <- function(years, filenames, baseDirectory=""){
  emissions <- list()
  for(j in 1:length(years)){
    if(baseDirectory == ""){
      setwd(paste(NEI.wd, years[j], NEI.version, sep="/"))
    } else{
      setwd(paste(NEI.wd, years[j], NEI.version, baseDirectory, sep="/"))
    }
    for(i in 1:length(filenames)){
      emissionsTemp <- fread(filenames[i])
      # sub set to 5 pollutants and format 
      emissionsTemp <- select.emissions(emissionsTemp)
      if(i == 1){
        emissions[[j]] <- emissionsTemp
      } else {
        emissions[[j]] <- rbind(emissions[[j]], emissionsTemp)
      }
      rm(emissionsTemp)
    }
  }
  return(emissions)
}

updateColumns <- function(data2014, targetColumns){
  
  data2014 <- data2014[,c("state_and_county_fips_code", "tribal_name", "st_usps_cd",
                                    "county_name", "data_category", "scc", "emissions_type_code",
                                    "emission_operating_type", "emissions_operating_type", "data_set",
                                    "pollutant_cd", "pollutant_desc" ,"total_emissions", "uom")]
  colnames(data2014) <- targetColumns
  return(data2014)
}

## Load fips codes ####

# read in FIPS codes
setwd(NEI.wd)
fips <- read.dta13("AP2_Fips_List.dta", convert.dates = TRUE, convert.factors = TRUE,
                   missing.type = FALSE, convert.underscore = FALSE)

# assign stack heights
fips$stack <- c(rep("area",3109), rep("low",3109), rep("med",3109), rep("tall",565), rep("tall2",91))
fips.stacks <- fips
fips <- fips[fips$stack == "area", "fips"]

## SCC codes ####
setwd(NEI.wd)
scc <- read.csv("SCCDownload-2018-0105-171115.csv")
scc$Code <- as.numeric(levels(scc$Code))[scc$Code]

## Load area sources ####
# read in nonroad, onroad, and nonpoint data

# 1. nonroad 
# list of working directories to access and filenames to read
filenames <- c("nonroad_123.csv", "nonroad_4.csv", "nonroad_5.csv", "nonroad_67.csv", "nonroad_8910.csv")
nonroad <- readEmissions(years, filenames, "Nonroad")

# 2. onroad
filenames <- c("onroad_123.csv", "onroad_4.csv", "onroad_5.csv", "onroad_67.csv", "onroad_8910.csv")
onroad <- readEmissions(years, filenames, "Onroad")

# 3. nonpoint
filenames <- "nonpoint.csv"
nonpoint <- readEmissions(years, filenames)
rm(filenames)

# Update column names from 2014 data
nonroad[[3]] <- updateColumns(data2014 = nonroad[[3]], colnames(nonroad[[1]]))
onroad[[3]] <- updateColumns(data2014 = onroad[[3]], colnames(onroad[[1]]))
nonpoint[[3]] <- updateColumns(data2014 = nonpoint[[3]], colnames(nonpoint[[1]]))

# merge area sources into one data frame and add SCC codes
area.sources <- list()
for (i in 1:length(years)){
  area.sources[[i]] <- rbind(nonpoint[[i]], nonroad[[i]], onroad[[i]])
  area.sources[[i]] <- merge(area.sources[[i]], scc[,c("Code", "sector.text", "scc.level.one.text")], by.x = "scc", by.y = "Code", all.x=T)
  area.sources[[i]] <- area.sources[[i]][,c("scc", "state_and_county_fips_code", "st_usps_cd", "county_name", "data_category_cd", "data_set_short_name",
                                            "pollutant_cd", "description", "total_emissions", "uom", "sector.text", "scc.level.one.text")]
}
rm(nonpoint); rm(nonroad); rm(onroad)


## Separate VOCs ####
VOCs <- llply(area.sources, subset, pollutant_cd == "VOC")

# function to summarize biogenic VOCs
biogenic.sum <- function(x){
  x.sub <- x[x$sector.text == "Biogenics - Vegetation and Soil", ]
  x.sum <- ddply(x.sub, ~ state_and_county_fips_code, summarize, total = sum(total_emissions, na.rm=T)) 
  return(x.sum)
}

VOCs.bio <- llply(VOCs, biogenic.sum)
VOCs <- llply(VOCs, ddply, ~ state_and_county_fips_code, summarize, total = sum(total_emissions, na.rm=T))

rename.list <- function(x, total){
  names(x) <- c("fips", total)
  return(x)
}

VOCs <- llply(VOCs, rename.list, total="total")
VOCs.bio <- llply(VOCs.bio, rename.list, total="biogenic")

for(i in 1:length(years)){
  VOCs[[i]] <- merge(VOCs[[i]], VOCs.bio[[i]], by="fips", all=T)
  VOCs[[i]]$anthropogenic <- VOCs[[i]]$total - VOCs[[i]]$biogenic
}
rm(VOCs.bio)

# subset to match existing fips codes
VOCs.save <- VOCs    # create temporary "save" data frame to check for missing counties
for(i in 1:length(years)){
  # match with AP2 fips list
  VOCs[[i]] <- merge(data.frame(fips = fips), VOCs[[i]], by="fips", all=T)
  VOCs[[i]] <- VOCs[[i]][, c("fips", "anthropogenic", "biogenic", "total")]
  VOCs.save[[i]] <- VOCs.save[[i]][, c("fips", "anthropogenic", "biogenic", "total")]
}

## Sum area sources by fips code ####
area.sums <- area.sources
for(i in 1:length(years)){
  names(area.sums[[i]])[names(area.sums[[i]]) == "state_and_county_fips_code"] <- "fips"
  area.sums[[i]] <- ddply(area.sums[[i]], ~ fips + pollutant_cd, summarize, total = sum(total_emissions, na.rm=T))
}

# summarize
area.sums <- llply(area.sums, spread, key="pollutant_cd", value="total")
area.sums.save <- area.sums   # create temporary "save" data frame to check for missing counties

# reduce to fips in CONUS, merge in VOCs
for(i in 1:length(years)){
  area.sums[[i]] <- merge(data.frame(fips = fips), area.sums[[i]], by="fips", all.x=T)
  area.sums[[i]] <- subset(area.sums[[i]], select=-VOC)
  area.sums[[i]] <- merge(area.sums[[i]], VOCs[[i]], by="fips", all.x=T)
  
  names(area.sums[[i]]) <- mapvalues(names(area.sums[[i]]),
                                     from = c("fips", "NH3", "NOX", "PM25-PRI", "SO2",  "anthropogenic", "biogenic", "total"),
                                     to = c("fips", "NH3", "NOX", "PM25-PRI", "SO2",  "VOC_A", "VOC_B", "VOC"))
  
  # processesing for missing counties
  area.sums.save[[i]] <- subset(area.sums.save[[i]], select=-VOC)
  area.sums.save[[i]] <- merge(area.sums.save[[i]], VOCs.save[[i]], by="fips", all=T)
  
  names(area.sums.save[[i]]) <- mapvalues(names(area.sums.save[[i]]),
                                          from = c("fips", "NH3", "NOX", "PM25-PRI", "SO2",  "anthropogenic", "biogenic", "total"),
                                          to = c("fips", "NH3", "NOX", "PM25-PRI", "SO2",  "VOC_A", "VOC_B", "VOC"))
}  
rm(VOCs); rm(VOCs.save)

## Check for missing fips codes ####

# changed counties
# https://www1.udel.edu/johnmack/frec682/fips_codes.html

area.sums[[1]][rowSums(is.na(area.sums[[1]])) > 0,]
area.sums[[2]][rowSums(is.na(area.sums[[2]])) > 0,]
area.sums[[3]][rowSums(is.na(area.sums[[3]])) > 0,]

for(i in 1:length(years)){
  # 12025 listed in AP2 fips, but now should be 12086 (Miami-Dade)
  area.sums[[i]][area.sums[[i]]$fips == 12025, -1] <- area.sums.save[[i]][area.sums.save[[i]]$fips == 12086, -1]   
  
  # 51560 collapsed into 51005 (change occurred in 2001)
  area.sums[[i]][area.sums[[i]]$fips == 51560, -1] <- 0
  
  # 8014 formed as new county; put emissions in 8013
  area.sums[[i]][area.sums[[i]]$fips == 8013, -1] <- area.sums[[i]][area.sums[[i]]$fips == 8013, -1] + area.sums.save[[i]][area.sums.save[[i]]$fips == 8014, -1] 
  
}

# 51515 collapsed into 51019 (change occurred in 2015, applied to 2014 NEI only)
area.sums[[3]][area.sums[[3]]$fips == 51515, -1] <- 0

# check on tribal emissions
ddply(area.sources[[1]][area.sources[[1]]$tribal_name != "",], ~ pollutant_cd, summarize, total = sum(total_emissions, na.rm=T))
ddply(area.sources[[2]][area.sources[[2]]$tribal_name != "",], ~ pollutant_cd, summarize, total = sum(total_emissions, na.rm=T))
ddply(area.sources[[3]][area.sources[[3]]$tribal_name != "",], ~ pollutant_cd, summarize, total = sum(total_emissions, na.rm=T))

rm(area.sums.save)

## Save area sources to csv ####
if(save){
  setwd(paste(NEI.wd, "AP2 inputs", sep="/"))
  for(i in 1:length(years)){
    area.sums[[i]]$PM10 <- 0
    if(identical(area.sums[[i]]$fips, fips)){
      area.sums.final <- area.sums[[i]][,c("NH3", "NOX", "PM10", "PM25-PRI", "SO2", "VOC_A", "VOC_B")]
      colnames(area.sums.final) <- NULL
      print(paste("Saving area sources", years[i]))
      write.csv(area.sums.final, paste(years[i], "- area sources.csv"), row.names=F)
      rm(area.sums.final)
    } else{
      print("Check FIPS code order.")
    }
  }
}

## Load point sources ####

# process based (needed for scc based emissions allocation)
filenames <- c("process_12345.csv", "process_678910.csv")
process <- readEmissions(years, filenames, "Process")

# facility summaries (point source totals)
filenames <- c("facilities.csv")
point <- readEmissions(years, filenames)
rm(filenames)

# rename fips code
for(i in 1:length(years)){
  names(point[[i]])[names(point[[i]]) == "state_and_county_fips_code"] <- "fips"
}

# manual adjustments to fips codes in point sources (not listed for tribal areas)
# see tall stack adjustments below
for(i in 1:length(years)){
  point[[i]][point[[i]]$facility_site_name == "NAVAJO GENERATING STATION", "fips"] <- 4005
  point[[i]][point[[i]]$facility_site_name == "Bonanza", "fips"] <- 49047
  point[[i]][point[[i]]$facility_site_name == "Four Corners Power Plant", "fips"] <- 35045
  
  # Broomfield CO
  point[[i]][point[[i]]$fips == 8014, "fips"] <- 8013
}

point.sums <- point
for(i in 1:length(years)){
  # convert EIS to  integer
  point.sums[[i]]$eis_facility_site_id <- as.numeric(point.sums[[i]]$eis_facility_site_id)
  point.sums[[i]] <- point.sums[[i]][,c("fips","facility_site_name", "eis_facility_site_id", "pollutant_cd", "total_emissions")]
  point.sums[[i]] <- spread(point.sums[[i]], key = "pollutant_cd", value = "total_emissions")
}


## Tall stacks ####

# read updated tall stack key (includes which power plants have closed since 2011)
setwd(NEI.wd)
tall1.revised <- read.dta13("Tall1_List_AP2_V_2011.dta", convert.dates = TRUE, convert.factors = TRUE,
                            missing.type = FALSE, convert.underscore = FALSE)
tall2.revised <- read.dta13("Tall2_List_AP2_V_2011_Update.dta", convert.dates = TRUE, convert.factors = TRUE,
                            missing.type = FALSE, convert.underscore = FALSE)

# formatting for tall stacks key
names(tall2.revised)[names(tall2.revised) == "fips2"] <- "fips"
names(tall2.revised)[names(tall2.revised) == "Row"] <- "row"
names(tall2.revised)[names(tall2.revised) == "eisidentifier"] <- "eis"

# Correct an eis transcription error
tall1.revised[tall1.revised$sitename2011== "Allegheny Energy Supply Co/Armstrong Power Sta", "eis"] <- rep(3866811, 2)

# merge data from 2008, 2011, and 2014 
points.merged <- merge(merge(point.sums[[1]], point.sums[[2]], by=c("fips", "eis_facility_site_id"), all=T), 
                       point.sums[[3]], by=c("fips", "eis_facility_site_id"), all=T)

points.merged <- points.merged[,c("fips", "eis_facility_site_id", 
                                  "facility_site_name.x", "facility_site_name.y", "facility_site_name",
                                  "NH3.x", "NOX.x", "PM25-PRI.x", "SO2.x",  "VOC.x",
                                  "NH3.y", "NOX.y", "PM25-PRI.y", "SO2.y",  "VOC.y",
                                  "NH3", "NOX", "PM25-PRI", "SO2",  "VOC")]


# convert names (pollutants MUST be in correct order)
pollutants <- c("NH3", "NOX", "PM25", "SO2", "VOC")
names(points.merged) <- c("fips", "eis", paste("facility_name", years, sep="_"), paste(rep(pollutants,3), rep(years, each=5), sep="_"))

# merge with tall stack lists
tall1.revised <- merge(tall1.revised, points.merged, by = c("fips", "eis"), all.x = T)
tall2.revised <- merge(tall2.revised, points.merged, by = c("fips", "eis"), all.x = T)

# activity flags
tall1.revised$active.2008 <- !is.na(tall1.revised$facility_name_2008)
tall2.revised$active.2008 <- !is.na(tall2.revised$facility_name_2008)

tall1.revised$active.2011 <- !is.na(tall1.revised$facility_name_2011)
tall2.revised$active.2011 <- !is.na(tall2.revised$facility_name_2011)

tall1.revised$active.2014 <- !is.na(tall1.revised$facility_name_2014)
tall2.revised$active.2014 <- !is.na(tall2.revised$facility_name_2014)

# manual edit for Salem Harbor
tall1.revised[tall1.revised$sitename2011 == "Salem Harbor Station", "active.2014"] <- TRUE

# function to allocate emissions by stack (square used to divide equally across stacks)
allocate.stacks <- function(x, sector=F){
  if (sector){
    x <- ddply(x, ~ eis + fips, transform, 
               NH3_stack = sum(NH3, na.rm=T) / length(eis)^2,
               NOX_stack = sum(NOX, na.rm=T) / length(eis)^2,
               PM25_stack = sum(PM25, na.rm=T) / length(eis)^2,
               SO2_stack = sum(SO2, na.rm=T) / length(eis)^2,
               VOC_stack = sum(VOC, na.rm=T) / length(eis)^2)
  } else {
    x <- ddply(x, ~ eis + fips, transform, NH3_stack_2008 = sum(NH3_2008, na.rm=T) / length(eis)^2,
               NOX_stack_2008 = sum(NOX_2008, na.rm=T) / length(eis)^2,
               PM25_stack_2008 = sum(PM25_2008, na.rm=T) / length(eis)^2,
               SO2_stack_2008 = sum(SO2_2008, na.rm=T) / length(eis)^2,
               VOC_stack_2008 = sum(VOC_2008, na.rm=T) / length(eis)^2,
               NH3_stack_2011 = sum(NH3_2011, na.rm=T) / length(eis)^2,
               NOX_stack_2011 = sum(NOX_2011, na.rm=T) / length(eis)^2,
               PM25_stack_2011 = sum(PM25_2011, na.rm=T) / length(eis)^2,
               SO2_stack_2011 = sum(SO2_2011, na.rm=T) / length(eis)^2,
               VOC_stack_2011 = sum(VOC_2011, na.rm=T) / length(eis)^2,
               NH3_stack_2014 = sum(NH3_2014, na.rm=T) / length(eis)^2,
               NOX_stack_2014 = sum(NOX_2014, na.rm=T) / length(eis)^2,
               PM25_stack_2014 = sum(PM25_2014, na.rm=T) / length(eis)^2,
               SO2_stack_2014 = sum(SO2_2014, na.rm=T) / length(eis)^2,
               VOC_stack_2014 = sum(VOC_2014, na.rm=T) / length(eis)^2)
  }
  return(x)
}

tall1.revised <- allocate.stacks(tall1.revised)              
tall2.revised <- allocate.stacks(tall2.revised)              

# verify allocation
# head(test[c("fips", "eis", "orispl", "nh3", "NH3_2008", "NH3_stack_2008", "NH3_2011", "NH3_stack_2011")])

## Missing & duplicate tall stacks ####

# load eGrid data to fill in the gaps for 2 missing plants
setwd(NEI.wd)
eGrid <- read.xlsx("eGRID2014_Data_v2.xlsx", sheet = 4, startRow = 2)

# # Relevant columns:
# # PNAME = plant name
# # ORISPL
# # FIPSST + FIPSCNTY = fips code
# # PSTATABB = state abbreviation
# # PLNGENAN = generation (MWh)
# # PLNOXAN = annual NOx (tons)
# # PLSO2AN = annual SO2 (tons)
# # PLCO2AN = annual CO2 (tons)

# Salem Harbor (EIS 5090811) for 2014
tall1.revised[grepl("Salem", tall1.revised$sitename2011), "NOX_stack_2014"] <- eGrid[grepl("Salem Harbor", eGrid$PNAME, ignore.case=T), "PLNOXAN" ]
tall1.revised[grepl("Salem", tall1.revised$sitename2011), "SO2_stack_2014"] <- eGrid[grepl("Salem Harbor", eGrid$PNAME, ignore.case=T), "PLSO2AN" ]

# R. E. Burger (divide equally among stacks) for 2014
burger.stacks <- sum(grepl("Burger", tall1.revised$sitename2011))
tall1.revised[grepl("Burger", tall1.revised$sitename2011), "NOX_stack_2014"] <- eGrid[grepl("Burger", eGrid$PNAME, ignore.case=T), "PLNOXAN" ] / burger.stacks
tall1.revised[grepl("Burger", tall1.revised$sitename2011), "SO2_stack_2014"] <- eGrid[grepl("Burger", eGrid$PNAME, ignore.case=T), "PLSO2AN" ] / burger.stacks

# added emissions from eGrid
SO2.added <- eGrid[grepl("Salem Harbor", eGrid$PNAME, ignore.case=T), "PLSO2AN" ] + eGrid[grepl("Burger", eGrid$PNAME, ignore.case=T), "PLSO2AN" ]
NOX.added <- eGrid[grepl("Salem Harbor", eGrid$PNAME, ignore.case=T), "PLNOXAN" ] + eGrid[grepl("Burger", eGrid$PNAME, ignore.case=T), "PLNOXAN" ]

# FIPS R.E.Burger 39013, Salem Harbor 25009
Added.Salem <- c(eGrid[grepl("Salem Harbor", eGrid$PNAME, ignore.case=T), "PLSO2AN" ], eGrid[grepl("Salem Harbor", eGrid$PNAME, ignore.case=T), "PLNOXAN" ])
Added.Burger <- c(eGrid[grepl("Burger", eGrid$PNAME, ignore.case=T), "PLSO2AN" ], eGrid[grepl("Burger", eGrid$PNAME, ignore.case=T), "PLNOXAN" ])
names(Added.Salem) <- c("SO2", "NOX"); names(Added.Burger) <- c("SO2", "NOX")

rm(eGrid); rm(burger.stacks)

# zero out two plants that are double counted across the two lists
# eis.list[duplicated(eis.list)]
vars <- paste(rep(c("NH3_stack", "NOX_stack", "PM25_stack", "SO2_stack", "VOC_stack"), 3), rep(years, each=5), sep="_")

## Nevada Reid

# tall1.revised[tall1.revised$eis == 6815611 & !is.na(tall1.revised$eis),]
# tall2.revised[tall2.revised$eis == 6815611 & !is.na(tall2.revised$eis),]
tall2.revised[tall2.revised$eis == 6815611 & !is.na(tall2.revised$eis), vars] <- 0

## Indiantown Cogeneration

# tall1.revised[tall1.revised$eis == 717611 & !is.na(tall1.revised$eis),]
# tall2.revised[tall2.revised$eis == 717611 & !is.na(tall2.revised$eis),]
tall2.revised[tall2.revised$eis == 717611 & !is.na(tall2.revised$eis), vars] <- 0

## Spurlock Station

# tall1.revised[tall1.revised$eis == 7335511 & !is.na(tall1.revised$eis),]
# tall2.revised[tall2.revised$eis == 7335511 & !is.na(tall2.revised$eis),]
tall2.revised[tall2.revised$eis == 7335511 & !is.na(tall2.revised$eis), vars] <- 0

## Warrick power plant

# tall1.revised[tall1.revised$eis == 8183111 & !is.na(tall1.revised$eis),]
# tall2.revised[tall2.revised$eis == 8183111 & !is.na(tall2.revised$eis),]
tall2.revised[tall2.revised$eis == 8183111 & !is.na(tall2.revised$eis), vars] <- 0

## Save tall stacks to csv ####

# enforce order
tall1.revised$row <- as.numeric(tall1.revised$row); tall2.revised$row <- as.numeric(tall2.revised$row)
tall1.revised <- tall1.revised[order(tall1.revised$row),]
tall2.revised <- tall2.revised[order(tall2.revised$row),]

tall1.revised$PM10 <- 0
tall1.revised$VOC_B <- 0
tall2.revised$PM10 <- 0
tall2.revised$VOC_B <- 0

saveTall <- function(tall, year, newTall=FALSE){
  
  tallSave <- tall[,c(paste(pollutants, "stack", year, sep="_"), "PM10", "VOC_B")]
  tallSave <- tallSave[,c(paste("NH3_stack", year, sep="_"),
                              paste("NOX_stack", year, sep="_"),
                              "PM10",
                              paste("PM25_stack", year, sep="_"),
                              paste("SO2_stack", year, sep="_"),
                              paste("VOC_stack", year, sep="_"),
                              "VOC_B")]
  colnames(tallSave) <- NULL
  setwd(paste(NEI.wd, "AP2 inputs", sep="/"))
  if(newTall){
    write.csv(tallSave, paste(year, "- tall 2 stacks.csv"), row.names=F)
    print(paste("Saving tall 2 stacks", year))
  } else{
    write.csv(tallSave, paste(year, "- tall stacks.csv"), row.names=F)
    print(paste("Saving tall stacks", year))
  }
  
  # data for tall stack sparse matrix (needed for AP2 source-receptor matrix)
  setwd(paste(NEI.wd, "AP2 inputs", "Sparse matrices", year, sep="/"))
  if (newTall){
    write.csv(tallSave, "New_Tall_Stack_4.csv", row.names=F)
  } else {
    write.csv(tallSave, "Tall_Stack_4.csv", row.names=F)
  }
  
}

if(save){
  for(i in 1:length(years)){
    saveTall(tall1.revised, year=years[i])
    saveTall(tall2.revised, year=years[i], newTall=T)
  }
}

rm(points.merged)

## Save tall stacks for sparse matrices ####
if(save){
  setwd(NEI.wd)
  source("Tall stack sparse matrix.R")
  # Note: emissions only; see code if S-R needs updating
}

## Medium and low point sources ####
# First subtract out tall stack emissions

# add tall and new tall
all.tall.revised <- rbind(tall1.revised[,c("eis", "fips", vars)], tall2.revised[,c("eis", "fips", vars)])

# sum emissions from tall stacks by fips and rename
tall.sums <- as.data.frame(t(sapply(split(all.tall.revised[,c(vars)], all.tall.revised$fips), colSums)))
names(tall.sums) <- gsub("stack", "tall", names(tall.sums))
tall.sums$fips <- rownames(tall.sums)
rm(all.tall.revised)

# separate into lists
tall.sums.list <- list() 
for(i in 1:length(years)){
  tall.sums.list[[i]] <- tall.sums[,c("fips", paste(pollutants, "tall", years[i], sep="_"))]
  names(tall.sums.list[[i]]) <- gsub(paste("_", years[i], sep=""), "", names(tall.sums.list[[i]]))
}

# sum by fips code
tall.vars <- c("NH3_tall", "NOX_tall", "PM25_tall", "SO2_tall", "VOC_tall")
for(i in 1:length(years)){
  names(point.sums[[i]])[names(point.sums[[i]]) == "PM25-PRI"] <- "PM25"
  
  # sum point sources by fips
  point.sums[[i]] <- ddply(point.sums[[i]], ~ fips, summarize, NOX = sum(NOX, na.rm=T),
                           PM25 = sum(PM25, na.rm=T),
                           SO2 = sum(SO2, na.rm=T),
                           VOC = sum(VOC, na.rm=T),
                           NH3 = sum(NH3, na.rm=T))
  
  # merge with total point source sums
  point.sums[[i]] <- merge(point.sums[[i]], tall.sums.list[[i]], by="fips", all=T)
  
  # convert tall stack NAs to zeros
  point.sums[[i]][is.na(point.sums[[i]]$NOX_tall), tall.vars] <- 0
  
  # subtract tall stack amounts
  point.sums[[i]]$NH3_nontall <- point.sums[[i]]$NH3 - point.sums[[i]]$NH3_tall
  point.sums[[i]]$NOX_nontall <- point.sums[[i]]$NOX - point.sums[[i]]$NOX_tall
  point.sums[[i]]$PM25_nontall <- point.sums[[i]]$PM25 - point.sums[[i]]$PM25_tall
  point.sums[[i]]$SO2_nontall <- point.sums[[i]]$SO2 - point.sums[[i]]$SO2_tall
  point.sums[[i]]$VOC_nontall <- point.sums[[i]]$VOC - point.sums[[i]]$VOC_tall
  
}

# add back values for R.E. Burger and Salem (FIPS R.E.Burger 39013, Salem Harbor 25009)
point.sums[[3]][point.sums[[3]]$fips == 39013 & !is.na(point.sums[[3]]$fips), "NOX_nontall"] <- Added.Burger["NOX"] +
  point.sums[[3]][point.sums[[3]]$fips == 39013 & !is.na(point.sums[[3]]$fips), "NOX_nontall"]

point.sums[[3]][point.sums[[3]]$fips == 39013 & !is.na(point.sums[[3]]$fips), "SO2_nontall"] <- Added.Burger["SO2"] +
  point.sums[[3]][point.sums[[3]]$fips == 39013 & !is.na(point.sums[[3]]$fips), "SO2_nontall"]

point.sums[[3]][point.sums[[3]]$fips == 25009 & !is.na(point.sums[[3]]$fips), "NOX_nontall"] <- Added.Salem["NOX"] +
  point.sums[[3]][point.sums[[3]]$fips == 25009 & !is.na(point.sums[[3]]$fips), "NOX_nontall"]

point.sums[[3]][point.sums[[3]]$fips == 25009 & !is.na(point.sums[[3]]$fips), "SO2_nontall"] <- Added.Salem["SO2"] +
  point.sums[[3]][point.sums[[3]]$fips == 25009 & !is.na(point.sums[[3]]$fips), "SO2_nontall"]

point.sums[[3]][point.sums[[3]]$fips == 39013 & !is.na(point.sums[[3]]$fips), "NOX"] <- Added.Burger["NOX"] +
  point.sums[[3]][point.sums[[3]]$fips == 39013 & !is.na(point.sums[[3]]$fips), "NOX"]

point.sums[[3]][point.sums[[3]]$fips == 39013 & !is.na(point.sums[[3]]$fips), "SO2"] <- Added.Burger["SO2"] +
  point.sums[[3]][point.sums[[3]]$fips == 39013 & !is.na(point.sums[[3]]$fips), "SO2"]

point.sums[[3]][point.sums[[3]]$fips == 25009 & !is.na(point.sums[[3]]$fips), "NOX"] <- Added.Salem["NOX"] +
  point.sums[[3]][point.sums[[3]]$fips == 25009 & !is.na(point.sums[[3]]$fips), "NOX"]

point.sums[[3]][point.sums[[3]]$fips == 25009 & !is.na(point.sums[[3]]$fips), "SO2"] <- Added.Salem["SO2"] +
  point.sums[[3]][point.sums[[3]]$fips == 25009 & !is.na(point.sums[[3]]$fips), "SO2"]


rm(tall.vars); rm(Added.Salem); rm(Added.Burger)

## Allocate to medium and low based on 2008 breakdown

# read AP2 low and medium stack emissions
setwd(NEI.wd)
low.emissions <- read.csv("Low stack emissions 2008.csv", header = FALSE)
med.emissions <- read.csv("Medium stack emissions 2008.csv", header = FALSE)

# calculate sum of low and medium
sum.low.med <- low.emissions + med.emissions

# calculate percentage shares
low.share <- low.emissions / sum.low.med 
med.share <- med.emissions / sum.low.med 

# national average of share by pollutant in case of missing values 
# (emissions may be zero in 2008, but should not necessarily be zeroed out in future years)
nat.average.low <- colSums(low.emissions) / colSums(sum.low.med)
nat.average.med <- colSums(med.emissions) / colSums(sum.low.med)

for(i in c("V1", "V2", "V3", "V4", "V5", "V6", "V7")){
  low.share[,i] <- ifelse(is.na(low.share[,i]), nat.average.low[i], low.share[,i] )
  med.share[,i] <- ifelse(is.na(med.share[,i]), nat.average.med[i], med.share[,i] )
}

# check that fractions sum to 1
test.sums <- low.share + med.share

test.sums[,c("V3", "V7")] <- 0
sum(complete.cases(test.sums)) # 3109

test.sums <- test.sums[complete.cases(test.sums),]
indx <- matrix(which(test.sums - 1 > 0.005, arr.ind=T), ncol=2)
test.sums[indx[,1],]

rm(indx); rm(test.sums); rm(nat.average.low); rm(nat.average.med)
rm(low.emissions); rm(med.emissions); rm(sum.low.med)

names(low.share) <- c("NH3", "NOX", "PM10", "PM25", "SO2", "VOC", "VOC_B")

# merge and deal with missing values
for(i in 1:length(years)){
  # save Miami-Dade
  point.sums[[i]][point.sums[[i]]$fips == 12086 & !is.na(point.sums[[i]]$fips), "fips"] <- 12025
  # subset to fips and order, then allocate to stacks
  point.sums[[i]] <- merge(data.frame(fips = fips), point.sums[[i]], by="fips", all.x=T)
  # NAs represent counties with no point source emissions - set to zero
  point.sums[[i]][is.na(point.sums[[i]]$NOX), -1] <- 0
}

# match nontall emissions to low/med shares
for(i in 1:length(years)){
  # check that fips order matches
  if(identical(point.sums[[i]]$fips, fips)){
    for(poll in c("NH3", "NOX", "PM25", "SO2", "VOC")){
      point.sums[[i]][,paste(poll, "low", sep="_")] <- point.sums[[i]][,paste(poll, "nontall", sep="_")] * low.share[,poll]
      point.sums[[i]][,paste(poll, "med", sep="_")] <- point.sums[[i]][,paste(poll, "nontall", sep="_")] - point.sums[[i]][,paste(poll, "low", sep="_")]
    }
  } else {
    print(paste("Check fips order for", years[i]))
  }
}


## Save med/low point to csv ####

if(save){
  setwd(paste(NEI.wd, "AP2 inputs", sep="/"))
  for(i in 1:length(years)){
    point.sums[[i]]$PM10 <- 0
    point.sums[[i]]$VOC_B <- 0
    # low
    low.save <- point.sums[[i]][, c("NH3_low", "NOX_low", "PM10", "PM25_low", "SO2_low", "VOC_low", "VOC_B")]
    colnames(low.save) <- NULL
    #write.csv(low.save, paste(years[i], "- low stacks.csv"), row.names=F)
    print(paste("Saving low stacks", years[i]))
    # medium
    med.save <- point.sums[[i]][, c("NH3_med", "NOX_med", "PM10", "PM25_med", "SO2_med", "VOC_med", "VOC_B")]
    colnames(med.save) <- NULL
    #write.csv(med.save, paste(years[i], "- medium stacks.csv"), row.names=F)
    print(paste("Saving med stacks", years[i]))
    rm(low.save); rm(med.save);
  }
}

# some house cleaning
rm(tall.sums); rm(tall.sums.list)

## Emissions for sectoral analysis ####

mapSectors <- function(sectorValues, scc){
  newSectorValues <- mapvalues(sectorValues,
                               from = levels(sort(unique(scc$sector.text))),
                               to = c(rep("Agriculture", 3), "Biogenics", "Transportation - infrastructure", "Commercial cooking", rep("Dust", 3),
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
  return(newSectorValues)
}
# function to sum by scc sector
scc.grouping <- function(data, state=F,facility=F,county=F){
  
  # subset
  data <- data[data$state_and_county_fips_code %in% fips48,]
  
  # sum by sector
  if(state){
    scc.sums <- ddply(data, ~ st_usps_cd + sector.text + pollutant_cd, summarize, emissions = sum(total_emissions, na.rm=T))
  } else if (facility) {
    scc.sums <- ddply(data, ~ eis_facility_site_id + state_and_county_fips_code + sector + pollutant_cd, summarize, emissions = sum(total_emissions, na.rm=T))
  } else if (county){
    scc.sums <- ddply(data, ~  state_and_county_fips_code + sector.text + pollutant_cd, summarize, emissions = sum(total_emissions, na.rm=T))
  } else {
    scc.sums <- ddply(data, ~ sector.text + pollutant_cd, summarize, emissions = sum(total_emissions, na.rm=T))
  }
  
  # reshape to wide
  scc.sums <- spread(scc.sums, key="pollutant_cd", value="emissions")
  
  # replace NAs with zeros
  scc.sums[is.na(scc.sums)] <- 0
  
  return(scc.sums)
}
# fips codes in original data that are within CONUS 
fips48 <- subset(area.sources[[1]], select = state_and_county_fips_code, 
                 st_usps_cd %in% c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", 
                                   "MD", "DC", "DE", "PA", "WV", "VA", "NC", "SC", 
                                   "GA", "FL", "AL", "MS", "TN", "KY", "LA", "AR", 
                                   "TX", "OK", "NM", "OH", "IN", "IL", "WI", "MI", 
                                   "MN", "KS", "MO", "IA", "NE", "ND", "SD", "CO", 
                                   "UT", "WY", "MT", "AZ", "NV", "CA", "WA", "OR", "ID"))

fips48 <- sort(unique(fips48$state_and_county_fips_code))

# process by scc code - map to sector.text
for(i in 1:length(years)){
  process[[i]] <- merge(process[[i]], scc[,c("Code", "sector.text", "scc.level.one.text")], by.x = "scc", by.y = "Code", all.x=T)
  }

## 1. tall stacks sources
tallStackCleaning <- function(tall, facilitySectorSub, pollutants){
  
  # merge with tall lists (Note: spot check confirms total emissions as upper bound on sector, but check more rigorously)
  tall.Sector <- merge(tall[,c("fips", "eis", "row")], 
                       facilitySectorSub, 
                       by.x = c("fips", "eis"), by.y = c("fips", "eis_facility_site_id"), all.x = T)
  
  # allocate emissions across stacks
  tall.Sector <- allocate.stacks(tall.Sector, sector=T)              
  
  # sort and return
  tall.Sector$row <- as.numeric(tall.Sector$row)
  tall.Sector <- tall.Sector[order(tall.Sector$row),]
  
  # PM10 and VOC_B
  tall.Sector$PM10 <- 0
  tall.Sector$VOC_B <- 0
  
  # add stack labels
  tall.Sector <- tall.Sector[,c("fips", "eis", paste(pollutants, "stack", sep="_"), "PM10", "VOC_B")]
  # reorder
  tall.Sector <- tall.Sector[,c("fips", "eis", "NH3_stack","NOX_stack","PM10","PM25_stack","SO2_stack","VOC_stack","VOC_B")]
  
  return(tall.Sector)
  
}
processSectorSummary <- function(processSub, sectorName, tall1.revised, tall2.revised, pollutants){
  
  processSub$sector <- mapSectors(processSub$sector.text, scc)
  
  # summarize emissions by facility and scc grouped sector (this is very slow...)
  # returns in wide format
  facilitySector <- scc.grouping(processSub, facility=T)
  
  # select sector (need to account for the fact that some sectors will be zero for point)
  facilitySectorSub <- facilitySector[facilitySector$sector == sectorName,]
  
  # if there are no emissions from that sector, return empty data frame 
  if (nrow(facilitySectorSub) == 0){
    print(paste(sectorName, "- no point source emissions"))
    facilitySectorSub <- data.frame(fips=fips.stacks[fips.stacks$stack == "area","fips"], NH3=0, NOX=0, PM10=0, PM25=0, SO2=0, VOC=0, VOC_B=0)     
    tall1 <- data.frame(fips=fips.stacks[fips.stacks$stack == "tall","fips"], NH3_stack=0, NOX_stack=0, PM10=0, PM25_stack=0, SO2_stack=0, VOC_stack=0, VOC_B=0) 
    tall2 <- data.frame(fips=fips.stacks[fips.stacks$stack == "tall2","fips"], NH3_stack=0, NOX_stack=0, PM10=0, PM25_stack=0, SO2_stack=0, VOC_stack=0, VOC_B=0) 
    return(list(facilitySectorSub, tall1, tall2))
  }
  
  colnames(facilitySectorSub)[colnames(facilitySectorSub) == "PM25-PRI"] <- "PM25"
  colnames(facilitySectorSub)[colnames(facilitySectorSub) == "state_and_county_fips_code"] <- "fips"
  
  tall1 <- tallStackCleaning(tall1.revised, facilitySectorSub, pollutants)
  tall2 <- tallStackCleaning(tall2.revised, facilitySectorSub, pollutants)
  
  # Zero out double counted plants
  tall2[tall2$eis == 6815611 & !is.na(tall2$eis),] <- 0     ## Nevada Reid
  tall2[tall2$eis == 717611 & !is.na(tall2$eis), ] <- 0     ## Indiantown Cogeneration
  tall2[tall2$eis == 7335511 & !is.na(tall2$eis),] <- 0     ## Spurlock Station
  tall2[tall2$eis == 8183111 & !is.na(tall2$eis),] <- 0     ## Warrick power plant
  
  return(list(facilitySectorSub, tall1, tall2))
  
}
## 2. Low and medium stacks
medAndLowStacks <- function(point, tall1, tall2, lowShares){
  
  # assign Broomfield county to Boulder
  point[point$fips == 8014 & !is.na(point$fips), "fips"] <- 8013
  
  nontallSum <- ddply(point, ~fips, summarize, NH3 = sum(NH3, na.rm=T),
                                               NOX = sum(NOX, na.rm=T),
                                               PM25 = sum(PM25, na.rm=T),
                                               SO2 = sum(SO2, na.rm=T),
                                               VOC = sum(VOC, na.rm=T))
  
  # save Miami-Dade
  nontallSum[nontallSum$fips == 12086 & !is.na(nontallSum$fips), "fips"] <- 12025
  
  #tall1 <- cbind(fips.stacks[fips.stacks$stack == "tall",], tall1)
  #tall2 <- cbind(fips.stacks[fips.stacks$stack == "tall2",], tall2)
  
  tallCombined <- rbind(tall1, tall2)
  
  tallSum <- ddply(tallCombined, ~fips, summarize, NH3_tall = sum(NH3_stack, na.rm=T),
                   NOX_tall = sum(NOX_stack, na.rm=T),
                   PM25_tall = sum(PM25_stack, na.rm=T),
                   SO2_tall = sum(SO2_stack, na.rm=T),
                   VOC_tall = sum(VOC_stack, na.rm=T))
  
  
  nontallSum <- merge(nontallSum, tallSum, by="fips", all.x=T)
  
  # NAs represent counties with no tall stack emissions - set to zero
  nontallSum[is.na(nontallSum)] <- 0
  
  # calculate non tall
  for (pollutant in c("NH3", "NOX", "PM25", "SO2", "VOC")){
    newCol <- paste(pollutant, "nontall", sep="_")
    oldCol <- paste(pollutant, "tall", sep="_")
    nontallSum[,newCol] <- nontallSum[,pollutant] - nontallSum[,oldCol]
    
    # round negatives that occur from floating point imprecision
    nontallSum[,newCol] <- ifelse(abs(nontallSum[,newCol]) < 0.0001, 0, nontallSum[,newCol])
  }
  
  # expand to spare matrix
  nontallSum <- merge(data.frame(fips=fips.stacks[fips.stacks$stack == "area", "fips"]), nontallSum, by="fips", all=T)
  
  # NAs represent counties with no point source emissions - set to zero
  nontallSum[is.na(nontallSum)] <- 0
  
  if(identical(nontallSum$fips, fips.stacks[fips.stacks$stack == "area", "fips"])){
    
    for (pollutant in c("NH3", "NOX", "PM25", "SO2", "VOC")){
      lowCol <- paste(pollutant, "low", sep="_")
      medCol <- paste(pollutant, "med", sep="_")
      nonTallCol <- paste(pollutant, "nontall", sep="_")
      
      nontallSum[,lowCol] <- nontallSum[,nonTallCol]  * lowShares[,pollutant]
      nontallSum[,medCol] <- nontallSum[,nonTallCol]  - nontallSum[,lowCol] 
    }
  } else {
    print("Check fips order")
  }
  
  return(nontallSum)
}
## 3. Area sources
areaSectorEmissions <- function(sectorName, areaYear){
  
  # assign Broomfield county to Boulder
  areaYear[areaYear$state_and_county_fips_code == 8014 & !is.na(areaYear$state_and_county_fips_code), "state_and_county_fips_code"] <- 8013
  # save Miami Dade
  areaYear[areaYear$state_and_county_fips_code == 12086 & !is.na(areaYear$state_and_county_fips_code), "state_and_county_fips_code"] <- 12025
  
  # map to larger sector names
  areaYear$sector <- mapSectors(areaYear$sector.text, scc)
  
  # summarize emissions by facility and scc grouped sector (this is very slow...)
  # returns in wide format
  areaYear <- scc.grouping(areaYear, county=T)
  
  # select sector (need to account for the fact that some sectors will be zero for point)
  areaYear <- areaYear[areaYear$sector == sectorName,]
  
  # if there are no emissions from that sector, return empty data frame
  if (nrow(areaYear) == 0){
    print(paste(sectorName, "- no area source emissions"))
    return(data.frame(fips=fips, NH3=0, NOX=0, PM10=0, PM25=0, SO2=0, VOC=0, VOC_B=0))
  }
  
  colnames(areaYear)[colnames(areaYear) == "state_and_county_fips_code"] <- "fips"
  colnames(areaYear)[colnames(areaYear) == "PM25-PRI"] <- "PM25"
  areaYear$PM10 <- 0; areaYear$VOC_B <- 0                                                   # assume VOC_B = 0 since no only selecting anthropogenic sources
  areaYear <- areaYear[,c("fips", "NH3", "NOX", "PM10", "PM25", "SO2", "VOC", "VOC_B")]
  
  # expand to sparse matrix
  areaSum <- merge(data.frame(fips=fips), areaYear, by="fips", all=T)
  
  # NAs represent counties with no point source emissions - set to zero
  areaSum[is.na(areaSum)] <- 0
  
  if(identical(areaSum$fips, fips)){
    return(areaSum)
  } else {
    print("Check fips order")
  }
}
# super sector function
sectorEmissions <- function(sectorName, year, pointSources, areaSources, tall1.revised, tall2.revised, pollutants, low.share){
  #start <- proc.time()
  print(paste(sectorName, year))
  if (year == 2008){
    yearIndex <- 1
  } else if (year == 2014){
    yearIndex <- 3
  }

  area <- areaSectorEmissions(sectorName, areaSources[[yearIndex]])
  
  # make sure to have merged SCC codes prior (see "process by scc code - map to sector.text")
  # may also need to update mapSectors, scc.grouping, and allocate.stacks functions
  pointTall <- processSectorSummary(pointSources[[yearIndex]], sectorName, tall1.revised, tall2.revised, pollutants)
  nonTallStacks <- medAndLowStacks(pointTall[[1]], pointTall[[2]], pointTall[[3]], low.share)
  
  nonTallStacks$PM10 <- 0; nonTallStacks$VOC_B <- 0                                                   # assume VOC_B = 0 since no only selecting anthropogenic sources
  
  setwd(paste(NEI.wd, "AP2 inputs", "Sector emissions", sectorName, sep="/"))
  write.table(area[,-1], paste(year, "- area sources.csv"), row.names=F, col.names=F, sep=",")
  write.table(nonTallStacks[,c("NH3_low",  "NOX_low", "PM10", "PM25_low", "SO2_low",  "VOC_low", "VOC_B")], 
              paste(year, "- low stacks.csv"), row.names=F, col.names=F, sep=",")
  write.table(nonTallStacks[,c("NH3_med",  "NOX_med", "PM10", "PM25_med", "SO2_med",  "VOC_med", "VOC_B")], 
              paste(year, "- medium stacks.csv"), row.names=F, col.names=F, sep=",")
  write.table(pointTall[[2]][,c("NH3_stack",  "NOX_stack", "PM10", "PM25_stack", "SO2_stack",  "VOC_stack", "VOC_B")], 
              paste(year, "- tall stacks.csv"), row.names=F, col.names=F, sep=",")
  write.table(pointTall[[3]][,c("NH3_stack",  "NOX_stack", "PM10", "PM25_stack", "SO2_stack",  "VOC_stack", "VOC_B")], 
              paste(year, "- tall 2 stacks.csv"), row.names=F, col.names=F, sep=",")
  
  # sparse matrices
  fipsT1 <- fips.stacks[fips.stacks$stack == "tall", "fips"]
  fipsT2 <- fips.stacks[fips.stacks$stack == "tall2", "fips"] 
  
  # import from Tall stack sparse matrix.R
  read.stacks(new.flag = FALSE, tall = fipsT1, emissions.only=T, name=paste(year, "- tall stacks.csv"))
  read.stacks(new.flag = TRUE, tall = fipsT2, emissions.only=T, name=paste(year, "- tall 2 stacks.csv"))
  
  #elapsed <- proc.time() - start; print(elapsed)
  return(list(area, nonTallStacks, pointTall[[2]], pointTall[[3]]))
}

sectors <- c("Electricity generation", "Transportation - Heavy duty vehicles", "Transportation - Light duty vehicles", 
             "Industrial processes - Oil & gas production", "Industrial boilers")
sectorEmissionsResults <- list(list())
for (sector in sectors){
  for (year in c(2008,2014)){
    sectorEmissionsResults[[sector]][[as.character(year)]] <- sectorEmissions(sector, year=year, process, area.sources, 
                                                                tall1.revised, tall2.revised, pollutants, low.share)
  }
}

## Final save ####
if(save){
  setwd(NEI.wd) 
  save.image("NEI emissions data.RData")
}

## Side analysis - Florida ###

fl2008 <- area.sums[[1]][area.sums[[1]]$fips >= 12000 & area.sums[[1]]$fips < 13000, c("fips", "NOX", "PM25-PRI", "SO2", "NH3", "VOC")]
fl2014 <- area.sums[[3]][area.sums[[3]]$fips >= 12000 & area.sums[[3]]$fips < 13000, c("fips", "NOX", "PM25-PRI", "SO2", "NH3", "VOC")]
fl <- merge(fl2008, fl2014, by="fips", all=T)

flPercent <- round((fl[paste0(c("NOX", "PM25-PRI", "SO2", "NH3", "VOC"), ".y")] - fl[paste0(c("NOX", "PM25-PRI", "SO2", "NH3", "VOC"), ".x")]) /
                                                  fl[paste0(c("NOX", "PM25-PRI", "SO2", "NH3", "VOC"), ".x")] * 100)

flPercent <- cbind(fips[fips >= 12000 & fips < 13000], flPercent)

fl2008 <- point[[1]][point[[1]]$fips %in% c(12059, 12091, 12119, 12131),]
fl2014 <- point[[3]][point[[3]]$fips %in% c(12059, 12091, 12119, 12131),]
colnames(fl2008)[colnames(fl2008) == "total_emissions"] <- "emissions_2008"
colnames(fl2014)[colnames(fl2014) == "total_emissions"] <- "emissions_2014"
flVars <- c("eis_facility_site_id", "fips", "facility_site_name", "naics_cd", "pollutant_cd")
flCombined <- merge(fl2008[, c(flVars, "emissions_2008")], fl2014[, c(flVars, "emissions_2014")], all=T)
flCombined$percentChange <- round((flCombined$emissions_2014 - flCombined$emissions_2008) / flCombined$emissions_2008 * 100)

flCombinedSub <- flCombined[flCombined$fips==12119,]
flCombinedSub[order(flCombinedSub$emissions_2014, decreasing=T),]
