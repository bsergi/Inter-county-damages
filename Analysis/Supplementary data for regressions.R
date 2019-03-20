## Supplementary data for regressions ####

require(readstata13)
library(np)
library(sfsmisc)
library(lme4)
library(stargazer)
library(plyr)
# library(xlsx)
library(readxl)
library(openxlsx)
library(reshape2)
library(zoo)
library(tidyr)
library(caret)

# mapping packages
library(maps)
library(mapproj)
library(ggplot2)
library(gridExtra)

baseWD <- "~/Documents/Carnegie Mellon/Box Sync/Research/Environmental justice"     # home directory (use for relative paths)
resultsWD <- c("/Users/Cartographer/Documents/Carnegie Mellon/Box Sync/Research/AP2/APEEP_Web_2008/PM",
               "/Volumes/Seagate Backup Plus Drive/AP2/APEEP_Web_2011/PM",
               "/Volumes/Seagate Backup Plus Drive/AP2/APEEP_Web_2014/PM")

setwd(baseWD)
load("County shares results V2.RData")

# load data for regressions

## 0. Basic panel data set ####

# combined export and import data from all three years
panel <- ldply(shares)

## 1. CEMS Facility data ####
setwd(paste(baseWD, "Other data", "CEMS", "EPADownload", sep="/")) 

# read emissions data
plants <- read.csv("Emissions.csv", row.names=NULL)   
# overwrite first row steam load (missing value was previously was messing up data loading, inserted 0 manually into excel sheet to rectify)
plants[1,"Steam.Load..1000lb."] <- NA

# subset to 2008, 2011, and 2014
plants <- subset(plants, Year %in% years)

# read facility data
facilities <- read.csv("Facilities.csv", row.names=NULL)   
# overwrite first row steam load (missing value was previously was messing up data loading, inserted 0 manually into excel sheet to rectify)
facilities[1,"Associated.Stacks"] <- ""

# subset to 2008, 2011, and 2014
facilities <- subset(facilities, Year %in% years)

# add full fips code
facilities <- merge(facilities, census.fips[,c("state", "state.num", "county.num", "fips")], 
                    by.x = c("State", "FIPS.Code"), by.y = c("state", "county.num"), all.x=T)


# merge on year, facility name, unit, and ORISPL to get fips codes
plants <- merge(plants, facilities[ ,c("Year", "Facility.ID..ORISPL.", "Facility.Name", "Unit.ID", "fips", "Source.Category")], 
                by = c("Year", "Facility.ID..ORISPL.", "Facility.Name", "Unit.ID", "Source.Category"), all.x=T)

# coal and gas generation (MWh)
plants$gen.coal <- ifelse(grepl("Coal", plants$Fuel.Type..Primary., ignore.case =T) & 
                           plants$Source.Category %in% c("Electric Utility", "Cogeneration", "Small Power Producer"), plants$Gross.Load..MW.h., 0)
plants$gen.gas <- ifelse(grepl("Natural Gas", plants$Fuel.Type..Primary., ignore.case =T) & 
                           plants$Source.Category %in% c("Electric Utility", "Cogeneration", "Small Power Producer"), plants$Gross.Load..MW.h., 0)
plants$gen.industrial.boilers <- ifelse(plants$Source.Category %in% c("Industrial Boiler", "Industrial Turbine", "Petroleum Refinery"), plants$Gross.Load..MW.h., 0)

# share of coal generation with SO2 controls (assume plant with controls is using them)
plants$gen.SO2.controls <- ifelse(plants$SO2.Control.s. == "", 0, plants$gen.coal)

# share of coal generation with NOx controls
plants$gen.NOx.controls <- ifelse(plants$NOx.Control.s. == "", 0, plants$gen.coal)

# share of coal generation with PM controls
plants$gen.PM.controls <- ifelse(plants$PM.Control.s. == "", 0, plants$gen.coal) 


# summarize by county
county.plants <- ddply(plants, ~ State + County + fips + Year, summarize, gen.coal = sum(gen.coal, na.rm=T), 
                       gen.SO2.controls = sum(gen.SO2.controls, na.rm=T), 
                       gen.NOx.controls = sum(gen.NOx.controls, na.rm=T), 
                       gen.PM.controls = sum(gen.PM.controls, na.rm=T),
                       gen.gas = sum(gen.gas, na.rm=T),
                       gen.industrial.boilers = sum(gen.industrial.boilers, na.rm=T))

# flag for nonzero coal generation
county.plants$coal.flag <- ifelse(county.plants$gen.coal > 0, TRUE, FALSE)
county.plants$gas.flag <- ifelse(county.plants$gen.gas > 0, TRUE, FALSE)
county.plants$gas.flag <- ifelse(county.plants$gen.gas > 0, TRUE, FALSE)
county.plants$boiler.flag <- ifelse(county.plants$gen.industrial.boilers > 0, TRUE, FALSE)

# lost coal plant variable
county.plants <- ddply(county.plants, ~ State + County + fips, transform, lostCoal=c(0, diff(!coal.flag)))


# merge with panel to get missing fips codes (i.e. counties with no power plants of any type)
county.plants <- merge(county.plants, panel[,c("state", "county.name", "fips", "year")], 
                       by.x = c("State", "fips", "Year"), by.y = c("state", "fips", "year"), all=T)

# remove extra county name variable
county.plants$County <- NULL

# remove NA values
county.plants[is.na(county.plants$county.name) & county.plants$fips == 12086, "county.name"] <- "Miami-Dade"
county.plants[is.na(county.plants)] <- 0

# generation share
county.plants$share.SO2.controls <- ifelse(county.plants$gen.coal > 0, county.plants$gen.SO2.controls / county.plants$gen.coal, 0)
county.plants$share.NOx.controls <- ifelse(county.plants$gen.coal > 0, county.plants$gen.NOx.controls / county.plants$gen.coal, 0)
county.plants$share.PM.controls <- ifelse(county.plants$gen.coal > 0, county.plants$gen.PM.controls / county.plants$gen.coal, 0)

# year == 0 indicates zero across all years (means )
# no.plants <- county.plants[county.plants$Year == 0,]
# no.plants <- rbind(no.plants, no.plants, no.plants)
# no.plants <- no.plants[order(no.plants$fips),]
# no.plants$Year <- rep(c(2008, 2011, 2014), nrow(no.plants) / 3)
# county.plants <- rbind(no.plants, county.plants[county.plants$Year != 0,])

county.plants <- county.plants[order(county.plants$fips),]

# add to panel set
# Note: missing Miami-Dade (12086) for 2008
panel <- merge(panel, county.plants[,c("fips", "Year", "gen.coal", "coal.flag",  "share.SO2.controls", "share.NOx.controls", "share.PM.controls", 
                                       "gen.gas", "gas.flag", "boiler.flag", "lostCoal")], 
               by.x = c("fips", "year"), by.y = c("fips", "Year"), all=T)

## 2. Attainment ####
setwd(paste(baseWD, "Other data", "Attainment", sep="/")) 
attain <- read.xlsx("nayro.xlsx", sheet=1)              # openxlsx

# subset to PM and Ozone standards
attain <- subset(attain, pollutant %in% c("PM-2.5 (1997)", "8-Hour Ozone (1997)", "1-Hour Ozone (1979)"))

# convert to numeric variable
years <- 1992:2016

for(year in years){
  year.col <- paste("yr", year, sep="")
  attain[,year.col] <- as.numeric(attain[,year.col])    # openxlsx
}

# replace year values with binary indicator (0 if labeled as NA, 1 otherwise)
attain[, paste("yr", years, sep="")] <- ifelse(is.na(attain[, paste("yr", years, sep="")]), 0, 1)

# calculate cumulative sum
attain$cumulative_2008 <- rowSums(attain[, paste("yr", 1992:2008, sep="")])
attain$cumulative_2011 <- rowSums(attain[, paste("yr", 1992:2011, sep="")])
attain$cumulative_2014 <- rowSums(attain[, paste("yr", 1992:2014, sep="")])

# current year 
attain$current_2008 <- attain[, "yr2008"]
attain$current_2011 <- attain[, "yr2011"]
attain$current_2014 <- attain[, "yr2014"]

# previous year 
attain$previous_2008 <- attain[, "yr2007"]
attain$previous_2011 <- attain[, "yr2010"]
attain$previous_2014 <- attain[, "yr2013"]

# previous five years
attain$previous_five_2008 <- rowSums(attain[, paste("yr", 2003:2007, sep="")])
attain$previous_five_2011 <- rowSums(attain[, paste("yr", 2006:2010, sep="")])
attain$previous_five_2014 <- rowSums(attain[, paste("yr", 2009:2013, sep="")])

# flipped (+1 = went into attainment, -1 = went out of attainment, 0 = no change)
attain$flipped_2008 <- attain[,"yr2007"] - attain[,"yr2008"]
attain$flipped_2011 <- attain[,"yr2010"] - attain[,"yr2011"]
attain$flipped_2014 <- attain[,"yr2013"] - attain[,"yr2014"]

attain$flipped_three_2008 <- attain[,"yr2005"] - attain[,"yr2008"]
attain$flipped_three_2011 <- attain[,"yr2008"] - attain[,"yr2011"]
attain$flipped_three_2014 <- attain[,"yr2011"] - attain[,"yr2014"]

# names of new variables
vars <- c(paste("cumulative_", c(2008, 2011, 2014), sep=""),
          paste("current_", c(2008, 2011, 2014), sep=""),
          paste("previous_", c(2008, 2011, 2014), sep=""),
          paste("previous_five_", c(2008, 2011, 2014), sep=""),
          paste("flipped_", c(2008, 2011, 2014), sep=""),
          paste("flipped_three_", c(2008, 2011, 2014), sep=""))

# create new column with full fips format (state + county)
attain$fips <- as.numeric(with(attain, paste(fips_state, fips_cnty, sep = "")))

# note: counties can be designated multiple times

# approach 1: mean of multiple sub-county estimates
# may consider trying weighted average based on population
attain.sub <- ddply(attain[,c("pollutant", "countyname", "fips", vars)], ~ pollutant + countyname + fips, numcolwise(mean))

# approach 2: eliminate rows with sub-county measurements

# # duplicated rows
# dups <- attain[duplicated(attain[,c("fips", "pollutant")]) | duplicated(attain[,c("fips", "pollutant")], fromLast=T),]
# dups <- dups[order(dups$fips, dups$pollutant),]
# 
# # clean duplicates
# for(fips.dup in unique(dups$fips)){
#   
#   dup.rows <- attain[attain$fips == fips.dup,]
#   
#   # subset by pollutant
#   for(poll.dup in unique(dup.rows$pollutant)){
#     
#     dup.rows.poll <- dup.rows[dup.rows$pollutant == poll.dup,]
#     
#     # if no difference in variables, drop extra rows
#     if(sum(apply(dup.rows.poll[,vars], 2, function(x){return(length(unique(x)))}) > 1) == 0){
#       
#       attain <- attain[!(attain$fips == fips.dup & attain$pollutant == poll.dup),]
#       attain <- rbind(attain, dup.rows.poll[1,])
#     }
#   }
# }
# 
# new.dups <- attain[duplicated(attain[,c("fips", "pollutant")]) | duplicated(attain[,c("fips", "pollutant")], fromLast=T),]
# new.dups <- dups[order(dups$fips, dups$pollutant),]

# reshape to long format for editing and creating year variables
attain.long <- gather(attain.sub[,c("fips", "pollutant", vars)], measurement, value, vars)
attain.long$year <- gsub(".*?([0-9]+).*", "\\1", attain.long$measurement)
attain.long$measurement <- gsub("_([0-9]+)", "", attain.long$measurement)

# reconvert measurement variables to wide format
attain.final <- spread(attain.long, measurement, value)

# convert to wide by pollutant
attain.final$pollutant <- mapvalues(attain.final$pollutant, from = c("PM-2.5 (1997)", "1-Hour Ozone (1979)", "8-Hour Ozone (1997)"),
                                    to = c("PM25", "Ozone_1_hr", "Ozone_8_hr"))
attain.final <- reshape(attain.final, idvar = c("fips", "year"), timevar = "pollutant",  direction = "wide")

# zeros for any NA value 
attain.final[is.na(attain.final)] <- 0

attain.names <- c("cumulative_PM25", "current_PM25", "flipped_PM25", "flipped_three_PM25", "previous_PM25", "previous_five_PM25",
                  "cumulative_Ozone_1_hr", "current_Ozone_1_hr", "flipped_Ozone_1_hr", "flipped_three_Ozone_1_hr",  "previous_Ozone_1_hr", 
                  "previous_five_Ozone_1_hr", "cumulative_Ozone_8_hr", "current_Ozone_8_hr", "flipped_Ozone_8_hr", "flipped_three_Ozone_8_hr", 
                  "previous_Ozone_8_hr", "previous_five_Ozone_8_hr")

names(attain.final) <- c("fips", "year", attain.names)

# change current and previous variables to binary flags
binary_vars <- c("current_PM25", "current_Ozone_1_hr", "current_Ozone_8_hr",
                 "previous_PM25", "previous_Ozone_1_hr", "previous_Ozone_8_hr",
                 "previous_five_PM25", "previous_five_Ozone_1_hr", "previous_five_Ozone_8_hr",
                 "cumulative_PM25", "cumulative_Ozone_1_hr", "cumulative_Ozone_8_hr")

for (bvar in binary_vars){
  attain.final[,paste(bvar, "flag", sep="_")] <- ifelse(attain.final[,bvar] > 0, "non-attainment", "attainment")
}

# merge into panel set
panel <- merge(panel, attain.final, by = c("fips", "year"), all.x = T)

# if county was not in greenbook, indicates attainment (absence indicates county was always in attainment for that measurement standard)
# replace these NA's with zeros
panel[,attain.names] <- apply(panel[,attain.names], 2, function(x){replace(x, is.na(x), 0)})
panel[,paste(binary_vars, "flag", sep="_")] <- apply(panel[,paste(binary_vars, "flag", sep="_")], 2, function(x){replace(x, is.na(x), "attainment")})

## 3. Poverty level (SAIPE) ####
setwd(paste(baseWD, "Other data", "SAIPE", sep="/")) 

SAIPE.2008 <- read.xlsx("2008 poverty estimates SAIPE.xlsx", sheet=1, startRow = 3)
SAIPE.2011 <- read.xlsx("2011 poverty estimates SAIPE.xlsx", sheet=1, startRow = 3)
SAIPE.2014 <- read.xlsx("2014 poverty estimates SAIPE.xlsx", sheet=1, startRow = 4)

names(SAIPE.2014) <- names(SAIPE.2008)

SAIPE.2008$year <- 2008; SAIPE.2011$year <- 2011; SAIPE.2014$year <- 2014

# merge all three years
SAIPE <- rbind(SAIPE.2008, SAIPE.2011, SAIPE.2014); rm(SAIPE.2008); rm(SAIPE.2011); rm(SAIPE.2014)

# convert FIPS to numeric
SAIPE$County.FIPS <- as.numeric(SAIPE$County.FIPS)

# Add zeros to county fips for merging fips
SAIPE$County.FIPS.string <- as.character(SAIPE$County.FIPS)

SAIPE$County.FIPS.string[SAIPE$County.FIPS< 10 & !is.na(SAIPE$County.FIPS)] <- 
  paste("0", SAIPE$County.FIPS.string[SAIPE$County.FIPS< 10 & !is.na(SAIPE$County.FIPS)], sep="")

SAIPE$County.FIPS.string[SAIPE$County.FIPS < 100 & !is.na(SAIPE$County.FIPS)] <- 
  paste("0", SAIPE$County.FIPS.string[SAIPE$County.FIPS < 100 & !is.na(SAIPE$County.FIPS)], sep="")

SAIPE$fips <- as.numeric(paste(SAIPE$State.FIPS, SAIPE$County.FIPS.string, sep=""))

# subset to poverty percent
SAIPE.subset <- subset(SAIPE, select = c(fips, Name, Poverty.Percent.All.Ages, Poverty.Estimate.All.Ages, year))

# convert to numeric
SAIPE.subset$Poverty.Percent.All.Ages <- as.numeric(SAIPE.subset$Poverty.Percent.All.Ages)
SAIPE.subset$Poverty.Estimate.All.Ages <- as.numeric(SAIPE.subset$Poverty.Estimate.All.Ages)

# convert to fraction
# SAIPE.subset$SAIPE.subset$Poverty.Percent.All.Ages <- SAIPE.subset$SAIPE.subset$Poverty.Percent.All.Ages / 100

panel <- merge(panel, SAIPE.subset[,c("fips", "year", "Poverty.Percent.All.Ages", "Poverty.Estimate.All.Ages")], 
               by = c("fips", "year"), all.x=T)

# poverty lines variable by size of household, updated by year

## 4. Minority status data (ethnicity) ####
# American Fact Finder C02003

setwd(paste(baseWD, "Other data", "American Community Survey", "Race", sep="/")) 

#race.2008 <- read.csv("ACS_08_3YR_C02003_with_ann.csv", skip = 1)
race.2008 <- read.csv("ACS_09_5YR_C02003_with_ann.csv", skip = 1)
race.2011 <- read.csv("ACS_11_5YR_C02003_with_ann.csv", skip = 1)
race.2014 <- read.csv("ACS_14_5YR_C02003_with_ann.csv", skip = 1)

#race.2008 <- read.csv("ACS_08_3YR_DP3YR5_with_ann.csv", skip = 1)
#race.2011 <- read.csv("ACS_11_5YR_DP05_with_ann.csv", skip = 1)
#race.2014 <- read.csv("ACS_14_5YR_DP05_with_ann.csv", skip = 1)

colnames(race.2008) <- colnames(race.2014)
colnames(race.2011) <- colnames(race.2014)

race.2008$year <- 2008; race.2011$year <- 2011; race.2014$year <- 2014

# merge all three years
race <- rbind(race.2008, race.2011, race.2014); rm(race.2008); rm(race.2011); rm(race.2014)

# calculate fraction of county population of different race 
race$percent.white <- race[,"Estimate..Population.of.one.race....White"] / race[,"Estimate..Total."] * 100
race$percent.black <- race[,"Estimate..Population.of.one.race....Black.or.African.American"] / race[,"Estimate..Total."] * 100
race$percent.native <- race[,"Estimate..Population.of.one.race....American.Indian.and.Alaska.Native"] / race[,"Estimate..Total."] * 100
race$percent.asian <- race[,"Estimate..Population.of.one.race....Asian.alone"] / race[,"Estimate..Total."] * 100


# percentages
# race$percent.white <- race[,"Estimate..Population.of.one.race....White"] / race[,"Estimate..Total."]
# race$fraction.african.american <- race[,"Estimate..Population.of.one.race....Black.or.African.American"] / race[,"Estimate..Total."]
# race$fraction.native.american <- race[,"Estimate..Population.of.one.race....American.Indian.and.Alaska.Native"] / race[,"Estimate..Total."]
# race$fraction.asian <- race[,"Estimate..Population.of.one.race....Asian.alone"] / race[,"Estimate..Total."]

# race$pop.hispanic <- race[,"Estimate..HISPANIC.OR.LATINO.AND.RACE...Total.population...Hispanic.or.Latino..of.any.race."]
# colnames(race)[grepl("Hispanic", colnames(race))]

# race$total.pop <- race[, "Estimate..SEX.AND.AGE...Total.population"]

#race$percent.white <- race[,"Percent..RACE...One.race...White"]
#race$percent.black <- race[,"Percent..RACE...One.race...Black.or.African.American"]
#race$percent.asian <- race[,"Percent..RACE...One.race...Asian"]
#race$percent.native <- race[,"Percent..RACE...One.race...American.Indian.and.Alaska.Native"]
#race$percent.hispanic <- race[,"Percent..HISPANIC.OR.LATINO.AND.RACE...Total.population...Hispanic.or.Latino..of.any.race."]

# race <- race[,c("Id2", "Geography", "year", "percent.white", "percent.black", "percent.asian", "percent.native", "percent.hispanic")]
race <- race[,c("Id2", "Geography", "year", "percent.white", "percent.black", "percent.asian", "percent.native")]

names(race)[1] <- "fips"

# convert to numerics (N's become NAs)
#race$percent.white <- as.numeric(levels(race$percent.white))[race$percent.white]
#race$percent.black <- as.numeric(levels(race$percent.black))[race$percent.black]
#race$percent.asian <- as.numeric(levels(race$percent.asian))[race$percent.asian]
#race$percent.native <- as.numeric(levels(race$percent.native))[race$percent.native]
#race$percent.hispanic <- as.numeric(levels(race$percent.hispanic))[race$percent.hispanic]

# nonwhite fraction
race$percent.nonwhite <- (1 - (race$percent.white / 100)) * 100 

panel <- merge(panel, race[,-2], by = c("fips", "year"), all.x=T)

## 5. Income ####
setwd(paste(baseWD, "Other data", "American Community Survey", "Income", sep="/")) 

income.2008 <- read.csv("ACS_09_5YR_S1903_with_ann.csv", skip = 1)
income.2011 <- read.csv("ACS_11_5YR_S1903_with_ann.csv", skip = 1)
income.2014 <- read.csv("ACS_14_5YR_S1903_with_ann.csv", skip = 1)

colnames(income.2008) <- colnames(income.2014)
colnames(income.2011) <- colnames(income.2014)

income.2008$year <- 2008; income.2011$year <- 2011; income.2014$year <- 2014

#income.2008$Geography <- as.character(income.2008$Geography)
#income.2011$Geography <- as.character(income.2011$Geography)
#income.2014$Geography <- as.character(income.2014$Geography)

income.2008 <- income.2008[,c("Id2", "year", "Median.income..dollars...Estimate..Households")]
income.2011 <- income.2011[,c("Id2", "year", "Median.income..dollars...Estimate..Households")]
income.2014 <- income.2014[,c("Id2", "year", "Median.income..dollars...Estimate..Households")]

# merge all three years
income <- rbind(income.2008, income.2011, income.2014); rm(income.2008); rm(income.2011); rm(income.2014)
names(income) <- c("fips", "year", "median.income")
panel <- merge(panel, income, by = c("fips", "year"), all.x=T)

## 6. Rural vs. urban ####
setwd(paste(baseWD, "Other data", sep="/")) 
MSAs <- read.dta13("State_Fips_SMSA.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, convert.underscore = FALSE)
MSAs[MSAs$fips == 12025, "fips"] <- 12086
panel <- merge(panel, MSAs[c("fips", "SMSA")], by="fips", all.x=T)

# final panel formatting for regressions
panel.final <- panel; rm(panel)

# variable formatting
panel.final$gen.coal <- panel.final$gen.coal / 1E6      # convert from MWh to TWh
panel.final$gen.gas <- panel.final$gen.gas / 1E6        # convert from MWh to TWh
panel.final$population <- panel.final$population / 1E6      # convert to million persons
panel.final$median.income <- panel.final$median.income / 1E3      # convert to thousand dollars

# binary variables for logit / probit regressions
panel.final$EI.flag <- ifelse(panel.final$Export_import_ratio > 1, 1, 0)    # 6479 values
panel.final$OI.flag <- ifelse(panel.final$Own_import_ratio > 1, 1, 0)    # 63 values
panel.final$OE.flag <- ifelse(panel.final$Own_export_ratio > 1, 1, 0)    # 210 values

# panel without outliers
panel.EI.drops <- panel.final[panel.final$Export_import_ratio <= 100,]     # 12 EI ratios above 100

## Regressions ####

# 1. Basic model
# 2. log dependent
# 3. logit / probit
# 4. damage values

reg.model <- formula(Export_import_ratio ~ coal.flag + coal.flag:gen.coal + coal.flag:gen.coal:share.SO2.controls + 
                       gas.flag + gas.flag:gen.gas + current_PM25 + flipped_three_PM25 + 
                       population + SMSA + epa.region + Poverty.Percent.All.Ages +
                       percent.nonwhite + Poverty.Percent.All.Ages:percent.nonwhite + factor(year) + county.name)

# 1. Basic model
EI.lm <- lm(reg.model, data=panel.final)
#OI.lm <- update(EI.lm, Own_import_ratio ~.)
#OE.lm <- update(EI.lm, Own_export_ratio ~.)


# 2.1 logged variables with poverty variables
EI.log <- update(EI.lm, log(Export_import_ratio) ~.)
OI.log <- update(EI.lm, log(Own_import_ratio) ~.)
OE.log <- update(EI.lm, log(Own_export_ratio) ~.)

# 2.2 using income data 
EI.log.income <- update(EI.log, . ~ . - Poverty.Percent.All.Ages + median.income - Poverty.Percent.All.Ages:percent.nonwhite + median.income:percent.nonwhite)
OI.log.income <- update(OI.log, . ~ . - Poverty.Percent.All.Ages + median.income - Poverty.Percent.All.Ages:percent.nonwhite + median.income:percent.nonwhite)
OE.log.income <- update(OE.log, . ~ . - Poverty.Percent.All.Ages + median.income - Poverty.Percent.All.Ages:percent.nonwhite + median.income:percent.nonwhite)

# 2.3 attainment variables
EI.log.attain1 <- update(EI.log, . ~ . - county.name)
OI.log.attain1 <- update(OI.log, . ~ . - county.name)
OE.log.attain1 <- update(OE.log, . ~ . - county.name)

EI.log.attain2 <- update(EI.log, . ~ . - county.name - Poverty.Percent.All.Ages + median.income - Poverty.Percent.All.Ages:percent.nonwhite + median.income:percent.nonwhite)
OI.log.attain2 <- update(OI.log, . ~ . - county.name - Poverty.Percent.All.Ages + median.income - Poverty.Percent.All.Ages:percent.nonwhite + median.income:percent.nonwhite)
OE.log.attain2 <- update(OE.log, . ~ . - county.name - Poverty.Percent.All.Ages + median.income - Poverty.Percent.All.Ages:percent.nonwhite + median.income:percent.nonwhite)

EI.log.attain3 <- update(EI.log, . ~ . - current_PM25)
OI.log.attain3 <- update(OI.log, . ~ . - current_PM25)
OE.log.attain3 <- update(OE.log, . ~ . - current_PM25)

EI.log.attain4 <- update(EI.log, . ~ . - current_PM25  - county.name)
OI.log.attain4 <- update(OI.log, . ~ . - current_PM25  - county.name)
OE.log.attain4 <- update(OE.log, . ~ . - current_PM25  - county.name)

# 3.1 Basic logit regression
# These take a long time to converge and don't seem particularly useful...
#EI.logit <- glm(update(reg.model, EI.flag ~.), data=panel.final, binomial(link = "logit"))
#OI.logit <- glm(update(reg.model, OI.flag ~.), data=panel.final, binomial(link = "logit"))
#OE.logit <- glm(update(reg.model, OE.flag ~.), data=panel.final, binomial(link = "logit"))

# probit models don't converge
# EI.probit <- glm(update(reg.model, EI.flag ~.), data=panel, binomial(link = "probit"))
# OI.probit <- glm(update(reg.model, OI.flag ~.), data=panel, binomial(link = "probit"))
# OE.probit <- glm(update(reg.model, OE.flag ~.), data=panel, binomial(link = "probit"))

# 4. with actual variables
real_values = T
if (real_values){
  Export.log <- update(EI.log, log(Export) ~.)
  Import.log <- update(EI.log, log(Imports) ~.)
  Own.log <- update(EI.log, log(Own_damage) ~.)
  #Net.lm <- update(EI.log, Net_imports / 1E9 ~.)
  
  Export.log.income <- update(EI.log.income, log(Export) ~ . )
  Import.log.income <- update(OI.log.income, log(Imports) ~ . )
  Own.log.income <- update(OE.log.income, log(Own_damage) ~ . )
  
  # 2.3 attainment variables
  Export.log.attain1 <- update(EI.log.attain1, log(Export) ~ . )
  Import.log.attain1 <- update(OI.log.attain1, log(Imports) ~ . )
  Own.log.attain1 <- update(OE.log.attain1, log(Own_damage) ~ . )
  
  Export.log.attain2 <- update(EI.log.attain2, log(Export) ~ . )
  Import.log.attain2 <- update(OI.log.attain2, log(Imports) ~ . )
  Own.log.attain2 <- update(OE.log.attain2, log(Own_damage) ~ . )
  
  Export.log.attain4 <- update(EI.log.attain3, log(Export) ~ . )
  Import.log.attain4 <- update(OI.log.attain3, log(Imports) ~ . )
  Own.log.attain4 <- update(OE.log.attain3, log(Own_damage) ~ . )
  
  Export.log.attain4 <- update(EI.log.attain4, log(Export) ~ . )
  Import.log.attain4 <- update(OI.log.attain4, log(Imports) ~ . )
  Own.log.attain4 <- update(OE.log.attain4, log(Own_damage) ~ . )
}


# summary(EI.log)$coefficients[!grepl("county.name", rownames(summary(EI.log)$coefficients)), ]
# summary(EI.logit)$coefficients[!grepl("county.name", rownames(summary(EI.logit)$coefficients)), ]


# see "regression.Rnw"

if (real_values){
  reg_models <- c("EI.log", "OI.log", "OE.log", "Export.log", "Import.log", "Own.log")
} else{
  reg_models <- c("EI.log", "OI.log", "OE.log") 
}

variations <- c("", ".income", paste(".attain", c(1,2,3,4), sep=""))
save_vars <- c("panel.final")

for (reg_model in reg_models){
  save_vars <- c(save_vars, paste(reg_model, variations, sep=""))
}

rm(list=setdiff(ls(), c("baseWD", "resultsWD", "fips", "attain.final", "census.fips.sub", save_vars)))

## SAVE ####
setwd(baseWD)
save.image("Regressions.RData")


## Regression diagnostics ####

# histograms
hist(panel.final$Export_import_ratio, breaks="FD", main = "", xlab="EI ratio")
hist(panel.final$Own_import_ratio, breaks="FD", main = "", xlab="OI ratio")
hist(panel.final$Own_export_ratio, breaks="FD", main = "", xlab="OE ratio")

hist(log(panel.final$Export_import_ratio), breaks="FD", main = "", xlab="log EI ratio")
hist(log(panel.final$Own_import_ratio), breaks="FD", main = "", xlab="log OI ratio")
hist(log(panel.final$Own_export_ratio), breaks="FD", main = "", xlab="log OE ratio")

# correlation
varCor <- cor(panel.final[,c("coal.flag", "gen.coal", "share.SO2.controls", "gas.flag", "gen.gas", 
                   "population", "SMSA", "Poverty.Percent.All.Ages",
                   "median.income","percent.nonwhite", "year", "current_PM25", "flipped_three_PM25")], use = "na.or.complete")
rownames(varCor) <- c("Coal plant", "Coal generation", "SO2 controls", "Gas plant", "Gas generation", "Population", "Metropolitan area", 
                      "Poverty percent", "Median income", "Percent nonwhite","Year", "Non-attainment", "Change in attainment")
colnames(varCor) <- c("Coal plant", "Coal generation", "SO2 controls", "Gas plant", "Gas generation", "Population", "Metropolitan area", 
                      "Poverty percent", "Median income", "Percent nonwhite","Year", "Non-attainment", "Change in attainment")


setwd(paste(baseWD, "figures", "Regression diagnostics", sep="/")) 
pdf("Correlation plot.pdf", width=6, height=6)
#corrplot(varCor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, method="number")
corrplot.mixed(varCor, number.cex=0.7, diag="u", tl.pos = "lt", tl.col = "black")
dev.off()


# logged model

regression.diagnostics <- function(model, filename, name){
  setwd(paste(baseWD, "figures", "Regression diagnostics", sep="/")) 
  pdf(paste(filename, " - residuals.pdf"), width=6, height = 6)
  par(mar = c(5, 4.5, 4, 2) + 0.1)
  hist(residuals(model), breaks="FD", xlab="Regression Residuals", main=paste(name, "residuals"))
  dev.off()
  
  pdf(paste(filename, " - jackknife residuals.pdf"), width=6, height = 6)
  par(mar = c(5, 4.5, 4, 2) + 0.1)
  hist(rstudent(model), breaks="FD", xlab="Jackknife Regression Residuals")
  dev.off()
  
  pdf(paste(filename, " - regression diagnostics.pdf"), width=10, height = 10)
  par(mfrow = c(2,2))
  plot(model)
  dev.off()
}

regression.diagnostics(EI.log, filename="EI OLS log", name="Log Export/Import")
regression.diagnostics(OI.log, filename="OI OLS log", name="Log Own/Import")
regression.diagnostics(OE.log, filename="OE OLS log", name="Log Own/Export")


# panel residuals
setwd(paste(baseWD, "figures", "Regression diagnostics", sep="/")) 

pdf(paste("Panel", "- jackknife residuals.pdf"), width=10, height=4)
par(mfrow = c(1,3))
par(mar = c(5, 4.5, 4, 2) + 0.1)
hist(rstudent(EI.log), breaks="FD", xlab="Jackknife Regression Residuals", main="Log(export/import)")

par(mar = c(5, 4.5, 4, 2) + 0.1)
hist(rstudent(OI.log), breaks="FD", xlab="Jackknife Regression Residuals", main="Log(self-inflicted/import)")

par(mar = c(5, 4.5, 4, 2) + 0.1)
hist(rstudent(OE.log), breaks="FD", xlab="Jackknife Regression Residuals", main="Log(self-inflicted/export)")
dev.off()


# without county version
pdf(paste("Panel", "without county FE - jackknife residuals.pdf"), width=10, height=4)
par(mfrow = c(1,3))
par(mar = c(5, 4.5, 4, 2) + 0.1)
hist(rstudent(EI.log.attain1), breaks="FD", xlab="Jackknife Regression Residuals", main="Log(export/import)")

par(mar = c(5, 4.5, 4, 2) + 0.1)
hist(rstudent(OI.log.attain1), breaks="FD", xlab="Jackknife Regression Residuals", main="Log(self-inflicted/import)")

par(mar = c(5, 4.5, 4, 2) + 0.1)
hist(rstudent(OE.log.attain1), breaks="FD", xlab="Jackknife Regression Residuals", main="Log(self-inflicted/export)")
dev.off()

## Regression plots ####

# EI.log, EI.log.attain1
# summary(EI.log)$coefficients[!grepl("county.name", rownames(summary(EI.log)$coefficients)), ]

probEstimate <- function(coefTable, coefs, regressors){
  return(sum(coefTable[coefTable$variable %in% coefs, "Estimate"] * regressors))
}

probStandardError <- function(model, coefs, regressors){
  # extract variance-covariance matrix
  vcov.mat <- vcov(model)[coefs, coefs]
  
  # matrix for values
  values <- matrix(1, nrow = dim(vcov.mat)[1], ncol = dim(vcov.mat)[2])
  rownames(values) <- rownames(vcov.mat)
  colnames(values) <- colnames(vcov.mat)
  
  # multiple values by regressors
  for (i in 1:length(regressors)){
    values[,coefs[i]] <- values[, coefs[i]] * regressors[i]
    values[coefs[i], ] <- values[coefs[i], ] * regressors[i]
  }
  
  variance <- sum(vcov.mat * values)
  
  # return standard error estimate for overall eta 
  return(sqrt(variance))
}

regPlotFormat <- function(reg.model, coefs, id){
  # extract coefficients
  coefTable <- summary(reg.model)$coefficients[rownames(summary(reg.model)$coefficients) %in% coefs, c("Estimate", "Std. Error")]
  coefTable <- as.data.frame(coefTable)
  coefTable$variable <- rownames(coefTable); rownames(coefTable) <- NULL
  
  
  # multipliers
  
  # Coal plant generating 2 TWh annually
  probResults <- data.frame(variable="Coal plant\n(3 TWh)",
                            estimate=probEstimate(coefTable, coefs=c("coal.flag", "coal.flag:gen.coal"), regressors=c(1,3)), 
                            se=probStandardError(EI.log, coefs=c("coal.flag", "coal.flag:gen.coal"), regressors=c(1,3)))
  
  # Coal plant generating 2 TWh annually w/ 100% SO2 controls
  probResults <- rbind(probResults,
                      data.frame(variable="Coal plant (3 TWh\n w/ scrubbers)",
                                 estimate=probEstimate(coefTable, coefs=c("coal.flag", "coal.flag:gen.coal","coal.flag:gen.coal:share.SO2.controls"), regressors=c(1,3,3)),
                                 se=probStandardError(EI.log, coefs=c("coal.flag", "coal.flag:gen.coal", "coal.flag:gen.coal:share.SO2.controls"), regressors=c(1,3,3))))
  
  # Gas plant generating TWh
  probResults <- rbind(probResults, 
                       data.frame(variable="Gas plant\n(3 TWh)",
                                  estimate=probEstimate(coefTable, coefs=c("gas.flag", "gas.flag:gen.gas"), regressors=c(1,3)), 
                                  se=probStandardError(EI.log, coefs=c("gas.flag", "gas.flag:gen.gas"), regressors=c(1,3))))
  
  # County in non-attainment
  probResults <- rbind(probResults, 
                       data.frame(variable="In non-attainment",
                                  estimate=coefTable[coefTable$variable=="current_PM25", "Estimate"], 
                                  se=coefTable[coefTable$variable=="current_PM25", "Std. Error"]))
  
  # County moved into attainment since last period (3 years prior)
  probResults <- rbind(probResults, 
                       data.frame(variable="Moved into attainment",
                                  estimate=coefTable[coefTable$variable=="flipped_three_PM25", "Estimate"], 
                                  se=coefTable[coefTable$variable=="flipped_three_PM25", "Std. Error"]))
  
  # 10% of the population under the poverty level (1st quartile)
  probResults <- rbind(probResults, 
                       data.frame(variable="10% population\nunder poverty line",
                                  estimate=coefTable[coefTable$variable=="Poverty.Percent.All.Ages", "Estimate"]*10, 
                                  se=coefTable[coefTable$variable=="Poverty.Percent.All.Ages", "Std. Error"]*10))
  
  # 20% of the population under the poverty level (3rd quartile)
  probResults <- rbind(probResults, 
                       data.frame(variable="20% population\nunder poverty line",
                                  estimate=coefTable[coefTable$variable=="Poverty.Percent.All.Ages", "Estimate"]*20, 
                                  se=coefTable[coefTable$variable=="Poverty.Percent.All.Ages", "Std. Error"]*20))
  
  # 5% of the population nonwhite (1st quartile)
  probResults <- rbind(probResults, 
                       data.frame(variable="5% pop nonwhite",
                                  estimate=coefTable[coefTable$variable=="percent.nonwhite", "Estimate"]*5, 
                                  se=coefTable[coefTable$variable=="percent.nonwhite", "Std. Error"]*5))
  
  # 20% of the population nonwhite (3rd quartile)
  probResults <- rbind(probResults, 
                       data.frame(variable="20% pop nonwhite",
                                  estimate=coefTable[coefTable$variable=="percent.nonwhite", "Estimate"]*20, 
                                  se=coefTable[coefTable$variable=="percent.nonwhite", "Std. Error"]*20))
  
  # upper and lower bounds (95% CI)
  probResults$upper <- probResults[,"estimate"] + 1.96 * probResults[,"se"]
  probResults$lower <- probResults[,"estimate"] - 1.96 * probResults[,"se"]
  
  # convert to percentages (log scale)
  probResults[,c("estimate", "upper", "lower")] <- probResults[,c("estimate", "upper", "lower")] * 100
  
  probResults$id <- id
  
  return(probResults)
}

coefs <- c("coal.flag", "coal.flag:gen.coal", "coal.flag:gen.coal:share.SO2.controls",
           "gas.flag", "gas.flag:gen.gas", "current_PM25", "flipped_three_PM25",
           "Poverty.Percent.All.Ages", "percent.nonwhite")

reg1 <- regPlotFormat(EI.log, coefs=coefs, id="With county fixed effects")
reg2 <- regPlotFormat(EI.log.attain1, coefs=coefs,id="Without county fixed effects")
reg <- rbind(reg1, reg2)

# reorder factors
#reg$variable <- factor(reg$variable, levels=rev(unique(reg$variable[order(reg[reg$id == "With county fixed effects", "estimate"])])))

# drop flipped variable
reg <- reg[reg$variable != "Moved into attainment",]

regressionPlot <- function(regressionData){
  
  # bar plot
  ggplot(regressionData, aes(x=variable, y=estimate, fill=id)) +
    geom_bar(stat = "identity", color="black", position=position_dodge()) +
    ylab("% increase in\nexport/import ratio") + xlab("") + theme_classic() +
    scale_fill_manual(values=c("#56B4E9", "#E69F00")) + 
    theme(text = element_text(size=16, color="black"),
          legend.title=element_blank(),
          legend.position = c(0.8, 0.7),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(angle=45, hjust=1, vjust=1),
          plot.margin = unit(c(20.5,5.5,1,5.5), "pt")) +
    geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=0.1, position=position_dodge(.9)) +
    ylim(c(-100, 100)) + 
    geom_hline(yintercept=0, linetype=1) + 
    geom_vline(xintercept=3.5, linetype=2) +
    geom_vline(xintercept=4.5, linetype=2) + 
    geom_text(x=2, y=95, label="Electricity generation") +
    geom_text(x=4, y=85, label="Attainment\nstatus") + 
    geom_text(x=6.5, y=95, label="Socioeconomic indicators")
  
}

regressionPlot(reg)

setwd(paste(baseWD, "figures", "Regression diagnostics", sep="/")) 
ggsave("EI regression results.pdf", width=10, height=5)

## Exploratory data analysis ####

EDA.panel <- function(data, ratio, ratio.name){
  
  pdf(paste("EDA - ", ratio.name, ".pdf", sep=""), width=4.45, height=4.45)
  margins <- c(4, 4, 2, 2) + 0.1
  #par(mfrow = c(2, 2))
  layout(matrix(c(1,2,3,4), ncol=2, byrow=TRUE), heights = c(0.48, 0.52))
  
  coal.sub <- data[data$coal.flag ==1, ]
  
  par(mar = margins)
  plot(coal.sub[,"gen.coal"], log(coal.sub[,ratio]), xlab = "Coal generation (TWh)", ylab = ratio.name, col=rgb(0,0,0,alpha=0.4), pch=19)
  abline(lm(log(coal.sub[,ratio]) ~ coal.sub[,"gen.coal"]), col="red")
  
  par(mar = margins)
  plot(data[,"percent.nonwhite"], log(data[,ratio]), xlab = "Percent nonwhite (%)", ylab = ratio.name, col=rgb(0,0,0,alpha=0.4), pch=19)
  abline(lm(log(data[,ratio]) ~ data[,"percent.nonwhite"]), col="red")
  
  par(mar = margins + c(2, 0, 0, 0))
  plot(data[,"median.income"], log(data[,ratio]), xlab = "Median income (thousands)", ylab = ratio.name, col=rgb(0,0,0,alpha=0.4), pch=19)
  abline(lm(log(data[,ratio]) ~ data[,"median.income"]), col="red")
  
  par(mar = margins + c(2, 0, 0, 0))
  boxplot(log(data[,ratio]) ~ data[,"epa.region"], xlab = "", ylab = ratio.name, las =2, col=rgb(0,0,0,alpha=0.4), pch=19)
  
  dev.off()
  
}

setwd(paste(baseWD, "figures", "Regression diagnostics", sep="/")) 
EDA.panel(panel.final, "Export_import_ratio", "log(EI)")
EDA.panel(panel.final, "Own_import_ratio", "log(OI)")
EDA.panel(panel.final, "Own_export_ratio", "log(OE)")


## Attainment status and ratios ####
attainSummary <- ddply(panel.final, ~ current_PM25 + SMSA , summarize, EI = mean(Export_import_ratio),
                       OI = mean(Own_import_ratio),
                       OE = mean(Own_export_ratio),
                       EI.sd = sd(Export_import_ratio),
                       OI.sd = sd(Own_import_ratio),
                       OE.sd = sd(Own_export_ratio),
                       count = length(Export_import_ratio))

attainBaseline <- c("all", apply(panel.final[,c("Export_import_ratio", "Own_import_ratio", "Own_export_ratio")], 2, mean),
                    apply(panel.final[,c("Export_import_ratio", "Own_import_ratio", "Own_export_ratio")], 2, sd))

names(attainBaseline) <- names(attainSummary)[-1]

attainSummary <- rbind(attainSummary, attainBaseline); rm(attainBaseline)

# add count of observations
attainSummary <- cbind(attainSummary, c(sum(panel.final$current_PM25 == 0), sum(panel.final$current_PM25 == 1), nrow(panel.final)))
names(attainSummary)[dim(attainSummary)[2]] <- "count"


attainSummary[,c("EI", "OI", "OE", "EI.sd", "OI.sd", "OE.sd")] <- apply(attainSummary[,c("EI", "OI", "OE", "EI.sd", "OI.sd", "OE.sd")], 2, 
                                                                        function(x){round(as.numeric(x), 3)})

wilcox.test(Export_import_ratio ~ current_PM25, data=panel.final)
wilcox.test(Own_import_ratio ~ current_PM25, data=panel.final)
wilcox.test(Own_export_ratio ~ current_PM25, data=panel.final)

t.test(Export_import_ratio ~ current_PM25, data=panel.final)
t.test(Own_import_ratio ~ current_PM25, data=panel.final)
t.test(Own_export_ratio ~ current_PM25, data=panel.final)



zScore <- function(x1, x2, s1, s2, n1, n2){
  return((x1-x2)/sqrt(s1^2/n1 + s2^2/n2))
}

getAttainZScore <- function(data, var){
  x1 <- data[data$current_PM25 == 0, var]
  x2 <- data[data$current_PM25 == 1, var]
  
  s1 <- data[data$current_PM25 == 0, paste(var, ".sd", sep="")]
  s2 <- data[data$current_PM25 == 1, paste(var, ".sd", sep="")]
  
  n1 <- data[data$current_PM25 == 0, "count"]
  n2 <- data[data$current_PM25 == 1, "count"]
  
  return(zScore(x1, x2, s1, s2, n1, n2))
}

getAttainZScore(attainSummary, "EI")
getAttainZScore(attainSummary, "EI")

pnorm(getAttainZScore(attainSummary, "EI"), lower.tail=FALSE)
pnorm(getAttainZScore(attainSummary, "OI"), lower.tail=TRUE)
pnorm(getAttainZScore(attainSummary, "OE"), lower.tail=TRUE)

qnorm(.025,lower.tail=FALSE)

ks.test(panel.final[panel.final$current_PM25 == 0, "Export_import_ratio"],
        panel.final[panel.final$current_PM25 == 1, "Export_import_ratio"])

ks.test(panel.final[panel.final$current_PM25 == 0, "Own_import_ratio"],
        panel.final[panel.final$current_PM25 == 1, "Own_import_ratio"])

ks.test(panel.final[panel.final$current_PM25 == 0, "Own_export_ratio"],
        panel.final[panel.final$current_PM25 == 1, "Own_export_ratio"])
# Notes for models to consider:
# - truncate "outlying" ratios
# - use indicator generation variable (has coal plant or not)
# - use indicator variables for natural gas, other facilities

# Other regression to do items:
# - confirm year trend matches CDFs
# - clean up variables in table
# - set up residual plots
# - plots of variable trends (EDA)

## Natural gas result ####

# how many counties switch from coal to gas?
flags2008 <- panel.final[panel.final$year == 2008, c("fips", "coal.flag", "gen.coal", "gas.flag", "gen.gas")]
flags2011 <- panel.final[panel.final$year == 2011, c("fips", "coal.flag", "gen.coal", "gas.flag", "gen.gas")]
flags2014 <- panel.final[panel.final$year == 2014, c("fips", "coal.flag", "gen.coal", "gas.flag", "gen.gas")]

# order by fips
flags2008 <- flags2008[order(flags2008$fips), ]
flags2011 <- flags2011[order(flags2011$fips), ]
flags2014 <- flags2014[order(flags2014$fips), ]

flags2011$switched <- ifelse(flags2008$coal.flag == 1 & flags2008$gas.flag == 0 & flags2011$coal.flag == 0 & flags2011$gas.flag == 1, 1, 0)
flags2014$switched <- ifelse(flags2011$coal.flag == 1 & flags2011$gas.flag == 0 & flags2014$coal.flag == 0 & flags2014$gas.flag == 1, 1, 0)

flags2011$newGas <- ifelse(flags2008$coal.flag == 0 & flags2008$gas.flag == 0 & flags2011$coal.flag == 0 & flags2011$gas.flag == 1, 1, 0)
flags2014$newGas <- ifelse(flags2011$coal.flag == 0 & flags2011$gas.flag == 0 & flags2014$coal.flag == 0 & flags2014$gas.flag == 1, 1, 0)

# reduction in coal generatio vs. increase in gas?
# flag for having switched from coal to gas?


with(panel.final, table(coal.flag, gas.flag, year))

# summaries of counties by generation
hist(panel.final$gen.gas[panel.final$gen.gas > 0])
summary(panel.final$gen.gas[panel.final$gen.gas > 0])

hist(panel.final$gen.coal[panel.final$gen.coal > 0])
summary(panel.final$gen.coal[panel.final$gen.coal > 0])

length(unique(panel.final[panel.final$gen.coal > 3, "fips"]))
length(unique(panel.final[panel.final$gen.coal > 2, "fips"]))
length(unique(panel.final[panel.final$gen.gas > 3, "fips"]))
length(unique(panel.final[panel.final$gen.gas > 2, "fips"]))

length(unique(panel.final[panel.final$coal.flag > 0, "fips"]))
length(unique(panel.final[panel.final$gas.flag > 0, "fips"]))

## Median income and race plots ####

# group counties into 5 categories by income
incomePlotData <- panel.final[,c("fips", "year", "county.name", "state","population", "median.income", "Imports", "Own_damage","Export")]
incomePlotData$damage <- incomePlotData$Imports + incomePlotData$Own_damage
#incomePlotData$damage <- incomePlotData$Export

# convert population from millions
incomePlotData$population <- incomePlotData$population * 1E6

# per capita damage
incomePlotData$damagePerCap <- incomePlotData$damage / incomePlotData$population

# cut data into fifths by year
incomePlotData <- ddply(incomePlotData, ~ year, transform, incomeCategory=cut(median.income, breaks=quantile(median.income, probs=seq(0,1,by=0.2)), include.lowest=T, 
                                                                              labels = c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%")))
# check assignment
with(incomePlotData, table(year, incomeCategory))

incomeSave <- incomePlotData
incomePlotData <- ddply(incomePlotData, ~ year + incomeCategory, transform, averageDamage=mean(damage),
                                                                            averageDamagePerCap=mean(damagePerCap),
                                                                            medianDamage=median(damage),
                                                                            medianDamagePerCap=median(damagePerCap))
# plot boxplots
ggplot(incomePlotData, aes(x=incomeCategory, y=damagePerCap, fill=factor(year))) + 
  geom_boxplot(position=position_dodge()) + theme_classic()

ggplot(incomePlotData, aes(x=incomeCategory, y=averageDamage, fill=factor(year))) + 
  geom_bar(stat = "identity", color="black", position=position_dodge()) + theme_classic()

ggplot(incomePlotData, aes(x=incomeCategory, y=averageDamagePerCap, fill=factor(year))) + 
  geom_bar(stat = "identity", color="black", position=position_dodge()) + theme_classic()

ggplot(incomePlotData, aes(x=incomeCategory, y=medianDamagePerCap, fill=factor(year))) + 
  geom_bar(stat = "identity", color="black", position=position_dodge()) + theme_classic()

## Boostrap on median damage per capita

# random sampling w/ replacement of vector data 
bootstrapFunction <- function(data, estimator){
  # draw bootstrap sample
  if(is.data.frame(data)){
    data.b <- data[sample(1:nrow(data), nrow(data), replace=T),]
  } else{
    data.b <- sample(data, length(data), replace=T)
  }
  # calculate parameter
  theta.hat.star <- estimator(data.b)
  return(theta.hat.star)
}
bootstrapHelperFunction <- function(data, estimator, B, alpha=0.05){
  theta.hat <- estimator(data)
  # bootstrap
  theta.hat.stars <- replicate(B, bootstrapFunction(data, estimator))
  # confidence interval
  ci.lower <- 2 * theta.hat - quantile(theta.hat.stars, 1 - alpha/2)  
  ci.upper <- 2 * theta.hat - quantile(theta.hat.stars, alpha/2)
  return(data.frame(estimate=theta.hat, ci.lower=unname(ci.lower), ci.upper = unname(ci.upper)))
}

set.seed(123)
years <- c(2008, 2011, 2014)
incomeBootResults <- data.frame()
for (year in years){
  for (income in c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%")){
    incomeToBoot <- incomePlotData[incomePlotData$year == year & incomePlotData$incomeCategory == income, "damagePerCap"]
    bootData <- bootstrapHelperFunction(incomeToBoot, median, 5000)
    incomeBootResults <- rbind(incomeBootResults, cbind(year, income, bootData))
  }
}; rm(bootData); rm(incomeToBoot); rm(year); rm(income)

incomeBootResults$year <- factor(incomeBootResults$year)
g1 <- ggplot(incomeBootResults, aes(x=year, y=estimate, fill=income)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge(0.9)) + theme_classic() + 
  geom_errorbar(aes(ymax = incomeBootResults$ci.upper, ymin = incomeBootResults$ci.lower), 
                colour="black", width=0.2, position=position_dodge(0.9)) +
  xlab("Year") + ylab("Median health damages\n($ per person)") + 
  theme(text = element_text(size=16),
        axis.text = element_text(color="black")) +
  guides(fill=guide_legend(title="Income quintile"))

setwd(paste(baseWD, "figures", "Income", sep="/")) 
ggsave("Income damage barplots - median.pdf", width=8)

# same plot by minority population
racePlotData <- panel.final[,c("fips", "year", "county.name", "state", "population", "percent.nonwhite", "Imports", "Own_damage", "Export")]
racePlotData$damage <- racePlotData$Imports + racePlotData$Own_damage
#racePlotData$damage <- racePlotData$Export

# convert population from millions
racePlotData$population <- racePlotData$population * 1E6

# per capita damage
racePlotData$damagePerCap <- racePlotData$damage / racePlotData$population

# cut data into fifths by year
racePlotData <- ddply(racePlotData, ~ year, transform, raceCategory=cut(percent.nonwhite, breaks=quantile(percent.nonwhite, probs=seq(0,1,by=0.2)), include.lowest=T, 
                                                                              labels = c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%")))
# check assignment
with(racePlotData, table(year, raceCategory))

set.seed(123)
raceBootResults <- data.frame()
for (year in years){
  for (race in c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%")){
    raceToBoot <- racePlotData[racePlotData$year == year & racePlotData$raceCategory == race, "damagePerCap"]
    bootData <- bootstrapHelperFunction(raceToBoot, mean, 5000)
    raceBootResults <- rbind(raceBootResults, cbind(year, race, bootData))
  }
}; rm(bootData); rm(raceToBoot); rm(year); rm(race)

raceBootResults$year <- factor(raceBootResults$year)
g2 <- ggplot(raceBootResults, aes(x=year, y=estimate, fill=race)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge(0.9)) + theme_classic() + 
  geom_errorbar(aes(ymax = raceBootResults$ci.upper, ymin = raceBootResults$ci.lower), 
                colour="black", width=0.2, position=position_dodge(0.9)) +
  xlab("Year") + ylab("Median health damages\n($ per person)") + 
  theme(text = element_text(size=16),
        axis.text = element_text(color="black")) +
  guides(fill=guide_legend(title="Minority population\nquintile"))

setwd(paste(baseWD, "figures", "Income", sep="/")) 
ggsave("Minority population damage barplots - median.pdf", width=8)

rm(incomePlotData); rm(racePlotData)

# combined panel plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(g1 + theme(legend.position="bottom") + guides(fill=guide_legend(title="Quintile")))

#g1 <- g1 + annotate("text", x=factor(2011), y=5400, label="Median income", size=6)
#g2 <- g2 + annotate("text", x=factor(2011), y=5400, label="Minority population", size=6)

#g2 <- g2 + annotate("text", x=factor(2011), y=5300, label="Percent population that is nonwhite")
#g2 <- g2 +  annotation_custom(grob = textGrob(label = "Minority population", hjust = 0, gp = gpar(cex = 1.5)),
#                                ymin = 5300,      # Vertical position of the textGrob
#                                xmax = factor(2011),
#                                xmin = factor(2011))
    
gr1 <- arrangeGrob(g1 + ggtitle("a             Median income") + 
                     theme(legend.position = "none")  + ylim(0, 5200), 
                   g2 + ggtitle("b          Minority population") + 
                     theme(legend.position = "none")  + ylim(0, 5200) + ylab(""), ncol=2, widths=c(1.05,1))
gr2 <- arrangeGrob(mylegend, ncol=1)
quintilePlot <- grid.arrange(grobs = list(gr1, gr2), heights=c(3, 0.25))

setwd(paste(baseWD, "figures", "Income", sep="/")) 
ggsave("Combined income and minority barplots - median.pdf",quintilePlot, width=12)


# approach using weighted median

# function to estimate weighted median
weightedMedian <- function(df, targetCol="damagePerCap"){
  
  df <- ddply(df, ~ incomeCategory, transform, weight=population/sum(population))
  # test weights
  ddply(df, ~ incomeCategory, summarize, weightSum=sum(weight))
  
  values <- df[,targetCol]
  weights <- df[,"weight"]
  
  if (length(values) != length(weights)){
    print("Weighted median error: values and weights vectors unequal.")
    return(NA)
  } 
  if(length(values) == 0){
    return(NA)
  }
  if(sum(weights < 0 | weights > 1)){
    print("Weighted median error: invalid weight.")
    return(NA)
  } else if(abs(sum(weights) - 1) > 0.0001){
    print("Weighted median error: weights do not sum to 1.")
    return(NA)
  }
  # sort by values
  newOrder <- order(values)
  values <- values[newOrder]
  weights <- weights[newOrder]
  weightsRev <- sort(weights, decreasing=T)
  
  cumWeight <- cumsum(weights)
  cumWeightRev <- cumsum(weightsRev)  # find median from left and right
  k_left <- which(cumWeight >= 0.5)[1]
  k_right <- which(cumWeightRev >= 0.5)[1] 
  
  x_k_left <- values[k_left]
  x_k_right <- rev(values)[k_right]
  
  if(x_k_left == x_k_right){            # if values are equal, return 1
    return(x_k_left)
  } else {                              # otherwise, take mean
    return(mean(c(x_k_left, x_k_right)))  
  }
}

set.seed(123)
incomeBootResults <- data.frame()
for (year in years){
  for (income in c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%")){
    incomeToBoot <- incomeSave[incomeSave$incomeCategory == income & incomeSave$year == year, ]
    bootData <- bootstrapHelperFunction(incomeToBoot, weightedMedian, 5000)
    incomeBootResults <- rbind(incomeBootResults, cbind(income, year, bootData))
  }; rm(bootData); rm(incomeToBoot); rm(income)
}

incomeBootResults$year <- factor(incomeBootResults$year)
g1 <- ggplot(incomeBootResults, aes(x=year, y=estimate, fill=income)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge(0.9)) + theme_classic() + 
  geom_errorbar(aes(ymax = incomeBootResults$ci.upper, ymin = incomeBootResults$ci.lower), 
                colour="black", width=0.2, position=position_dodge(0.9)) +
  xlab("Year") + ylab("Median health damages\n($ per person)") + 
  theme(text = element_text(size=16),
        axis.text = element_text(color="black")) +
  guides(fill=guide_legend(title="Income quintile"))

setwd(paste(baseWD, "figures", "Income", sep="/")) 
ggsave("Income damage barplots - weighted median.pdf", width=8)



## PM share analysis ####
# analyze counties that are in non-attainement because of out of state sources

readPMmatrix <- function(year){
  # set directory based on year
  wd <- resultsWD[which(year==years)]
  setwd(paste(wd, "Other outputs", sep="/")) 
  PMmatrix <- read.csv("PM matrix.csv", header=FALSE)
  return(PMmatrix)
}

readPMbase <- function(year){
  # set directory based on year
  wd <- resultsWD[which(year==years)]
  setwd(paste(wd, "Other outputs", sep="/")) 
  PMbase <- read.csv("PM base.csv", header=FALSE)
  PMbase <- data.frame(fips=fips, PM=PMbase$V1)
  #PMbase$fips[PMbase$fips == 12025] <- 12086
  return(PMbase)
}

PM <- list(); PMBase <- list()
years <- c(2008, 2011, 2014)
for (i in 1:length(years)){
  PM[[i]] <- readPMmatrix(years[i])
  PMBase[[i]] <- readPMbase(years[i])
}

# compare matrix to baseline
comparePM <- function(matrixPM, basePM){
  return(unlist(basePM - colSums(matrixPM)))
}

diff <- comparePM(PM[[1]], PMBase[[1]]$PM)
hist(diff, breaks="FD")

calcPMshare <- function(PMmatrix){
  share <- sweep(PMmatrix, colSums(PMmatrix), MARGIN=2, FUN="/")
  colnames(share) <- fips; rownames(share) <- fips
  return(share)
}

# read in FIPS codes from AP2
setwd(paste(baseWD, "Other data", sep="/"))
fips.stacks <- read.dta13("AP2_Fips_List.dta", convert.dates = TRUE, convert.factors = TRUE,
                          missing.type = FALSE, convert.underscore = FALSE)
fips.stacks$stack <- c(rep("area",3109), rep("low",3109), rep("med",3109), rep("tall",565), rep("tall2",91))
fips <- fips.stacks[fips.stacks$stack == "area", "fips"]

# allocate percentage of PM by source
PMShare <- llply(PM, calcPMshare)

# compare to attainment records
attainComparison <- function(attain, basePM, year){
  attainSub <- attain[attain$year == year, c("fips", "current_PM25")]
  colnames(attainSub) <- c("fips", "attainment_actual")
  attainSub[attainSub$fips == 12086, "fips"] <- 12025
  # Broomfield and Alleghany counties both in attainment all years 
  basePM <- merge(basePM, attainSub, by="fips", all.x=T, sort=F)
  basePM$attainment_actual <- factor(ifelse(is.na(basePM$attainment_actual), 0, basePM$attainment_actual))
  basePM$attainment_model <- factor(ifelse(basePM$PM > 12, 1, 0))
  return(basePM)
}



for (i in 1:length(years)){
  PMBase[[i]] <- attainComparison(attain.final, PMBase[[i]], year=years[i])
}


confusionMatrix <- function(model, actual){
   tableResults <- table(model, actual)
   colSums <- colSums(tableResults); rowSums <- rowSums(tableResults)
   tableResults[,1] <- tableResults[,1] / sum(tableResults[,1])
   tableResults[,2] <- tableResults[,2] / sum(tableResults[,2])
   tableResults <- round(tableResults*100)
   tableResults <- cbind(tableResults, rowSums)
   tableResults <- rbind(tableResults, c(colSums, sum(colSums)))
   colnames(tableResults) <- c("0 actual (%)", "1 actual (%)", "actual (total)")
   rownames(tableResults) <- c("0 model (%)", "1 model (%)", "model (total)")
   return(tableResults)
}

attainMatch <- function(df){
  return(with(df, confusionMatrix(attainment_model, attainment_actual)))
}

attainMatchData <- llply(PMBase, attainMatch)
# matches attainment status for 85-90% of counties

# explore counties in non-attainment because of outside sources
# modify to classify "external" as out of state
externalShare <- function(colName, PMmatrix, fips, fipsMapping){
  # adjust for Miami-Dade
  fipsMapping[fipsMapping$fips == 12086, "fips"] <- 12025
  # merge to enfore order
  stateMap <- merge(data.frame(fips=fips), fipsMapping[,c("fips", "state")])
  # down select to state
  targetState <- stateMap[stateMap$fips == colName, "state"]
  stateMap$withinState <- ifelse(stateMap$state == targetState, 1, 0)
  internalShare <- sum(stateMap$withinState * PMmatrix[,colName])
  externalShare <- 1-internalShare
  shares <- data.frame(fips=as.character(colName), state=as.character(targetState), internalStateShare=internalShare, externalStateShare=externalShare) 
  return(shares)
}

start <- proc.time()
for (i in 1:length(years)){
  stateShareResults <- data.frame()
  for(fipsCode in as.character(fips)){
    stateShareResults <- rbind(stateShareResults, externalShare(fipsCode, PMShare[[i]], fips, census.fips.sub))
  }
  PMBase[[i]] <- merge(PMBase[[i]], stateShareResults, by="fips", all.x=T)
  rm(stateShareResults)
}
elapsed <- proc.time()-start

attainmentFlip <- function(basePM){
  basePM$PM_internal <- basePM$PM * basePM$internalStateShare
  basePM$attain_withinState <- ifelse(basePM$PM_internal > 12, 1, 0)
  basePM$flipped <- as.numeric(levels(basePM$attainment_actual))[basePM$attainment_actual] - basePM$attain_withinState
  return(basePM)
}


for (i in 1:length(years)){
  PMBase[[i]] <- attainmentFlip(PMBase[[i]])
}

ldply(PMBase, function(x){sum(x$flipped)/sum(x$attainment_actual == 1)})
ldply(PMBase, function(x){sum(x$flipped)})
ldply(PMBase, function(x){sum(x$attainment_actual == 1)})


## Final save ####
setwd(baseWD) 
save.image("County shares results with analysis.RData")
