# county shares analysis code V2

# created March 28, 2017
# V2 created Oct 9, 2017
# Updated July 29, 2018

# Note: can either re-load data or load pre-formatted from prior workspace (see "Pre-saved data" section)

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

# mapping packages
library(maps)
library(mapproj)
library(ggplot2)
library(gridExtra)
library(grid)

## Settings ####
baseWD <- "~/Documents/Carnegie Mellon/Box Sync/Research/Environmental justice"     # home directory (use for relative paths)
years <- c(2008, 2011, 2014)   
VSLs <- c(7876913, 8218104, 8729089)

# directories where each year of AP3 run is stored
resultsWD <- c("/Users/Cartographer/Documents/Carnegie Mellon/Box Sync/Research/AP2/APEEP_Web_2008/PM",
               "/Volumes/Seagate Backup Plus Drive/AP2/APEEP_Web_2011/PM",
               "/Volumes/Seagate Backup Plus Drive/AP2/APEEP_Web_2014/PM")

## FIPS codes ####

# read in FIPS codes from AP2
setwd(paste(baseWD, "Other data", sep="/"))
fips.stacks <- read.dta13("AP2_Fips_List.dta", convert.dates = TRUE, convert.factors = TRUE,
                   missing.type = FALSE, convert.underscore = FALSE)

# assign stack heights
fips.stacks$stack <- c(rep("area",3109), rep("low",3109), rep("med",3109), rep("tall",565), rep("tall2",91))
fips <- fips.stacks[fips.stacks$stack == "area", "fips"]

# edits for fips codes that have changed
fips[fips == 12025] <- 12086
# fips[fips == 51560] <- 51005  (currently in data set, need to handle diffrently)

# census fips codes for mapping to state values
setwd(paste(baseWD, "Other data", sep="/"))
census.fips <- read.csv("Census FIPS codes.txt", header=FALSE, colClasses = "character")
colnames(census.fips) <- c("state", "state.num", "county.num", "county.name", "fips.class")

# create new column with full fips format (state + county)
census.fips$fips <- as.numeric(with(census.fips, paste(state.num, county.num, sep = "")))
census.fips$county.num <- as.numeric(census.fips$county.num)

# adjustment for fips codes that have change
new.rows <- data.frame(state = c("VA"), state.num = c(51), county.num = c(0), 
                       county.name = c("Alleghany County"), fips.class = c("H1"), fips = c(51560))

census.fips <- rbind(census.fips, new.rows)
rm(new.rows)

# create column of abbreviated county names
census.fips$county.name.short <- census.fips$county.name
census.fips$county.name.short <- gsub(" County", "", census.fips$county.name.short)
census.fips$county.name.short <- gsub(" Parish", "", census.fips$county.name.short)
census.fips$county.name.short <- gsub("city", "(City)", census.fips$county.name.short)
census.fips$county.name.short <- gsub("St\\.", "Saint", census.fips$county.name.short)

# map to regions
census.fips$region <- mapvalues(census.fips$state, 
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

census.fips$epa.region <- mapvalues(census.fips$state, 
                                from = c("ME", "NH", "VT", "MA", "RI", "CT", 
                                         "NY", "NJ", 
                                         "MD", "DC", "DE", "PA", "WV", "VA", 
                                         "NC", "SC", "GA", "FL", "AL", "MS", "TN", "KY", 
                                         "LA","AR", "TX", "OK", "NM",
                                         "OH", "IN", "IL", "WI", "MI", "MN", 
                                         "KS", "MO", "IA", "NE", 
                                         "ND", "SD","CO", "UT", "WY", "MT",
                                         "AZ", "NV","CA",
                                         "WA", "OR", "ID"),
                                to = c(rep("New England", 6),
                                       rep("NY/NJ", 2),
                                       rep("Mid-Atlantic", 6),
                                       rep("Southeast", 8),
                                       rep("South", 5),
                                       rep("Midwest", 6),
                                       rep("Great Plains", 4),
                                       rep("Mountain", 6),
                                       rep("Southwest", 3),
                                       rep("Northwest", 3)))

# fips codes corresponding to the data in AP2 only
census.fips.sub <- census.fips[census.fips$fips %in% c(fips,12086), ]

## Load data functions ####

# function to load import/export data by year
load.data <- function(year){

  wd <- resultsWD[which(year==years)]
  setwd(paste(wd, "Other outputs", sep="/")) 
  
  county_shares <- read.csv("Export Import results.csv", header=FALSE)
  
  # relabel columns
  colnames(county_shares) <- c("Export", "Imports", "Own_damage", "Gross_import", "Total_damage",
                               "Export_import_ratio", "Own_import_ratio", "Own_export_ratio")
  
  # merge with data
  county_shares <- cbind(fips, county_shares)
  
  # merge 
  county_shares <- merge(county_shares, census.fips[,c("fips", "county.name", "state", "region", "epa.region")], all.x=TRUE)
  county_shares$year <- year
  
  # population data
  setwd(paste(wd, "Other outputs", sep="/")) 
  pop <- read.csv("Population.csv", header=FALSE)
  pop <- data.frame(population = rowSums(pop))
  county_shares <- cbind(county_shares, pop)
  
  # drop two counties which have zero own damage (Miami-Dade & Colorado where FIPS code changed)
  # also need to check missing counties in LA and WA
  
  county_shares_mod <- county_shares[!(county_shares$Own_damage == 0),]
  #county_shares_mod <- county_shares[!(county_shares$fips %in% c(12025, 51560)),]
  #county_shares_mod <- county_shares
  
  # convert to 2014 dollars
  county_shares_mod[,c("Export","Imports", "Own_damage", "Gross_import", "Total_damage")] <- 
                              apply(county_shares_mod[,c("Export","Imports", "Own_damage", "Gross_import", "Total_damage")], 
                                                                   2, FUN=convert.nominal, nominal.year=year, real.year = 2014)
  
  # calculate net imports (imports - exports)
  county_shares_mod$Net_imports <- county_shares_mod$Imports - county_shares_mod$Export
  
  return(county_shares_mod)
  
}

# function to convert nominal to real
convert.nominal <- function(damages, nominal.year, real.year){
  WTP.nominal <- VSLs[which(nominal.year==years)]
  WTP.real <- VSLs[which(real.year==years)]
  return(damages * WTP.real / WTP.nominal)
}

## Load data ####

# load AP2 county shares output (includes exports, imports, and own damage)
shares <- list()

# load data (note: drops missing counties with "0" own damage, Miami-Dade and one in CO)
for(i in 1:length(years)){
  shares[[i]] <- load.data(years[i])
}

## Per capita estimates ####

# per capita estimates
per.capita <- function(data.vector, pop){
  return(data.vector / pop)
}

# temp. df for per capita exports, imports, and own damage + pop. weighted ratios
shares.pc <- list()

for(i in 1:length(years)){
  county_shares_mod <- shares[[i]]
  
  county_shares_pc <- data.frame(sapply(county_shares_mod[, c("Export", "Imports", "Own_damage", "Net_imports", "Gross_import")], 
                                        FUN = per.capita, 
                                        pop = county_shares_mod[,"population"]))
  
  names(county_shares_pc) <- paste0(names(county_shares_pc), "_per_cap")
  
  shares.pc[[i]] <- cbind(county_shares_mod, county_shares_pc)
  
  # drop values with population of 0
  shares.pc[[i]] <- shares.pc[[i]][shares.pc[[i]][,"population"] != 0, ]
  
}; rm(county_shares_pc); rm(county_shares_mod)



## Statistics ####

# function for computing population weighted average 
pop.weighted.average <- function(data.vector, pop){
  return(sum(data.vector * pop) / sum(pop))
}

stat.summary <- function(county_shares_mod, year, pc=FALSE){
  
  # data summary on all six variables
  data.summary <- sapply(county_shares_mod[,vars], summary)
  data.summary[,vars.sub] <- data.summary[,vars.sub] / 1E6        # convert to millions

  # compute means for damages, population weighted expected value for three ratio variables
  means <- sapply(county_shares_mod[, vars.sub], mean, na.rm=T)
  pop.weighted <- sapply(county_shares_mod[, vars], pop.weighted.average, pop = county_shares_mod[,"population"])
  
  # replace pop weighted means for non-ratios
  pop.weighted[vars.sub] <- pop.weighted[vars.sub] / 1E6  # convert to millions
  
  # check pop weighting function (compare to pop.weighted)
  # sum(county_shares_mod[,"population"] * county_shares_mod[,"Export"]) / sum(county_shares_mod[,"population"]) / 1E6
  
  # combined data summary
  data.summary <- rbind(data.summary, pop.weighted)
  
  # significant figures
  data.summary <- signif(data.summary, 3)
  data.summary[,"Net_imports"] <- signif(data.summary[,"Net_imports"], 2)
  
  # change column names
  colnames(data.summary) <- paste(colnames(data.summary), c(rep(" (million $)", 4), rep("", 3)), sep="")
  colnames(data.summary) <- gsub("_", " ", colnames(data.summary))
  
  rownames(data.summary)[7] <- "Pop. weighted mean"
  
  # save to csv file
  setwd(paste(baseWD, "figures", "Tables", sep="/")) 
  if(pc){
    write.csv(data.summary, paste("Data summary (per capita) ", year, ".csv", sep=""))
  } else {
    write.csv(data.summary, paste("Data summary ", year, ".csv", sep=""))
  }
  
  return(data.summary)
}

stat.summary.var <- function(county_shares_all, var){
  
  # note: currently drops fips codes that aren't in all 3 sets (only Miami-Dade at present)
  year.slice <- merge(county_shares_all[[1]][,c("fips", var)], county_shares_all[[2]][,c("fips", var)], by = "fips")
  year.slice <- merge(year.slice, county_shares_all[[3]][,c("fips", var)], by = "fips")
  
  pop.slice <-  merge(county_shares_all[[1]][,c("fips", "population")], county_shares_all[[2]][,c("fips", "population")], by = "fips")
  pop.slice <- merge(pop.slice, county_shares_all[[3]][,c("fips", "population")], by = "fips")
  
  names(year.slice) <- c("fips", paste(var, years))
  
  # data summary on all six variables
  data.summary <- sapply(year.slice[,2:4], summary)
  
  # pop weighted averages 
  for(i in 2:4){
    if(i == 2){
      pop.weight.mean <- pop.weighted.average(year.slice[,i], pop.slice[,i])
    } else {
      pop.weight.mean <- c(pop.weight.mean, pop.weighted.average(year.slice[,i], pop.slice[,i]))
    }
  }

  # combined data summary
  data.summary <- rbind(data.summary, pop.weight.mean)
  
  # divide by millions for non-ratio or per cap variables
  if(!(grepl("ratio", var) | grepl("per_cap", var))){
    data.summary <- data.summary / 1E6        # convert to millions
  }
  
  # significant figures
  data.summary <- signif(data.summary, 3)
  rownames(data.summary)[7] <- "Pop. weighted mean"
  
  # save to csv file
  setwd(paste(baseWD, "figures", "Tables", sep="/")) 
  write.csv(data.summary, paste("Yearly data summary  ", var, ".csv", sep=""))
  return(data.summary)
}
  
stats <- list()
stats.vars <- list()

vars <- c("Export", "Imports", "Own_damage", "Net_imports", "Export_import_ratio", "Own_import_ratio", "Own_export_ratio")
vars.sub <- c("Export", "Imports", "Own_damage", "Net_imports")

for(i in 1:length(years)){
  stats[[i]] <- stat.summary(shares[[i]], years[i])
}

vars <- c("Export", "Imports", "Own_damage", "Export_import_ratio", "Own_import_ratio", "Own_export_ratio", 
          "Net_imports", "Export_per_cap", "Imports_per_cap", "Own_damage_per_cap", "Net_imports_per_cap")

for(i in vars){
  stats.vars[[i]] <- stat.summary.var(shares.pc, i)
}


# combined stats table

export.combined <- t(ldply(stats, function(x){return(x[,1])}))
import.combined <- t(ldply(stats, function(x){return(x[,2])}))
own.combined <- t(ldply(stats, function(x){return(x[,3])}))
net.import.combined <- t(ldply(stats, function(x){return(x[,4])}))


damages.combined <- cbind(export.combined, import.combined, own.combined, net.import.combined)
colnames(damages.combined) <- rep(years, 4)

setwd(paste(baseWD, "figures", "Tables", sep="/")) 
write.csv(damages.combined, "Summary - Export Imports Own Net.csv")

EI.combined <- t(ldply(stats, function(x){return(x[,5])}))
OI.combined <- t(ldply(stats, function(x){return(x[,6])}))
OE.combined <- t(ldply(stats, function(x){return(x[,7])}))

ratios.combined <- cbind(EI.combined, OI.combined, OE.combined)
colnames(ratios.combined) <- rep(years, 3)

setwd(paste(baseWD, "figures", "Tables", sep="/")) 
write.csv(ratios.combined, "Summary - EI OE OI.csv")

# workspace cleaning
rm(export.combined); rm(import.combined); rm(own.combined); rm(damages.combined); rm(net.import.combined)
rm(EI.combined); rm(OI.combined); rm(OE.combined); rm(ratios.combined)

## CDF plots ####
pop.cumulative <- function(data, ratio){
  return(sum(data[data[,ratio] < 1, "population"]) / sum(data[, "population"]))
}

# table across all 3 values
pop.cdf <- ldply(shares, pop.cumulative, ratio = "Export_import_ratio")
pop.cdf <- cbind(pop.cdf, ldply(shares, pop.cumulative, ratio = "Own_import_ratio"))
pop.cdf <- cbind(pop.cdf, ldply(shares, pop.cumulative, ratio = "Own_export_ratio"))

colnames(pop.cdf) <- c("Export / Import", "Own / Import", "Own / Export")
rownames(pop.cdf) <- c("2008", "2011", "2014")

setwd(paste(baseWD, "figures", "Density", sep="/")) 
write.csv(round(pop.cdf, 3), "Pop weighted CDF.csv", row.names = F)

# panel plot for all regions 

#cols <- c("red", "blue", "green", "purple", "orange")
#regions <- c("NE", "SE", "SW", "MW", "W")

cols <- c("red", "blue", "green", "purple", "orange", "cadetblue1", "brown", "pink", "darkgoldenrod2", "black")
regions <- c("New England", "NY/NJ", "Mid-Atlantic", "Southeast", "Midwest", 
             "South", "Great Plains", "Mountain", "Southwest", "Northwest")

setwd(paste(baseWD, "figures", "Density", sep="/")) 

pdf("CDF EPA Regional All 2008-2014.pdf", width = 12, height = 6)
# layout(matrix(c(1,2,3,4), ncol=1, byrow=TRUE), heights = c(0.31, 0.31, 0.31, 0.07))
layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights = c(0.84, 0.16))
# export/import
par(mar = c(5, 5, 4, 2) + 0.1)
i <- 1
for(region in regions){
  plot.sub.2008 <- sort(shares[[1]]$Export_import_ratio[shares[[1]]$epa.region == region])
  plot.sub.2014 <- sort(shares[[3]]$Export_import_ratio[shares[[3]]$epa.region == region])
  if(region == regions[1]){
    plot(plot.sub.2008, (1:length(plot.sub.2008))/length(plot.sub.2008), type="s", xlim = c(0, 10), 
         main ="Exports / Imports", xlab="Ratio", ylab="CDF", cex.main=2, cex.axis=1.5, cex.lab=1.75, col = cols[i])
  } else{
    lines(plot.sub.2008, (1:length(plot.sub.2008))/length(plot.sub.2008), type="s", lty=1, col = cols[i])
  }
    lines(plot.sub.2014, (1:length(plot.sub.2014))/length(plot.sub.2014), type="s", lty=2, col = cols[i])
  #mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)
  i <- i + 1
}
abline(v=1)
# own/import
par(mar = c(5, 4, 4, 2) + 0.1)
i <- 1
for(region in regions){
  plot.sub.2008 <- sort(shares[[1]]$Own_import_ratio[shares[[1]]$epa.region == region])
  plot.sub.2014 <- sort(shares[[3]]$Own_import_ratio[shares[[3]]$epa.region == region])
  if(region == regions[1]){
    plot(plot.sub.2008, (1:length(plot.sub.2008))/length(plot.sub.2008), type="s", xlim = c(0, 0.5), 
         main ="Self-inflicted / Imports", xlab="Ratio", ylab="", cex.main=2, cex.axis=1.5, cex.lab=1.75, col = cols[i])
    #axis(side=2, labels=F)
  } else{
    lines(plot.sub.2008, (1:length(plot.sub.2008))/length(plot.sub.2008), type="s", lty=1, col = cols[i])
  }
  lines(plot.sub.2014, (1:length(plot.sub.2014))/length(plot.sub.2014), type="s", lty=2, col = cols[i])
  #mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)
  i <- i + 1
}
abline(v=1)
# own/export
par(mar = c(5, 4, 4, 2) + 0.1)
i <- 1
for(region in regions){
  plot.sub.2008 <- sort(shares[[1]]$Own_export_ratio[shares[[1]]$epa.region == region])
  plot.sub.2014 <- sort(shares[[3]]$Own_export_ratio[shares[[3]]$epa.region == region])
  if(region == regions[1]){
    plot(plot.sub.2008, (1:length(plot.sub.2008))/length(plot.sub.2008), type="s", xlim = c(0, 0.8), 
         main ="Self-inflicted / Exports", xlab="Ratio", ylab="", cex.main=2, cex.axis=1.5, cex.lab=1.75, col = cols[i])
    #axis(side=2, labels=F)
  } else{
    lines(plot.sub.2008, (1:length(plot.sub.2008))/length(plot.sub.2008), type="s", lty=1, col = cols[i])
  }
  lines(plot.sub.2014, (1:length(plot.sub.2014))/length(plot.sub.2014), type="s", lty=2, col = cols[i])
  #mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)
  i <- i + 1
}
abline(v=1)
# legend
par(mar = c(0, 0, 0, 0) + 0.1)
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x= "top", legend = regions, col = cols,
       lty= 1, lwd=2, cex=1.75, bty="n", ncol = 5) #horiz=TRUE)
legend(x="bottom", legend = c("2008", "2014"),
       col = c("black", "black"),
       lty  = c(1,2), lwd=2, cex=1.75, bty="n", horiz=TRUE)
dev.off()


## Lorenz curves ####

setwd(paste(baseWD, "figures", "Density", sep="/")) 
# format lorenz plot data
lorenz.plot.data <- function(data,var,sort, absolute=F,decrease=T, colorVar=NULL){
  if(!(is.null(colorVar))){
    if(var == "population"){
      lorenz <- data[, c(var, "epa.region", "county.name", colorVar)]
    } else{
      lorenz <- data[, c(var, "population", "epa.region", "county.name", colorVar)]
    }
  } else{
    lorenz <- data[, c(var, "population", "epa.region", "county.name")]
  }
  # sort on variable (smooth curve)
  # use "decreasing=T" for a reverse Lorenz curve
  if (sort == "damage"){
    lorenz <- lorenz[order(lorenz[,var], decreasing=decrease),]
  } else if (sort == "epa"){
    # sort on region
    lorenz$epa.region <- factor(lorenz$epa.region, 
                                levels=c("Great Plains", "New England", "Mountain", "South", "Southeast", "Northwest", "Southwest", "Midwest", "Mid-Atlantic", "NY/NJ"))
    
    lorenz <- lorenz[order(lorenz[,"epa.region"], lorenz[,var]),]
  }
  
  lorenz$cum.county <- 1:nrow(lorenz) / nrow(lorenz) * 100
  if(absolute){
    lorenz$cum.damages <- cumsum(lorenz[,var])
  } else{
    lorenz$cum.damages <- cumsum(lorenz[,var]) / sum(lorenz[,var]) * 100
  }
  return(lorenz)
}
# year plot by region
lorenzRegionPlot <- function(data, var, ginipos, legend=TRUE, colorVar=NULL, absolute=F, textScale=1,
                             xlab.text="Cumulative counties (%)", ylab.text = "Cumulative share of damages (%)"){
  lorenz <- lorenz.plot.data(data, var, sort="damage", decrease=F, colorVar=colorVar)
  x <- c(0, lorenz$cum.county, 100)
  y <- c(0, lorenz$cum.damages, 100)
  
  #xlab.text <- "Cumulative counties (%)"
  #if(var == "Export"){
  #  ylab.text <- "Cumulative share of exported damages (%)"
  #} else if (var == "Imports"){
  #  ylab.text <- "Cumulative share of imported damages (%)"
  #} else {
  #  ylab.text <- "Cumulative share of own damages (%)"
  #}
  
  if(var == "Export"){
    mainText <- "Exports"
  } else if (var == "Imports"){
    mainText <- "Imports"
  } else {
    mainText <- "Self-inflicted"
  }
  mainText=""
  
  plot(c(0,100), c(0,100), type="l", xaxs="i", yaxs="i", xlab = xlab.text, ylab = ylab.text, cex.lab=2/textScale, cex.main=2/textScale, cex.axis=1.5/textScale, main=mainText)
  polygon(x=c(0,100,100,0), y=c(0,0,100,0), col="lightgrey")
  #lines(c(0,100), c(0,100))
  lines(x,y)
  if(!is.null(colorVar)){
    if(colorVar == "SMSA"){
      colorScheme <- c('#1F78B4', '#E31A1C')
      lorenz[,colorVar] <- mapvalues(lorenz[,colorVar], from=c(0,1), to=c("Non-MSA", "MSA"))
      
      lorenz[,colorVar] <- factor(lorenz[,colorVar])
    } else {
      colorScheme <- c('#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d')
    }
    lorenz$color <- mapvalues(lorenz[,colorVar], from=levels(lorenz[,colorVar]), to=colorScheme)    
    legendVals <- levels(lorenz[,colorVar])    
  } else{
    #colorScheme <- c('#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A') #[c(2,4,6,8,10)]
    colorScheme <- c('#e41a1c','#377eb8','#4daf4a','#984ea3', '#ff7f00') 
    lorenz$epa.region <- factor(lorenz$epa.region)
    
    # map epa regions
    lorenz$epa.region2 <- mapvalues(lorenz$epa.region, from=c("New England", "NY/NJ", "Mid-Atlantic", "Midwest", "Southeast", 
                                                              "South", "Great Plains", "Mountain", "Southwest", "Northwest"),
                                                        to=c("Northeast", "Northeast", "Northeast", "Midwest", "South", 
                                                             "South", "Midwest", "Mountain", "West", "West"))    
    
    
    #legendVals <- c("New England", "NY/NJ", "Mid-Atlantic", "Midwest", "Southeast", 
    #  "South", "Great Plains", "Mountain", "Southwest", "Northwest")
    legendVals <- c("Northeast", "South", "Midwest", "Mountain", "West")
    lorenz$color <- mapvalues(lorenz$epa.region2, from=legendVals, to=colorScheme)    
  }

  lorenz$color <- as.character(lorenz$color)
  for(i in 1:nrow(lorenz)){
    if(i == 1){
      x1 <- 0; y1 = 0
    } else{
      x1 <- lorenz[i-1, "cum.county"]
      y1 <- lorenz[i-1, "cum.damages"]
    }
    x2 <- lorenz[i, "cum.county"]
    y2 <- lorenz[i, "cum.damages"]
    polygon(x=c(x1, x2, x2, x1), y=c(0, 0, y2, y1), col=lorenz$color[i], border=lorenz$color[i])
  }
  x <- c(0, lorenz$cum.county, 100)
  y <- c(0, lorenz$cum.damages, 100)
  
  curveArea <- ABC(x, y)
  ginicoef <- GINI(curveArea)

  text(ginipos[1], ginipos[2], labels=round(ginicoef, 2), cex=2/textScale, font=1)
  if(legend){
    legend("topleft", legend = legendVals, fill = colorScheme, cex=1.5/textScale, bty="n")
  }
}

# calculate area between curves
ABC <- function(x, y){
  AUC <- sum(diff(x/100)*rollmean(y/100, 2))
  ABC <- 0.5 - AUC
  return(ABC)
}
GINI <- function(curveArea){
  return(curveArea /0.5)
}
fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {
  
  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))
  
  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
    
    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }
  
  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }
  
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
  
  x1 <- switch(pos,
               topleft     =x[1] + sw, 
               left        =x[1] + sw,
               bottomleft  =x[1] + sw,
               top         =(x[1] + x[2])/2,
               center      =(x[1] + x[2])/2,
               bottom      =(x[1] + x[2])/2,
               topright    =x[2] - sw,
               right       =x[2] - sw,
               bottomright =x[2] - sw)
  
  y1 <- switch(pos,
               topleft     =y[2] - sh,
               top         =y[2] - sh,
               topright    =y[2] - sh,
               left        =(y[1] + y[2])/2,
               center      =(y[1] + y[2])/2,
               right       =(y[1] + y[2])/2,
               bottomleft  =y[1] + sh,
               bottom      =y[1] + sh,
               bottomright =y[1] + sh)
  
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
  
  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}
# https://logfc.wordpress.com/2017/03/15/adding-figure-labels-a-b-c-in-the-top-left-corner-of-the-plotting-region/

lorenzExports <- lorenz.plot.data(shares[[3]], "Export", sort="damage", absolute=F, decrease=F)
lorenzImports <- lorenz.plot.data(shares[[3]], "Imports", sort="damage", absolute=F, decrease=F)
lorenzOwn <- lorenz.plot.data(shares[[3]], "Own_damage", sort="damage", absolute=F, decrease=F)

threshold <- 40
exportCoord <- lorenzExports[abs(lorenzExports$cum.damages - threshold) < 0.01,]
importCoord <- lorenzImports[abs(lorenzImports$cum.damages - threshold) < 0.05,]
ownCoord <- lorenzOwn[abs(lorenzOwn$cum.damages - threshold) < 0.05,]

giniPos <- c(50, 30)
pdf("Lorenz multi panel regions.pdf", width=12, height=5)
par(mfrow=c(1,3))
par(mar=c(5, 4.5, 8, 4) + 0.1)
lorenzRegionPlot(shares[[3]], "Export", ginipos=giniPos, xlab.text="")
# add lines to plot
arrows(exportCoord$cum.county, 103, 100, 103, length=0.05, angle=90, code=3, lty=1, xpd=TRUE) # horiztonal
mtext(paste0(signif(100 - exportCoord$cum.county, 2), "% of counties"), side = 3, line = 1.5, at = 93, cex = 1, las = 1)
segments(exportCoord$cum.county, threshold, 100, threshold, lty=2)

arrows(103, threshold, 103, 100, length=0.05, angle=90, code=3, lty=1, xpd=TRUE) # vertical
mtext(paste0(100-threshold, "% of exports"), side = 4, line = 1.5, at = 70, cex = 1, las = 0)
segments(exportCoord$cum.county, threshold, exportCoord$cum.county, 100, lty=2)
title(main="Exports", line=6.7, cex.main=2)
fig_label("a", cex=2, font=2)

lorenzRegionPlot(shares[[3]], "Imports", legend=FALSE, ginipos=giniPos, ylab.text = "")

# add lines to plot
arrows(importCoord$cum.county, 103, 100, 103, length=0.05, angle=90, code=3, lty=1, xpd=TRUE) # horiztonal
mtext(paste0(signif(100 - importCoord$cum.county, 1), "% of counties"), side = 3, line = 1.5, at = 93, cex = 1, las = 1)
segments(importCoord$cum.county, threshold, 100, threshold, lty=2)

arrows(103, threshold, 103, 100, length=0.05, angle=90, code=3, lty=1, xpd=TRUE) # vertical
mtext(paste0(100-threshold, "% of imports"), side = 4, line = 1.5, at = 70, cex = 1, las = 0)
segments(importCoord$cum.county, threshold, importCoord$cum.county, 100, lty=2)
title(main="Imports", line=6.7, cex.main=2)
fig_label("b", cex=2, font=2)

lorenzRegionPlot(shares[[3]], "Own_damage", legend=FALSE, ginipos=giniPos, ylab.text = "", xlab.text="")

# add lines to plot
arrows(ownCoord$cum.county, 103, 100, 103, length=0.05, angle=90, code=3, lty=1, xpd=TRUE) # horiztonal
mtext(paste0(signif(100 - ownCoord$cum.county, 1), "% of counties"), side = 3, line = 1.5, at = 93, cex = 1, las = 1)
segments(ownCoord$cum.county, threshold, 100, threshold, lty=2)

arrows(103, threshold, 103, 100, length=0.05, angle=90, code=3, lty=1, xpd=TRUE) # vertical
mtext(paste0(100-threshold, "% of self-inflicted damage"), side = 4, line = 1.5, at = 70, cex = 1, las = 0)
segments(ownCoord$cum.county, threshold, ownCoord$cum.county, 100, lty=2)
title(main="Self-inflicted", line=6.7, cex.main=2)
fig_label("c", cex=2, font=2)

dev.off()



# add county income data
addCountyIncomes <- function(shareData){
  setwd(paste(baseWD, "Other data", "American Community Survey", "Income", sep="/")) 
  
  income.2008 <- read.csv("ACS_09_5YR_S1903_with_ann.csv", skip = 1)
  income.2011 <- read.csv("ACS_11_5YR_S1903_with_ann.csv", skip = 1)
  income.2014 <- read.csv("ACS_14_5YR_S1903_with_ann.csv", skip = 1)
  
  colnames(income.2008) <- colnames(income.2014)
  colnames(income.2011) <- colnames(income.2014)
  
  income.2008 <- income.2008[,c("Id2", "Median.income..dollars...Estimate..Households")]
  income.2011 <- income.2011[,c("Id2", "Median.income..dollars...Estimate..Households")]
  income.2014 <- income.2014[,c("Id2", "Median.income..dollars...Estimate..Households")]
  
  names(income.2008) <- c("fips", "median.income")
  names(income.2011) <- c("fips", "median.income")
  names(income.2014) <- c("fips", "median.income")
  
  shareData[[1]] <- merge(shareData[[1]], income.2008, by="fips")
  shareData[[2]] <- merge(shareData[[2]], income.2011, by="fips")
  shareData[[3]] <- merge(shareData[[3]], income.2014, by="fips")
  
  superIncome <- c(shareData[[1]]$median.income, shareData[[2]]$median.income, shareData[[3]]$median.income)
  
  for(i in 1:3){
    shareData[[i]]$incomeCategory <- cut(shareData[[i]]$median.income, breaks=quantile(superIncome, probs=seq(0,1,by=0.2)), include.lowest=T, 
                                                                          labels = c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%"))    
  }

  return(shareData)
}


incomeShares <- addCountyIncomes(shares)
setwd(paste(baseWD, "figures", "Density", sep="/")) 

pdf("Lorenz multi panel using incomes.pdf", width=12, height=5)
par(mfrow=c(1,3))
par(mar=c(5, 4.5, 4, 2) + 0.1)
lorenzRegionPlot(incomeShares[[3]], "Export", ginipos=c(70, 50), colorVar="incomeCategory")
#fig_label("A", cex=1.25, font=2)
lorenzRegionPlot(incomeShares[[3]], "Imports", legend=FALSE, ginipos=c(70, 50), colorVar="incomeCategory")
#fig_label("B", cex=1.25, font=2)
lorenzRegionPlot(incomeShares[[3]], "Own_damage", legend=FALSE, ginipos=c(70, 50), colorVar="incomeCategory")
#fig_label("C", cex=1.25, font=2)
dev.off()

superPopulation <- c(incomeShares[[1]]$population, incomeShares[[2]]$population, incomeShares[[3]]$population)
for(i in 1:3){
  incomeShares[[i]]$popCategory <- cut(incomeShares[[i]]$population, breaks=quantile(superPopulation, probs=seq(0,1,by=0.2)), include.lowest=T, 
                                       labels = c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%"))    
}

pdf("Lorenz multi panel using population.pdf", width=12, height=5)
par(mfrow=c(1,3))
par(mar=c(5, 4.5, 4, 2) + 0.1)
lorenzRegionPlot(incomeShares[[3]], "Export", ginipos=c(70, 50), colorVar="popCategory")
#fig_label("A", cex=1.25, font=2)
lorenzRegionPlot(incomeShares[[3]], "Imports", legend=FALSE, ginipos=c(70, 50), colorVar="popCategory")
#fig_label("B", cex=1.25, font=2)
lorenzRegionPlot(incomeShares[[3]], "Own_damage", legend=FALSE, ginipos=c(70, 50), colorVar="popCategory")
#fig_label("C", cex=1.25, font=2)
dev.off()


# MSAs

setwd(paste(baseWD, "Other data", sep="/")) 
MSAs <- read.dta13("State_Fips_SMSA.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, convert.underscore = FALSE)
MSAs[MSAs$fips == 12025, "fips"] <- 12086

incomeShares[[3]] <- merge(incomeShares[[3]], MSAs, by="fips", all.x=T)
setwd(paste(baseWD, "figures", "Density", sep="/")) 

pdf("Lorenz multi panel using MSAs.pdf", width=12, height=5)
par(mfrow=c(1,3))
par(mar=c(5, 4.5, 4, 2) + 0.1)
lorenzRegionPlot(incomeShares[[3]], "Export", ginipos=c(70, 50), colorVar="SMSA")
#fig_label("A", cex=1.25, font=2)
lorenzRegionPlot(incomeShares[[3]], "Imports", legend=FALSE, ginipos=c(70, 50), colorVar="SMSA")
#fig_label("B", cex=1.25, font=2)
lorenzRegionPlot(incomeShares[[3]], "Own_damage", legend=FALSE, ginipos=c(70, 50), colorVar="SMSA")
#fig_label("C", cex=1.25, font=2)
dev.off()

# population data
# pdf("Lorenz population curve.pdf", width=6, height=6)
# par(mar=c(5, 4.5, 4, 2) + 0.1)
# lorenzRegionPlot(incomeShares[[3]], "population", ginipos=c(70, 50), colorVar="SMSA", textScale=1.5,
#                  ylab.text = "Cumulative share of population (%)")
# dev.off()

rm(incomeShares)

calcGini <- function(data, var){
  lorenz <- lorenz.plot.data(data, var, sort="damage", absolute=F, decrease=F)
  x <- c(0, lorenz$cum.county, 100)
  y <- c(0, lorenz$cum.damages, 100)
  gini = GINI(ABC(x, y))
  return(gini)
}

ginis <- expand.grid(year=c(2008, 2011, 2014), metric=c("Export", "Imports", "Own_damage"))
ginis$gini <- 0

for(i in c(1:3)){
  for(var in c("Export", "Imports", "Own_damage")){
    ginis[ginis$year == years[i] & ginis$metric == var, "gini"] <- calcGini(shares[[i]], var=var)
  }
}

# call bootstrapping function
source("Lorenze curve bootstrap.R")

## Maps ####
# function to bin points for plotting (accepts either number of bins or set break points which do not include max/min)
map.bins <- function(vector.df, set.breaks){
  quant.breaks <- set.breaks
  quants <- length(set.breaks)

  factor.labels <- rep(paste0("[", signif(quant.breaks[1], 2), ", ", signif(quant.breaks[2],2), "]"), length(vector.df))
  factor.levels <- paste0("[", signif(quant.breaks[1], 2), ", ", signif(quant.breaks[2],2), "]")
  for(i in 2:(quants-1)){
    factor.labels <- ifelse(vector.df > quant.breaks[i], 
                            paste0("[", signif(quant.breaks[i], 2), ", ", signif(quant.breaks[i+1],2), "]"),
                            factor.labels)
    factor.levels <- c(factor.levels, paste0("[", signif(quant.breaks[i], 2), ", ", signif(quant.breaks[i+1],2), "]"))
  }
  # set order
  factor.labels <- factor(factor.labels, levels = factor.levels)
  return(factor.labels)
}
map.plot <- function(county.plot.df, legend.title, title, cols = c("darkblue", "blue", "lightblue", "red", "darkred"), save=TRUE){
  # County choropleth map
  g.plot <- ggplot() + 
    geom_polygon(data=county.plot.df, aes(x=long, y=lat, group = group, fill=plot.cat), 
                 colour="black", size=0.025) + 
    scale_fill_manual(values=cols, na.value="purple") + theme_bw()  +
    labs(fill = legend.title, title = "", x="", y="") + 
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  # add state borders
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  g.plot <- g.plot + geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black")
  if(save){
    ggsave(paste0(title, ".pdf"), g.plot, width = 8, height = 4.5)
  } else {
    return(g.plot)
  }
}

setwd(paste(baseWD, "figures", "Ratios", sep="/")) 

# base map
all_counties <- map_data("county", projection  = "albers", par = c(30,0))

all_counties$polyname <- paste(all_counties$region, all_counties$subregion, sep = ",")
all_counties <- merge(all_counties, county.fips, by="polyname", all.x=T)    # county.fips comes with map package

all_counties[!is.na(all_counties$fips) & all_counties$fips == 8014 , "fips"] <- 8013     # Broomfield, CO

all_counties[all_counties$polyname == "florida,okaloosa", "fips"] <- 12091
all_counties[all_counties$polyname == "virginia,accomack", "fips"] <- 51001
all_counties[all_counties$polyname == "louisiana,st martin", "fips"] <- 22099
all_counties[all_counties$polyname == "washington,pierce", "fips"] <- 53053
all_counties[all_counties$polyname == "texas,galveston", "fips"] <- 48167
all_counties[all_counties$polyname == "north carolina,currituck", "fips"] <- 37053
all_counties[all_counties$polyname == "washington,san juan", "fips"] <- 53055

county.plot <- list()

for(i in 1:length(years)){
  county.plot[[i]] <-  merge(all_counties, shares[[i]], by="fips", all.x=T)
  # sort
  county.plot[[i]] <- county.plot[[i]][order(county.plot[[i]]$group, county.plot[[i]]$order),]
}


# Export / Imports
colorSchemeRatios <- rev(c('#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac'))
colorSchemeRatiosV2 <- rev(c('#b2182b', '#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d'))


ldply(county.plot, function(x){quantile(x$Export_import_ratio, probs=seq(0,1,0.2))})
EI.plot.df <- list()    #c(0.25, 0.75, 1, 1.25, 5)
for(i in 1:length(years)){
  county.plot[[i]]$plot.cat <- map.bins(county.plot[[i]]$Export_import_ratio, 
                                        set.breaks = c(0.05, 0.25, 0.75, 1, 1.5, 5, 400))
  # map.plot(county.plot[[i]], legend.title = "Exports / Imports", title = paste("Export - Import ratio", 
  #                            years[i]), cols = colorSchemeRatios)  
  EI.plot.df[[i]] <- county.plot[[i]]
             #"darkblue", "blue", "indianred1",))
}

# Own / Imports


#  set.breaks = c(0.1, 0.125, 0.15, 1), min.val=0.00019, max.val=12
ldply(county.plot, function(x){quantile(x$Own_import_ratio, probs=seq(0,1,0.2))})

OI.plot.df <- list()
for(i in 1:length(years)){
  county.plot[[i]]$plot.cat <- map.bins(county.plot[[i]]$Own_import_ratio, 
                                        set.breaks = c(0.007, 0.1, 0.15, 0.2, 0.25, 1, 8))
  # map.plot(county.plot[[i]], legend.title = "Own / Imports", title = paste("Own - Import ratio", years[i]), 
  #          cols = colorSchemeRatiosV2)
  OI.plot.df[[i]] <- county.plot[[i]]
}

# Own / Exports
ldply(county.plot, function(x){quantile(x$Own_export_ratio, probs=seq(0,1,0.2))})

OE.plot.df <- list()
for(i in 1:length(years)){
  county.plot[[i]]$plot.cat <- map.bins(county.plot[[i]]$Own_export_ratio, 
                                        set.breaks = c(0.0001, 0.05, 0.1, 0.15, 0.25, 1, 13))
  # map.plot(county.plot[[i]], legend.title = "Own / Export", title = paste("Own - Export ratio", years[i]), 
  #          cols = colorSchemeRatiosV2)
  OE.plot.df[[i]] <- county.plot[[i]]
}


# net imports
for(i in 1:length(years)){
  county.plot[[i]]$plot.cat <- map.bins(county.plot[[i]]$Net_imports/1E6, set.breaks = c(-100, -25, 0, 25, 100))
  # map.plot(county.plot[[i]], legend.title = "Net imports\n(million $)", title = paste("Net imports", years[i]), 
  #          cols = rev(c("darkblue", "blue", "lightblue", "indianred1", "red", "darkred")))
}

# 2008 vs. 2014
county.plot[[1]]$plot.cat <- map.bins(county.plot[[1]]$Net_imports/1E6, set.breaks = c(-100, -25, 0, 25, 100))
net.2008.plot <- map.plot(county.plot[[1]], legend.title = "Net imports\n(million $)", title = paste("Net imports", years[1]), 
                          cols = rev(c("darkblue", "blue", "lightblue", "indianred1", "red", "darkred")), save=F)


county.plot[[3]]$plot.cat <- map.bins(county.plot[[3]]$Net_imports/1E6, set.breaks = c(-100, -25, 0, 25, 100))
net.2014.plot <- map.plot(county.plot[[3]], legend.title = "Net imports\n(million $)", title = paste("Net imports", years[3]), 
                          cols = rev(c("darkblue", "blue", "lightblue", "indianred1", "red", "darkred")), save=F)

# extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# super ratio panel
plots <- list()

plots[[1]] <- map.plot(EI.plot.df[[1]], legend.title = "Export\n/ Import", title = paste("", years[i]), 
                       cols = (colorSchemeRatios), save=F) 
plots[[2]] <- map.plot(EI.plot.df[[3]], legend.title = "Export\n/ Import", title = paste("", years[i]), 
                       cols = (colorSchemeRatios), save=F)
plots[[3]] <- map.plot(OI.plot.df[[1]], legend.title = "Self-inflicted\n/ Import", title = paste("", years[i]), 
                       cols = colorSchemeRatiosV2, save=F) 
plots[[4]] <- map.plot(OI.plot.df[[3]], legend.title = "Self-inflicted\n/ Import", title = paste("", years[i]), 
                       cols = colorSchemeRatiosV2, save=F)
plots[[5]] <- map.plot(OE.plot.df[[1]], legend.title = "Self-inflicted\n/ Export", title = paste("", years[i]), 
                       cols = colorSchemeRatiosV2, save=F) 
plots[[6]] <- map.plot(OE.plot.df[[3]], legend.title = "Self-inflicted\n/ Export", title = paste("", years[i]), 
                       cols = colorSchemeRatiosV2, save=F)

t1 <- arrangeGrob(textGrob(expression(bold("2008"))), textGrob(expression(bold("2014"))), ncol=2)
g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("a"), plots[[2]], ncol=2, widths=c(1, 1.25))
g2 <- arrangeGrob(plots[[3]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("b"), plots[[4]], ncol=2, widths=c(1, 1.25))
g3 <- arrangeGrob(plots[[5]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("c"), plots[[6]], ncol=2, widths=c(1, 1.25))

ratio.panel <- grid.arrange(t1, g1, g2, g3, nrow = 4, heights = c(0.04, 0.32, 0.32, 0.32))

setwd(paste(baseWD, "figures", "Ratios", sep="/")) 
ggsave("Panel of ratios.pdf", ratio.panel, height = 10, width = 10)
rm(g1); rm(g2); rm(g3); rm(t1); rm(ratio.panel)

# Export ratio panel
plots <- list()

plots[[1]] <- map.plot(EI.plot.df[[1]], legend.title = "Export / Import", title = paste("", years[i]), 
                       cols = (colorSchemeRatios), save=F) 
plots[[2]] <- map.plot(EI.plot.df[[3]], legend.title = "Export / Import", title = paste("", years[i]), 
                       cols = (colorSchemeRatios), save=F)

#lg1 <- g_legend(plots[[2]] + theme(legend.key.size=unit(0.5, "cm")))
#  + coord_cartesian(clip="off")

# plots[[2]] <- plots[[2]] + annotate("segment", x=0.1, xend=0.6, y=-3.4, yend=-3.35) 
plots[[2]] <- plots[[2]] + ggtitle("b") + theme(legend.key.size=unit(0.5, "cm"), plot.title = element_text(face="bold")) +
              annotation_custom(grob=segmentsGrob(arrow=arrow(angle=90, length=unit(0.1, "cm"), ends="both"), gp=gpar(col=colorSchemeRatios[1])),
                                xmin=0.48, xmax=0.48, ymin=-3.351, ymax=-3.254) + 
              annotation_custom(grob=segmentsGrob(arrow=arrow(angle=90, length=unit(0.1, "cm"), ends="both"), gp=gpar(col=colorSchemeRatios[6])),
                                xmin=0.48, xmax=0.48, ymin=-3.3549, ymax=-3.453) +
              annotation_custom(grob=textGrob(label="Net\nexporter", gp=gpar(fontsize=10, col=colorSchemeRatios[6])),
                                xmin=0.41, xmax=0.41, ymin=-3.40395, ymax=-3.40395) + 
              annotation_custom(grob=textGrob(label="Net\nimporter", gp=gpar(fontsize=10, col=colorSchemeRatios[1])),
                                xmin=0.41, xmax=0.41, ymin=-3.3025, ymax=-3.3025) #x = 0.4
  

gt <- ggplot_gtable(ggplot_build(plots[[2]]))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

#t1 <- arrangeGrob(textGrob(expression(bold("2008"))), textGrob(expression(bold("2014"))), ncol=2)
#g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("A"), 
#                  plots[[2]] + ggtitle("B") + theme(legend.key.size=unit(0.5, "cm"), plot.title = element_text(face="bold")), 
#                  ncol=2, widths=c(1, 1.28))

g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("a"), 
                  gt, ncol=2, widths=c(1, 1.25))

ratio.panel <- grid.arrange(g1, nrow = 1, widths = c(1))

setwd(paste(baseWD, "figures", "Ratios", sep="/")) 
ggsave("Panel of Export Import ratios.pdf", ratio.panel, height=3.2, width=10)

rm(g1); rm(g2); rm(g3); rm(t1)

# change in exports, imports, and own damages

shift <- merge(shares[[1]][,c("fips", "Export", "Imports", "Own_damage")], shares[[3]][,c("fips", "Export", "Imports", "Own_damage")], by = "fips")
names(shift) <- c("fips", "Export_2008", "Imports_2008", "Own_damage_2008", "Export_2014", "Imports_2014", "Own_damage_2014")
shift$Export_change <- shift$Export_2014 - shift$Export_2008
shift$Import_change <- shift$Imports_2014 - shift$Imports_2008
shift$Own_change <- shift$Own_damage_2014 - shift$Own_damage_2008

# set up map for plotting
shift.plot <-  merge(all_counties, shift, by="fips", all.x=T)
shift.plot <- shift.plot[order(shift.plot$group, shift.plot$order),]

shift.plot$plot.cat <- map.bins(shift.plot$Export_change/1E6, set.breaks = c(-10, 0, 10, 50))
map.plot(shift.plot, legend.title = "Change in exports\n(million $)", title = paste("Change in exports 2008 to 2014"), 
         cols = c("blue", "lightblue", "indianred1", "red", "darkred"))
  
shift.plot$plot.cat <- map.bins(shift.plot$Import_change/1E6, set.breaks = c(-25, 2, 0, 2, 25))
map.plot(shift.plot, legend.title = "Change in imports\n(million $)", title = paste("Change in imports 2008 to 2014"), 
         cols = c("darkblue", "blue", "lightblue", "indianred1", "red", "darkred"))

shift.plot$plot.cat <- map.bins(shift.plot$Own_change/1E6, set.breaks = c(-10, 0, 10, 50))
map.plot(shift.plot, legend.title = "Change in own damage\n(million $)", title = paste("Change in own damage 2008 to 2014"), 
         cols = c("darkblue", "blue", "lightblue", "indianred1", "red", "darkred"))


# basic maps

# Exports
i <- 1
county.plot[[i]]$plot.cat <- map.bins(county.plot[[i]]$Export/1E6, set.breaks = c(5, 75, 150, 300, 1000, 16000))
map.plot(county.plot[[i]], legend.title = "Exports\n(million $)", title = paste("Exports", years[i]), 
         cols = c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000"))

# Imports
county.plot[[i]]$plot.cat <- map.bins(county.plot[[i]]$Import/1E6, set.breaks = c(0, 30, 70, 150, 330, 26000))
map.plot(county.plot[[i]], legend.title = "Imports\n(million $)", title = paste("Imports", years[i]), 
         cols = c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d"))

## SAVE ####
setwd(baseWD) 
save.image("County shares results V2.RData")


