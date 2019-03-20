# Damages incurred maps
# created September 20, 2017
# updated Jan 11, 2018

## Notes: in some cases marginal damages very close to zero are read as negatives; for the purpose of this analysis these values are set to zero.

library(data.table)
library(readstata13)
library(maps)
library(mapproj)
library(ggplot2)
library(plyr)
library(reshape2)
library(plotrix)
library(grid)
library(gridExtra)
library(gtable)

## Settings ####

# set working directories
baseWD <- "~/Documents/Carnegie Mellon/Box Sync/Research/Environmental justice"

# directories where each year of AP3 run is stored
resultsWD <- c("/Users/Cartographer/Documents/Carnegie Mellon/Box Sync/Research/AP2/APEEP_Web_2008/PM",
               "/Volumes/Seagate Backup Plus Drive/AP2/APEEP_Web_2011/PM",
               "/Volumes/Seagate Backup Plus Drive/AP2/APEEP_Web_2014/PM")

# directory where emissions are stored (see output from Emissions breakdown V3.R)
emissionsWD <- "/Volumes/Seagate Backup Plus Drive/AP2/NEI/AP2 inputs"

sensitivityWD <- "/Users/Cartographer/Documents/Carnegie Mellon/Box Sync/Research/AP2/Sensitivity"

# directory with MD summaries
mdWD <- "~/Documents/Carnegie Mellon/Box Sync/Research/AP2/Calibration"

# base parameters for analysis
years <- c(2008, 2011, 2014)
VSLs <- c(7876913, 8218104, 8729089)

# add GDP data (trillion $)
GDP <- data.frame(Year = factor(years), GDP=c(16331, 16475, 17521)/1E3)

## Functions for loading data ####

# function to load marginal damage and emissions data by year
loadEmissions <- function(year){
  setwd(emissionsWD)
  # read in emissions. one sheet per stack, one column per pollutant
  # order from AP2: NH3, NOx, PM_10, PM_25, SO2, VOCs
  emissions <- list()
  sourceList <- c("area sources", "low stacks", "medium stacks", "tall stacks", "tall 2 stacks")
  for(stack_height in sourceList){
    index <- which(stack_height == sourceList)
    emissions[[index]] <- as.matrix(fread(paste(year, " - ", stack_height, ".csv", sep="")))
    # remove PM10
    emissions[[index]] <- emissions[[index]][,c(1,2,4,5,6,7)]
    colnames(emissions[[index]]) <- c("NH3", "NOx", "PM_25", "SO2", "VOC_A", "VOC_B")
    # zero out VOC_B (do not include biogenics in valuation of total damages)
    # currently not included in damage count
    # emissions[[i]][,"VOC_B"] <- 0
  }
  names(emissions) <- c("Area", "Low stack", "Medium stack", "Tall stack", "New Tall stack")
  return(emissions)
}

loadMD <- function(year, run){
  wd <- resultsWD[which(year==years)]
  setwd(paste(wd, "Marginal damages", run, sep="/")) 
  marginal.damages <- list()
  i <- 0
  # read marginal damages for stack heights and pollutants
  # each marginal damage is a matrix of source (row) to receptor of damages (column)
  # this takes about 4 minutes to run
  for(stack_height in c("Area", "Low", "Medium", "Tall", "New Tall")){
    for(pollutant in c("NH3", "NOx", "PM25", "SO2", "VOC")){
      i <- i + 1
      print(paste("Reading ", year, " ", stack_height, " MD receptor matrix ", pollutant, ".csv...", sep =""))
      marginal.damages[[i]] <- as.matrix(fread(paste(stack_height," MD receptor matrix ", pollutant, ".csv", sep ="")))
      names(marginal.damages)[i] <- paste(stack_height, pollutant) 
    }
  }
  return(marginal.damages)
}

loadMDSummary <- function(year, run){
  wd <- resultsWD[which(year==years)]
  setwd(paste(wd, "Marginal damages", run, sep="/")) 
  marginal.damages <- as.data.frame(fread("All MD results.csv"))
  colnames(marginal.damages) <- c("NH3", "NOx", "PM10", "PM25", "SO2", "VOC")
  marginal.damages <- cbind(fips.stacks, marginal.damages)
  return(marginal.damages)
}

loadPopData <- function(year){
  wd <- resultsWD[which(year==years)]
  setwd(paste(wd, "Other outputs", sep="/")) 
  pop <- read.csv("Population.csv", header=FALSE)
  pop <- cbind(fips, rowSums(pop))
  colnames(pop) <- c("fips", "population")
  return(as.data.frame(pop))
}

loadMortality <- function(filename){
  mort <- c()
  for (wd in resultsWD){
    setwd(paste(wd, "Other outputs", sep="/"))
    mortTemp <- read.csv(filename, header=F)
    mort <- c(mort, mortTemp[1,])
  }
  return(mort)
}

## Functions to calculate damages ####

# function to load marginal damage and emissions data by year
calcDamages <- function(marginal.damages, emissions){
  
  # calculate total damages caused by source county from all pollutants
  area.damages <- matrix(0, nrow = 3109, ncol = 3109)
  low.damages <- matrix(0, nrow = 3109, ncol = 3109)
  medium.damages <- matrix(0, nrow = 3109, ncol = 3109)
  tall.damages <- matrix(0, nrow = 565, ncol = 3109)
  new.tall.damages <- matrix(0, nrow = 91, ncol = 3109)
  
  # multiply emissions by rows to get total damages across all receptors
  # order of emissions: NH3, NOx, PM_25, SO2, VOC_A
  for(i in 1:5){
    area.damages <- area.damages + sweep(marginal.damages[[i]], emissions[[1]][,i], MARGIN=1, FUN="*")
    low.damages <- low.damages + sweep(marginal.damages[[i + 5]], emissions[[2]][,i], MARGIN=1, FUN="*")
    medium.damages <- medium.damages + sweep(marginal.damages[[i + 10]], emissions[[3]][,i], MARGIN=1, FUN="*")
    tall.damages <- tall.damages + sweep(marginal.damages[[i + 15]], emissions[[4]][,i], MARGIN=1, FUN="*")
    new.tall.damages <- new.tall.damages + sweep(marginal.damages[[i + 20]], emissions[[5]][,i], MARGIN=1, FUN="*")
  }
  
  return(list(area.damages, low.damages, medium.damages, tall.damages, new.tall.damages))
}
# total damage by receptor county
calcIncurredDamages <- function(marginal.damages, emissions, area=T, byStack=F){
  # full damage SR matrix
  damages <- calcDamages(marginal.damages, emissions)
  # calculate incurred damage by county
  if(area){
    total.damages <- ldply(damages, colSums)
  } else {
    total.damages <- ldply(damages[2:5], colSums)   # exclude area sources if only calculating point sources
  }
  if(byStack){
    total.damages <- t(total.damages)
    total.damages <- cbind(fips, total.damages)
    colnames(total.damages) <- c("fips", "area", "low", "medium", "tall", "tall2")
  } else{
    total.damages <- rowSums(t(total.damages))
    total.damages <- data.frame(fips = fips, damages = total.damages)
  }

  return(total.damages)
}
# function to create sparse damage matrix for tall damages
tallFormatting <- function(tallDamages, type){
  
  tall.damages.df <- cbind(fips.stacks[fips.stacks$stack == type, "fips"], tallDamages)
  # summarize damages by plants in same fips
  tall.damages.df <- data.frame(tall.damages.df)
  names(tall.damages.df)[1] <- "fips"
  tall.damages.df <- aggregate(. ~ fips, data = tall.damages.df, FUN = sum)
  names(tall.damages.df) <- NULL
  
  # extract list of non-tall fips, create sparse matrix
  non.tall.fips <- fips[!(fips %in% fips.stacks[fips.stacks$stack == type, "fips"])]
  sparse.tall <- matrix(0, nrow = length(non.tall.fips), ncol = 3109)
  sparse.tall <- cbind(non.tall.fips, sparse.tall)
  
  # format for merge
  names(tall.damages.df) <- NULL; colnames(sparse.tall) <- NULL
  tall.damages.df <- as.matrix(tall.damages.df)
  
  # merge sparse and tall martix, sort and remove fips
  tall.damages.df <- rbind(tall.damages.df, sparse.tall)
  tall.damages.df <- tall.damages.df[order(tall.damages.df[,1]),]
  tall.damages.df <- tall.damages.df[,-1]
  
  return(tall.damages.df)
}
# function to load marginal damage and emissions data by year
calcDamageMatrix <- function(marginal.damages, emissions, area=TRUE){
  
  # full damage SR matrix by 5 stack categories
  damages <- calcDamages(marginal.damages, emissions)
  
  # combine list elements by matching fips codes (irregular for tall and new tall)
  damages[[4]] <- tallFormatting(damages[[4]], "tall")
  damages[[5]] <- tallFormatting(damages[[5]], "tall2")
  
  # total damages by receptor
  if(area){
    total.damages <- Reduce("+", damages)
  } else {
    total.damages <- Reduce("+", damages) - damages[[1]]  # subtract area sources to get damages from point sources only
  }
  return(total.damages)
}

# function to load marginal damage and emissions data by year by stack height
calcDamagesByPollutant <- function(marginal.damages, emissions){

  # multiply emissions by rows to get total damages across all receptors
  for(i in 1:5){
    area.damages <- sweep(marginal.damages[[i]], emissions[[1]][,i], MARGIN=1, FUN="*")
    low.damages <- sweep(marginal.damages[[i + 5]], emissions[[2]][,i], MARGIN=1, FUN="*")
    medium.damages <- sweep(marginal.damages[[i + 10]], emissions[[3]][,i], MARGIN=1, FUN="*")
    tall.damages <- sweep(marginal.damages[[i + 15]], emissions[[4]][,i], MARGIN=1, FUN="*")
    new.tall.damages <- sweep(marginal.damages[[i + 20]], emissions[[5]][,i], MARGIN=1, FUN="*")
    
    area.damages.sum <- sum(colSums(area.damages))
    low.damages.sum <- sum(colSums(low.damages))
    medium.damages.sum <- sum(colSums(medium.damages))
    tall.damages.sum <- sum(colSums(tall.damages))
    new.tall.damages.sum <- sum(colSums(new.tall.damages))
    
    if (i == i){
      damages <- data.frame(area = area.damages.sum, low = low.damages.sum, medium = medium.damages.sum,
                           tall = tall.damages.sum, newTall = new.tall.damages.sum)
    } else{
      newDF <- data.frame(area = area.damages.sum, low = low.damages.sum, medium = medium.damages.sum,
                                  tall = tall.damages.sum, newTall = new.tall.damages.sum)
      
      damages <- rbind(damages, newDF)
    }
  }
  rownames(damages) <- c("NH3", "NOx", "PM25", "SO2", "VOC")
  return(damages)
}


## Functions for plotting maps of results ####

# labels for multipaneled figures
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
# Source: https://logfc.wordpress.com/2017/03/15/adding-figure-labels-a-b-c-in-the-top-left-corner-of-the-plotting-region/

# function to bin points for plotting (accepts either number of quantiles or set breaks)
map.bins <- function(vector.df, quants=5, set.breaks){
  if(is.null(set.breaks)){
    quant.breaks <- quantile(vector.df, seq(0, 1, length.out = quants), na.rm=T)
  } else {
    quant.breaks <- set.breaks
    quants <- length(set.breaks)
  }
  factor.labels <- rep(paste0("[", signif(quant.breaks[1], 2), ", ", signif(quant.breaks[2],2), "]"), length(vector.df))
  factor.levels <- paste0("[", signif(quant.breaks[1], 2), ", ", signif(quant.breaks[2],2),"]")
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

# function to plot map of damages 
countyDamagesMap <- function(damages, targetVar, legend.title, title, millions=T, limits=NULL, binary=F,
                               set.breaks=NULL, state.subset=NULL, discrete=T, quants=5, cols=NULL, save=T){
  
  damages <- damages[!is.na(damages[,targetVar]),]
  
  all_counties <- map_data("county", projection  = "albers", par = c(30,0))
  all_counties$polyname <- paste(all_counties$region, all_counties$subregion, sep = ",")
  all_counties <- merge(all_counties, county.fips, by="polyname", all.x=T)    # county.fips comes with map package
  # some fips code adjustments
  all_counties[!is.na(all_counties$fips) & all_counties$fips == 8014 , "fips"] <- 8013     # Broomfield, CO
  all_counties[all_counties$polyname == "florida,okaloosa", "fips"] <- 12091
  all_counties[all_counties$polyname == "virginia,accomack", "fips"] <- 51001
  all_counties[all_counties$polyname == "louisiana,st martin", "fips"] <- 22099
  all_counties[all_counties$polyname == "washington,pierce", "fips"] <- 53053
  all_counties[all_counties$polyname == "texas,galveston", "fips"] <- 48167
  all_counties[all_counties$polyname == "north carolina,currituck", "fips"] <- 37053
  all_counties[all_counties$polyname == "washington,san juan", "fips"] <- 53055
  damages[!is.na(damages$fips) & damages$fips == 12025, "fips"] <- 12086    # Miami-Dade, FL
  # subset states
  if(!is.null(state.subset)){
    all_counties <- subset(all_counties, region %in% state.subset)
  }
  county.plot <- merge(all_counties, damages, by="fips", all=T)
  # sort
  county.plot <- county.plot[order(county.plot$group, county.plot$order),]
  # convert damages to millions
  if(millions){
    county.plot[,targetVar] <- county.plot[,targetVar] / 1E6
  }
  setwd(paste(baseWD, "figures", "Maps", sep="/"))
  if(discrete){
    county.plot$plot.cat <- map.bins(county.plot[,targetVar], quants = quants, set.breaks)
    # set colors
    #cols <- c("#f1eef6", "#bdc9e1", "#74a9cf", "#0570b0")
    if(is.null(cols)){
      cols <- c("#f1eef6", "#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d")[1:quants]
    }
    # County choropleth map
    g.plot <- ggplot() + 
      geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=plot.cat), 
                   colour="black", size=0.05) + 
      scale_fill_manual(values=cols, na.value="purple") + theme_bw()  +
      labs(fill = legend.title, title = "", x="", y="") + 
      scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
    #coord_fixed(1.3) +
    #geom_point(aes(x=mapproject(40.6348, -80.4159, projection  = "albers", par = c(30,0))$x, 
    #               y=mapproject(40.6348, -80.4159, projection  = "albers", par = c(30,0))$y), color = "yellow", size = 3)
  } else if (binary){
    county.plot$binary <- ifelse(county.plot[,targetVar] < 0, "Decreased damages", "Increased damages")
    g.plot <- ggplot() + 
      geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=binary), 
                   colour="black", size=0.01) + 
      scale_fill_manual(values=c("#2b8cbe", "#de2d26"), na.value="purple") + theme_bw()  +
      labs(fill = legend.title, title = "", x="", y="") + 
      scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  } else {
    if(is.null(cols)){
      cols <- c("#2b8cbe", "#de2d26")
    }
    county.plot$target <- county.plot[,targetVar]
    g.plot <- ggplot() + 
      geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=target), 
                   colour="black", size=0.01) + 
      scale_fill_gradient2(low=cols[1], high=cols[2], midpoint=0) + theme_bw()  +
      labs(fill = legend.title, title = "", x="", y="") + 
      scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  }
  # add state borders
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  # subset states
  if(!is.null(state.subset)){
    all_states <- subset(all_states, region %in% state.subset)
  }
  g.plot <- g.plot + geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black")
  if(save){
    ggsave(paste0(title, ".pdf"), g.plot, width = 8, height = 4.5)
  }
  return(g.plot)
}

# extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

## Function for inflation adjustment ####

# function to convert nominal to real
convertToNominal <- function(damageVals, nominal.year, real.year){
  if(nominal.year == 2008){
    WTP.nominal <- VSLs[1]
  } else if(nominal.year == 2011){
    WTP.nominal <- VSLs[2]
  } else if(nominal.year == 2014){
    WTP.nominal <- VSLs[3]
  }
  if(real.year == 2008){
    WTP.real <- VSLs[1]
  } else if(real.year == 2011){
    WTP.real <- VSLs[2]
  } else if(real.year == 2014){
    WTP.real <- VSLs[3]
  }
  return(damageVals * WTP.real / WTP.nominal)
}

inflateDamages <- function(df, var, yearFrom, yearTo){
  df[,var] <- convertToNominal(df[,var], nominal.year=yearFrom, real.year=yearTo)
  return(df)
}


## Load fips codes ####

# read in FIPS codes
setwd(paste(baseWD, "Other data", sep="/"))
fips <- read.dta13("AP2_Fips_List.dta", convert.dates = TRUE, convert.factors = TRUE,
                   missing.type = FALSE, convert.underscore = FALSE)

# assign stack heights
fips$stack <- c(rep("area",3109), rep("low",3109), rep("med",3109), rep("tall",565), rep("tall2",91))
fips.stacks <- fips
fips <- fips[fips$stack == "area", "fips"]

# separate FIPS data for name of each county
census.fips <- read.csv("Census FIPS codes.txt", header=FALSE, colClasses = "character")
colnames(census.fips) <- c("state", "state.num", "county.num", "county.name", "fips.class")

# create new column with full fips format (state + county)
census.fips$fips <- as.numeric(with(census.fips, paste(state.num, county.num, sep = "")))
census.fips$county.num <- as.numeric(census.fips$county.num)

## Baseline damages ####

mergeDamages <- function(df1, df2, cNames){
  df <- merge(df1, df2, by="fips", all=T)
  colnames(df) <- c("fips", cNames)
  return(df)
}

# adjustment for Bedford city (fips code merged over course of study)
adjustMissingFips <- function(df){
  # only need to change 2008
  df[df$fips == 51019, "damages"] <- df[df$fips == 51019, "damages"] + df[df$fips == 51515, "damages"]
  if(sum(colnames(df)=="pointDamages")){
    df[df$fips == 51019, "pointDamages"] <- df[df$fips == 51019, "pointDamages"] + df[df$fips == 51515, "pointDamages"]
  }
  df[df$fips == 51019, "population"] <- df[df$fips == 51019, "population"] + df[df$fips == 51515, "population"]
  return(df)
}

# load main data and save into variables
emissions <- llply(years, loadEmissions)
md <- llply(years, loadMD, run="Baseline")

# calculate damages by county
damages <- mapply(calcIncurredDamages, md, emissions, SIMPLIFY=F)
damagesPoint <- mapply(calcIncurredDamages, md, emissions, area=F, SIMPLIFY=F)
damages <- mapply(mergeDamages, df1=damages, df2=damagesPoint, MoreArgs = list(cNames =c("damages", "pointDamages")),  SIMPLIFY=F); rm(damagesPoint)

# inflation adjustment (convert to $2014)
damages <- mapply(inflateDamages, damages, var="damages", yearFrom=years, yearTo=2014, SIMPLIFY=F)
damages <- mapply(inflateDamages, damages, var="pointDamages", yearFrom=years, yearTo=2014, SIMPLIFY=F)

# calculate per capita values
pop <- llply(years, loadPopData)
damages <- mapply(mergeDamages, df1=damages, df2=pop, MoreArgs=list(cNames =c("damages", "pointDamages", "population")),  SIMPLIFY=F)
damages[[1]] <- adjustMissingFips(damages[[1]])

# per capita values
calcPerCapitaDamages <- function(df, vars, popVar="population"){
  for(var in vars){
    newVar <- paste(var, "PerCap", sep="")
    df[,newVar] <- df[,var] / df[,popVar]
  }
  return(df)
}
damages <- llply(damages, calcPerCapitaDamages, vars=c("damages", "pointDamages"))

# calculate difference between 2008 and 2014
calcDifference <- function(df1, df2, vars){
  df <- merge(df1[,c("fips", vars)], df2[,c("fips", vars)], by="fips")  
  
  for (var in vars){
    df[,var] <- df[,paste(var, ".y", sep="")] - df[,paste(var, ".x", sep="")]
    df[,paste(var, ".y", sep="")] <- NULL
    df[,paste(var, ".x", sep="")] <- NULL
  }
  return(df)
}

diffs <- list()
diffs[[1]] <- calcDifference(damages[[1]], damages[[2]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2008 to 2011
diffs[[2]] <- calcDifference(damages[[1]], damages[[3]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2008 to 2014
diffs[[3]] <- calcDifference(damages[[2]], damages[[3]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2011 to 2014

# damage matrices
damageMatrices <- mapply(calcDamageMatrix, md, emissions, SIMPLIFY=F)
damageMatricesPoint <- mapply(calcDamageMatrix, md, emissions, area=F, SIMPLIFY=F)

damageMatrices[[1]] <- convertToNominal(damageMatrices[[1]],nominal.year=2008, real.year=2014)
damageMatrices[[2]] <- convertToNominal(damageMatrices[[2]],nominal.year=2011, real.year=2014)

damageMatricesPoint[[1]] <- convertToNominal(damageMatricesPoint[[1]],nominal.year=2008, real.year=2014)
damageMatricesPoint[[2]] <- convertToNominal(damageMatricesPoint[[2]],nominal.year=2011, real.year=2014)

## Baseline maps #### 
setwd(paste(baseWD, "figures", "Maps", sep="/"))

## 1. total damages by year

quantile(damages[[1]]$damages, na.rm=T)/1E6
quantile(damages[[3]]$damages, na.rm=T)/1E6

mapply(countyDamagesMap, damages, title=paste("Total damages (", years, ")", sep=""), 
                          MoreArgs=list(targetVar="damages", legend.title="Damages (million $)", 
                                        cols =c('#fef0d9','#fdd49e','#fdbb84','#fc8d59','#e34a33','#b30000'),
                                        set.breaks = c(0, 50, 100, 250, 1000, 10000, 101000),millions = TRUE),  SIMPLIFY=F)

## 2. change in damages
quantile(diffs[[1]]$damages, na.rm=T)/1E6
quantile(diffs[[2]]$damages, na.rm=T)/1E6
quantile(diffs[[3]]$damages, na.rm=T)/1E6

damagesPlotTitles <- c("Change in damages (2008 to 2011)",
                       "Change in damages (2008 to 2014)",
                       "Change in damages (2011 to 2014)")

damagePlots <- list()
for(i in 1:length(years)){
  
  damagePlots[[i]] <- countyDamagesMap(diffs[[i]], targetVar="damages", 
                                       legend.title = "Change in damages\n(million $)", 
                                       title=damagesPlotTitles[i],
                                       millions = TRUE, set.breaks = c(-13100, -500, -100, 0, 100, 500, 1750),
                                       cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"))
}; rm(damagesPlotTitles); rm(damagePlots)


## 3. change in point damages

# change in damages
quantile(diffs[[1]]$pointDamages, na.rm=T)/1E6
quantile(diffs[[2]]$pointDamages, na.rm=T)/1E6
quantile(diffs[[3]]$pointDamages, na.rm=T)/1E6

damagesPlotTitles <- c("Change in point source damages (2008 to 2011)",
                       "Change in point source damages (2008 to 2014)",
                       "Change in point source damages (2011 to 2014)")

damagePlots <- list()
for(i in 1:length(years)){
  
  damagePlots[[i]] <- countyDamagesMap(diffs[[i]], targetVar="pointDamages", 
                                       legend.title = "Change in damages\n(million $)", 
                                       title=damagesPlotTitles[i],
                                       millions = TRUE, set.breaks = c(-3300, -50, -10, 0, 10, 50, 410),
                                       cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"))
}; rm(damagesPlotTitles); rm(damagePlots)

## 4. change in per capita damages
quantile(diffs[[1]]$damagesPerCap, na.rm=T)
quantile(diffs[[2]]$damagesPerCap, na.rm=T)
quantile(diffs[[3]]$damagesPerCap, na.rm=T)

perCapPlotTitles <- c("Per capita change in damages (2008 to 2011)",
                      "Per capita change in damages (2008 to 2014)",
                      "Per capita change in damages (2011 to 2014)")
perCapPlots <- list()

ldply(diffs, function(x){quantile(x$damagesPerCap, probs=seq(0,1,0.2), na.rm=T)})
perCapScale <- c(-4000, -1000, -500, 0, 500, 1000, 3000)

for(i in 1:length(years)){
  perCapPlots[[i]] <- countyDamagesMap(diffs[[i]], targetVar="damagesPerCap", 
                                  legend.title = "Change in damages\n($ per person)", 
                                  title=perCapPlotTitles[i],
                                  millions = FALSE, set.breaks = perCapScale,
                                  cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"))
}

## 4.1 per capita panel plot
mylegend <- g_legend(perCapPlots[[1]])
g1 <- arrangeGrob(perCapPlots[[1]] + ggtitle("a") + theme(plot.title = element_text(face="bold"), legend.position = "none")) 
g3 <- arrangeGrob(perCapPlots[[3]] + ggtitle("b") + theme(plot.title = element_text(face="bold"), legend.position = "none")) 
g2 <- arrangeGrob(mylegend) 

perCapPanel <- grid.arrange(grobs = list(g1, g2, g3), widths=c(3, 1), layout_matrix=rbind(c(1,2),c(3,2)) )

setwd(paste(baseWD, "figures", "Maps", sep="/"))
ggsave("Per capita change in damages - panel.pdf", perCapPanel, height=6, width=6)


rm(perCapPanel); rm(g1); rm(perCapPlots)


## 5. change in per capita damages from point sources

perCapPlotTitles <- c("Per capita change in point source damages (2008 to 2011)",
                      "Per capita change in point source damages (2008 to 2014)",
                      "Per capita change in point source damages (2011 to 2014)")
perCapPlots <- list()
for(i in 1:length(years)){
  perCapPlots[[i]] <- countyDamagesMap(diffs[[i]], targetVar="pointDamagesPerCap", 
                                       legend.title = "Change in damages\n($ per person)", 
                                       title=perCapPlotTitles[i],
                                       millions = FALSE, set.breaks = perCapScale,
                                       cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"))
}

## Emissions ####

readSectorEmissions <- function(sectorName, year){
  if(sectorName=="all"){
    setwd(emissionsWD)
  } else{
    setwd(paste(emissionsWD, "Sector emissions", sectorName, sep="/"))
  }
  
  area <- read.csv(paste(year, "- area sources.csv"), header=F)
  low <- read.csv(paste(year, "- low stacks.csv"), header=F)
  med <- read.csv(paste(year, "- medium stacks.csv"), header=F)
  tall1 <- read.csv(paste(year, "- tall stacks.csv"), header=F)
  tall2 <- read.csv(paste(year, "- tall 2 stacks.csv"), header=F)
  
  return(list(area, low, med, tall1, tall2))
}

emissionsSummary <- llply(c(2008, 2011, 2014), readSectorEmissions, sectorName="all")
emissionsSummaryTotals <- llply(emissionsSummary, function(x){df <- ldply(x, colSums); colnames(df)<-c("NH3", "NOX", "PM10", "PM25", "SO2", "VOC_A", "VOC_B"); return(df)})
emissionsSummaryTotals <- ldply(emissionsSummaryTotals, colSums)

(emissionsSummaryTotals[2,] - emissionsSummaryTotals[1,])/ emissionsSummaryTotals[1,] *100
(emissionsSummaryTotals[3,] - emissionsSummaryTotals[2,])/ emissionsSummaryTotals[2,] *100
(emissionsSummaryTotals[3,] - emissionsSummaryTotals[1,])/ emissionsSummaryTotals[1,] *100

## Total damages ####

nationalDamagesSum <- function(damage){
  return(colSums(damage[c("damages", "pointDamages")]))
}
totalDamagePlot <- function(totalDamage, save=F, plotName=""){
  g1 <- ggplot(totalDamage, aes(x=Year, y=Damages, fill=Source)) +
    geom_bar(stat = "identity", color="black") +
    ylab("Health damages (trillion $)") + theme_classic() +
    scale_fill_manual(values=c("#E69F00", "#56B4E9")) + 
    theme(text = element_text(size=16, color="black"),
          legend.title=element_blank(),
          legend.position="bottom",
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) 
  if(save){
    setwd(paste(baseWD, "figures", "Total damages", sep="/"))
    ggsave(plotName, g1, width=8)
    
  }
  return(g1)
}
totalDamagePlottingFunction <- function(damages, plotName="", save=F, plotGDP=T, shareGDP=NULL){
  totalDamage <- ldply(damages, nationalDamagesSum)
  totalDamage$areaDamages <- totalDamage$damages - totalDamage$pointDamages
  totalDamage$year <- years; colnames(totalDamage) <- c("total", "point", "area", "year")
  damagesPlot <- melt(totalDamage[,-1], id.vars="year")
  colnames(damagesPlot) <- c("Year", "Source", "Damages")
  
  #damagesPlot$Source <- factor(damagesPlot$Source, levels(damagesPlot$Source)[c(2,1)])
  damagesPlot$Year <- factor(damagesPlot$Year)
  
  damagesPlot <- merge(damagesPlot, GDP, by="Year"); rm(GDP)
  
  # convert damages to trillion $
  damagesPlot$Damages <- damagesPlot$Damages / 1E12
  
  totalDamageSums <- ddply(damagesPlot, ~ Year, summarize, totalDamage = sum(Damages))
  damagesPlot <- merge(damagesPlot, totalDamageSums, by="Year")
  
  # calculate share of GDP (in percent)
  # use scale factor to adjust for second axis
  scale <- 20
  damagesPlot$Share <- damagesPlot$totalDamage / damagesPlot$GDP 
  damagesPlot$ShareYTrans <- damagesPlot$Share * scale
  damagesPlot$ShareVar <- "share of GDP"
  
  # damage plot
  g1 <- totalDamagePlot(damagesPlot, save, plotName)
  
  setwd(paste(baseWD, "figures", "Total damages", sep="/"))
  #pdf(plotName)
  #par(mar=c(6,4,3,2)+0.1)
  if(plotGDP){
    if(is.null(shareGDP)){
      
      # add GDP share
      damagePlotGDP <- g1 + geom_point(aes(x=as.numeric(Year), y=ShareYTrans, shape=ShareVar), 
                                       col="black", fill="black", size=2.5) + 
        scale_y_continuous(sec.axis = sec_axis(~./scale*100, name = "Share of GDP (%)")) +
        scale_shape_manual(name="", breaks=c("share of GDP"), labels=c("share of GDP"), values=23) +
        guides(fill=guide_legend(order=1), shape= guide_legend(order=2)) + 
        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10))) 
      
      
      # g2 <- ggplot(GDP, aes(x=Year, y=Damages, fill=Source)) +
      #   geom_bar(stat = "identity", color="black") + ylim(c(0,18)) +
      #   ylab("GDP (trillion $)") + theme_classic() +
      #   scale_fill_manual(values=c("#FF9999")) + 
      #   theme(text = element_text(size=16, color="black"),
      #         legend.title=element_blank(),
      #         legend.position="bottom")      
      
      # invertedDamages <- spread(damagesPlot, "Source", "Damages")
      # rownames(invertedDamages) <- invertedDamages$Year; invertedDamages$Year <- NULL
      # invertedDamages <- t(invertedDamages)
      # invertedDamages <- invertedDamages[c(2,1),]
      # 
      # yEdge <- sum(invertedDamages[,1]) * 1.1
      # mids <- barplot(invertedDamages, ylab="Health damages (trillion $)", col=c("#56B4E9","#E69F00"),
      #         axes=F, ylim=c(-0.05, yEdge), col.axis="gray40", cex.axis=1.25)
      # xEdge <- mids[length(mids)] + (mids[2] - mids[1])
      # axis(1, at=c(0, mids, xEdge), labels=FALSE, xpd=FALSE, lwd.ticks=0.5, cex.axis=1.25)
      # axis(2, at=seq(0, yEdge, 0.5), xpd=FALSE, lwd.ticks=0.5, las=1, cex.axis=1.25, col.axis="gray40")
      # axis(2, at=c(-0.05, 0), xpd=FALSE, lwd.ticks=0, labels=F)
      # axis(4, at)
      # 
      # par(xpd=T)
      # legendPos <- mids[2] - (mids[2] - mids[1]) / 1.5
      # legend(legendPos, -0.15,  rownames(invertedDamages), fill=c("#56B4E9","#E69F00"), bty="n", horiz = T, 
      #        cex=1.5, text.width=0.25, x.intersp=0.5)
      
    } else {
      g2 <- ggplot(shareGDP, aes(x=GDP, y=damage, shape=Year, color=Year)) +
        geom_point(size=5) + ylim(c(0,18)) + xlim(c(0, 18)) +
        ylab("Health damages (trillion $)") + theme_classic() +
        xlab("GDP (trillion $)") +
        theme(text = element_text(size=16, color="black"),
              legend.title=element_blank(),
              legend.position="bottom") 
    }
    
    # reduce space between labels and axis
    # reduce space between legend labels and colors, shift left
    # add second axis
    
    #dev.off()
    
    #damagePlotGDP <- arrangeGrob(g1 + theme(plot.title = element_text(face="bold")) + ggtitle("A"), 
    #                             g2 + ggtitle("B") + theme(plot.title = element_text(face="bold")), ncol=2, widths=c(1, 1))
    #damagePlotGDP <- grid.arrange(damagePlotGDP, nrow = 1, heights = c(1))
    ggsave(plotName, damagePlotGDP, width=6)
  }
  return(damagesPlot)
}

totalDamages <- totalDamagePlottingFunction(damages, plotName="Total damage barplot with GDP.pdf")

annualizedGrowth <- function(dfVector, interval=3){
  return(((dfVector[2:length(dfVector)] / dfVector[1:length(dfVector)-1])^(1/interval)-1)*100)
}

# GDP
annualizedGrowth(c(16331, 16475, 17521))
annualizedGrowth(totalDamages[totalDamages$Source == "point", "Damages"])
annualizedGrowth(totalDamages[totalDamages$Source == "area", "Damages"])
annualizedGrowth(totalDamages[totalDamages$Source == "area", "Damages"] + totalDamages[totalDamages$Source == "point", "Damages"])

shareGDP <- ddply(totalDamages, ~ Year, summarize, damage=sum(Damages))
shareGDP$GDP <- GDP$GDP
shareGDP$share <- shareGDP$damage / shareGDP$GDP

# alternative plot with scatterplot of GDP vs. damages
# totalDamagePlottingFunction(damages, plotName="Total damage barplot with GDP - alt GDP.pdf", shareGDP=shareGDP)


## Total damage + GDP plot 
# See version 2.0 (totalDamagePanelPlotV2) with results for range of CR / VSL below
# Note: load emissionsSummaryTotals from sector analysis section below
totalDamagePanelPlot <- function(damages, emissions, emissionsSummaryTotals, plotName=""){
  
  # Panel 1: total damages
  totalDamage <- ldply(damages, nationalDamagesSum)
  totalDamage$areaDamages <- totalDamage$damages - totalDamage$pointDamages
  totalDamage$year <- years; colnames(totalDamage) <- c("total", "point", "area", "year")
  damagesPlot <- melt(totalDamage[,-1], id.vars="year")
  colnames(damagesPlot) <- c("Year", "Source", "Damages")
  
  # convert damages to trillion $
  damagesPlot$Damages <- damagesPlot$Damages / 1E12
  
  g1 <- ggplot(damagesPlot, aes(x=factor(Year), y=Damages, fill=Source, label=signif(Damages, 2))) +
    geom_bar(stat = "identity", color="black") +
    ylab("Annual health damages\n(trillion $)") + xlab("") + theme_classic() +
    scale_fill_manual(values=c("#E69F00", "#56B4E9")) + 
    theme(legend.title=element_blank(),
          text = element_text(size=14),
          axis.text = element_text(size=12)) + 
    geom_text(size = 4, position = position_stack(vjust = 0.5), fontface="bold") + 
    geom_text(aes(x=1.75, y=1.55, label="Point sources"), colour="#E69F00", vjust="bottom", hjust="left") + 
    geom_text(aes(x=1.96875, y=1.4, label="Area sources"), colour="#56B4E9", vjust="bottom", hjust="left") + 
    geom_segment(aes(x=1.35, y=1.3, xend=1.7, yend=1.54), size=0.5) +
    geom_segment(aes(x=1.35, y=1, xend=1.91875, yend=1.39), size=0.5)
  
  legend1 <- g_legend(g1)
  
  # Panel 2: Emissions (load from sector section)
  emissionsSummaryTotals$year <- c(2008, 2011, 2014)
  emissionsSummaryTotals$PM10 <- NULL
  #emissionsSummaryTotals$VOCs <- emissionsSummaryTotals$VOC_A + emissionsSummaryTotals$VOC_B
  #emissionsSummaryTotals$VOC_A <- NULL;  
  emissionsSummaryTotals$VOC_B <- NULL
  emissionsPlot <- melt(emissionsSummaryTotals, id.vars = "year")
  
  emissionsPlot$variable <- mapvalues(emissionsPlot$variable, from="VOC_A", to="VOC")
  emissionsPlot$variable <- factor(emissionsPlot$variable, levels = rev(c("VOC", "PM25", "NH3", "NOX", "SO2")))
  
  plotExpressions <- c(expression("VOC"), expression("PM"[2.5]), expression("NH"[3]), expression("NO"[x]), expression("SO"[2])) 
  
  g2 <- ggplot(emissionsPlot, aes(x=factor(year), y=value/1E6, fill=variable, label=signif(value/1E6, 2))) + 
    geom_bar(stat='identity', color="black") + theme_classic() +
    ylab("Total annual emissions\n(million tons)") + xlab("") + 
    theme(legend.title=element_blank(),
          text = element_text(size=14),
          axis.text = element_text(size=12),
          legend.text.align = 0) +
    scale_fill_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'),
                      labels=rev(plotExpressions)) + 
    geom_text(size = 4, position = position_stack(vjust = 0.5), fontface="bold")
  
  legend2 <- g_legend(g2)
  
  # Panel 3: GDP
  # add GDP data (converted trillion $)
  GDP <- data.frame(Year = factor(years), GDP=c(16331, 16475, 17521)/1E3)
  
  # calculate damage shares (in percent)
  totalDamageSums <- ddply(damagesPlot, ~ Year, summarize, totalDamage = sum(Damages))
  GDP <- merge(GDP, totalDamageSums, by="Year")
  GDP$Share <- GDP$totalDamage / GDP$GDP * 100 
  shareChange <- signif(GDP$Share[3] - GDP$Share[1],2)
  shareChangeText <- paste0(shareChange, "//%")
  
  print(GDP)
  
  g3 <- ggplot(GDP, aes(x=Year, y=Share, group=1)) + geom_path() + geom_point(shape=23, size=4, fill="black") + theme_classic() + 
    ylim(c(5,10)) + xlab("") + ylab("Annual health damages\n(% of GDP)") + 
    geom_segment(aes(x=1.25, xend=2.75, y=9.5, yend=8.25), size = 0.5, 
                 arrow = arrow(length = unit(0.25, "cm"))) +
    geom_text(aes(x=2, y=9.5, label=sprintf("%0.1f%%", shareChange)), fontface="bold") + 
    theme(text = element_text(size=14),
          axis.text = element_text(size=12))
  
  # Panel 4: Average marginal damages (emissions-weighted using values for medium stacks)
  medAvgMD <- melt(loadAverageMD(emissionsWeighted=F), id.vars="Year") 
  
  # Index to 2008
  vals2008 <- medAvgMD[medAvgMD$Year == 2008,]; colnames(vals2008)[3] <- "value2008"
  medAvgMD <- merge(medAvgMD, vals2008[,c("variable", "value2008")], by="variable")
  medAvgMD$indexed <- medAvgMD$value / medAvgMD$value2008
  
  # standardize pollutant colors
  medAvgMD$variable <- factor(medAvgMD$variable, levels=rev(c("VOC_A", "PM25", "NH3", "NOx", "SO2")))
  medAvgMD$variable <- mapvalues(medAvgMD$variable, from=c("VOC_A", "NOx"), to=c("VOC", "NOx"))
  
  g4 <- ggplot(medAvgMD, aes(x=factor(Year), y=value/1E3, color=variable, group=variable, fill=variable)) + 
    geom_path() + geom_point(shape=23, size=4) + theme_classic() +
    scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) +
    scale_fill_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) + xlab("") +
    #ylab("Marginal damage\n(indexed to 2008)") + ylim(c(0.9, 1.3)) +
    ylab("Average marginal damage\n(thousand $ per ton)") + ylim(c(0, 78)) +
    theme(legend.title=element_blank(),
          text = element_text(size=14),
          axis.text = element_text(size=12),
          plot.margin = unit(c(5.5, 60.5, 5.5, 5.5), "points")) +
    annotate("text", x = 3.75, y = 0, label = "", parse = TRUE, hjust = 0) +
    annotate("text", x = 3.25, y = 33, label = "SO[2]", parse = TRUE, hjust = 0, color='#e41a1c', fontface="bold") +
    annotate("text", x = 3.25, y = 75, label = "PM[2.5]", parse = TRUE, hjust = 0, color='#984ea3', fontface="bold") +
    annotate("text", x = 3.25, y = 48, label = "NH[3]", parse = TRUE, hjust = 0, color='#4daf4a', fontface="bold") +
    annotate("text", x = 3.25, y = 15, label = "NO[x]", parse = TRUE, hjust = 0, color='#377eb8', fontface="bold") +
    annotate("text", x = 3.25, y = 3, label = "VOC", parse = TRUE, hjust = 0, color='#ff7f00', fontface="bold")
  
  #annotation_custom(grob=textGrob(label="SO"), xmin=4, xmax=4, ymin=35, ymax=35)
  
  #g4 <- ggplot_gtable(ggplot_build(g4))
  #g4$layout$clip[g4$layout$name == "panel"] <- "off"
  
  
  #geom_text(data=mdLabels, aes(x=4, y=Yend, label=label), fontface="bold", vjust="bottom", hjust="left") #+ 
  #geom_segment(aes(x=1.35, y=1.3, xend=1.7, yend=1.54), size=0.5) +
  
  #panelPlotTop <- arrangeGrob(g1 + theme(plot.title = element_text(face="bold")) + ggtitle("A"), 
  #                            g2 + theme(plot.title = element_text(face="bold")) + ggtitle("B"), ncol=2, widths=c(1, 1))
  
  #panelPlotBottom <- arrangeGrob(g3 + theme(plot.title = element_text(face="bold")) + ggtitle("C"),
  #                               g4 + theme(plot.title = element_text(face="bold")) + ggtitle("D"), ncol=2, widths=c(1, 1))
  
  g1 <- g1 + theme(plot.title = element_text(face="bold"), legend.position = "none") + ggtitle("A")
  g2 <- g2 + theme(plot.title = element_text(face="bold")) + ggtitle("B")
  g3 <- g3 + theme(plot.title = element_text(face="bold")) + ggtitle("C")
  g4 <- g4 + theme(plot.title = element_text(face="bold"),  legend.position = "none") + ggtitle("D")
  
  lay <- rbind(c(1,2),
               c(3,4))
  
  panelPlot <- grid.arrange(g1, g2, 
                            g3, g4, layout_matrix = lay, heights = c(0.6, 0.4), widths=c(0.45, 0.55))
  setwd(paste(baseWD, "figures", "Total damages", sep="/"))
  ggsave(plotName, panelPlot, width=8, height=6)
  
}

loadAverageMD <- function(emissionsWeighted, height="Average"){
  setwd(paste(mdWD, "Model results", sep="/"))
  
  avgMD <- list()
  for (year in years){
    if(emissionsWeighted){
      x <- read.table(paste0("AP2 MD weighted average ", year, ".txt"), sep=",")
    } else{
      x <- read.table(paste0("AP2 MD average ", year, ".txt"), sep=",")
    }
    colnames(x) <- c("NH3", "NOx", "PM10", "PM25", "SO2", "VOC_A", "VOC_B")
    rownames(x) <- c("Area", "Low", "Medium", "Tall", "New Tall", "Average")
    x <- x[,c("PM25","NH3", "SO2", "NOx", "VOC_A")]
    avgMD[[which(year == years)]] <- x
  }
  
  # conver to $2014
  avgMD[[1]] <- avgMD[[1]] * VSLs[3] / VSLs[1]
  avgMD[[2]] <- avgMD[[2]] * VSLs[3] / VSLs[2]
  
  # return medium stack averages
  medAvgs <- ldply(avgMD, function(x){return(x[height,])})
  medAvgs$Year <- years
  return(medAvgs)
}

totalDamagePanelPlot(damages, emissions, emissionsSummaryTotals, plotName="Damage and GDP panel.pdf")

## Constant MD ####

# calculate damages using 2008 marginal damages for all years
damagesConstantMD <- llply(emissions, calcIncurredDamages, marginal.damages=md[[1]])
damagesConstantMDPoint <- llply(emissions, calcIncurredDamages, marginal.damages=md[[1]], area=F)
  
damagesConstantMD <- mapply(mergeDamages, df1=damagesConstantMD, df2=damagesConstantMDPoint, 
                            MoreArgs = list(cNames =c("damages", "pointDamages")),  SIMPLIFY=F); rm(damagesConstantMDPoint)

# inflation adjustment (convert to $2014; all years now start in $2008)
damagesConstantMD <- mapply(inflateDamages, damagesConstantMD, var="damages", yearFrom=2008, yearTo=2014, SIMPLIFY=F)
damagesConstantMD <- mapply(inflateDamages, damagesConstantMD, var="pointDamages", yearFrom=2008, yearTo=2014, SIMPLIFY=F)

# calculate per capita values
damagesConstantMD <- mapply(mergeDamages, df1=damagesConstantMD, df2=pop, MoreArgs=list(cNames =c("damages", "pointDamages", "population")),  SIMPLIFY=F)
damagesConstantMD[[1]] <- adjustMissingFips(damagesConstantMD[[1]])
damagesConstantMD <- llply(damagesConstantMD, calcPerCapitaDamages, vars=c("damages"))

diffsConstantMDs <- list()
diffsConstantMDs[[1]] <- calcDifference(damagesConstantMD[[1]], damagesConstantMD[[2]], vars = c("damages", "damagesPerCap"))
diffsConstantMDs[[2]] <- calcDifference(damagesConstantMD[[1]], damagesConstantMD[[3]], vars = c("damages", "damagesPerCap"))
diffsConstantMDs[[3]] <- calcDifference(damagesConstantMD[[2]], damagesConstantMD[[3]], vars = c("damages", "damagesPerCap"))

perCapPlotTitles <- c("Per capita change in damages (2008 to 2011)",
                      "Per capita change in damages (2008 to 2014)",
                      "Per capita change in damages (2011 to 2014)")

ldply(diffsConstantMDs, function(x){quantile(x$damagesPerCap, probs=seq(0,1,0.2), na.rm=T)})

perCapPlotsConstantMDs <- list()


for(i in 1:length(years)){
  perCapPlotsConstantMDs[[i]] <- countyDamagesMap(diffsConstantMDs[[i]], targetVar="damagesPerCap", 
                                       legend.title = "Change in damages\n($ per person)", 
                                       title=paste(perCapPlotTitles[i], "- Constant MDs"),
                                       millions = FALSE, set.breaks = perCapScale,
                                       cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"), save=T)
}

g1 <- arrangeGrob(perCapPlotsConstantMDs[[1]] + ggtitle("a") + theme(plot.title = element_text(face="bold"), legend.position = "none"), 
                  perCapPlotsConstantMDs[[2]] + ggtitle("b") + theme(plot.title = element_text(face="bold")), ncol=2, widths=c(1, 1.4))
perCapPanel <- grid.arrange(g1, nrow = 1, heights=1)

setwd(paste(baseWD, "figures", "Maps", sep="/"))
ggsave("Per capita change in damages - panel - Constant MDs.pdf", perCapPanel, height=3.2, width=10)
rm(perCapPanel); rm(g1); rm(perCapPlotsConstantMDs)

# total damages using constant MDs
totalDamagesConstantMD <- totalDamagePlottingFunction(damagesConstantMD, plotName="Total damage barplot - constant MDs.pdf")


## Constant population ####
for(i in 1:length(years)){
  damages[[i]]$pop2008 <- damages[[1]]$population
}

# recalculate per capita values
damagesConstantPop <- llply(damages, calcPerCapitaDamages, vars=c("damages", "pointDamages"), popVar="pop2008")

diffsConstantPop <- list()
diffsConstantPop[[1]] <- calcDifference(damagesConstantPop[[1]], damagesConstantPop[[2]], vars = c("damagesPerCap", "pointDamagesPerCap"))
diffsConstantPop[[2]] <- calcDifference(damagesConstantPop[[1]], damagesConstantPop[[3]], vars = c("damagesPerCap", "pointDamagesPerCap"))
diffsConstantPop[[3]] <- calcDifference(damagesConstantPop[[2]], damagesConstantPop[[3]], vars = c("damagesPerCap", "pointDamagesPerCap"))


perCapPlotsConstantPop <- list()
for(i in 1:length(years)){
  perCapPlotsConstantPop[[i]] <- countyDamagesMap(diffsConstantPop[[i]], targetVar="damagesPerCap", 
                                       legend.title = "Change in damages\n($ per person)", 
                                       title=paste(perCapPlotTitles[i], "- Constant population"),
                                       millions = FALSE, set.breaks = perCapScale,
                                       cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"))
}

g1 <- arrangeGrob(perCapPlotsConstantPop[[1]] + ggtitle("a") + theme(plot.title = element_text(face="bold"), legend.position = "none"), 
                  perCapPlotsConstantPop[[2]] + ggtitle("b") + theme(plot.title = element_text(face="bold")), ncol=2, widths=c(1, 1.4))
perCapPanelConstantPop <- grid.arrange(g1, nrow = 1, heights=1)

setwd(paste(baseWD, "figures", "Maps", sep="/"))
ggsave("Per capita change in damages - panel - Constant pop.pdf", perCapPanelConstantPop, height=3.2, width=10)
rm(perCapPanelConstantPop); rm(g1); rm(perCapPlotsConstantPop)


## Transfer tables ####

# Specify mapping of EPA regions to states/fips
new.rows <- data.frame(state = c("FL", "VA"), state.num = c(12, 51), county.num = c(0,0), 
                       county.name = c("Miami-Dade County", "Alleghany County"), fips.class = c("H1", "H1"), fips = c(12025, 51560))

census.fips <- rbind(census.fips, new.rows)
state.fips.map <- merge(data.frame(fips=fips), census.fips[,c("fips", "state")], all.x=T, by = "fips", sort=FALSE)
identical(fips, state.fips.map$fips)

state.epa.map <- data.frame(state = unique(state.fips.map$state))
state.epa.map$region <- mapvalues(state.epa.map$state, 
                                  from = c("ME", "NH", "VT", "MA", "RI", "CT",
                                           "NY", "NJ",
                                           "PA", "WV", "VA", "DE", "MD", "DC",
                                           "NC", "SC", "GA", "FL", "AL", "MS", "TN", "KY",
                                           "TX", "LA", "AR", "OK", "NM",
                                           "OH", "IN", "IL", "WI", "MI", "MN",
                                           "KS", "MO", "IA", "NE",
                                           "CO", "UT", "WY", "MT", "ND", "SD",
                                           "CA", "NV", "AZ",
                                           "WA", "OR", "ID"),
                                  to = c(rep("NE", 6),
                                         rep("NY", 2),
                                         rep("MA", 6),
                                         rep("SE", 8),
                                         rep("S", 5),
                                         rep("MW", 6),
                                         rep("GP", 4),
                                         rep("M", 6),
                                         rep("SW", 3),
                                         rep("NW", 3)))

state.epa.map <- state.epa.map[order(state.epa.map$state),]

# function for consolidating by mapping region
geographic.breakdown <- function(damage.matrix, mapping, indx){
  rownames(damage.matrix) <- mapping[,indx]
  colnames(damage.matrix) <- mapping[,indx]
  # sum by sources
  reduced.matrix <- t(sapply(by(damage.matrix, rownames(damage.matrix), colSums),identity))
  # invert
  reduced.matrix <- t(reduced.matrix)
  # sum by receptors
  reduced.matrix <- t(sapply(by(reduced.matrix, rownames(reduced.matrix), colSums), identity))
  # invert
  reduced.matrix <- t(reduced.matrix)
  return(reduced.matrix)
}

calcRegionTransferTable <- function(md, emissions, totalMortality, pointOnly=F, region=T, state=F, absolute=F){
  # calc source-receptor damage matrix
  if (!pointOnly){
    damage.matrix <- calcDamageMatrix(md, emissions)
  }
  else{
    damage.matrix <- calcDamageMatrix(md, emissions, area=FALSE)
  }
  # translate damages into deaths
  death.matrix <- convertDamageToMort(damage.matrix, totalMortality)

  # summarize at state levels
  state.matrix <- geographic.breakdown(death.matrix, state.fips.map, indx=2)

  if (region){
    
    # summarize at region level
    region.matrix <- geographic.breakdown(state.matrix, state.epa.map, indx=2)
    
    region.order <- c("NE", "NY", "MA", "MW", "SE", "S", "GP", "M", "SW", "NW")
    region.matrix <- region.matrix[region.order, region.order]    
    
    if(absolute){
      return(region.matrix)
    } else { 
      return(calcPercent(region.matrix))
    }
  } else if (state){
    if(absolute){
      return(state.matrix)
    } else { 
      return(calcPercent(state.matrix))
    }  } else {
    return(death.matrix)
  }
}
# calculate percent of total damage for S-R and multiplies by total deaths
convertDamageToMort <- function(damage.matrix, totalMorts){
  death.matrix <- damage.matrix / sum(damage.matrix) * totalMorts
  return(death.matrix)
}
calcPercent <- function(death.matrix){
  deaths.caused <- rowSums(death.matrix)
  deaths.inccurred <- c(colSums(death.matrix), sum(deaths.caused))
  
  death.matrix <- cbind(death.matrix, deaths.caused)
  death.matrix <- rbind(death.matrix, deaths.inccurred)
  
  deaths <- round(sweep(death.matrix, 2, deaths.inccurred, "/"), 3) * 100
  # overwrite percentages in last row/col
  deaths[,ncol(deaths)] <- c(deaths.caused, 0)
  deaths[nrow(deaths),] <- deaths.inccurred
  
  return(deaths)
}

# read total mortality
mortalities <- loadMortality("Baseline deaths.csv")
transferTables <- mapply(calcRegionTransferTable, md, emissions, mortalities, SIMPLIFY=F)
transferTablesAbs <- mapply(calcRegionTransferTable, md, emissions, mortalities, 
                                              MoreArgs = list(absolute=T), SIMPLIFY=F)

# table plotting functions
getGridVals <- function(matrix){
  return(matrix[-11,-11])
}
getColorScale <- function(matrix, valRange){
  plotVals <- getGridVals(matrix)
  diag(plotVals) <- NA
  # account for rounding errors in R
  plotVals[plotVals < 0] <- 0 
  colors <- color.scale(plotVals, cs1=c(1,1,1), cs2=c(1,1,0), cs3=c(1,0,0), na.color="#000000", xrange=valRange)
  return(colors)
  
}
valFont <- function(n){
  textColors <- matrix("black", ncol=n, nrow=n)
  diag(textColors) <- "white"
  return(textColors)  
}
tablePlotCore <- function(matrix, tableName, valRange, labels, textSize=0.85, absolute=FALSE, cex.axis=1){
  par(mar = c(5.25, 7, 7, 5))
  plotVals <- getGridVals(matrix)
  valFonts <- valFont(dim(plotVals)[1])
  colors <- getColorScale(matrix, valRange)
  
  color2D.matplot(plotVals, cellcolors=colors, 
                  show.values=1, 
                  axes = FALSE,
                  xlab = "",
                  ylab = "",
                  vcex = textSize,         # size of text in cells
                  vcol = valFonts)    # font of text
  # deaths
  if(absolute){
    deathsCaused <- rowSums(matrix) 
    deathsOccurred <- colSums(matrix)
  } else {
    deathsCaused <- matrix[-11, 11]
    deathsOccurred <- matrix[11,-11]
  }
  axis(1, at = seq_len(ncol(plotVals)) - 0.5, line=-0.5,
       labels = format(signif(deathsOccurred, 2), big.mark = ","), tick = FALSE, las=2, cex.axis=cex.axis)
  axis(4, at = seq_len(nrow(plotVals)) -0.5, line=-0.5,
       labels = format(signif(rev(deathsCaused), 2), big.mark = ","), tick = FALSE, las = 1, cex.axis=cex.axis)
  
  # region labels
  axis(3, at = seq_len(ncol(plotVals)) - 0.5, line=-0.5,
       labels = labels, tick = FALSE, las=2, cex.axis=cex.axis)
  
  axis(2, at = seq_len(nrow(plotVals)) -0.5, line=-0.5,
       labels = rev(labels), tick = FALSE, las = 1, cex.axis=cex.axis)
  
  # axis titles
  axis(1, at = 5, line=2.5,
       labels = "Annual deaths occuring", tick = FALSE, las=0, cex.axis=cex.axis)
  axis(2, at = 5, line=5,
       labels = "Region where pollution is emitted", tick = FALSE, las=0, cex.axis=cex.axis)
  axis(3, at = 5, line=5,
       labels = "Region where deaths occur", tick = FALSE, las=1, cex.axis=cex.axis)
  axis(4, at = 5, line=2.5,
       labels = "Annual deaths caused", tick = FALSE, las=0, cex.axis=1, srt=-90)
}
transferTablePlot <- function(matrix, tableName, valRange, labels, absolute=FALSE){
  setwd(paste(baseWD, "figures", "Transfer tables", sep="/"))
  pdf(paste(tableName, ".pdf", sep=""), width=6, height=6)
  tablePlotCore(matrix, tableName, valRange, labels, absolute=FALSE)
  dev.off()
}
multiTransferTable <- function(tableList, tableName, valRange, labels, absolute=FALSE, grid, plotDim, textSize=0.85){
  setwd(paste(baseWD, "figures", "Transfer tables", sep="/"))
  pdf(paste(tableName, ".pdf", sep=""), height=plotDim[1], width=plotDim[2])
  par(mfrow=grid) 
  panelLabels <- c("a", "b", "c")
  for (i in 1:length(tableList)){
    tablePlotCore(tableList[[i]], tableName, valRange, labels, absolute=absolute, cex.axis=1, textSize)
    fig_label(panelLabels[i], cex=1.25, font=2)
  }
  dev.off()
}

# table plots
labels <- c("New England", "NY/NJ", "Mid-Atlantic", "Midwest", "Southeast", "South", "Great Plains", "Mountain", "Southwest", "Northwest")

# value range chosen based on max values across plots
maxVal <- 0
for (t in 1:3){
  for (i in 1:10){
    for (j in 1:10){
      if(i != j){
        if(transferTables[[t]][i,j] > maxVal){
          maxVal <- transferTables[[t]][i,j]
        }
      }
    }
  }
}
maxVal <- ceiling(maxVal)

transferTablePlot(transferTables[[1]], "Region matrix 2008", valRange=c(0,maxVal), labels)
transferTablePlot(transferTables[[2]], "Region matrix 2011", valRange=c(0,maxVal), labels)
transferTablePlot(transferTables[[3]], "Region matrix 2014", valRange=c(0,maxVal), labels)

multiTransferTable(list(transferTables[[1]], transferTables[[3]]), "Region matrix All years", 
                   valRange=c(0,maxVal), labels, grid=c(1,2), plotDim=c(6, 12))

multiTransferTable(list(transferTablesAbs[[1]]/100, transferTablesAbs[[2]]/100, 
                        transferTablesAbs[[3]]/100), "Region matrix All years Absolute", 
                   valRange=c(0,42), labels, grid=c(2,2), plotDim=c(12, 12), absolute=TRUE, textSize=1)

## State based transfer analysis ####
stateTransfer <- mapply(calcRegionTransferTable, md, emissions, mortalities, SIMPLIFY=F, state=T, region=F)

# percent of deaths out of state
sum((100 - diag(stateTransfer[[1]][1:49,1:49]))/100 * stateTransfer[[1]][50, -50]) / stateTransfer[[1]][50,50]
sum((100 - diag(stateTransfer[[2]][1:49,1:49]))/100 * stateTransfer[[2]][50, -50]) / stateTransfer[[2]][50,50]
sum((100 - diag(stateTransfer[[3]][1:49,1:49]))/100 * stateTransfer[[3]][50, -50]) / stateTransfer[[3]][50,50]

## Alternative VSL results ####
convertVSL <- function(df, oldVSL, newVSL, vars){
  for (var in vars){
    df[,var] <- df[,var] / oldVSL * newVSL
  }
  return(df)
}

# alternative VSL based on $2.8 CV value from Kochi study, inflated from 2006 to 2014
damagesAltVSL <- llply(damages, convertVSL, oldVSL=VSLs[3], newVSL=3.3E6, vars=c("damages", "damagesPerCap", "pointDamages"))
diffsAltVSL <- llply(diffs, convertVSL, oldVSL=VSLs[3], newVSL=3.3E6, vars=c("damages", "damagesPerCap", "pointDamages"))

perCapPlots <- list()

for(i in 1:length(years)){
  perCapPlots[[i]] <- countyDamagesMap(diffsAltVSL[[i]], targetVar="damagesPerCap", 
                                       legend.title = "Change in damages\n($ per person)", 
                                       title=paste(perCapPlotTitles[i], "- Alternative VSL"),
                                       millions = FALSE, set.breaks = perCapScale,
                                       cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"), save=T)
}

# per capita panel plot
g1 <- arrangeGrob(perCapPlots[[1]] + ggtitle("A") + theme(plot.title = element_text(face="bold"), legend.position = "none"), 
                  perCapPlots[[2]] + ggtitle("B") + theme(plot.title = element_text(face="bold")), ncol=2, widths=c(1, 1.4))
perCapPanel <- grid.arrange(g1, nrow = 1, heights=1)

setwd(paste(baseWD, "figures", "Maps", sep="/"))
ggsave("Per capita change in damages - panel - Alternative VSL.pdf", perCapPanel, height=3.2, width=10)

# total damages using alternative VSL
totalDamagesAltVSL <- totalDamagePlottingFunction(damagesAltVSL, plotGDP=F)

## VSLY results ####
mdVSLY <- llply(years, loadMD, run="VSLY")

# calculate damages by county
damagesVSLY <- mapply(calcIncurredDamages, mdVSLY, emissions, SIMPLIFY=F)
damagesPointVSLY <- mapply(calcIncurredDamages, mdVSLY, emissions, area=F, SIMPLIFY=F)
rm(mdVSLY)

# merge point and area sources
damagesVSLY <- mapply(mergeDamages, df1=damagesVSLY, df2=damagesPointVSLY, 
                           MoreArgs = list(cNames =c("damages", "pointDamages")),  SIMPLIFY=F); rm(damagesPointVSLY)

# inflation adjustment (convert to $2014)
damagesVSLY <- mapply(inflateDamages, damagesVSLY, var="damages", yearFrom=years, yearTo=2014, SIMPLIFY=F)
damagesVSLY <- mapply(inflateDamages, damagesVSLY, var="pointDamages", yearFrom=years, yearTo=2014, SIMPLIFY=F)

# calculate per capita values
damagesVSLY <- mapply(mergeDamages, df1=damagesVSLY, df2=pop, 
                                      MoreArgs=list(cNames =c("damages", "pointDamages", "population")),  SIMPLIFY=F)
damagesVSLY[[1]] <- adjustMissingFips(damagesVSLY[[1]])

# total damages using VSLY
totalDamagesVSLY <- totalDamagePlottingFunction(damagesVSLY, plotGDP=F)

# per capita VSLY maps
damagesVSLY <- llply(damagesVSLY, calcPerCapitaDamages, vars=c("damages", "pointDamages"))

diffsVSLY <- list()
diffsVSLY[[1]] <- calcDifference(damagesVSLY[[1]], damagesVSLY[[2]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2008 to 2011
diffsVSLY[[2]] <- calcDifference(damagesVSLY[[1]], damagesVSLY[[3]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2008 to 2014
diffsVSLY[[3]] <- calcDifference(damagesVSLY[[2]], damagesVSLY[[3]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2011 to 2014

perCapPlots <- list()
for(i in 1:length(years)){
  perCapPlots[[i]] <- countyDamagesMap(diffsVSLY[[i]], targetVar="damagesPerCap", 
                                       legend.title = "Change in damages\n($ per person)", 
                                       title=paste(perCapPlotTitles[i], "- VSLY"),
                                       millions = FALSE, set.breaks = perCapScale,
                                       cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"), save=T)
}


## Alternative dose response function ####
mdAltDR <- llply(years, loadMD, run="Alternative dose response")

# calculate damages by county
damagesAltDR <- mapply(calcIncurredDamages, mdAltDR, emissions, SIMPLIFY=F)
damagesPointAltDR <- mapply(calcIncurredDamages, mdAltDR, emissions, area=F, SIMPLIFY=F)

# merge point and area sources
damagesAltDR <- mapply(mergeDamages, df1=damagesAltDR, df2=damagesPointAltDR, 
                      MoreArgs = list(cNames =c("damages", "pointDamages")),  SIMPLIFY=F); rm(damagesPointAltDR)

# inflation adjustment (convert to $2014)
damagesAltDR <- mapply(inflateDamages, damagesAltDR, var="damages", yearFrom=years, yearTo=2014, SIMPLIFY=F)
damagesAltDR <- mapply(inflateDamages, damagesAltDR, var="pointDamages", yearFrom=years, yearTo=2014, SIMPLIFY=F)

# calculate per capita values
damagesAltDR <- mapply(mergeDamages, df1=damagesAltDR, df2=pop, 
                      MoreArgs=list(cNames =c("damages", "pointDamages", "population")),  SIMPLIFY=F)
damagesAltDR[[1]] <- adjustMissingFips(damagesAltDR[[1]])


# total damages using alternative DR
totalDamagesAltDR <- totalDamagePlottingFunction(damagesAltDR, plotGDP=F)

# per capita VSLY maps
damagesAltDR <- llply(damagesAltDR, calcPerCapitaDamages, vars=c("damages", "pointDamages"))

diffsAltDR <- list()
diffsAltDR[[1]] <- calcDifference(damagesAltDR[[1]], damagesAltDR[[2]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2008 to 2011
diffsAltDR[[2]] <- calcDifference(damagesAltDR[[1]], damagesAltDR[[3]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2008 to 2014
diffsAltDR[[3]] <- calcDifference(damagesAltDR[[2]], damagesAltDR[[3]], vars = c("damages", "damagesPerCap", "pointDamages", "pointDamagesPerCap")) # 2011 to 2014

ldply(diffsAltDR, function(x){quantile(x$damagesPerCap, probs=seq(0,1,0.2), na.rm=T)})

perCapScaleMod <- perCapScale

perCapScaleMod[1] <- signif(min(ldply(diffsAltDR, function(x){quantile(x$damagesPerCap, probs=seq(0,1,0.2), na.rm=T)})), 2)
perCapScaleMod[length(perCapScaleMod)] <- signif(max(ldply(diffsAltDR, function(x){quantile(x$damagesPerCap, probs=seq(0,1,0.2), na.rm=T)})), 2)

perCapPlots <- list()
for(i in 1:length(years)){
  perCapPlots[[i]] <- countyDamagesMap(diffsAltDR[[i]], targetVar="damagesPerCap", 
                                       legend.title = "Change in damages\n($ per person)", 
                                       title=paste(perCapPlotTitles[i], "- alternate DR"),
                                       millions = FALSE, set.breaks = perCapScaleMod,
                                       cols = c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26"), save=T)
}

# alternate dose response transfer table
mortalitiesAltDr <- loadMortality("Baseline deaths - alt dr.csv")

transferTablesAltDR <- mapply(calcRegionTransferTable, mdAltDR, emissions, mortalitiesAltDr, SIMPLIFY=F)

multiTransferTable(list(transferTablesAltDR[[1]], transferTablesAltDR[[2]], 
                        transferTablesAltDR[[3]]), "Region matrix All years - Alt DR", 
                   valRange=c(0,maxVal), labels, grid=c(2,2), plotDim=c(12, 12), textSize = 1)
rm(mdAltDR)


## Total damage VSL comparison ####
totalDamages$VSL <- "EPA VSL ($8.7 million)"
totalDamagesAltVSL$VSL <- "Alternative VSL ($3.3 million)"
totalDamagesVSLY$VSL <- "VSLY approach ($370,000 per year)"
VSLcomp <- rbind(totalDamages, totalDamagesAltVSL, totalDamagesVSLY)

VSLcomp$VSL <- factor(VSLcomp$VSL, levels=c("EPA VSL ($8.7 million)", "Alternative VSL ($3.3 million)", "VSLY approach ($370,000 per year)"))

scale <- 20
VSLcomp$ShareYTrans <- VSLcomp$Share * scale

g1 <- ggplot(VSLcomp, aes(x=Year, y=Damages, fill=Source, label=signif(Damages, 2))) +
  facet_wrap(~ VSL) +
  geom_bar(stat = "identity", color="black") +
  ylab("Health damages (trillion $)") + theme_classic() +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(text = element_text(size=16, color="black"),
        legend.title=element_blank(),
        legend.position="bottom") +
  geom_text(size = 4, position = position_stack(vjust = 0.5), fontface="bold") 

g1 <- g1 + geom_point(aes(x=as.numeric(Year), y=ShareYTrans, shape=ShareVar), 
                         col="black", fill="black", size=2.5) + 
  geom_text(aes(x=as.numeric(Year)+0.1, y=ShareYTrans+0.125, label=signif(Share*100, 2))) +
  geom_path(aes(group=VSL, x=as.numeric(Year), y=ShareYTrans)) +
  scale_y_continuous(sec.axis = sec_axis(~./scale*100, name = "Share of GDP (%)")) +
  scale_shape_manual(name="", breaks=c("share of GDP"), labels=c("share of GDP"), values=23) +
  guides(fill=guide_legend(order=1), shape= guide_legend(order=2)) + 
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10))) 

setwd(paste(baseWD, "figures", "Total damages", sep="/"))
ggsave("Total damage barplot - VSL comparison.pdf", width=10.5); rm(g1)

# dose response
totalDamages$DR <- "Baseline concentration-response"
totalDamagesAltDR$DR <- "Alternative concentration-response"
totalDamagesAltDR$VSL <- ""
DRcomp <- rbind(totalDamages, totalDamagesAltDR)

DRcomp$DR <- factor(DRcomp$DR, levels=c("Baseline concentration-response", "Alternative concentration-response"))

scale <- 20
DRcomp$ShareYTrans <- DRcomp$Share * scale
DRcomp$ShareVar <- "share of GDP"

g1 <- ggplot(DRcomp, aes(x=Year, y=Damages, fill=Source, label=signif(Damages,2))) +
  facet_wrap(~ DR) +
  geom_bar(stat = "identity", color="black") +
  ylab("Health damages (billon $)") + theme_classic() +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(text = element_text(size=16, color="black"),
        legend.title=element_blank(),
        legend.position="bottom") +
  geom_text(size = 4, position = position_stack(vjust = 0.5), fontface="bold") 


g1 <- g1 + geom_point(aes(x=as.numeric(Year), y=ShareYTrans, shape=ShareVar), 
                      col="black", fill="black", size=2.5) + 
  geom_text(aes(x=as.numeric(Year)+0.1, y=ShareYTrans+0.2, label=signif(Share*100, 2))) +
  geom_path(aes(group=VSL, x=as.numeric(Year), y=ShareYTrans)) +
  scale_y_continuous(sec.axis = sec_axis(~./scale*100, name = "Share of GDP (%)")) +
  scale_shape_manual(name="", breaks=c("share of GDP"), labels=c("share of GDP"), values=23) +
  guides(fill=guide_legend(order=1), shape= guide_legend(order=2)) + 
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10))) 

setwd(paste(baseWD, "figures", "Total damages", sep="/"))
ggsave("Total damage barplot - DR comparison.pdf", width=12, height=6); rm(g1)


# constant MD
totalDamages$MD <- "MD by year of emissions (baseline)"
totalDamagesConstantMD$MD <- "Constant MD using 2008 values"
totalDamagesConstantMD$VSL <- ""; totalDamagesConstantMD$DR <- ""
MDcomp <- rbind(totalDamages, totalDamagesConstantMD)

MDcomp$MD <- factor(MDcomp$MD, levels=c("MD by year of emissions (baseline)", "Constant MD using 2008 values"))

scale <- 20
MDcomp$ShareYTrans <- MDcomp$Share * scale
MDcomp$ShareVar <- "share of GDP"

g1 <- ggplot(MDcomp, aes(x=Year, y=Damages, fill=Source, label=signif(Damages,2))) +
  facet_wrap(~ MD) +
  geom_bar(stat = "identity", color="black") +
  ylab("Health damages (billon $)") + theme_classic() +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(text = element_text(size=16, color="black"),
        legend.title=element_blank(),
        legend.position="bottom") +
  geom_text(size = 4, position = position_stack(vjust = 0.5), fontface="bold") 

g1 <- g1 + geom_point(aes(x=as.numeric(Year), y=ShareYTrans, shape=ShareVar), 
                      col="black", fill="black", size=2.5) + 
  geom_text(aes(x=as.numeric(Year)+0.1, y=ShareYTrans+0.2, label=signif(Share*100, 2))) +
  geom_path(aes(group=VSL, x=as.numeric(Year), y=ShareYTrans)) +
  scale_y_continuous(sec.axis = sec_axis(~./scale*100, name = "Share of GDP (%)")) +
  scale_shape_manual(name="", breaks=c("share of GDP"), labels=c("share of GDP"), values=23) +
  guides(fill=guide_legend(order=1), shape= guide_legend(order=2)) + 
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10))) 

setwd(paste(baseWD, "figures", "Total damages", sep="/"))
ggsave("Total damage barplot - MD comparison.pdf", width=10); rm(g1)

## Sensitivity plots ####

setwd(sensitivityWD)
sens <- list()
sensPoint <- list()
for (i in 1:length(years)){
  sens[[i]] <- read.csv(paste("Sensitivity", years[i], "- all.csv"), header=FALSE)
  sensPoint[[i]] <- read.csv(paste("Sensitivity", years[i], "- point.csv"), header=FALSE)
}

# convert to $2014 ($ year based on emissions)
sens[[1]] <- sens[[1]] * (VSLs[3] / VSLs[1])
sens[[2]] <- sens[[2]] * (VSLs[3] / VSLs[2])

sensPoint[[1]] <- sensPoint[[1]] * (VSLs[3] / VSLs[1])
sensPoint[[2]] <- sensPoint[[2]] * (VSLs[3] / VSLs[2])

formatSensData <- function(df){
  rownames(df) <- years
  colnames(df) <- years
  # convert to billion dollars
  df <- df/1E9 
  # population variable
  df$Pop <- rownames(df)
  # reshape to long
  df <- melt(df, id = "Pop")
  names(df) <- c("Population", "Mortality", "Damage")
  return(df)
}

sens.df <- ldply(sens, formatSensData)
sensPoint.df <- ldply(sensPoint, formatSensData)

# emissions year variable
sens.df$Emissions <- paste(rep(years, each=9), "Emissions")
sensPoint.df$Emissions <- paste(rep(years, each=9), "Emissions")

setwd(paste(baseWD, "figures", "Sensitivity", sep="/")) 

ggplot(sens.df, aes(x = Population, y = Damage, fill = Mortality))+
  geom_bar(stat = "identity", position = "dodge") + facet_wrap( ~ Emissions, ncol=3 ) +
  scale_fill_manual(values=c("#599ad3", "#f9a65a", "#9e66ab"),
                    name = "Mortality\nrates year") + xlab("Population data year") +
  ylab("Total damages\n(billion $2014)") + theme_bw()

ggsave("Sensitivity analysis - All damages.pdf", width=10, height=6, device = "pdf")

ggplot(sensPoint.df, aes(x = Population, y = Damage, fill = Mortality))+
  geom_bar(stat = "identity", position = "dodge") + facet_wrap( ~ Emissions, ncol=3 ) +
  scale_fill_manual(values=c("#599ad3", "#f9a65a", "#9e66ab"),
                    name = "Mortality\nrates year") + xlab("Population data year") +
  ylab("Total damages\n(billion $2014)") + theme_bw()

ggsave("Sensitivity analysis - Point sources.pdf", width=10, height=6, device = "pdf")


## Sector per capita maps ####
## Approach 1: MD


calcSectorDamages <- function(md, sectorEmissions, yearNom){
  damageSector <- calcIncurredDamages(md, sectorEmissions)
  damageSector <- merge(damageSector, census.fips[,c("fips", "state", "county.name")], by = "fips", all.x=TRUE)
  
  # convert to $2014 dollars
  damageSector$damages <- convertToNominal(damageSector$damages, nominal.year=yearNom, real.year=2014)
  return(damageSector)
}
calcPerCapitaDiff <- function(damage2008, damage2014, pop){
  
  pop2008 <- merge(damage2008, pop[[1]], by="fips", all=T)
  pop2014 <- merge(damage2014, pop[[3]], by="fips", all=T)
  
  # adjustment for Bedford city (fips code merged over course of study)
  pop2008[pop2008$fips == 51019, "damages"] <- pop2008[pop2008$fips == 51019, "damages"] + pop2008[pop2008$fips == 51515, "damages"]
  pop2008[pop2008$fips == 51019, "population"] <- pop2008[pop2008$fips == 51019, "population"] + pop2008[pop2008$fips == 51515, "population"]
  
  pop2008$damages <- pop2008$damages / pop2008$population
  pop2014$damages <- pop2014$damages / pop2014$population
  
  damagesDiff <- merge(pop2014, pop2008, by=c("fips", "state", "county.name"))
  
  names(damagesDiff) <- c("fips", "state", "county.name", "2014", "population.2014", "2008", "population.2008")
  damagesDiff$damages <- damagesDiff[,"2014"] - damagesDiff[,"2008"]
  
  # drop missing values
  damagesDiff <- damagesDiff[!is.na(damagesDiff$damages),]
  
  return(damagesDiff)
  
}
sectorPerCapData <- function(sector, md, pop, mdYears=c(2008, 2014)){
  sectorEmissions2008 <- readSectorEmissions(sector, 2008)
  sectorEmissions2014 <- readSectorEmissions(sector, 2014)
  
  damage2008 <- calcSectorDamages(md=md[[1]], sectorEmissions=sectorEmissions2008, yearNom=mdYears[1])
  damage2014 <- calcSectorDamages(md=md[[3]], sectorEmissions=sectorEmissions2014, yearNom=mdYears[2])
  return(calcPerCapitaDiff(damage2008, damage2014, pop))
}
sectorPerCapMap <- function(sectorName, sectorPerCapDamages, breaks){
  setwd(paste(baseWD, "figures", "Maps", sep="/"))
  return(countyDamagesMap(sectorPerCapDamages, targetVar = "damages", legend.title = "Change in damages\n($ per person)", 
                            title = paste("Sector per capita change -", sectorName, "(2008 to 2014)"),
                            millions = FALSE, save=T, set.breaks=breaks,
                            cols=c("#2b8cbe", "#a6bddb", "#ece7f2", "#fee0d2", "#fc9272", "#de2d26")))
}

sectorPlots <- list()
sectorDamages <- list()
sectorNames <- c("Electricity generation", "Transportation - Heavy duty vehicles", "Transportation - Light duty vehicles", 
                 "Industrial processes - Oil & gas production", "Industrial boilers")
sectorScales <- list(c(-650, -100, -50, 0, 50, 100, 370),
                     c(-1100, -100, -25, 0, 25, 50, 150),
                     c(-500, -50, -25, 0, 25, 50, 175),
                     c(-250, -100, -25, 0, 25, 100, 720),
                     c(-330, -50, -5, 0, 5, 50, 610))
# loop through sectors
# change to linear scale / add binary option
for (sector in sectorNames){
  print(paste("Plotting", sector))
  sectorDamages[[sector]] <- sectorPerCapData(sector, md, pop)
  sectorPlots[[sector]] <- sectorPerCapMap(sector, sectorDamages[[sector]], breaks=sectorScales[[which(sector == sectorNames)]])
}
 
# sector panel plot


#newTheme <- theme(legend.position = "none", plot.title = element_text(face="bold"))
newTheme <- theme(plot.title = element_text(face="bold"))
#newScale <- scale_fill_gradient2(low="#2166ac", high="#b2182b", limits=c(-1050, 500), midpoint=0)
#newScale <- scale_fill_gradient(low="#2166ac", high="#b2182b", limits=c(-1050, 500))

mylegend <- g_legend(sectorPlots[[1]])

g1 <- arrangeGrob(sectorPlots[[1]] + ggtitle("a") + newTheme, 
                  sectorPlots[[2]] + ggtitle("b") + newTheme, ncol=2)
g2 <- arrangeGrob(sectorPlots[[3]] + ggtitle("c") + newTheme, 
                  sectorPlots[[4]] + ggtitle("d") + newTheme, ncol=2)
g3 <- arrangeGrob(sectorPlots[[5]] + ggtitle("e") + newTheme, ncol=2)

rm(mylegend); rm(newTheme); #rm(newScale)
sectorPanel <- grid.arrange(g1, g2, g3, nrow = 3, heights = c(0.333, 0.333, 0.334))
rm(g1); rm(g2); rm(g3)

setwd(paste(baseWD, "figures", "Maps", sep="/"))
ggsave("Sector panel.pdf", sectorPanel, height = 10, width = 12)







# Note: sector approach 2 not providing consistent results with marginal damages
# Now reworked to use total deaths
## Sector Approach 2: Model ####
sectors <- c("Electricity generation", "Transportation - Heavy duty vehicles", "Transportation - Light duty vehicles",
             "Industrial processes - Oil & gas production", "Industrial boilers", "Biogenics")

sectorDeaths <- list()
setwd("/Users/Cartographer/Documents/Carnegie Mellon/Box Sync/Research/AP2/APEEP_Web_2008/PM/Other outputs")
sectorDeaths[[1]] <- read.csv("Sector deaths.csv", header=F)
setwd("/Volumes/Seagate Backup Plus Drive/AP2/APEEP_Web_2011/PM/Other outputs")
sectorDeaths[[2]] <- read.csv("Sector deaths.csv", header=F)
setwd("/Volumes/Seagate Backup Plus Drive/AP2/APEEP_Web_2014/PM/Other outputs")
sectorDeaths[[3]] <- read.csv("Sector deaths.csv", header=F)

formatsectorDeaths <- function(sectorDeaths, fipsList, sectors, popYear){

  sectorDeaths <- cbind(sectorDeaths, fipsList)
  colnames(sectorDeaths) <- c("Baseline", sectors, "fips")
  
  # transform into long
  # sectorLong <- melt(sectorDeaths, id.vars="fips", measure.vars = c("Baseline", sectors))

  # sum by fips code
  # sectorSums <- ddply(sectorLong, ~ fips + variable, summarize, damage = sum(value))

  # per capita valus
  # sectorSums <- merge(sectorSums, popYear[,c("fips", "population")], by="fips")
  # sectorSums$damages <- sectorSums$damage / sectorSums$population

  # transform into wide
  # sectorSums <- dcast(sectorSums, fips ~ variable, value.var="damages")
  return(sectorDeaths)
}

sectorDeaths[[1]] <- formatsectorDeaths(sectorDeaths[[1]], fips, sectors, pop[[1]])
sectorDeaths[[2]] <- formatsectorDeaths(sectorDeaths[[2]], fips, "Biogenics", pop[[2]])
sectorDeaths[[3]] <- formatsectorDeaths(sectorDeaths[[3]], fips, sectors, pop[[3]])

llply(sectorDeaths, colSums)

# sectorDiffs <- sectorDeaths[[2]]
# sectorDiffs[,c("Baseline", sectors)] <- sectorDiffs[,c("Baseline", sectors)] - sectorDeaths[[1]][,c("Baseline", sectors)]
# 
# plotRanges <- list(c(-1100, -100, -25, 0, 25, 50, 150),
#                    c(-1100, -100, -25, 0, 25, 50, 150),
#                    c(-500, -50, -25, 0, 25, 50, 175),
#                    c(-250, -100, -25, 0, 25, 100, 720),
#                    c(-330, -50, -5, 0, 5, 50, 610))
# 
# names(plotRanges) <- sectors
# 
# for (sector in sectors){
#   sectorDiffs$damages <- sectorDiffs[,sector]
#   sectorPerCapMap(paste(sector, "AP2"), sectorDiffs, unname(unlist(plotRanges[sector])))
# }

## Total damages panel plot V2 ####

totalDamageRange <- VSLcomp
#drop VSLY
totalDamageRange <- droplevels(totalDamageRange[totalDamageRange$VSL != "VSLY approach ($370,000 per year)",])
altDR <- totalDamagesAltDR
altDR$VSL <- "High estimate"
altDR$DR <- NULL

totalDamageRange <- rbind(totalDamageRange, altDR); rm(altDR)

totalDamageRange$VSL <- mapvalues(totalDamageRange$VSL, from=c("EPA VSL ($8.7 million)", "Alternative VSL ($3.3 million)"),
                                                        to=c("Baseline", "Low estimate"))

scale <- 21
totalDamageRange$ShareYTrans <- totalDamageRange$Share * scale

totalDamageRange$label <- signif(totalDamageRange$Damages, 2)
totalDamageRange$label <- ifelse(totalDamageRange$label < 0.1, round(totalDamageRange$label,2), totalDamageRange$label)

totalDamagePanelPlotV2 <- function(damagesRange, emissions, emissionsSummaryTotals, plotName=""){
  
  # Panel 1: total damages with sensitivity (top row)
  textData <- data.frame(x=c(3.25, 3.25), y=c(2.8, 2.1), label=c("Point sources", "Area sources"), 
                         Source=c("point", "area"), VSL="Low estimate")
  textData2 <- data.frame(x=1.3, y=2.12, label=c("% of GDP (right axis)"), 
                         VSL="Baseline", Source="point")
  arrowData <- data.frame(x=1.3, xend=2, y=1.95, yend=1.95, VSL="Baseline")
  
  g1 <- ggplot(damagesRange, aes(x=factor(Year), y=Damages, fill=Source, label=label, 2)) +
    facet_wrap(~ VSL) +
    geom_bar(stat = "identity", color="black") +
    ylab("Annual health damages\n(trillion $)") + xlab("") + theme_classic() +
    scale_fill_manual(values=c("#E69F00", "#56B4E9")) + 
    scale_colour_manual(values=rev(c("#E69F00", "#56B4E9"))) + 
    theme(legend.title=element_blank(),
          text = element_text(size=14),
          axis.text = element_text(size=12)) + 
    guides(label=FALSE, colour=FALSE, fill=F) +
    geom_text(size = 3, position = position_stack(vjust = 0.5), fontface="bold") + 
    geom_text(data=textData, aes(x=x, y=y, label=label, colour=Source), vjust="bottom", hjust="right")

  # add share of GDP
  g1 <- g1 + geom_point(aes(y=ShareYTrans, shape=ShareVar), col="black", fill="black", size=2.5) + 
    geom_text(aes(x=as.numeric(Year)+0.1, y=ShareYTrans+0.25, label=signif(Share*100, 2)), size=3) +
    geom_path(aes(group=VSL, x=as.numeric(Year), y=ShareYTrans)) +
    scale_y_continuous(sec.axis = sec_axis(~./scale*100, name = "Share of GDP (%)")) +
    scale_shape_manual(name="", breaks=c("share of GDP"), labels=c("Share of GDP\n(right axis)"), values=23) +
    theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
          legend.position = c(0.1, 0.85),
          legend.text = element_text(size=10)) +
    geom_text(data=textData2, aes(x=x, y=y, label=label), size=3, colour="black", vjust="bottom", hjust="left") + 
    geom_segment(data=arrowData, aes(x=x, xend=xend, y=y, yend=yend), inherit.aes=F, size = 0.5, arrow = arrow(length = unit(0.25, "cm"))) 
  

  # Panel 2: Emissions (load from sector section)
  emissionsSummaryTotals$year <- c(2008, 2011, 2014)
  emissionsSummaryTotals$PM10 <- NULL
  emissionsSummaryTotals$VOC_B <- NULL
  emissionsPlot <- melt(emissionsSummaryTotals, id.vars = "year")
  
  emissionsPlot$variable <- mapvalues(emissionsPlot$variable, from="VOC_A", to="VOC")
  emissionsPlot$variable <- factor(emissionsPlot$variable, levels = rev(c("VOC", "PM25", "NH3", "NOX", "SO2")))
  
  plotExpressions <- c(expression("VOC"), expression("PM"[2.5]), expression("NH"[3]), expression("NO"[x]), expression("SO"[2])) 
  
  g2 <- ggplot(emissionsPlot, aes(x=factor(year), y=value/1E6, fill=variable, label=signif(value/1E6, 2))) + 
    geom_bar(stat='identity', color="black") + theme_classic() +
    ylab("Total annual emissions\n(million tons)") + xlab("") + 
    theme(legend.title=element_blank(),
          text = element_text(size=14),
          axis.text = element_text(size=12),
          legend.text.align = 0) +
    scale_fill_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'),
                      labels=rev(plotExpressions)) + 
    geom_text(size = 3, position = position_stack(vjust = 0.5), fontface="bold")
  
  legend2 <- g_legend(g2)
  
  # Panel 3: Average marginal damages (emissions-weighted using values for medium stacks)
  medAvgMD <- melt(loadAverageMD(emissionsWeighted=F), id.vars="Year") 
  
  # Index to 2008
  vals2008 <- medAvgMD[medAvgMD$Year == 2008,]; colnames(vals2008)[3] <- "value2008"
  medAvgMD <- merge(medAvgMD, vals2008[,c("variable", "value2008")], by="variable")
  medAvgMD$indexed <- medAvgMD$value / medAvgMD$value2008
  
  # standardize pollutant colors
  medAvgMD$variable <- factor(medAvgMD$variable, levels=rev(c("VOC_A", "PM25", "NH3", "NOx", "SO2")))
  medAvgMD$variable <- mapvalues(medAvgMD$variable, from=c("VOC_A", "NOx"), to=c("VOC", "NOx"))
  
  g3 <- ggplot(medAvgMD, aes(x=factor(Year), y=value/1E3, color=variable, group=variable, fill=variable)) + 
    geom_path() + geom_point(shape=23, size=4) + theme_classic() +
    scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) +
    scale_fill_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) + xlab("") +
    ylab("Average marginal damage\n(thousand $ per ton)") + ylim(c(0, 78)) +
    theme(legend.title=element_blank(),
          text = element_text(size=14),
          axis.text = element_text(size=12),
          plot.margin = unit(c(5.5, 40.5, 5.5, 5.5), "points")) +
    annotate("text", x = 3.75, y = 0, label = "", parse = TRUE, hjust = 0) +
    annotate("text", x = 3.25, y = 33, label = "SO[2]", parse = TRUE, hjust = 0, color='#e41a1c', fontface="bold") +
    annotate("text", x = 3.25, y = 75, label = "PM[2.5]", parse = TRUE, hjust = 0, color='#984ea3', fontface="bold") +
    annotate("text", x = 3.25, y = 48, label = "NH[3]", parse = TRUE, hjust = 0, color='#4daf4a', fontface="bold") +
    annotate("text", x = 3.25, y = 15, label = "NO[x]", parse = TRUE, hjust = 0, color='#377eb8', fontface="bold") +
    annotate("text", x = 3.25, y = 3, label = "VOC", parse = TRUE, hjust = 0, color='#ff7f00', fontface="bold")
  
  g1 <- g1 + theme(plot.title = element_text(face="bold"), legend.position = "none") + ggtitle("a")
  g2 <- g2 + theme(plot.title = element_text(face="bold")) + ggtitle("b")
  g3 <- g3 + theme(plot.title = element_text(face="bold"),  legend.position = "none") + ggtitle("c")
  
  # turn off clipping for annotation in Panel 1
  myGrob <- grobTree(segmentsGrob(x0=0, x1=0.5, y0=1, y1=0))
  myGrob2 <- grobTree(textGrob(label="(right axis)", x=0, y=0, just=c("left", "bottom")))
  
  g1 <- g1 + annotation_custom2(myGrob, xmin=0, xmax=1, ymin=2.35, ymax=2.85, data=data.frame(VSL="High estimate"))
  g1 <- g1 + annotation_custom2(myGrob, xmin=0, xmax=1, ymin=1.65, ymax=2.15, data=data.frame(VSL="High estimate"))
  #g1 <- g1 + annotation_custom2(myGrob2, xmin=3, xmax=3.5, ymin=1.75, ymax=2, data=data.frame(VSL="Baseline"))
  
  # see annotation custom function here: https://stackoverflow.com/questions/32807665/removing-one-tablegrob-when-applied-to-a-box-plot-with-a-facet-wrap
  
  g1 <- ggplot_gtable(ggplot_build(g1))
  g1$layout$clip[g1$layout$name %in% c("panel-1-1", "panel-2-1", "panel-3-1")] <- "off"

  lay <- rbind(c(1,1),
               c(2,3))
  
  panelPlot <- grid.arrange(g1, g2, g3, layout_matrix = lay, heights = c(0.55, 0.45), widths=c(0.5, 0.5))
  setwd(paste(baseWD, "figures", "Total damages", sep="/"))
  ggsave(plotName, panelPlot, width=8, height=6)
  
}

# custom annotation plotting function
# source: https://stackoverflow.com/questions/32807665/removing-one-tablegrob-when-applied-to-a-box-plot-with-a-facet-wrap
annotation_custom2 <- function(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
    layer(data = data, stat = StatIdentity, position = PositionIdentity, 
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = F, params = list(grob = grob, 
                                            xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax))
}

totalDamagePanelPlotV2(totalDamageRange, emissions, emissionsSummaryTotals, plotName="Damage and GDP panel with sensitivity.pdf")


## SAVE ####
rm(md)
setwd(baseWD)
save.image("Damage analysis.RData")



## Extra analysis ####

# mdSummaries <- llply(years, loadMDSummary, run="Baseline")
# 
# percentChange <- list()
# 
# percentChange[[1]] <- round((mdSummaries[[2]][,c("NH3", "NOx", "PM25", "SO2", "VOC")] - mdSummaries[[1]][,c("NH3", "NOx", "PM25", "SO2", "VOC")]) / mdSummaries[[1]][,c("NH3", "NOx", "PM25", "SO2", "VOC")] * 100)
# percentChange[[2]] <- round((mdSummaries[[3]][,c("NH3", "NOx", "PM25", "SO2", "VOC")] - mdSummaries[[2]][,c("NH3", "NOx", "PM25", "SO2", "VOC")]) / mdSummaries[[2]][,c("NH3", "NOx", "PM25", "SO2", "VOC")] * 100)
# 
# percentChange[[1]] <- cbind(mdSummaries[[1]][,c("fips", "stack")], percentChange[[1]])
# percentChange[[2]] <- cbind(mdSummaries[[1]][,c("fips", "stack")], percentChange[[2]])
# 
# tx1 <- percentChange[[1]][percentChange[[1]]$fips > 48000 & percentChange[[1]]$fips < 49000,]
# tx2 <- percentChange[[2]][percentChange[[2]]$fips > 48000 & percentChange[[2]]$fips < 49000,]
# 
# # Fort Bend: 48157
# # Harris: 48201
# # Dallas: 48113
# # Tarrant: 48439
# 
# tx1[tx1$fips %in% c(48157, 48201, 48113, 48439),]
# tx2[tx2$fips %in% c(48157, 48201, 48113, 48439),]
# 
# pop[[2]][pop[[2]]$fips %in% c(48157, 48201, 48113, 48439), "population"] - pop[[1]][pop[[1]]$fips %in% c(48157, 48201, 48113, 48439), "population"]
# pop[[3]][pop[[3]]$fips %in% c(48157, 48201, 48113, 48439), "population"] - pop[[2]][pop[[2]]$fips %in% c(48157, 48201, 48113, 48439), "population"]
# 
# combinedMDs <- cbind(mdSummaries[[1]][,c("fips", "stack", "NH3", "NOx", "PM25", "SO2", "VOC")], 
#                mdSummaries[[2]][,c("NH3", "NOx", "PM25", "SO2", "VOC")],  mdSummaries[[3]][,c("NH3", "NOx", "PM25", "SO2", "VOC")] )
# 
# txMDs <- combinedMDs[combinedMDs$fips > 48000 & combinedMDs$fips < 49000 & combinedMDs$stack == "area", ]
# colnames(txMDs)[-(1:2)] <- paste0(colnames(txMDs)[-(1:2)], "_", rep(years, each=5))
# txMelted <- melt(txMDs, id.vars = c("fips", "stack"))
# txMelted$year <- gsub("[A-z0-9]+_", "", txMelted$variable)
# txMelted$pollutant <- gsub("_[0-9]+", "", txMelted$variable)
# 
# ggplot(txMelted[txMelted$fips %in% c(48157, 48201, 48113, 48439),], aes(x=factor(fips), y=value, fill=year)) + 
#   geom_bar(stat="identity", position="dodge") + facet_wrap( ~ pollutant, ncol=2, scales = "free") 
# 
# percentChange[[1]]$period <- "2008 to 2011"
# percentChange[[2]]$period <- "2011 to 2014"
# 
# percentChangeMelt <- rbind(percentChange[[1]], percentChange[[2]])
# percentChangeMelt <- melt(percentChangeMelt, id.vars = c("fips", "stack", "period"))
# 
# keyFips <- rowSums(sign(percentChange[[1]][, c("NH3", "NOx", "PM25", "SO2", "VOC")]) != sign(percentChange[[2]][, c("NH3", "NOx", "PM25", "SO2", "VOC")]))
# keyFips <- percentChange[[1]]$fips[keyFips==1]
# colnames(percentChangeMelt)[-c(1:3)] <- c("pollutant", "percent.change")
# 
# ggplot(percentChangeMelt[percentChangeMelt$fips %in% keyFips & percentChangeMelt$fips > 48000 & percentChangeMelt$fips < 49000
#                          & percentChangeMelt$stack == "area",], aes(x=factor(fips), y=percent.change, fill=period)) + 
#   geom_bar(stat="identity", position="dodge") + facet_wrap( ~ pollutant, ncol=2, scales = "free") + xlab("fips code") + ylab("percent change")
# ggsave("Percent change TX counties.pdf")
# 
# write.csv(percentChangeMelt, "Percentage change in MD.csv", row.names = F)

# selectedFips <- fips[fips < 13000 & fips > 12000]
# flDamages1 <- damageMatrices[[1]][, fips %in% selectedFips]
# flDamages2 <- damageMatrices[[3]][, fips %in% selectedFips]
# 
# colnames(flDamages1) <- paste0(selectedFips, "_2008")
# colnames(flDamages2) <- paste0(selectedFips, "_2014")
# 
# flDamages <- as.data.frame(cbind(fips, flDamages1, flDamages2))
# 
# oneFips <- "12099"
# flComp <- flDamages[order(flDamages[,paste0(oneFips, "_2014")], decreasing=T), c("fips", paste0(oneFips, c("_2008","_2014")))]
# flComp$diff <- signif(flComp[,paste0(oneFips, "_2014")] - flComp[,paste0(oneFips, "_2008")], 3)
# flComp <- merge(flComp, pop[[1]], by="fips", all.x=T)
# flComp <- merge(flComp, pop[[3]], by="fips", all.x=T)
# 
# flComp$perCapApprox <- round(flDamages[,paste0(oneFips, "_2014")] / flComp$population.y - flDamages[,paste0(oneFips, "_2008")] / flComp$population.x, 3)
# 
# head(flComp[order(flComp$perCapApprox, decreasing=T),], 20)
# 
# for (i in selectedFips){
#   flDamages[, paste0("diff_", i)] <- flDamages[, paste0(i, "_2014")] - flDamages[, paste0(i, "_2008")]
# }
# 
# results <- data.frame()
# for (i in selectedFips){
#   varName <- paste0("diff_", i)
#   temp <- flDamages[which.max(flDamages[, varName]), c("fips", varName)]
#   results <- rbind(results, data.frame(receptor=i, source=temp[,1], diff=temp[,2]))
# }
# 
# colnames(flDamages) <- c("fips", "damages2008", "damages2014")
# # flDamagesSub <- flDamages[flDamages$fips>28000 & flDamages$fips<29000 | flDamages$fips < 2000 | flDamages$fips > 12000 & flDamages$fips < 13000, ]
# 
# 
# flDamages$diff <- flDamages$damages2014 - flDamages$damages2008
# flDamages[, c("damages2008", "damages2014", "diff")] <- flDamages[, c("damages2008", "damages2014", "diff")]  / 1E6
# head(flDamages[order(flDamages$diff, decreasing=T),], 30)

