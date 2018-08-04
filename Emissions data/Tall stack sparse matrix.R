# create sparse matrices for tall stacks
# 3/21/17
# updated 1/10/18

# Note: should be called by "Emissions breakdown.R"

# function to create sparse matrix (see call below)
read.stacks <- function(new.flag, tall, emissions.only=F, name=NULL){
  
  if(emissions.only){
    range <- 4
  } else{
    range <- 1:5
  }
  
  for(i in range){
    
    if (!is.null(name)){
        stacks <- read.csv(name, header=FALSE)
    } else {
      if(new.flag){
        stacks <- read.csv(paste("New_Tall_Stack_", i, ".csv", sep=""), header=FALSE)
      } else{
        stacks <- read.csv(paste("Tall_Stack_", i, ".csv", sep=""), header=FALSE)
      }
    }

    
    # join with fips list for tall stacks, save as data frame
    stacks <- cbind(tall, stacks)
    stacks <- as.data.frame(stacks)
    
    # if damages (i = 1-3 or 5) then average duplicates
    # NOTE: currently assumes same fips = same plant. Updated with list of tall stacks
    # can also check to see how important variation in damage fractions are
    
    # if emissions (i == 4) then sum across fips
    if(i != 4){
      stacks <- ddply(stacks, ~ tall, numcolwise(mean))
    } else{
      stacks <- ddply(stacks, ~ tall, numcolwise(sum))
    }
    
    # new sparse matrix
    new.stacks <- matrix(0, nrow = 3109, ncol = ncol(stacks) - 1)
    new.stacks <- as.data.frame(new.stacks)
    
    # assign fips, save as data frame
    new.stacks <- cbind(fips, new.stacks)
    colnames(new.stacks)[1] <- "tall"
    
    # combine and remove duplicate fips codes (i.e. remove zeros for counties with tall stacks)
    new.stacks <- rbind(stacks, new.stacks)
    new.stacks <- new.stacks[!duplicated(new.stacks$tall),]
    new.stacks <- new.stacks[order(new.stacks$tall),] 
    
    # remove fips code column and export
    new.stacks <- new.stacks[,-1]

    if(new.flag){
      write.table(new.stacks, paste("Sparse_New_Tall_Stack_", i, ".csv", sep=""), sep = ",", row.names=F, col.names=F)
    } else{
      write.table(new.stacks, paste("Sparse_Tall_Stack_", i, ".csv", sep=""), sep = ",", row.names=F, col.names=F)
    }
  }
}

# fips code format
tall <- fips.stacks[fips.stacks$stack == "tall", "fips"]
new.tall <- fips.stacks[fips.stacks$stack == "tall2", "fips"] 

# call sparse function for each year
for(i in 1:length(years)){
  setwd(paste(NEI.wd, "AP2 inputs", "Sparse matrices", years[i], sep="/"))
  
  read.stacks(new.flag = FALSE, tall = tall, emissions.only=T)
  read.stacks(new.flag = TRUE, tall = new.tall, emissions.only=T)
}

# clean up workspace
rm(tall); rm(new.tall)




