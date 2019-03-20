# Lorenz curve bootstrap
set.seed(123)

calcGini <- function(dataVector){
  # sort
  lorenz <- dataVector[order(dataVector)]
  # calc cumulative counties and damages
  cumulativeCounties <- 1:length(lorenz) / length(lorenz) * 100
  cumulativeDamages <- cumsum(lorenz) / sum(lorenz) * 100
  x <- c(0, cumulativeCounties)
  y <- c(0, cumulativeDamages)
  curveArea <- ABC(x, y)
  ginicoef <- GINI(curveArea)
  return(ginicoef)
}

bootstrap <- function(data, estimator){
  # draw bootstrap sample
  data.b <- sample(data, length(data), replace=T)
  # calculate parameter
  theta.hat.star <- estimator(data.b)
  return(theta.hat.star)
}

bootstrapHelper <- function(shares, var, region, year, B, estimator, alpha=0.05){
  if (year == 2008){
    yearVar = 1
  } else if (year == 2014){
    yearVar = 3
  }
  data <- shares[[yearVar]]
  if (region != "All US"){
    data <- data[data$epa.region == region, var]
  } else{
    data <- data[,var] 
  }
  theta.hat <- estimator(data)
  # bootstrap
  theta.hat.stars <- replicate(B, bootstrap(data, estimator))
  # confidence interval
  ci.lower <- 2 * theta.hat - quantile(theta.hat.stars, 1 - alpha/2)  
  ci.upper <- 2 * theta.hat - quantile(theta.hat.stars, alpha/2)
  return(data.frame(region=region, year=year, estimate=theta.hat, ci.lower=unname(ci.lower), ci.upper = unname(ci.upper)))
}

start <- proc.time()
result <- bootstrapHelper(shares, var="Export", region="All US", year=2008, B=5000, estimator=calcGini)
result <- rbind(result, bootstrapHelper(shares, var="Export", region="All US", year=2014, B=5000, estimator=calcGini))
result <- rbind(result, bootstrapHelper(shares, var="Own_damage", region="All US", year=2008, B=5000, estimator=calcGini))
result <- rbind(result, bootstrapHelper(shares, var="Own_damage", region="All US", year=2014, B=5000, estimator=calcGini))
result <- rbind(result, bootstrapHelper(shares, var="Imports", region="All US", year=2008, B=5000, estimator=calcGini))
result <- rbind(result, bootstrapHelper(shares, var="Imports", region="All US", year=2014, B=5000, estimator=calcGini))

#regions <- c("Midwest","Mid-Atlantic","Southwest","Great Plains","Mountain")
#regions <- unique(shares[[1]]$epa.region)
#for (region in regions){
#  result <- rbind(result, bootstrapHelper(shares, var="Export", region=region, year=2008, B=5000))
#  result <- rbind(result, bootstrapHelper(shares, var="Export", region=region, year=2014, B=5000))
#}
elapsed <- proc.time()-start
giniBootResults <- result; rm(result)


# #giniBootResults$Damage <- c("Exported", "Exported", "Self-inflicted", "Self-inflicted")
# # bar plot of results
# giniBootResults$year <- factor(giniBootResults$year)
# ggplot(giniBootResults, aes(x=region, y=estimate, fill=year)) +
#   geom_bar(stat = "identity", color="black", position=position_dodge()) +
#   ylab("Gini coefficient") + xlab("") + theme_classic() +
#   scale_fill_manual(values=c("#56B4E9", "#E69F00")) + 
#   theme(text = element_text(size=16, color="black"),
#         legend.title=element_blank(),
#         axis.text.x = element_text(angle=90, hjust=1, vjust=0.25)) +
#   geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), colour="black", width=0.1, position=position_dodge(.9)) +
#   geom_hline(yintercept=0, linetype=1) 
# 
# setwd("~/Documents/Carnegie Mellon/Box Sync/Research/Environmental justice/figures/Density")
# ggsave("Gini coefficent estmates.pdf")
# 
# 
# 
# # Export / import ratio results
# 
resultEI <- bootstrapHelper(shares, var="Export_import_ratio", region="All US", year=2008, B=5000, estimator=mean)
resultEI <- rbind(resultEI, bootstrapHelper(shares, var="Export_import_ratio", region="All US", year=2014, B=5000, estimator=mean))
resultEI <- rbind(resultEI, bootstrapHelper(shares, var="Export_import_ratio", region="All US", year=2008, B=5000, estimator=median))
resultEI <- rbind(resultEI, bootstrapHelper(shares, var="Export_import_ratio", region="All US", year=2014, B=5000, estimator=median))


