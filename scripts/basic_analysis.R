import_aa <- function(x=24) {
  
  ##Import AdWords and Analytics raw csv data
  adwords_raw <<- read.csv("adwords.csv", skip=1, header=TRUE )
  campaigns <- read.csv("campaigns.csv", skip=1, sep=",", header=TRUE )[1:900,c(1,8,9)]
  campaigns <- ddply(campaigns, .(Day),numcolwise(sum))
  campaigns <- campaigns[1:24,]
  
  analytics <- read.csv("analytics.csv", skip=10,header=TRUE)[1:x,]
  analytics[,1] <- as.Date(analytics[,1], format="%m/%d/%y")
  analytics[,2] <- as.numeric(as.character(analytics[,2]))
  colnames(analytics)[1] <- "Day"
  analytics <- analytics
  
  aa <- cbind(campaigns, Sessions=analytics[,2]) 
  
  aa$ClicksSessionDiff <- aa$Clicks - aa$Sessions
  aa$CTR <- signif((aa$Clicks/aa$Impressions), digits=3)
  aa <<- aa[c(1,3,2,6,4,5)]



}