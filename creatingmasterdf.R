# Code to analyze behavioral trajectory data from pilot subjects in foraging task

library(data.table)
library(trajr)
library(dplyr)
library(ggplot2)
library(gganimate)
library(plotly)
library(stats)
library(tidyr)
library(tidyverse)
library(stringr)
library(plyr)
library(gridExtra)



### dataframe manipulations - adding pilot subjects and runs
subjects <- c("WILD_P01","WILD_P02","WILD_P03","WILD_P04","WILD_P05","WILD_P06","WILD_P07","WILD_P08","WILD_P09","WILD_P10","WILD_P11","WILD_P12","WILD_P13","WILD_P14","WILD_P15","WILD_P16","WILD_P17","WILD_P18","WILD_P19","WILD_P20","WILD_P21","WILD_P22","WILD_P23")
runs <- c("deer_1","crow_2","shrub_3","water_4","deer_5","crow_6","shrub_7","water_8")
#runs <- c("shrub_3")
trials <- c("1","2","3","4")
runnumbers <- c("1","2","3","4","5","6","7","8")
water <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_water_4/water_locations.csv")
shrub <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_shrub_3/bush_locations.csv")
#crow <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_crow_2/crow_locations.csv")
#deer <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_deer_1/deer_locations.csv")

### creating master df for all subjects and all runs
masterdf <- data.frame()
timedf <- data.frame()
rewarddf <- data.frame()

for (s in subjects){
  this_subject <- s
  this_spath <- paste0("Desktop/Archive/",s,"/",s,"_")
  for (r in runs){
    this_run <- read.csv(paste0(this_spath,r,"/moving_actor.csv"))
    this_run$run <- r
    this_run$subject <- s
    newtime <- read.table(paste0(this_spath,r,"/ScanStartTimeUTC.csv"))
    newtime <- unite(newtime, UTC, 1:2,sep = " ", remove = TRUE)
    newtime$UTC <- as.POSIXct(newtime$UTC)
    newtime$run <- r
    newtime$subject <- s
    newreward <- read.csv(paste0(this_spath,r,"/TotalRewardCount.csv"))
    newreward$totalrwd <- newreward$Crows + newreward$Deer + newreward$Shrubs + newreward$Water
    newreward$run <- r
    newreward$subject <- s
    timedf <- rbind(timedf,newtime)
    rewarddf <- rbind(rewarddf,newreward)
    this_run$starttimeUTC <- newtime$UTC
    this_run$totalrwd <- newreward$totalrwd
    masterdf <- rbind(masterdf,this_run)
    masterdf$UTC <- as.character(masterdf$UTC)
  }
}

bigmasterdf <- data.frame()

for (s in subjects){
  which_subject <- s
  intermediate <- filter(masterdf, subject==which_subject)
  intermediate <- arrange(intermediate,starttimeUTC)
  intermediate$runorder <- 1
  intermediate$run
  orderidx <- unique(intermediate$starttimeUTC) #unique times
  
  for (i in 2:length(orderidx)){
    intermediate[intermediate$starttimeUTC==orderidx[i],"runorder"] <- intermediate[intermediate$starttimeUTC==orderidx[i],"runorder"] + (i-1)
  }
  bigmasterdf <-rbind(bigmasterdf,intermediate)
}

bigmasterdf <- bigmasterdf %>%
  arrange((starttimeUTC))

finalmasterdf <- data.frame()

### Creating the biggest master
for (s in subjects){
  gg_df <- data.frame()
  intermediate <- data.frame()
  which_subject <- s
  rwdvec <- c("crow","deer","water","shrub")
  
  # creating a massive plotting df (gg_df) for a single subject
  for (v in rwdvec){
    intermediate <- filter(bigmasterdf, subject==which_subject,grepl(v,run))
    intermediate <- arrange(intermediate,starttimeUTC)
    intermediate$condition <- c(v)
    intermediate$trialorder <- intermediate$trial
    intidx <- unique(intermediate$run) #unique_runs
    orderidx <- unique(intermediate$starttimeUTC) #unique times
    
    for (i in 2:length(intidx)){
      intermediate[intermediate$run==intidx[i],"trialorder"] <- intermediate[intermediate$run==intidx[i],"trialorder"] + 4
    }
    gg_df <- rbind(gg_df,intermediate)
    finalmasterdf <-rbind(finalmasterdf,intermediate)
  }
}


write.csv(finalmasterdf,"Desktop/Archive/MASTERDF.csv", row.names = FALSE)