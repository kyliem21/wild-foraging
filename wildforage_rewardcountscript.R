### creating script for reward count 
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

rewardcountdf <- data.frame()

for (s in subjects){
  this_subject <- s
  this_spath <- paste0("Desktop/Archive/",s,"/",s,"_")
  for (r in runs){
    this_run <- read.csv(paste0(this_spath,r,"/TotalRewardCount.csv"))
    this_run$run <- r
    this_run$subject <- s
    rewardcountdf <- rbind(rewardcountdf,this_run)
  }
}

rewardcountdf$total <- rewardcountdf$Crows + rewardcountdf$Deer + rewardcountdf$Shrubs + rewardcountdf$Water
rewardcountdf <- subset(rewardcountdf, select = c(5:7))
###somehow transpose
write_csv(rewardcountdf, "rewardcountdf.csv")