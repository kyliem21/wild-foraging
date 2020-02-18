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



### dataframe manipulations - adding pilot subjects and runs
subjects <- c("WILD_P01","WILD_P02","WILD_P03","WILD_P04","WILD_P05","WILD_P05","WILD_P06","WILD_P07","WILD_P08")
runs <- c("deer_1","crow_2","shrub_3","water_4","deer_5","crow_6","shrub_7","water_8")
water <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_water_4/water_locations.csv")
shrub <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_shrub_3/bush_locations.csv")
crow <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_crow_2/crow_locations.csv")

masterdf <- data.frame()
water <- filter(water, trial==1)
shrub <- filter(shrub, trial==1)


### creating master df for all subjects and all runs
for (s in subjects){
  this_subject <- s
  this_spath <- paste0("Desktop/Archive/",s,"/",s,"_")
  for (r in runs){
    this_run <- read.csv(paste0(this_spath,r,"/moving_actor.csv"))
    this_run$run <- r
    this_run$subject <- s
    masterdf <- rbind(masterdf,this_run)
  }
}

### filtering and plotting using TRAJR. Select subject and run
ex <- filter(masterdf, subject=="WILD_P03",run=="shrub_7")
reward <- filter(ex, reward.count.per.TR==1)

coords <- data.frame(ex$player.x, ex$player.y, ex$time)
trj <- TrajFromCoords(coords)
plot(trj)

arena_plot <- ggplot(data = ex,
                     aes(x=ex$player.x,
                         y=ex$player.y,
                         color=ex$time)) +
  geom_point() +
  scale_colour_gradientn(colours=rainbow(10)) + 
  #geom_point(data = reward,aes(x=player.x,y=player.y), color = "blue", size = 2) +
  #geom_point(data = water,aes(x=posx,y=posy), color = "blue", size = 2, alpha=0.5) 
  geom_point(data = shrub,aes(x=posx,y=posy), color = "green", size = 2, alpha=0.5)  

print(arena_plot) 


