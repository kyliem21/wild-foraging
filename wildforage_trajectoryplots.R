# Code to visualize (trial x condition) behavioral trajectory data from pilot subjects in foraging task

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
masterdf <- read.csv("Desktop/Archive/MASTERDF.csv")



### PLOTS: 8x4 GRID PLOTS
# Creating intermediate dfs to filtering reward by condition and renumbering trials
for (s in subjects){
  gg_df <- data.frame()
  intermediate <- data.frame()
  which_subject <- s
  rwdvec <- c("crow","deer","water","shrub")
  
# creating a massive plotting df (gg_df) for a single subject
  for (v in rwdvec){
      intermediate <- filter(masterdf, subject==which_subject,grepl(v,run))
      intermediate <- arrange(intermediate,starttimeUTC)
      intermediate$condition <- c(v)
      intermediate$trialorder <- intermediate$trial
      intidx <- unique(intermediate$run) #unique_runs
      orderidx <- unique(intermediate$starttimeUTC) #unique times
      
      for (i in 2:length(intidx)){
        intermediate[intermediate$run==intidx[i],"trialorder"] <- intermediate[intermediate$run==intidx[i],"trialorder"] + 4
      }
    gg_df <- rbind(gg_df,intermediate)
  }
  
#creating a grid of 8x4 plots by trial and condition, colored by heading direction
  for (g in runs) {
    for (t in trials) {
        ex <- filter(gg_df, subject==which_subject)
        reward <- filter(gg_df, reward.count.per.TR==1)
        
        arena_plot <- ggplot(data = gg_df,
                             aes(x=gg_df$player.x,
                                 y=gg_df$player.y,
                                 color=gg_df$player.heading)) +
          scale_colour_gradientn(colours=rainbow(10)) + 
          theme(aspect.ratio=1) +
          xlab("player x coordinate") +
          xlim(-15000,15000) +
          ylab("player y coordinate") +
          ylim(15000,-15000) +
          ggtitle(paste0(which_subject)) +
          geom_point(size=0.8,alpha=0.5) +
          geom_point(data = water,aes(x=posx,y=posy), color = "darkblue", size = 1, alpha=0.2) +
          geom_point(data = shrub,aes(x=posx,y=posy), color = "darkgreen", size = 1, alpha=0.2) +
          geom_point(data = reward,aes(x=player.x,y=player.y), color = "black", size = 1) +
          facet_grid(condition ~ trialorder)
      }
  }

#saving arena_plot for each subject
  plot(arena_plot) 
  imagename  <- paste0(s,"_heading_32.jpeg")
  imagepath <- paste0("Desktop/Archive/")
  ggsave(imagename, device = "jpg", path = imagepath,width = 20, height = 10)

}




### Creating pie charts of proportionality --- tweak to make this work

rwd_df <- data.frame()
rwd_df <- distinct(gg_df,gg_df$condition,.keep_all = TRUE)
rwd_df$totalrwd <- as.numeric(rwd_df$totalrwd)
rwd_df <- rwd_df[-c(1:14,19:20)]
rwd_df$prop <- (rwd_df$totalrwd / sum(rwd_df$totalrwd)) *100
rwd_df$prop <- round(rwd_df$prop,digits=2)
mycols <- c("#000000","#FF0000","#00FF00","#0000FF")

rwd_df <- rwd_df %>%
  arrange(desc(condition)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
rwd_df

#plot
pie <- ggplot(rwd_df,aes(x="",y=prop,fill=condition)) +
  geom_bar(width = 1,stat="identity",color="white") +
  coord_polar("y",start=0) +
  scale_fill_manual(values=mycols) +
  geom_text(aes(y=lab.ypos,label=prop),color="white")+
  theme_void()

pie


### MASTER CODE FOR STUFF THAT WORKS


### 8x4 grid plot single subject ~~~~~~~~~~~~~~~

for (g in runs) {
  for (t in trials) {
    ex <- filter(gg_df, subject==which_subject)
    reward <- filter(gg_df, reward.count.per.TR==1)
    
    arena_plot <- ggplot(data = gg_df,
                         aes(x=gg_df$player.x,
                             y=gg_df$player.y,
                             color=gg_df$time)) +
      scale_colour_gradientn(colours=rainbow(10)) + 
      theme(aspect.ratio=1) +
      xlab("player x coordinate") +
      xlim(-15000,15000) +
      ylab("player y coordinate") +
      ylim(15000,-15000) +
      ggtitle(paste0(which_subject)) +
      geom_point(size=0.8,alpha=0.5) +
      geom_point(data = water,aes(x=posx,y=posy), color = "darkblue", size = 1, alpha=0.2) +
      geom_point(data = shrub,aes(x=posx,y=posy), color = "darkgreen", size = 1, alpha=0.2) +
      geom_point(data = reward,aes(x=player.x,y=player.y), color = "black", size = 1) +
      facet_grid(condition ~ trialorder)
  }
}
arena_plot



### Filtering and plotting using trajr
### Saving image plots for HEADING
for (j in subjects) {
  for (g in runs){
    ex <- filter(masterdf, subject==j,run==g)
    reward <- filter(ex, reward.count.per.TR==1)
    
    coords <- data.frame(ex$player.x, ex$player.y, ex$time)
    trj <- TrajFromCoords(coords)
    plot(trj)
    
    arena_plot <- ggplot(data = ex,
                         aes(x=ex$player.x,
                             y=ex$player.y,
                             color=ex$player.heading)) +
      geom_point() +
      scale_colour_gradientn(colours=rainbow(10)) + 
      xlab("player x coordinate") +
      xlim(-15000,15000) +
      ylab("player y coordinate") +
      ylim(15000,-15000) +
      ggtitle(paste0(j," ",g)) +
      #geom_point(data = reward,aes(x=player.x,y=player.y), color = "blue", size = 2) +
      geom_point(data = water,aes(x=posx,y=posy), color = "blue", size = 2, alpha=0.5) +
      geom_point(data = shrub,aes(x=posx,y=posy), color = "darkgreen", size = 2, alpha=0.5)  
    
    plot(arena_plot) 
    
    imagename  <- paste0(j,"_",g,"_heading.jpeg")
    imagepath <- paste0("Desktop/Archive/",j)
    ggsave(imagename, device = "jpg", path = imagepath)
  }
}


### Filtering and plotting using trajr
### Saving image plots for TIME
for (j in subjects) {
  for (g in runs){
    ex <- filter(masterdf, subject==j,run==g)
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
      xlab("player x coordinate") +
      xlim(-15000,15000) +
      ylab("player y coordinate") +
      ylim(15000,-15000) +
      ggtitle(paste0(j," ",g)) +
      #geom_point(data = reward,aes(x=player.x,y=player.y), color = "blue", size = 2) +
      geom_point(data = water,aes(x=posx,y=posy), color = "blue", size = 2, alpha=0.5) +
      geom_point(data = shrub,aes(x=posx,y=posy), color = "black", size = 2, alpha=0.5)  
    
    plot(arena_plot) 
    
    imagename  <- paste0(j,"_",g,"_trialtime.jpeg")
    imagepath <- paste0("Desktop/Archive/",j)
    ggsave(imagename, device = "jpg", path = imagepath)
  }
}



### Single Run Trajectory Plot (1 subject, 4 trials) ~~~~~~~
par(mfrow=c(1,1))

which_subject <- "WILD_P03"
which_run <- "shrub_3"
ex <- filter(masterdf, subject==which_subject,run==which_run)
reward <- filter(ex, reward.count.per.TR==1)

coords <- data.frame(ex$player.x, ex$player.y, ex$time)
trj <- TrajFromCoords(coords)
plot(trj)

arena_plot <- ggplot(data = ex,
                     aes(x=ex$player.x,
                         y=ex$player.y,
                         color=ex$player.heading)) +
  geom_point() +
  scale_colour_gradientn(colours=rainbow(10)) + 
  xlab("player x coordinate") +
  xlim(-15000,15000) +
  ylab("player y coordinate") +
  ylim(15000,-15000) +
  ggtitle(paste0(which_subject," ",which_run)) +
  #geom_point(data = reward,aes(x=player.x,y=player.y), color = "blue", size = 2) +
  geom_point(data = water,aes(x=posx,y=posy), color = "blue", size = 2, alpha=0.5) +
  geom_point(data = shrub,aes(x=posx,y=posy), color = "black", size = 2, alpha=0.5)  

plot(arena_plot)

imagename  <- paste0(which_subject,"_",which_run,".jpeg")
imagepath <- paste0("Desktop/Archive/",which_subject)
ggsave(imagename, device = "jpg", path = imagepath)




### Trying density plot
ggplot(data=ex, aes(x=ex$player.x,y=ex$player.y,color=ex$time)) +
  geom_point() +
  scale_fill_continuous(type = "viridis") +
  xlab("player x coordinate") +
  xlim(-15000,15000) +
  ylab("player y coordinate") +
  ylim(15000,-15000) +
  theme_bw()


