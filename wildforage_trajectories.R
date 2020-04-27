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










### Creating intermediate dfs to filtering reward by condition and renumbering trials
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
    bigmasterdf <-rbind(bigmasterdf,intermediate)
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
  
  #creating a reward df (rwd_df) that enables pie chart visualization for each subject
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

  #saving arena_plot for each subject
  #plot(arena_plot) 
  imagename  <- paste0(s,"_heading_32.jpeg")
  imagepath <- paste0("Desktop/Archive/")
  #ggsave(imagename, device = "jpg", path = imagepath,width = 20, height = 10)
 
  
  #saving pie chart for each subject
  pie <- ggplot(rwd_df,aes(x="",y=prop,fill=condition)) +
    geom_bar(width = 1,stat="identity",color="white") +
    coord_polar("y",start=0) +
    scale_fill_manual(values=mycols) +
    geom_text(aes(y=lab.ypos,label=prop),color="white")+
    theme_void()
  
  pie
  piename  <- paste0(s,"_piechart.jpeg")
  ggsave(piename, device = "jpg", path = imagepath)
}




### Creating pie charts of proportionality
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
"""
### master code for stuff that works

which_subject <- "WILD_P02"
intermediate <- filter(masterdf, subject==which_subject,grepl("crow",run))
intermediate <- arrange(intermediate,starttimeUTC)
intermediate$condition <- c("crow")
intermediate$trialorder <- intermediate$trial
intidx <- unique(intermediate$run) #unique_runs

for (i in 2:length(intidx)){
  intermediate[intermediate$run==intidx[i],"trialorder"] <- intermediate[intermediate$run==intidx[i],"trialorder"] + 4
  }
"""


"""
### trying to work with dfs to reassign trials and conditions

singlesubjecttime <- data.frame()
which_subject <- "WILD_P02"
singlesubjecttime <- filter(timedf, subject==which_subject)
singlesubjecttime <- singlesubjecttime %>%
  arrange((UTC))
"""


"""
### Trying this 4x8 grid thing for individual trials
which_subject <- "WILD_P01"
which_run <- "water_4"
which_trial <- "1"

ex <- filter(masterdf, subject==which_subject,run==which_run)
reward <- filter(ex, reward.count.per.TR==1)

arena_plot <- masterdf %>%
  filter(subject==which_subject, run==which_run) %>%
  ggplot(aes(x=player.x,y=player.y,color=player.heading)) +
  geom_point() +
  scale_colour_gradientn(colours=rainbow(10)) + 
  xlab("player x coordinate") +
  xlim(-15000,15000) +
  ylab("player y coordinate") +
  ylim(15000,-15000) +
  ggtitle(paste0(which_subject))
  #facet_grid(run ~ trial)

#masterdf eventually has to be coded with conditions ("deer", "shrub", etc.) and trials as 1-8
arena_plot

## trying to do plot_list and multiplot function
plot_list <- list()

#for (t in 1:length(trials)){
for (t in trials){
  ex <- filter(masterdf, subject==which_subject,run==which_run)
  ex <- filter(ex, trial==t)
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
    geom_point(data = water,aes(x=posx,y=posy), color = "darkblue", size = 2, alpha=0.5)
  plot_list[[t]] <- arena_plot
}
arena_plot

plot_list[[t]]

multiplot(plot_list[[4L]],plot_list[[4L]],plot_list[[4L]],plot_list[[4L]],cols=2)
grid.arrange(plot_list[[4L]],plot_list[[4L]],ncol=2)

#grid.arrange(arena_plot,arena_plot,arena_plot,arena_plot,arena_plot,arena_plot,arena_plot,arena_plot,arena_plot,arena_plot,ncol=8)
"""

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



### Master code for single plot
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

















### Modeling to fit Levy
xs <- 0:10
cauchy_theoretical <- dcauchy(xs, location = 0, scale = 1, log = FALSE)
cauchy.th <- data.frame(xs, cauchy_theoretical)
names(cauchy.th) <- c("Value", "Freq")
cauchy.th$CumFreq <- cumsum(cauchy_theoretical)
cauchy.th$Source <- "Theoretical"


# Correlated random walk
par(mfrow=c(2,2))
trj <- TrajGenerate(n = 200)
# Plot it
plot(trj)
mtext("a)", 3, -1.3, adj = .05)

# Directed walk
trj <- TrajGenerate(n = 20, random = FALSE)
plot(trj)
mtext("b)", 3, -1.3, adj = .05)

# Brownian motion
trj <- TrajGenerate(n = 500, angularErrorDist = function(n) stats::runif(n, -pi, pi))
plot(trj)
mtext("c)", 3, -1.3, adj = .05)

# Levy walk - path lengths follow a cauchy distribution
trj <- TrajGenerate(linearErrorDist = stats::rcauchy)
plot(trj)
mtext("d)", 3, -1.3, adj = .05)





# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




### creating script for reward count - TURN THIS INTO A NEW SCRIPT
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