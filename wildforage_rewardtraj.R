# code to visualise post-reward stuff

library(data.table)
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
library(trajr)

subjects <- c("WILD_P01","WILD_P02","WILD_P03","WILD_P04","WILD_P05","WILD_P06","WILD_P07","WILD_P08","WILD_P09","WILD_P10","WILD_P11","WILD_P12","WILD_P13","WILD_P14","WILD_P15","WILD_P16","WILD_P17","WILD_P18","WILD_P19","WILD_P20","WILD_P21","WILD_P22","WILD_P23")
runs <- c("deer_1","crow_2","shrub_3","water_4","deer_5","crow_6","shrub_7","water_8")
waterorder <- c("1","2")
trials <- c("1","2","3","4")
lasttrials <- c("5","6","7","8")
runnumbers <- c("1","2","3","4","5","6","7","8")
water <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_water_4/water_locations.csv")
shrub <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_shrub_3/bush_locations.csv")
#crow <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_crow_2/crow_locations.csv")
#deer <- read.csv("Desktop/Archive/WILD_P03/WILD_P03_deer_1/deer_locations.csv")
masterdf <- read.csv("Desktop/Archive/MASTERDF.csv")
waterdf <-  read.csv("Desktop/Archive/WATERCONDDF.csv")


##can do by subject, by run, then by trial. take trials 5-8 and turn into trajectories. compute straightness, sinuosity, directional change

names <- c("subject","trialorder","straight","sinuos","dirchg")
trajrdf <- data.frame()
for (k in names) trajrdf[[k]] <- as.character()


for (s in subjects){
  df <- filter(waterdf, subject==s, waterorder==2, as.integer(gameplay.)==2)
  for (t in lasttrials){
    newdf <- filter(df, trialorder==t)
    newdf <- newdf %>% select(player.x,player.y,time)
    newtraj <- TrajFromCoords(newdf)
    
    st <- TrajStraightness(newtraj)
    sin <- TrajSinuosity(newtraj)
    dchange <- mean(TrajDirectionalChange(newtraj))
    intdf <- data.frame(s,t,st,sin,dchange)
    names(intdf) <- c("subject","trialorder","straight","sinuos","dirchg")
    
    trajrdf <- rbind(trajrdf, intdf)
    #trajdf gives us a dataframe containing trajr-calculated measures of straightness, sinuosity, and directional change on a (late) trial by trial basis 
  }
}

#find avg for last 4 trials and store in df
names <- c("subject","avgstraight","avgsinuos","avgdirchg")
avgdf <- data.frame()
for (k in names) avgdf[[k]] <- as.character()

for (s in subjects){
    newdf <- subset(trajrdf,subject==s)
    avgst <- mean(newdf$straight)
    avgsin <- mean(newdf$sinuos)
    avgdirchg <- mean(newdf$dirchg)
    intdf <- data.frame(s,avgst,avgsin,avgdirchg)
    names(intdf) <- c("subject","avgstraight","avgsinuos","avgdirchg")
    
    avgdf <- rbind(avgdf, intdf)
    #avgdf averages trial by trial measures presented in trajrdf for a single score for each measure per participant
}




### Taking one subject-run-trial and turing into trajectory into the 3 sec pre and post reward collection
### Very very important and exciting yay

names <- c("subject","trialorder","straight","sinuos","dirchg")
prepostdf <- data.frame()
for (k in names) prepostdf[[k]] <- as.character()

for (s in subjects){
  df <- filter(waterdf, subject==s, waterorder==2, as.integer(gameplay.)==2)
  for (t in lasttrials){
    newdf <- filter(df, trialorder==t)
    x <- which(newdf$reward.count.per.TR>0)
    slice1 <- x-15
    slice2 <- x+15
    k <- c()
    
    #instead of appending, p gives each "trajectory"
    for (g in seq_along(slice1)){
      p <- slice1[g]:slice2[g]
      #k<-append(k,p)
      nudf <- slice(newdf,p)
      
      nudf <- nudf %>% select(player.x,player.y,time)
      newtraj <- TrajFromCoords(nudf)
      st <- TrajStraightness(newtraj)
      sin <- TrajSinuosity(newtraj)
      dchange <- mean(TrajDirectionalChange(newtraj))
      intdf <- data.frame(s,t,st,sin,dchange)
      names(intdf) <- c("subject","trialorder","straight","sinuos","dirchg")
      
      prepostdf <- rbind(prepostdf, intdf)
      #prepostdf gives us a dataframe containing trajr-calculated measures of straightness, sinuosity, and directional change...
      #...for each segment (i.e. the 3 seconds before and after a reward was collected) from late trials for each participant
    }  
    
  }
}
      

    
#find avg for last 4 trials and store in df
names <- c("subject","avgstraight","avgsinuos","avgdirchg")
avgdf <- data.frame()
for (k in names) avgdf[[k]] <- as.character()

for (s in subjects){
  newdf <- subset(prepostdf,subject==s)
  avgst <- mean(newdf$straight)
  avgsin <- mean(newdf$sinuos)
  avgdirchg <- mean(newdf$dirchg)
  intdf <- data.frame(s,avgst,avgsin,avgdirchg)
  names(intdf) <- c("subject","avgstraight","avgsinuos","avgdirchg")
  
  avgdf <- rbind(avgdf, intdf)
  #avgdf averages measures presented in prepostdf for a single score for each measure per participant
  
}
  



"""
#early working code. save as example just in case...
prepostdf <- filter(masterdf, subject=="WILD_P03", run=="water_8", trial==4, as.integer(gameplay.)==2)
x <- which(prepostdf$reward.count.per.TR>0)
slice1 <- x-15
slice2 <- x+15
k <- c()

for (s in 1:length(slice1)){
  p <- slice1[s]:slice2[s]
  k<-append(k,p)
}
prepostdf<- slice(prepostdf,k)
"""

### Taking one subject-run-trial and turing into trajectory
newtraj <- filter(masterdf, subject=="WILD_P03", run=="water_8", trial==3, as.integer(gameplay.)==2)
newtraj <- newtraj %>% select(player.x,player.y,time)
newtraj <- TrajFromCoords(newtraj)
plot(newtraj)

"""
# straightness (1= straight line)... not a good fit for us: just D/L
TrajStraightness(newtraj)

#sinuosity
TrajSinuosity(newtraj)
"""

#directional change... could be good!
mean(TrajDirectionalChange(newtraj))

# Direction autocorrelation...interesting also for sinusoidal rhythms
corr <- TrajDirectionAutocorrelations(newtraj)
plot(corr)


