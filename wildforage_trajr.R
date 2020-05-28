# code to visualzie behavioral trajectories using trajr

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





### Modeling to fit Levy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
