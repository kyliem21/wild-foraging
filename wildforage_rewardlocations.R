library(ggplot2)
library(gganimate)

df_water <- read.table("water_locations.csv", header = TRUE, sep=",")
df_bush <- read.table("bush_locations.csv", header = TRUE, sep=",")
df_deer <- read.table("deer_locations.csv", header = TRUE, sep=",")
df_crow <- read.table("crow_locations.csv", header = TRUE, sep=",")
df_moving <- read.table("deer.csv", header=TRUE, sep = ",")

par(mfrow=c(2,2))
plot(df_water$posx, df_water$posy, cex=1, pch=20, col='blue')
plot(df_bush$posx, df_bush$posy, cex=1, pch=20, col='green')
plot(df_deer$posx, df_deer$posy, cex=1, pch=20, col='red')
plot(df_crow$posx, df_crow$posy, cex=1, pch=20, col='black')

points(18.57193,-80.46991)
points(2518.572021,-80.46991)
points(18.571821, 2419.530029)
points(-2481.427979,-80.470131)
points(18.57196, -2580.469971)

par(mfrow=c(1,1))
plot(df_water$posx, df_water$posy, cex=1, pch=3, col='blue', xlim= c(-15000,15000), ylim=c(-15000,15000), xlab = "x", ylab = "y")
points(df_bush$posx, df_bush$posy, cex=1, pch=20, col='green')
points(df_deer$posx, df_deer$posy, cex=1, pch=20, col='red')
points(df_crow$posx, df_crow$posy, cex=1, pch=4, col='black')

theme_set(theme_bw())
p <- ggplot(
  df_moving,
  aes(x=df_moving$x, y=df_moving$y)
  ) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  labs(x="x",y="y")
p 

  