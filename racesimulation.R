## simulate race
##
## Input: a dataframe with a row per heat
## Heat Starttime Distance N min_time max_time
##
## output: a dataframe
## Time N_on_track Passes
##
## Each subject is assigned a duration from which speed (meter/min) is derived
## Race is simulated minute by minute and since last minute
## Total distance, distance on route, rank on route (increase since last minute)

library(tidyverse)
library(lubridate)

tracklength <- 7

## Input
heats <- 
  tribble(
    ~Heat, ~Starttime, ~Distance,  ~N, ~mintime, ~maxtime, 
  "21A", ymd_hms("2020-11-15 09:00:00"), 21,  50, 108, 160,
  "14A", ymd_hms("2020-11-15 10:00:00"), 14,  50,  75, 100,
  "14B", ymd_hms("2020-11-15 11:30:00"), 14,  50, 100, 130,
  "14C", ymd_hms("2020-11-15 12:15:00"), 14,  50, 100, 130,
  "07A",  ymd_hms("2020-11-15 13:30:00"),  7,  50,  35,  50,
  "07B",  ymd_hms("2020-11-15 14:15:00"),  7,  50,  50,  75,
  "07C",  ymd_hms("2020-11-15 14:30:00"),  7,  50,  75,  90
  )

cat("Last runner should arrive:\n")
print( max(heats$Starttime + minutes(heats$maxtime)) )

## print heats, but adding pace (min/km) ranges for each group
## perhaps add column just with clock-time (and no date). prettify.

## simulate runners initial

## pace = min/km convert to speed (meter/min) or km/h
runners <-   tribble(
  ~No, ~Duration, ~Heat, ~Distance,  ~Starttime
  )

for (heat in 1:nrow(heats)) {
  runners.heat.no <- heat*100+1:heats[heat,]$N   
  runners.heat.duration <- duration(round( runif(heats[heat,]$N, heats[heat,]$mintime, heats[heat,]$maxtime ) ), units = "minutes")
  runners.heat <- tibble( runners.heat.no,
                         runners.heat.duration,
                         heats[heat,]$Heat,
                         heats[heat,]$Distance,
                         heats[heat,]$Starttime)
  runners <- rbind(runners,runners.heat)
}
colnames(runners) <- c("No", "Duration", "Heat", "Distance", "Starttime")

runners$Endtime <- runners$Starttime + runners$Duration
runners$pace <- runners$Duration/runners$Distance
runners$speed <- 1/as.numeric(runners$pace)*60
runners$delay <- as.numeric(runners$Starttime-min(runners$Starttime),unit="mins")

timerange <- range(runners$Starttime,runners$Endtime)
raceduration <- timerange[2] - timerange[1]
min.max <- as.numeric(raceduration,units="mins")

Sims <- matrix(0,nrow=nrow(runners),ncol=min.max) 
for (i in 1:nrow(runners)) {
  N <- as.numeric(runners[i,]$Duration,unit="mins")
  idx <- runners[i,]$delay + 1:N
  Sims[i,idx] <- (1:N)*runners[i,]$speed  
}

Sims.ontrack <- Sims>0
n.ontrack <- apply(Sims.ontrack,2,sum)

plot(n.ontrack)

Sims.location <- Sims %% tracklength

Sims.rank <- apply(Sims.location, 2, rank)

Sims.rankdiff <- Sims.rank
for (j in 2:ncol(Sims.rank)) 
  Sims.rankdiff[,j] <- Sims.rank[,j] - Sims.rank[,j-1]  

## make a matrix with distance %% route_length which is location on route
## make a histogram of 'arrival times' (use kernel-density to smooth)



