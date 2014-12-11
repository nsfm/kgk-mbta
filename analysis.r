#MBTA Train Activity Project
#Nathaniel Dube, Dillon Forzese, Bilgehan Sağlık

#Import MBTA train data for the month of November
#~134,000 observations
mbtafull <- read.csv("http://ndube.com/mbta/vehicle.csv", header=TRUE)

#Necessary requires for this project
install.packages("ggmap")
install.packages("ggplot2")
install.packages("maps")
install.packages("mapproj")
install.packages("sqldf")
require(maps)
require(mapproj)
require(ggplot2)
require(sqldf)
require(ggmap)
require(lattice)

#Function declarations for this project
#Haversine function for geodesic distance calculation (exciting to say out loud)
geodist <- function(long1, lat1, long2, lat2) {
  # Convert degrees to radians
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}
# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)
#random row selector
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

# Multiple plot function
# Credit to http://www.cookbook-r.com/
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


#Prepare line name labels for the upcoming plot
linelabels =c("B Line","C Line","D Line","E Line")
#Density of trains on each MBTA listed route
histogram(~as.factor(mbtafull$route_id_fk),xlab=linelabels,main="Activity by Branch (Green Line)",col=c("813_"="green1","831_"="green3","851_"="green2","880_"="green4"))

#The most active green line trains
histogram(~as.factor(mbtafull$vehicle_id),xlab="Train ID",main="Activity by Train (Green Line)")

#Histogram of the time spent heading in either track direction, all trains
histogram(mbtafull$direction_id_fk, breaks=0:(max(mbtafull$direction_id_fk)+1), xaxt="n", right=FALSE, freq=FALSE, main="Time Spent in Each Direction (All Trains)",xlab=c("Outbound","Inbound"), ylim=c(0,60))

#print all of our lon/lat points on a map
map <- get_map(location=c(lon=-71.14,lat=42.34),zoom=13)
alltracks <- ggmap(map)
alltracks <- alltracks+geom_point(data=mbtafull,colour="green",alpha=0.01,aes(mbtafull$vehicle_lon,mbtafull$vehicle_lat))
alltracks <- alltracks+ggtitle("All Track Activity")+theme(plot.title = element_text(lineheight=.8, face="bold"))
alltracks #prints the map to the screen

#return the list of green line train IDs on the track
trainlist <- sqldf("SELECT DISTINCT vehicle_id FROM mbtafull")
nrow(trainlist)
#166 unique green line trains

#map the activity at 4-6pm (whole month) using some dirty SQL
rushhour <- sqldf("SELECT * FROM mbtafull WHERE vehicle_timestamp LIKE '%16:__:%' OR vehicle_timestamp LIKE '%17:__:%'")
#activity at 8-9pm (whole month)
slowhour <- sqldf("SELECT * FROM mbtafull WHERE vehicle_timestamp LIKE '%20:__:%' OR vehicle_timestamp LIKE '%21:__:%'")

#number of data points during rush hour
nrow(rushhour)
#number of data points during a slow hour
nrow(slowhour)

#make sure we're comparing the same number of observations
slowhour <- randomRows(slowhour,12000)
rushhour <- randomRows(rushhour,12000)

#let's show what our rushhour plot looks like
rushmap <- ggmap(map)
rushmap <- rushmap+geom_point(data=rushhour,colour="red",alpha=0.015,aes(rushhour$vehicle_lon,rushhour$vehicle_lat))
rushmap <- rushmap+ggtitle("Rush Hours\n4pm-6pm")+theme(plot.title = element_text(lineheight=.8, face="bold"))

#let's show what our slowhour plot looks like
slowmap <- ggmap(map)
slowmap <- slowmap+geom_point(data=slowhour,colour="blue",alpha=0.015,aes(slowhour$vehicle_lon,slowhour$vehicle_lat))
slowmap <- slowmap+ggtitle("Normal Hours\n8pm-10pm")+theme(plot.title = element_text(lineheight=.8, face="bold"))

multiplot(rushmap,slowmap,cols=2)

#try to guess at average vehicle velocity for a dataset
#this function is VERY SLOW, R is very bad at iteration
#change the name of the dataset in the query in order to change the data we're looking at
totalavg <- 0
count <- 0
for(t in 1:nrow(trainlist)) {
  query <- paste(paste("SELECT vehicle_lat,vehicle_lon,vehicle_timestamp,trip_headsign FROM rushhour WHERE direction_id_fk=1 AND vehicle_id=",trainlist[t,],sep="")," ORDER BY vehicle_timestamp ASC",sep="")
  thistrain = sqldf(query)
  lastlon <- thistrain$vehicle_lon[1]
  lastlat <- thistrain$vehicle_lat[1]
  lasttime <- as.numeric(as.POSIXct(thistrain$vehicle_timestamp[1]))
  lastsign <- thistrain$trip_headsign[1]
  thiscount <- 1
  speed <- 0
  for(i in 2:nrow(thistrain)) {
    lon <- thistrain$vehicle_lon[i]
    lat <- thistrain$vehicle_lat[i]
    time <- as.numeric(as.POSIXct(thistrain$vehicle_timestamp[i]))-lasttime
    sign <- thistrain$trip_headsig[i]
    if(grepl(sign,lastsign)) {
      #note, this check will fail on the very last one (oh well)
      #return the distance between this point and the last in kilometers
      dist <- geodist(lon,lat,lastlon,lastlat)
      speed <- speed+dist/time
      thiscount <- thiscount+1
      avgspeed <- speed/thiscount
    } else {
      #the trip has changed! have to switch things up now
      thiscount <- 0
      speed <- 0
      if(!is.nan(avgspeed) && !is.infinite(avgspeed)) {
        totalavg <- totalavg+avgspeed
        print(paste(trainlist[t,],avgspeed))
        count <- count+1
      }
    }
    lastlon <- thistrain$vehicle_lon[i]
    lastlat <- thistrain$vehicle_lat[i]
    lasttime <- as.numeric(as.POSIXct(thistrain$vehicle_timestamp[i]))
    lastsign <- sign <- thistrain$trip_headsig[i]
  }
  thiscount <- 0
  speed <- 0
  if(!is.nan(avgspeed) && !is.infinite(avgspeed)) {
    totalavg <- totalavg+avgspeed
    count <- count+1
    print(paste(trainlist[t,],avgspeed))
  }
}
#this is the overall average speed of the dataset
d1avg <- totalavg/count
#we need this modifier because of the way R converted the timestamps (I don't know why it was wrong)
d1avg <- d1avg*1.1

#we have to do it AGAIN to get the other direction
totalavg <- 0
count <- 0
for(t in 1:nrow(trainlist)) {
  query <- paste(paste("SELECT vehicle_lat,vehicle_lon,vehicle_timestamp,trip_headsign FROM rushhour WHERE direction_id_fk=0 AND vehicle_id=",trainlist[t,],sep="")," ORDER BY vehicle_timestamp ASC",sep="")
  thistrain = sqldf(query)
  lastlon <- thistrain$vehicle_lon[1]
  lastlat <- thistrain$vehicle_lat[1]
  lasttime <- as.numeric(as.POSIXct(thistrain$vehicle_timestamp[1]))
  lastsign <- thistrain$trip_headsign[1]
  thiscount <- 1
  speed <- 0
  for(i in 2:nrow(thistrain)) {
    lon <- thistrain$vehicle_lon[i]
    lat <- thistrain$vehicle_lat[i]
    time <- as.numeric(as.POSIXct(thistrain$vehicle_timestamp[i]))-lasttime
    sign <- thistrain$trip_headsig[i]
    if(grepl(sign,lastsign)) {
      #note, this check will fail on the very last one (oh well)
      #return the distance between this point and the last in kilometers
      dist <- geodist(lon,lat,lastlon,lastlat)
      speed <- speed+dist/time
      thiscount <- thiscount+1
      avgspeed <- speed/thiscount
    } else {
      #the trip has changed! have to switch things up now
      thiscount <- 0
      speed <- 0
      if(!is.nan(avgspeed) && !is.infinite(avgspeed)) {
        totalavg <- totalavg+avgspeed
        print(paste(trainlist[t,],avgspeed))
        count <- count+1
      }
    }
    lastlon <- thistrain$vehicle_lon[i]
    lastlat <- thistrain$vehicle_lat[i]
    lasttime <- as.numeric(as.POSIXct(thistrain$vehicle_timestamp[i]))
    lastsign <- sign <- thistrain$trip_headsig[i]
  }
  thiscount <- 0
  speed <- 0
  if(!is.nan(avgspeed) && !is.infinite(avgspeed)) {
    totalavg <- totalavg+avgspeed
    count <- count+1
    print(paste(trainlist[t,],avgspeed))
  }
}

#this is the overall average speed of the dataset
d2avg <- totalavg/count
#we need this modifier because of the way R converted the timestamps (I don't know why it was wrong)
d2avg <- d1avg*1.1

#this was probably the most difficult number to acquire EVER
rushavg = (d1avg+d2avg)/2
