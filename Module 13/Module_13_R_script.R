library(ggplot2)
library(maps)
library(mapdata)
 
usa<-map_data("usa")
states<-map_data("state")
state_base<-ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) + coord_fixed(1.3) + geom_polygon(color="black", fill="gray")
miles<-read.csv("milesdrive.csv")
colnames(miles)<-c("State","VMT")
miles$VMT<-as.numeric(as.character(miles$VMT))
names(states)[names(states)=="region"]<-"State"
miles$State<-tolower(miles$State)
stco<-merge(states, miles, by="State")
em1<-state_base + geom_polygon(data=stco, aes(fill=VMT), color="white") + geom_polygon(color="black", fill=NA) + coord_fixed(1.3) + theme_bw() + ggtitle("Miles Driven in Different States")
em2<-em1 + scale_fill_gradient(low="lightgreen", high="darkgreen")
em2


library(ggplot2)
library(maps)
library(mapdata)
 
usa<-map_data("usa")
states<-map_data("state")
state_base<-ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) + coord_fixed(1.3) + geom_polygon(color="black", fill="gray")
life<-read.csv("lifeexp.csv")
life$Life.Expectancy<-as.numeric(as.character(life$Life.Expectancy))
names(states)[names(states)=="region"]<-"State"
life$State<-tolower(life$State)
stco<-merge(states, life, by="State")
el1<-state_base + geom_polygon(data=stco, aes(fill=Life.Expectancy), color="white") + geom_polygon(color="black", fill=NA) + coord_fixed(1.3) + theme_bw() + ggtitle("Life Expectancy in Different States")
el2<-el1 + scale_fill_gradient(low="yellow", high="darkorange")
el2


library(ggplot2)
library(maps)
library(mapdata)
 
usa<-map_data("usa")
states<-map_data("state")
counties<-map_data("county")
state_base<-ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) + coord_fixed(1.3) + geom_polygon(color="black", fill="gray") + ggtitle("Counties with high Life Expectancy (79 or above) in each State")
p<-state_base + geom_polygon(data=counties, fill=NA, color="gray") + geom_polygon(color="black", fill=NA)
life<-read.csv("lifeexp.csv")
colnames(life)<-c("State","Expectancy")
life$Expectancy<-as.numeric(as.character(life$Expectancy))
life$State<-tolower(life$State)
names(counties)[names(counties)=="region"]<-"State"
countylife<-merge(counties, life, by="State")
biglife<-subset(countylife, countylife$Expectancy >= 79)
bigl<-p + geom_point(inherit.aes=F, aes(x=long, y=lat), color="blue", size=0.5, data=biglife)
bigl
