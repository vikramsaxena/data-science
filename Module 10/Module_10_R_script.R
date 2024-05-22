a<-read.csv("penguin.csv")
year<-a$Year
png<-a$Penguins
plot(a$Year, a$Penguins, xlab="Year", ylab="Number of Penguins", main="Number of Penguins in each of the given Years")
yearSqu<-year*year
quadm<-lm(png~year+yearSqu)
s<-seq(0,3000,0.5)
pc<-predict(quadm, list(year=s, yearSqu=s^2))
lines(s, pc, col="red", lwd=4)

a<-read.csv("beerfroth.csv")
plot(a$Time, a$Foam, xlab="Time", ylab="Height of Beer Foam", main="Height of Beer Foam as Time passes")
em<-lm(log(a$Foam)~a$Time)
pd<-exp(predict(em, list(a$Time)))
lines(a$Time, pd, col="green", lwd=4)