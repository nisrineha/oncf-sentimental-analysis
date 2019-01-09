trip = read.csv("HubwayTrips.csv")
str(trip)
summary(trip)
#les biblio
library(lattice)
library(caret)
library(ggplot2)
#nomralisation
preobj= preProcess(trip[,-10 ], method= c("center", "scale"))
newtrip= predict(preobj,trip[,-10 ] )
summary(newtrip)
sd(newtrip$Duration)
#kmeans method
library(caTools)
set.seed(20)
kresults= kmeans(newtrip,4 )
str(kresults)
table(kresults$cluster)
kresults$centers#avg
kresults$betweenss/kresults$totss
index
plot(index, y=kresults$cluster[index])

wss=0
for (i in 1:20) {
  km.out= kmeans(newtrip, centers = i, nstart = 20, iter.max = 50)
  
   wss[i]= km.out$tot.withinss
  
}
plot(1:20, wss, type = "b", xlab = "number", ylab = "ff")

#market semengtation for title 




kresults_optimal= kmeans(newtrip, 15)
kresults_optimal
table(kresults_optimal$cluster)


kresults_optimal




