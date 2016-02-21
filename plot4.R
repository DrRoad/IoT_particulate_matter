## This code plots Emissions from U.S. coal combustion sources for the years
## 1999 and 2008.

## If needed, read data
if(!exists("NEI")){
     NEI <- readRDS("./data/summarySCC_PM25.rds")
     SCC <- readRDS("./data/Source_Classification_Code.rds")     
}
## Find emissions sources from burning coal
sourceData<-SCC[intersect(grep("Comb",SCC[,4]),
                           grep("Coal",SCC[,4])),]

sources<-as.character(unique(sourceData$SCC),rm.na=T)

## Get corresponding emissions records
coalEm<-subset(NEI,SCC %in% sources)
## could also use coalEm<-NEI[NEI$SCC %in% sources,]

coalEm<-with(coalEm,tapply(Emissions,year,sum,na.rm=T))

## Format years
years<-c(1999,2002,2005,2008)

## Output graphs
png(file="plot4.png")
par(mfrow=c(1,1),mar=c(5,5,4,2))
plot(years,as.numeric(coalEm),pch=20,xlim=c(1999,2008),
     xlab="Year",ylab=expression("Tons of "*PM[2.5]),
     main="Emissions Change: U.S.: Coal Combustion Sources")

segments(c(1999,2002,2005),coalEm[1:3],
         c(2002,2005,2008),coalEm[2:4])

model<-lm(as.numeric(coalEm)~years)
abline(model,lwd=1,col="red")
dev.off()
