## This code plots motor vehicle emissions for Baltimore City from 1999-2008.

## If needed, read data
if(!exists("NEI")){
     NEI <- readRDS("./data/summarySCC_PM25.rds")
     SCC <- readRDS("./data/Source_Classification_Code.rds")     
}
## Subset emissions records for Baltimore City and Los Angeles
baltEm<-subset(NEI,(fips=="24510"),rm.na=T)

## Find motor vehicle emission sources
sourceData<-SCC[intersect(grep("Mobile",SCC[,4]),
                          grep("Vehicles",SCC[,4])),]
sources<-as.character(unique(sourceData$SCC),rm.na=T)

## Calc emissions
baltEm<-subset(baltEm,SCC %in% sources)
baltEm<-with(baltEm,tapply(Emissions,year,sum,na.rm=T))

## Format years
years<-c(1999,2002,2005,2008)

png(file="plot5.png")
par(mfrow=c(1,1))
plot(years,as.numeric(baltEm),pch=20,xlim=c(1999,2008),
     xlab="Year",ylab=expression("Tons of "*PM[2.5]),
     col="black",lwd=2,
     main="Vehicle Emissions: Baltimore")

segments(c(1999,2002,2005),baltEm[1:3],
         c(2002,2005,2008),baltEm[2:4])

model<-lm(as.numeric(baltEm)~years)
abline(model,lwd=1,col="red")

dev.off()