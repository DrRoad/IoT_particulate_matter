## This code plots tons of PM2.5 produced in Baltimore City for the years 1999, 
## 2002, 2005 and 2008. It then fits a linear model to show the trend.

## If needed, read data
if(!exists("NEI")){
     NEI <- readRDS("./data/summarySCC_PM25.rds")
     SCC <- readRDS("./data/Source_Classification_Code.rds")     
}
## Calc total Baltimore emissions by year
baltEm<-with(subset(NEI,fips=="24510"),
             tapply(Emissions,year,sum,na.rm=T))

## Format years
years<-c(1999,2002,2005,2008)

## Draw graph
png(file="plot2.png")
par(mfrow=c(1,1),mar=c(5,5,4,2))
plot(years,as.numeric(baltEm),pch=20,xlim=c(1999,2008),
     xlab="Year",ylab=expression("Tons of "*PM[2.5]),
     main="Emissions Change: Baltimore City: All Sources")

## Connect dots
segments(c(1999,2002,2005),baltEm[1:3],
         c(2002,2005,2008),baltEm[2:4])

## Fit a line
model<-lm(as.numeric(baltEm)~years)
abline(model,lwd=1,col="red")
dev.off()