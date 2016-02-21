## This code plots a base line graph showing tons (in 000's) of PM2.5 produced 
## for the years 1999, 2002, 2005 and 2008. 

## If needed, read data
if(!exists("NEI")){
     NEI <- readRDS("./data/summarySCC_PM25.rds")
     SCC <- readRDS("./data/Source_Classification_Code.rds")     
}
## Calc total emissions
totEm<-with(NEI,tapply(Emissions,year,sum,na.rm=T))

## Format years
years<-c(1999,2002,2005,2008)

## Draw graph
png(file="plot1.png")
par(mar=c(5,5,4,2))
plot(years,as.numeric(totEm)/1000,pch=20,xlim=c(1998,2009),
     xlab="Year",ylab=expression("000's of "*PM[2.5]*" Tons"),
     main="Emissions Change: U.S.: All Sources")

## Connect dots
segments(c(1999,2002,2005),totEm[1:3]/1000,
         c(2002,2005,2008),totEm[2:4]/1000)

## Fit a line
model<-lm(as.numeric(totEm)~years)
abline(model$coefficients[1]/1000,model$coefficients[2]/1000,lwd=1,col="red")
dev.off()