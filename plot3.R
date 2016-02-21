## This code plots the log tons of PM2.5 produced in Baltimore City for each of
## four Emission "types". It then fits a linear model to the results in each plot.
## Finally, it prouces a table to validate results. 

## If needed, read data
if(!exists("NEI")){
     NEI <- readRDS("./data/summarySCC_PM25.rds")
     SCC <- readRDS("./data/Source_Classification_Code.rds")     
}
baltEm<-subset(NEI,fips=="24510")

# setting zeros to nominal value to ensure we can log below
baltEm$Emissions[baltEm$Emissions==0]=0.01

## Begin ggplot() graph. Note: used log of emissions. 
library(ggplot2)
g<-ggplot(baltEm,aes(year,log(Emissions)))

## Output graph
png(file="plot3.png")
g+geom_point()+facet_grid(.~type)+geom_smooth(method="lm")+
     labs(title="Emissions Change: Baltimore City: By Type",
          y=expression("log("*PM[2.5]*") in Tons"),
          x="Year")
dev.off()

## Validate results by calculating means for comparison.
baltEm<-split(baltEm,baltEm$type)
as.data.frame(lapply(baltEm,function(x) 
     tapply(x$Emissions,x$year,mean,na.rm=T)))