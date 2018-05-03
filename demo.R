##if(!file.exists("E:/R/Reproducible Research/4/Assignment")){dir.create("E:/R/Reproducible Research/4/Assignment")}
##url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
##download.file(url, "E:/R/Reproducible Research/4/Assignment/Data_Assessment.csv.bz2",method="auto") 
##bunzip2("Data_Assessment.csv.bz2", "Data_Assessment.csv", remove = FALSE, skip = TRUE)

##stormdata$EVTYPE = factor(toupper(stormdata$EVTYPE))
##stormdata$EVTYPEGRP<-factor(stormdata$EVTYPEGRP)

```{r , echo=TRUE}
eventsum<-storm_type %>%
  group_by(evtypegrp)%>%
  summarize(damage=sum(DAMAGETOTAL), property=sum(PROPDMGTOTAL), crops=sum(CROPDMGTOTAL), fatallities=sum(FATALITIES), injuries=sum(INJURIES))
```

```{r , echo=TRUE}
plot1<-barplot(summary(stormdata$EVTYPE),col=rainbow(4))
```

stormdatasum<-stormdata %>%
  group_by(EVTYPEGRP)%>%
  summarize(fatallities=sum(FATALITIES), injuries=sum(INJURIES))

groupstorm <- group_by(stormdata, EVTYPEGRP)
stormdatasum<-summarize(groupstorm, FATALITIES= sum(FATALITIES, na.rm = TRUE ),INJURIES= sum(INJURIES, na.rm = TRUE ))


stormfatalities<-stormdatasum[order(stormdatasum$FATALITIES, decreasing = TRUE),]
with(stormdatasum,barplot(stormfatalities[1:10,2],legend.text=stormfatalities[1:10,1],col = rainbow(10)))
