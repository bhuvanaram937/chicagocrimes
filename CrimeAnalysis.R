## Pre-Processing



install.packages("highcharter",repos = "http://cran.us.r-project.org")
library(highcharter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
library(lubridate)
library(xts)
library(maps)
library(ggmap)
library(gridExtra)
library(lubridate)
library(tidyverse)
library(forcats)
library(dplyr)
library(hts)
library(forecast)
install.packages('tis',repos = "http://cran.us.r-project.org")
library(tis)
library(prophet)
install.packages("timetk",repos = "http://cran.us.r-project.org")
library(timetk)

datafile1<-read.csv("Data/Chicago_Crimes_2012_to_2017 2.csv",header=T, sep=",")
datafile2<-read.csv("Data/PoliceBeatDec2012.csv",header = T,sep = ",")
datafile3<-merge(x = datafile1, y = datafile2, by.x = "Beat",by.y = "BEAT_NUM",  all.x = TRUE)
datafile4<-read.csv("Data/IUCR codes.csv",header = T,sep = ",")
datafile5<-read.csv("Data/WARDS_2015.csv",header = T,sep = ",")
datafile6<-read.csv("Data/CommAreas.csv",header = T,sep = ",")
datafile7<-read.csv("Data/PoliceDistrictDec2012.csv",header = T,sep = ",")
datafile8<-merge(x = datafile3, y = datafile6[,c("AREA_NUM_1","COMMUNITY")], 
                 by.x = "Community.Area", by.y = "AREA_NUM_1",  all.x = TRUE)
datafile9<-merge(x = datafile8, y = datafile7[,c("DIST_LABEL","DIST_NUM")], 
                 by.x = "District", by.y = "DIST_NUM",  all.x = TRUE)
crime_database<-datafile9


# ADDING YEAR,HOUR and MONTH COLUMN FROM DATE

crime_database <- crime_database %>% filter(crime_database$Year != 2017)
crime_database$hour<-factor(hour(as.POSIXct(crime_database$Date,format="%m/%d/%Y %I:%M:%S %p")))

crime_database$day <- factor(day(as.POSIXlt(crime_database$Date, 
                                            format="%m/%d/%Y %I:%M:%S %p ")))
crime_database$month <- factor(month(as.POSIXlt(crime_database$Date, 
                                                format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))
crime_database$year <- factor(year(as.POSIXlt(crime_database$Date, 
                                              format="%m/%d/%Y %I:%M:%S %p")))
crime_database$weekday <- factor(wday(as.POSIXlt(crime_database$Date, 
                                                 format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))

crime_database$Date <- as.Date(crime_database$Date, "%m/%d/%Y %I:%M:%S %p")


# Analysis of yearly, monthly and hourly crime numbers in Chicago

## Total crimes over different years 

# This plot shows the total number of crimes over different year in Chicago.

crime_total<- crime_database %>%  
  group_by(year) %>% summarise(n=n()) 

ggplot(crime_total,aes(x=year,y=n)) + geom_bar(stat='identity') +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Year",y="Total number of crimes")+
  ggtitle("Trend of total crimes across 2012-16")


## Creating timeseries of crimes with ggplot

crimes_by_date<-crime_database %>% group_by(Date)  %>% 
  summarise(n=n())
ggplot(crimes_by_date, aes(x=Date,y= n)) + geom_point(na.rm = TRUE)+
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Year",y="Number of crimes")+
  ggtitle("Trend of Crimes in Chicago")



## Crimes by month of each year

crimes_month <- crime_database %>% group_by(year,month) %>% summarise(n=n()) %>% arrange(desc(n)) 

ggplot(crimes_month,aes(x=month,y=n,color=year)) + geom_point(size=3) + 
  geom_line(aes(group=1),linetype='dotted',size=1)+ 
  facet_wrap(~year,nrow=1) + theme(legend.position="none") + 
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(x="Month",y="Number of crimes ")+
  ggtitle("Crimes per month across years 2012-16")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=7),
        axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=20,face="bold"))


## Crime by different hours of the day in each year

crime_hour <- crime_database %>% group_by(year,month,hour) %>% summarise(n=n()) 

ggplot(crime_hour, aes(x=month, y=hour)) + geom_tile(aes(fill = n) ,colour = "white") + 
  facet_grid(~year) +
  scale_fill_gradient(low = "steelblue",high = "red",na.value="grey50") + 
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,0)) +
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Month",y="Number of crimes per hour")+
  ggtitle("Crimes by different hours of the day in each year")+
  guides(fill=guide_legend("Number of crimes by hour of the day"))


# Analysis of different types of crimes 

## Number of different crime types

top_crime_type <- crime_database %>% group_by(Primary.Type) %>%
  summarise(n()) %>% arrange(desc(`n()`)) %>% top_n(10,`n()`) 

crimes_total_color<-crime_database %>% group_by(Primary.Type)  %>% 
  filter(Primary.Type %in% c(as.vector(top_crime_type$Primary.Type))) %>%
  summarise(n=n())

ggplot(crimes_total_color,aes(x=fct_reorder(Primary.Type,n,.desc=TRUE),y=n,
                              fill=n))+
  geom_bar(stat="identity")+
  scale_y_discrete(limits=c(75000,150000,225000,300000,375000))+
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20,face="bold"))+
  ggtitle("Number of crimes by Crime Type")+
  labs(x="Crime Type",y="Number of crimes")+
  guides(fill = guide_legend(title = "Crime Type"))


## Crime type trend over the years 

# The trend of different crime types over different year and the number of crimes of each type over different years.




crime_type_trend<- crime_database %>% group_by(Primary.Type,year) %>% summarise(n=n()) %>% 
  filter(Primary.Type %in% c(as.vector(top_crime_type$Primary.Type)))

ggplot(crime_type_trend,aes(x=Primary.Type,y=n,color=year,group=year)) + geom_point(size=3) +
  geom_line(linetype='dotted')+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=10),
        axis.title=element_text(size=20,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  guides(fill = guide_legend(title = "CRIME TYPE"))+
  labs(x="Year",y="Number of crimes")+
  ggtitle("Trend of top 10 crimes across 2012-16")

ggplot(crime_type_trend,aes(x=year,y=n,fill=fct_reorder(Primary.Type,n,.desc=TRUE))) + 
  geom_bar(stat='identity',position = 'dodge') +
  theme(axis.text.x=element_text(angle=360,hjust=0.5),axis.text=element_text(size=20),
        axis.title=element_text(size=10,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  guides(fill = guide_legend(title = "CRIME TYPE"))+
  labs(x="Year",y="Number of crimes")+
  ggtitle("Trend of top 10 crimes across 2012-16")


## Monthly distribution of top 5 crimes between 2012 to 2016 

top_crime_type_5 <- crime_database %>% group_by(Primary.Type) %>%
  summarise(n()) %>% arrange(desc(`n()`)) %>% top_n(5,`n()`)

crimes_month_type <- crime_database %>% group_by(year,month,Primary.Type) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% 
  filter(Primary.Type %in% c(as.vector(top_crime_type_5$Primary.Type))) 

ggplot(crimes_month_type,aes(x=month,y=n,color=Primary.Type,group=Primary.Type))+geom_point(size=3) + 
  geom_line() + facet_grid(~year) +
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x="Month",y="Number of crimes ")+
  ggtitle("Crimes per month across years 2012-16")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=7),
        axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=20,face="bold"))



# Analysis of crimes across Chicago neighbourhoods

## Heatmap analysis of crimes in Chicago neighbourhoods


#install.packages("ggmap",repos = "http://cran.us.r-project.org")
library(ggmap)

chicago.map <- get_map(location= 'Chicago,Illinois', 
                       maptype='roadmap', color='bw',source='google',zoom=14)
crime_map <- crime_database %>% 
  filter(Primary.Type %in% top_crime_type$Primary.Type) %>%
  group_by(Longitude,Latitude,Primary.Type) %>% 
  summarise(n=n()) %>% arrange(desc(n)) 
crime_map<-na.omit(crime_map)


ggmap(chicago.map) + 
  geom_point(data=crime_map,aes(x=Longitude,y=Latitude,color=Primary.Type,size=as.numeric(n)),size=1,alpha=.7) +
  facet_wrap(~Primary.Type) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Crimes in Chicago")+
  guides(color=guide_legend("Crime Type"))

chicago.map <- get_map("Chicago,Illinois",zoom=11)

ggmap(chicago.map, extent = "device") +
  geom_density2d(data = crime_database, aes(x = Longitude, y = Latitude), size = 0.3) +
  stat_density2d(data = crime_database, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 5,
                 bins = 20, geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size=20,face="bold"))+
  ggtitle("Heatmap analysis of crimes in Chicago neighbourhoods")+
  labs(x="Longitude",y="Latitude")

top_crime_type_3 <- crime_database %>% group_by(Primary.Type) %>%
  summarise(n()) %>% arrange(desc(`n()`)) %>% top_n(3,`n()`) 

c_11thdis<-crime_database%>%filter(DIST_LABEL=="11TH") %>% 
  filter(Primary.Type %in% top_crime_type_3$Primary.Type) 

chicago.map1<-get_map("11th district,Chicago,Illinois",zoom=13)

ggmap(chicago.map1) + 
  geom_point(data=c_11thdis,
             aes(x=Longitude,y=Latitude,color=Primary.Type),
             size=2,alpha=.7)+
  facet_wrap(~year)+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Crimes across years 2012-16 in 11th District")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=14),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size=20,face="bold",hjust=0.5))+
  guides(color=guide_legend("Crime Type"))


## Top crime districts by year 

# The distribution of crimes in different districts from year 2012 to 2016.

top_crime_district<- crime_database %>% group_by(DIST_LABEL) %>% summarise(n=n()) %>% arrange(desc(n)) %>% top_n(15,n)

crime_district_trend<- crime_database %>% group_by(DIST_LABEL,year) %>% summarise(n=n()) %>% 
  filter(DIST_LABEL %in% as.vector(top_crime_district$DIST_LABEL)) %>%
  arrange(desc(n),.by_group=TRUE)

ggplot(crime_district_trend,aes(x=DIST_LABEL,y=n,color=year,group=year)) + geom_point(size=3) +
  geom_line(linetype='dotted',size=1) +
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),plot.title =element_text(size=20,face="bold",hjust=0.5) )+
  labs(x="District",y="Number of Crimes")+
  ggtitle("Number of crimes by District")



## Top 3 crimes in top 5 districts

# The trend of top crimes type in the districts most prone to crimes along different years.


top_crime_district<- crime_database %>% group_by(DIST_LABEL) %>% summarise(n=n()) %>% arrange(desc(n)) %>%
  top_n(5,n)

top_crime_type_3 <- crime_database %>% group_by(Primary.Type) %>%
  summarise(n()) %>% arrange(desc(`n()`)) %>% top_n(3,`n()`) 


total_district_crimes<- crime_database %>% group_by(year,DIST_LABEL,Primary.Type) %>% summarise(n=n()) %>%
  filter(DIST_LABEL %in% as.vector(top_crime_district$DIST_LABEL)) %>%
  filter(Primary.Type %in% c(as.vector(top_crime_type_3$Primary.Type)))


ggplot(total_district_crimes,aes(x=DIST_LABEL,y=n,group=1L,color=Primary.Type)) + 
  geom_point(size=1) + 
  geom_line(size=0.5) + 
  facet_grid(year~Primary.Type) + 
  theme(legend.position="none")+
  scale_y_discrete(breaks=c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000)) +
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"))+
  labs(x="District",y="Number of Crimes")+
  ggtitle("Number of crimes by District")


## Types of crimes in each district over different years


top_crime_type_5 <- crime_database %>% group_by(Primary.Type) %>%
  summarise(n()) %>% arrange(desc(`n()`)) %>% top_n(5,`n()`)

crime_type_district_trend<- crime_database %>% group_by(DIST_LABEL,year,Primary.Type) %>% 
  summarise(n=n()) %>% filter(Primary.Type %in% c(as.vector(top_crime_type_5$Primary.Type))) %>%
  filter(DIST_LABEL %in% as.vector(top_crime_district$DIST_LABEL)) %>% 
  arrange(desc(n),.by_group=TRUE)

ggplot(crime_type_district_trend,aes(x=DIST_LABEL,y=n,fill=Primary.Type)) + 
  geom_bar(stat='identity') +
  facet_wrap(~year,nrow=1) +
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size=20,face="bold",hjust=0.5))+
  ggtitle("Number of crimes per district")+
  labs(x='District',y='Number of crimes')+
  guides(fill = guide_legend(title = "CRIME TYPE"))


## Trend of each crime type over differnt years at 5 districts 


ggplot(crime_type_district_trend,aes(x=DIST_LABEL,y=n,group=Primary.Type,
                                     color=Primary.Type,linetype=Primary.Type,shape=Primary.Type)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  facet_wrap(~year,nrow=1) + 
  scale_x_discrete(limits= c(as.vector(top_crime_district$DIST_LABEL)))+
  labs(x='District',
       y='Number of Crimes')+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"))


## CRIMES BY COMMUNITY

crime_database %>%
  group_by(COMMUNITY) %>%
  summarize(n = n()) %>%
  top_n(10,n) %>%
  ggplot(aes(x=fct_reorder(COMMUNITY,n,.desc=TRUE),y=n,fill=n)) + 
  geom_bar(stat='identity') + 
  ylab('Number of crimes')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Community')+
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=7),
        axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  ggtitle("Communities most prone to crimes ")+
  guides(fill=guide_legend("Number of crimes"))

top_crime_community<-crime_database%>%group_by(COMMUNITY) %>%summarise(n=n()) %>%
  arrange(desc(n)) %>% top_n(10,n)
crimes_community<-crime_database%>% 
  filter(Primary.Type %in% c(as.vector(top_crime_type_5$Primary.Type))) %>%
  filter(COMMUNITY %in% c(as.vector(top_crime_community$COMMUNITY))) %>%
  group_by(COMMUNITY,Primary.Type) %>% summarise(n=n()) %>%
  arrange(desc(n)) %>% top_n(10,n)

ggplot(crimes_community,aes(x=COMMUNITY,y=n,group=Primary.Type,color=Primary.Type))+ geom_point() +
  geom_line(size=0.5,linetype='dotted') +
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=7),
        axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Community",y="Number of crimes by crime type") +
  ggtitle("Trend of Crime Types across different communities")+
  guides(color=guide_legend("Crime Type"))


## Crimes by location


crime_location<-crime_database %>% group_by(Location.Description) %>% summarise(n=n()) %>% arrange(desc(n)) %>%
  top_n(10,n)


ggplot(crime_location,aes(x=fct_reorder(Location.Description,n,.desc=TRUE),y=n,
                          fill=n))+
  geom_bar(stat="identity")+
  scale_y_discrete(limits=c(0,100000,200000,300000))+
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20,face="bold",hjust=0.5))+
  ggtitle("Number of crimes by location")+
  labs(x="Location",y="Number of crimes")+
  guides(fill = guide_legend(title = "Number of crimes"))+
  scale_fill_gradient(low="#FF8888",high="#FF0000")



# Analysis of crime arrests

## Total number of arrest between 2012 to 2016


arrest_total<- crime_database %>%  
  group_by(year) %>% filter(Arrest == 'True') %>% summarise(n=n()) 

ggplot(arrest_total,aes(x=year,y=n)) + geom_bar(stat='identity') +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Year",y="Total number of arrests")+
  ggtitle("Trend of total arrests between 2012-16")

## Time series plot of crimes and arrests



by_Date <- na.omit(crime_database) %>% group_by(Date) %>% summarise(Total = n())
tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date))

arrests_by_date<-na.omit(crime_database) %>% filter(Arrest == 'True') %>% group_by(Date) %>%
  summarise(n=n())

arrests_tseries <- xts(arrests_by_date$n, order.by=as.POSIXct(arrests_by_date$Date))

hchart(tseries, name = "Crimes") %>% 
  hc_add_series(arrests_tseries, name = "Arrests") %>%
  hc_title(text = "Times Series plot of Chicago Crimes and Arrests") %>%
  hc_legend(enabled = TRUE)



## Creating timeseries of arrests made with ggplot



ggplot(arrests_by_date,aes(Date,n))+geom_point(na.rm = TRUE)+
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Year",y="Number of Arrested crimes")+
  ggtitle("Trend of Arrested Crimes in Chicago")




## Heat map analysis of crime versus arrest


arrests_count <- crime_database %>% filter(Arrest=='True') %>%
  group_by(year, month) %>% summarise(Total = n())

arrests <- ggplot(arrests_count, aes(year, month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Arrests by Year and Month(2012-2016)")


crime_count <- crime_database %>% group_by(year, month) %>% summarise(Total = n())

crimes <- ggplot(crime_count, aes(year, month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Crimes by Year and Month(2012-2016)")


grid.arrange(crimes, arrests, ncol = 2)


## Number of crimes with and without arrests

arrested_crime_frequency<-crime_database %>% group_by(year,month,Arrest) %>%
  summarise(arrested=n())

ggplot(arrested_crime_frequency,aes(x=month,y=arrested,color=Arrest,group=Arrest))+
  facet_wrap(~year,nrow = 1)+
  geom_point()+
  geom_line(size=0.5)+
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Month",y="Number of arrests")+
  ggtitle("Crimes in Chicago")+
  guides(color=guide_legend("True/False"))

ggplot(arrested_crime_frequency,
       aes(x=year,y=arrested,fill=Arrest)) + geom_bar(position='dodge',stat='Identity') +
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Month",y="Number of arrests")+
  ggtitle("Crimes in Chicago")+
  guides(color=guide_legend("True/False")) 



## Number of arrests by crime type


arrested_color<-crime_database%>% group_by(Primary.Type) %>%
  filter(Primary.Type %in% c(as.vector(top_crime_type$Primary.Type))) %>% 
  filter(Arrest=="True") %>% summarise(n=n())

ggplot(arrested_color,aes(x=fct_reorder(Primary.Type,n,.desc=TRUE),y=n,
                          fill=n))+
  geom_bar(stat="identity")+
  scale_y_discrete(limits=c(20000,40000,60000,80000,100000,120000))+
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20,face="bold",hjust=0.5))+
  ggtitle("Number of arrests crimes by crime Type")+
  labs(x="Primary Type",y="Number of arrests")+
  guides(fill = guide_legend(title = "Crime Type"))


## Analysis on domestic crimes

domestic_crime_frequency<-crime_database %>% group_by(year,Domestic) %>%
  summarise(domestic=n())


ggplot(domestic_crime_frequency,
       aes(x=year,y=domestic,fill=Domestic)) + geom_bar(position='dodge',stat='Identity') +
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),plot.title = element_text(size=20,face="bold",hjust=0.5))+
  labs(x="Month",y="Number of Domestic crimes")+
  ggtitle("Domestic crimes in Chicago")+
  guides(color=guide_legend("True/False"))




# Forecasting the number of future crimes in Chicago


## Time series Analysis to forecast

#Forecast seasonal arima model

crime_day<-crime_database %>% group_by(Date) %>% summarise(Day_total = n())

date<-as.Date(crime_day$Date)
total_crimes<-as.numeric(crime_day$Day_total)

# using xts

myxts<-xts(total_crimes,date)


d1.arima<-auto.arima(ts(myxts,frequency=365),D=1)
d1.forecast<-forecast(d1.arima,h=365)
plot(d1.forecast)

idx <- tk_index(myxts)
idx_future1<-tk_make_future_timeseries(idx, n_future = 365)

myts_future1 <- cbind(y = d1.forecast$mean, y.lo.80 = d1.forecast$lower[,1],
                      y.hi.80 = d1.forecast$upper[,1],y.lo.95=d1.forecast$lower[,2],
                      y.hi.95 = d1.forecast$upper[,2])
myxts_future1 <- xts(myts_future1, idx_future1)
myxts_future1

myxts_reformatted1 <- cbind(y = myxts, y.lo.80 = NA, y.hi.80 = NA,y.lo.95 = NA, y.hi.95 = NA)
myxts_final1 <- rbind(myxts_reformatted1, myxts_future1)
plot(myxts_final1)


tbl_myxts_final1<-tk_tbl(myxts_final1)

ggplot(tbl_myxts_final1, aes(index, y)) + geom_line() +
  scale_x_date(date_labels  = "%b %Y",date_breaks = "1 month") + xlab("Month") + ylab("Number of crimes")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_ribbon(aes(ymin = y.lo.80, ymax = y.hi.80), alpha = 0.2)+
  geom_ribbon(aes(ymin = y.lo.95, ymax = y.hi.95), alpha = 0.2)+
  guides(linetype = guide_legend(title = "Number of crimes"))+
  ggtitle("Trend of crimes across years 2012-16 with 2017 forecast")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20,face="bold",hjust=0.5))+
  guides(color=guide_legend(title = "Confidence Intervals"))


## Validation of forecast with 2017 data


datafile1_17<-read.csv("Data/Crimes_-_2017.csv",header=T, sep=",")
datafile2_17<-read.csv("Data/PoliceBeatDec2012.csv",header = T,sep = ",")
datafile3_17<-merge(x = datafile1_17, y = datafile2_17, by.x = "Beat",by.y = "BEAT_NUM",  all.x = TRUE)
datafile4_17<-read.csv("Data/IUCR codes.csv",header = T,sep = ",")
datafile5_17<-read.csv("Data/WARDS_2015.csv",header = T,sep = ",")
datafile6_17<-read.csv("Data/CommAreas.csv",header = T,sep = ",")
datafile7_17<-read.csv("Data/PoliceDistrictDec2012.csv",header = T,sep = ",")
datafile8_17<-merge(x = datafile3_17, y = datafile6_17[,c("AREA_NUM_1","COMMUNITY")], 
                    by.x = "Community.Area", by.y = "AREA_NUM_1",  all.x = TRUE)
datafile9_17<-merge(x = datafile8_17, y = datafile7_17[,c("DIST_LABEL","DIST_NUM")], 
                    by.x = "District", by.y = "DIST_NUM",  all.x = TRUE)
crime_database_17<-datafile9_17

# ADDING YEAR COLUMN FROM DATE
#crime_database <- crime_database %>% mutate(year=year(mdy_hm(Date)),hour=hour(mdy_hm(Date)),
#                                            month=month.abb[month(mdy_hm(Date))]) 

crime_database_17$hour<-factor(hour(as.POSIXct(crime_database_17$Date,format="%m/%d/%Y %I:%M:%S %p ")))

crime_database_17$day <- factor(day(as.POSIXlt(crime_database_17$Date, 
                                               format="%m/%d/%Y %I:%M:%S %p ")))
crime_database_17$month <- factor(month(as.POSIXlt(crime_database_17$Date, 
                                                   format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))
crime_database_17$year <- factor(year(as.POSIXlt(crime_database_17$Date, 
                                                 format="%m/%d/%Y %I:%M:%S %p")))
crime_database_17$weekday <- factor(wday(as.POSIXlt(crime_database_17$Date, 
                                                    format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))

crime_database_17$Date <- as.Date(crime_database_17$Date, "%m/%d/%Y %I:%M:%S %p")

crime_17_day <- crime_database_17 %>% group_by(Date) %>% arrange(Date) %>% summarise(n=n())
head(crime_17_day,100)
date_17<-as.Date(crime_17_day$Date)
crimes_17<-as.numeric(crime_17_day$n)
myxts_17<-xts(crimes_17,date_17)

myxts_17<-tk_tbl(myxts_17)



tmp <- tbl_myxts_final1 %>% filter (index >= "2017-01-01" & index <= "2017-11-25")
tmp$actual<-myxts_17$value


ggplot(tmp, aes(index, y)) + geom_line() + geom_line(aes(y=actual),color='red')+
  scale_x_date(date_labels  = "%b %Y",date_breaks = "1 month") + xlab("Month") + ylab("Number of crimes")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_ribbon(aes(ymin = y.lo.80, ymax = y.hi.80), alpha = 0.2)+
  geom_ribbon(aes(ymin = y.lo.95, ymax = y.hi.95), alpha = 0.2)+
  guides(linetype = guide_legend(title = "Number of crimes"))+
  ggtitle("Forecasted Vs Actual number of Crimes")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20,face="bold",hjust=0.5))

ggplot(tmp,aes(x=index,y=y)) + geom_line() + geom_line(aes(y=actual),color='red')+
  labs(x="Month",y="Number of crimes")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=8),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20,face="bold",hjust=0.5))+
  ggtitle("Actual VS Forecasted")


tmp_1<-filter(tmp,index != "2017-01-01")


## Using prophet package


crime_day<-crime_database %>% group_by(Date) %>% summarise(Day_total = n())
crime_day <- crime_day %>% mutate(Day_total=log(Day_total))
names(crime_day)<-c("ds","y")
crime_day$ds<-factor(crime_day$ds)

my_df<-prophet(crime_day,daily.seasonality = TRUE)

future <- make_future_dataframe(my_df, periods = 365 * 4)

forecast <- predict(my_df, future)

plot(my_df, forecast)

prophet_plot_components(my_df, forecast)

