setwd("/Users/arjun/Documents/Data course/R case study 3 (Visualization)")
dfSales <- read.csv("SalesData.csv")
require (Hmisc)
require (dplyr)
require (ggplot2)
require (plotrix)


describe(dfSales) 
  

#Before starting visualization, I checked that there were no missing values & data was clean

#Q1
 
Datafrom2015 = dfSales[c("Region", "Sales2015")]
Datafrom2015 = Datafrom2015 %>% mutate(Year = "2015")
Datafrom2015 = Datafrom2015%>%rename("Sales" = "Sales2015")

Datafrom2016 = dfSales[c("Region", Sales = "Sales2016")]
Datafrom2016 = Datafrom2016 %>% mutate(Year = "2016")
Datafrom2016 = Datafrom2016%>%rename("Sales" = "Sales2016")

appendedsales = rbind(Datafrom2015,Datafrom2016)

q1data = appendedsales %>% group_by(Region,Year) %>%
summarise(TotalSales = sum(Sales))

options(scipen=5)

ggplot2::ggplot(data=q1data) + 
  aes(x=Region, y= TotalSales , fill=Year)+ 
  geom_bar(stat="identity", position = "dodge")+
  geom_text(aes(label =round(TotalSales,0) , vjust = 0.5))
            
 
#Q2

#require(plotrix)

pieframe = dfSales %>% dplyr::group_by(Region) %>%
  summarise(TotalSales16= sum(Sales2016 ))

percentlabs = pieframe$TotalSales16 / sum(pieframeTotalSales16)

pie(pieframe$TotalSales16, labels = pieframe$Region, main = "Pie Chart for 2016 Sales")  


pieframe = pieframe %>% mutate(PercentVal = round(TotalSales16*100/sum(TotalSales16),1))
pieframe$newest = paste ( pieframe$PercentVal,"% : ",pieframe$Region)

pie(pieframe$TotalSales16, labels = pieframe$newest, col=c("red", "green", "blue"),  main = "Pie Chart for 2016 Sales")  

pie3D(pieframe$TotalSales16, labels = pieframe$newest, explode = 0.1, main = "Pie Chart  for 2016 Sales ")


#Q3

tieredDatafrom2015 = dfSales[c("Region", "Sales2015", "Tier")]
tieredDatafrom2015 = tieredDatafrom2015 %>% mutate(Year = "2015")
tieredDatafrom2015 = tieredDatafrom2015%>%rename("Sales" = "Sales2015")

tieredDatafrom2016 = dfSales[c("Region", Sales = "Sales2016", "Tier")]
tieredDatafrom2016 = tieredDatafrom2016 %>% mutate(Year = "2016")
tieredDatafrom2016 = tieredDatafrom2016%>%rename("Sales" = "Sales2016")

tieredappendedsales = rbind(tieredDatafrom2015,tieredDatafrom2016)

q3data = tieredappendedsales %>% group_by(Region,Year,Tier) %>%
  summarise(TotalSales = sum(Sales))

ggplot2::ggplot(data=q3data) + aes(x=Tier, y=TotalSales, fill=Year) +
  geom_bar(stat="identity", position = "dodge") + facet_grid(.~Region)


#Q4

dfSalesEast = dfSales[which(dfSales$Region == "East"),]
stateDatafrom2015 = dfSalesEast[c(  "Sales2015", "State")]
stateDatafrom2015 = stateDatafrom2015 %>% mutate(Year = "2015")
stateDatafrom2015 = stateDatafrom2015%>%rename("Sales" = "Sales2015")

stateDatafrom2016 = dfSalesEast[c(  Sales = "Sales2016", "State")]
stateDatafrom2016 = stateDatafrom2016 %>% mutate(Year = "2016")
stateDatafrom2016 = stateDatafrom2016%>%rename("Sales" = "Sales2016")

yearlystatesales = rbind(stateDatafrom2015,stateDatafrom2016)

q4data = yearlystatesales %>% group_by(State,Year) %>%
  summarise(TotalSales = sum(Sales))

ggplot2::ggplot(data=q4data) + aes(x=State, y=TotalSales, fill=Year) +
  geom_bar(stat="identity", position = "dodge") + labs(caption = "In the East region, NY saw a decline from 2015 to 2016") +
  theme(plot.caption = element_text(hjust = 0))


#Q5


dfSalesHigh = dfSales[which(dfSales$Tier == "High"),]

HighDatafrom2015 = dfSalesHigh[c(  "Units2015", "Division")]
HighDatafrom2015 = HighDatafrom2015 %>% mutate(Year = "2015")
HighDatafrom2015 = HighDatafrom2015%>%rename("Units" = "Units2015")

HighDatafrom2016 = dfSalesHigh[c(  Sales = "Units2016", "Division")]
HighDatafrom2016 = HighDatafrom2016 %>% mutate(Year = "2016")
HighDatafrom2016 = HighDatafrom2016%>%rename("Units" = "Units2016")

yearlyHDSales = rbind(HighDatafrom2015,HighDatafrom2016)

q5data = yearlyHDSales %>% group_by(Division,Year) %>%
  summarise(TotalUnits = sum(Units))

ggplot2::ggplot(data=q5data) + aes(x=Division, y=TotalUnits, fill=Year) +
  geom_bar(stat="identity", position = "dodge") + 
  labs(caption = "There is no division that declined in 2016") +
  theme(plot.caption = element_text(hjust = 0))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#Q6

dfSales$Qtr <- ifelse(dfSales$Month %in% c("Jan", "Feb", "Mar") ,"Q1",  
                   ifelse   (dfSales$Month %in% c("Apr", "May", "Jun") ,"Q2",
                    ifelse  (dfSales$Month %in% c("Jul", "Aug", "Sep") ,"Q3", "Q4")
                       )
                    )

#Q7


QtrDatafrom2015 = dfSales [c(  "Sales2015",  "Qtr")]
QtrDatafrom2015 = QtrDatafrom2015 %>% mutate(Year = "2015")
QtrDatafrom2015 = QtrDatafrom2015%>%rename("Sales" = "Sales2015")

  
QtrDatafrom2016 = dfSales [c(  Sales = "Sales2016", "Qtr")]
QtrDatafrom2016 = QtrDatafrom2016 %>% mutate(Year = "2016")
QtrDatafrom2016 = QtrDatafrom2016%>%rename("Sales" = "Sales2016")
 
QuarterlySales = rbind(QtrDatafrom2015 , QtrDatafrom2016)

q7data = QuarterlySales %>% group_by(Qtr,Year) %>%
  summarise(TotalSales = sum(Sales))

ggplot2::ggplot(data=q7data) + aes(x=Qtr, y=TotalSales, fill=Year) +
  geom_bar(stat="identity", position = "dodge")  


#Q8

q8data  = dfSales [c("Qtr", "Tier","Sales2015")]
q8a <- q8data[q8data$Qtr=="Q1",]
q8a = q8a %>% group_by( Tier)%>% summarise(Total = sum(Sales2015))
pie(q8a$Total, labels = q8a$Tier, main = "Qtr-1")  

q8b <- q8data[q8data$Qtr=="Q2",]
q8b = q8b %>% group_by( Tier)%>% summarise(Total = sum(Sales2015))
pie(q8b$Total, labels = q8b$Tier, main = "Qtr-2") 

q8c <- q8data[q8data$Qtr=="Q3",]
q8c = q8c %>% group_by( Tier)%>% summarise(Total = sum(Sales2015))
pie(q8c$Total, labels = q8c$Tier, main = "Qtr-3") 

q8d <- q8data[q8data$Qtr=="Q3",]
q8d = q8d %>% group_by( Tier)%>% summarise(Total = sum(Sales2015))
pie(q8d$Total, labels = q8d$Tier, main = "Qtr-4") 
