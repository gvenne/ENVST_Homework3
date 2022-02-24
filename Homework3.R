#In class prompts
install.packages(c("dplyr", "ggplot2"))
install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(dplyr)

datCC <- read.csv("/cloud/project/activity03/climate-change.csv")
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

datCC$Date <- ymd(datCC$Day)
# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)
# make a vector of all levels
name.Ent <- levels(datCO2$Entity)

# new data frame for US
NorthernH <- datCC[datCC$Entity == "Northern Hemisphere",]
SouthernH <- datCC[datCC$Entity == "Southern Hemisphere",]

plot(datCC$Day, datCC$temperature_anomaly)

#Prompt 1
#ggplot
ggplot(data = datCC [datCC$Entity != "World",], # data for plot
       aes(x = Date, y= temperature_anomaly ,
           color= Entity))+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Date", y="temperature_anomaly")+ #axis labels
  theme_classic()

#plot
plot(NorthernH$Date, # x data
     NorthernH$temperature_anomaly, # y data
     type = "l", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Date", #y axis label
     xlab = "temperature_anomaly") # turn off y axis
points(SouthernH$Date, SouthernH$temperature_anomaly, 
       type = "l", 
       col= "tomato3")

#Prompt 2
#rename
colnames(datCO2)[4] <- "CO2"

# subset data for just
NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y=expression(paste("US fossil fuel emissions (tons CO"[2],")")))+ # make axis labels
  theme_classic()+
  scale_color_manual(values = c("#7FB3D555","#34495E55", "#E7B80055")) #specify colors using hex codes that include transparency

####################
#HOMEWORK
#Question1
#pick countries
# subset CO2 to meet conditons
CO2 <- datCO2[datCO2$Entity == "Sweden" |
                datCO2$Entity == "India" |
                datCO2$Entity == "Japan" , ]

#plot1
ggplot(data=CO2, aes(x=Year, y=CO2, fill=Entity))+ # data
  geom_area()+ # geometry
  ggtitle(expression(paste("CO"[2]," Emissions Over Time")))+
  labs(x="Year", y=expression(paste("Fossil fuel emissions (tons CO"[2],")")))+ # make axis labels
  xlim(c(1900, 2020))

#Question 2
ggplot(data = datCO2[datCO2$Entity == "World", ], # data for plot
       aes(x = Year, y=CO2, color=Entity ))+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  ggtitle(expression(paste("CO"[2]," Emissions Over Time")))+
  theme_classic()+
  labs(x="Year", y=expression(paste("Fossil fuel emissions (tons CO"[2],")")))+ # make axis labels
  scale_color_manual(values = c("#009E73"))

#plot CC
ggplot(data = datCC[datCC$Entity == "World", ], # data for plot
       aes(x = Date, y= temperature_anomaly ,
           color= Entity))+ # aes, x and y
  geom_line()+ # use lines to connect data points
  ggtitle("Global Temperatures Anamolies Over Time")+
  labs(x="Year", y="Temperature Anomaly")+ #axis labels
  theme_classic()+
  scale_color_manual(values = c("#D55E00"))

#Question 3
defo <- read.csv("/cloud/project/region-share-tropical-deforestation.csv")

forest <- ggplot(data = defo, # data for plot
  aes(x = Entity, y= Share.of.commodity.driven.deforestation, fill= Entity)) + # aes, x and y
  geom_bar(stat="identity")+
  geom_text(aes(label= round(Share.of.commodity.driven.deforestation, 1)), vjust=-0.3, size=3.5)+
  theme_minimal()+
  ggtitle("Countries with Highest Driven Deforestation from Commodities")+
  labs(x="Countries", y="Driven deforestation (percent)") #axis labels

#vertical bar plot
forest 

#horizontal bar plot
forest + coord_flip()
