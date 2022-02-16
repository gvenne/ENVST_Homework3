# read in data
# cloud is always lowercase
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

# check column names
colnames(datCO2)

# change the 4 column name
colnames(datCO2)[4] <- "CO2"
# check names again
colnames(datCO2)

# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)
# make a vector of all levels
name.Ent <- levels(datCO2$Entity)

#run for names
name.Ent

#plot
plot(datCO2$Year, datCO2$CO2)

# new data frame for US
US <- datCO2[datCO2$Entity == "United States",]
# new data frame for Mexico
ME <- datCO2[datCO2$Entity == "Mexico",]

# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (tons CO2)", #y axis label
     xlab = "Year") #x axis label

# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )

# make a plot of US CO2 ----

plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")

# make a plot of US CO2 ----

plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n", # turn off y axis
     ylim = c(0,6200000000)) # change y axis range
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")
legend("topleft",
       c("United States", "Mexico"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")

install.packages("ggplot2")
library(ggplot2)

ggplot(data = US, # data for plot
       aes(x = Year, y=CO2 ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)") # make axis labels

ggplot(data = US, # data for plot
       aes(x = Year, y=CO2 ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ #axis labels
  theme_classic()

# subset data for just
NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()

# subset data for just
NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()+
  scale_color_manual(values = c("#7FB3D555","#34495E55", "#E7B80055")) #specify colors using hex codes that include transparency

# subset CO2 to meet conditons
compCO2 <- datCO2[datCO2$Year >= 1950 & datCO2$Entity == "France" |
                    datCO2$Year >= 1950 & datCO2$Entity == "India" |
                    datCO2$Year >= 1950 &  datCO2$Entity == "Russia" , ]

ggplot(data = compCO2 , aes(x=Entity, y=CO2))+ # look at CO2 by country
  geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
  geom_boxplot(width=0.03,size=0.15, fill="grey90")+ # add grey 
  #boxplots and make them smaller to fit in the violin (width)
  #than normal with thinner lines (size_ than normal
  theme_classic()+ # get rid of ugly gridlines
  labs(x = "Country", y="Annual emissions (tons CO2)")

ggplot(data=compCO2, aes(x=Year, y=CO2, fill=Entity))+ # data
  geom_area() # geometry

ggplot(data=compCO2,
       aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+ #fill works for polygons/shaded areas 
  geom_ribbon(alpha=0.5 )+ #fill in with 50% transparency
  labs(x="Year", y="Annual emissions (tons CO2)")

b <- ggplot(data=compCO2,aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+
  geom_ribbon(alpha=0.5 )+
  labs(x="Year", y="Carbon emissions (tons CO2)") +
  theme_classic()

b + annotate("segment", # line label
             x=1991, # start x coordinate
             y=2450000000, # start y coordinate
             xend=1991, # end x coordinate
             yend=2600000000) + # end y coordinate
  annotate("text", # add text label
           x=1991, # center of label x coordinate
           y= 2700000000, # center of label y coordinate
           label="end of USSR") # label to add
