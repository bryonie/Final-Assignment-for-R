# title: "Final Project"
# author: 'u-number'
# date: "1/4/2020"

library(dplyr)
library(tidyverse) 
library(plyr)
library(gridExtra) #for plotting
library(ggrepel) #For plotting
library(naniar) # for missing values


# Introduction

# Over the past few decades consoles for playing video games have become commonplace 
# in houses across the country. Software and hardware advancements of the personal 
# computer (PC) have led to many different machines being developed. Every year, the 
# video game industry churns out hundreds of titles and sells hundreds of millions of 
# units around the globe. Many consoles have come and gone, but a few modern machines 
# have carved their niche in society and continue to evolve and thrive. Xbox (developed 
# by Microsoft), Playstation (developed by Sony), Nintendo, and the personal computer 
# are examples of the platforms that have the biggest hit games from a variety of 
# publishers.

# In this report, the following questions of interest are addressed:

# • Is there any evidence of differences between sales in different regions of the 
# world?

# • Are there any associations when comparing the genre of a game and the platform on 
# which it is released?

# • Are there any associations in sales or titles when we consider more current popular 
# platforms?

# • Do critic ratings correlate with sales? Do critics tend to favor some 
# publishers/genres/platforms over others?


summary(cars)


# Data exploration and cleaning
gameStats <- read.csv ("./gamesales.csv", stringsAsFactors = FALSE)

##Summary of data
str(gameStats)
summary(gameStats)
gameStats %>% glimpse()

##table showing rating
table(gameStats$Rating)

#Rename the abbreviated ratings to their full name
gameStats$Rating[gameStats$Rating=="RP"]="RatingPending"
gameStats$Rating[gameStats$Rating=="EC"]="EarlyChildhood"
gameStats$Rating[gameStats$Rating=="E"]="Everyone"
gameStats$Rating[gameStats$Rating=="E10+"]="Everyone10+"
gameStats$Rating[gameStats$Rating=="T"]="Teen"
gameStats$Rating[gameStats$Rating=="M"]="Mature"
gameStats$Rating[gameStats$Rating=="AO"]="AdultOnly"
gameStats$Rating[gameStats$Rating=="K-A"]="KidsToAdults"

# table with year of release for each game 
table(gameStats$Year_of_Release)

# check for missing values 
gg_miss_var(gameStats)
# There seems to be many missing values and from the descrption of the data set these m
# For the missing values, I think I will leave them as is. Replacing or removing that 
# many observations might not give the most meaningful results 

#check for duplication 
duplicated(gameStats) %>%
    sum()


# What are the top selling games by region and their platform & genre?

# For this plot the Top Games are considered the ones with more than 20 million sales
ggplot(gameStats[gameStats$Global_Sales>20,], aes(x=reorder(Name,Global_Sales), y=Global_Sales, 
                                            fill=Genre)) +
    geom_bar(stat="identity")+
    theme(legend.position="top", 
    )+ #axis.text.x=element_text(angle = 50, vjust=1)
    labs(title="Top Selling Games Globally", x= "Game and Platform", 
         y= "Global Sales in Millions")+
    geom_text(aes(label=Platform), hjust= 1.1,color="black", size=2) +
    coord_flip()


### Pie Char (Fail)
ggplot(gameStats[gameStats$Global_Sales>20,], aes(x="", y=Global_Sales, 
fill=reorder(Name,Global_Sales))) + 
geom_bar(stat="identity", width=1) + 
coord_polar("y", start=0) 
# + geom_text(aes(label = paste0(Genre)), position = position_stack(vjust = 0.5))

###############################################################################
###############################################################################
######################## Final Plot Global Sales ##############################
###############################################################################
###############################################################################
#Bar Plot
ggplot(gameStats[gameStats$Global_Sales>20,], 
aes(x=reorder(Name,Platform, na.last=NA), y=Platform)) +
  geom_point(aes(col=factor(Genre)), size=3) +
  theme(legend.position="top") +
  labs(title="Top Selling Games Globally", 
  x= "Game and Global Sales (in millions)", 
  y= "Console Platform",
  color="Genre") +
  geom_text(aes(label=paste0(Global_Sales)), 
  hjust= 1.1,color="black", size=2) +
  coord_flip()
###############################################################################
###############################################################################



###############################################################################
###############################################################################
########################### Final Plots Top 8 #################################
###############################################################################
###############################################################################

# Japan Sales
JP = ggplot(gameStats[gameStats$JP_Sales>5.33,], 
aes(x=reorder(Name,Platform, na.last=NA), y=Platform)) +
    #  geom_bar(stat="identity")+
    geom_point(aes(col=factor(Genre)), size=3) +
    labs( x= "Game and Sales (M)", 
    y= "Japan Sales in Millions",
    color="Genre")+
    geom_text(aes(label=paste0(JP_Sales)), 
    hjust=1.3, color="black", size=3)+
    coord_flip()

# North America Sales
N_A = ggplot(gameStats[gameStats$NA_Sales>14,], 
aes(x=reorder(Name,Platform, na.last=NA), y=Platform)) +
    #  geom_bar(stat="identity")+
    geom_point(aes(col=factor(Genre)), size=3) +
    labs( x= "Game and Sales (M)", 
    y= "North America Sales in Millions",
    color="Genre")+
    geom_text(aes(label=paste0(NA_Sales)), 
    hjust=1.3, color="black", size=3)+
    coord_flip()

# Europe Sales
EU = ggplot(gameStats[gameStats$EU_Sales>9.0,], 
aes(x=reorder(Name,Platform, na.last=NA), y=Platform)) +
    #  geom_bar(stat="identity")+
    geom_point(aes(col=factor(Genre)), size=3) +
    labs( x= "Game and Sales (M)", 
    y= "Europe Sales in Millions",
    color="Genre")+
    geom_text(aes(label=paste0(EU_Sales)), 
    hjust=1.3, color="black", size=3)+
    coord_flip()

# Other Sales
OS = ggplot(gameStats[gameStats$Other_Sales>2.85,], 
aes(x=reorder(Name,Platform, na.last=NA), y=Platform)) +
    #  geom_bar(stat="identity")+
    geom_point(aes(col=factor(Genre)), size=3) +
    labs( x= "Game and Sales (M)", 
    y= "Other Region Sales in Millions",
    color="Genre")+
    geom_text(aes(label=paste0(Other_Sales)), 
    hjust=1.3, color="black", size=3)+
    coord_flip()

N_A

grid.arrange(JP, N_A, EU, OS, nrow=4)
###############################################################################
###############################################################################


# What are the top 8 games per region?

p1=ggplot(gameStats[gameStats$JP_Sales>5.33,], aes(x=reorder(Name,JP_Sales), y=JP_Sales, 
                fill=Platform))+
     geom_bar(stat="identity")+
    labs( x= "Game and Genre", y= "Japan Sales in Millions")+
    geom_text(aes(label=Genre), hjust=1.1, color="white", size=3)+
    coord_flip()

p2=ggplot(gameStats[gameStats$NA_Sales>14,], aes(x=reorder(Name,NA_Sales), y=NA_Sales, 
                                            fill=Platform)) +
    geom_bar(stat="identity")+
    labs(x= "Game and Genre", y= "North America Sales in Millions")+
    geom_text(aes(label=Genre), hjust=1.1,color="white", size=3)+
    coord_flip()

p2

p3=ggplot(gameStats[gameStats$EU_Sales>9.0,], aes(x=reorder(Name,EU_Sales), y=EU_Sales, 
                                            fill=Platform)) +
    geom_bar(stat="identity")+
    labs(x= "Game and Genre", y= "Europe Sales in Millions")+
    geom_text(aes(label=Genre), hjust=1.1,color="white", size=3)+
    coord_flip()

p4=ggplot(gameStats[gameStats$Other_Sales>2.85,], aes(x=reorder(Name,Other_Sales), y=Other_Sales, 
                                            fill=Platform)) +
    geom_bar(stat="identity")+
    labs(x= "Game and Genre", y= "Sales of the Other Regions in Millions")+
    geom_text(aes(label=Genre), hjust=1.1,color="white", size=3)+
    coord_flip()

grid.arrange(p1,p2,p3,p4, nrow=4)

# What is the total number of games released every year?¶

ggplot(gameStats[gameStats$Year_of_Release!="N/A",], aes(x=Year_of_Release, fill=..count..)) +
    geom_bar()+
    scale_color_gradient(low="purple", high="orange")+
    scale_fill_gradient(low="purple", high="orange")+
    labs(title="Number of Games Released every Year", x= "Game", 
         y= "Total Number of Games")+
    theme( axis.text.x=element_text(angle = 90, vjust=-0.1)) +
    geom_text(stat='count',aes(label=..count..), angle=90, hjust=-0.05,color="black", size=2.5)
    # coord_flip()

# How has the sales of games changed throughout the years?

yearly=gameStats %>%
    group_by(Year_of_Release) %>%
    dplyr::summarize(numrelease=n(),Japan=sum(JP_Sales), NorthAmerica=sum(NA_Sales),
                     Europe=sum(EU_Sales),Others=sum(Other_Sales), 
                    Worldwide=sum(Global_Sales)) 

yearly$Year_of_Release = as.numeric((yearly$Year_of_Release))
yearly=yearly[!(is.na(yearly$Year_of_Release)),]

# There are only 3 observations in the year 2017 so I will remove them
yearly=subset(yearly, yearly$Year_of_Release!=2017)


years = yearly %>% 
  gather(Region, Value, -c(Year_of_Release, numrelease)) %>% 
  arrange(Region)

ggplot(years, aes(x=Year_of_Release, y=Value, color=Region))+
    geom_point()+
    geom_line()+
    theme(legend.position="top")+
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous("Year",labels = as.character(years$Year_of_Release), 
                       breaks=years$Year_of_Release)


# What are the highest global selling games per decade?
# To answer this, I will first need to create a new variable called Decade which 
# will have 4 values, and assign each game based on their year of release to their 
# appropriate decade. Then I'll display the results in a circular bar chart

Decade <- c("2010", "2000", "1990", "1980")

topglobal= gameStats %>%
    arrange(desc(Global_Sales)) %>%
    group_by(Decade) %>%
    slice(1:5)%>%
    filter(!is.na(Decade)) #removed data with missing decade (since year of release was N/A)
topglobal$NamePlat=paste(topglobal$Name, topglobal$Platform, sep=" -")
topglobal=topglobal %>%
    arrange(match(Decade, c("2010s","2000s", "90s","80s")), 
            desc(Global_Sales))
topglobal$id=as.numeric(seq(1,20)) #id for the display of each value on the circular barchart
topglobal$Decade=factor(topglobal$Decade,
                        levels=c("2010s","2000s", "90s","80s"))

gameStats$Decade[gameStats$Year_of_Release>=1980 & gameStats$Year_of_Release<=1989]="80s"
gameStats$Decade[gameStats$Year_of_Release>=1990 & gameStats$Year_of_Release<=1999]="90s"
gameStats$Decade[gameStats$Year_of_Release>=2000 & gameStats$Year_of_Release<=2009]="2000s"
gameStats$Decade[gameStats$Year_of_Release>=2010 & gameStats$Year_of_Release<=2019]="2010s"

# Which publishers have the highest game sales?

pub = gameStats %>%
    group_by(Publisher) %>%
    dplyr::summarize(total=n(), salestotal=round(sum(Global_Sales),1), 
                    salesjp=round(sum(JP_Sales),1),
                    salesna=round(sum(NA_Sales),1),
                    saleseu=round(sum(EU_Sales),1),
                    other=round(sum(Other_Sales),1)) 

# In this plot the highest game sales are considered the ones with more than 
# 100 million sales
ggplot(pub[pub$salestotal>100,], aes(x=reorder(Publisher,salestotal), y=salestotal,
fill=salestotal)) +
    # geom_point(aes(color=salestotal), shape=19, size=3)+
    geom_bar(stat="identity") +
    scale_fill_gradient2(low="moccasin", high="orange")+
    # geom_segment(aes(x=Publisher, xend=Publisher, y=0,yend=salestotal, color=salestotal), 
                #  size=12) + 
    theme(legend.position="top")+
    labs(title="Publishers with Top Selling Games Worldwide", x= "Publisher", 
         y= "Total Global Sales in Millions")+
    theme_bw()+
    geom_text(aes(label=salestotal),size=3,data=pub[pub$salestotal>100,],
                    color="black")+ 
    coord_flip()




pt1 =ggplot(pub[pub$salesjp>50,], aes(x=reorder(Publisher,salesjp), y=salesjp
    , fill=salesjp)) +
    geom_bar(stat="identity") +
    # geom_point(aes(color=salesjp), shape=19, size=3)+
    # geom_segment(aes(x=Publisher, xend=Publisher,y=0,yend=salesjp,color=salesjp), size=1)+ 
    labs( x= "Publisher", y= "Total Japan Sales in Millions")+
    theme_bw()+
    scale_fill_gradient2(low="moccasin", high="orange")+
    coord_flip()

pt2 = ggplot(pub[pub$salesna>110,], aes(x=reorder(Publisher,salesna), y=salesna
    , fill=salesna)) +
    geom_bar(stat="identity") +
    # geom_point(aes(color=salesna), shape=19, size=3)+
    # geom_segment(aes(x=Publisher, xend=Publisher,y=0,yend=salesna,color=salesna), size=1)+ 
    labs( x= "Publisher", y= "Total North America Sales in Millions")+
    theme_bw()+
    scale_fill_gradient2(low="moccasin", high="orange")+
    coord_flip()

pt3 = ggplot(pub[pub$saleseu>80,], aes(x=reorder(Publisher,saleseu), y=saleseu
    , fill=saleseu)) +
    geom_bar(stat="identity") +
    # geom_point(aes(color=saleseu), shape=19, size=3)+
    # geom_segment(aes(x=Publisher, xend=Publisher,y=0,yend=saleseu,color=saleseu), size=1)+ 
    labs( x= "Publisher", y= "Total Europe Sales in Millions")+
    theme_bw()+
    scale_fill_gradient2(low="moccasin", high="orange")+
    coord_flip()

pt4 =  ggplot(pub[pub$other>40,], aes(x=reorder(Publisher,other), y=other
    , fill=other)) +
    geom_bar(stat="identity") +
    # geom_point(aes(color=other),shape=19, size=3)+
    # geom_segment(aes(x=Publisher, xend=Publisher,y=0,yend=other,color=other),
    #              size=1)+ 
    labs( x= "Publisher",y= "Total Sales in Other Regions in Millions")+
    theme_bw()+
    scale_fill_gradient2(low="moccasin", high="orange")+
    coord_flip()

grid.arrange(pt1,pt2,pt3,pt4, nrow=4)

# What are the most famous gaming genres released by the top publishers?
# Here what I want to find is what are the gaming genres that the top selling publishers 
# release. From the plot above,I will use the top 6 selling publishers worldwide which 
# are Nintendo, Electronic Arts, Activision, Sony Computer Entertainment , Ubisoft and 
# Take-Two Interactive.

gpub= gameStats %>%
    group_by (Publisher, Genre) %>%
    dplyr:: summarize(total=n(), sumsales=round(sum(Global_Sales),1)) %>%
    filter(Publisher %in% c("Nintendo", "Electronic Arts", "Activision", 
                      "Sony Computer Entertainment","Ubisoft", "Take-Two Interactive")) 

gpub$Publisher = factor(gpub$Publisher, 
                               levels=c("Nintendo", "Electronic Arts", "Activision", 
                      "Sony Computer Entertainment","Ubisoft", "Take-Two Interactive"))

ggplot( gpub, aes(x=Genre, y=total, fill=Genre))+
    geom_bar(stat="identity") +
    facet_wrap(~Publisher, ncol=2, scale="free") +
    theme(legend.position= "none")+
    labs(title="Total Number of Releases per Genre from the Top 6 Publishers",
         x= "Genre", 
         y= "Total Number of Releases")+
    theme(axis.text.x=element_text(angle=60, hjust=1, size=8))+
    geom_text(aes(label=total), vjust=0.5,color="black", size=2.25)

