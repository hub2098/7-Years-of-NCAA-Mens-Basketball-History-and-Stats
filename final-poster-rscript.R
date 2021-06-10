##### Ken Hubbard
################# Final Poster Project
######################################### NCAA Basketball Data

setwd("~/School/School Courses/2021/IST 719/Final")

cbb <- read.csv("cbb.csv"
                , header= TRUE
                , stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggforce)
library(treemapify)

###########################################

####################### Viz 1+2 - Pie Chart ####################

# Percentage showing where teams fall post-season
# Percentages excluding DNF
sum(cbb$POSTSEASON == "2ND", na.rm=TRUE) # 7 
0.21008403*7 # 1.47%
sum(cbb$POSTSEASON == "Champions", na.rm=TRUE) # 7
0.21008403*7 # 1.47%
sum(cbb$POSTSEASON == "E8", na.rm=TRUE) # 28
0.21008403*28 # 5.88%
sum(cbb$POSTSEASON == "F4", na.rm=TRUE) # 14
0.21008403*14 # 2.94%
nrow(cbb) # 2455
sum(cbb$POSTSEASON == "DNF", na.rm=TRUE) # 2455 - 476 = 1979
sum(cbb$POSTSEASON == "R32", na.rm=TRUE) # 112
0.21008403*112 # 23.53%
sum(cbb$POSTSEASON == "R64", na.rm=TRUE) # 224
0.21008403*224 # 47.06%
sum(cbb$POSTSEASON == "R68", na.rm=TRUE) # 28
0.21008403*28 # 5.88%
sum(cbb$POSTSEASON == "S16", na.rm=TRUE) # 56
0.21008403*56 # 11.76%

########################### FIRST PLOT ######################
## Graphing it
Placement <- c("2ND", "Champions", "E8", "F4", "R32"
               , "R64", "R68", "S16")
Percent <- c(1.47, 1.47, 5.88, 2.94, 23.53, 47.06, 5.88, 11.76)
Percentages <- c("1.47%", "1.47%", "5.88%", "2.94%", 
                 "23.53%", "47.06%", "5.88%", "11.76%")
piedata <- data.frame(Placement,Percent)
piedataNEW <- piedata %>% 
    mutate(end = 2 * pi * cumsum(Percent)/sum(Percent),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

pp <- ggplot(piedataNEW) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Placement)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = Percentages,
                  hjust = hjust, vjust = vjust)) +
    coord_fixed() +
    scale_x_continuous(limits = c(-1.5, 1.5),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1, 1.1),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    ggtitle("March Madness Placement Distribution") +
    theme_void()
pp
#### With the exception  of R68, placement gets smaller and smaller further you go

#################### SECOND PLOT #################
#Percentage showing how many teams make the tournament

sum(cbb$TOURNAMENT == "YES", na.rm=TRUE) # 476
0.0407332*476 # 19.39%
sum(cbb$TOURNAMENT == "NO", na.rm=TRUE) # 1979
0.0407332*1979 # 80.61%

Placement2 <- c("YES", "NO")
Percent2 <- c(476, 1979)
Percentages2 <- c("19.39%", "80.61%")

piedata2 <- data.frame(Placement2,Percent2)
piedataNEW2 <- piedata2 %>% 
    mutate(end = 2 * pi * cumsum(Percent2)/sum(Percent2),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

pp2 <- ggplot(piedataNEW2) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                     start = start, end = end, fill = Placement2)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = Percentages2,
                  hjust = hjust, vjust = vjust)) +
    coord_fixed() +
    scale_x_continuous(limits = c(-1.5, 1.5),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1, 1.1),    # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    ggtitle("Teams that Made March Madness") +
    theme_void()
pp2

####################### Viz 3 - Boxplot ####################

# Distribution of WAB for teams

bxp <- ggplot(cbb, aes(y=WAB)) + 
    geom_boxplot(fill=alpha('tomato1')) +
    theme_classic() +
    ggtitle("Distribution of Games Won over a MM Team")
bxp

####################### Viz 4 - Barplot #################

# Comparison of ADJOE + ADJDE to Postseason Finish

#this is to get the numbers
POSTSEASON <- table(cbb$POSTSEASON)
cbb %>% group_by(POSTSEASON) %>% summarize(Avg = mean(ADJOE))
cbb %>% group_by(POSTSEASON) %>% summarize(Avg = mean(ADJDE))

#this is to graph it
Postseason_Finish <- factor(df$Postseason_Finish, 
                            levels=c("Champions", "2ND", "F4"
                                     , "E8", "S16", "R32"
                                     , "R64", "R68", "DNF"))
df <- data.frame(Efficiency=rep(c("Offensive", "Defensive"), each=9),
                 Postseason.Finish= c("Champions", "2ND", "F4"
                                    , "E8", "S16", "R32"
                                    , "R64", "R68", "DNF"
                                    , "Champions", "2ND", "F4"
                                    , "E8", "S16", "R32"
                                    , "R64", "R68", "DNF"),
                 Points=c(121, 120, 116, 118, 116, 113, 109, 107, 101,
                          90.4, 91.5, 92, 92.8, 93.8, 95.1, 98.5, 103, 105))
head(df)

bp <- ggplot(data=df, aes(x=Postseason.Finish, y=Points, fill=Efficiency)) +
    geom_bar(stat="identity", position=position_dodge()) + 
    coord_cartesian(ylim=c(75,125)) +
    scale_fill_manual(values=c('tomato1','mediumseagreen')) +
    ggtitle("Offensive and Defensive Efficiency Translation to Postseason Finish")+
    theme_classic()
bp

###################### Viz 5 - Scatter ##################

# Comparison of EFG_O and EFG_D to Games won

Field_Goals <- c(cbb$EFG_D, cbb$EFG_O)
sp <- ggplot(data=cbb, aes(x=EFG_O, y=W, colour=EFG_O)) +
    geom_point(aes(x=EFG_O, y=W, colour="EFG_O")) + 
    geom_smooth(aes(EFG_O,W), method=lm, se=FALSE) +
    geom_point(aes(x=EFG_D, y=W, colour="EFG_D"), alpha=1/5) +
    geom_smooth(aes(EFG_D,W), method=lm, se=FALSE) +
    xlab("Field Goal Percentage") +
    ylab("Wins") +
    ggtitle("How Field Goal Percentage Affects Games Won") +
    theme_classic() + labs(colour="Field_Goals") +
    scale_colour_manual(values=c("red", "green"))
sp

#################### Viz 6 - Time Series #####################

cbb2 <- filter(cbb, POSTSEASON %in% c("Champions", "DNF"))
df.year <- aggregate(cbb2$W
                       , list(year = cbb2$YEAR, Finish = cbb2$POSTSEASON)
                       , mean)
tsp <- ggplot(df.year) +
    aes(x = year, y = x, color = Finish) +
    geom_line(size=2) +
    theme_classic() +
    xlab("Year") +
    ylab("Games Won") +
    ggtitle("Games Won by MM Champs vs. Teams not in Tournament") +
    scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019)) +
    scale_y_continuous(breaks=c(5,10,15,20,25,30,35,40,45,50)) +
    coord_cartesian(ylim = c(5, 50))
tsp


#################### Viz 7 - Tree Map ############################

cbb3 <- filter(cbb, TOURNAMENT %in% c("YES"))
cbb4 <- filter(cbb3, YEAR %in% c("2019"))
tree <- aggregate(cbb4$COUNT
                  , list(t=cbb4$POSTSEASON!="DNF", w = cbb4$CONF, r = cbb4$POSTSEASON)
                  , sum)
trp <- ggplot(tree) + aes(area = x, fill = r, subgroup = r) +
    geom_treemap() +
    geom_treemap_subgroup_text(size = 20, color = "red", place = "bottomleft") +
    geom_treemap_text(aes(label = w), color = "blue") +
    guides(fill = FALSE) +
    scale_fill_manual(values = c("cornsilk"
                                 , "cornsilk3"
                                 , "burlywood"
                                 , "burlywood4"
                                 , "darkgoldenrod4"
                                 , "sienna4", "sienna2"
                                 , "sienna3")) +
    ggtitle("Distribution of Conferences at Postseason Finished")
trp

###################### Viz 8 - Line THE BIG ONEEEE ##############################

cbb5 <- filter(cbb3, CONF %in% c("ACC", "B10", "B12", "BE", "SEC"))
table(cbb5$CONF)
df.year <- aggregate(cbb5$COUNT
                     , list(t=cbb5$POSTSEASON!="DNF",Conferences = cbb5$CONF, r = cbb5$YEAR)
                     , sum)
tsp2 <- ggplot(df.year) +
    aes(x = r, y = x, color = Conferences) +
    geom_line() +
    geom_smooth(method='lm', formula= y~x, size=2) +
    theme_classic() +
    xlab("Year") +
    ylab("Count") +
    ggtitle("Count and Prediction for Top Conference Appearances in MM") +
    scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019)) +
    scale_y_continuous(breaks=c(1:10)) +
    coord_cartesian(ylim = c(1, 10)) +
    coord_cartesian(xlim = c(2013,2019))
tsp2
## The conference with the most appearances in the 2020
## March Madness Tournament would have been the ACC according 
## to these regressions