library(dplyr)
library(ggplot2)
library(RColorBrewer)

rm(list=ls())
ProjDir <- "~/Projects/SFAN-P4CC-workshop/Historical/"
data <- read.csv(paste0(ProjDir, "Historical-TS.csv"))
data <- data %>% group_by(ID) %>% 
  mutate(rTmean = zoo::rollmean(TavgF, 5, align="center",fill=NA)) |> 
  mutate(rPpt = zoo::rollmean(PptIn, 5, align="center" ,fill=NA))

head(data)

display.brewer.pal(5, "YlOrRd")
display.brewer.pal(5, "Blues")
brewer.pal(5,"YlOrRd")
brewer.pal(5, "Blues")

temp.pal <- c("#F03B20","#BD0026","#FD8D3C")
  ggplot(data, aes(x=Year, y=TavgF,group=ID)) + geom_line(aes(Year, TavgF,color=ID), na.rm=TRUE,size=.2) +
  ylab(expression(paste("Avg Temperature", ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  # geom_smooth(aes(Year, TavgF),se=F, method="lm", na.rm=TRUE,linetype=1) +
  geom_line(aes(Year, rTmean, color=ID), size=1.3) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  coord_cartesian(ylim = c(min(data$TavgF)-1, max(data$TavgF)))+
  geom_smooth(method = lm,se=F, aes(Year, TavgP2), na.rm=TRUE,colour="black",linetype=1) +
  ##! Add aesthetics to make background white
  theme_classic() +
  scale_color_manual(guide="none",values=temp.pal)
  ggsave("Historical-temp-TS.png", width = 6, height = 4, path = ProjDir)
  
  brewer.pal(5, "Blues")
  ppt.pal <- c("#3182BD","#6BAED6" ,"#08519C")
  ggplot(data, aes(x=Year, y=PptIn,group=ID)) + geom_line(aes(Year, PptIn,color=ID), na.rm=TRUE,size=.2) +
    ylab("Total Precipitation (in/year)") + xlab("") +
    # geom_text(aes(x=1895, y= 13.5, label = "B")) +
    # geom_smooth(aes(Year, TavgF),se=F, method="lm", na.rm=TRUE,linetype=1) +
    geom_line(aes(Year, rPpt, color=ID), size=1.3) +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
    coord_cartesian(ylim = c(min(data$PptIn)-5, max(data$PptIn)))+
    geom_smooth(method = lm,se=F, aes(Year, PptP2), na.rm=TRUE,colour="black",linetype=2) +
    ##! Add aesthetics to make background white
    theme_classic() +
    scale_color_manual(guide="none",values=ppt.pal)
  ggsave("Historical-precip-TS.png", width = 6, height = 4, path = ProjDir)
  
