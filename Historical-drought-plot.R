library(plyr)
library(ggplot2)
library(tidyr)
library(SPEI)

PlotDir <- "~/Projects/SFAN-P4CC-workshop/"
TableDir <- "~/Projects/SFAN-P4CC-workshop/GOGA/WarmWet_HotDry/tables/"



SPEI_annual_bar <- function(data, period.box=T, title,CFmethod=""){
  ggplot(data = data, aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
    {if(period.box==T) geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray")} +
    geom_bar(stat="identity",aes(fill=col),col="gray30",size=.8) + 
    geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
    scale_fill_manual(name="",values =c("white","black"),drop=FALSE) +
    labs(title = title, 
         x = "Date", y = "SPEI",caption=
           if(MethodCaption == "Y"){CFmethod}) +
    guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
}

MonthlyWB <- read.csv(paste0(TableDir,"WB-Monthly.csv")) %>% 
  left_join(WB_GCMs,by="GCM") %>% 
  mutate(CF = replace_na(CF,"Historical"),
         CF = factor(CF, levels=c("Historical",CFs)),
         Date = as.POSIXct(paste(substr(yrmon,1,4),substr(yrmon,5,6),"1",sep="-"),format="%Y-%m-%d"),
         Year = format(Date, "%Y")) %>% 
  arrange(Date)


M1 <- list()
for (i in 1:length(CFs)){
  M = MonthlyWB %>% filter(CF %in% c("Historical",CFs[i])) %>% 
    complete(Date = seq(min(Date), max(Date), by = "1 month"), 
             fill = list(value = NA)) 
  
  tp<-ts(M$sum_p.mm,frequency=12,start=c(SPEI_start,1)); tp[is.na(tp)]<-0
  tpet<-ts(M$sum_pet.mm,frequency=12,start=c(SPEI_start,1)); tpet[is.na(tpet)]<-0
  SPEI<-spei(tp-tpet,SPEI_per,ref.start=c(SPEI_start,1),ref.end=c(SPEI_end,12))
  M$SPEI = SPEI$fitted[1:length(SPEI$fitted)]
  M1[[i]]<-M %>% drop_na()
}
all2<- plyr::ldply(M1, data.frame) #convert back to df
all2$SPEI[which(is.infinite(all2$SPEI))]<- -5 #getting some -Inf values that are large jumps, temp fix

# 
# all3<-subset(all2,Month==9) #Because we aggregated drought years as only applying to growing season
#                             # If you are doing for place where winter drought would be important, use following line
all3<-aggregate(cbind(sum_pet.mm,SPEI)~Year+CF,all2,mean)

###################################### PLOT ANNUAL TIME-SERIES #################################################

# MACA prep dataframe
all3$col[all3$SPEI>=0]<-"above average"
all3$col[all3$SPEI<0]<-"below average"
all3$col<-factor(all3$col, levels=c("above average","below average"))
all3$Year<-as.numeric(all3$Year)

# CF 
CF1<-subset(all3, Year <= 2020 )

SPEI_annual_bar(CF1, period.box=T,
                title=paste(SiteID, "-Historical SPEI values", sep = " " ),CFmethod="I") 
ggsave("Historical-SPEI.png", path = PlotDir, width = 12, height = 6)
