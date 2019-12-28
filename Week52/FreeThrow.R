library(tidyverse)
library(magrittr)
mydata<-read_csv(file = "./Week52/Data/freethrows.csv")
mydata$round<-as.integer(mydata$round)

mydata %>% group_by(round) %>% summarise(totalshotmade = sum(shot_made_flag))
mydata %>% group_by(round) %>% summarise(numshots=n())


freethrowpercent<-mydata %>% count(round,shot_made_flag,name = "ShotsByRound") %>%
  group_by(round) %>% 
  mutate(percs= (ShotsByRound/sum(ShotsByRound))*100) %>% 
  filter(shot_made_flag==1) %>%
  select(round,percs)
freethrowpercent %<>% mutate(colr= if_else(percs>=70,"1","0"))



  
  ggplot(data = freethrowpercent) +
    geom_line(mapping = aes(x=round,y=percs)) +
    geom_point(mapping = aes(x=round ,y=percs, color=colr)) +
    scale_color_manual(values = c("0"="darkgrey","1"="orange")) +
    theme_light() +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.ticks.y= element_line(color="lightgrey",size = .25)) +
    #scale_x_discrete(position = "bottom",  limit=c(0,1,2,3,4,5,6,7,8,9,10,11),drop=FALSE,expand = c(0,0.05)) +
    scale_x_discrete(position = "bottom",  limit=c(0,1,2,3,4,5,6,7,8,9,10,11),drop=FALSE,expand = expand_scale(mult=c(0,0.1))) +
    scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0,100),expand = c(0,0), labels=function(x) paste0(x,"%")) +
    #scale_y_discrete(limits=c(0,100),breaks=c("0%","50%","100%"),expand=c(0,0))+
    expand_limits(x=0,y=0) +
    labs(x="Round",y="Free Throw %",title = "Free Throw Percentage by Round") +
    theme(axis.line.y = element_line(color = "grey"))+
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.border = element_blank()) +
    theme(legend.position = "none")
   

  
    

  runningdata<-mydata %>% group_by(round) %>% 
    summarise(grptotals=n(),shotmades=sum(shot_made_flag)) %>% 
    mutate(cuttot=cumsum(grptotals),cushot=cumsum(shotmades),cupercs=round(cushot*100/cuttot,2) , colr=if_else(cupercs >=70 , "1", "0"))
  
    
  
  
  ggplot(data = runningdata) +
    geom_line(mapping = aes(x=round,y=cupercs)) +
    geom_point(mapping = aes(x=round ,y=cupercs, color=colr)) +
    scale_color_manual(values = c("0"="darkgrey","1"="orange")) +
    theme_light() +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.ticks.y= element_line(color="lightgrey",size = .25)) +
    scale_x_discrete(position = "bottom",  limit=c(0,1,2,3,4,5,6,7,8,9,10,11),expand = expand_scale(mult=c(0,0.1))) +
    scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0,100),expand = c(0,0),labels=function(x) paste0(x,"%")) +
    #scale_y_discrete(limits=c(0,100),breaks=c("0%","50%","100%"),expand=c(0,0))+
    expand_limits(x=0,y=0) +
    labs(x="Round",y="Running Free Throw %",title = "Running Free Throw Percentage") +
    theme(axis.line.y = element_line(color = "grey"))+
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.border = element_blank()) +
    theme(legend.position = "none")
  

  runningdatabyround<- mydata %>% 
    group_by(round) %>% 
    mutate(curnd=cumsum(shot_made_flag),rupercs=round((curnd*100/shot_number),2))
  
  runningdatabyround %<>% inner_join(y=freethrowpercent,by="round")
  

  ggplot(data = runningdatabyround ) +
    geom_line(mapping = aes(x=shot_number,y=rupercs,color=colr)) +
    scale_color_manual(values = c("0"="darkgrey","1"="orange")) +
    theme_light() +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.ticks.y= element_line(color="lightgrey",size = .25)) +
    scale_x_discrete(position = "bottom",  limit=c(0,10,20,30,40,50,60,70),expand = expand_scale(mult=c(0,0.1))) +
    scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0,100),expand = c(0,0),labels=function(x) paste0(x,"%")) +
    #scale_y_discrete(limits=c(0,100),breaks=c("0%","50%","100%"),expand=c(0,0))+
    expand_limits(x=0,y=0) +
    labs(x="",y="Free Throw %",title = "Running Free Throw Percentage") +
    theme(axis.line.y = element_line(color = "red"))+
    theme(panel.grid = element_blank()) +
    theme(panel.border = element_blank()) +
    theme(legend.position = "none") +
    facet_wrap(.~round ,nrow = 1) +
    theme(strip.background = element_blank())
  
    
  # 
  
  # seqlen<-rle(mydata%>%filter(round==4) %>% select(shot_made_flag) %>% pull(shot_made_flag))
  # max(seqlen$lengths[seqlen$values==1])
  # max(seqlen$lengths[seqlen$values==0])
 
streaks <- function(x,wl="w"){
    seqlen<-rle(x)
    if(wl=="w")
      {
      streak<-max(seqlen$lengths[seqlen$values==1])
      }else
      {
      streak<-max(seqlen$lengths[seqlen$values==0])
      }
    
    return(streak)
}
  
wlstreaks<-mydata %>% group_by(round) %>% summarise(WinningStreak=streaks(shot_made_flag), LosingStreak=streaks(shot_made_flag,"l"))

wlstreaksm<-wlstreaks %>% pivot_longer(cols = c("WinningStreak","LosingStreak"),names_to = "StreakDef")

ggplot(data = wlstreaksm,mapping = aes(x=round,y=value,fill=StreakDef)) +
  scale_fill_manual(values = c("grey","grey"))+
  geom_bar(stat = "identity",position = position_dodge(width = 10),width = .25) +
  geom_text( aes(label=value),position = position_dodge(width = 10),vjust=0,hjust=0) +
  theme_bw()+
  coord_flip()+
  facet_wrap(.~round,nrow = 1) +
  theme(panel.grid = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  labs( x="",y="")
)
