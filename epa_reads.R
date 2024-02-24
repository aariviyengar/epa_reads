library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(nflreadr)
library(dplyr)
library(ggrepel)
first_read <- load_ftn_charting(seasons=most_recent_season())|>
  filter(read_thrown==1)|>
  group_by(nflverse_game_id,nflverse_play_id)
not_first_read <- load_ftn_charting(seasons=most_recent_season())|>
  filter(read_thrown!=1)|>
  group_by(nflverse_game_id,nflverse_play_id)
pbp <- load_pbp(seasons=most_recent_season())|>
  filter(pass==1)|>
  group_by(passer_player_id)|>
  filter(!is.na(epa))
first_read <- left_join(first_read,pbp,by=c("nflverse_game_id"="game_id","nflverse_play_id"="play_id"))|>
  group_by(passer_player_id)|>
  summarize(name=first(passer_player_name),
            team=first(posteam),
            passes = n(),
            epa_play_first_read = mean(epa))|>
  filter(!is.na(name))|>
  filter(passes>=100)
not_first_read <- left_join(not_first_read,pbp,by=c("nflverse_game_id"="game_id","nflverse_play_id"="play_id"))|>
  group_by(passer_player_id)|>
  summarize(name=first(passer_player_name),
            team=first(posteam),
            passes=n(),
            epa_play_other_reads = mean(epa))
reads <- left_join(first_read,not_first_read,by=c("passer_player_id","name","team"))
reads <- reads|>
  mutate(epa_difference=epa_play_first_read-epa_play_other_reads)

reads <- left_join(reads,teams_colors_logos,by=c("team"="team_abbr"))
reads|>
  ggplot(aes(x=epa_play_first_read,y=epa_play_other_reads))+
  geom_point(aes(fill=team_color,color=team_color2),shape=21,alpha=0.9)+
  scale_color_identity(aesthetics = c("fill","color"))+
  geom_text_repel(aes(label=paste(name)))+
  theme_bw()+
  labs(x="EPA/Play When Throwing to First Read",
       y="EPA/Play When Not Throwing to First Read",
       title = "EPA/Play When Throwing and Not Throwing to First Read",
       subtitle = "2023 Season",
       caption= "By Aariv Iyengar | @AarivAnalytics")+
  theme(panel.grid.major.y = element_blank(),
        plot.title=element_text(size=22, hjust=0.5,face="bold"),
        plot.subtitle=element_text(size=16,hjust=0.5))+
  annotate("segment", x=-0.2,xend=0.45,y=-0.6,yend=0,linetype="dashed")+
  annotate("segment",x=0.1,xend=0.75,y=-0.6,yend=0,linetype="dashed")+
  annotate("text",x=-0.1,y=-0.1,label="Great Past First Read",size=5)+
  annotate("text",x=0.5,y=-0.5,label="Bad Past First Read",size=5)
ggsave(filename="epa_reads.png",width=14,height=10,dpi="retina")


