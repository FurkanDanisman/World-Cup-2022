# Libraries

# Libraries (It is combined library for all the codes regarding World Cup 2022 Analysis)
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(gghighlight)
library(colorspace)
library(curl)
library(BasketballAnalyzeR)
library(htmltab)
library(gridExtra)
library(cowplot)
library(kableExtra)
library(magick)
library(ggbreak) 
library(patchwork)
library(tidyr)
library(dplyr)
library(kableExtra)
library(knitr)

# Upload the scraped data
file_names <- list.files(pattern = "World",full.names = T)

all_stat <- read_csv(file_names[1],show_col_types = F)


for (f in file_names[-1]) all_stat <- rbind(all_stat, read_csv(f, show_col_types = FALSE))

length(unique(all_stat))

all_stat[,1] <- NULL

# First Graph of the blog
a <- (all_stat %>% filter(Min>500 & Team=="Argentina"))
c <- (all_stat %>% filter(Min>500 & Opponent=="Argentina"))

a$Opponent_xG <- c$`Expected >> xG`

a$game <- c("Round of 16","Semi-Final", "Final", "Group Stage", "Group Stage", "Quarter-Final", "Group Stage")


G_vs_xG_Argentina <- a %>%
  ggplot(aes(x = `Expected >> xG`, y = `Performance >> Gls`)) +
  geom_point(col = "orange2",size=4) +
  labs(title = "Argentina in 2022 World Cup",
       subtitle = expression(bold("Goals") * " vs " * bold("Expected Goals") * " in Each Game"),
       caption = "Data source: fbref.com",size=5) +
  xlim(0, 4) +
  ylim(0, 4) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.7, color = "grey2") +
  theme_classic() +
  xlab("Expected Goals") +
  ylab("Goals") +
  ggrepel::geom_text_repel(data = a,
                           aes(x = `Expected >> xG`, y = `Performance >> Gls`, label = paste0(Opponent, "\n", game),),
                           color = "orange2", nudge_x = 0, nudge_y = 0.2,
                           size=5) +
  geom_point(aes(mean(a$`Expected >> xG`), y = mean(a$`Performance >> Gls`)), color = "black", size = 5) +
  annotate("text", x = mean(a$`Expected >> xG`), y = mean(a$`Performance >> Gls`), label = "AVG", color = "black", vjust = -1,size=5) +
  gghighlight(`Expected >> xG` < `Performance >> Gls`)+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size=12),
        axis.title = element_text(size=15)
  )

G_vs_xG_Argentina
ggsave("GvsxG_Argentina.png", w = 16.5, h = 8.5, dpi = 600)

# Second graph of the blog
xG_Argentina_Opponent <- a %>%
  ggplot(aes(y = `Expected >> xG`, x = `Opponent_xG`)) +
  geom_point(col = "orange2",size=4) +
  labs(title = "Argentina in 2022 World Cup",
       subtitle = expression(bold("Argentina's Expected Goals") * " vs " * bold("Opponents' Expected Goals")),
       caption = "Data source: fbref.com") +
  xlim(0, 4) +
  ylim(0, 4) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.7, color = "grey2") +
  theme_classic() +
  xlab("Opponents' Expected Goal") +
  ylab("Argentina's Expected Goal") +
  geom_text(data = a,
            aes(y = `Expected >> xG`, x = `Opponent_xG`, label = paste0(Opponent, "\n", game)),
            color = "orange2", 
            nudge_x = ifelse(a$Opponent == "Australia", -0.15, ifelse(a$Opponent == "Netherlands", 0.2, 0)),
            nudge_y = ifelse(a$Opponent %in% c("Australia"), -0.07, 0.2),size=5) +
  geom_point(aes(mean(a$Opponent_xG), y = mean(a$`Expected >> xAG`)), color = "black", size = 5) +
  annotate("text", y = mean(a$`Expected >> xAG`), x = mean(a$Opponent_xG), label = "AVG", color = "black", vjust = -1,size=5) +
  gghighlight(`Expected >> xG` >= `Opponent_xG`)+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size=12),
        axis.title = element_text(size=15)
  )

xG_Argentina_Opponent
ggsave("xG_Argentina_Opponent.png", w = 16.5, h = 8.5, dpi = 600)

# Fourth Graph of the blog
b <- all_stat %>% 
  filter(Team == "Argentina",Player=="Lionel Messi")

b$game <- c("Round of 16","Semi-Final", "Final", "Group Stage", "Group Stage", "Quarter-Final", "Group Stage")

b %>% 
  ggplot(aes(x = `Expected >> xG`, y = `Performance >> Gls`)) +
  geom_point(color = ifelse(b$`Performance >> Gls` >= b$`Expected >> xG`, "navy", "grey"),size=4) +
  theme_light() +
  ggrepel::geom_text_repel(aes(label = ifelse(`Player` == "Lionel Messi", paste0(Opponent,"\n", game), "")),
                           color = ifelse(b$`Performance >> Gls` >= b$`Expected >> xG`, "navy", "grey"),
                           nudge_x = ifelse(b$Opponent == "Croatia", -0.15, 0),
                           nudge_y = ifelse(b$Opponent == "Croatia", 0.15, 0.16),
                           size=5) +
  labs(title = "Lionel Messi in 2022 World Cup",
       subtitle = expression(bold("Goals") * " vs " * bold("Expected Goals") * " in Each Game"),
       caption = "Data source: fbref.com") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.5) +
  xlim(0, 3) +
  ylim(0, 3) +
  xlab("Expected Goals")+
  ylab("Goals")+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size=12),
        axis.title = element_text(size=15)
  )

ggsave("GvsxG_Messi2,e.png", w = 16.5, h = 8.5, dpi = 600)

# Third graph of the blog
c <- all_stat %>% 
  filter(Team == "France",Player=="Kylian Mbappé")
c$game <- c("Final","Quarter-Final","Group Stage","Group Stage", "Semi-Final","Round of 16","Group Stage")  

c %>% 
  ggplot(aes(x = `Expected >> xG`, y = `Performance >> Gls`)) +
  geom_point(color = ifelse(c$`Performance >> Gls` >= c$`Expected >> xG`, "navy", "grey"),size=4) +
  theme_light() +
  ggrepel::geom_text_repel(aes(label = ifelse(`Player` == "Kylian Mbappé", paste0(Opponent, "\n", game), "")),
                           color = ifelse(c$`Performance >> Gls` >= c$`Expected >> xG`, "navy", "grey"),
                           arrow = arrow(length = unit(0.4, "cm")),
                           nudge_x = 0, nudge_y = 0.2,
                           size=5) +
  labs(title = "Kylian Mbappé in 2022 World Cup",
       subtitle = expression(bold("Goals") * " vs " * bold("Expected Goals") * " in Each Game"),
       caption = "Data source: fbref.com") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.5) +
  xlim(0, 3) +
  ylim(0, 4) +
  xlab("Expected Goals")+
  ylab("Goals")+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size=12),
        axis.title = element_text(size=15))

ggsave("GvsxG_Mbappe.png", w = 16.5, h = 8.5, dpi = 600)

# Turning data into a overall player data
all_stat_player <- all_stat %>% subset(Min<300) 

str(all_stat_player)

all_stat_player$Team <- as.factor(all_stat_player$Team)
all_stat_player$Club <- as.factor(all_stat_player$Club)
all_stat_player$Pos <- as.factor(all_stat_player$Pos)
all_stat_player$Age <-  sub("-.*","",all_stat_player$Age)
all_stat_player$`#` <-  as.factor(all_stat_player$`#`)
all_stat_player$Age <- as.numeric(all_stat_player$Age)
all_stat_player <- all_stat_player[,-c(1,3,10)]
colnames(all_stat_player)[7] <- "Number"

all_stat_player <- all_stat_player %>%
  mutate_at(vars(-Team), ~ifelse(is.na(.), 0, .))

all_stat_player <- all_stat_player %>% 
  group_by(Player,Team,Age,Club,Number) %>%
  summarise_each(list(sum))

all_stat_player <- all_stat_player %>%
  group_by(Player)

all_stat_player <- all_stat_player %>%
  arrange(desc(all_stat_player[is.numeric(all_stat_player)])) %>%
  slice(1)

all_stat_player <- all_stat_player %>%
  ungroup()

all_stat_player$`Launched >> Cmp%` <- round(all_stat_player$`Launched >> Cmp`/all_stat_player$`Launched >> Att`,2)
all_stat_player$`Take-Ons >> Succ%` <- round(all_stat_player$`Take-Ons >> Succ`/all_stat_player$`Take-Ons >> Att`,2)
all_stat_player$`Take-Ons >> Tkld%` <-  round(all_stat_player$`Take-Ons >> Tkld`/all_stat_player$`Take-Ons >> Att`,2)
all_stat_player$`Shot Stopping >> Save%` <- round(all_stat_player$`Shot Stopping >> Saves`/all_stat_player$SoTA,2)
all_stat_player$`Crosses >> Stp%` <- round(all_stat_player$`Crosses >> Stp`/all_stat_player$`Crosses >> Opp`,2)
all_stat_player <- all_stat_player %>%
  mutate_at(vars(-Team), ~ifelse(is.na(.), 0, .))

# First table in the blog

all_stat_player_Table <- all_stat_player
all_stat_player_Table$xG_Diff <-  all_stat_player_Table$`Performance >> Gls` - all_stat_player_Table$`Expected >> xG`
all_stat_player_Table$xAG_Diff <- all_stat_player_Table$`Performance >> Ast` - all_stat_player_Table$`Expected >> xAG`
all_stat_player_Table$xG_Min <- round(all_stat_player_Table$`Expected >> xG`/all_stat_player$Min,3)
all_stat_player_Table$xAG_Min <- round(all_stat_player_Table$`Expected >> xAG`/all_stat_player$Min,3)

Player_xG_Performance <- all_stat_player_Table[c("Player", "Team", "Min", "Performance >> Gls", "Expected >> xG", "xG_Diff", "xG_Min")]
Player_xG_Performance <-  Player_xG_Performance %>% subset(Player %in% c("Lionel Messi","Kylian Mbappé","Olivier Giroud","Neymar","Robert Lewandowski"))
colnames(Player_xG_Performance) <- c("Player","Team","Minutes","Goals","xG","xG Diff","xG Min")

# Sort the data frame by "xG" in descending order
Player_xG_Performance <- Player_xG_Performance[order(-Player_xG_Performance$xG), ]
No. <- c(1,2,3,4,5)
Player_xG_Performance <- cbind(No.,Player_xG_Performance)
Player_xG_Performance %>%
  kable("html") %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )
ggsave("xG_Table.png", w = 16.5, h = 8.5, dpi = 600)
