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



