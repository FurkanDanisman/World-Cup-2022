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
setwd("C://Users//zenci//OneDrive//Masaüstü//WORLD_CUP_2022")

# Web Scraping
page <- read_html("https://fbref.com/en/comps/1/schedule/World-Cup-Scores-and-Fixtures")
links_1 <- unlist(page %>% html_nodes("a") %>% html_attr("href"))
head(links_1)

links_2 <- strsplit(links_1,'"')
links_3[100:110]
links_3 <- links_2[grepl("matches",links_2)]
links_4 <- links_3[grepl("FIFA",links_3)]
all_urls <- unique(links_4)


team_urls <- all_urls[grepl("Argentina",all_urls)]


selected_urls <- paste("https://fbref.com",team_urls,sep = "")
head(selected_urls)

g <- 1
nchar("https://fbref.com/en/matches/109ad6ba/")  
nchar("-FIFA-World-Cup")

game_data <- substr(selected_urls[g],39,nchar(selected_urls[g])-15)  

game_data <- str_replace(game_data, "January", "Jan")
game_data <- str_replace(game_data, "February", "Feb")
game_data <- str_replace(game_data, "March", "Mar")
game_data <- str_replace(game_data, "April", "Apr")
game_data <- str_replace(game_data, "June", "Jun")
game_data <- str_replace(game_data, "July", "Jul")
game_data <- str_replace(game_data, "August", "Aug")
game_data <- str_replace(game_data, "September", "Sep")
game_data <- str_replace(game_data, "October", "Oct")
game_data <- str_replace(game_data, "November", "Nov")
game_data <- str_replace(game_data, "December", "Dec")

selected_urls <- str_replace(selected_urls, "-1-", "-01-")
selected_urls <- str_replace(selected_urls, "-2-", "-02-")
selected_urls <- str_replace(selected_urls, "-3-", "-03-")
selected_urls <- str_replace(selected_urls, "-4-", "-04-")
selected_urls <- str_replace(selected_urls, "-5-", "-05-")
selected_urls <- str_replace(selected_urls, "-6-", "-06-")
selected_urls <- str_replace(selected_urls, "-7-", "-07-")
selected_urls <- str_replace(selected_urls, "-8-", "-08-")
selected_urls <- str_replace(selected_urls, "-9-", "-09-")

date <- substr(game_data,nchar(game_data)-10,nchar(game_data))

teams <- substr(game_data,1,nchar(game_data)-12)

teams <- str_replace(teams,"Saudi-Arabia","Saudi Arabia")
teams <- str_replace(teams,"Costa-Rica","Costa Rica")
teams <- str_replace(teams,"Korea-Republic","Korea Republic")
teams <- str_replace(teams,"IR-Iran","Iran")

teamA <- sub("-.*","",teams)
teamB <- sub(".*-","",teams)

url <- selected_urls[g]

statA <- curl::curl(url) %>%
  xml2::read_html() %>%
  rvest::html_nodes('table') %>%
  rvest::html_table() %>%
  .[[4]]

colnames(statA) <- paste0(colnames(statA)," >> ", statA[1,])
names(statA)[1:6] <- paste0(statA[1,1:6])
statA <- statA[-c(1),]

statA <- cbind(date,Team=teamA, Opponent=teamB,statA)

statB <- curl::curl(url) %>%
  xml2::read_html() %>%
  rvest::html_nodes('table') %>%
  rvest::html_table() %>%
  .[[11]]

colnames(statB) <- paste0(colnames(statB)," >> ", statB[1,])
names(statB)[1:6] <- paste0(statB[1,1:6])
statB <- statB[-c(1),]

statB <- cbind(date,Team=teamB, Opponent=teamA,statB)

stat_both <- rbind(statA,statB)

selected_urls <- paste("https://fbref.com",all_urls,sep = "")
selected_urls
all_stat <- NULL
full_stat <- NULL
g <- 1
i <- 5

for (g in 1:length(selected_urls)) {
  game_data <- substr(selected_urls[g],39,nchar(selected_urls[g])-15)  
  
  game_data <- str_replace(game_data, "January", "Jan")
  game_data <- str_replace(game_data, "February", "Feb")
  game_data <- str_replace(game_data, "March", "Mar")
  game_data <- str_replace(game_data, "April", "Apr")
  game_data <- str_replace(game_data, "June", "Jun")
  game_data <- str_replace(game_data, "July", "Jul")
  game_data <- str_replace(game_data, "August", "Aug")
  game_data <- str_replace(game_data, "September", "Sep")
  game_data <- str_replace(game_data, "October", "Oct")
  game_data <- str_replace(game_data, "November", "Nov")
  game_data <- str_replace(game_data, "December", "Dec")
  
  selected_urls <- str_replace(selected_urls, "-1-", "-01-")
  selected_urls <- str_replace(selected_urls, "-2-", "-02-")
  selected_urls <- str_replace(selected_urls, "-3-", "-03-")
  selected_urls <- str_replace(selected_urls, "-4-", "-04-")
  selected_urls <- str_replace(selected_urls, "-5-", "-05-")
  selected_urls <- str_replace(selected_urls, "-6-", "-06-")
  selected_urls <- str_replace(selected_urls, "-7-", "-07-")
  selected_urls <- str_replace(selected_urls, "-8-", "-08-")
  selected_urls <- str_replace(selected_urls, "-9-", "-09-")
  
  date <- substr(game_data,nchar(game_data)-10,nchar(game_data))
  
  teams <- substr(game_data,1,nchar(game_data)-12)
  
  teams <- str_replace(teams,"Saudi-Arabia","Saudi Arabia")
  teams <- str_replace(teams,"Costa-Rica","Costa Rica")
  teams <- str_replace(teams,"Korea-Republic","Korea Republic")
  teams <- str_replace(teams,"IR-Iran","Iran")
  teams <- str_replace(teams,"United-States","USA")
  
  teamA <- sub("-.*","",teams)
  teamB <- sub(".*-","",teams)
  
  url <- selected_urls[g]
  
  statA <- curl::curl(url) %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() %>%
    .[[4]]
  
  colnames(statA) <- paste0(colnames(statA)," >> ", statA[1,])
  names(statA)[1:6] <- paste0(statA[1,1:6])
  statA <- statA[-c(1),]
  
  statA <- cbind(date,Team=teamA, Opponent=teamB,statA)
  
  statB <- curl::curl(url) %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() %>%
    .[[11]]
  
  colnames(statB) <- paste0(colnames(statB)," >> ", statB[1,])
  names(statB)[1:6] <- paste0(statB[1,1:6])
  statB <- statB[-c(1),]
  
  statB <- cbind(date,Team=teamB, Opponent=teamA,statB)
  
  stat_both <- rbind(statA,statB)
  
  all_stat <- stat_both
  Sys.sleep(15)
  
  #loop for all tables related to the game
  for(i in 5:10){
    statA <- curl::curl(url) %>%
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[i]]
    colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])
    names(statA)[1:6] <- paste0(statA[1,1:6])
    statA <- statA[-c(1),]
    statA <- cbind(date, Team=teamA, Opponent=teamB, statA)
    statB <- curl::curl(url)  %>%
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[i+7]]
    colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
    names(statB)[1:6] <- paste0(statB[1,1:6])
    statB <- statB[-c(1),]
    statB <- cbind(date, Team=teamB, Opponent=teamA, statB)
    stat_both <- rbind(statA, statB)
    all_stat <- merge(all_stat, stat_both, all=T)
    
    #remove any duplicates
    all_stat <- unique(all_stat)
    
    #remove any leading or trailing white spaces
    all_stat$Player <- str_trim(all_stat$Player, side = c("both", "left", "right"))
    
    #convert all stats into numeric variables
    if(colnames(all_stat[7])=="Pos") all_stat <- cbind(all_stat[,1:8], mutate_all(all_stat[,9:ncol(all_stat)], function(x) as.numeric(as.character(x))))
    write.csv(all_stat,paste0("World_Cup_2022_",game_data,".csv"))
    Sys.sleep(15)
  }
  full_stat <- rbind(full_stat,all_stat)
}

file_names <- list.files(pattern = "World",full.names = T)

all_stat <- read_csv(file_names[1],show_col_types = F)


for (f in file_names[-1]) all_stat <- rbind(all_stat, read_csv(f, show_col_types = FALSE))

length(unique(all_stat))

all_stat[,1] <- NULL


a <- (all_stat %>% filter(Min>500 & Team=="Argentina"))
c <- (all_stat %>% filter(Min>500 & Opponent=="Argentina"))
a$Opponent_xG <- c$`Expected >> xG`
a$game <- c("Round of 16","Semi-Final", "Final", "Group Stage", "Group Stage", "Quarter-Final", "Group Stage")
head(a)

plot <- a %>%
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
  annotate("text", x = 2.214286, y = 2.142857, label = "AVG", color = "black", vjust = -1,size=5) +
  gghighlight(`Expected >> xG` < `Performance >> Gls`)+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size=12),
        axis.title = element_text(size=15)
  )

plot
ggsave("GvsxG_Argentina.png", w = 16.5, h = 8.5, dpi = 600)

plot <- a %>%
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
ggsave("xG_Argentina_Opponent.png", w = 16.5, h = 8.5, dpi = 600)

plot

all_stat %>% filter(Min<=320 & Team=="Argentina") %>%
  ggplot(aes(x = `Expected >> xG`, y = `Performance >> Gls`)) +
  geom_point() +
  theme_light() +
  ggrepel::geom_text_repel(aes(label = ifelse(`Performance >> Gls`>0 | `Expected >> xG`>0.5,paste0(Player,"\n",Opponent),"")), color = "black", size = 2.5, segment.color = "black",max.iter = 10000) +
  labs(title = "Expected Goals and Goals Scored\n by Argentina Players",
       subtitle = "2022 WORLD CUP",
       caption = "Data source: fbref.com")+
  geom_abline(intercept = 0, slope = 1,
              linetype="dashed", size=0.5)+
  xlim(0,3)+
  ylim(0,3)

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
mean(c$`Expected >> xG`)
mean(b$`Expected >> xG`)
mean(c$`Performance >> Gls`)
mean(b$`Performance >> Gls`)

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
  
Messi_xG_Performance <- data.frame(Team="Argentina",Minutes=sum(b$Min),Goals=sum(b$`Performance >> Gls`)
                               ,xG=sum(b$`Expected >> xG`),
           xG_Dif=sum(b$`Expected >> xG`)-sum(b$`Performance >> Gls`),xG_Game=round(mean(b$`Expected >> xG`),2))  
colnames(Messi_xG_Performance)[5] <- "xG Diff"  
colnames(Messi_xG_Performance)[6] <- "xG Game" 
Messi_xG_Performance

Messi_xAG_Performance <- data.frame(Team="Argentina",Minutes=sum(b$Min),Assists=sum(b$`Performance >> Ast`)
                                   ,xAG=sum(b$`Expected >> xAG`),
                                   xAG_Dif=sum(b$`Expected >> xAG`)-sum(b$`Performance >> Ast`),xAG_Game=round(mean(b$`Expected >> xAG`),2))  
colnames(Messi_xAG_Performance)[5] <- "xAG Diff"  
colnames(Messi_xAG_Performance)[6] <- "xAG Game" 
Messi_xAG_Performance


#Sum all stats for each player

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

# Top Field Goalkeepers
all_stat_player$GK_Rank1 <- 0
all_stat_player$GK_Rank2 <- 0
all_stat_player$GK_Rank3 <- 0

columns_to_compare_GK <- c(
  "Shot Stopping >> Saves",
  "Shot Stopping >> PSxG",
  "Launched >> Cmp",
  "Launched >> Cmp%",
  "Goal Kicks >> AvgLen",
  "Crosses >> Stp",
  "Crosses >> Stp%",
  "Shot Stopping >> Save%",
  "Sweeper >> #OPA",
  "Sweeper >> AvgDist"
)
columns_to_name_GK <- c("Saves",
  "Post-Shot Expected Goals",
  "Passes Completed (Launched)",
  "Pass Completion % (Launched)",
  "Average Lenght of Goal Kicks",
  "Crosses Stopped",
  "Crosses Stopped %",
  "Save %",
  "Defense Outside of the Pen Area",
  "Distance of Defensive Action")

for (i in 1:nrow(all_stat_player)){
  # Loop through the columns to compare
  for (col_name in columns_to_compare_GK) {
    for (x in 1:3) {
      max_col_value <- sort(all_stat_player[col_name],decreasing = T)[x,1]
      if (all_stat_player[i,col_name] == max_col_value) {
        # Your action for matching values (replace with your actual action)
        # For example, you can increment a counter or perform other operations
        # For now, let's just print a message
        all_stat_player[i,paste0("GK_Rank",as.character(x))] <- all_stat_player[i,paste0("GK_Rank",as.character(x))] + 1 
      }
    }
  }
}
gk_tier_list <- all_stat_player %>% subset(GK_Rank1>0)
gk_tier_list$`Shot Stopping >> Save%`
gk_tier_list$GK_Rank1
gk_tier_list$GK_Rank2
gk_tier_list$GK_Rank3
# Sort the data by Rank_1 in descending order
gk_filtered_data <- gk_tier_list[order(gk_tier_list$GK_Rank1),]
gk_filtered_data$color_1 <- ifelse(seq_along(gk_filtered_data$GK_Rank1) >= nrow(gk_filtered_data), "blue", "grey")


gk_a <- ggplot(gk_filtered_data, aes(x = GK_Rank1, y = reorder(Player, GK_Rank1), fill = color_1)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    axis.title.x = element_text(),  # Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank() 
  ) +
  labs(
    title = "Top Goalkeepers in 2022 WORLD CUP",
    subtitle = expression("Number of GK Stats That "  * bold("Goalkeepers") * " Ranked " * bold("First")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Ranked First") +
  ylab("")+
  scale_x_cut(breaks = c(1,2),scales = 1.2/8,which = c(1,2))

# Sort the data by Rank_2 in descending order
gk_tier_list_2 <- all_stat_player %>% subset(GK_Rank2>0)
gk_filtered_data <- gk_tier_list_2[order(gk_tier_list_2$GK_Rank2),]

# Create a new column to specify the bar color
gk_filtered_data$color_2 <- ifelse(seq_along(gk_filtered_data$GK_Rank2) >= nrow(gk_filtered_data), "blue", "grey")

# Create the horizontal bar plot for Rank_2

gk_b <- ggplot(gk_filtered_data, aes(x = GK_Rank2, y = reorder(Player, GK_Rank2), fill = color_2)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  scale_x_cut(breaks = c(1,2),scales = 1.2/8,which = c(1,2)) +
  theme(
    axis.title.x = element_text(),  # Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank() 
  ) +
  labs(
    title = "Top Goalkeepers in 2022 WORLD CUP",
    subtitle = expression("Number of GK Stats That "  * bold("Goalkeepers") * " Ranked " * bold("Second")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Ranked Second") +
  ylab("")

# Sort the data by Rank_3 in descending order
gk_tier_list_3 <- all_stat_player %>% subset(GK_Rank3>0)
gk_filtered_data <- gk_tier_list_3[order(gk_tier_list_3$GK_Rank3),]

gk_filtered_data$GK_Rank3
# Create a new column to specify the bar color
gk_filtered_data$color_3 <- ifelse(seq_along(gk_filtered_data$GK_Rank3) >= nrow(gk_filtered_data) - 2, "blue", "grey")

# Create the horizontal bar plot for Rank_3
gk_c <- ggplot(gk_filtered_data, aes(x = GK_Rank3, y = reorder(Player, GK_Rank3), fill = color_3)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    axis.title.x = element_text(),  # Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank() 
  ) +
  labs(
    title = "Top Goalkeepers in 2022 WORLD CUP",
    subtitle = expression("Number of GK Stats That "  * bold("Goalkeepers") * " Ranked " * bold("Third")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Ranked Third") +
  ylab("")+
  scale_x_cut(breaks = c(1,2,3),scales = 1.2/8,which = c(1,2,3))



# Top Field Players # 
all_stat_player$Rank_1 <- 0
all_stat_player$Rank_2 <- 0
all_stat_player$Rank_3 <- 0

columns_to_compare <- c(
  "Performance >> Gls",
  "Expected >> xG",
  "Performance >> Sh",
  "Performance >> SoT",
  "SCA >> GCA",
  "SCA >> SCA",
  "Performance >> Ast",
  "Expected >> xAG",
  ">> KP",
  ">> PrgP",
  "Carries >> PrgC",
  "Receiving >> PrgR",
  "Total >> PrgDist",
  "Long >> Cmp",
  ">> 1/3",
  "Carries >> 1/3",
  "Take-Ons >> Tkld",
  "Take-Ons >> Tkld%",
  "Take-Ons >> Succ%",
  "Carries >> TotDist",
  "Take-Ons >> Succ",
  "Blocks >> Blocks",
  ">> Tkl+Int"
)

for (i in 1:nrow(all_stat_player)){
    # Loop through the columns to compare
    for (col_name in columns_to_compare) {
      for (x in 1:3) {
        max_col_value <- sort(all_stat_player[col_name],decreasing = T)[x,1]
        if (all_stat_player[i,col_name] == max_col_value) {
          # Your action for matching values (replace with your actual action)
          # For example, you can increment a counter or perform other operations
          # For now, let's just print a message
          all_stat_player[i,paste0("Rank_",as.character(x))] <- all_stat_player[i,paste0("Rank_",as.character(x))] + 1 
      }
      }
    }
  }

selected_players <- c("Luka Modrić", "Lionel Messi", "Kylian Mbappé", "Bruno Fernandes", "Harry Kane", "Antoine Griezmann", "Ivan Perišić", "Achraf Hakimi", "Julián Álvarez")

filtered_data <- all_stat_player %>%
  filter(Player %in% selected_players)

filtered_data <- filtered_data %>%
  mutate_all(~ifelse(is.na(.), 0, .))


# Sort the data by Rank_1 in descending order
filtered_data <- filtered_data[order(filtered_data$Rank_1),]
filtered_data$color_1 <- ifelse(seq_along(filtered_data$Rank_1) >= nrow(filtered_data)-2, "blue", "grey")

a <- ggplot(filtered_data, aes(x = Rank_1, y = reorder(Player, Rank_1), fill = color_1)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    axis.title.x = element_text(),  # Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank() 
  ) +
  labs(
    title = "Top Players in 2022 WORLD CUP",
    subtitle = expression("Number of Stats That "  * bold("Players") * " Ranked " * bold("First")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Ranked First") +
  ylab("")+
  scale_x_cut(c(1,2,3,4,5,6,7,8),scales = 1)

# Sort the data by Rank_2 in descending order
filtered_data <- filtered_data[order(filtered_data$Rank_2),]

# Create a new column to specify the bar color
filtered_data$color_2 <- ifelse(seq_along(filtered_data$Rank_2) >= nrow(filtered_data) - 1, "blue", "grey")

# Create the horizontal bar plot for Rank_2

b <- ggplot(filtered_data, aes(x = Rank_2, y = reorder(Player, Rank_2), fill = color_2)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  scale_x_cut(c(1, 2, 3, 4, 5, 6, 7, 8), scales = 1) +
  theme(
    axis.title.x = element_text(),  # Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank() 
  ) +
  labs(
    title = "Top Players in 2022 WORLD CUP",
    subtitle = expression("Number of Stats That "  * bold("Players") * " Ranked " * bold("Second")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Ranked Second") +
  ylab("")


# Sort the data by Rank_3 in descending order
filtered_data <- filtered_data[order(filtered_data$Rank_3),]

# Create a new column to specify the bar color
filtered_data$color_3 <- ifelse(seq_along(filtered_data$Rank_3) >= nrow(filtered_data) - 1, "blue", "grey")

# Create the horizontal bar plot for Rank_3
c <- ggplot(filtered_data, aes(x = Rank_3, y = reorder(Player, Rank_3), fill = color_3)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    axis.title.x = element_text(),  # Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank() 
  ) +
  labs(
    title = "Top Players in 2022 WORLD CUP",
    subtitle = expression("Number of Stats That "  * bold("Players") * " Ranked " * bold("Third")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Ranked Third") +
  ylab("")+
  scale_x_cut(breaks = c(1,2,3),scales = 1.2/8,which = c(1,2,3))

# Argentina Ranking / Lionel Messi - Emilano Martinez - Alexis Mac Allister

columns_to_name <- c(
  "Goals",
  "Expected Goals",
  "Shots",
  "Shots on Target",
  "Goal Creating Actions",
  "Shot Creating Actions",
  "Assists",
  "Expected Assisted Goals",
  "Key Passes",
  "Progressive Passes",
  "Progressive Carries",
  "Progressive Pass Received",
  "Progressive Carrying Distance",
  "Long Completed Pass",
  "Passes into Final Third",
  "Carrying into Final Third",
  "Tackled on Take-Ons",
  "Tackle % on Take-Ons",
  "Successful % on Take-Ons",
  "Total Carrying Distance",
  "Successful Take-Ons",
  "Blocks",
  "Tackled + Interception"
)

# Messi
messi_data <- all_stat_player %>%
  filter(Player == "Lionel Messi")
Messi_stat <- data.frame("Stat" = columns_to_name,"Rank" =0,"Value"=0)

c <- 1
  # Loop through the columns to compare
  for (col_name in columns_to_compare) {
    for (j in 1:nrow(all_stat_player)) {
      if (messi_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
        # Your action for matching values (replace with your actual action)
        # For example, you can increment a counter or perform other operations
        # For now, let's just print a message
        Messi_stat[c,"Rank"] <- j
        Messi_stat[c,"Value"] <- messi_data[1,col_name]
        c <- c+1
        break
      }
    }
  }
Messi_stat 

# Alexis Mac Allister
Alexis_Mac_Allister_data <- all_stat_player %>%
  filter(Player == "Alexis Mac Allister")
Alexis_Mac_Allister_stat <- data.frame("Stat" = columns_to_name,"Rank" =0,"Value"=0)

c <- 1
# Loop through the columns to compare
for (col_name in columns_to_compare) {
  for (j in 1:nrow(all_stat_player)) {
    if (Alexis_Mac_Allister_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
      # Your action for matching values (replace with your actual action)
      # For example, you can increment a counter or perform other operations
      # For now, let's just print a message
      Alexis_Mac_Allister_stat[c,"Rank"] <- j
      Alexis_Mac_Allister_stat[c,"Value"] <- Alexis_Mac_Allister_data[1,col_name]
      c <- c+1
      break
    }
  }
}
Alexis_Mac_Allister_stat

# Emiliano Martínez - GK
Emiliano_Martínez_data <- all_stat_player %>%
  filter(Player == "Emiliano Martínez")
Emiliano_Martínez_stat <- data.frame("Stat" = columns_to_name_GK,"Rank" =0,"Value"=0)


c <- 1
# Loop through the columns to compare
for (col_name in columns_to_compare_GK) {
  for (j in 1:nrow(all_stat_player)) {
    if (Emiliano_Martínez_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
      # Your action for matching values (replace with your actual action)
      # For example, you can increment a counter or perform other operations
      # For now, let's just print a message
      Emiliano_Martínez_stat[c,"Rank"] <- j
      Emiliano_Martínez_stat[c,"Value"] <- Emiliano_Martínez_data[1,col_name]
      c <- c+1
      break
    }
  }
}
Emiliano_Martínez_stat 


# Argentina Plots #
Messi_filtered <- Messi_stat[order(Messi_stat$Rank),]
Messi_filtered[16,3] <- "2k"
Messi_filtered[19,3] <- "1k"
messi_rank <- ggplot(Messi_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none",color="none") +
  labs(
    title = "Lionel Messi in 2022 World Cup",
    subtitle =expression("Where "  * bold("Lionel Messi") * " ranks on " * bold("Player Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(seq(1,40,by=1)),position = "top",limits = c(0,40))

messi_rank1 <- messi_rank +
  geom_segment(data = subset(Messi_filtered, Rank > 40),aes(x = 37.44, y = Stat, xend =38.94, yend = Stat ),
               arrow = arrow(length = unit(0.5, "cm")),color="azure4")+
  geom_text(data = subset(Messi_filtered, Rank > 40), aes(x = 39, y = Stat, label = paste( Rank, "th...")),
            vjust = 0.2, hjust = -0.24)


Alexis_Mac_Allister_filtered <- Alexis_Mac_Allister_stat[order(Alexis_Mac_Allister_stat$Rank),]
Alexis_Mac_Allister_filtered[12,3] <- "1k"
ama_rank <- ggplot(Alexis_Mac_Allister_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Alexis Mac Allister in 2022 World Cup",
    subtitle =expression("Where "  * bold("Alexis Mac Allister") * " ranks on " * bold("Player Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(seq(1,40,by=1)),position = "top",limits = c(0,40))

ama_rank1 <- ama_rank +
  geom_segment(data = subset(Alexis_Mac_Allister_filtered, Rank > 40),aes(x = 37.44, y = Stat, xend =38.94, yend = Stat ),
               arrow = arrow(length = unit(0.5, "cm")),color="azure4")+
  geom_text(data = subset(Alexis_Mac_Allister_filtered, Rank > 40), aes(x = 39, y = Stat, label = paste( Rank, "th...")),
            vjust = 0.2, hjust = -0.24)

Emiliano_filtered <- Emiliano_Martínez_stat[order(Emiliano_Martínez_stat$Rank),]
Emiliano_filtered
e_martinez_rank <- ggplot(Emiliano_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=20,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 5,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Emiliano Martínez in 2022 World Cup",
    subtitle =expression("Where "  * bold("Emiliano Martínez") * " ranks on " * bold("GK Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(1,seq(2,40,by=1)),position = "top",limits = c(0,40))


# France Ranking / Kylian Mbappé - Hugo Lloris - Antoine Griezmann

# Kylian Mbappé
Kylian_Mbappé_data <- all_stat_player %>%
  filter(Player == "Kylian Mbappé")
Kylian_Mbappé_stat <- data.frame("Stat" = columns_to_name,"Rank" =0,"Value"=0)

c <- 1
# Loop through the columns to compare
for (col_name in columns_to_compare) {
  for (j in 1:nrow(all_stat_player)) {
    if (Kylian_Mbappé_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
      # Your action for matching values (replace with your actual action)
      # For example, you can increment a counter or perform other operations
      # For now, let's just print a message
      Kylian_Mbappé_stat[c,"Rank"] <- j
      Kylian_Mbappé_stat[c,"Value"] <- Kylian_Mbappé_data[1,col_name]
      c <- c+1
      break
    }
  }
}
Kylian_Mbappé_stat 


# Antoine Griezmann
Antoine_Griezmann_data <- all_stat_player %>%
  filter(Player == "Antoine Griezmann")
Antoine_Griezmann_stat <- data.frame("Stat" = columns_to_name,"Rank" =0,"Value"=0)


c <- 1
# Loop through the columns to compare
for (col_name in columns_to_compare) {
  for (j in 1:nrow(all_stat_player)) {
    if (Antoine_Griezmann_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
      # Your action for matching values (replace with your actual action)
      # For example, you can increment a counter or perform other operations
      # For now, let's just print a message
      Antoine_Griezmann_stat[c,"Rank"] <- j
      Antoine_Griezmann_stat[c,"Value"] <- Antoine_Griezmann_data[1,col_name]
      c <- c+1
      break
    }
  }
}
Antoine_Griezmann_stat 

# Hugo Lloris - GK
Hugo_Lloris_data <- all_stat_player %>%
  filter(Player == "Hugo Lloris")
Hugo_Lloris_stat <- data.frame("Stat" = columns_to_name_GK,"Rank" =0,"Value"=0)


c <- 1
# Loop through the columns to compare
for (col_name in columns_to_compare_GK) {
  for (j in 1:nrow(all_stat_player)) {
    if (Hugo_Lloris_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
      # Your action for matching values (replace with your actual action)
      # For example, you can increment a counter or perform other operations
      # For now, let's just print a message
      Hugo_Lloris_stat[c,"Rank"] <- j
      Hugo_Lloris_stat[c,"Value"] <- Hugo_Lloris_data[1,col_name]
      c <- c+1
      break
    }
  }
}

Hugo_Lloris_stat 

# France Ranking #
Kylian_Mbappé_filtered <- Kylian_Mbappé_stat[order(Kylian_Mbappé_stat$Rank),]
Kylian_Mbappé_filtered[15,3] <- "2k"
Kylian_Mbappé_rank <- ggplot(Kylian_Mbappé_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Kylian Mbappé in 2022 World Cup",
    subtitle =expression("Where "  * bold("Kylian Mbappé") * " ranks on " * bold("Player Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(seq(1,40,by=1)),position = "top",limits = c(0,40))

Kylian_Mbappé_rank1 <- Kylian_Mbappé_rank +
  geom_segment(data = subset(Kylian_Mbappé_filtered, Rank > 40),aes(x = 37.44, y = Stat, xend =38.94, yend = Stat ),
               arrow = arrow(length = unit(0.5, "cm")),color="azure4")+
  geom_text(data = subset(Kylian_Mbappé_filtered, Rank > 40), aes(x = 39, y = Stat, label = paste( Rank, "th...")),
            vjust = 0.2, hjust = -0.24)

Antoine_Griezmann_filtered <- Antoine_Griezmann_stat[order(Antoine_Griezmann_stat$Rank),]
Antoine_Griezmann_filtered[12,3] <- "1k"
Antoine_Griezmann_rank <- ggplot(Antoine_Griezmann_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Antoine Griezmann in 2022 World Cup",
    subtitle =expression("Where "  * bold("Antoine Griezmann") * " ranks on " * bold("Player Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(seq(1,40,by=1)),position = "top",limits = c(0,40))

Antoine_Griezmann_rank1 <- Antoine_Griezmann_rank +
  geom_segment(data = subset(Antoine_Griezmann_filtered, Rank > 40),aes(x = 37.44, y = Stat, xend =38.94, yend = Stat ),
               arrow = arrow(length = unit(0.5, "cm")),color="azure4")+
  geom_text(data = subset(Antoine_Griezmann_filtered, Rank > 40), aes(x = 39, y = Stat, label = paste( Rank, "th...")),
            vjust = 0.2, hjust = -0.24)

Hugo_Lloris_filtered <- Hugo_Lloris_stat[order(Hugo_Lloris_stat$Rank),]
e_martinez_rank
Hugo_Lloris_rank <- ggplot(Hugo_Lloris_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=20,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 5,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Hugo Lloris in 2022 World Cup",
    subtitle =expression("Where "  * bold("Hugo Lloris") * " ranks on " * bold("GK Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(1,seq(2,40,by=1)),position = "top",limits = c(0,40))

# Croatia Ranking / Luka Modrić - Dominik Livaković - Ivan Perišić

# Luka Modrić
Luka_Modrić_data <- all_stat_player %>%
  filter(Player == "Luka Modrić")
Luka_Modrić_stat <- data.frame("Stat" = columns_to_name,"Rank" =0,"Value"=0)

c <- 1
# Loop through the columns to compare
for (col_name in columns_to_compare) {
  for (j in 1:nrow(all_stat_player)) {
    if (Luka_Modrić_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
      # Your action for matching values (replace with your actual action)
      # For example, you can increment a counter or perform other operations
      # For now, let's just print a message
      Luka_Modrić_stat[c,"Rank"] <- j
      Luka_Modrić_stat[c,"Value"] <- Luka_Modrić_data[1,col_name]
      c <- c+1
      break
    }
  }
}


# Ivan Perišić 
Ivan_Perišić_data <- all_stat_player %>%
  filter(Player == "Ivan Perišić")
Ivan_Perišić_stat <- data.frame("Stat" = columns_to_name,"Rank" =0,"Value"=0)

c <- 1
# Loop through the columns to compare
for (col_name in columns_to_compare) {
  for (j in 1:nrow(all_stat_player)) {
    if (Ivan_Perišić_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
      # Your action for matching values (replace with your actual action)
      # For example, you can increment a counter or perform other operations
      # For now, let's just print a message
      Ivan_Perišić_stat[c,"Rank"] <- j
      Ivan_Perišić_stat[c,"Value"] <- Ivan_Perišić_data[1,col_name]
      c <- c+1
      break
    }
  }
}
Ivan_Perišić_stat 

# Dominik Livaković - GK
Dominik_Livaković_data <- all_stat_player %>%
  filter(Player == "Dominik Livaković")
Dominik_Livaković_stat <- data.frame("Stat" = columns_to_name_GK,"Rank" =0,"Value"=0)


c <- 1
# Loop through the columns to compare
for (col_name in columns_to_compare_GK) {
  for (j in 1:nrow(all_stat_player)) {
    if (Dominik_Livaković_data[1,col_name] == sort(all_stat_player[col_name],decreasing = T)[j,1]) {
      # Your action for matching values (replace with your actual action)
      # For example, you can increment a counter or perform other operations
      # For now, let's just print a message
      Dominik_Livaković_stat[c,"Value"] <- Dominik_Livaković_data[1,col_name]
      Dominik_Livaković_stat[c,"Rank"] <- j
      c <- c+1
      break
    }
  }
}

Dominik_Livaković_stat 

# Croatia Ranking #
Luka_Modrić_filtered <- Luka_Modrić_stat[order(Luka_Modrić_stat$Rank),]
Luka_Modrić_filtered[3,3] <- "2k"
Luka_Modrić_filtered[6,3] <- "3k"

Luka_Modrić_rank <- ggplot(Luka_Modrić_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Luka Modrić in 2022 World Cup",
    subtitle =expression("Where "  * bold("Luka Modrić") * " ranks on " * bold("Player Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(seq(1,40,by=1)),position = "top",limits = c(0,40))

Luka_Modrić_rank1 <- Luka_Modrić_rank +
  geom_segment(data = subset(Luka_Modrić_filtered, Rank > 40),aes(x = 37.44, y = Stat, xend =38.94, yend = Stat ),
               arrow = arrow(length = unit(0.5, "cm")),color="azure4")+
  geom_text(data = subset(Luka_Modrić_filtered, Rank > 40), aes(x = 39, y = Stat, label = paste(Rank, "th...")),
            vjust = 0.2, hjust = -0.24)

Ivan_Perišić_filtered <- Ivan_Perišić_stat[order(Ivan_Perišić_stat$Rank),]
Ivan_Perišić_filtered[14,3] <- "1k"
Ivan_Perišić_rank <- ggplot(Ivan_Perišić_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Ivan Perišić in 2022 World Cup",
    subtitle =expression("Where "  * bold("Ivan Perišić") * " ranks on " * bold("Player Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(seq(1,40,by=1)),position = "top",limits = c(0,40))

Ivan_Perišić_rank1 <- Ivan_Perišić_rank +
  geom_segment(data = subset(Ivan_Perišić_filtered, Rank > 40),aes(x = 37.44, y = Stat, xend =38.94, yend = Stat ),
               arrow = arrow(length = unit(0.5, "cm")),color="azure4")+
  geom_text(data = subset(Ivan_Perišić_filtered, Rank > 40), aes(x = 39, y = Stat, label = paste(Rank, "th...")),
            vjust = 0.2, hjust = -0.24)

Dominik_Livaković_filtered <- Dominik_Livaković_stat[order(Dominik_Livaković_stat$Rank),]
Dominik_Livaković_rank <- ggplot(Dominik_Livaković_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=20,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 5,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  labs(
    title = "Dominik Livaković in 2022 World Cup",
    subtitle =expression("Where "  * bold("Dominik Livaković") * " ranks on " * bold("GK Stats")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Rank") +
  ylab("")+
  scale_x_continuous(breaks = c(1,seq(2,40,by=1)),position = "top",limits = c(0,40))

# xG, xAG Tables # 

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

Player_xAG_Performance <- all_stat_player_Table[c("Player", "Team", "Min", "Performance >> Ast", "Expected >> xAG", "xAG_Diff", "xAG_Min")]
Player_xAG_Performance <- Player_xAG_Performance %>% subset(Player %in% c("Lionel Messi","Kylian Mbappé","Antoine Griezmann","Kevin De Bruyne","Bruno Fernandes"))
colnames(Player_xAG_Performance) <- c("Player","Team","Minutes","Assits","xAG","xAG Diff","xAG Min")

# Sort the data frame by "xAG" in descending order
Player_xAG_Performance <- Player_xAG_Performance[order(-Player_xAG_Performance$xAG), ]
No. <- c(1,2,3,4,5)
Player_xAG_Performance <- cbind(No.,Player_xAG_Performance)
Player_xAG_Performance %>%
  kable("html") %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )


#####
# PLAYERS RADIAL PLOT
#####
all_stat_player$xG_Diff <-  all_stat_player$`Performance >> Gls` - all_stat_player$`Expected >> xG`
all_stat_player$xAG_Diff <- all_stat_player$`Performance >> Ast` - all_stat_player$`Expected >> xAG`

selected_players <- subset(all_stat_player, 
                           Player=="Lionel Messi" |
                             Player=="Julián Álvarez" |
                             Player=="Lautaro Martínez" |
                             Player=="Robert Lewandowski" |
                             Player=="Neymar" |
                             Player=="Wout Weghorst" |
                             Player=="Olivier Giroud" |
                             Player=="Antoine Griezmann" |
                             Player=="Kylian Mbappé")

selected_players[c(1,5),] <- selected_players[c(5,1),]
selected_players[c(2,3),] <- selected_players[c(3,2),]
selected_players[c(3,9),] <- selected_players[c(9,3),]
selected_players[c(4,7),] <- selected_players[c(7,4),]
selected_players[c(5,9),] <- selected_players[c(9,5),]
selected_players[c(6,9),] <- selected_players[c(9,6),]
selected_players[c(7,8),] <- selected_players[c(8,7),]
selected_players[c(8,9),] <- selected_players[c(9,8),]
selected_players[c(3,6),] <- selected_players[c(6,3),]

head(selected_players)
selected_players
#####
# Step 9: Create the radar plots
#####
#attach the dataset
attach(selected_players)
#select the statistics we want to see and prepare for the plot
Sel <- data.frame("xG"=`Expected >> xG`,
                  "G"=`Performance >> Gls`,
                  "xG-D"=`xG_Diff`,
                  "xAG"=`Expected >> xAG`,
                  "Ast"=`Performance >> Ast`,
                  "xAG-D"=`xAG_Diff`)
Sel <- mutate_all(Sel, function(x) as.numeric(as.character(x)))
#run the radialprofile function with std=T, which standardizes the data so that the scale looks normal
p <- radialprofile(data=Sel, title=selected_players$Player, std=T)
detach(selected_players)
?radialprofile
#####
# Step 10: Make the graph presentable
#####

# Your ggplot code
g <- grid.arrange(grobs = p[1:length(p)], ncol = 3)

# Create a custom legend using a separate ggplot
legend_data <- data.frame(
  label = c("xG = Expected Goals", "G = Goal Scored", "xG-D = G - xG", 
            "xAG = Expected Assisted Goals", "Ast = Assist made", "xAG-D = Ast - xAG"),
  x = rep(1, 6), y = seq(0.8, 0.2, length.out = 6)
)

legend_plot <- ggplot(legend_data, aes(x, y, label = label)) +
  geom_text(size = 6) +
  theme_void()+
  theme(
    legend.spacing.y = unit(0.0001, "lines")  # Adjust this value to control spacing between legend items
  )

legend_plot

# Combine the plot and the custom legend using cowplot
combined_plot <- plot_grid(g, legend_plot, ncol = 2, rel_widths = c(3, 1))


g2 <- cowplot::ggdraw(combined_plot) +
  theme_minimal() +
  labs(
    title = "Offense Players in 2022 World Cup",
    subtitle = expression(bold("Radar Plots ")* "of the Selected " * bold("Offensive")* " Stats. Values are standardized (μ=0, sd=1)"),
    caption = "Data Source: fbref.com"
  )

# Print or display the modified plot
print(g2)
ggsave("radar-key-final2.png", w = 15, h = 7.5, dpi = 600)



selected_players <- subset(all_stat_player, 
                           Player=="Lionel Messi" |
                             Player=="Julián Álvarez" |
                             Player=="Lautaro Martínez" |
                             Player=="Rodrigo De Paul" |
                             Player=="Ángel Di María" |
                             Player=="Enzo Fernández")

selected_players[c(2,5),] <- selected_players[c(5,2),]
head(selected_players)

#####
# Step 9: Create the radar plots
#####
#attach the dataset
attach(selected_players)
#select the statistics we want to see and prepare for the plot
Sel <- data.frame("xG"=`Expected >> xG`,
                  "xAG"=`Expected >> xAG`,
                  "Pass"=`Passes >> Cmp`,
                  "Sh"=`Performance >> Sh`,
                  "SoT"=`Performance >> SoT`,
                  "KP"=`>> KP`)
Sel <- mutate_all(Sel, function(x) as.numeric(as.character(x)))
#run the radialprofile function with std=T, which standardizes the data so that the scale looks normal
p <- radialprofile(data=Sel, title=selected_players$Player, std=T)
detach(selected_players)

#####
# Step 10: Make the graph presentable
#####

# Your ggplot code
g <- grid.arrange(grobs = p[1:length(p)], ncol = 3)

# Create a custom legend using a separate ggplot
legend_data <- data.frame(
  label = c("xG = Expected Goals", "xAG = Expected Assisted Goals", "Pass = Completed Passes", 
            "Sh = Shots", "SoT = Shots on Target", "KP = Key Passes"),
  x = rep(1, 6), y = seq(0.8, 0.2, length.out = 6)
)

legend_plot <- ggplot(legend_data, aes(x, y, label = label)) +
  geom_text(size = 6) +
  theme_void()+
  theme(
    legend.spacing.y = unit(0.0001, "lines")  # Adjust this value to control spacing between legend items
  )

legend_plot
# Combine the plot and the custom legend using cowplot
combined_plot <- plot_grid(g, legend_plot, ncol = 2, rel_widths = c(3, 1))


g2 <- cowplot::ggdraw(combined_plot) +
  theme_minimal() +
  labs(
    title = "Argentine Players in 2022 World Cup",
    subtitle = expression(bold("Radar Plots ")* "of the " * bold("Argentine Players'")* " Stats. Values are standardized (μ=0, sd=1)"),
    caption = "Data Source: fbref.com"
  )

# Print or display the modified plot
print(g2)
ggsave("radar-key-final2.png", w = 15, h = 7.5, dpi = 600)


# Group E - Justice Table

Japan <- 0.49+2.06+1.18
Spain <- 2.98+0.94+1.54
Costa_Rica <- 0.01+0.64+0.05
Germany <- 2.37+1.77+2.93

Japan # 3
Spain # 2
Costa_Rica # 4
Germany # 1

# Probabilities - Group E # 

# Japan vs Germany (Japan Won)

j_g_t <- 3-(0.49+2.37) # Tie
j_g_jw <- (0.49 - j_g_t)/3 # Japan Win
j_g_gw <- 1- j_g_jw - j_g_t # Germany Win

# Spain vs Costa Rica (Spain Won)

s_cr_t <- 3-(2.98+0.01) # Tie 
s_cr_sw <- (2.98 - s_cr_t)/3 # Costa Rica win
s_cr_crw <- 0.01 

# Japan vs Costa Rica (Costa Rica Won)

j_cr_t <- 3-(2.06+0.64)
j_cr_jw <- (2.06 - j_cr_t)/3
j_cr_crw <- 1- j_cr_jw - j_cr_t

# Spain vs Germany (Tie)

s_g_t <- 3-(0.94+1.77)
s_g_sw <- (0.94 - s_g_t)/3
s_g_gw <- 1- s_g_sw - s_g_t

# Japan vs Spain (Japan Won)

j_s_t <- 3-(1.18+1.54)
j_s_jw <- (1.18 - j_s_t)/3
j_s_sw <- 1- j_s_jw - j_s_t

# Germany vs Costa Rica (Germany Won)

g_cr_t <- 3-(2.93+0.05)
g_cr_gw <- (2.93 - g_cr_t)/3
g_cr_crw <- 1- g_cr_gw - g_cr_t

# Probability of it is happening (0.001)
j_g_jw*s_cr_sw*j_cr_crw*s_g_t*j_s_jw*g_cr_gw

specific_result_count <- 0
# Monte Carlo Simulation Method 
for (i in 1:100000) {
  # Simulate outcomes for each match
  outcome_j_g <- sample(c("jw", "gw", "t"), size = 1, prob = c(j_g_jw, 1 - j_g_jw - j_g_t, j_g_t))
  outcome_s_cr <- sample(c("sw", "t", "cw"), size = 1, prob = c(s_cr_sw, 1 - s_cr_sw - s_cr_crw, s_cr_crw))
  outcome_j_cr <- sample(c("jw", "t", "cw"), size = 1, prob = c(j_cr_jw, 1 - j_cr_jw - j_cr_crw, j_cr_crw))
  outcome_s_g <- sample(c("sw", "gw", "t"), size = 1, prob = c(s_g_sw, g_cr_gw, s_g_t))
  outcome_j_s <- sample(c("jw", "t", "sw"), size = 1, prob = c(j_s_jw, 1 - j_s_jw - j_s_sw, j_s_sw))
  outcome_g_cr <- sample(c("gw", "t", "cw"), size = 1, prob = c(g_cr_gw, 1 - g_cr_gw - g_cr_crw, g_cr_crw))
  
  # Check if the specific result occurs
  if (outcome_j_g == "jw" && outcome_s_cr == "sw" && outcome_j_cr == "cw" &&
      outcome_s_g == "t" && outcome_j_s == "jw" && outcome_g_cr == "gw") {
    specific_result_count <- specific_result_count + 1
  }
}

# Print the estimated probability of the specific result occurring
estimated_probability <- specific_result_count / 100000
cat("Estimated probability of the specific result:", estimated_probability, "\n")



# Set the number of simulations
num_simulations <- 100000

# Initialize variables to count the outcomes
germany_lower_than_2nd_count <- 0
japan_at_top_count <- 0
specific_ranking_count <- 0

points <- c(japan = 0, germany = 0, spain = 0, costa_rica = 0)

# Iterate over the number of simulations
for (i in 1:num_simulations) {
  # Simulate outcomes for each match
  outcome_j_g <- sample(c("j_g_jw", "j_g_gw", "j_g_t"), size = 1, prob = c(j_g_jw, j_g_gw, j_g_t))
  outcome_s_cr <- sample(c("s_cr_sw", "s_cr_crw", "s_cr_t"), size = 1, prob = c(s_cr_sw, s_cr_crw, s_cr_t))
  outcome_j_cr <- sample(c("j_cr_jw", "j_cr_crw", "j_cr_t"), size = 1, prob = c(j_cr_jw, j_cr_crw, j_cr_t))
  outcome_s_g <- sample(c("s_g_sw", "s_g_gw", "s_g_t"), size = 1, prob = c(s_g_sw, s_g_gw, s_g_t))
  outcome_j_s <- sample(c("j_s_jw", "j_s_sw", "j_s_t"), size = 1, prob = c(j_s_jw, j_s_sw, j_s_t))
  outcome_g_cr <- sample(c("g_cr_gw", "g_cr_crw", "g_cr_t"), size = 1, prob = c(g_cr_gw, g_cr_crw, g_cr_t))
  
  # Calculate points for each team
  points["japan"] <- points["japan"] +
    sum(c(3, 1, 0)[match(outcome_j_g, c("j_g_jw", "j_g_t", "j_g_gw"))]) +
    sum(c(3, 1, 0)[match(outcome_j_cr, c("j_cr_jw", "j_cr_t", "j_cr_crw"))]) +
    sum(c(3, 1, 0)[match(outcome_j_s, c("j_s_jw", "j_s_t", "j_s_sw"))])
  
  points["germany"] <- points["germany"] +
    sum(c(0, 1, 3)[match(outcome_j_g, c("j_g_jw", "j_g_t", "j_g_gw"))]) +
    sum(c(3, 1, 0)[match(outcome_s_g, c("s_g_gw", "s_g_t", "s_g_sw"))]) +
    sum(c(3, 1, 0)[match(outcome_g_cr, c("g_cr_gw", "g_cr_t", "g_cr_crw"))])
  
  points["spain"] <- points["spain"] +
    sum(c(3, 1, 0)[match(outcome_s_cr, c("s_cr_sw", "s_cr_t", "s_cr_crw"))]) +
    sum(c(3, 1, 0)[match(outcome_s_g, c("s_g_sw", "s_g_t", "s_g_gw"))]) +
    sum(c(3, 1, 0)[match(outcome_j_s, c("j_s_sw", "j_s_t", "j_s_jw"))])
  
  points["costa_rica"] <- points["costa_rica"] +
    sum(c(0, 1, 3)[match(outcome_j_cr, c("j_cr_jw", "j_cr_t", "j_cr_crw"))]) +
    sum(c(3, 1, 0)[match(outcome_s_cr, c("s_cr_crw", "s_cr_t", "s_cr_sw"))]) +
    sum(c(3, 1, 0)[match(outcome_g_cr, c("g_cr_crw", "g_cr_t", "g_cr_gw"))])


  
  # Calculate rankings
  rankings <- rank(-points)  # -points for descending order
  
  # Check if Germany finishes lower than 2nd place
  if (rankings["germany"] > 2) {
    germany_lower_than_2nd_count <- germany_lower_than_2nd_count + 1
  }
  
  # Check if Japan finishes at the top
  if (rankings["japan"] == 1) {
    japan_at_top_count <- japan_at_top_count + 1
  }
  
  # Check if the specific ranking occurs (Japan 1st, Spain 2nd, Germany 3rd, Costa Rica 4th)
  if (rankings["japan"]==1 & rankings["germany"]==3 & rankings["spain"]==2 & rankings["costa_rica"]==4 ) {
    specific_ranking_count <- specific_ranking_count + 1
  }
  points <- c(japan = 0, germany = 0, spain = 0, costa_rica = 0)
}

# Calculate probabilities based on simulation results
germany_lower_than_2nd_probability <- germany_lower_than_2nd_count / num_simulations
japan_at_top_probability <- japan_at_top_count / num_simulations
specific_ranking_probability <- specific_ranking_count / num_simulations

# Print the results
cat("Probability that Germany finishes lower than 2nd place:", germany_lower_than_2nd_probability, "\n")
cat("Probability that Japan finishes at the top:", japan_at_top_probability, "\n")
cat("Probability of specific ranking (Japan 1st, Spain 2nd, Germany 3rd, Costa Rica 4th):", specific_ranking_probability, "\n")

