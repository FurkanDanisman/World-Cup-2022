# Libraries

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
library(flextable)

# Data Entry

file_names <- list.files(pattern = "World",full.names = T)

all_stat <- read_csv(file_names[1],show_col_types = F)


for (f in file_names[-1]) all_stat <- rbind(all_stat, read_csv(f, show_col_types = FALSE))

all_stat[,1] <- NULL

# Sum all stats for each player

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

# Readjusting the Percentage Values

all_stat_player$`Launched >> Cmp%` <- round(all_stat_player$`Launched >> Cmp`/all_stat_player$`Launched >> Att`,2)
all_stat_player$`Take-Ons >> Succ%` <- round(all_stat_player$`Take-Ons >> Succ`/all_stat_player$`Take-Ons >> Att`,2)
all_stat_player$`Take-Ons >> Tkld%` <-  round(all_stat_player$`Take-Ons >> Tkld`/all_stat_player$`Take-Ons >> Att`,2)
all_stat_player$`Shot Stopping >> Save%` <- round(all_stat_player$`Shot Stopping >> Saves`/all_stat_player$SoTA,2)
all_stat_player$`Crosses >> Stp%` <- round(all_stat_player$`Crosses >> Stp`/all_stat_player$`Crosses >> Opp`,2)
all_stat_player <- all_stat_player %>%
  mutate_at(vars(-Team), ~ifelse(is.na(.), 0, .))

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

# For First Table

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

Explanation_vector <- c("Goals scored","Include penalty kicks, but do not include penalty kickouts",
                        "Do not include penalty kicks",
                        "Do not include penalty kicks",
                        "The two offensive actions directly leading to a goal, such as passes, dribbles and drawing fouls. Note: A single player can receive credit for multiple actions and the shot-taker can also receive credit.",
                        "The two offensive actions directly leading to a shot, such as passes, dribbles and drawing fouls. Note: A single player can receive credit for multiple actions and the shot-taker can also receive credit.",
                        "Assists made",
                        "xG which follows a pass that assists a shot",
                        "Passes that directly lead to a shot",
                        "Completed passes that move the ball towards to opponent's goal line at least 10 yards from its furthest point in the last six passes, or any completed passes into the penalty area. Excludes passes from defending 40% of the pitch.",
                        "Carries that move the ball towards to opponent's goal line at least 10 yards from its furthest point in the last six passes. Excludes carries from defending 50% of the pitch.",
                        "Recieving completed passes that move the ball towards to opponent's goal line at least 10 yards from its furthest point in the last six passes, or any completed passes into the penalty area. Excludes passes from defending 40% of the pitch.",
                        "Total distance, in yards, a player moved the ball while controling with their feet towards to opponent's goal",
                        "Completed passes that is longer than 30 yards",
                        "Completed passes that enters the 1/3 of the pitch closest to the goal.",
                        "Carries that enters the 1/3 of the pitch closest to the goal.",
                        "Number of times tackled by a defender during a take-on attempt",
                        "Percentage of times tackled by a defender during a take-on attempt",
                        "Percentage of take-ons completed successfully. Unsuccessful take-ons include attempts where the dribbler retained its possession but was unable to get past the defender.",
                        "Total distance in yards player moved the ball while controlling with their feet, in any direction",
                        "Number of defenders taken on successfully, by dribbling past them.",
                        "Number of times blocking the ball by standing in its path",
                        "Number of players tackled plus number of interceptions")

stats_describtion <- data.frame("Stat"=columns_to_name,"Description"=Explanation_vector)

# Table Function from Github by Flextable

theme_zebra <- function(x, odd_header = "#CFCFCF", odd_body = "#EFEFEF",
                        even_header = "transparent", even_body = "transparent") {
  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.", "theme_zebra()"))
  }
  
  h_nrow <- nrow_part(x, "header")
  f_nrow <- nrow_part(x, "footer")
  b_nrow <- nrow_part(x, "body")
  
  x <- border_remove(x)
  x <- align(x = x, align = "center", part = "header")
  
  if (h_nrow > 0) {
    even <- seq_len(h_nrow) %% 2 == 0
    odd <- !even
    
    x <- bg(x = x, i = odd, bg = odd_header, part = "header")
    x <- bg(x = x, i = even, bg = even_header, part = "header")
    x <- bold(x = x, bold = TRUE, part = "header")
  }
  if (f_nrow > 0) {
    even <- seq_len(f_nrow) %% 2 == 0
    odd <- !even
    
    x <- bg(x = x, i = odd, bg = odd_header, part = "footer")
    x <- bg(x = x, i = even, bg = even_header, part = "footer")
    x <- bold(x = x, bold = TRUE, part = "footer")
  }
  if (b_nrow > 0) {
    even <- seq_len(b_nrow) %% 2 == 0
    odd <- !even
    
    x <- bg(x = x, i = odd, bg = odd_body, part = "body")
    x <- bg(x = x, i = even, bg = even_body, part = "body")
  }
  x <- align_text_col(x, align = "left", header = TRUE)
  x <- align_nottext_col(x, align = "right", header = TRUE)
  
  x
}

table <- flextable(stats_describtion,cwidth = 5,cheight = 100000)
theme_zebra(table)
ggsave("Stat Explanation.png", w = 16.5, h =8.5, dpi = 1000)

# Adding Rank System

for (i in 1:nrow(all_stat_player)){
  # Loop through the columns to compare
  for (col_name in columns_to_compare) {
    for (x in 1:3) {
      max_col_value <- sort(all_stat_player[col_name],decreasing = T)[x,1]
      if (all_stat_player[i,col_name] == max_col_value) {
        all_stat_player[i,paste0("Rank_",as.character(x))] <- all_stat_player[i,paste0("Rank_",as.character(x))] + 1 
      }
    }
  }
}

# Selecting Players that I am interested in Analyzing
selected_players <- c("Luka Modrić", "Lionel Messi", "Kylian Mbappé", "Bruno Fernandes", "Harry Kane", "Antoine Griezmann", "Ivan Perišić", "Achraf Hakimi", "Julián Álvarez")

filtered_data <- all_stat_player %>%
  filter(Player %in% selected_players)

filtered_data <- filtered_data %>%
  mutate_all(~ifelse(is.na(.), 0, .))


# Sort the data by Rank_1 in descending order
filtered_data <- filtered_data[order(filtered_data$Rank_1),]
filtered_data$color_1 <- ifelse(seq_along(filtered_data$Rank_1) >= nrow(filtered_data)-2, "blue", "grey")

# First Plot
a <- ggplot(filtered_data, aes(x = Rank_1, y = reorder(Player, Rank_1), fill = color_1)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    axis.title.x = element_text(size=15),
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),# Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 15,color = ifelse(filtered_data$Player %in% filtered_data[(nrow(filtered_data)-2):nrow(filtered_data),"Player"]$Player,"black","grey"))
  ) +
  labs(
    title = "Top Players in 2022 WORLD CUP",size=6,
    subtitle = expression("Number of Stats That "  * bold("Players") * " Ranked " * bold("First")),
    caption = "Data source: fbref.com"
    ,size=5) +
  xlab("Ranked First") +
  ylab("")+
  scale_x_cut(c(1,2,3,4,5,6,7,8),scales = 1)
a
ggsave("Ranked First.png", w = 16.5, h = 8.5, dpi = 600)

# Sort the data by Rank_2 in descending order
filtered_data <- filtered_data[order(filtered_data$Rank_2),]

# Create a new column to specify the bar color
filtered_data$color_2 <- ifelse(seq_along(filtered_data$Rank_2) >= nrow(filtered_data) - 1, "blue", "grey")

# Create the plot for Rank_2

b <- ggplot(filtered_data, aes(x = Rank_2, y = reorder(Player, Rank_2), fill = color_2)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  scale_x_cut(c(1, 2, 3, 4, 5, 6, 7, 8), scales = 1) +
  theme(
    axis.title.x = element_text(size=15),  # Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 15,color = ifelse(filtered_data$Player %in% filtered_data[(nrow(filtered_data)-1):nrow(filtered_data),"Player"]$Player,"black","grey"))
  ) +
  labs(
    title = "Top Players in 2022 WORLD CUP",
    subtitle = expression("Number of Stats That "  * bold("Players") * " Ranked " * bold("Second")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Ranked Second") +
  ylab("")
b
ggsave("Ranked Second.png", w = 16.5, h = 8.5, dpi = 600)
# Sort the data by Rank_3 in descending order
filtered_data <- filtered_data[order(filtered_data$Rank_3),]

# Create a new column to specify the bar color
filtered_data$color_3 <- ifelse(seq_along(filtered_data$Rank_3) >= nrow(filtered_data) - 1, "blue", "grey")

# Create the plot for Rank_3
c <- ggplot(filtered_data, aes(x = Rank_3, y = reorder(Player, Rank_3), fill = color_3)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    axis.title.x = element_text(size=15),  # Hide x-axis labels and values but keep the title
    axis.text.x = element_blank(),
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 15,color = ifelse(filtered_data$Player %in% filtered_data[(nrow(filtered_data)-1):nrow(filtered_data),"Player"]$Player,"black","grey"))
  ) +
  labs(
    title = "Top Players in 2022 WORLD CUP",
    subtitle = expression("Number of Stats That "  * bold("Players") * " Ranked " * bold("Third")),
    caption = "Data source: fbref.com"
  ) +
  xlab("Ranked Third") +
  ylab("")+
  scale_x_cut(breaks = c(1,2,3),scales = 1.2/8,which = c(1,2,3))
c
ggsave("Ranked Third.png", w = 16.5, h = 8.5, dpi = 600)

# To further analyze where they were ranked for each stat / Argentina #

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

# Argentina Plots #

# Messi's Plot

Messi_filtered <- Messi_stat[order(Messi_stat$Rank),]
Messi_filtered[16,3] <- "2k" # Converting big numbers 
Messi_filtered[19,3] <- "1k" 
messi_rank <- ggplot(Messi_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3",size=12) +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size=12),
    axis.title.x = element_text(size=15),# Hide x-axis labels and values but keep the title
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),
    axis.text.y = element_text(size = 15,color = "azure4")
  ) +
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
            vjust = 0.2, hjust = -0.24,size=5)
ggsave("Messi Rank1.png", w = 16.5, h = 8.5, dpi = 600)

# Alexis Mac Allister's Plot

Alexis_Mac_Allister_filtered <- Alexis_Mac_Allister_stat[order(Alexis_Mac_Allister_stat$Rank),]
Alexis_Mac_Allister_filtered[12,3] <- "1k"
ama_rank <- ggplot(Alexis_Mac_Allister_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size=12),
    axis.title.x = element_text(size=15),# Hide x-axis labels and values but keep the title
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),
    axis.text.y = element_text(size = 15,color = "azure4")
  ) +
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
            vjust = 0.2, hjust = -0.24,size=5)
ggsave("Alexis Mac Allister Rank.png", w = 16.5, h = 8.5, dpi = 600)



# To further analyze where they were ranked for each stat / France #

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

# France Ranking Plots #

# Kylian Mbappé

Kylian_Mbappé_filtered <- Kylian_Mbappé_stat[order(Kylian_Mbappé_stat$Rank),]
Kylian_Mbappé_filtered[15,3] <- "2k"
Kylian_Mbappé_rank <- ggplot(Kylian_Mbappé_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size=12),
    axis.title.x = element_text(size=15),# Hide x-axis labels and values but keep the title
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),
    axis.text.y = element_text(size = 15,color = "azure4")
  ) +
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
            vjust = 0.2, hjust = -0.24,size=5)
ggsave("Mbappe.png", w = 16.5, h = 8.5, dpi = 600)

# Antoine_Griezmann
Antoine_Griezmann_filtered <- Antoine_Griezmann_stat[order(Antoine_Griezmann_stat$Rank),]
Antoine_Griezmann_filtered[12,3] <- "1k"
Antoine_Griezmann_rank <- ggplot(Antoine_Griezmann_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size=12),
    axis.title.x = element_text(size=15),# Hide x-axis labels and values but keep the title
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),
    axis.text.y = element_text(size = 15,color = "azure4")
  ) +
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
            vjust = 0.2, hjust = -0.24,size=5)
ggsave("Griezmann Rank.png", w = 16.5, h = 8.5, dpi = 600)

# To further analyze where they were ranked for each stat / Croatia #

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

# Croatia Ranking Plots #

# Luka Modrić

Luka_Modrić_filtered <- Luka_Modrić_stat[order(Luka_Modrić_stat$Rank),]
Luka_Modrić_filtered[3,3] <- "2k"
Luka_Modrić_filtered[6,3] <- "3k"

Luka_Modrić_rank <- ggplot(Luka_Modrić_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size=12),
    axis.title.x = element_text(size=15),# Hide x-axis labels and values but keep the title
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),
    axis.text.y = element_text(size = 15,color = "azure4")
  ) +
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
            vjust = 0.2, hjust = -0.24,size=5)
ggsave("Modric Rank.png", w = 16.5, h = 8.5, dpi = 600)

# Ivan Perišić

Ivan_Perišić_filtered <- Ivan_Perišić_stat[order(Ivan_Perišić_stat$Rank),]
Ivan_Perišić_filtered[14,3] <- "1k"
Ivan_Perišić_rank <- ggplot(Ivan_Perišić_filtered, aes(x = Rank, y = reorder(Stat,-Rank))) +
  geom_point(stat = "identity",pch=22,cex=10,color="blue",fill="blue3") +
  geom_text(aes(label = Value), vjust = 0.3, hjust = 0.5, size = 4,color="azure2") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size=12),
    axis.title.x = element_text(size=15),# Hide x-axis labels and values but keep the title
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=20),
    plot.caption = element_text(size=12),
    axis.text.y = element_text(size = 15,color = "azure4")
  ) +
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
            vjust = 0.2, hjust = -0.24,size=5)
ggsave("Perisic Rank.png", w = 16.5, h = 8.5, dpi = 600)

