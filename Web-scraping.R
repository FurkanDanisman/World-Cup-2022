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

# Web Scraping

# Read the HTML content of a webpage
page <- read_html("https://fbref.com/en/comps/1/schedule/World-Cup-Scores-and-Fixtures")

# Extract all href attributes from anchor tags on the webpage
links_1 <- unlist(page %>% html_nodes("a") %>% html_attr("href"))
head(links_1)

# Split the links using double quotes
links_2 <- strsplit(links_1, '"')
links_3[100:110]

# Filter links containing "matches"
links_3 <- links_2[grepl("matches", links_2)]

# Further filter links containing "FIFA"
links_4 <- links_3[grepl("FIFA", links_3)]

# Remove duplicate URLs
all_urls <- unique(links_4)

# Filter URLs containing "Argentina"
team_urls <- all_urls[grepl("Argentina", all_urls)]

# Create complete URLs
selected_urls <- paste("https://fbref.com", team_urls, sep = "")
head(selected_urls)

# Initialize variables
g <- 1
nchar("https://fbref.com/en/matches/109ad6ba/")
nchar("-FIFA-World-Cup")

# Extract game data from the selected URL
game_data <- substr(selected_urls[g], 39, nchar(selected_urls[g]) - 15)

# Replace full month names with abbreviations
game_data <- str_replace_all(game_data, month_replacements)

# Replace single-digit months with leading zeros
selected_urls <- str_replace_all(selected_urls, month_leading_zeros)

# Extract date from game data
date <- substr(game_data, nchar(game_data) - 10, nchar(game_data))

# Extract team names from game data
teams <- substr(game_data, 1, nchar(game_data) - 12)

# Replace team names with proper formatting
teams <- str_replace_all(teams, team_name_replacements)

# Split team names into two teams
teamA <- sub("-.*", "", teams)
teamB <- sub(".*-", "", teams)

# Get the URL for the current game
url <- selected_urls[g]

# Scrape table data for Team A
statA <- curl::curl(url) %>%
  xml2::read_html() %>%
  rvest::html_nodes('table') %>%
  rvest::html_table() %>%
  .[[4]]

# Rename column names for Team A statistics
colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])
names(statA)[1:6] <- paste0(statA[1, 1:6])

# Remove the first row (header row)
statA <- statA[-c(1),]

# Add date and team information to the table
statA <- cbind(date, Team = teamA, Opponent = teamB, statA)

# Scrape table data for Team B
statB <- curl::curl(url) %>%
  xml2::read_html() %>%
  rvest::html_nodes('table') %>%
  rvest::html_table() %>%
  .[[11]]

# Rename column names for Team B statistics
colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
names(statB)[1:6] <- paste0(statB[1, 1:6])

# Remove the first row (header row)
statB <- statB[-c(1),]

# Add date and team information to the table for Team B
statB <- cbind(date, Team = teamB, Opponent = teamA, statB)

# Combine statistics for both teams
stat_both <- rbind(statA, statB)

# Create a list of selected URLs
selected_urls <- paste("https://fbref.com", all_urls, sep = "")
selected_urls

# Initialize variables
all_stat <- NULL
full_stat <- NULL
g <- 1
i <- 5

# Loop through all selected URLs
for (g in 1:length(selected_urls)) {
  game_data <- substr(selected_urls[g], 39, nchar(selected_urls[g]) - 15)
  
  # Replace full month names with abbreviations
  game_data <- str_replace_all(game_data, month_replacements)
  
  # Replace single-digit months with leading zeros
  selected_urls <- str_replace_all(selected_urls, month_leading_zeros)
  
  # Extract date from game data
  date <- substr(game_data, nchar(game_data) - 10, nchar(game_data))
  
  # Extract team names from game data
  teams <- substr(game_data, 1, nchar(game_data) - 12)
  
  # Replace team names with proper formatting
  teams <- str_replace_all(teams, team_name_replacements)
  
  # Handle specific team name replacements
  teams <- str_replace(teams, "United-States", "USA")
  
  # Split team names into two teams
  teamA <- sub("-.*", "", teams)
  teamB <- sub(".*-", "", teams)
  
  # Get the URL for the current game
  url <- selected_urls[g]
  
  # Scrape table data for Team A
  statA <- curl::curl(url) %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() %>%
    .[[4]]
  
  # Rename column names for Team A statistics
  colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])
  names(statA)[1:6] <- paste0(statA[1, 1:6])
  
  # Remove the first row (header row)
  statA <- statA[-c(1),]
  
  # Add date and team information to the table for Team A
  statA <- cbind(date, Team = teamA, Opponent = teamB, statA)
  
  # Scrape table data for Team B
  statB <- curl::curl(url) %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() %>%
    .[[11]]
  
  # Rename column names for Team B statistics
  colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
  names(statB)[1:6] <- paste0(statB[1, 1:6])
  
  # Remove the first row (header row)
  statB <- statB[-c(1),]
  
  # Add date and team information to the table for Team B
  statB <- cbind(date, Team = teamB, Opponent = teamA, statB)
  
  # Combine statistics for both teams
  stat_both <- rbind(statA, statB)
  
  # Store the current game statistics in 'all_stat'
  all_stat <- stat_both
  
  # Pause execution for 15 seconds
  Sys.sleep(15)
  
  # Loop to scrape additional tables related to the game
  for(i in 5:10){
    # Scrape table data for Team A
    statA <- curl::curl(url) %>%
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[i]]
    
    # Rename column names for Team A statistics
    colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])
    names(statA)[1:6] <- paste0(statA[1, 1:6])
    
    # Remove the first row (header row)
    statA <- statA[-c(1),]
    
    # Add date and team information to the table for Team A
    statA <- cbind(date, Team = teamA, Opponent = teamB, statA)
    
    # Scrape table data for Team B
    statB <- curl::curl(url) %>%
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[i+7]]
    
    # Rename column names for Team B statistics
    colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
    names(statB)[1:6] <- paste0(statB[1, 1:6])
    
    # Remove the first row (header row)
    statB <- statB[-c(1),]
    
    # Add date and team information to the table for Team B
    statB <- cbind(date, Team = teamB, Opponent = teamA, statB)
    
    # Combine statistics for both teams
    stat_both <- rbind(statA, statB)
    
    # Merge current game statistics with 'all_stat' allowing duplicates
    all_stat <- merge(all_stat, stat_both, all = TRUE)
    
    # Remove any duplicate rows
    all_stat <- unique(all_stat)
    
    # Remove any leading or trailing white spaces in the "Player" column
    all_stat$Player <- str_trim(all_stat$Player, side = c("both", "left", "right"))
    
    # Convert all numeric columns to numeric data type
    if (colnames(all_stat[7]) == "Pos")
      all_stat <- cbind(all_stat[, 1:8], mutate_all(all_stat[, 9:ncol(all_stat)], function(x) as.numeric(as.character(x))))
    
    # Write the combined statistics to a CSV file with a specific format
    write.csv(all_stat, paste0("World_Cup_2022_", game_data, ".csv"))
    
    # Pause execution for 15 seconds
    Sys.sleep(15)
  }
  
  # Append current game statistics to 'full_stat'
  full_stat <- rbind(full_stat, all_stat)
}
