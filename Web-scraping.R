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

# Extract and store all the href attributes of 'a' elements on the page
links_1 <- unlist(page %>% html_nodes("a") %>% html_attr("href"))

# Display the first few elements of links_1
head(links_1)

# Split the URLs using double quotes
links_2 <- strsplit(links_1, '"')

# Display elements 100 to 110 of links_3 (Note: links_3 is not defined here)
links_3[100:110]

# Filter the URLs containing "matches" and store them in links_3 (Note: links_3 is not defined here)
links_3 <- links_2[grepl("matches", links_2)]

# Filter the URLs containing "FIFA" and store them in links_4 (Note: links_4 is not defined here)
links_4 <- links_3[grepl("FIFA", links_3)]

# Remove duplicate URLs and store them in all_urls
all_urls <- unique(links_4)

# Filter URLs containing "Argentina" and store them in team_urls
team_urls <- all_urls[grepl("Argentina", all_urls)]

# Create selected_urls by prepending "https://fbref.com" to team_urls
selected_urls <- paste("https://fbref.com", team_urls, sep = "")

# Display the first few elements of selected_urls
head(selected_urls)

# First just testing for one game, then for all games with for loop
g <- 1

# Calculate the character count of a specific URL and "-FIFA-World-Cup"
nchar("https://fbref.com/en/matches/109ad6ba/")
nchar("-FIFA-World-Cup")

# Extract game_data from selected_urls[g] based on character positions
game_data <- substr(selected_urls[g], 39, nchar(selected_urls[g]) - 15)

# Replace full month names with abbreviated month names in game_data
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

# Replace numeric month components with zero-padded versions in selected_urls
selected_urls <- str_replace(selected_urls, "-1-", "-01-")
selected_urls <- str_replace(selected_urls, "-2-", "-02-")
selected_urls <- str_replace(selected_urls, "-3-", "-03-")
selected_urls <- str_replace(selected_urls, "-4-", "-04-")
selected_urls <- str_replace(selected_urls, "-5-", "-05-")
selected_urls <- str_replace(selected_urls, "-6-", "-06-")
selected_urls <- str_replace(selected_urls, "-7-", "-07-")
selected_urls <- str_replace(selected_urls, "-8-", "-08-")
selected_urls <- str_replace(selected_urls, "-9-", "-09-")

# Extract date from game_data based on character positions
date <- substr(game_data, nchar(game_data) - 10, nchar(game_data))

# Extract teams from game_data based on character positions
teams <- substr(game_data, 1, nchar(game_data) - 12)

# Replace team names with their standard forms
teams <- str_replace(teams, "Saudi-Arabia", "Saudi Arabia")
teams <- str_replace(teams, "Costa-Rica", "Costa Rica")
teams <- str_replace(teams, "Korea-Republic", "Korea Republic")
teams <- str_replace(teams, "IR-Iran", "Iran")

# Extract teamA and teamB from teams
teamA <- sub("-.*", "", teams)
teamB <- sub(".*-", "", teams)

# Store the current URL in url
url <- selected_urls[g]

# Extract statistics for Team A from the webpage
statA <- curl::curl(url) %>%
  xml2::read_html() %>%
  rvest::html_nodes('table') %>%
  rvest::html_table() %>%
  .[[4]]

# Rename column names for Team A statistics
colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])
names(statA)[1:6] <- paste0(statA[1, 1:6])

# Remove the first row (header row) from statA
statA <- statA[-c(1),]

# Add date, Team, Opponent to the table for Team A and store it in statA
statA <- cbind(date, Team = teamA, Opponent = teamB, statA)

# Extract statistics for Team B from the webpage
statB <- curl::curl(url) %>%
  xml2::read_html() %>%
  rvest::html_nodes('table') %>%
  rvest::html_table() %>%
  .[[11]]

# Rename column names for Team B statistics
colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
names(statB)[1:6] <- paste0(statB[1, 1:6])

# Remove the first row (header row) from statB
statB <- statB[-c(1),]

# Add date, Team, Opponent to the table for Team B and store it in statB
statB <- cbind(date, Team = teamB, Opponent = teamA, statB)

# Combine statistics for both teams and store it in stat_both
stat_both <- rbind(statA, statB)

# Create selected_urls by prepending "https://fbref.com" to all_urls
selected_urls <- paste("https://fbref.com", all_urls, sep = "")
selected_urls

# Initialize variables all_stat and full_stat
all_stat <- NULL
full_stat <- NULL

# To try without for loop to test it
g <- 1
i <- 5

# Loop through selected_urls
for (g in 1:length(selected_urls)) {
  # Extract game_data from selected_urls[g] based on character positions
  game_data <- substr(selected_urls[g], 39, nchar(selected_urls[g]) - 15)

  # Replace full month names with abbreviated month names in game_data
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

  # Replace numeric month components with zero-padded versions in selected_urls
  selected_urls <- str_replace(selected_urls, "-1-", "-01-")
  selected_urls <- str_replace(selected_urls, "-2-", "-02-")
  selected_urls <- str_replace(selected_urls, "-3-", "-03-")
  selected_urls <- str_replace(selected_urls, "-4-", "-04-")
  selected_urls <- str_replace(selected_urls, "-5-", "-05-")
  selected_urls <- str_replace(selected_urls, "-6-", "-06-")
  selected_urls <- str_replace(selected_urls, "-7-", "-07-")
  selected_urls <- str_replace(selected_urls, "-8-", "-08-")
  selected_urls <- str_replace(selected_urls, "-9-", "-09-")

  # Extract date from game_data based on character positions
  date <- substr(game_data, nchar(game_data) - 10, nchar(game_data))

  # Extract teams from game_data based on character positions
  teams <- substr(game_data, 1, nchar(game_data) - 12)

  # Replace team names with their standard forms
  teams <- str_replace(teams, "Saudi-Arabia", "Saudi Arabia")
  teams <- str_replace(teams, "Costa-Rica", "Costa Rica")
  teams <- str_replace(teams, "Korea-Republic", "Korea Republic")
  teams <- str_replace(teams, "IR-Iran", "Iran")
  teams <- str_replace(teams, "United-States", "USA")

  # Extract teamA and teamB from teams
  teamA <- sub("-.*", "", teams)
  teamB <- sub(".*-", "", teams)

  # Store the current URL in url
  url <- selected_urls[g]

  # Extract statistics for Team A from the webpage
  statA <- curl::curl(url) %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() %>%
    .[[4]]

  # Rename column names for Team A statistics
  colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])
  names(statA)[1:6] <- paste0(statA[1, 1:6])

  # Remove the first row (header row) from statA
  statA <- statA[-c(1),]

  # Add date, Team, Opponent to the table for Team A and store it in statA
  statA <- cbind(date, Team = teamA, Opponent = teamB, statA)

  # Extract statistics for Team B from the webpage
  statB <- curl::curl(url) %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() %>%
    .[[11]]

  # Rename column names for Team B statistics
  colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
  names(statB)[1:6] <- paste0(statB[1, 1:6])

  # Remove the first row (header row) from statB
  statB <- statB[-c(1),]

  # Add date, Team, Opponent to the table for Team B and store it in statB
  statB <- cbind(date, Team = teamB, Opponent = teamA, statB)

  # Combine statistics for both teams and store it in stat_both
  stat_both <- rbind(statA, statB)

  # Initialize all_stat
  all_stat <- stat_both

  # Pause execution for 15 seconds
  Sys.sleep(15)

  # Loop through tables related to the game
  for (i in 5:10) {
    # Extract statistics for Team A from the webpage
    statA <- curl::curl(url) %>%
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[i]]

    # Rename column names for Team A statistics
    colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])
    names(statA)[1:6] <- paste0(statA[1, 1:6])

    # Remove the first row (header row) from statA
    statA <- statA[-c(1),]

    # Add date, Team, Opponent to the table for Team A and store it in statA
    statA <- cbind(date, Team = teamA, Opponent = teamB, statA)

    # Extract statistics for Team B from the webpage
    statB <- curl::curl(url) %>%
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[i + 7]]

    # Rename column names for Team B statistics
    colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
    names(statB)[1:6] <- paste0(statB[1, 1:6])

    # Remove the first row (header row) from statB
    statB <- statB[-c(1),]

    # Add date, Team, Opponent to the table for Team B and store it in statB
    statB <- cbind(date, Team = teamB, Opponent = teamA, statB)

    # Combine statistics for both teams and store it in stat_both
    all_stat <- merge(all_stat, stat_both, all = TRUE)

    # Remove any duplicates from all_stat
    all_stat <- unique(all_stat)

    # Remove any leading or trailing white spaces from Player column
    all_stat$Player <- str_trim(all_stat$Player, side = c("both", "left", "right"))

    # Convert all stats into numeric variables if the column name is "Pos"
    if (colnames(all_stat[7]) == "Pos") all_stat <- cbind(all_stat[, 1:8], mutate_all(all_stat[, 9:ncol(all_stat)], function(x) as.numeric(as.character(x))))

    # Write the data to a CSV file with a filename based on the game_data
    write.csv(all_stat, paste0("World_Cup_2022_", game_data, ".csv"))

    # Pause execution for 15 seconds
    Sys.sleep(15)
  }

  # Combine statistics for all games into full_stat
  full_stat <- rbind(full_stat, all_stat)
}
# To open to files into one data
file_names <- list.files(pattern = "World",full.names = T)

all_stat <- read_csv(file_names[1],show_col_types = F)

for (f in file_names[-1]) all_stat <- rbind(all_stat, read_csv(f, show_col_types = FALSE))

# To check Duplicates                                                                                    
length(unique(all_stat))
                                                                                      
# Redundant Column
all_stat[,1] <- NULL                                                                          
