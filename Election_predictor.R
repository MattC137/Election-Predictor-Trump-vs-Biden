library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(SuppDists)

setwd("~/workspace/Election Predictor")

#### Electoral Votes

votes_map_html <- read_html("https://www.realclearpolitics.com/epolls/2020/president/2020_elections_electoral_college_map.html")

Toss_ups <- votes_map_html %>% 
  str_extract_all('(?<=<span class="full">)(.*?)(?=</span>)') %>% 
  unlist()

Solid_States <- votes_map_html %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

Electoral_Votes <- c(Toss_ups, 
                    Solid_States[[16]][[1]],
                    Solid_States[[16]][[2]],
                    Solid_States[[17]][[1]],
                    Solid_States[[17]][[2]])

Electoral_Votes <- Electoral_Votes[str_detect(Electoral_Votes, " ")]

Electoral_Votes <- Electoral_Votes %>% str_replace_all('\\)', '')

Electoral_Votes <- str_split_fixed(Electoral_Votes, " \\(", 2) %>% 
  as.data.frame()

names(Electoral_Votes) <- c("State", "Votes")

Electoral_Votes$Votes <- as.numeric(
  as.character(Electoral_Votes$Votes)
  ) 

rm(votes_map_html, Toss_ups, Solid_States)

# sum(Electoral_Votes$Votes, na.rm = T)

#### Getting the 2020 Links and Data

Summary_2020 <- read_html(
  "https://www.realclearpolitics.com/epolls/2020/president/2020_elections_electoral_college_map.html"
  )

Summary_2020 <- Summary_2020 %>% 
  str_extract_all('(?<=href=")(.*?)(?=">)') %>% 
  unlist() %>% 
  unique()

Summary_2020 <- Summary_2020[str_detect(Summary_2020, 
                                        "trump_vs_biden")]

Summary_2020 <- Summary_2020 %>% 
  str_replace_all("/epolls/2020/president/", "") %>% 
  str_replace_all(".html", "") %>% 
  str_replace_all("-", "/") %>% 
  as.data.frame()

names(Summary_2020) <- "Link"

Summary_2020 <- str_split_fixed(Summary_2020$Link, "/", 3) %>% 
  as.data.frame()

names(Summary_2020) <- c("Abbrev", "state_id", "id")

Summary_2020 <- Summary_2020 %>% mutate(
  State = state_id %>% 
    str_replace_all("_trump", "") %>% 
    str_replace_all("_vs", "") %>% 
    str_replace_all("_biden", "") %>% 
    str_replace_all("_jorgensen", "") %>% 
    str_replace_all("_hawkins", "") %>%
    str_replace_all("_", " ") %>% 
    str_to_title() %>%
    str_replace_all("Cd", "CD"),
  Link = paste0("https://www.realclearpolitics.com/",
                "epolls/2020/president/", 
                Abbrev, "/", state_id, "-", id, ".html")
) %>% arrange(State)

State_Polls_2020_PA <- read_html(
  "https://www.realclearpolitics.com/epolls/2020/president/pa/pennsylvania_trump_vs_biden-6861.html") %>%
  html_nodes("table") %>%
  html_table()

State_Polls_2020_PA[[4]]

rcp_average <- State_Polls_2020_PA[[4]][1,1]

State_Polls_2020 <- as.data.frame(matrix(NA, nrow = 0, ncol = 7))
names(State_Polls_2020) <- c("Poll", "Date", "Sample", 
                             "Biden (D)", "Trump (R)", 
                             "State", "Rank")

for(i in 1:nrow(Summary_2020)){
  link <- Summary_2020$Link[i]
  state <- Summary_2020$State[i]
  print(state)
  
  state_polls <- read_html(link) %>% 
    html_nodes("table") %>% 
    html_table()
  
  max <- length(state_polls)
  
  if(max > 0){
    state_polls <- state_polls[[max]] %>% 
      select(Poll, Date, Sample, `Biden (D)`, `Trump (R)`)
    state_polls$State <- state
    state_polls <- state_polls %>% filter(Poll != rcp_average)
    state_polls$Rank <- 1:nrow(state_polls)
    State_Polls_2020 <- rbind(State_Polls_2020, state_polls)
  }
}

State_Polls_2020 <- State_Polls_2020 %>% 
  rename("Biden" = `Biden (D)`, "Trump" = `Trump (R)`) %>% 
  mutate(Spread = Trump - Biden)

RCP_Averages_2020 <- State_Polls_2020 %>% 
  filter(Poll == rcp_average)

State_Polls_2020 <- State_Polls_2020 %>% 
  filter(Poll != rcp_average)

# Calculate the average of the last 5 state polls
State_Summary_2020 <- State_Polls_2020 %>% 
  filter(Rank <= 5) %>% 
  group_by(State) %>% 
  summarize(
    Spread_2020 = mean(Spread)
    )

# Calculate the SD for all state polls
SD_2020 <- State_Polls_2020 %>% 
  group_by(State) %>% 
  summarize(
    Stdev_2020 = sd(Spread)
    )

# Combine Averages and Standard Deviation
State_Summary_2020 <- State_Summary_2020 %>% 
  left_join(SD_2020)

rm(SD_2020, state_polls, State_Polls_2020_PA, Summary_2020)

#### 2020 National Polls

National_2020 <- read_html(
  "https://www.realclearpolitics.com/epolls/2020/president/us/general_election_trump_vs_biden-6247.html") %>%
  html_nodes("table") %>%
  html_table()

National_2020 <- National_2020[[4]]

National_2020 <- National_2020 %>% 
  rename("Biden" = `Biden (D)`, "Trump" = `Trump (R)`) %>% 
  mutate(Spread = Trump - Biden)

National_Spread_2020_Current <- as.numeric(National_2020[1, 7])
National_Sd_2020 <- sd(National_2020$Spread[-1])

rm(National_2020)

#### 2016 State and National Data

# UPDATE ALL THE VARIABLES WITH 2020 TO 2016

# UPDATE THE 2016 ELECTORAL COLLEGE MAP LINK
Summary_2016 <- read_html(
  "https://www.realclearpolitics.com/epolls/2016/president/2016_elections_electoral_college_map.html"
)

Summary_2016 <- Summary_2016 %>% 
  str_extract_all('(?<=href=")(.*?)(?=">)') %>% 
  unlist() %>% 
  unique()

# UPDATE TO trump_vs_clinton
Summary_2016 <- Summary_2016[str_detect(Summary_2016, 
                                        "trump_vs_clinton")]
# UPDATE /epolls/2016/president/
Summary_2016 <- Summary_2016 %>% 
  str_replace_all("/epolls/2016/president/", "") %>% 
  str_replace_all(".html", "") %>% 
  str_replace_all("-", "/") %>% 
  as.data.frame()

names(Summary_2016) <- "Link"

Summary_2016 <- str_split_fixed(Summary_2016$Link, "/", 3) %>% 
  as.data.frame()

names(Summary_2016) <- c("Abbrev", "state_id", "id")

# UPDATE THE CANDIDATES CLINTON, JOHNSON, STEIN, MCMULLIN
Summary_2016 <- Summary_2016 %>% mutate(
  State = state_id %>% 
    str_replace_all("_trump", "") %>% 
    str_replace_all("_vs", "") %>% 
    str_replace_all("_clinton", "") %>% 
    str_replace_all("_johnson", "") %>% 
    str_replace_all("_stein", "") %>% 
    str_replace_all("_mcmullin", "") %>% 
    str_replace_all("_", " ") %>% 
    str_to_title() %>%
    str_replace_all("Cd", "CD"),
  Link = paste0("https://www.realclearpolitics.com/",
                "epolls/2016/president/", 
                Abbrev, "/", state_id, "-", id, ".html")
) %>% arrange(State)

# UPDATE FOR CLINTON
State_Polls_2016 <- as.data.frame(matrix(NA, nrow = 0, ncol = 7))
names(State_Polls_2016) <- c("Poll", "Date", "Sample", 
                             "Clinton (D)", "Trump (R)", 
                             "State", "Rank")

for(i in 1:nrow(Summary_2016)){
  link <- Summary_2016$Link[i]
  state <- Summary_2016$State[i]
  print(state)
  
  state_polls <- read_html(link) %>% 
    html_nodes("table") %>% 
    html_table()
  
  max <- length(state_polls)
  
  if(max > 0){
    
    # UPDATE TO CLINTON
    state_polls <- state_polls[[max]] %>% 
      select(Poll, Date, Sample, `Clinton (D)`, `Trump (R)`)
    
    state_polls$State <- state
    state_polls$Rank <- 1:nrow(state_polls)
    State_Polls_2016 <- rbind(State_Polls_2016, state_polls)
  }
}

# UPDATE TO CLINTON
State_Polls_2016 <- State_Polls_2016 %>% 
  rename("Clinton" = `Clinton (D)`, "Trump" = `Trump (R)`) %>% 
  mutate(Spread = Trump - Clinton)

# UPDATE: Take the final results instead of the average
RCP_Finals_2016 <- State_Polls_2016 %>% 
  filter(Poll == "Final Results")

State_Polls_2016 <- State_Polls_2016 %>% 
  filter(Poll != "Final Results", Poll != rcp_average)

# Calculate the SD for all state polls
SD_2016 <- State_Polls_2016 %>% 
  group_by(State) %>% 
  summarize(
    Stdev_2016 = sd(Spread)
  )

# Combine Averages and Standard Deviation
State_Summary_2016 <- RCP_Finals_2016 %>% 
  left_join(SD_2016) %>% 
  select(State, Spread_2016 = Spread, Stdev_2016)

rm(SD_2016, state_polls, Summary_2016)

#### 2016 National Polls

# UPDATE THE GENERAL ELECTION LINK
National_2016 <- read_html(
  "https://www.realclearpolitics.com/epolls/2016/president/us/general_election_trump_vs_clinton-5491.html") %>%
  html_nodes("table") %>%
  html_table()

National_2016 <- National_2016[[4]]

# UPDATE FOR CLINTON
National_2016 <- National_2016 %>% 
  rename("Clinton" = `Clinton (D)`, "Trump" = `Trump (R)`) %>% 
  mutate(Spread = Trump - Clinton)

National_Spread_2016_Average <- as.numeric(National_2016[2, 7])
National_Sd_2016 <- sd(National_2016$Spread[-1])

rm(National_2016)

#### Build the forecast model

dataset <- Electoral_Votes %>% 
  left_join(State_Summary_2016) %>% 
  left_join(State_Summary_2020)

dataset <- dataset %>% mutate(
  National_Adj = National_Spread_2020_Current - National_Spread_2016_Average,
  National_SD = National_Sd_2020,
  Spread = case_when(
    is.na(Spread_2020) ~ Spread_2016 + National_Adj,
    !is.na(Spread_2020) ~ (0.5*Spread_2020) + (0.5*(Spread_2016+National_Adj))
  ),
  Sd = case_when(
    is.na(Stdev_2016) & (is.na(Stdev_2020)|is.nan(Stdev_2020)) ~ National_SD,
    !is.na(Stdev_2016) & (is.na(Stdev_2020)|is.nan(Stdev_2020)) ~ Stdev_2016,
    !is.na(Stdev_2016) & !(is.na(Stdev_2020)|is.nan(Stdev_2020)) ~ (0.5*Stdev_2020) + (0.5*Stdev_2016)
  )
)

n = 10000

results_matrix <- matrix(0, nrow = 54, ncol = n)

Dist <- list(
  gamma = 0,
  delta = 0.5,
  xi = 0.01,
  lambda = 1,
  type = "SN"
)

dist_multiplier <- rJohnson(n, Dist)

for(i in 1:54){
  results_matrix[i, ] <- dataset[i, 9] + dist_multiplier*dataset[i, 10]
}

trump_wins <- ifelse(results_matrix > 0, 1, 0)
trump_state_probs <- apply(trump_wins, 1, sum)/n
dataset$Trump_Prob <- trump_state_probs

biden_wins <- ifelse(results_matrix < 0, 1, 0)
biden_state_probs <- apply(biden_wins, 1, sum)/n
dataset$Biden_Prob <- biden_state_probs

votes <- matrix(Electoral_Votes$Votes, ncol = 1)

for(i in 1:10000){
  trump_wins[ ,i] <- trump_wins[ ,i] * votes
  biden_wins[ ,i] <- biden_wins[ ,i] * votes
}

trump_votes <- apply(trump_wins, 2, sum)
biden_votes <- apply(biden_wins, 2, sum)

results <- data.frame(trump_votes, biden_votes)

results <- results %>% mutate(
  winner = ifelse(trump_votes >= biden_votes, "Trump", "Biden")
)

win_table <- results %>% 
  group_by(winner) %>% 
  tally() %>% 
  rename(Percent = n) %>% 
  mutate(Percent = Percent/n)



results %>% 
  group_by(winner) %>% 
  tally() %>% 
  rename(Percent = n) %>%   
  mutate(Percent = scales::percent(Percent/n, accuracy = 1.0))
