

library(rvest)
library(stringr)
library(tidyverse)


###################################
#          Extract Data           #
###################################

#### Functions for extracting tables  ####

extract_tables <- function(x){
  z <- html_attr(x, "style") == "width:100%;"
  return(x[z])
}

bind_tables <- function(x){
  tabs <- html_table(x)
  tab_master <- do.call(rbind, tabs)
  return(tab_master)
}


#### Pull Data from web page ####

urls <- paste0("https://web.enrboenyc.us/rcv/024306_", 1:8, ".html")


tibs  <- urls%>% 
  lapply(read_html) %>% 
  lapply(function(x) html_nodes(x, "table")) %>% 
  lapply(extract_tables) %>% 
  lapply(bind_tables)

# Add indicator for round
## Note: Not sure if there is better way than for loop to do this
for(i in 1:length(tibs)){
  tibs[[i]]$round <- i
}



###################################
#          Full Dataset           #
###################################


#### Clean Data ####

# 1) Bind rows in tibs list
# 2) Rename columns
# 3) Split results into two columns (n votes and vote share)
# 4) Add indicator for eliminated candidates

nyc_mayor_rcv_full <- tibs %>%  
                      bind_rows %>% 
                      rename(candidate = X1, 
                             result = X2)  %>% 
                      separate(result, into = c('votes', 'share'), sep = " ",
                               remove = FALSE, fill ="left", extra = "drop") %>% 
                      mutate(share = str_extract(share, "\\d+\\.\\d+"),
                             elim = as.integer(if_else(result == "eliminated", 1, 0)),
                             votes  = as.integer(votes),
                             share = as.numeric(share))

#### Find Rounds in which Candidates Eliminated #####

# Note: Not Sure if I need this
elim <- nyc_mayor_rcv_full %>% 
            filter(elim == 1 )  %>% 
            rename(elim_candidate= candidate) %>% 
            group_by(elim_candidate) %>% 
            summarize(elim_rnd = min(round)) %>% 
            arrange(elim_rnd) 
elim



#### Remove Eliminated Candidates  #####

# Remove Eliminated Candidates from Subsequent Rounds
## Note: Do I want to keep vote share and elimination variables
nyc_mayor_rcv <- nyc_mayor_rcv_full %>% 
                      filter(!is.na(votes)) %>%
                      select(candidate, votes, round, share, elim)




#### Count Remaining Candidates by Round #####

nyc_mayor_rcv <- nyc_mayor_rcv %>% 
                     add_count( round, name = "n_cand")

#### Check Dataset and Output ####

#View each round as separate tibble
nyc_mayor_rcv  %>% 
  split(nyc_mayor_rcv$round)

# Check classes
nyc_mayor_rcv  %>% glimpse

# Output dataset as csv
write.csv(nyc_mayor_rcv, file = "data/nyc_mayor_rcv.csv")

###################################
#       Network Dataset          #
###################################


#### Calculate Redistributed Votes ####

# 1) Subset rounds 6 through 8
# 2) Create lag variable of votes from previous round
# 3) Drop Round 6 rows (i.e. where vote_prev_rnd will be missing)
# 4) Calculate previous round and vote difference from previous round
# 5) For now, dropping share and elim variables 

## Note: previous round needed for Flourish Sankey, but maybe not for networkD3 sankey
final_rnds <- nyc_mayor_rcv  %>% 
                  filter(round >= 6) %>% 
                  arrange(candidate, round) %>% 
                  group_by(candidate) %>% 
                  mutate(vote_prev_rnd = lag(votes)) %>% 
                  ungroup() %>% 
                  filter(!is.na(vote_prev_rnd )) %>% 
                  mutate(vote_chg = votes - vote_prev_rnd,
                         prev_round = round -1) %>% 
                  select(-share, -elim)
final_rnds

## NoteL Might be useful to calculate number of candidates from previous round

#### Add Elimininated Candidates to Subsequent Rounds ####

# Merge with elim dataset to add previous candidate (i.e. candidate eliminated from previous round) 
# Now each row shows how votes were redistributed in each round and from which eliminated candidates
final_rnds_elim <- final_rnds %>% 
                  left_join(elim, by = c("round"= "elim_rnd")) %>%
                  rename(prev_candidate = elim_candidate) %>% 
                  select(prev_candidate, candidate, vote_chg, round,
                         votes, vote_prev_rnd, everything())

final_rnds_elim 



#### Split Data frame #####

# Now we can define two
# 1) choice_same: uses vote from previous round (i.e. voters whose votes weren't eliminated in previous round)
# 2) choice_diff: use vote_chng (i.e. next choice of voters whose candidate was eliminated in previous round)s

choice_same <- final_rnds_elim %>% 
                  mutate(prev_candidate = candidate, 
                                  vote = vote_prev_rnd,
                                  vote_rnd_change = 0) %>% 
                  select(prev_candidate, candidate, vote, prev_round, round, n_cand, vote_rnd_change)
choice_same

choice_diff <- final_rnds_elim %>% 
                    mutate(vote = vote_chg,
                           vote_rnd_change = 1 ) %>% 
                    select(prev_candidate, candidate, vote, prev_round, round, n_cand, vote_rnd_change)
choice_diff


#### Combine Datasets ####

# 1) Append datsets rowwise
# 2) Calculate source and target by converting prev_candidate, candidate to factor then integer

# Need this to Order Dataset and then vertical position of nodes
## Note: these only apply to rounds 6 through 8 
cand_levels <- c('Eric L. Adams', 'Kathryn A. Garcia',
                 'Maya D. Wiley', 'Andrew Yang', 'Inactive ballots')


rcv <- choice_same %>% 
            rbind(choice_diff) %>% 
            mutate(source_old = factor(prev_candidate, levels = cand_levels),
                   target_old = factor(candidate, levels = cand_levels),
                   source_old = as.integer(source_old) - 1 ,
                   target_old = as.integer(target_old) - 1 ) %>% 
            arrange(prev_round)

rcv

#### Create Links Dataset ####

# Note: This is messy should try to find general formula
# Note: Because Inactive Ballots is lowest node but is not eliminated, the calculation is complicated
# Note: Could create two different factors levels;  One by elimination and one by order of nodes 

rcv_links1 <- rcv %>%  
      arrange(round, source_old, target_old) %>%        
      mutate(source = case_when(round == 7 ~ source_old,
                                    round == 8 & prev_candidate != "Inactive ballots" ~ source_old + n_cand + 2,
                                    round == 8 & prev_candidate == "Inactive ballots" ~ 8),
            target = case_when(round == 7 & candidate != "Inactive ballots" ~ target_old + n_cand +1 ,
                                   round == 7 & candidate == "Inactive ballots" ~ 8,
                                   round == 8 & candidate != "Inactive ballots" ~ target_old +(n_cand + 1)*2 + 1,
                                   round == 8 & candidate == "Inactive ballots" ~ 11)) 
rcv_links1

# 1) Create two different link groups (depending on which color to use)
# 2) Paste round variables to candidate variables to create unique names
# 3) Convert from tibble to data.frame
rcv_links <- rcv_links1 %>% 
                      mutate(linkGroup0= prev_candidate,
                             linkGroup1 = candidate,
                             prev_candidate = paste(prev_round, prev_candidate),
                             candidate = paste(round, candidate)) %>% 
                        select(prev_candidate, candidate, source, target, vote, linkGroup0, linkGroup1) %>% 
                        as.data.frame
rcv_links
   



#### Create nodes dataset ####

# Create node ID
nodeID <- c(rcv_links$prev_candidate, rcv_links$candidate) %>%  unique %>%  sort
nodeID

# Create node groups to keep same color across rounds
nodeGroup <- factor(str_sub(nodeID, 3), levels = cand_levels)
nodeGroup
nodeRound <- as.integer(str_sub(nodeID, end = 1))
nodeRound

# 1) Combine variables into dataset
# 2) Order by nodeRound and then by factor levels of nodeGroup
rcv_nodes <- data.frame(nodeID, nodeGroup, nodeRound) %>% 
                    arrange(nodeRound, nodeGroup) %>% 
            mutate(nodeGroup = as.character(nodeGroup),
                   nodeNum= row_number() - 1)
rcv_nodes

rcv_links %>%    select(prev_candidate, candidate, source, target)
rcv_nodes

# Note: - Should create rcv_nodes before numbering nodes in rcv_links
#       - Then can merge by prev_candidate for source and candidate for target


#### Output Sankey Network Datasets ####
write.csv(rcv_links, "data/rcv_links.csv")
write.csv(rcv_nodes, "data/rcv_nodes.csv")