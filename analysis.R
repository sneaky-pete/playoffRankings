###############################################################################
# Author: Peter Hlawitschka
# Date created: July 2016
###############################################################################

# Set working directory
setwd('~/Home/playoffRankingData')

# Require packages I'll be using
require(dplyr)
require(tidyr)
require(ggplot2)
# require(stringr)
# require(scales)
# require(chron)


###############################################################################

# Ranking data

data <- read.csv('nflPlayoffResults.csv', header = F, stringsAsFactors = F)
names(data) <- c('roundString', 'day', 'date', 'winner','at','loser','boxscore','winningScore','losingScore','year')

# Translate the round of the playoffs into a numeric value (e.g. first round == 1)
data <- data %>%
        mutate(round = ifelse(roundString == 'WildCard', 1 , 
                    ifelse(roundString == 'Division', 2 , 
                    ifelse(roundString == 'ConfChamp', 3 , 
                    ifelse(roundString == 'SuperBowl', 4, NA)))))


data$roundString = ifelse(data$roundString == 'WildCard', 'Wild Card' , 
                          ifelse(data$roundString == 'Division', 'Divisional Round' , 
                                 ifelse(data$roundString == 'ConfChamp', 'Conference Championship' , 
                                        ifelse(data$roundString == 'SuperBowl', 'Super Bowl', NA))))

dataTrim <- select(data, winner, loser, roundString, round, year)


# New datafram for super bowl winners
superBowlWinners <- data %>%
                        filter(roundString == 'Super Bowl')

superBowlWinners <- select(superBowlWinners, winner, loser, roundString, round, year)
# Make round == 5 for all of these, to signify they're the champs
superBowlWinners$round <- 5
superBowlWinners$roundString <- 'World Champion'

# Clean seed data

seed <- read.csv('nflPlayoffSeeds.csv', header = F, stringsAsFactors = F)

names(seed) <-c('seed','teamAfc','teamNfc','year')
seed$afc <- str_replace(seed$teamAfc, ' \\(.*\\)', '')
seed$nfc <- str_replace(seed$teamNfc, ' \\(.*\\)', '')

seed <- select(seed, seed, year, afc, nfc)

    # Get this data set on the team-year level
    seedLong <- gather(seed, seed, year)
    names(seedLong) <- c('seed','year','division','team')

    
# Merge
playoffResults <-merge(dataTrim, seedLong, by.x = c('loser', 'year'), by.y = c('team', 'year'))
playoffResults <- arrange(playoffResults, year, seed)

superBowlResults <-merge(superBowlWinners, seedLong, by.x = c('winner', 'year'), by.y = c('team', 'year'))

# Aooend the two datasets together
completeResults <- rbind(playoffResults, superBowlResults)

# Remove the 1982 playoffs from the dataset, which were weird because of a strike
completeResults <- completeResults %>%
                    filter(year != 1982)


# Now condense down into a small table
table <- completeResults %>%
    group_by(roundString, seed) %>%
    summarize(count = n())   


# Format table so it's good to graph
names(table) <- c('source','target','value')
table$target = ifelse(table$target == 1, '1st seed' , 
                          ifelse(table$target == 2, '2nd seed' , 
                                 ifelse(table$target == 3, '3rd seed' , 
                                        ifelse(table$target == 4, '4th seed' , 
                                               ifelse(table$target == 5, '5th seed' , 
                                                    ifelse(table$target == 6, '6th seed', NA))))))
# Save table
write.csv(table, 'nflResults.csv', row.names = F)

