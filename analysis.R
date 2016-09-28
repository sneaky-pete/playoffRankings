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
### NFL DATA
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


# New dataframe for super bowl winners
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


# Now summarize in a small table describing how often each seed LOST at that round of the playoffs (e.g. that round was the furthest they made it)
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




###############################################################################
### NBA DATA
###############################################################################

nbaData <- read.csv('nbaPlayoffResults.csv', header = T, stringsAsFactors = F)

# Clean 'er up!

    # Check out a couple of weird columns. I think they were just there for aesthetic reasons to make the table display nicely online
    nrow(nbaData)
    table(is.na(nbaData$X..1))
    table(is.na(nbaData$X..2))
    # Yep, they're always NA. We'll drop them soon...
    
    
    # Rename columns
    names(nbaData) <- c('year','league','round', 'dates', 'blank','winner','winnerWins','blank2','loser','loserWins')
    
    # Drop columns of blanks
    nbaData <- select(nbaData, year, round, winner, winnerWins, loser, loserWins)
    # I think there are some rows that aren't data in here still (the column titles are repeated throughout the table for the viewer's reference)
    table(nbaData$year) # E.g. there are 25 rows where the year == 'Yr'
    nrow(nbaData)
    nbaData <-nbaData[nbaData$year != 'Yr',]
    nrow(nbaData) # Cool. We appear to have dropped 25 rows
    
    # To compare to other sports, let's also take just years 1980 and beyond
    nbaData$year <- as.numeric(nbaData$year)
    nbaData <- filter(nbaData, year > 1979)

# Extract seeding from the 'winner' and 'loser' columns
nbaData$winnerSeed <- gsub(".*\\((.*)\\).*", "\\1", nbaData$winner)
nbaData$loserSeed <- gsub(".*\\((.*)\\).*", "\\1", nbaData$loser)
nbaData$seed <- nbaData$loserSeed

# Now need to add a row for NBA champions
nbaChamps <- nbaData %>%
    filter(round == 'Finals')

# Make some tweaks to this set
nbaChamps$round <- 'World Champs'
nbaChamps$seed <- nbaChamps$winnerSeed


# Now append the champs data set to the original data
nbaData <- rbind(nbaData, nbaChamps)

# And lets give the rounds numerical values
nbaData$roundNum = ifelse(nbaData$round == 'Eastern Conf First Round' | nbaData$round == 'Western Conf First Round', 1,
                      ifelse(nbaData$round == 'Eastern Conf Semifinals' | nbaData$round == 'Western Conf Semifinals', 2, 
                             ifelse(nbaData$round == 'Eastern Conf Finals' | nbaData$round == 'Western Conf Finals', 3, 
                                    ifelse(nbaData$round == 'Finals', 4, 
                                                  ifelse(nbaData$round == 'World Champs', 5, NA)))))


# Now summarize in a small table describing how often each seed LOST at that round of the playoffs (e.g. that round was the furthest they made it)
nbaSummary <- nbaData %>%
    group_by(round, seed) %>%
    summarize(count = n())       
    


View(nbaData %>% filter(round == 'World Champs'))
