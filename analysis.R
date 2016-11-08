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
require(stringr)
# require(scales)
# require(chron)


###############################################################################
### NFL DATA
###############################################################################

# Ranking data

nflData <- read.csv('nflPlayoffResults.csv', header = F, stringsAsFactors = F)
names(nflData) <- c('roundString', 'day', 'date', 'winner','at','loser','boxscore','winningScore','losingScore','year')

# Translate the round of the playoffs into a numeric value (e.g. first round == 1)
nflData <- nflData %>%
        mutate(round = ifelse(roundString == 'WildCard', 1 , 
                    ifelse(roundString == 'Division', 2 , 
                    ifelse(roundString == 'ConfChamp', 3 , 
                    ifelse(roundString == 'SuperBowl', 4, NA)))))


nflData$roundString = ifelse(nflData$roundString == 'WildCard', 'Wild Card' , 
                          ifelse(nflData$roundString == 'Division', 'Divisional Round' , 
                                 ifelse(nflData$roundString == 'ConfChamp', 'Conference Championship' , 
                                        ifelse(nflData$roundString == 'SuperBowl', 'Super Bowl', NA))))

nflDataTrim <- select(nflData, winner, loser, roundString, round, year)


# New dataframe for super bowl winners
superBowlWinners <- nflData %>%
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
playoffResults <-merge(nflDataTrim, seedLong, by.x = c('loser', 'year'), by.y = c('team', 'year'))
playoffResults <- arrange(playoffResults, year, seed)

superBowlResults <-merge(superBowlWinners, seedLong, by.x = c('winner', 'year'), by.y = c('team', 'year'))

# Aooend the two datasets together
completeResults <- rbind(playoffResults, superBowlResults)

# Remove the 1982 playoffs from the dataset, which were weird because of a strike
completeResults <- completeResults %>%
                    filter(year != 1982)


# Now summarize in a small table describing how often each seed LOST at that round of the playoffs (e.g. that round was the furthest they made it)
nflSummary <- completeResults %>%
    group_by(roundString, seed) %>%
    summarize(count = n())   


# Recode table so the graph is descriptive
nflSummary$seed = ifelse(nflSummary$seed == 1, '1st seed' , 
                          ifelse(nflSummary$seed == 2, '2nd seed' , 
                                 ifelse(nflSummary$seed == 3, '3rd seed' , 
                                        ifelse(nflSummary$seed == 4, '4th seed' , 
                                               ifelse(nflSummary$seed == 5, '5th seed' , 
                                                    ifelse(nflSummary$seed == 6, '6th seed', NA))))))

# Translate table to proportions (e.g. 50% of Super Bowl winners were the 1 seed, rather than 4 Super Bowl winnners were the 1 seed)
nflSummary <- nflSummary %>%
            group_by(roundString) %>%
            mutate(totalGames = sum(count)) %>%
            mutate(freq = count/totalGames) %>%
            select(roundString, seed, freq) %>%
            rename(source = roundString, target = seed, value = freq)



# Save table
write.csv(nflSummary, 'nflSummary.csv', row.names = F)




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

# And lets give the rounds the same values (regardless of conference)
nbaData$roundNum = ifelse(nbaData$round == 'Eastern Conf First Round' | nbaData$round == 'Western Conf First Round', 'First Round',
                      ifelse(nbaData$round == 'Eastern Conf Semifinals' | nbaData$round == 'Western Conf Semifinals', 'Conference Semis', 
                             ifelse(nbaData$round == 'Eastern Conf Finals' | nbaData$round == 'Western Conf Finals', 'Conference Finals', 
                                    ifelse(nbaData$round == 'Finals', 'NBA Finals', 
                                                  ifelse(nbaData$round == 'World Champs', 'World Champs', NA)))))


# Get a table summarizing results, which I'll use to make the Sankey Charts
nbaSummary <- nbaData %>%
    group_by(roundNum, seed) %>%
    summarize(count = n())   


# Recode table so the graph is descriptive
nbaSummary$seed = ifelse(nbaSummary$seed == 1, '1st seed' , 
                         ifelse(nbaSummary$seed == 2, '2nd seed' , 
                                ifelse(nbaSummary$seed == 3, '3rd seed' , 
                                       ifelse(nbaSummary$seed == 4, '4th seed' , 
                                              ifelse(nbaSummary$seed == 5, '5th seed' , 
                                                     ifelse(nbaSummary$seed == 6, '6th seed' , 
                                                            ifelse(nbaSummary$seed == 7, '7th seed' , 
                                                                ifelse(nbaSummary$seed == 8, '8th seed', NA))))))))


# Translate table to proportions (e.g. 50% of world champs were the 1 seed, rather than 4 world champs were the 1 seed)
nbaSummary <- nbaSummary %>%
    group_by(roundNum) %>%
    mutate(totalGames = sum(count)) %>%
    mutate(freq = count/totalGames) %>%
    select(roundNum, seed, freq) %>%
    rename(source = roundNum, target = seed, value = freq)



# Save table
write.csv(nbaSummary, 'nbaSummary.csv', row.names = F)





###############################################################################
### MLB DATA
###############################################################################

mlbData <- read.csv('mlbPlayoffResults.csv', header = F, stringsAsFactors = F)

# Clean 'er up!

# Remove some of the weird headers that got pulled in while I was scraping
mlbData <- mlbData %>%
            mutate(headerFlag = as.numeric(str_replace_all(V1,  "[^[:alnum:]]", ""))) %>%
            filter(is.na(headerFlag)) %>%
            select(-headerFlag)

# Put the year into its own column
mlbData <- mlbData %>%
            separate(V1, c('year', 'text'), "(?<=[0-9]) (?=[:alpha:])")


# Put the round into its own column
rounds <- c('ALDS','NLDS','ALCS','NLCS','ALWC','NLWC','World Series',"World series")
mlbData <- mlbData %>%
    mutate(round = str_extract(text, paste(rounds, collapse="|")))

# Get the winning team, which is always the first one listed
mlbData <- mlbData %>%
            mutate(winner = gsub('(.*)(\\n)(.*)( vs\\.)(.*)', '\\3', text) ) %>%
            mutate(winner = gsub('(.*)( \\()(.*)', '\\1', winner), winner) %>%
            mutate(loser = gsub('(.*)(\\n)(.*)( vs\\.)(.*)', '\\5', text) ) %>%
            mutate(loser = gsub('(.*)( \\()(.*)', '\\1', loser), loser) %>%
            mutate(loser = trimws(loser)) %>%
            mutate(winner = trimws(winner))

# Mark Wild Card teams, for later use
mlbData <- mlbData %>%
            mutate(seed = ifelse(str_detect(mlbData$loser, '\\*'), 'Wild Card', '')) %>%
            # But now get rid of the apostrophe, so that the team names are clean
            mutate(loser = gsub('\\*', '', loser), loser) %>%
            mutate(winner = gsub('\\*', '', winner), winner)


# Dictionary
dict <- read.csv('mlbTeamDictionary.csv', header = F)
names(dict) <- c('short','long')
dict$long <- trimws(dict$long)
dict$short <- trimws(dict$short)
# Merge in leagues to each one of the teams
leagues <- read.csv('mlbTeamLeagues.csv')
dict <- merge(dict, leagues, by.x = 'short', by.y = 'Tm', all = T)

# Use the above dictionary to transalte all teams into three-letter codes
mlbData <- mlbData %>%    
    mutate(winnerShort = as.character(with(dict, short[match(mlbData$winner, long)]))) %>%
    mutate(winnerShort = ifelse(is.na(winnerShort), winner, winnerShort)) %>%
    mutate(winnerShort = ifelse(winnerShort == 'WSN', 'WSH', ifelse(winnerShort == 'LAA', 'ANA', winnerShort))) %>%
    mutate(loserShort = as.character(with(dict, short[match(mlbData$loser, long)]))) %>%
    mutate(loserShort = ifelse(is.na(loserShort), loser, loserShort)) %>%
    mutate(loserShort = ifelse(loserShort == 'WSN', 'WSH', ifelse(loserShort == 'LAA', 'ANA', loserShort))) %>%
    mutate(league = as.character(with(dict, league[match(loserShort, short)]))) 


# Filter to only 1980 and beyond, and drop 1994 where there were no playoffs, and 2016 cause WS ain't complete
mlbData <- mlbData %>% 
            filter(year > 1979) %>%
            filter(year != 1994) %>%
            filter(year != 2016)

# And now keep just the columns we're interested in, and rename
mlbData <- mlbData %>%
            select(year, round, winnerShort, loserShort, seed, league) %>%
            rename(winner = winnerShort, loser = loserShort)
                
# Add one more 'round' where it's just world series winners
mlbChamps <- mlbData %>%
                filter(round == 'World Series') %>%
                mutate(round = 'World Champs', loser = winner, seed = '', league = '') %>%
                # The Marlins won the WS as a WC, two things I'm dealing with manually. So I eed to hard-code this in, too
                mutate(seed = ifelse(loser == 'FLA', 'Wild Card', ''))
mlbChamps$league <- as.character(with(dict, league[match(mlbChamps$loser, short)]))

# Append the WS champs onto the rest of the mlbData
mlbData <- rbind(mlbData, mlbChamps)


# Now for MLB Seeding
mlbSeedData <- read.csv('mlbPlayoffSeeds.csv', header = F, stringsAsFactors = F)


# Rename the columns
mlbSeedData <- mlbSeedData %>%
    rename(teamLong = V1, wins = V2, losses = V3, winPct = V4, gamesBack = V5, homeRecord = V6, awayRecord = V7, year = V8)

# Extract seed from teams where the seed is provided in this data
mlbSeedData$seed <- str_extract(mlbSeedData$teamLong, '\\d')


# Convert names to 3-lettered initials
mlbSeedData <- mlbSeedData %>%    
                # Won't match to the dictionary if the team name is something like "(2) Texas Rangers"
                mutate(teamStripped = gsub('\\([0-9]\\) ', '', mlbSeedData$teamLong)) %>%
                mutate(team = as.character(with(dict, short[match(teamStripped, long)]))) %>%
                select(-teamStripped)

# Wikipedia is weird for 2010 season -- missed the Phillies and the Braves. Manually insert these.
mlbSeedData <- mlbSeedData %>%
            rbind(c('(1) Philadelphia Phillies',97,65,0.599,'—','54–30','43–35', 2010, 1, 'PHI')) %>%
            rbind(c('(4) Atlanta Braves',91,71,0.562,6,'56–25','35–46', 2010, 4, 'ATL'))


# So maybe we want to merge first -- that way we can separate out AL and Nl
# Then we say, for any team with AL in the series title, sort by number of wins and enumerate them
# Merge with the loser, because we want to know info for the the last round the team made it to
# We want to merge onto 
mlbCombinedData <- merge(mlbSeedData, mlbData, by.x = c('team','year'), by.y = c('loser','year'), all = T)

# One more weird thing -- the Astros and the Brewers switched divisions at different points
mlbCombinedData <- mlbCombinedData %>%
    mutate(league = ifelse(as.integer(year) < 1998 & team == 'MIL', 'AL', league)) %>%
    mutate(league = ifelse(as.integer(year) < 2013 & team == 'HOU', 'NL', league))


# Create seed for all the teams

    # For everybody with a seed already, get that down on paper
    mlbCombinedData <- mlbCombinedData %>%
                    mutate(seed = seed.y) %>%
                    mutate(seed = ifelse(!is.na(seed.x), seed.x, seed)) %>%
                    # For 95-97, the WC was the 4 seed
                    mutate(seed = ifelse(seed == 'Wild Card', 4, seed))

    # Now, for teams without a seed, calculate 'seeds' retrospectively using number of wins
    natLeagueUnseeded <- mlbCombinedData %>%
        filter(league == 'NL', seed == '') %>%
        group_by(year) %>%
        mutate(wins = as.integer(wins)) %>%
        arrange(-as.integer(wins)) %>%
        mutate(seed = 1:n())
    natLeagueSeeded <- mlbCombinedData %>%
        filter(league == 'NL', seed != '')
    natLeague <- rbind(natLeagueSeeded,natLeagueUnseeded)

    amLeagueUnseeded <- mlbCombinedData %>%
        filter(league == 'AL', seed == '') %>%
        group_by(year) %>%
        mutate(wins = as.integer(wins)) %>%
        arrange(-as.integer(wins)) %>%
        mutate(seed = 1:n())
    amLeagueSeeded <- mlbCombinedData %>%
        filter(league == 'AL', seed != '')
    amLeague <- rbind(amLeagueSeeded,amLeagueUnseeded)    
    
    # Get rid of unneeded data
    rm(amLeagueSeeded, amLeagueUnseeded, natLeagueSeeded,natLeagueUnseeded)

    
# Get your final dataset!!
mlbComplete <- rbind(amLeague, natLeague)



# And lets give the rounds the same values (regardless of league)
mlbComplete$roundGeneral = ifelse(mlbComplete$round == 'ALWC' | mlbComplete$round == 'NLWC', 'Wild Card',
                          ifelse(mlbComplete$round == 'ALDS' | mlbComplete$round == 'NLDS', 'Divisional Series', 
                                 ifelse(mlbComplete$round == 'ALCS' | mlbComplete$round == 'NLCS', 'League Championship', 
                                        ifelse(mlbComplete$round == 'World Series', 'World Series', 
                                               ifelse(mlbComplete$round == 'World Champs', 'World Champs', NA)))))
# Drop unneeded columns
mlbComplete <- mlbComplete %>%
                select(-seed.x, -seed.y)


# MLB can't make up its mind about playoff formats, so we have to break it into different 'eras'
mlbTwoSeeds <- mlbComplete %>%
                    filter(year < 1995 & year != 1981)

mlbFourSeeds <- mlbComplete %>%
                    filter((year < 2012 & year > 1993)  | year == 1981)

mlbFiveSeeds <- mlbComplete %>%
                    filter(year > 2011)
         

# Now create a summary table for each of the different playoff formats
mlbSummaryTwoSeeds <- mlbTwoSeeds %>%
    group_by(roundGeneral, seed) %>%
    summarize(count = n()) %>%
    # Only examine up until the wild card was introduced. WC introduced in 2012, adding a few more seeds
    group_by(roundGeneral) %>%
    mutate(totalGames = sum(count)) %>%
    mutate(freq = count/totalGames) %>%
    select(roundGeneral, seed, freq) %>%
    rename(source = roundGeneral, target = seed, value = freq)

# Now create a summary table for each of the different playoff formats
mlbSummaryFourSeeds <- mlbFourSeeds %>%
    group_by(roundGeneral, seed) %>%
    summarize(count = n()) %>%
    # Only examine up until the wild card was introduced. WC introduced in 2012, adding a few more seeds
    group_by(roundGeneral) %>%
    mutate(totalGames = sum(count)) %>%
    mutate(freq = count/totalGames) %>%
    select(roundGeneral, seed, freq) %>%
    rename(source = roundGeneral, target = seed, value = freq)

# Now create a summary table for each of the different playoff formats
mlbSummaryFiveSeeds <- mlbFiveSeeds %>%
    group_by(roundGeneral, seed) %>%
    summarize(count = n()) %>%
    # Only examine up until the wild card was introduced. WC introduced in 2012, adding a few more seeds
    group_by(roundGeneral) %>%
    mutate(totalGames = sum(count)) %>%
    mutate(freq = count/totalGames) %>%
    select(roundGeneral, seed, freq) %>%
    rename(source = roundGeneral, target = seed, value = freq)

write.csv(mlbSummaryTwoSeeds, 'mlbSummaryTwoSeeds.csv', row.names = F)
write.csv(mlbSummaryFourSeeds, 'mlbSummaryFourSeeds.csv', row.names = F)
write.csv(mlbSummaryFiveSeeds, 'mlbSummaryFiveSeeds.csv', row.names = F)







###############################################################################
### NHL DATA
###############################################################################            

