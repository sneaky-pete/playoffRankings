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
require(scales)
install.packages("extrafont");library(extrafont)
# require(chron)


###############################################################################
### NFL DATA
###############################################################################

### NFL Playoff result data

    nflData <- read.csv('nflPlayoffResults.csv', header = F, stringsAsFactors = F)
    
    nflData <- nflData %>%
            rename(round = V1, day = V2, date = V3, winner = V4, at = V5, loser = V6, boxscore = V7, winningScore = V8, losingScore = V9, year = V10) %>%
            select(winner, loser, round, year)
    
    # Fix some of the formatting for the rounds
    nflData$round = ifelse(nflData$round == 'WildCard', 'Wild Card' , 
                              ifelse(nflData$round == 'Division', 'Divisional Round' , 
                                     ifelse(nflData$round == 'ConfChamp', 'Conference Championship' , 
                                            ifelse(nflData$round == 'SuperBowl', 'Super Bowl', NA))))
    

    
    # Get playoff result data on team-year level
    nflWinners <- nflData %>% 
        select(-loser) %>%
        rename(team = winner)
    
    nflLosers <- nflData %>% 
        select(-winner) %>%
        rename(team = loser)
    
    # New dataframe for super bowl winners
    nflChamps <- nflData %>%
        filter(round == 'Super Bowl') %>%
        select(winner, loser, round, year) %>%
        mutate(round = 'World Champs') %>%
        select(-loser) %>%
        rename(team = winner)
    
    nflData <- rbind(nflWinners, nflLosers, nflChamps) %>%
        filter(year != 1982)

### NFL Seed data
    
    seed <- read.csv('nflPlayoffSeeds.csv', header = F, stringsAsFactors = F)
    
    names(seed) <-c('seed','teamAfc','teamNfc','year')
    seed$afc <- str_replace(seed$teamAfc, ' \\(.*\\)', '')
    seed$nfc <- str_replace(seed$teamNfc, ' \\(.*\\)', '')
    
    seed <- select(seed, seed, year, afc, nfc)
    
    # Get seed data set on the team-year level
    seedLong <- gather(seed, seed, year)
    names(seedLong) <- c('seed','year','division','team')


### NFL Final dataset creation

    nflDataComplete <- merge(nflData, seedLong, by = c('team', 'year')) %>%
                        arrange(year, seed)
    
    
    nflSummary <- nflDataComplete %>%
        group_by(round, seed) %>%
        summarize(count = n())  %>%
        group_by(round) %>%
        mutate(totalGames = sum(count)) %>%
        mutate(freq = count/totalGames) %>%
        select(round, seed, freq) %>%
        rename(source = round, target = seed, value = freq)
    
    # Recode table so the graph is descriptive
    nflSummary$target = ifelse(nflSummary$target == 1, '1st seed' , 
                             ifelse(nflSummary$target == 2, '2nd seed' , 
                                    ifelse(nflSummary$target == 3, '3rd seed' , 
                                           ifelse(nflSummary$target == 4, '4th seed' , 
                                                  ifelse(nflSummary$target == 5, '5th seed' , 
                                                         ifelse(nflSummary$target == 6, '6th seed', NA))))))
    
    
    # Create a vector with playoff rounds in the desired order (need to be ordered properly for d3 sankey graphing later)
    playoffOrder <- c("World Champs", "Super Bowl", "Conference Championship", "Divisional Round", "Wild Card")
    
    nflSummary <- nflSummary %>%
        ungroup() %>%
        mutate(category =  factor(source, levels = playoffOrder)) %>%
        arrange(category, target) %>%
        select(-category)

write.csv(nflSummary, 'nflSummary.csv', row.names = F)




###############################################################################
### NBA DATA
###############################################################################

### NBA playoff results (contains seed data as well)
    
    nbaData <- read.csv('nbaPlayoffResults.csv', header = T, stringsAsFactors = F)

    # Rename columns
    names(nbaData) <- c('year','league','round', 'dates', 'blank','winner','winnerWins','blank2','loser','loserWins')
    
    # Drop columns of blanks
    nbaData <- select(nbaData, year, round, winner, winnerWins, loser, loserWins)
    # I think there are some rows that aren't data in here still (the column titles are repeated throughout the table for the viewer's reference)
    table(nbaData$year) # E.g. there are 25 rows where the year == 'Yr'
    nbaData <-nbaData %>%
                filter(year != 'Yr')

    # To compare to other sports, let's also take just years 1980 and beyond
    nbaData$year <- as.numeric(nbaData$year)
    nbaData <- filter(nbaData, year > 1979)
    
    # Extract seeding from the 'winner' and 'loser' columns
    nbaData$winnerSeed <- gsub(".*\\((.*)\\).*", "\\1", nbaData$winner)
    nbaData$loserSeed <- gsub(".*\\((.*)\\).*", "\\1", nbaData$loser)
    
    # Now need to add a row for NBA champions
    nbaChamps <- nbaData %>%
        filter(round == 'Finals') %>%
        mutate(round = 'World Champs') 
    
    # And lets give the rounds the same values (regardless of conference)
    nbaData$roundNum = ifelse(nbaData$round == 'Eastern Conf First Round' | nbaData$round == 'Western Conf First Round', 'First Round',
                          ifelse(nbaData$round == 'Eastern Conf Semifinals' | nbaData$round == 'Western Conf Semifinals', 'Conference Semis', 
                                 ifelse(nbaData$round == 'Eastern Conf Finals' | nbaData$round == 'Western Conf Finals', 'Conference Finals', 
                                        ifelse(nbaData$round == 'Finals', 'NBA Finals', NA))))
    
    # Get this to the team-year level
    nbaWinners <- nbaData %>%
                    select(year, roundNum, winner, winnerSeed) %>%
                    rename(team = winner, seed = winnerSeed, round = roundNum)
    nbaLosers <- nbaData %>%
                    select(year, roundNum, loser, loserSeed) %>%
                    rename(team = loser, seed = loserSeed, round = roundNum)
    nbaChamps <- nbaChamps %>%
                    select(year, round, winner, winnerSeed) %>%
                    rename(team = winner, seed = winnerSeed)
    
    nbaDataComplete <- rbind(nbaWinners,nbaLosers,nbaChamps)
    
    
    # Get a table summarizing results, which I'll use to make the Sankey Charts
    nbaSummary <- nbaDataComplete %>%
        group_by(round, seed) %>%
        summarize(count = n()) %>%
        group_by(round) %>%
        mutate(totalGames = sum(count)) %>%
        mutate(freq = count/totalGames) %>%
        select(round, seed, freq) %>%
        rename(source = round, target = seed, value = freq)  
    
    # Recode table so the graph is descriptive
    nbaSummary$target = ifelse(nbaSummary$target == 1, '1st seed' ,
                             ifelse(nbaSummary$target == 2, '2nd seed' , 
                                    ifelse(nbaSummary$target == 3, '3rd seed' , 
                                           ifelse(nbaSummary$target == 4, '4th seed' , 
                                                  ifelse(nbaSummary$target == 5, '5th seed' , 
                                                         ifelse(nbaSummary$target == 6, '6th seed' , 
                                                                ifelse(nbaSummary$target == 7, '7th seed' , 
                                                                    ifelse(nbaSummary$target == 8, '8th seed', NA))))))))
    
    # Create a vector with playoff rounds in the desired order (need to be ordered properly for d3 sankey graphing later)
    playoffOrder <- c("World Champs", "NBA Finals", "Conference Finals", "Conference Semis", "First Round")
    
    nbaSummary <- nbaSummary %>%
        ungroup() %>%
        mutate(category =  factor(source, levels = playoffOrder)) %>%
        arrange(category, target) %>%
        select(-category)

    
# Because of a weird quirk in the D3 Sankey graphs, manually fill in some of the (empty) seed-game pairings
head(nbaSummary, 15)    
four <- c('World Champs','4th seed',0)
five <- c('World Champs','5th seed',0)
seven <- c('World Champs','7th seed',0)	
eight <- c('World Champs','8th seed',0)	
nbaSummarySankey <- rbind(nbaSummary, four, five, seven, eight)
nbaSummarySankey <- nbaSummarySankey %>%
	ungroup() %>%
	mutate(category =  factor(source, levels = playoffOrder)) %>%
	arrange(category, target) %>%
	select(-category)    

# Save table
write.csv(nbaSummary, 'nbaSummary.csv', row.names = F)
write.csv(nbaSummarySankey, 'nbaSummarySankey.csv', row.names = F)



###############################################################################
### MLB DATA
###############################################################################

### MLB playoff result data 

    mlbData <- read.csv('mlbPlayoffResults.csv', header = F, stringsAsFactors = F)
    
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
    
    # Separate out winning and losing teams into their own columns
    mlbData <- mlbData %>%
                mutate(winner = gsub('(.*)(\\n)(.*)( vs\\.)(.*)', '\\3', text) ) %>%
                mutate(winner = gsub('(.*)( \\()(.*)', '\\1', winner), winner) %>%
                mutate(loser = gsub('(.*)(\\n)(.*)( vs\\.)(.*)', '\\5', text) ) %>%
                mutate(loser = gsub('(.*)( \\()(.*)', '\\1', loser), loser) %>%
                mutate(loser = trimws(loser)) %>%
                mutate(winner = trimws(winner))
    
    # Mark Wild Card teams, for later use
    mlbData <- mlbData %>%
                mutate(loserSeed = ifelse(str_detect(mlbData$loser, '\\*'), 'Wild Card', '')) %>%
                mutate(winnerSeed = ifelse(str_detect(mlbData$winner, '\\*'), 'Wild Card', '')) %>%
                # But now get rid of the apostrophe, so that the team names are clean
                mutate(loser = gsub('\\*', '', loser), loser) %>%
                mutate(winner = gsub('\\*', '', winner), winner)
    
    
    # Dictionary, for converting teams to their three-letter initials
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
        mutate(winnerShort = ifelse(winnerShort == 'WSN', 'WSH', ifelse(winnerShort == 'LAA', 'ANA', ifelse(winnerShort == 'MON', 'MTL', winnerShort)))) %>%
        mutate(loserShort = as.character(with(dict, short[match(mlbData$loser, long)]))) %>%
        mutate(loserShort = ifelse(is.na(loserShort), loser, loserShort)) %>%
        mutate(loserShort = ifelse(loserShort == 'WSN', 'WSH', ifelse(loserShort == 'LAA', 'ANA', ifelse(loserShort == 'MON', 'MTL', loserShort))))
    
    # Filter to only 1980 and beyond, and drop 1994 where there were no playoffs
    # And do a little cleaning
    mlbData <- mlbData %>% 
                filter(year > 1979) %>%
                filter(year != 1994) %>%
                select(year, round, winnerShort, loserShort, winnerSeed, loserSeed) %>%
                rename(winner = winnerShort, loser = loserShort)
                    
    # Add one more 'round' where it's just world series winners
    mlbChamps <- mlbData %>%
                    filter(round == 'World Series') %>%
                    select(year, round, winner, winnerSeed) %>%            
                    rename(team = winner, seed = winnerSeed) %>%
                    mutate(round = 'World Champs') %>%
                    # The Marlins won the WS as a WC twice, two things I'm dealing with manually. So I eed to hard-code this in, too
                    mutate(seed = ifelse(team == 'FLA', 'Wild Card', ''))
    
    # Get data on team-year-game level (will make the df doubly as long)
    mlbWinners <- mlbData %>%
                    select(-loser, -loserSeed) %>%
                    rename(team = winner, seed = winnerSeed)
    mlbLosers <- mlbData %>%
                    select(-winner, -winnerSeed) %>%
                    rename(team = loser, seed = loserSeed)
    
    mlbData <- rbind(mlbWinners, mlbLosers, mlbChamps)


### MLB Seeding

    mlbSeedData <- read.csv('mlbPlayoffSeeds.csv', header = F, stringsAsFactors = F)
    
    # Rename the columns
    mlbSeedData <- mlbSeedData %>%
        rename(teamLong = V1, wins = V2, losses = V3, winPct = V4, gamesBack = V5, homeRecord = V6, awayRecord = V7, year = V8)
    
    # Extract seed from teams where the seed is provided in this data (e.g. "Yankees (1)")
    mlbSeedData$seed <- str_extract(mlbSeedData$teamLong, '\\d')
    
    
    # Convert names to 3-lettered initials and add League
    mlbSeedData <- mlbSeedData %>%    
        # Won't match to the dictionary if the team name is something like "(2) Texas Rangers"
        mutate(teamStripped = gsub('\\([0-9]\\) ', '', mlbSeedData$teamLong)) %>%
        mutate(team = as.character(with(dict, short[match(teamStripped, long)]))) %>%
        mutate(league = as.character(with(dict, league[match(teamStripped, long)]))) %>%
        select(-teamStripped) %>%
        # One more weird thing -- the Astros and the Brewers switched divisions at different points
        mutate(league = ifelse(as.integer(year) < 1998 & team == 'MIL', 'AL', league)) %>%
        mutate(league = ifelse(as.integer(year) < 2013 & team == 'HOU', 'NL', league))
    
    
    # Wikipedia is weird for 2010 season -- missed the Phillies and the Braves. Manually insert these.
    mlbSeedData <- mlbSeedData %>%
        rbind(c('(1) Philadelphia Phillies',97,65,0.599,'—','54–30','43–35', 2010, 1, 'PHI', 'NL')) %>%
        rbind(c('(4) Atlanta Braves',91,71,0.562,6,'56–25','35–46', 2010, 4, 'ATL', 'NL'))
    
    
    # Calculate seeds
    
    # First -- have to take into account which teams are wild cards, which is in the other dataset
    wildCards <- mlbData %>% 
                    filter(seed == 'Wild Card') %>%
                    select(year, team, seed) %>%
                    rename(wildCard = seed) %>% 
                    # right now, WC data is at year-team-round level. Reduce it to year-team level
                    group_by(year, team, wildCard) %>% 
                    filter(row_number(year) == 1)
    
    # If you were marked as a WC in mlbData AND you don't have a seed from the more reliable mlbSeedData, you'll be a 4 seed
    # Manually checked and this is okay. It only occurs six times anyways...
    mlbSeedData <- merge(mlbSeedData, wildCards, by = c('year', 'team'), all = T ) %>%
                        mutate(seed = ifelse((is.na(seed) & wildCard == 'Wild Card'), 4, seed)) %>%
                        select(-wildCard)
    
    # Now, calculate seeds based on wins
    # For teams without a seed, calculate 'seeds' retrospectively using number of wins
    natLeagueUnseeded <- mlbSeedData %>%
        filter(league == 'NL', is.na(seed)) %>%
        group_by(year) %>%
        mutate(wins = as.integer(wins)) %>%
        arrange(-as.integer(wins)) %>%
        mutate(seed = 1:n())
    natLeagueSeeded <- mlbSeedData %>%
        filter(league == 'NL', !is.na(seed))
    natLeague <- rbind(natLeagueSeeded,natLeagueUnseeded)
    
    amLeagueUnseeded <- mlbSeedData %>%
        filter(league == 'AL', is.na(seed)) %>%
        group_by(year) %>%
        mutate(wins = as.integer(wins)) %>%
        arrange(-as.integer(wins)) %>%
        mutate(seed = 1:n())
    amLeagueSeeded <- mlbSeedData %>%
        filter(league == 'AL', !is.na(seed))
    amLeague <- rbind(amLeagueSeeded,amLeagueUnseeded)
    
    # Get rid of unneeded data
    rm(amLeagueSeeded, amLeagueUnseeded, natLeagueSeeded,natLeagueUnseeded)
    
    # Rebuild mlbSeedData to include imputed seeds
    mlbSeedData <- rbind(amLeague, natLeague)


### Final dataset creation

    # Merge seed data onto playoff result data
    mlbDataComplete <- merge(mlbSeedData, mlbData, by = c('team','year'), all = T) %>%
        select(-seed.y) %>%
        rename(seed = seed.x)
    
    # And lets give the rounds the same values (regardless of league)
    mlbDataComplete$roundGeneral = ifelse(mlbDataComplete$round == 'ALWC' | mlbDataComplete$round == 'NLWC', 'Wild Card',
                              ifelse(mlbDataComplete$round == 'ALDS' | mlbDataComplete$round == 'NLDS', 'Divisional Series', 
                                     ifelse(mlbDataComplete$round == 'ALCS' | mlbDataComplete$round == 'NLCS', 'League Championship', 
                                            ifelse(mlbDataComplete$round == 'World Series', 'World Series', 
                                                   ifelse(mlbDataComplete$round == 'World Champs', 'World Champs', NA)))))
    
    # MLB can't make up its mind about playoff formats, so we have to break it into different 'eras'
    mlbTwoSeeds <- mlbDataComplete %>%
                        filter(year < 1995 & year != 1981)
    mlbFourSeeds <- mlbDataComplete %>%
                        filter((year < 2012 & year > 1993)  | year == 1981)
    mlbFiveSeeds <- mlbDataComplete %>%
                        filter(year > 2011)
             
    # Create a vector with playoff rounds in the desired order (need to be ordered properly for d3 sankey graphing later)
    playoffOrder<- c("World Champs", "World Series", "League Championship")
    # Now create a summary table for each of the different playoff formats
    mlbSummaryTwoSeeds <- mlbTwoSeeds %>%
        group_by(roundGeneral, seed) %>%
        summarize(count = n()) %>%
        # Only examine up until the wild card was introduced. WC introduced in 2012, adding a few more seeds
        group_by(roundGeneral) %>%
        mutate(totalGames = sum(count)) %>%
        mutate(freq = count/totalGames) %>%
        select(roundGeneral, seed, freq) %>%
        rename(source = roundGeneral, target = seed, value = freq) %>%
        ungroup() %>%
        mutate(category =  factor(source, levels = playoffOrder)) %>%
        arrange(category, target) %>%
        select(-category)
    
    # Create a vector with playoff rounds in the desired order (need to be ordered properly for d3 sankey graphing later)
    playoffOrder<- c("World Champs", "World Series", "League Championship", "Divisional Series")
    # Now create a summary table for each of the different playoff formats
    mlbSummaryFourSeeds <- mlbFourSeeds %>%
        group_by(roundGeneral, seed) %>%
        summarize(count = n()) %>%
        # Only examine up until the wild card was introduced. WC introduced in 2012, adding a few more seeds
        group_by(roundGeneral) %>%
        mutate(totalGames = sum(count)) %>%
        mutate(freq = count/totalGames) %>%
        select(roundGeneral, seed, freq) %>%
        rename(source = roundGeneral, target = seed, value = freq)%>%
        ungroup() %>%
        mutate(category =  factor(source, levels = playoffOrder)) %>%
        arrange(category, target) %>%
        select(-category)
    
    # Create a vector with playoff rounds in the desired order (need to be ordered properly for d3 sankey graphing later)
    playoffOrder<- c("World Champs", "World Series", "League Championship", "Divisional Series", "Wild Card")
    # Now create a summary table for each of the different playoff formats
    mlbSummaryFiveSeeds <- mlbFiveSeeds %>%
        group_by(roundGeneral, seed) %>%
        summarize(count = n()) %>%
        # Only examine up until the wild card was introduced. WC introduced in 2012, adding a few more seeds
        group_by(roundGeneral) %>%
        mutate(totalGames = sum(count)) %>%
        mutate(freq = count/totalGames) %>%
        select(roundGeneral, seed, freq) %>%
        rename(source = roundGeneral, target = seed, value = freq) %>%
        ungroup() %>%
        mutate(category =  factor(source, levels = playoffOrder)) %>%
        arrange(category, target) %>%
        select(-category)

write.csv(mlbSummaryTwoSeeds, 'mlbSummaryTwoSeeds.csv', row.names = F)
write.csv(mlbSummaryFourSeeds, 'mlbSummaryFourSeeds.csv', row.names = F)
write.csv(mlbSummaryFiveSeeds, 'mlbSummaryFiveSeeds.csv', row.names = F)




###############################################################################
### NHL DATA
###############################################################################            

### NHL Playoff result data

    nhlData <- read.csv('nhlPlayoffResults.csv', header = F, stringsAsFactors = F)
    
    # Get rid of weird separator rows that got scraped
    # And rename columns, while you're at it
    nhlData <- nhlData %>%
                    rename(round = V1, winLoss = V2, teams = V3, blank = V4, junk = V5, year = V6) %>%
                    filter(teams != '') %>%
                    select(-junk, -blank) %>%
                    separate(teams, c('winner', 'loser'), sep = ' over ')
    
    nhlWinners<- nhlData %>%
                    select(winner, year, round) %>%
                    rename(team = winner)
    
    nhlLosers<- nhlData %>%
        select(loser, year, round) %>%
        rename(team = loser)
    
    # Make separate frame for World Champions
    nhlChampions <- nhlData %>%
        filter(round == 'Stanley Cup Final') %>%
        mutate(round = 'World Champs') %>%
        select(winner, year, round) %>%
        rename(team = winner)
    
    
### NHL Seed data
        
    nhlSeed <- read.csv('nhlPlayoffSeeds.csv', header = F, stringsAsFactors = F)
    
    # Cleaning
    nhlSeed <- nhlSeed %>%
                rename(team = V1, year = V2, seed = V3, conf = V4) %>%
                # First column comes with a lot of info besides just the team name, all after a comma. Extract team from this
                separate(team, c('team','junk'), sep = ',') %>%
                select(-junk) %>%
                # Sometimes the separator is a hyphen-looking thing (CAREFUL! THAT'S NOT A HYPHEN IS A SPEC. CHAR)
                separate(team, c('team','junk'), sep = ' – ') %>%
                select(-junk)
    

### NHL final, merged data sets

    # Append the three data sets
    nhlData <- rbind(nhlWinners, nhlLosers, nhlChampions)
    # Merge in seed info
    nhlCompleteData <- merge(nhlData, nhlSeed, by = c('team', 'year'))
    
    
    # Create a vector with playoff rounds in the desired order (need to be ordered properly for d3 sankey graphing later)
    playoffOrder<- c("World Champs", "Stanley Cup Final", "Conference Finals", "Conference Semi-Finals", "Conference Quarter-Finals")
    # Create a summary table
    nhlSummary <- nhlCompleteData %>%
        group_by(round, seed) %>%
        summarize(count = n()) %>%
        group_by(round) %>%
        mutate(totalGames = sum(count)) %>%
        mutate(freq = count/totalGames) %>%
        select(round, seed, freq) %>%
        rename(source = round, target = seed, value = freq) %>%
        ungroup() %>%
        mutate(category =  factor(source, levels = playoffOrder)) %>%
        arrange(category, target) %>%
        select(-category)
    
    
# Save
write.csv(nhlSummary, 'nhlSummary.csv', row.names = F)







################################################################################
################################################################################
### Chart creation
################################################################################
################################################################################

nflBar <- nflSummary %>% 
			filter(source == 'World Champs')

ggplot(data=nflBar, aes(x=target, y=value)) +
	geom_bar(stat="identity") +
	ggtitle("Probably of winning the 'ship, by seed") +
	scale_y_continuous(labels=percent) +
	theme(text=element_text(family="Avenir"))


font_import("Trebuchet MS")



test <- (nhlCompleteData%>% filter(round == 'Conference Quarter-Finals') %>% group_by(year, seed) %>% summarize(n = n()))
View(nhlCompleteData%>% filter(round == 'Conference Quarter-Finals'))
