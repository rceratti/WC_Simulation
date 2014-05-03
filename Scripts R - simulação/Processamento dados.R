# Carregamento dos dados
setwd('~/Rubem_Ceratti/Outros/WC')

load(file = "match_data.RData")
countries <- read.csv('WC_countries.csv', colClasses = rep('character', 2))
ratings <- read.csv('eloRatings.csv', colClasses = c('character', 'numeric'))



# Países participantes e suas pontuações
countries_1 <- merge(countries, ratings, by = 'Team', all.x = T, sort = F)
#countries_1[1, 3] <- countries_1[1, 3] + 100



# Partidas catalogadas que serão usadas na estimação do modelo
matches_1 <- mapply(function(x, y) {
  ht <- x$HomeTeam == y
  at <- x$AwayTeam == y

  htDat <- x[ht, c('Year', 'HomeTeam', 'AwayTeam', 'HomeScore', 'AwayScore',  
                   'HomeRating', 'AwayRating', 'HomeVar', 'AwayVar')]
  atDat <- x[at, c('Year', 'AwayTeam', 'HomeTeam', 'AwayScore', 'HomeScore',  
                   'AwayRating', 'HomeRating', 'AwayVar', 'HomeVar')]

  names(htDat) <- c('Year', 'Team', 'Opp', 'TeamScore', 'OppScore', 'TeamRating', 
                   'OppRating', 'TeamVar', 'OppVar')
  names(atDat) <- names(htDat)
  x <- rbind(htDat, atDat)

  x$RatingDiff <- with(x, ((TeamRating-TeamVar)-(OppRating-OppVar))/100)
  x[order(x$Year), ]
}, x = matches, y = names(matches), SIMPLIFY = FALSE)

matches_2 <- do.call(rbind, matches_1)
matches_2$YearTrans <- scale(matches_2$Year)

wdl <- with(matches_2, TeamScore - OppScore)
matches_2$GameResult <- ifelse(wdl == 0, 'Draw', ifelse(wdl > 0, 'Win', 'Lose'))

opp <- matches_2$Opp
opp <- gsub('Commonwealth of Independent States|Soviet Union', 'Russia', opp)
opp <- gsub('West Germany', 'Germany', opp)
opp <- gsub('Great Britain', 'England', opp)

save(countries_1, file = 'countries_1.RData')
save(matches_2, file = 'matches_2.RData')