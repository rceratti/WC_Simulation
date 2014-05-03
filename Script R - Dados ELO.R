library(XML)


dir.create("~/Rubem_Ceratti/Outros/WC")
setwd("~/Rubem_Ceratti/Outros/WC")

# Base URL to ELO 
eloURL <- "http://www.eloratings.net/"



# Current ratings
eloRatings <- readHTMLTable(eloURL, which = 3, stringsAsFactors = FALSE)
eloRatings <- eloRatings[eloRatings$V1 != "rank", 2:3]
names(eloRatings) <- c("Team", "Rating")
eloRatings$Rating <- as.numeric(eloRatings$Rating)

write.csv(eloRatings, "eloRatings.csv", row.names = FALSE)



# Links to each country's match history 
cntryLinks <- getHTMLLinks(eloURL)
cntryLinks <- cntryLinks[19:251]



# List of WC countries
cntryWC <- readHTMLTable("http://www.fifa.com/worldcup/standings/index.html", 
                         stringsAsFactors = FALSE)
cntryWC <- lapply(cntryWC, '[[', 'Team')
cntryWC <- do.call(c, cntryWC)
groups <- gsub("^.*([A-H])[1-4]$", "\\1", names(cntryWC))
cntryWC <- data.frame(Team = cntryWC, Group = groups, stringsAsFactors = FALSE)
rownames(cntryWC) <- NULL

elo <- c("Ivory Coast", "Bosnia and Herzegovina", "United States", "South Korea")
wc <- c("Côte d'Ivoire", "Bosnia-Herzegovina", "USA", "Korea Republic")
for(i in 1:4) cntryWC$Team[cntryWC$Team == wc[i]] <- elo[i]

write.csv(cntryWC, "WC_countries.csv", row.names = FALSE)



# Get match data for WC countries
ind <- match(cntryWC$Team, eloRatings$Team)

matches <- lapply(ind, function(j) {
  cntry <- cntryLinks[j]
  a <- readHTMLTable(paste0(eloURL, cntry), which = 2, stringsAsFactors = FALSE,
                     encoding = "utf-8")
  a <- a[-1, c(1:3, 5:6)]
  a <- a[nchar(a$V3) == 2 & nchar(a$V6) == 8, ]

  teamArg <- eloRatings$Team[j]
  if(teamArg == "Russia")
    teamArg <- "Russia|Commonwealth of Independent States|Soviet Union"
  if(teamArg == "Germany")
    teamArg <- "Germany|West Germany"
  if(teamArg == "England")
    teamArg <- "England|Great Britain"

  teams <- strsplit(a$V2, teamArg)
  teams <- lapply(teams, function(x) {
    if(length(x) == 1)
      c(x, eloRatings$Team[j])
    else
      c(eloRatings$Team[j], x[2])
  })
  teams <- do.call(rbind, teams)

  date <- substr(a$V1, nchar(a$V1)-3, nchar(a$V1))
  scores <- cbind(substr(a$V3, 1, 1), substr(a$V3, 2, 2))
  rating <- cbind(substr(a$V6, 1, 4), substr(a$V6, 5, 8))
  variation <- cbind(gsub("([+|-][0-9]+)([+|-][0-9]+)", "\\1", a$V5),
                     gsub("([+|-][0-9]+)([+|-][0-9]+)", "\\2", a$V5))

  dat <- cbind(date, teams, scores, rating, variation)
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  dat$date <- as.integer(dat$date)
  for(k in 4:9) dat[, k] <- as.numeric(dat[, k])
  names(dat) <- c('Year', 'HomeTeam', 'AwayTeam', 'HomeScore', 'AwayScore', 
                  'HomeRating', 'AwayRating', 'HomeVar', 'AwayVar')
  dat
})

names(matches) <- eloRatings$Team[ind]
save(matches, file = "match_data.RData")
