# Função de conveniência para Apply+Combine
rlapply <- function(X, FUN, ...) do.call(rbind, lapply(X, FUN, ...))



# Função para simulação do placar um jogo
matchScores <- function(RD, model) {
  nr <- length(RD)
  RD <- RD/100
  mu1 <- predict(model, data.frame(RatingDiff =  RD))
  mu2 <- predict(model, data.frame(RatingDiff = -RD))
  t1gf <- rpois(nr, mu1); t2gf <- rpois(nr, mu2)
  cbind(t1gf, t2gf)
}



# Função para simulação de jogos da fase de grupos
groupStage <- function(x, model) {
  games <- as.data.frame(t(combn(x$Team, 2)))

  rT1 <- x$Rating[match(games$V1, x$Team)]
  rT2 <- x$Rating[match(games$V2, x$Team)]
  RD <- rT1 - rT2

  score <- matchScores(RD, model)
  t1gf <- score[, 1]; t2gf <- score[, 2]
  t1gd <- t1gf - t2gf; t2gd <- - t1gd

  t1pt <- ifelse(t1gd > 0, 3, ifelse(t1gd < 0, 0, 1))
  t2pt <- ifelse(t2gd > 0, 3, ifelse(t2gd < 0, 0, 1))

  results <- data.frame(Team = unlist(c(games)), GF = c(t1gf, t2gf), 
                        GD = c(t1gd, t2gd),Pts = c(t1pt, t2pt))

  results <- aggregate(. ~ Team, results, sum)
  attr(results, 'Group') <- x$Group[1]  
  results <- results[with(results, order(Pts, GD, GF, decreasing = T)), ]
  attr(results, 'Advance') <- groupAdvance(results)
  results
}



# Países classificados e posição
groupAdvance <- function(x) {
  x <- x[1:2, ]
  x$Id <- paste0(1:2, attr(x, "Group"))
  x[, c('Team', 'Id')]
}



# Função para simulação de jogos das fases eliminatórias
knockoutStage <- function(x, model) {
  playRound <- function(teams, x, model) {
    game <- x[match(teams, x$Id), ]
    score <- 0
    RD <- with(game, Rating[1] - Rating[2])
    while(score == 0) {
      score <- diff(c(matchScores(RD, model)))
    }
    winner <- ifelse(score < 0, 1, 2)
    game[winner, 'Id']
  }

  AH <- LETTERS[1:8]
  pos <- c(paste0(rep(1:2, 4), AH), paste0(rep(2:1, 4), AH))
  survivors <- data.frame(Position = pos, index = rep(1:8, each = 2),
                          stringsAsFactors = FALSE)

  standings <- vector('list', 5)

  for(i in c(4, 2, 1, 0)) {
    pairs <- with(survivors, split(Position, index))
    winners <- sapply(pairs, playRound, x = x, model = model)
    survInd <- match(winners, survivors$Position)
    losers <- x[match(survivors[-survInd, 1], x$Id), 'Team']
    survivors <- survivors[survInd, ]
    losersPos <- switch(paste(i), '4' = 'R32', '2' = 'R16', '1' = 'Semis',
                        '0' = 'Final')
    standings[[i+1]] <- data.frame(Team = losers, Pos = losersPos)
    if(i == 0)
      break
    survivors$index <- rep(1:i, each = 2)
  }

  standings <- do.call(rbind, standings)
  champion <- data.frame(Team = x[match(winners, x$Id), 'Team'], 
                         Pos = 'Champion')
  rbind(champion, standings)
}



# Função para simulação de um torneio completo
tournamentWC <- function(data, model) {
  # Stage 1 -- Group matches
  data_s1 <- split(data, data$Group)
  stage1 <- lapply(data_s1, groupStage, model = model)
  stage1Pass <- rlapply(stage1, attr, 'Advance')

  # Stage 2 -- Knock-out matches
  data_s2 <- merge(data[, -2], stage1Pass)
  stage2 <- knockoutStage(data_s2, model)

  # Tournament results
  list(standings = stage2, grpStage = stage1)
}