# Formatação dos resultados para a fase de grupos
gsFormat <- function(j, data) {
  gs <- lapply(data, '[[', j)

  # Average Pts, GF, GD
  gsPts <- do.call(rbind, gs)
  gsPts <- aggregate(. ~ Team, gsPts, mean)

  # Probability of being First and Second 
  gsPos <- rlapply(gs, attr, 'Advance')
  gsPos <- dcast(gsPos, Team ~ Id, length, value.var = 'Id')
  gsPos[, 2:3] <- gsPos[, 2:3]/length(data)
  names(gsPos)[2:3] <- c('First', 'Second')

  # Combining both statistics 
  gs1 <- merge(gsPts, gsPos, by = 'Team', all.x = TRUE) 
  gs1$Group <- rep(attr(gs[[1]], "Group"), 4)
  gs1
}



# Formatação dos resultados para a fase eliminatória
ksFormat <- function(data) {
  ks <- do.call(rbind, data)
  ks <- dcast(ks, Team ~ Pos, length, value.var = 'Pos')

  # Averaging out the simulation outcomes for the knock-out stage
  for(i in 1:nrow(ks)){
    ks[i, 2:6] <- cumsum(c(ks[i, 2:6]))/length(data)
  }

  ks[, c(1, 6:2)]
}



# Função para formatação final dos resultados da simulação
simFormat <- function(simulation) {
  simulation <- lapply(1:2, function(i) lapply(simulation, '[[', i))

  # Knock-out stage
  stage2 <- simulation[[1]]
  stage2_1 <- ksFormat(stage2)

  # Group stage
  stage1 <- simulation[[2]]
  stage1_1 <- rlapply(1:8, gsFormat, stage1)
  
  # Combining group and knock-out stages
  final <- merge(stage1_1, stage2_1, by = 'Team', all.x = TRUE, sort = FALSE)
  final[, c(7, 1:6, 8:ncol(final))]
}