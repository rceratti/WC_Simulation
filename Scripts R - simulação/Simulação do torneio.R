library(doParallel)
library(reshape2)



# Carregamento dos dados
setwd('~/Rubem_Ceratti/Outros/WC')

load('matches_2.RData')
load('countries_1.RData')


# Carregamento das fun��es de jogo e de formata��o
setwd(paste0(getwd(), '/Scripts R - simula��o v5'))
source('Fun��es de jogos.R')
source('Fun��es de formata��o dos resultados.R')



# Modelo: Loess -- Utilizado nas simula��es para gerar placares
m0 <- loess(TeamScore ~ RatingDiff, matches_2)



# Simula��es do torneio
cl <- makeCluster(3)
registerDoParallel(cl)
snow::clusterSetupRNGstream(cl, seed = 3245636)

n <- 1e4

system.time({
  simWC <- foreach(i = 1:n) %dopar% tournamentWC(countries_1, m0)
})

stopCluster(cl)

save(simWC, file = 'WC_simulation.RData')



# Resultados
final_results <- simFormat(simWC)
write.csv(final_results, 'Simulation Results WC v5.csv', row.names = FALSE)



# Gr�fico com as probabilidades de vencer
res <- final_results$Champion
names(res) <- final_results$Team

pdf('WC_winners_v5.pdf', w = 11, h = 8)
p <- par(mar = c(8, 5, 4, 1))
barplot(sort(res, decreasing = T), las = 2, ylim = c(0, .5), 
        ylab = "Probability of Winning")
par(p)
dev.off()