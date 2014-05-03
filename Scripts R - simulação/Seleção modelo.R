library(lme4)
library(VGAM)


setwd('~/Rubem_Ceratti/Outros/WC')

load('matches_2.RData')

train <- matches_2[matches_2$Year < 2010, ]
test <- matches_2[matches_2$Year >= 2010, ]
test <- test[test$Opp %in% unique(train$Opp), ]


# Modelos:
#   - m0: GLM Poisson -- TeamScore ~ RatingDiff
#   - m1: GLM Poisson -- TeamScore ~ RatingDiff + YearTrans
#   - m2: GLMM Poisson -- TeamScore ~ RatingDiff + (1|Team) + (1|Opp) 
#   - m3: GLMM Poisson -- TeamScore ~ RatingDiff + YearTrans + (1|Team) + (1|Opp)
#   - m4: GLMM Poisson -- TeamScore ~ RatingDiff + YearTrans + (YearTrans|Team) + 
#                                     (YearTrans|Opp)
#   - m5: Loess -- TeamScore ~ RatingDiff

m0 <- glm(TeamScore ~ RatingDiff, poisson, train)
m1 <- glm(TeamScore ~ RatingDiff + YearTrans, poisson, train)
m2 <- glmer(TeamScore ~ RatingDiff + (1|Team) + (1|Opp), train, poisson)
m3 <- glmer(TeamScore ~ RatingDiff + YearTrans + (1|Team) + (1|Opp), train, poisson)
m4 <- glmer(TeamScore ~ RatingDiff + YearTrans + (YearTrans|Team) +
                        (YearTrans|Opp), train, poisson)
m5 <- loess(TeamScore ~ RatingDiff, train)


mods <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5)

sapply(mods, function(m, newdata) {
  predScore <- predict(m, newdata)
  if(class(m)[1] != 'loess')
    predScore <- exp(predScore)
  mean((newdata$TeamScore - predScore)^2, na.rm = TRUE)
}, newdata = test)

#       m1       m2       m3       m4       m5 
# 1.799528 1.766079 1.792980 1.788951 1.787281




# Vizualização das probabilidades em função do RD
dwin <- Vectorize(function(RD, model) {
  RD <- RD/100
  x <- -50:50
  mu1 <- predict(model, data.frame(RatingDiff = RD))
  mu2 <- predict(model, data.frame(RatingDiff = -RD))
  psk <- dskellam(x, mu1, mu2)
  c(sum(psk[x > 0]), sum(psk[x == 0]), sum(psk[x < 0]))
}, 'RD')


rd <- c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 
120L, 130L, 140L, 150L, 160L, 170L, 180L, 190L, 200L, 210L, 220L, 
230L, 240L, 250L, 260L, 270L, 280L, 290L, 300L, 325L, 350L, 375L, 
400L, 425L, 450L, 475L, 500L, 525L, 550L, 575L, 600L, 625L, 650L, 
675L, 700L, 725L, 750L, 775L, 800L)


# Vizualização Modelo 5 (Loess)
winTable <- t(dwin(rd, m5))
pdf('Scripts R - simulação v5/Probs_loess.pdf', w = 10)
matplot(rd, winTable, ty = 'l', col = c(1, 4, 2), ylim = 0:1, ylab = 'Probability',
        lwd = 1, lty = c(1, 1, 1))
legend(50, .9, c('Win', 'Draw', 'Lose'), col = c(1, 4, 2), lty = c(1, 1, 1), lwd = 2)
dev.off()