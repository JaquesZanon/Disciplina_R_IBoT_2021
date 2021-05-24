install.packages("vegan")

library(vegan)


source("coldiss.R")

rowSums(spe)

# Remove site 8
spe <- spe[-8, ]
env <- env[-8, ]
spa <- spa[-8, ]


spe.db <- vegdist(spe, method="bray")	# method = "bray" (default)
spe.db

# scale(env) = decostand(spe, "standardize")


spe.norm <- decostand(spe, "standardize")

spe.dc <- vegdist(spe.norm, method="euclidian")

# vegdist(spe, method="euclidian") = dist(spe.norm)

plot(spe.db, spe.dc)

env[,1:3]
env$das
env[,c("das","alt","pen")]



spe.hel <- decostand(spe, "hel")# ponderar sp muito abundante
spe.dh <- vegdist(spe.hel, method="euclidian")

plot(spe.db, spe.dh)




coldiss(spe.dh, nc = 16, diag = TRUE)
coldiss(spe.db, nc = 16, diag = TRUE)


env.norm <- decostand(env, "standardize")

env.dc <- vegdist(env.norm, method="euclidian")

coldiss(env.dc, nc = 16, diag = TRUE)

env.dc

env.das <- decostand(env$das, "standardize")

env.das.dc <- vegdist(env.norm, method="euclidian")
coldiss(env.das.dc, nc = 16, diag = TRUE)




## R-mode correlation matrices

# Pearson r linear correlation among environmental variables

env.cor <- cor(env)	
env.cor
as.dist(env.cor)


heatmap(env.cor)

round(env.cor, 2)


pairs(
  env,
  cex.labels = 2,
  lower.panel = panel.smooth,
  upper.panel = panel.smooth,
  #diag.panel = panel.hist,
  main = "Pearson Correlation Matrix"
)








spe.ch.ward <- hclust(spe.ch, method = "ward.D2")
spe.ch.ward.coph <- cophenetic(spe.ch.ward)
cor(spe.ch, spe.ch.ward.coph)