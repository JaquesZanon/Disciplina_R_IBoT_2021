#library(datasets)
library(vegan)
library(BiodiversityR)
library(adespatial)


install.packages(c("BiodiversityR","adespatial"))



#data(iris)
#data(mite)
#data(duna)
#summary(iris)
#plot(iris)


##### PCoA ou MDS ##### 


?cmdscale

bjo1 <- decostand(spe, method= "hel")

spe.dh <-vegdist(bjo1, method='eu')



loc <- cmdscale(spe.dh)
loc
plot(loc, col=k2$cluster)




loc.hel  <- cmdscale(spe.dh, eig=TRUE)
loc.bray <- cmdscale(vegdist(spe, method = "bray"),eig=TRUE)
loc.eu   <- cmdscale(vegdist(spe, method = "eu"),eig=TRUE)
loc.manh <- cmdscale(vegdist(spe, method = "manhattan"),eig=TRUE)
loc.chord <- cmdscale(vegdist(spe, method = "chord"),eig=TRUE)
loc.chis <- cmdscale(vegdist(spe, method = "chisq"),eig=TRUE)




par(mfrow=c(2,3))


plot(loc.hel$points, col=k2$cluster, pch=19, main="Hellinger")
plot(loc.bray$points, col=k2$cluster,pch=19, main="Bray")
plot(loc.eu$points, col=k2$cluster,pch=19, main="Euclidean")
plot(loc.manh$points, col=k2$cluster,pch=19, main="Manhatan")
plot(loc.chord$points, col=k2$cluster,pch=19, main="chord" )
plot(loc.chis$points, col=k2$cluster,pch=19, main="chisq")



library(BiodiversityR)



loc.hel<- add.spec.scores(loc.hel, 
                          spe.hel, 
                          method="pcoa.scores", 
                          Rscale=FALSE, scaling=1, 
                          multi=1)
loc.hel$eig.cumpercen


plot(loc.hel$points, 
     col=k2$cluster,
     axes = FALSE)
axis(1)
axis(2)

abline(h=0,v=0, col='grey')

text(loc.hel$cproj*10, 
     rownames(loc.hel$cproj), 
     col='blue',
     cex=1)


#arrows(0,0,loc.hel$cproj[,1]*0.8, loc.hel$cproj[,2]*0.8, length = 0.1)

plot(loc.hel$points, 
     col="gray", pch =19)

ordispider(loc.hel$points, 
           k2$cluster, 
           label=TRUE,
           col = c("black","red"))

ordihull(loc.hel$points,
         k2$cluster,
         label=TRUE,
         col = c("black","red"))

plot(loc.hel$points, 
     col="gray", pch =19)


ordisurf(loc.hel$points,
         env$alt,
         col = 'blue'
         )

points(loc.hel$points,
      col = k2$cluster,
      pch = 19,
      cex=1)

















#### NMDS #####

set.seed(42)

example_NMDS <- metaMDS(spe.dh ,k=2, trymax=100)

example_NMDS


NMDS.sco<-scores(example_NMDS)


stressplot(example_NMDS)

plot(NMDS.sco, col=k2$cluster, pch=19)
ordispider(NMDS.sco, k2$cluster,
           label=TRUE)





ordihull(example_NMDS,groups=k2$cluster,draw="polygon",col="grey90",label=F)





##### RDA ######
# Physiography (upstream-downstream gradient)
envtopo <- env[, c(1 : 3)]
names(envtopo)
# Water quality
envchem <- env[, c(4 : 10)]
names(envchem)



spe.rda <- rda(spe.hel ~ ., env)

spe.rda <- rda(spe.hel, env)

summary(spe.rda)	



R2adj <- RsquareAdj(spe.rda)$adj.r.squared
R2adj



# Global test of the RDA result
anova(spe.rda, permutations = how(nperm = 999))



plot(spe.rda,
     scaling = 1,
     display = c("sp", "lc", "cn"),
     main = "Triplot RDA spe.hel ~ env"
)
spe.sc1 <- 
  scores(spe.rda, 
         choices = 1:2, 
         scaling = 1, 
         display = "sp"
  )
arrows(0, 0, 
       spe.sc1[, 1] * 0.92,
       spe.sc1[, 2] * 0.92,
       length = 0, 
       lty = 1, 
       col = "red"
)



plot(spe.rda, 
     scaling = 1, 
     main = "Triplot RDA spe.hel ~ env3 - scaling 1 - wa scores"
)
arrows(0, 0, 
       spe.sc1[, 1] * 0.92, 
       spe.sc1[, 2] * 0.92, 
       length = 0, 
       lty = 1, 
       col = "red"
)



## Partial RDA: effect of water chemistry, holding physiography


# Simple syntax; X and W may be in separate tables of quantitative 
# variables




spechem.physio <- rda(spe.hel, envchem, envtopo)

summary(spechem.physio)

plot(spechem.physio)


anova(spechem.physio, permutations = how(nperm = 999))

plot(spechem.physio, 
     scaling = 1, 
     display = c("sp", "lc", "cn"), 
     main = "Triplot RDA spe.hel ~ chem | Topo - scaling 1 - lc scores")


spe3.sc <- 
  scores(spechem.physio, 
         choices = 1:2, 
         scaling = 1, 
         display = "sp"
  )
arrows(0, 0, 
       spe3.sc[, 1] * 0.92, 
       spe3.sc[, 2] * 0.92, 
       length = 0, 
       lty = 1, 
       col = "red"
)



### Forward selection of explanatory variables
library(adespatial)
# RDA with all explanatory variables except dfs
spe.rda.all <- rda(spe.hel ~ ., data = env)
# Global adjusted R^2
(R2a.all <- RsquareAdj(spe.rda.all)$adj.r.squared)

# Forward selection using forward.sel()
forward.sel(spe.hel, env, adjR2thresh = R2a.all)

step.backward <-
  ordistep(spe.rda.all,
           permutations = how(nperm = 499))
RsquareAdj(step.backward)

## Parsimonious RDA
(spe.rda.pars <- rda(spe.hel ~ alt + oxy + dbo, data = env))
anova(spe.rda.pars, permutations = how(nperm = 999))
anova(spe.rda.pars, permutations = how(nperm = 999), by = "axis")
(R2a.pars <- RsquareAdj(spe.rda.pars)$adj.r.squared)







par(mfrow = c(1, 3), mar = c(1, 1, 1, 1))

showvarparts(2, bg = c("red", "blue"))


spe.part.all <- varpart(spe.hel, envchem, envtopo)


plot(spe.part.all, 
     digits = 2, 
     bg = c("red", "blue"),
     Xnames = c('envchem', 'envtopo'))
