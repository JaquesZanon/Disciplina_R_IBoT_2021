### PCA ###
install.packages(vegan)
library(vegan)

# env <- env[-8, ]# para alex

head(env)
head(scale(env))

env.norm <- decostand(env, "standardize")
env.norm <- scale(env)

mob <- rda(env.norm)
?rda

summary(mob)

plot(mob)

par(mfrow=c(1,2))

env.sco <- scores(mob)$sites
plot(env.sco,type='n',axes=T)
text(env.sco, row.names(env.sco),col='red')
abline(h=0,v=0)
axis(1)
axis(2)

env.loading <- scores(mob)$species
plot(env.loading,type='n')
text(env.loading, row.names(env.loading),col='blue')
abline(h=0,v=0)

par(mfrow=c(1,2))

plot(spa,
     main = "Site Locations",
     xlab = "x coordinate (km)",
     ylab = "y coordinate (km)",
     cex = abs(env.sco[,2]+0.8), 
     col= ifelse(env.sco[,1]<0,"black","red"))

lines(spa, col = "light blue")# adicionar linha azul

text(spa, row.names(spa), cex = 1, col = "red")

plot(y=env.sco[,1],
     x=env$alt,
     ylab = "Scores PC1",
     xlab = "alt")

plot(x=env.sco[,1],
     #x=env$nit,
     ylab = "Scores PC1",
     xlab = "nit")

plot(mob)










library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(rioja)

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

fviz_cluster(k2, data = df)

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")



# cluster

spe.ch.ward <- hclust(spe.dh, method = "ward.D2")
plot(spe.ch.ward, hang=-1,
     labels = rownames(spe), 
     main = "Chord - Ward")


rect.hclust(spe.ch.ward, k=3, border="red")
gr4 <- cutree(spe.ch.ward, k = 4)


hcd <- as.dendrogram(spe.ch.ward)
plot(hcd, type = "triangle", ylab = "Height")



# Horizontal plot
plot(hcd,  xlab = "Height",
     #nodePar = nodePar, 
     horiz = TRUE)






heatmap(
  as.matrix(spe.dh),
  
  symm = TRUE,
  margin = c(3, 3)
)

# Ward clustering
spe.ch.ward.coph <- cophenetic(spe.ch.ward)
cor(spe.dh, spe.ch.ward.coph)

# Default method CONISS
# On the percentage difference dissimilarity matrix
spe.chcl <- chclust(vegdist(spe))

# Cut the dendrogram in 4 clusters
k <- 4
gr4 <- cutree(spe.chcl, k = k)

