###
spe <- read.csv("DoubsSpe.csv", header = TRUE, row.names = 1, sep = ";")
head(spe)

spa <- read.csv("DoubsSpa.csv", header = TRUE, row.names = 1, sep = ",")
head(spa)

env <- read.csv("DoubsEnv.csv", header = TRUE, row.names = 1, sep = ",")
head(env)





spe                       # Display the whole data frame in the 
# console
# Not recommended for large datasets!

head(spe)                 # Display only the first 6 lines

tail(spe)                 # Display only the last 6 rows

nrow(spe)                 # Number of rows (sites)

ncol(spe)                 # Number of columns (species)

dim(spe)                  # Dimensions of the data frame (rows, 
# columns)


colnames(spe)             # Column labels (descriptors = species)

rownames(spe)             # Row labels (objects = sites)

summary(spe)              # Descriptive statistics for columns


range(spe)




# Number of absences
sum(spe == 0)
spe == 0




# Proportion of zeros in the community data set
sum(spe == 0) / (nrow(spe) * ncol(spe))





# Map of the locations of the sites ===============================

head(spa)

plot(spa$x, spa$y)

plot(spa)

# Plotando as variaveis espacias
plot(spa,
     main = "Site Locations",
     xlab = "x coordinate (km)",
     ylab = "y coordinate (km)")

lines(spa, col = "light blue")# adicionar linha azul

text(spa, row.names(spa), cex = 1, col = "red")

# Entendendo o argumento cex

par(mfrow = c(1,2))

plot(spa,
     main = "Abundance CHA",
     xlab = "x coordinate (km)",
     ylab = "y coordinate (km)",
     cex = spe[,1])
lines(spa, col = "light blue")# adicionar linha azul

plot(spa,
     main = "Abundance LOC",
     xlab = "x coordinate (km)",
     ylab = "y coordinate (km)",
     cex = spe[,4])
lines(spa, col = "light blue")# adicionar linha azul


# plotando 4 species
par(mfrow = c(2,2))

plot(spa, 
     col = "brown", 
     cex = spe$SPI, 
     main = "Brown trout", 
     xlab = "x coordinate (km)", 
     ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

plot(spa, 
     col = "brown", 
     cex = spe$TRU, 
     main = "Grayling", 
     xlab = "x coordinate (km)", 
     ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

plot(spa, 
     col = "brown", 
     cex = spe$BAR, 
     main = "Barbel", 
     xlab = "x coordinate (km)", 
     ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

plot(spa, 
     col = "brown", 
     cex = spe$ABL, 
     main = "Common bream", 
     xlab = "x coordinate (km)", 
     ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

# Locais que cada sp está presente
spe == 0

spe != 0

spe > 0


colSums(spe > 0)




#Riqueza de sp para cada local

riqueza <- rowSums(spe > 0)


plot(riqueza,
     type = "s",
     col="gray",
     main="Species Richness Vs. \n Upstream-Downstrream Gradien", 
     xlab="Positions os sites along the river", 
     ylab= "Species richness")
text(riqueza, row.names(spe), cex=.8, col='red')

plot(spa, 
     col = "brown", 
     cex = riqueza/3, 
     main = "S", 
     xlab = "x coordinate (km)", 
     ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")












hist(colSums(spe > 0), 
     main = "Species Occurrences", 
     right = FALSE, 
     las = 1, 
     xlab = "Number of occurrences", 
     ylab = "Number of species", 
     #breaks = seq(0, 30, by = 5),
     col = "bisque"
)


plot(spa, 
     asp = 1, 
     main = "Map of Species Richness", 
     pch = 21, 
     col = "white", 
     bg = "brown", 
     cex = 5 * sit.pres / max(sit.pres), 
     xlab = "x coordinate (km)", 
     ylab = "y coordinate (km)"
)
