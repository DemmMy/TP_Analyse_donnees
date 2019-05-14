
#1. Tracez en dimension 3 le nuage de 10 points
# On récupère les données puis on peut tracer les graphe à partir des données présentes dans les 
# trois colonnes de individus

library(psych)
individus <- read.delim("C:/Users/gabrielle/Desktop/polytech/4A/Analyse_donnees/TP2/data1TP2.txt")
individus <- na.omit(individus)


A <- individus[,1:3]
scatterplot3d(A)

#2. Écrivez le tableau centré B, et la matrice de covariance V.
# Dans B on obtient la matrice centrée (center) de A (mais pasz réduite car scale=FALSE)
# Dans C on a la matrice de covariance de A
B <- scale(A, center = TRUE, scale = FALSE)
B
V <- cov(A)
V


#3.Déterminez la représentation spectrale (valeurs propres et vecteurs propres de V)
#La fonction eigen renvoie deux paramètres les valeurs propres qui sont dans $values 
# et les vecteurs propres qui sont dans $vectors
valeurP <- eigen( V)$values
vecteurP <- eigen( V)$vectors


#4. Indiquez les axes principaux (dans l'ordre)

#Le premier axe principal est le premier vector que l'on obtient avec la fonction eigen
#https://bioinfo-fr.net/lanalyse-en-composantes-principales-avec-r#exemple
pca <- prcomp(A)
pca$rotation
ecartType <- pca$sdev
proporVariance <- 100 * pca$sdev^2 / sum(pca$sdev^2)  
varianceTotale <- sum(100 * (pca$sdev^2)[1:2] / sum(pca$sdev^2)) # variance totale entre 2 premiÃ¨res colonnes 

ecartType
proporVariance
varianceTotale
plot(pca)

#5. Générez le tableau C en multipliant B par les vecteurs propres de V
# Pour ce faire on utilise la formule %*% qui permet de multiplier deux matrices

C <- B  %*%  vecteurP
princomp(A)$scores

# Si on compare nos données avec la fonction princomp(A)$scores on remarque que le résultat obtenu est le même


#6.Observez en dimension 3 le nuage de points avec tracé du premier axe principal

pc1 <- pca$rotation[,1]

pc1 <- as.vector(pc1)
x <- c(pca[2]$rotation[1,1],0)
y <- c(pca[2]$rotation[2,1],0)
z <- c(pca[2]$rotation[3,1],0)

plot3d(x,y,z, type="l")
scatter(pc2,pc3)

traceAxe <- function(A,pc1) {
  C <- matrix(nrow = 10,ncol=1);
  for( i in 1:10){
    C[i,1] <- pc1[1] * A[i,1] + pc1[2] * A[i,2] + pc1[3] * A[i,3]
  }
  return(C)
}

traceAxe(A,pc1)
plt<-scatterplot3d(pca$rotation)
plt$points3d(pc1, type="l", col="blue", lwd=2)


#ou
scatterplot3d(x=c(0,-300*vecteurP[1,1]), y= c(0,-300*vecteurP[2,1]), z= c(0,-300*vecteurP[3,1]), type="l")
scatterplot3d(x=c(0,-300*vecteurP[1,2]), y= c(0,-300*vecteurP[2,2]), z= c(0,-300*vecteurP[3,2]), type="l")
scatterplot3d(x=c(0,-300*vecteurP[1,3]), y= c(0,-300*vecteurP[2,3]), z= c(0,-300*vecteurP[3,3]), type="l")

#7.Représentez le nuage de points en dimension 2, projetés des points de départ sur le plan formé des
# deux premiers axes principaux.

plot(C[,1], C[,2])
