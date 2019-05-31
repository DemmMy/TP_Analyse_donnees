matrice<-matrix(nrow = 300,ncol = 2)
a<-matrix(nrow = 3,ncol = 2)

unifx<-runif(100,0,1)
unify<-runif(100,0,1)

gaussx1<-rnorm(100, mean = 4, sd = 1)
gaussy1<-rnorm(100,4,1)

gaussx2<-rnorm(100, mean = 0.5, sd = sqrt(2))
gaussy2<-rnorm(100, mean = 6, sd = sqrt(2))

for(i in 1:100){
  matrice[i,1]<-unifx[i]
  matrice[i,2]<-unify[i]
  matrice[100+i,1]<-gaussx1[i]
  matrice[100+i,2]<-gaussy1[i]
  matrice[200+i,1]<-gaussx2[i]
  matrice[200+i,2]<-gaussy2[i]
}
a[1,1]<-matrice[1,1]
a[1,2]<-matrice[1,2]
a[2,1]<-matrice[2,1]
a[2,2]<-matrice[2,2]
a[3,1]<-matrice[3,1]
a[3,2]<-matrice[3,2]
plot(matrice)

D<-dist(matrice, method="euclidean")
D<-as.matrix(D)
D[upper.tri(D)]<-NA
diag(D)<-NA
s<-which(D==min(D,na.rm=T), arr.ind = T)
s[1,1]

class<-function(K,matrice){
  D<-dist(matrice, method="euclidean")
  D<-as.matrix(D)
  D[upper.tri(D)]<-NA
  diag(D)<-NA
  while(nrow(D)>K){
    s<-which(D==min(D,na.rm=T), arr.ind = T)
    elim1<-s[1,1]
    elim2<-s[1,2]
    
    moyenneX <- mean(matrice[elim1,1],matrice[elim2,1])
    moyenneY <- mean(matrice[elim1,2],matrice[elim2,2])
    matrice[elim1,1] <- moyenneX
    matrice[elim1,2] <- moyenneY
    
    rownames(matrice)[elim1]<-paste(rownames(matrice)[elim1],rownames(matrice)[elim2])
    
    matrice <- matrice[-elim2,]
    D <- dist(matrice,method="euclidean")
    D <- as.matrix(D)
    
    D[upper.tri(D)] <- NA
    diag(D) <- NA
  
  }
  return(matrice)
  
}
a<-class(3,matrice)
plot(matrice)
points(a,col="red")


# D<-dist(matrice, method="euclidean")
# AscH<-hclust(D, method="complete")
# plot(AscH, cex=0.6, hang=-1)
# cluster<-cutree(AscH,3)


