
#TEST NON PARAMETRIQUE

#7.
#creation du jeu de donnees
ratio <- c(9,3,3,1)
observe<-c(1528,106,117,381)
mat<-matrix(c(ratio, observe), nrow=2, ncol=4, byrow=T)
rownames(mat) <- c("ratio","observe")
colnames(mat) <- c("violet,long", "violet,rond", "rouge,long", "rouge,rond")

#a. 
#pour calculer la valeur théorique on utilise la fonction suivante (cette dernière applique la formule : (sommeration/ratio)*n)
#n correspond au nombre total de sujets 
valth<- function(mat){
  a<-list()
  sommeratio<-0
  total<-0
  for (i in 1:4){
    sommeratio<-sommeratio+mat[1,i]
    total<-total+mat[2,i]
  }
  for(i in 1:4){
    valthe<-mat[1,i]/sommeratio*total
    a<-append(a,valthe)
    valhe<-0
  }
 
  return(a)
}

b<-valth(mat)
b[[2]]

khideux<-function(mat,b,n){
  res<-0
  for(i in 1:n){
    s<-mat[2,i]-b[[i]]
    t<-s^2
    res<-res+ (t/b[[i]])
  }
  return(res)
}

khideux(mat,b,4)

#On pose : HO la ratio utilisé est bon
#comme resultat du khi2 on obtient 966.61 qui est superieur a toutes les valeurs de la table du khi deux donc on rejette H0
#donc le ratio est mauvais

#8. 

#creation du jeu de donnees
common_n <- c(29,5,46)
atypical_n<-c(40,32,8)
melanome_n<-c(18,22,0)
mat_mel<-matrix(c(common_n, atypical_n,melanome_n), nrow=3, ncol=3, byrow=T)


common <- c(20,60)
atypical<-c(29,51)
melanome<-c(12,28)
mat_mela<-matrix(c(common, atypical,melanome), nrow=3, ncol=2, byrow=T)

#on calcul la valeur theorique pour chaque valeur du jeu de donnees
#on utilise la formule suivante : element = (somme ligne* somme colonne)/n
valtheorique<-function(mat,nro,rco){
  matri<-matrix(nrow = nro,ncol=rco)
  n<-sum(mat)
  for(i in 1:nro){
    for(j in 1:rco){
      matri[i,j]=sum(mat[i,])*sum(mat[,j])/n
    }
  }
  return(matri)
}



val1<-valtheorique(mat_mel,3,3)
val2<-valtheorique(mat_mela,3,2)


khideuxb<-function(mat,b,a,v){
  res<-0
  for(i in 1:a){
    for(j in 1:v){
      s<-mat[i,j]-b[i,j]
      t<-s^2
      res<-res+ (t/b[i,j])
    }

  }
  return(res)
}

#Pour savoir si un jeu de données est utilisable on va calculer sa valeur en utilisant khi2 puis on comparera à la valeur présente dans la table

#pour le premier tableau
khideuxb(mat_mel,val1,3,3)
#resultat 75.15 
#lecture dans la table 5.99
#or 75.15>5.99 donc on ne peut pas utiliser ceci pour détecter le mélanome

khideuxb(mat_mela,val2,3,2)
#resultat 2.39
#lecture dans la table 3.84
#2.39<3.84 ce critère pourrait aider à la détection du mélanome

#9.
# Le test t de Student est classé comme paramétrique car il se base sur des donnees quatitatives, il faut donc certaines conditions pour que les tests soient valides
# pour pouvoir utiliser le test de Student il faut que les donnees associees suivent une loi normale et les variances des echantillons doivent etre homogenes.
# l'utiliser sur des donnees qualitatives rendrait le resultat non fiable

# Le test du khi deux est classé comme non parametrique car il se base sur des donnees qualitatives (variables discontinues).
# Pour ce type de tests on ne se base pas sur les distributions statistiques on peut donc les utiliser sans avoir de conditions prealables


#10. 
# on appliquer spearman mais pas pearson aux donnees qualitatives
