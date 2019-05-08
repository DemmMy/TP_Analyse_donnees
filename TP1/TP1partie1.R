d <- read.delim("C:/Users/gabrielle/Desktop/polytech/4A/Analyse_donnees/TP1/data1TP1.txt", header=TRUE, sep="\t")
#1.
#Affichage du nuage de points

plot(d$A,d$Y)
plot(d$B,d$Y)
plot(d$C,d$Y)
plot(d$D,d$Y)
plot(d$E,d$Y)

#Lorsque l'on affiche les nuages de points on constate qu'ils sont tous différents on observe autant des nuages de points décroissants/croissants et d'autres complètement anachiques

#2.
#coefficient r de Pearson

coeff_r<-function(x,y){
  a<-cov(x, y, use = "everything",method = "pearson")  #ici on utilise cette fonction pour calculer la covariance de X et Y
  xi<-sqrt(var(x))
  yi<-sqrt(var(y))
  return(a/(xi*yi))
}
 #Comparaisons entre la fonction cree et celle existante
coeff_r(d$A,d$Y)
cor(d$A,d$Y)
coeff_r(d$B,d$Y)
cor(d$B,d$Y)
coeff_r(d$C,d$Y)
cor(d$C,d$Y)
coeff_r(d$D,d$Y)
cor(d$D,d$Y)
coeff_r(d$E,d$Y)
cor(d$E,d$Y)

#On remarque que les résultats entre la fonction cree et celle déjà existante sont identiques
#A possede le plus petit coefficient de correlation car sa courbe est strictement decroissante

#3.
#fonction du coefficient de' spearman (q3ex1)
coeff_sp<-function(x,y,n){
  somme<-0
  rangx<-rank(x) #renvoie le rang de toutes les valeurs de X dans un tableau
  rangy<-rank(y)
  for(i in 1:n){
    somme<-somme+(rangx[i]-rangy[i])^2
  }
  res<-6*somme
  fin<-1-(res/(n^3-n))
  return(fin)
}

coeff_sp(d$A,d$Y,15)
cor(d$A,d$Y, method='spearman')
coeff_sp(d$B,d$Y,15)
cor(d$B,d$Y, method='spearman')
coeff_sp(d$C,d$Y,15)
cor(d$C,d$Y, method='spearman')
coeff_sp(d$D,d$Y,15)
cor(d$D,d$Y, method='spearman')
coeff_sp(d$E,d$Y,15)

#ici on cosntate que les résultats obtenus sont légèrement différents mais on peut expliquer cet écart par le nombre de calculs
#ce qui peut amener à une imprécision

#Lorsque l'on compare les deux méthodes de calculs on obtient les résultats suivants :

#    pearson:          spearman:
#  A : -0.9722452      -0.9973214 
#  B : 0.9815886       0.9982143
#  C : 0.4119462       0.4169643
#  D : 0.7513686       1
#  E : 0.210302        0.3419643
# 
# pearson d'utilise lors de l'analyse des relations lineaires 
# spearman s'utilise pour les relations non lineaires et monotones
# Ayant des valeurs complètement différentes on observe que des algorithmes sont plus adaptés à un certain jeux de données
# Par exemple D étant non linéaire lorsque l'on applique spearman le résultat passe à 1


#pour E on remarque que les résultats sont différents car E est non linéaire et non monotone
cor(d$E,d$Y, method='pearson')
cor(d$E,d$Y, method='spearman')
