d <- read.delim("C:/Users/gabrielle/Desktop/polytech/4A/Analyse_donnees/TP1/data2TP1.txt", header=TRUE, sep="\t")

#TEST PARAMETRIQUE
#5. 
score_t<-function(donnees, moy_th,n){
  a<-abs(mean(donnees)-moy_th)
  sigm<-sqrt(var(donnees))
  res<-a/(sigm/sqrt(n))
  return(res)
}

score_t(d$Marseille,19,length(d$Marseille))
#On veut savoir si l'inflation a influencÈe le cout de la vie ‡ Marseille 
#Tout d'abord on pose nos hypotheses
#H0 : il n'y a pas de lien entre l'inflation et le cout de la vie
#H1 : il y a un lien entre l'inflation et le cout de la vie
#on applique la fonction de score avec les donnees de Marseille et la valeur donnee soit 19 euros
#comme rÈsultat on obtient 2.177369 
#maintenant on va lire dans le tableau la valeur correspondant soit 2.145 (colonne 95% et ligne 14 (n-1))
#comme 2.145<2.17 on rejette l'hypothese H0 au seuil 5%
#si on avait eu l'inverse on n'aurait pas conclut


#6.
score_t2<-function(x,y){
  a<-abs(mean(x)-mean(y))
  a1<-var(x)/length(x)
  a2<-var(y)/length(y)
  res<-a/sqrt(a1+a2)
  
  return(res)
}

score_t2(d$Marseille,d$Aix)

#Maintenant nous allons Ètudier s'il existe une dÈpendance significative entre le cout de la vie a Aix et a Marseille
#H0 il n'existe pas de diff√©rence significative entre Marseille-Aix
#H1 il existe une diff√©rence significative entre Marseille-Aix
#On utilise la meme technique que precedemment 
#on obtient comme r√©sultat 2.32
#dans le tableau on lit 2.048 (degr√© de libert√© length(x)+length(y)=30 -2 =28)

#comme 2.32>2.048 on rejette H0 au seuil 5%

#avec alpha = 2% on a 2.468>2.32 donc on ne peut pas conclure
#la valeur d'alpha influe sur le r√©sultat que l'on peut avoir