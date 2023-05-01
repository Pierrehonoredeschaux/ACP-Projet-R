
library (FactoMineR)
library(openxlsx)

data("longley")
head(longley)

# Statistiques descriptives 

summary(longley)

# Ecart-type non corrigé

sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}
apply(longley, 2, sd.p)

# Données centrées et réduites

longley_cr=scale(longley, center = T, scale = T)
View(longley_cr)

##### ETAPE n°2 #####

# Matrice des variances covariances 

# Le code ci-dessous donne la même chose juste arrondi à 3 chiffres après la virgule
round(var(longley_cr),3)

# Matrice des corrélations R 

# Le code ci-dessous donne la même chose juste arrondi à 3 chiffres après la virgule
round(cor(longley_cr),3)

# Corrélation (forte, faible, positive, négative) entre les variables à commenter

# Les MVC et corrélations sont identiques car les données sont centrées et réduites

##### ETAPE n°3 #####

# Analyse en composantes principales

acp = PCA(longley_cr, scale.unit=T)

# R va vous afficher un graphique qu'il ne faut pas commenter pour l'instant

# On affiche tous les objets que fournit la fonction PCA, on va utiliser les infos contenus dans ces objets pour la suite

acp

##### ETAPE n°4 #####

# Valeurs propres des dimensions (axes ou composantes)

acp$eig

# Le code ci-dessous est pareil pour seulement 3 chiffres après la virgule 
round(acp$eig,3)


# Eboulis des pourcentages de variance des différentes dimensions

barplot(acp$eig[,3], main = "Eboulis des pourcentage de variance", 
        names.arg = paste("dim",1:nrow(acp$eig)))

##### ETAPE n°5 #####

# Vecteurs propres 

vecpropres=sweep(acp$var$coord,2,sqrt(acp$eig[1:ncol(acp$var$coord),1]),FUN="/")
vecpropres



# Qualité de représentation des variables sur les axes retenus

round(acp$var$cos2[,1:2],4)


##### ETAPE n°6 ##### 

### VARIABLES ###

# Coordonnées des variables sur les axes retenus: les 3 codes donnent la même chose
# Les coordonnées donnent la longueur des flèches qu'on voit sur le graphique des variables et des individus


acp$var$coord [,1:2]
round (acp$var$coord [,1:2],4)
round (acp$var$cor[,1:2],4)

# Graphe des variables sur le premier plan

plot (acp, choix = "var")


### INDIVIDUS ### 

# Coordonnées des individus sur les axes (CP)

acp$ind$coord

round(acp$ind$coord,2)


# J'affiche les coordonnées des individus seulement sur les axes retenus

acp$ind$coord[,1:2]
round (acp$ind$coord[,1:2],2)

# Qualité de représentation des individus sur les différents axes (cos2)

round(acp$ind$cos2[,1:2],4)

# Attention coordonées de représentation est différent de qualité de représentation 

#Graphique des individus sur le premier plan

plot (acp, choix = "ind", axes = 1:2)


plot (acp, choix = "ind", axes = 2:3)

# On a résumé tous les résultats de l'ACP dans acp il suffit juste de l'afficher avec "summary"

summary(acp)


























