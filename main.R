# Title     : Analyse statistique langage R
# Objective : TP Langage R
# Created by: maxime
# Created on: 06/03/2020

# Le fichier ventes.csv contient des données indiquant les chiffres d’affaires réalisés par
# 440 antennes d’une chaine de distribution, pour divers types de produits :
# « ProduitsFrais », « Lait », « Epicerie », « Surgele », « Detergents » et « Traiteur ».
# Importer ce fichier dans R et vérifier son bon chargement avec la fonction str.

ventes <- read.csv("ventes.csv")
ventes <- subset(ventes, select = c("Region", "ProduitsFrais", "Lait", "Epicerie", "Surgele", "Detergents", "Traiteur"))
#head(ventes, 5)

# Partie 1

# Supprimer la colonne « Canal », puis ajouter successivement à la droite du tableau :
# - une colonne « Total » qui indique la somme des chiffres d’affaires des colonnes
# « ProduitsFrais », « Lait », « Epicerie », « Surgele », « Detergents » et « Traiteur » ;

nomsProduits <- c("ProduitsFrais", "Lait", "Epicerie", "Surgele", "Detergents", "Traiteur")
ventes$Total <- rowSums(ventes[, nomsProduits])
#head(ventes, 5)

# - une colonne « % ProduitsFrais » qui indique les pourcentages (arrondis à l’entier près)
# de chiffre d’affaires des produits « ProduitsFrais » par rapport au « Total », et ainsi de
# suite pour tous les produits.

for (name in nomsProduits) {
  ventes[, paste0("% ", name)] <- round((ventes[, name] / ventes$Total) * 100)
}
#head(ventes, 5)

# Exporter ce tableau avec l’instruction :
# write.csv2(x, file = "TPR.csv")
# où x désigne le nom que vous avez donné à votre tableau final dans R.

write.csv2(ventes, file = "TPR.csv")

# Enfin, afin de comparer la performance de chaque région, afficher dans la console R, pour
# chacune des régions, le chiffre d’affaire « Total » moyen :
# CA Total moyen Region 1 : 30997.57 euros
# etc.

for (i in seq_along(unique(ventes$Region))) {
  print(paste("CA Total moyen Region", i, ":", round(mean(ventes[ventes$Region == i, "Total"]), 2), "euros", sep = " "), quote = FALSE)
}

# Partie 2

# Dans cette deuxième partie, on travaillera sur le tableau original (celui qui contient les
# données du fichier ventes.csv).
# On demande :
# - de déterminer la paire de variables qui a le meilleur coefficient de corrélation linéaire,
# parmi les variables produits (« ProduitsFrais », « Lait », « Epicerie », « Surgele »,
# « Detergents » et « Traiteur ») ;

bestCor <- 0.0
bestProduits <- NULL
for (col in combn(nomsProduits, 2, simplify = FALSE)) {
  actualCor <- cor(ventes[col[1]], ventes[col[2]], use="complete.obs")
  if (bestCor < abs(actualCor)) {
    bestCor <- abs(actualCor)
    bestProduits <- col
  }
}

# - d’effectuer les deux régressions linéaires sur cette paire de variables (Y par rapport à
# X et X par rapport à Y) ;


produit1 <- ventes[, bestProduits[1]]
produit2 <- ventes[, bestProduits[2]]
reg1 <- lm(produit1 ~ produit2, data = ventes)
reg2 <- lm(produit2 ~ produit1, data = ventes)
print(reg1)
print(reg2)

# - d’effectuer une prévision, avec chacune de ces régressions, lorsque la valeur d’une des
# deux variables est égale à sa valeur moyenne (autrement dit, prévoir Y pour la valeur
# moyenne de X et prévoir X pour la valeur moyenne de Y) ;

pred1 <- predict(reg1, data.frame(produit2 = mean(produit2)))
pred2 <- predict(reg2, data.frame(produit1 = mean(produit1)))
print(paste("Il faut prévoir", round(pred1[[1]]), bestProduits[1], "pour le montant moyen de", bestProduits[2], sep = " "), quote = FALSE)
print(paste("Il faut prévoir", round(pred2[[1]]), bestProduits[2], "pour le montant moyen de", bestProduits[1], sep = " "), quote = FALSE)

# - d’afficher le nuage de points et la droite de régression correspondants à l’une des deux
# régressions effectuées.

plot(produit1, produit2, xlab = bestProduits[1], ylab = bestProduits[2])
abline(reg2)

