###############################################################
## UE Démarche scientifique appliquée à l'écologie évolutive ##
##                      TP biostats                          ##
##                  Script R à compléter                     ##
###############################################################

###############
## Démarrage ##
###############

# Prise en main de R -----

3+4


################################################ START OF TP ON FEB 15 2024 ##############################################

data <- read.csv("/media/crunchy/Crunchy USB/2024-Documents-TPbiostats/data.csv", sep=";", stringsAsFactors=TRUE)

data_2023 = subset(data, Annee == "2023")
data_2023 = droplevels(data_2023)

plot(Nb_ligu ~ Nb_tubu, data = data_2023, main="MONKEY PLOT", xlab="NOMBRE DE TUBU", ylab="NOMBRE DE LIGU", col="#f4bcd1")

data_2023$Nb_ligu

par(mfrow=c(1,2))

hist(data_2023$Nb_ligu)
hist(data_2023$Nb_tubu)

#W IS THE STATISTIQUE DU TEST, WE USE IT TO SEE IF ITS SIGNICATIF OR NOT
#P-VALUE IS THE PROBABILITE CRITIQUE (RISQUE DE PREMIER ESPECE)

#UNDER 5 PERCENT, SO NOT NORMAL (LIGU). DETECTED SIGNIFICATIVE DIFFERENCE FROM THE NORMALITE. ON REJETE H0
shapiro.test(data_2023$Nb_ligu)

#OVER 5 PERCENT, SO NORMAL (TUBU). WE SAY IT CANNOT BE SIGNATIVEMENT DIFFERENT A LA NORMALITE
shapiro.test(data_2023$Nb_tubu)

#LETS TRANSFORM THIS TO LOG FUNCTION AND HOPE TO GOD IT BECOMES NORMAL
tolog = log(data_2023$Nb_ligu)
hist(tolog)
shapiro.test(tolog)
#result: ITS STILL UNDER 5 PERCENT EVEN AFTER TURNING TO LOG, SO ITS REALLY NOT NORMAL

#NOW WE ARE TESTING CORRELATION IN BETWEEN BOTH
cor.test(tolog, data_2023$Nb_tubu, method="pearson")

#t is the statistique of the test
#df is degrees of freedom, degress de liberte
#p value is the probabilite critique
#HO is no correlation, H1 is correlation. So under 5%, there is a correlation. The test is bi directional because thats the default.
#in this case, we reject H0

cor.test(tolog, data_2023$Nb_tubu, method="pearson", alternative="greater")
#we get a correlation of 0,57 and the correlation is indeed positive. We reject H0 since our alpha is under 5 percent
#we can say there is a positive correlation and that we can say the alternative hypothesis is true

#we can also do this for spearmann!

cor.test(tolog, data_2023$Nb_tubu, method="spearman", alternative="greater")
#p value is super small
#H0 is that rho is > 0. We can neither reject H0 or reject H1
#our rho is 0,559




################################################ END OF TP ON FEB 15 2024 ##############################################

################################################ DOING EXERCIES ON MY OWN ON FEB 16 2024 ###############################

# Exercice 2 : Le nombre de fleurs tubulées des pâquerettes de milieu semi-urbain est-il le
#même durant l'été 2021 et au printemps 2023 ?

data_su = subset(data, Milieu == "Semi-urbain")
#Now we are going to make a boxplot from the subset we created
boxplot(Nb_tubu ~ Annee, data = data_su, xlab = "YEAR", ylab =" NUMBER OF TUBUS", col="#f4bcd1")






################################################# END OF DOING MY OWN EXERCIES ON FEB 16 2024 ##########################



# tapez ici une autre opération de votre choix et affichez son résultat


# Les objets dans R -----

objet1 = 3 + 4 
# tapez ici la commande permettant d'afficher le contenu de objet 1
# tapez ici une commande permettant de calculer objet 2 (cf. poly)


# Importation du tableau de données -----

# utilisez le menu "Import dataset" pour importer le tableau de données (cf. poly)
# recopiez ici les 2 lignes de commande apparues dans la console lors de l'importation (sans le symbole >)


# Description des données -----
summary(data_paq)


################
## EXERCICE 1 ##
################

# Le nombre de fleurs ligulées et le nombre de fleurs tubulées des pâquerettes prélevées en 2023 sont-ils liés ? -----

data_2023 = subset(data_paq, Annee=="2023")

plot(Nb_ligu ~ Nb_tubu, data=data_2023)
# remplacez les ??? dans la commande ci-dessous pour ajouter un titre explicite au graphique tracé
plot(Nb_ligu ~ Nb_tubu, data=data_2023, main = "???")
# tapez ici la commande permettant de reproduire ce graphique avec un titre explicite aux axes des abcisses et des ordonnées, un titre général et les points en bleu (cf. poly)

data_2023$Nb_ligu

par(mfrow=c(1,2))
# remplacez les ??? dans la commande ci-dessous permettant de visualiser la distribution du nombre de fleurs ligulées
hist(???)
# tapez ici la commande permettant de tracer le même graphique pour le nombre de fleurs tubulées

# remplacez les ??? dans la commande ci-dessous permettant de réaliser le test de Shapiro pour le nombre de fleurs ligulées
shapiro.test(???)
# tapez ici la commande permettant de réaliser le même test pour le nombre de fleurs tubulées

log(data_2023$Nb_ligu)

par(mfrow=c(1,1))
# tapez ici la commande permettant de visualiser la distribution du nombre de fleurs ligulées transformé en logarithme
# tapez ici la commande permettant de réaliser le test de Shapiro pour le nombre de fleurs ligulées transformé en logarithme

# en vous aidant de l'aide, remplacez les ??? de la commande ci-dessous afin de réaliser un test de corrélation paramétrique :
?cor.test
cor.test(log(data_2023$Nb_ligu), data_2023$Nb_tubu, method="???")
# recopiez ici la commande précédente et ajoutez-y l'argument alternative = "greater"
# tapez ici la commande permettant de réaliser le test de corrélation non-paramétrique


################
## EXERCICE 2 ##
################

# Le nombre de fleurs tubulées des pâquerettes prélevées durant l'été 2021 et au printemps 2023 dans le milieu semi-urbain est-il le même ? ------

# tapez ici la commande permettant de construire un sous-tableau nommé data_su ne contenant que les pâquerettes prélevées en milieu semi-urbain

# complétez les ??? de la commande ci-dessous afin de représenter le nombre de fleurs tubulées en fonction de l’année pour les pâquerettes du milieu semi-urbain
par(mfrow=c(1,1))
boxplot(Nb_tubu ~ Annee, data = ???)
# modifiez la commande ci-dessus afin d'ajouter un titre général et à chacun des axes, et de représenter les boîtes en couleur

summary(data_su$Nb_tubu)

# complétez les ??? des  commandes ci-dessous pour créer deux vecteurs contenant le nombre de fleurs tubulées pour une seule année :
Nb_tubu_2021 = subset(???$Nb_tubu, ???$Annee == "???")
Nb_tubu_2023 = subset(???$Nb_tubu, ???$Annee == "???")

mean(Nb_tubu_2021)
mean(Nb_tubu_2023)

# tapez ici les commandes permettant de visualiser la distribution du nombre de fleurs tubulées en 2021 et en 2023
# tapez ici les commandes permettant de réaliser le test de normalité pour le nombre de fleurs tubulées en 2021 et en 2023

var.test(Nb_tubu ~ Annee, data=data_su) 

# complétez les ??? de la commande ci-dessous en vous aidant de l'aide de la fonction t.test :
t.test(Nb_tubu ~ Annee, data=data_su, var.equal = ???, alternative = "two.sided") 

# tapez ici la commande permettant de réaliser le test de Student

wilcox.test(Nb_tubu ~ Annee, data=data_su, alternative = "two.sided") 


################
## EXERCICE 3 ##
################

# Le nombre de fleurs ligulées des pâquerettes prélevées en 2023 dépend-il du milieu ? -----

# ré-exécutez la commande permettant de créer un sous-tableau de données ne contenant que les pâquerettes prélevées en 2023 (cf. exo 1)

# tapez ici la commande permettant de représenter le nombre de fleurs ligulées en fonction des trois milieux (cf. exo 2)

mod_ligu = lm(Nb_ligu ~ Milieu, data = data_2023)
mod_ligu

mod_ligu$residuals

par(mfrow=c(1,2))
# tapez ici la commande permettant de tracer l'histogramme des résidus du modèle

qqnorm(mod_ligu$residuals)
qqline(mod_ligu$residuals)

# tapez ici la commande permettant de tester la normalité des résidus du modèle

plot(mod_ligu$residuals ~ mod_ligu$fitted.values)
abline(h=0)
library(lmtest)
bptest(mod_ligu)

# complétez les ??? de la commande ci-dessous pour ajuster un autre modèle dont la variable dépendante est le logarithme du nombre de fleurs ligulées
mod_ligu_log = ???(??? ~ Milieu, data = data_2023)

# tapez ici les commandes permettant de s'assurer que les conditions de validité du modèle linéaire sont bien remplies pour le nouveau modèle (cf. ci-dessus)

anova(mod_ligu_log)

par(mfrow=c(1,1))
boxplot(log(Nb_ligu) ~ Milieu, data = data_2023, ylab = "Nombre de fleurs ligulées (log)")

kruskal.test(Nb_ligu ~ Milieu, data = data_2023) 
