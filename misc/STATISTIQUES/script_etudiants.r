###############################################################
## UE Démarche scientifique appliquée à l'écologie évolutive ##
##                      TP biostats                          ##
##                  Script R à compléter                     ##
###############################################################
########### START OF TP ON FEB 15 2024 ##############################################


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

################################################ TP FEB 29 2024 ###############################

# Exercice 2 : Le nombre de fleurs tubulées des pâquerettes de milieu semi-urbain est-il le
#même durant l'été 2021 et au printemps 2023 ?

data_su = subset(data, Milieu == "Semi-urbain")
#Now we are going to make a boxplot from the subset we created
boxplot(Nb_tubu ~ Annee, data = data_su, xlab = "YEAR", ylab =" NUMBER OF TUBUS", col="#f4bcd1")

Nb_tubu_2021 = subset(data_su$Nb_tubu, data_su$Annee == "2021")

Nb_tubu_2023 = subset(data_su$Nb_tubu, data_su$Annee == "2023")

mean(Nb_tubu_2021)
mean(Nb_tubu_2023)

#test de student, parametrique
#wilcoxon, variance non parametrique (for when one of the data sets are not normale)
#1. test de normality
#deux options, normale ou pas. Si pas normale, on peut essayer de transformer avec logarithm. Si ca marche pas, il faut vraiment faire wilcoxon ou autre test parametrique
#2 test de homoscedacite avec fisher
#si les donnees ont pas une homoscedacite (meme variance), il faut utiliser student avec correction de welch

#2021

#1 representation graphique

hist(Nb_tubu_2021)
#looks to be normal... lets do the test
shapiro.test(Nb_tubu_2021)
#p=0.8587, w=0.99357. NORMALE

#2023

#1 representation graphique

hist(Nb_tubu_2023)
#looks to be NOT normal... lets do the test
shapiro.test(Nb_tubu_2023)
#p=0.98676, w=0.7905. NORMALE

#time for fisher to check if variances are the same
# ~ means as a function of. so this is number of tubu flowers as a function of the year
var.test(Nb_tubu ~ Annee, data=data_su)
#f=0.21159, degrees of freedom = 119 and p is exponentially small. 
#h1 is variances are not equal, h0 is that they are equal (variances are the same).
# in this case, the variances are considered the same and the datasets are normal.
# we use the poly to find that student is the correct test

t.test(Nb_tubu_2021, Nb_tubu_2023, alternative="two.sided", var.equal = TRUE)
#t = -13.929, degrees of freedom is 180 and p < 2.2e-16
# we can reject h0 and accept h1. we have detected a significant difference between the average
#number of tubu flowers in between 2021 and 2023

#Now lets try a wilcoxon test for fun

wilcox.test(Nb_tubu ~ Annee, data = data_su, alternative = "two.sided")
#W=377 and p < 2.2e-16
#H1 is the two populations come from different laws (Test non parametrique)
#H0 is the two populations come from the same laws (Test non parametrique)

#we can now reject h0 and accept h1, the two populations come from different laws. There is a 
#significant difference between the number of tubu flowers in 2021 and 2023


################################################# END OF TP FEB 29 2024 ##########################

data_2023 = subset(data, Annee == "2023")


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
