
#------------------------------------------------------------------------
#--------------Importation des librairies et des données-----------------
#------------------------------------------------------------------------

library("dplyr")
library("rpart")
library("ggcorrplot")
library("DALEX")
library("rpart.plot")
library("readxl")


bank <- read.csv("C:/SAS/Master2/Analyse de données et Scoring/bank/data-society-bank-marketing-data/bankfull.csv", sep = ";")
train <- read_xlsx("C:/SAS/Master2/Analyse de données et Scoring/bank/data-society-bank-marketing-data/train.xlsx")
test <- read_xlsx("C:/SAS/Master2/Analyse de données et Scoring/bank/data-society-bank-marketing-data/test.xlsx")



#------------------------------------------------------------------------
#-----------------Statistiques descriptives------------------------------ 
#------------------------------------------------------------------------


#Vue d'ensemble des données
str(bank)
summary(bank)
train$day <- as.factor(train$day)
test$day <- as.factor(test$day)



#------------------------------------------------------------------------
#-----------------Représentation graphiques------------------------------ 
#------------------------------------------------------------------------



ggplot(data=bank) +
  geom_bar(aes(x = poutcome, fill = y)) +
  theme_bw()+
  xlab("Résultat campagne marketing précédente") + 
  ylab("Fréquence") + 
  ggtitle("Répartition des clients en fonction du résultat
          de la campagne précédente")

ggplot(data=bank, aes(x = y, y = duration)) +
  geom_boxplot() + 
  #geom_jitter(col="steelblue",alpha=0.1)+
  ggtitle("Distribution de y en fonction de la durée")

ggplot(data=bank, aes(x = y, y = age)) +
  geom_boxplot() + 
  #geom_jitter(col="steelblue",alpha=0.1)+
  ggtitle("Distribution de y en fonction de l'âge")

ggplot(data=bank) +
  geom_bar(aes(x = education, fill = y)) +
  theme_bw()+
  xlab("Niveau d'études") + 
  ylab("Fréquence") + 
  ggtitle("Répartition des clients en fonction du niveau d'éducation")

ggplot(data=bank) +
  geom_bar(aes(x = housing, fill = y)) +
  theme_bw()+
  xlab("Défaut de crédit") + 
  ylab("Fréquence") + 
  ggtitle("Répartition des clients en fonction du statut de crédit")

ggplot(data=bank) +
  geom_bar(aes(x = marital, fill = y)) +
  theme_bw()+
  xlab("sistuation maritale") + 
  ylab("Fréquence") + 
  ggtitle("Répartition des clients en fonction de la situation maritale")



ggplot(data=bank) +
  geom_histogram(aes(x = campaign, fill = marital)) +
  theme_bw()+
  xlab("Nombre d'appels") + 
  ylab("Fréquence") + 
  ggtitle("Nombre d'appels en fonction du statut marital")
  


#------------------------------------------------------------------------
#-------------------------Arbre de décision------------------------------ 
#------------------------------------------------------------------------

  
#arbre de décision
arbre_train <-  rpart(y ~. , data = train)

#afficher l'arbre sous forme graphique
rpart.plot(arbre_train , box.palette ="RdBu" , type =5 )

summary(arbre_train)
summary(arbre_train)$splits

#Nombre d'arbres idéal
 plotcp (arbre_train)

 
#Importance des variables dans la construction de l'arbre
importance <- data.frame(importance =  arbre$variable.importance)

#Test de l'arbre
arbre_test1 <- predict(arbre_train, test)

#Classe prédites
arbre_class <- data.frame(y_predict = predict(arbre_train, test, type = "class"))

y_test <- subset(test, select=c(y))


#Matrice de confusion
matrice <- data.frame(y_test, arbre_class)
table(matrice)
confusion <- data.frame(table(matrice))

