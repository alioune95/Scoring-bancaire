
#------------------------------------------------------------------------
#--------------Importation des librairies et des donn�es-----------------
#------------------------------------------------------------------------

library("dplyr")
library("rpart")
library("ggcorrplot")
library("DALEX")
library("rpart.plot")
library("readxl")


bank <- read.csv("C:/SAS/Master2/Analyse de donn�es et Scoring/bank/data-society-bank-marketing-data/bankfull.csv", sep = ";")
train <- read_xlsx("C:/SAS/Master2/Analyse de donn�es et Scoring/bank/data-society-bank-marketing-data/train.xlsx")
test <- read_xlsx("C:/SAS/Master2/Analyse de donn�es et Scoring/bank/data-society-bank-marketing-data/test.xlsx")



#------------------------------------------------------------------------
#-----------------Statistiques descriptives------------------------------ 
#------------------------------------------------------------------------


#Vue d'ensemble des donn�es
str(bank)
summary(bank)
train$day <- as.factor(train$day)
test$day <- as.factor(test$day)



#------------------------------------------------------------------------
#-----------------Repr�sentation graphiques------------------------------ 
#------------------------------------------------------------------------



ggplot(data=bank) +
  geom_bar(aes(x = poutcome, fill = y)) +
  theme_bw()+
  xlab("R�sultat campagne marketing pr�c�dente") + 
  ylab("Fr�quence") + 
  ggtitle("R�partition des clients en fonction du r�sultat
          de la campagne pr�c�dente")

ggplot(data=bank, aes(x = y, y = duration)) +
  geom_boxplot() + 
  #geom_jitter(col="steelblue",alpha=0.1)+
  ggtitle("Distribution de y en fonction de la dur�e")

ggplot(data=bank, aes(x = y, y = age)) +
  geom_boxplot() + 
  #geom_jitter(col="steelblue",alpha=0.1)+
  ggtitle("Distribution de y en fonction de l'�ge")

ggplot(data=bank) +
  geom_bar(aes(x = education, fill = y)) +
  theme_bw()+
  xlab("Niveau d'�tudes") + 
  ylab("Fr�quence") + 
  ggtitle("R�partition des clients en fonction du niveau d'�ducation")

ggplot(data=bank) +
  geom_bar(aes(x = housing, fill = y)) +
  theme_bw()+
  xlab("D�faut de cr�dit") + 
  ylab("Fr�quence") + 
  ggtitle("R�partition des clients en fonction du statut de cr�dit")

ggplot(data=bank) +
  geom_bar(aes(x = marital, fill = y)) +
  theme_bw()+
  xlab("sistuation maritale") + 
  ylab("Fr�quence") + 
  ggtitle("R�partition des clients en fonction de la situation maritale")



ggplot(data=bank) +
  geom_histogram(aes(x = campaign, fill = marital)) +
  theme_bw()+
  xlab("Nombre d'appels") + 
  ylab("Fr�quence") + 
  ggtitle("Nombre d'appels en fonction du statut marital")
  


#------------------------------------------------------------------------
#-------------------------Arbre de d�cision------------------------------ 
#------------------------------------------------------------------------

  
#arbre de d�cision
arbre_train <-  rpart(y ~. , data = train)

#afficher l'arbre sous forme graphique
rpart.plot(arbre_train , box.palette ="RdBu" , type =5 )

summary(arbre_train)
summary(arbre_train)$splits

#Nombre d'arbres id�al
 plotcp (arbre_train)

 
#Importance des variables dans la construction de l'arbre
importance <- data.frame(importance =  arbre$variable.importance)

#Test de l'arbre
arbre_test1 <- predict(arbre_train, test)

#Classe pr�dites
arbre_class <- data.frame(y_predict = predict(arbre_train, test, type = "class"))

y_test <- subset(test, select=c(y))


#Matrice de confusion
matrice <- data.frame(y_test, arbre_class)
table(matrice)
confusion <- data.frame(table(matrice))

