#### Devoir preparee par Marie Bermann GERMAIN
#### J'ai utilise les donnees relatives a la Banque Central D'HAITI(BRH)
#### le mardi 14 juin 2022


# importation d'un fichier xls en ligne (without downloading it)
library(tidyverse)
library(readxl)
library(tseries)
library(lmtest)


url1<-'https://www.brh.ht/wp-content/uploads/agregatsmon.xls'
p1f <- tempfile()
download.file(url1, p1f, mode="wb")
agregatsmon <-read_excel(path = p1f, skip = 2)

# head(agregatsmon) # visualisons les 6 premieres lignes
# tail(agregatsmon, n = 10) # # visualisons les 10 premieres lignes



# data cleaning
agregatsmon <- agregatsmon[1:517,] # enlevons les 10 dernieres lignes

agregatsmon <- rename(agregatsmon, Date = ...1) 

agregatsmon <- agregatsmon[-1,] # enlevons la premiere ligne


agregatsmon$Date <- as.numeric(agregatsmon$Date)
# conversion date format charactere -> numerique

#sum(is.na(agregatsmon$Date)) 
# 17 dates non formatees: 309-320, 360-364

agregatsmon$Date <- as.Date(agregatsmon$Date, 
                            origin = "1899-12-30")
# conversion dates format nombre en correct format 
# en prenant 30 Decembre 1899 comme point de depart 

#agregatsmon$Date # verification pour exact match avec fichier Excel

# Formattage des 17 dates manquantes
# Hint: taper les valeurs en observant le fichier excel 

agregatsmon$Date[309] <- "2004-07-01"
agregatsmon$Date[310] <- "2004-08-01"
agregatsmon$Date[311] <- "2004-09-01"
agregatsmon$Date[312] <- "2004-10-01"
agregatsmon$Date[313] <- "2004-11-01"
agregatsmon$Date[314] <- "2004-12-01"
agregatsmon$Date[315] <- "2005-01-01"
agregatsmon$Date[316] <- "2005-02-01"
agregatsmon$Date[317] <- "2005-03-01"
agregatsmon$Date[318] <- "2005-04-01"
agregatsmon$Date[319] <- "2005-05-01"
agregatsmon$Date[320] <- "2005-06-01"

agregatsmon$Date[360] <- "2008-10-01"
agregatsmon$Date[361] <- "2008-11-01"
agregatsmon$Date[362] <- "2008-12-01"
agregatsmon$Date[363] <- "2009-01-01"
agregatsmon$Date[364] <- "2009-02-01"


###############
# 2. Filtrer a partir d'Octobre 1990
#################

Donnees <- agregatsmon %>% 
  filter(Date >= "1990-10-01")

#head(Donnees)

###############
# 3. Retirer tous les colonnes inutiles
###########

Donnees<- Donnees[,-c(5,9,11,15,20,23,25,30,35,40,42,47,56,79,84)]

#head(Donnees)

agregats<- Donnees[, c(9,10,37)]


Donnees <- Donnees %>% 
  rename(BM1 = 'BASE MONETAIRE...12',
         BM2 = `BASE MONETAIRE...13`,
         reserves_nette = "Réserves nettes de change du système banc.(millions de $)")

Donnees <- Donnees %>% 
  mutate(BM1 = as.numeric(BM1),
         BM2 = as.numeric(BM2),
         reserves_nette = as.numeric(reserves_nette))

#head(Donnees)


Donnees$Date <- Donnees$Date

### Realisation  d'un graphique en nuage de point representant la base monetaire 1 
library(ggplot2)

ggplot(Donnees, aes(x = Date, y = BM1))+
  geom_point(size=3,color='red')+
  labs(title = " Base Monetaire",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")


# Realisation d'un graphique en baton representant la base monetaire 2

ggplot(Donnees, aes(x = Date, y = BM2))+
  geom_bar(stat = 'identity',fill='orangered')+
  labs(title = " Base monetaire 2",
       y = "BM2",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")


# Realisation d'un graphique en ligne representant  des reserves nettes de changes de la BRH 
# allant d'Octobre 1990 a Octobre 2021

ggplot(Donnees, aes(x = Date, y = reserves_nette))+
  geom_line(col='pink')+
  labs(title = " réserves nettes de changes ",
       y = "Réserves nettes de change du système banc. en millions de $",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")


