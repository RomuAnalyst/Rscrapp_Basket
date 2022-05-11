
Roster95_2022 <- read.csv2("Roster.csv", sep = ",") #Equipes par saison. De 95 à 2022.
attach(Roster95_2022)
tibble::data_frame(Roster95_2022)
saison21_22 <- filter(Roster95_2022, season == "21/22")
tibble::data_frame(saison21_22)

roster95_2022 <- read.csv2("Roster.csv", sep = ",")
tibble::tibble(roster95_2022)

age <- time_length(interval(bday, date_etude), "years")
roster_age <- roster95_2022 |> mutate(age = floor(age)) 



--------------------------------------------------------------------------

Linreg <- lm(roster_age$poids ~ roster_age$taille)
sunflowerplot(x = roster_age$taille, y = roster_age$poids, xlab = "Taille (cm)", ylab = "Poids (kg)",las = 1,
              main = "Modélisation du croisement des données - Poids/Taille")

abline(Linreg, lwd=3, col="purple") # Droite des moindres carrées (en violet)
points(mean(roster_age$taille),mean(roster_age$poids),col="green",pch=15) # Centre du nuage (point vert)

#Plus les joueurs sont grands, plus le poids augmente ce qui est "normal", mais nous avons aussi de plus en plus de valeurs supérieures à la droite des moindres carrées.



library(lubridate)  
roster95_2022 <- read.csv2("Roster.csv", sep = ",") #Equipes par saison. De 95 à 2022.
tibble::tibble(roster95_2022)
date_etude <- ymd("2022-05-01") # date pour calcul de l'âge
age <- time_length(interval(bday, date_etude), "years") # calcul de l'âge
roster_age <- roster95_2022 |> mutate(age = floor(age)) |> mutate(taille = floor(inches*2.54 + feet*30.48)) |> mutate(poids = round(weight/2.2046)) # Calcul âge (entier inférieur), conversion taille en cm et poids en kg



# FAIRE UN NUAGE DE POINTS ENTRE LA TAILLE ET LE POIDS


library(tidyverse)
R_age_saision <- roster_age |> filter(season == "21/22") # sélection de la saison 21/22
test <- R_age_saision |> filter(team == "Boston Celtics" | team == "Chicago Bulls" | team == "Philadelphia 76ers" | team == "Los Angeles Lakers" | team == "Detroit Pistons" | team == "San Antonio Spurs" | team =="Minnesota Timberwolves" | team == "Orlando Magic" | team == "New Orleans Pelicans")
ggplot(data = test, aes(x = team, y = age, color = team)) +  
  geom_point(alpha = .25) + geom_boxplot()

library(ggplot2)

ggplot(test) +
  aes(x = team, y = age, fill = team) +
  geom_boxplot() +
  geom_jitter() +
  scale_fill_manual(values = c(`Boston Celtics` = "#87A991", 
                               `Chicago Bulls` = "#180F3E", `Detroit Pistons` = "#451077", `Los Angeles Lakers` = "#721F81", `Miami Heat` = "#9F2F7F", 
                               `Minnesota Timberwolves` = "#CD4071", `New Orleans Pelicans` = "#F1605D", `Orlando Magic` = "#FD9567", 
                               `Philadelphia 76ers` = "#FEC98D", `San Antonio Spurs` = "#FCFDBF")) +
  theme_minimal() +
  theme(legend.position = "none")


library(ggplot2)

ggplot(test) +
  aes(x = team, y = age, fill = team, color = team) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  scale_fill_brewer() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_point(alpha = .25)


ggplot(test) +
  aes(x = team, y = age, fill = team, color = team) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  scale_fill_brewer() +
  theme_minimal() +
  theme(legend.position = "none")



