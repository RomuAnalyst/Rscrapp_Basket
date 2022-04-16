
Roster95_2022 <- read.csv2("Roster 95-22.csv", sep = ",")
tibble::data_frame(Roster95_2022)
saison21_22 <- filter(Roster95_2022, season == "21/22")
tibble::data_frame(saison21_22)

