# install.packages("ggplot2")
# library("ggplot2") 

install.packages("readxl")
library("readxl")

install.packages("psych")
library(psych)

install.packages("dplyr")
library(dplyr)

# Setup
data <- read_excel("Vorstudie-Data/results.xlsx")
describe(data)

# ---------------- data cleaning ----------------
clean_data <- function(data) {
# rename columns
# anthropomorphism
colnames(data) <- gsub(".  \\[Unecht \\| Natürlich\\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Wie eine Maschine \\| Wie ein Mensch\\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Hat kein Bewusstsein \\| Hat ein Bewusstsein\\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Künstlich \\| Realistisch \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Bewegt sich steif \\| Bewegt sich flüssig\\]",  "", colnames(data))

# animacy
colnames(data) <- gsub(".  \\[Tot \\| Lebendig \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Unbewegt \\| Lebendig \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Mechanisch \\| Organisch \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Träge \\| Interaktiv \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Apathisch \\| Reagierend \\]",  "", colnames(data))

# likeability
colnames(data) <- gsub(".  \\[Nicht mögen \\| Mögen \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Unfreundlich \\| Freundlich \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Unhöflich \\| Höflich \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Unangenehm \\| Angenehm \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Furchtbar \\| Nett \\]",  "", colnames(data))

# perceived intelligence
colnames(data) <- gsub(".  \\[Inkompetent \\| Kompetent \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Ungebildet \\| Unterrichtet \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Verantwortungslos \\| Verantwortungsbewusst \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Unintelligent \\| Intelligent \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Unvernünftig \\| Vernünftig \\]",  "", colnames(data))

# perceived safety
colnames(data) <- gsub(".  \\[Ängstlich \\| Entspannt \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Ruhig \\| Aufgewühlt \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Still \\| Überrascht\\]",  "", colnames(data))

# copy and relocate anthro4
data$`toonBlue[Anima4]` <- data$`toonBlue[Anthro4]`
data <- data %>%
  relocate(`toonBlue[Anima4]`, .after = `toonBlue[Anima3]`)

data$`hyperRealistic[Anima4]` <- data$`hyperRealistic[Anthro4]`
data <- data %>%
  relocate(`hyperRealistic[Anima4]`, .after = `hyperRealistic[Anima3]`)

data$`panda[Anima4]` <- data$`panda[Anthro4]`
data <- data %>%
  relocate(`panda[Anima4]`, .after = `panda[Anima3]`)

data$`realisticWeird[Anima4]` <- data$`realisticWeird[Anthro4]`
data <- data %>%
  relocate(`realisticWeird[Anima4]`, .after = `realisticWeird[Anima3]`)

data$`toonBrown[Anima4]` <- data$`toonBrown[Anthro4]`
data <- data %>%
  relocate(`toonBrown[Anima4]`, .after = `toonBrown[Anima3]`)

return(data)
}

data <- clean_data(data)

calc_anthro_means <- function(data) {

anthro_toonBlue <<- (mean(data$`toonBlue[Anthro1]`) +
                      mean(data$`toonBlue[Anthro2]`) +
                      mean(data$`toonBlue[Anthro3]`) +
                      mean(data$`toonBlue[Anthro4]`) +
                      mean(data$`toonBlue[Anthro5]`)) / 5

anthro_hyperRealistic <<- (mean(data$`hyperRealistic[Anthro1]`) +
                   mean(data$`hyperRealistic[Anthro2]`)+ 
                   mean(data$`hyperRealistic[Anthro3]`)+ 
                   mean(data$`hyperRealistic[Anthro4]`) +
                   mean(data$`hyperRealistic[Anthro5]`)) / 5

anthro_panda <<- (mean(data$`panda[Anthro1]`) +
                   mean(data$`panda[Anthro2]`)+ 
                   mean(data$`panda[Anthro3]`)+ 
                   mean(data$`panda[Anthro4]`) +
                   mean(data$`panda[Anthro5]`)) / 5

anthro_realisticWeird <<- (mean(data$`realisticWeird[Anthro1]`) +
                   mean(data$`realisticWeird[Anthro2]`)+ 
                   mean(data$`realisticWeird[Anthro3]`)+ 
                   mean(data$`realisticWeird[Anthro4]`) +
                   mean(data$`realisticWeird[Anthro5]`)) / 5

anthro_toonBrown <<- (mean(data$`toonBrown[Anthro1]`) +
                            mean(data$`toonBrown[Anthro2]`)+ 
                            mean(data$`toonBrown[Anthro3]`)+ 
                            mean(data$`toonBrown[Anthro4]`) +
                            mean(data$`toonBrown[Anthro5]`)) / 5
return(data)
}

calc_anima_means <- function(data) {
  anima_toonBlue <<- (mean(data$`toonBlue[Anima1]`) +
                         mean(data$`toonBlue[Anima2]`) +
                         mean(data$`toonBlue[Anima3]`) +
                         mean(data$`toonBlue[Anima4]`) +
                         mean(data$`toonBlue[Anima5]`) +
                        mean(data$`toonBlue[Anima6]`)) / 6
  
  anima_hyperRealistic <<- (mean(data$`hyperRealistic[Anima1]`) +
                               mean(data$`hyperRealistic[Anima2]`)+ 
                               mean(data$`hyperRealistic[Anima3]`)+ 
                               mean(data$`hyperRealistic[Anima4]`) +
                               mean(data$`hyperRealistic[Anima5]`) +
                              mean(data$`hyperRealistic[Anima6]`)) / 6
  
  anima_panda <<- (mean(data$`panda[Anima1]`) +
                      mean(data$`panda[Anima2]`)+ 
                      mean(data$`panda[Anima3]`)+ 
                      mean(data$`panda[Anima4]`) +
                      mean(data$`panda[Anima5]`) +
                     mean(data$`panda[Anima6]`)) / 6
  
  anima_realisticWeird <<- (mean(data$`realisticWeird[Anima1]`) +
                               mean(data$`realisticWeird[Anima2]`)+ 
                               mean(data$`realisticWeird[Anima3]`)+ 
                               mean(data$`realisticWeird[Anima4]`) +
                               mean(data$`realisticWeird[Anima5]`) +
                              mean(data$`realisticWeird[Anima6]`)) / 6
  
  anima_toonBrown <<- (mean(data$`toonBrown[Anima1]`) +
                          mean(data$`toonBrown[Anima2]`)+ 
                          mean(data$`toonBrown[Anima3]`)+ 
                          mean(data$`toonBrown[Anima4]`) +
                          mean(data$`toonBrown[Anima5]`) +
                         mean(data$`toonBrown[Anima6]`)) / 6
  
  return(data)
}

data <- calc_anthro_means(data)
data <- calc_anima_means(data)

# create column variable
toonBlue_Anthropomorphism <- rowMeans(data[, c("toonBlue[Anthro1]", 
                                                  "toonBlue[Anthro2]",
                                                  "toonBlue[Anthro3]",
                                                  "toonBlue[Anthro4]", 
                                                  "toonBlue[Anthro5]")], na.rm = TRUE) / 5

panda_Anthropomorphism <- rowMeans(data[, c("panda[Anthro1]", 
                                            "panda[Anthro2]",
                                            "panda[Anthro3]]",
                                            "panda[Anthro4]", 
                                            "panda[Anthro5]")], na.rm = TRUE) / 5

hyperRealistic_Anthropomorphism <- rowMeans(data[, c("hyperRealistic[Anthro1]", 
                                            "hyperRealistic[Anthro2]",
                                            "hyperRealistic[Anthro3]]",
                                            "hyperRealistic[Anthro4]", 
                                            "hyperRealistic[Anthro5]")], na.rm = TRUE) / 5

t_panda_toon <- t.test(toonBlue_Anthropomorphism, panda_Anthropomorphism, paired = FALSE)
t_hyper_toon <- t.test(toonBlue_Anthropomorphism, hyperRealistic_Anthropomorphism, paired = FALSE)
print(mean(toonBlue_Anthropomorphism))
print(mean(panda_Anthropomorphism))
print(mean(hyperRealistic_Anthropomorphism))
print(t_panda_toon)
print(t_hyper_toon)
