install.packages("ggplot2")
library("ggplot2") 

install.packages("readxl")
library("readxl")

install.packages("psych")
library(psych)

# Setup
data <- read_excel("Vorstudie-Data/results.xlsx")
describe(data)

toonBlue_Anthro <- (mean(data$`toonBlue[Anthro1].  [Unecht | Natürlich]`) +
                      mean(data$`toonBlue[Anthro2].  [Wie eine Maschine | Wie ein Mensch]`) +
                      mean(data$`toonBlue[Anthro3].  [Hat kein Bewusstsein | Hat ein Bewusstsein]`) +
                      mean(data$`toonBlue[Anthro4].  [Künstlich | Realistisch ]`) +
                      mean(data$`toonBlue[Anthro5].  [Bewegt sich steif | Bewegt sich flüssig]`)) / 5

hyper_Anthro <- (mean(data$`hyperRealistic[Anthro1].  [Unecht | Natürlich]`) +
                   mean(data$`hyperRealistic[Anthro2].  [Wie eine Maschine | Wie ein Mensch]`)+ 
                   mean(data$`hyperRealistic[Anthro3].  [Hat kein Bewusstsein | Hat ein Bewusstsein]`)+ 
                   mean(data$`hyperRealistic[Anthro4].  [Künstlich | Realistisch ]`) +
                   mean(data$`hyperRealistic[Anthro5].  [Bewegt sich steif | Bewegt sich flüssig]`)) / 5

panda_Anthro <- (mean(data$`panda[Anthro1].  [Unecht | Natürlich]`) +
                   mean(data$`panda[Anthro2].  [Wie eine Maschine | Wie ein Mensch]`)+ 
                   mean(data$`panda[Anthro3].  [Hat kein Bewusstsein | Hat ein Bewusstsein]`)+ 
                   mean(data$`panda[Anthro4].  [Künstlich | Realistisch ]`) +
                   mean(data$`panda[Anthro5].  [Bewegt sich steif | Bewegt sich flüssig]`)) / 5

# create column variable
toonBlue_Anthropomorphism <- rowMeans(data[, c("toonBlue[Anthro1].  [Unecht | Natürlich]", 
                                                  "toonBlue[Anthro2].  [Wie eine Maschine | Wie ein Mensch]",
                                                  "toonBlue[Anthro3].  [Hat kein Bewusstsein | Hat ein Bewusstsein]",
                                                  "toonBlue[Anthro4].  [Künstlich | Realistisch ]", 
                                                  "toonBlue[Anthro5].  [Bewegt sich steif | Bewegt sich flüssig]")], na.rm = TRUE)

panda_Anthropomorphism <- rowMeans(data[, c("panda[Anthro1].  [Unecht | Natürlich]", 
                                            "panda[Anthro2].  [Wie eine Maschine | Wie ein Mensch]",
                                            "panda[Anthro3].  [Hat kein Bewusstsein | Hat ein Bewusstsein]",
                                            "panda[Anthro4].  [Künstlich | Realistisch ]", 
                                            "panda[Anthro5].  [Bewegt sich steif | Bewegt sich flüssig]")], na.rm = TRUE)

hyperRealistic_Anthropomorphism <- rowMeans(data[, c("hyperRealistic[Anthro1].  [Unecht | Natürlich]", 
                                            "hyperRealistic[Anthro2].  [Wie eine Maschine | Wie ein Mensch]",
                                            "hyperRealistic[Anthro3].  [Hat kein Bewusstsein | Hat ein Bewusstsein]",
                                            "hyperRealistic[Anthro4].  [Künstlich | Realistisch ]", 
                                            "hyperRealistic[Anthro5].  [Bewegt sich steif | Bewegt sich flüssig]")], na.rm = TRUE)

t_panda_toon <- t.test(toonBlue_Anthropomorphism, panda_Anthropomorphism, paired = FALSE)
t_hyper_toon <- t.test(toonBlue_Anthropomorphism, hyperRealistic_Anthropomorphism, paired = FALSE)
print(mean(toonBlue_Anthropomorphism))
print(mean(panda_Anthropomorphism))
print(t_panda_toon)
print(t_hyper_toon)
