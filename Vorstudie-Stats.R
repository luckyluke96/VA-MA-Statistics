install.packages("ggplot2")
library("ggplot2") 

install.packages("readxl")
library("readxl")

install.packages("psych")
library(psych)

# Setup
data <- read_excel("Vorstudie-Data/results.xlsx")
describe(data)

# rename columns
colnames(data) <- gsub(".  \\[Unecht \\| Natürlich\\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Wie eine Maschine \\| Wie ein Mensch\\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Hat kein Bewusstsein \\| Hat ein Bewusstsein\\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Künstlich \\| Realistisch \\]",  "", colnames(data))
colnames(data) <- gsub(".  \\[Bewegt sich steif \\| Bewegt sich flüssig\\]",  "", colnames(data))

toonBlue_Anthro <- (mean(data$`toonBlue[Anthro1]`) +
                      mean(data$`toonBlue[Anthro2]`) +
                      mean(data$`toonBlue[Anthro3]`) +
                      mean(data$`toonBlue[Anthro4]`) +
                      mean(data$`toonBlue[Anthro5]`)) / 5

hyper_Anthro <- (mean(data$`hyperRealistic[Anthro1]`) +
                   mean(data$`hyperRealistic[Anthro2]`)+ 
                   mean(data$`hyperRealistic[Anthro3]`)+ 
                   mean(data$`hyperRealistic[Anthro4]`) +
                   mean(data$`hyperRealistic[Anthro5]`)) / 5

panda_Anthro <- (mean(data$`panda[Anthro1]`) +
                   mean(data$`panda[Anthro2]`)+ 
                   mean(data$`panda[Anthro3]`)+ 
                   mean(data$`panda[Anthro4]`) +
                   mean(data$`panda[Anthro5]`)) / 5

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
