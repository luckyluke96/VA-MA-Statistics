install.packages("ggplot2")
install.packages("readxl")
install.packages("psych")
install.packages("dplyr")
install.packages("car")
install.packages("dunn.test")
install.packages("reshape2")

library("ggplot2") 
library("readxl")
library(psych)
library(dplyr)
library(tidyr)
library(car)
library(dunn.test)
library(reshape2)


# Setup
# data <- read_excel("Vorstudie-Data/results.xlsx")
data <- read_excel("Vorstudie-Data/results-with-robot-wo-safety.xlsx")
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

data$`Robot[Anima4]` <- data$`Robot[Anthro4]`
data <- data %>%
  relocate(`Robot[Anima4]`, .after = `Robot[Anima3]`)

# delete rows with NaNs
data <- data %>%
  filter(!is.na(`Robot[Anthro1]`) & `Robot[Anthro1]` != "")

return(data)
}

data <- clean_data(data)
describe(data)
print(paste("Durchschnittsalter:", mean(data$`G01Q07. Wie alt sind Sie?`)))

gender_counts <- table(data$`G06Q08. Welchem Geschlecht fühlen Sie sich zugehörig?`)
print(gender_counts)

# function adds mean of 5 godspeed variables
add_mean_columns <- function(data) {
  data <- data %>%
    mutate(`toonBrown[AnthroMean]` = rowMeans(select(., `toonBrown[Anthro1]`, `toonBrown[Anthro2]`, `toonBrown[Anthro3]`, `toonBrown[Anthro4]`, `toonBrown[Anthro5]`), na.rm = TRUE)) %>%
    mutate(`toonBrown[AnimaMean]` = rowMeans(select(., `toonBrown[Anima1]`, `toonBrown[Anima2]`, `toonBrown[Anima3]`, `toonBrown[Anima4]`, `toonBrown[Anima5]`, `toonBrown[Anima6]`), na.rm = TRUE)) %>%
    mutate(`toonBrown[LikeMean]` = rowMeans(select(., `toonBrown[Like1]`, `toonBrown[Like2]`, `toonBrown[Like3]`, `toonBrown[Like4]`, `toonBrown[Like5]`), na.rm = TRUE)) %>%
    mutate(`toonBrown[IntMean]` = rowMeans(select(., `toonBrown[Int1]`, `toonBrown[Int2]`, `toonBrown[Int3]`, `toonBrown[Int4]`, `toonBrown[Int5]`), na.rm = TRUE)) %>%
    relocate(`toonBrown[AnthroMean]`, .after = `toonBrown[Anthro5]`) %>%  
    relocate(`toonBrown[AnimaMean]`, .after = `toonBrown[Anima6]`) %>%
    relocate(`toonBrown[LikeMean]`, .after = `toonBrown[Like5]`) %>%
    relocate(`toonBrown[IntMean]`, .after = `toonBrown[Int5]`) 
  
  data <- data %>%
    mutate(`toonBlue[AnthroMean]` = rowMeans(select(., `toonBlue[Anthro1]`, `toonBlue[Anthro2]`, `toonBlue[Anthro3]`, `toonBlue[Anthro4]`, `toonBlue[Anthro5]`), na.rm = TRUE)) %>%
    mutate(`toonBlue[AnimaMean]` = rowMeans(select(., `toonBlue[Anima1]`, `toonBlue[Anima2]`, `toonBlue[Anima3]`, `toonBlue[Anima4]`, `toonBlue[Anima5]`, `toonBlue[Anima6]`), na.rm = TRUE)) %>%
    mutate(`toonBlue[LikeMean]` = rowMeans(select(., `toonBlue[Like1]`, `toonBlue[Like2]`, `toonBlue[Like3]`, `toonBlue[Like4]`, `toonBlue[Like5]`), na.rm = TRUE)) %>%
    mutate(`toonBlue[IntMean]` = rowMeans(select(., `toonBlue[Int1]`, `toonBlue[Int2]`, `toonBlue[Int3]`, `toonBlue[Int4]`, `toonBlue[Int5]`), na.rm = TRUE)) %>%
    relocate(`toonBlue[AnthroMean]`, .after = `toonBlue[Anthro5]`) %>%  
    relocate(`toonBlue[AnimaMean]`, .after = `toonBlue[Anima6]`) %>%
    relocate(`toonBlue[LikeMean]`, .after = `toonBlue[Like5]`) %>%
    relocate(`toonBlue[IntMean]`, .after = `toonBlue[Int5]`) 
  
  data <- data %>%
    mutate(`hyperRealistic[AnthroMean]` = rowMeans(select(., `hyperRealistic[Anthro1]`, `hyperRealistic[Anthro2]`, `hyperRealistic[Anthro3]`, `hyperRealistic[Anthro4]`, `hyperRealistic[Anthro5]`), na.rm = TRUE)) %>%
    mutate(`hyperRealistic[AnimaMean]` = rowMeans(select(., `hyperRealistic[Anima1]`, `hyperRealistic[Anima2]`, `hyperRealistic[Anima3]`, `hyperRealistic[Anima4]`, `hyperRealistic[Anima5]`, `hyperRealistic[Anima6]`), na.rm = TRUE)) %>%
    mutate(`hyperRealistic[LikeMean]` = rowMeans(select(., `hyperRealistic[Like1]`, `hyperRealistic[Like2]`, `hyperRealistic[Like3]`, `hyperRealistic[Like4]`, `hyperRealistic[Like5]`), na.rm = TRUE)) %>%
    mutate(`hyperRealistic[IntMean]` = rowMeans(select(., `hyperRealistic[Int1]`, `hyperRealistic[Int2]`, `hyperRealistic[Int3]`, `hyperRealistic[Int4]`, `hyperRealistic[Int5]`), na.rm = TRUE)) %>%
    relocate(`hyperRealistic[AnthroMean]`, .after = `hyperRealistic[Anthro5]`) %>%  
    relocate(`hyperRealistic[AnimaMean]`, .after = `hyperRealistic[Anima6]`) %>%
    relocate(`hyperRealistic[LikeMean]`, .after = `hyperRealistic[Like5]`) %>%
    relocate(`hyperRealistic[IntMean]`, .after = `hyperRealistic[Int5]`) 
  
  data <- data %>%
    mutate(`realisticWeird[AnthroMean]` = rowMeans(select(., `realisticWeird[Anthro1]`, `realisticWeird[Anthro2]`, `realisticWeird[Anthro3]`, `realisticWeird[Anthro4]`, `realisticWeird[Anthro5]`), na.rm = TRUE)) %>%
    mutate(`realisticWeird[AnimaMean]` = rowMeans(select(., `realisticWeird[Anima1]`, `realisticWeird[Anima2]`, `realisticWeird[Anima3]`, `realisticWeird[Anima4]`, `realisticWeird[Anima5]`, `realisticWeird[Anima6]`), na.rm = TRUE)) %>%
    mutate(`realisticWeird[LikeMean]` = rowMeans(select(., `realisticWeird[Like1]`, `realisticWeird[Like2]`, `realisticWeird[Like3]`, `realisticWeird[Like4]`, `realisticWeird[Like5]`), na.rm = TRUE)) %>%
    mutate(`realisticWeird[IntMean]` = rowMeans(select(., `realisticWeird[Int1]`, `realisticWeird[Int2]`, `realisticWeird[Int3]`, `realisticWeird[Int4]`, `realisticWeird[Int5]`), na.rm = TRUE)) %>%
    relocate(`realisticWeird[AnthroMean]`, .after = `realisticWeird[Anthro5]`) %>%  
    relocate(`realisticWeird[AnimaMean]`, .after = `realisticWeird[Anima6]`) %>%
    relocate(`realisticWeird[LikeMean]`, .after = `realisticWeird[Like5]`) %>%
    relocate(`realisticWeird[IntMean]`, .after = `realisticWeird[Int5]`) 
  
  data <- data %>%
    mutate(`panda[AnthroMean]` = rowMeans(select(., `panda[Anthro1]`, `panda[Anthro2]`, `panda[Anthro3]`, `panda[Anthro4]`, `panda[Anthro5]`), na.rm = TRUE)) %>%
    mutate(`panda[AnimaMean]` = rowMeans(select(., `panda[Anima1]`, `panda[Anima2]`, `panda[Anima3]`, `panda[Anima4]`, `panda[Anima5]`, `panda[Anima6]`), na.rm = TRUE)) %>%
    mutate(`panda[LikeMean]` = rowMeans(select(., `panda[Like1]`, `panda[Like2]`, `panda[Like3]`, `panda[Like4]`, `panda[Like5]`), na.rm = TRUE)) %>%
    mutate(`panda[IntMean]` = rowMeans(select(., `panda[Int1]`, `panda[Int2]`, `panda[Int3]`, `panda[Int4]`, `panda[Int5]`), na.rm = TRUE)) %>%
    relocate(`panda[AnthroMean]`, .after = `panda[Anthro5]`) %>%  
    relocate(`panda[AnimaMean]`, .after = `panda[Anima6]`) %>%
    relocate(`panda[LikeMean]`, .after = `panda[Like5]`) %>%
    relocate(`panda[IntMean]`, .after = `panda[Int5]`) 
  
  # calculate godspeed mean
  data <- data %>%
    mutate(`toonBrown[AnthroMean]` = rowMeans(select(., `toonBrown[Anthro1]`, `toonBrown[Anthro2]`, `toonBrown[Anthro3]`, `toonBrown[Anthro4]`, `toonBrown[Anthro5]`), na.rm = TRUE)) %>%
    mutate(`toonBrown[AnimaMean]` = rowMeans(select(., `toonBrown[Anima1]`, `toonBrown[Anima2]`, `toonBrown[Anima3]`, `toonBrown[Anima4]`, `toonBrown[Anima5]`, `toonBrown[Anima6]`), na.rm = TRUE)) %>%
    mutate(`toonBrown[LikeMean]` = rowMeans(select(., `toonBrown[Like1]`, `toonBrown[Like2]`, `toonBrown[Like3]`, `toonBrown[Like4]`, `toonBrown[Like5]`), na.rm = TRUE)) %>%
    mutate(`toonBrown[IntMean]` = rowMeans(select(., `toonBrown[Int1]`, `toonBrown[Int2]`, `toonBrown[Int3]`, `toonBrown[Int4]`, `toonBrown[Int5]`), na.rm = TRUE)) %>%
    relocate(`toonBrown[AnthroMean]`, .after = `toonBrown[Anthro5]`) %>%  
    relocate(`toonBrown[AnimaMean]`, .after = `toonBrown[Anima6]`) %>%
    relocate(`toonBrown[LikeMean]`, .after = `toonBrown[Like5]`) %>%
    relocate(`toonBrown[IntMean]`, .after = `toonBrown[Int5]`) 
  
  data <- data %>%
    mutate(`Robot[AnthroMean]` = rowMeans(select(., `Robot[Anthro1]`, `Robot[Anthro2]`, `Robot[Anthro3]`, `Robot[Anthro4]`, `Robot[Anthro5]`), na.rm = TRUE)) %>%
    mutate(`Robot[AnimaMean]` = rowMeans(select(., `Robot[Anima1]`, `Robot[Anima2]`, `Robot[Anima3]`, `Robot[Anima4]`, `Robot[Anima5]`, `Robot[Anima6]`), na.rm = TRUE)) %>%
    mutate(`Robot[LikeMean]` = rowMeans(select(., `Robot[Like1]`, `Robot[Like2]`, `Robot[Like3]`, `Robot[Like4]`, `Robot[Like5]`), na.rm = TRUE)) %>%
    mutate(`Robot[IntMean]` = rowMeans(select(., `Robot[Int1]`, `Robot[Int2]`, `Robot[Int3]`, `Robot[Int4]`, `Robot[Int5]`), na.rm = TRUE)) %>%
    relocate(`Robot[AnthroMean]`, .after = `Robot[Anthro5]`) %>%  
    relocate(`Robot[AnimaMean]`, .after = `Robot[Anima6]`) %>%
    relocate(`Robot[LikeMean]`, .after = `Robot[Like5]`) %>%
    relocate(`Robot[IntMean]`, .after = `Robot[Int5]`) 
  
  return(data)
}
data <- add_mean_columns(data)

# ---------------- statistical tests ----------------

# ----- anthropomorphism anova ----
# normality checks, shapiro: if p > 0.05, it is normally distributed
columns <- data[, c("toonBlue[AnthroMean]", "toonBrown[AnthroMean]", "hyperRealistic[AnthroMean]",
                    "realisticWeird[AnthroMean]", "panda[AnthroMean]", "Robot[AnthroMean]")]
shapiro_anthro <- lapply(columns, shapiro.test)
describe(columns)
print(shapiro_anthro)
# usually it violated the homogeneity of variances for the anthro condition -> conduct a kruskal wallis test
hist(data$`Robot[AnthroMean]`) # anthro for robot is probably not normally distributed -> skewed towards low anthro values

anova_anthro <- function(data){
  data_selected <- data %>%
    select(`toonBlue[AnthroMean]`, `toonBrown[AnthroMean]`, `hyperRealistic[AnthroMean]`,
           `realisticWeird[AnthroMean]`, `panda[AnthroMean]`, `Robot[AnthroMean]`)
  
  # convertion to long format for anova
  data_long <- data_selected %>%
    pivot_longer(cols = everything(),
                 names_to = "Condition",
                 values_to = "AnthroMean")
  
  levene_test <- leveneTest(AnthroMean ~ Condition, data = data_long)
  print(levene_test)
  
  res = levene_test$`Pr(>F)`[1]
  
  if (levene_test$`Pr(>F)`[1] > 0.05) {
    print(paste("Varianz gleichverteilt: p = ", res))
  } else {
    print(paste("Varianz nicht gleichverteilt: p = ", res))
  }
  
  
  anova_result <- aov(AnthroMean ~ Condition, data = data_long)
  summary(anova_result)
  
  kruskal_test <- kruskal.test(AnthroMean ~ Condition, data = data_long)
  print(kruskal_test)
  
  summary_stats <- data_long %>%
    group_by(Condition) %>%
    summarize(Mean = mean(AnthroMean), SD = sd(AnthroMean))
  
  print(summary_stats)
  
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
  max_condition <- summary_stats %>%
    filter(Mean == max(Mean)) %>%
    select(Condition, Mean)
  
  min_condition <- summary_stats %>%
    filter(Mean == min(Mean)) %>%
    select(Condition, Mean)
  
  print(paste("Condition with maximum mean value:", max_condition$Condition, "Mean:", max_condition$Mean))
  print(paste("Condition with minimum mean value:", min_condition$Condition, "Mean:", min_condition$Mean))

}

anova_anthro_with_log_trans <- function(data){
  data_selected <- data %>%
    select(`toonBlue[AnthroMean]`, `toonBrown[AnthroMean]`, `hyperRealistic[AnthroMean]`,
           `realisticWeird[AnthroMean]`, `panda[AnthroMean]`, `Robot[AnthroMean]`)
  
  data_log_transformed <- log(data_selected)
  
  data_long_log <- data_log_transformed %>%
    pivot_longer(cols = everything(),
                 names_to = "Condition",
                 values_to = "AnthroMean")
  
  # Conduct Levene's test for homogeneity of variances on log-transformed data
  levene_test_log <- leveneTest(AnthroMean ~ Condition, data = data_long_log)
  print(levene_test_log)
  
  res = levene_test$`Pr(>F)`[1]
  
  if (levene_test$`Pr(>F)`[1] > 0.05) {
    print(paste("Varianz gleichverteilt: p = ", res))
  } else {
    print(paste("Varianz nicht gleichverteilt: p = ", res))
  }
  
  # If Levene's test p-value > 0.05, proceed with ANOVA
  if (levene_test_log$`Pr(>F)`[1] > 0.05) {
    anova_result_log <- aov(AnthroMean ~ Condition, data = data_long_log)
    print(summary(anova_result_log))
    
    # Conduct post-hoc test to find out which specific variables have significant differences
    tukey_result_log <- TukeyHSD(anova_result_log)
    print(tukey_result_log)
    
    # Check the mean values for each variable
    mean_values_log <- data_long_log %>%
      group_by(Condition) %>%
      summarise(Mean = mean(AnthroMean, na.rm = TRUE))
    
    print(mean_values_log)
  } else {
    print("The data does not meet the assumption of homogeneity of variances for ANOVA after log transformation.")
  }
  
  max_condition <- summary_stats %>%
    filter(Mean == max(Mean)) %>%
    select(Condition, Mean)
  
  min_condition <- summary_stats %>%
    filter(Mean == min(Mean)) %>%
    select(Condition, Mean)
  
  print(paste("Condition with maximum mean value:", max_condition$Condition, "Mean:", max_condition$Mean))
  print(paste("Condition with minimum mean value:", min_condition$Condition, "Mean:", min_condition$Mean))
}

kruskal_wallis_anthro <- function(data) {
  data_selected <- data %>%
    select(`toonBlue[AnthroMean]`, `toonBrown[AnthroMean]`, `hyperRealistic[AnthroMean]`,
           `realisticWeird[AnthroMean]`, `panda[AnthroMean]`, `Robot[AnthroMean]`)
  
  data_long <- data_selected %>%
    pivot_longer(cols = everything(),
                 names_to = "Condition",
                 values_to = "AnthroMean")
  
  kruskal_result <- kruskal.test(AnthroMean ~ Condition, data = data_long)
  print(kruskal_result)
  
  if (kruskal_result$p.value < 0.05) {
    
    dunn_result <- dunn.test(data_long$AnthroMean, data_long$Condition, method = "bonferroni")
    
    print(dunn_result)
    
    group_means <- aggregate(AnthroMean ~ Condition, data = data_long, mean)
    print(group_means)
  }
}

kruskal_wallis_anthro(data)

# wilcox.test(data$`toonBlue[AnthroMean]`, data$`panda[AnthroMean]`)

anova_anthro(data)

create_boxplot <- function(columns, godspeed_var) {
  # Calculate the means for each column
  column_means <- colMeans(columns)
  # Order the columns by their means
  ordered_columns <- columns[, order(column_means)]
  
  # Convert the ordered columns to a data frame for easy manipulation
  ordered_df <- as.data.frame(ordered_columns)
  
  # Rename the columns to remove special characters for easy plotting
  colnames(ordered_df) <- c("Toon", "Toon Uncanny", "Realistic", "Realistic Uncanny", "Panda", "Robot")[order(column_means)]
  
  # Melt the data frame for ggplot
  melted_df <- melt(ordered_df)
  
  # Create the boxplots
  ggplot(melted_df, aes(x = variable, y = value)) + 
    geom_boxplot() +
    labs(x = "Condition", y = godspeed_var, title = paste("Boxplots of", godspeed_var)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_boxplot(columns, "Anthropomorphism")


# ----- anima anova -----
columns <- data[, c("toonBlue[AnimaMean]", "toonBrown[AnimaMean]", "hyperRealistic[AnimaMean]",
                    "realisticWeird[AnimaMean]", "panda[AnimaMean]", "Robot[AnimaMean]")]
shapiro_anima <- lapply(columns, shapiro.test)
print(shapiro_anima)
anova_anima <- function(data) {
  data_selected <- data %>%
    select(`toonBlue[AnimaMean]`, `toonBrown[AnimaMean]`, `hyperRealistic[AnimaMean]`,
           `realisticWeird[AnimaMean]`, `panda[AnimaMean]`, `Robot[AnimaMean]`)
  
  data_long <- data_selected %>%
    pivot_longer(cols = everything(),
                 names_to = "Condition",
                 values_to = "AnimaMean")
  
  levene_test <- leveneTest(AnimaMean ~ Condition, data = data_long)
  print(levene_test)
  
  res = levene_test$`Pr(>F)`[1]
  
  if (levene_test$`Pr(>F)`[1] > 0.05) {
    print(paste("Varianz gleichverteilt: p = ", res))
  } else {
    print(paste("Varianz nicht gleichverteilt: p = ", res))
  }
  
  anova_result <- aov(AnimaMean ~ Condition, data = data_long)
  summary(anova_result)
  
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)

  summary_stats <- data_long %>%
    group_by(Condition) %>%
    summarize(Mean = mean(AnimaMean), SD = sd(AnimaMean))
  
  print(summary_stats)
  
  max_condition <- summary_stats %>%
    filter(Mean == max(Mean)) %>%
    select(Condition, Mean)
  
  min_condition <- summary_stats %>%
    filter(Mean == min(Mean)) %>%
    select(Condition, Mean)
  
  print(paste("Condition with maximum mean value:", max_condition$Condition, "Mean:", max_condition$Mean))
  print(paste("Condition with minimum mean value:", min_condition$Condition, "Mean:", min_condition$Mean))
}

kruskal_wallis_anima <- function(data) {
  data_selected <- data %>%
    select(`toonBlue[AnimaMean]`, `toonBrown[AnimaMean]`, `hyperRealistic[AnimaMean]`,
           `realisticWeird[AnimaMean]`, `panda[AnimaMean]`, `Robot[AnimaMean]`)
  
  data_long <- data_selected %>%
    pivot_longer(cols = everything(),
                 names_to = "Condition",
                 values_to = "AnimaMean")
  
  kruskal_result <- kruskal.test(AnimaMean ~ Condition, data = data_long)
  print(kruskal_result)
  
  if (kruskal_result$p.value < 0.05) {
    
    dunn_result <- dunn.test(data_long$AnimaMean, data_long$Condition, method = "bonferroni")
    
    print(dunn_result)
    
    group_means <- aggregate(AnimaMean ~ Condition, data = data_long, mean)
    print(group_means)
  }
}
anova_anima(data) # varianz nicht gleichverteilt, levene's p = 0.0057
kruskal_wallis_anima(data)

create_boxplot(columns, "Anima")


# ----- likability anova -----
columns <- data[, c("toonBlue[LikeMean]", "toonBrown[LikeMean]", "hyperRealistic[LikeMean]",
                    "realisticWeird[LikeMean]", "panda[LikeMean]", "Robot[LikeMean]")]
shapiro_like <- lapply(columns, shapiro.test)
print(shapiro_like)

anova_like <- function(data) {
  data_selected <- data %>%
    select(`toonBlue[LikeMean]`, `toonBrown[LikeMean]`, `hyperRealistic[LikeMean]`,
           `realisticWeird[LikeMean]`, `panda[LikeMean]`, `Robot[LikeMean]`)
  
  data_long <- data_selected %>%
    pivot_longer(cols = everything(),
                 names_to = "Condition",
                 values_to = "LikeMean")
  
  anova_result <- aov(LikeMean ~ Condition, data = data_long)
  print(summary(anova_result))
  
  levene_test <- leveneTest(LikeMean ~ Condition, data = data_long)
  print(levene_test)
  
  res = levene_test$`Pr(>F)`[1]
  
  if (levene_test$`Pr(>F)`[1] > 0.05) {
    print(paste("Varianz gleichverteilt: p = ", res))
  } else {
    print(paste("Varianz nicht gleichverteilt: p = ", res))
  }
  
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
  summary_stats <- data_long %>%
    group_by(Condition) %>%
    summarize(Mean = mean(LikeMean), SD = sd(LikeMean))
  
  print(summary_stats)
  
  like_max_condition <<- summary_stats %>%
    filter(Mean == max(Mean)) %>%
    select(Condition, Mean)
  
  like_min_condition <<- summary_stats %>%
    filter(Mean == min(Mean)) %>%
    select(Condition, Mean)
  
  print(paste("Condition with maximum mean value:", like_max_condition$Condition, "Mean:", max_condition$Mean))
  print(paste("Condition with minimum mean value:", like_min_condition$Condition, "Mean:", min_condition$Mean))
}

anova_like(data)
# t.test(data$`toonBrown[LikeMean]`, data$`hyperRealistic[LikeMean]`)

create_boxplot(columns, "Likability")

# ----- Intelligence anova -----
columns <- data[, c("toonBlue[IntMean]", "toonBrown[IntMean]", "hyperRealistic[IntMean]",
                    "realisticWeird[IntMean]", "panda[IntMean]", "Robot[IntMean]")]
shapiro_int <- lapply(columns, shapiro.test)
print(shapiro_int)
describe(columns)

hist(data$`Robot[IntMean]`)

anova_int <- function(data) {
  data_selected <- data %>%
    select(`toonBlue[IntMean]`, `toonBrown[IntMean]`, `hyperRealistic[IntMean]`,
           `realisticWeird[IntMean]`, `panda[IntMean]`, `Robot[IntMean]`)
  
  data_long <- data_selected %>%
    pivot_longer(cols = everything(),
                 names_to = "Condition",
                 values_to = "IntMean")
  
  levene_test <- leveneTest(IntMean ~ Condition, data = data_long)
  print(levene_test)
  
  res = levene_test$`Pr(>F)`[1]
  
  if (levene_test$`Pr(>F)`[1] > 0.05) {
    print(paste("Varianz gleichverteilt: p = ", res))
  } else {
    print(paste("Varianz nicht gleichverteilt: p = ", res))
  }
  
  anova_result <- aov(IntMean ~ Condition, data = data_long)
  print(summary(anova_result))
  print(anova_result)
  
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
  summary_stats <- data_long %>%
    group_by(Condition) %>%
    summarize(Mean = mean(IntMean), SD = sd(IntMean))
  
  print(summary_stats)
  
  max_condition <- summary_stats %>%
    filter(Mean == max(Mean)) %>%
    select(Condition, Mean)
  
  min_condition <- summary_stats %>%
    filter(Mean == min(Mean)) %>%
    select(Condition, Mean)
  
  print(paste("Condition with maximum mean value:", max_condition$Condition, "Mean:", max_condition$Mean))
  print(paste("Condition with minimum mean value:", min_condition$Condition, "Mean:", min_condition$Mean))
}

anova_int(data)

create_boxplot(columns, "Intelligence")



anova_anthro(data)
anova_anima(data)
anova_like(data)
anova_int(data)


t_panda_toon <- t.test(toonBlue_Anthropomorphism, panda_Anthropomorphism, paired = FALSE)
t_hyper_toon <- t.test(toonBlue_Anthropomorphism, hyperRealistic_Anthropomorphism, paired = FALSE)
print(mean(toonBlue_Anthropomorphism))
print(mean(panda_Anthropomorphism))
print(mean(hyperRealistic_Anthropomorphism))
print(t_panda_toon)
print(t_hyper_toon)
