#Part 1. Code
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)


data_m = read.xlsx("https://github.com/bmaitner/Statistical_ecology_course/raw/refs/heads/main/data/Mangroves/Cannicci_et_al_Final_dataset_4.0.xlsx")
data_m
data_m[is.na(data_m)] <- 0    #replacing NAs for zeros

# Figure 1 ----------------------------------------------------------------
table1= table(data_m$Species, data_m$Country, data_m$Encrustering, data_m$Phylum)
table1                    
country_totals <- data_m %>%
  count(Country)
country_totals

ggplot(country_totals, aes(x = Country, y = n)) +
  geom_bar(stat = "identity", fill= "pink") +
  theme_classic() +
  labs(
    title = " Global Species Richness of Invertebrates in Mangrove Ecosystems",
    y = "Species Number") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )
# Figure 1. Global Species Richness of Crustaceans and Mollusks in Mangrove Ecosystems. This graph shows which countries have the highest recorded diversity of crustaceans and mollusks in their mangrove forests. Hong Kong has the greatest biodiversity of these two phyla, while Mauritania has the lowest.



# Figure 2 ----------------------------------------------------------------
tp.data <- data.frame(
  Tidal = c("Micro", "Meso", "Macro"),
  Crustacea = c(64, 291, 15),
  Mollusca = c(54, 161, 2)
)
tp.data

tp.data$Total <- tp.data$Crustacea + tp.data$Mollusca
tp.data

library(tidyr)
tp.data.long <- tp.data %>%
  pivot_longer(cols = c("Crustacea", "Mollusca"),
               names_to = "Phylum",
               values_to = "Count")
tp.data.long
tp.data.long$Tidal <- factor(tp.data.long$Tidal, levels = c("Micro", "Meso", "Macro"))

ggplot(tp.data.long, aes(x = Tidal, y = Count, fill = Phylum)) +
  geom_col(position = position_dodge(width = 0.9)) +
  theme_light() +
  scale_fill_manual(
    values = c("Crustacea" = "#1E3888",  
               "Mollusca"  = "#47A8BD") ) +
  labs(
    title = "Presence of Crustacea and Mollusca Across Mangrove Tidal Regimes",
    x = "Tidal Regime",
    y = "Organisms Number"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))

#Figure 2. Presence of Crustacea and Mollusca Across Mangrove Tidal Regimes. This graph shows the ratio of Crustacea and Mollusca present across different tidal ranges worldwide. Meso has the highest number of Crustacea and Mollusca present. 




# Figure 3 ----------------------------------------------------------------
table3= subset_data <- data_m %>% select(1,15:24)
table3
trait_long <- table3 %>%
  pivot_longer(
    cols = -`site.#`,        
    names_to = "Trait",
    values_to = "Value"
  )
trait_long

trait_counts <- trait_long %>%
  group_by(Trait) %>%
  summarise(count = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(count))
trait_counts

ggplot(trait_counts, aes(x = reorder(Trait, count), y = count)) +
  geom_col(fill = "#7e9181") +
  coord_flip() +
  labs(
    title = "Trophic Roles Observed in Mangroves",
    x = "",
    y = "Organisms Number"
  ) +
  theme_minimal() 

#Figure 3. Common Trophic Roles Played on Mangroves. This graph shows the common trophic roles of organisms in mangrove forests. The analysis is focused on crustaceans and molluscs. The most frequent roles were microalgae and bacteria feeder and detritivore. 




# GLMS --------------------------------------------------------------------

#1.Do feeding traits vary by Country?

feeding_trait_glm <- glm(Trait_Count ~ Country * Feeding_Trait,
          data = data_trait, 
          family = binomial())
summary(feeding_trait_glm)

#2. Does species richness vary across tidal ranges?
species_tidal_nb <- glm.nb(species_richness ~ Tidal.Regime,
                           data = data_richness)
summary(species_tidal_nb)
