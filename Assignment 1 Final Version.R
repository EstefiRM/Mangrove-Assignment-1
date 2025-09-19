library(tidyverse)
library(readxl)
data_m = read_xlsx("C:/Users/estef/Downloads/Cannicci_et_al_Final_dataset_4.0.xlsx")
data_m
#DESCRIPTION: The data used in this assignment come from a study by Cannicci et al.
#The authors created a database to examine the biodiversity of Crustacea and Mollusca 
#in mangrove forests around the world.
#The database includes fields such as country, region, phylum, and species,
#as well as more specific fields like feeding habits, behavioral traits, and tidal regime. 
#Their goal was to assess whether species and functional roles overlap across mangrove forests globally.
#S. Cannicci, S.Y. Lee, H. Bravo, J.R. Cantera-Kintz, F. Dahdouh-Guebas, S. Fratini, M. Fusi, P.J. Jimenez, I. Nordhaus, F. Porri, & K. Diele, A functional analysis reveals extremely low redundancy in global mangrove invertebrate fauna, Proc. Natl. Acad. Sci. U.S.A. 118 (32) e2016913118, https://doi.org/10.1073/pnas.2016913118 (2021).
class(data_m)
#Variables of interest: Country, Species, Tidal Regime, Phylum, Trophic roles (Microalgae & bacteria feeder, Detritivore, Leaf litter & propagule feeder, Omnivore, Macroalgal feeder, Lignivore, Fresh mangrove leaves feeder, Scavenger)
class("Country")
class("Species")
class("Tidal Regime")
class("Phylum")
class("Omnivore") #Applies to all trophic role variables listed in line 8
summary(data_m)
table(data_m$Country, useNA = "ifany")
table(data_m$Phylum, useNA = "ifany")
table(data_m$Species, useNA = "ifany")
table(data_m$`Tidal Regime`, useNA = "ifany")
table(data_m$Region, useNA = "ifany")
prop.table(table(data_m$Species, useNA = "ifany"))
str(data_m)

library(dplyr)
data_m[is.na(data_m)] <- 0
data_m$`Leaf storing`#Checking if N/As were replaced by 0

library(ggplot2)

table1= table(data_m$Species, data_m$Country, data_m$Encrustering, data_m$Phylum)
table1                      #Using tables to visualize the data I want to use
country_totals <- data_m %>%
  count(Country)
country_totals

ggplot(country_totals, aes(x = Country, y = n)) +
  geom_bar(stat = "identity", fill= "pink") +
  theme_classic() +
  labs(
    title = " Global Biodiversity of Crustaceans and Mollusks in Mangrove Ecosystems",
    y = "Species Number") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )
# Figure 1. Global Biodiversity of Crustaceans and Mollusks in Mangrove Ecosystems. 
#This graph shows which countries have the highest recorded diversity of crustaceans and mollusks in their mangrove forests. 
#Hong Kong has the greatest biodiversity of these two phyla, while Mauritania has the lowest.

table2= table(data_m$`Tidal Regime`, data_m$Phylum) 
table2

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

#Figure 2. Presence of Crustacea and Mollusca Across Mangrove Tidal Regimes. 
#This graph shows the ratio of Crustacea and Mollusca present across different tidal ranges worldwide. 
#Meso has the highest number of Crustacea and Mollusca present. 

table3= subset_data <- data_m %>% select(1,15:24)
table3

trait_long <- table3 %>%
  pivot_longer(
    cols = -`site #`,        
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

#Figure 3. Common Trophic Roles Played on Mangroves. 
#This graph shows the common trophic roles of organisms in mangrove forests. 
#The analysis is focused on crustaceans and molluscs. 
#The most frequent roles were microalgae and bacteria feeder and detritivore. 