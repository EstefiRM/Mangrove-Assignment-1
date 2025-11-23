library(openxlsx)
data_m = read.xlsx("https://github.com/bmaitner/Statistical_ecology_course/raw/refs/heads/main/data/Mangroves/Cannicci_et_al_Final_dataset_4.0.xlsx")
data_m
library(dplyr)
library(tidyr)
data_m[is.na(data_m)] <- 0
data_m

# Do feeding traits vary by Country? --------------------------------------
#Biological question: How are trophic roles shaped by geography in mangrove systems?

table_t= subset_data <- data_m %>% select(4,15:24)
table_t
data_trait <- table_t |> 
  pivot_longer(
    cols = !c(Country),
    names_to = "Feeding_Trait",
    values_to = "Trait_Count"
  ) |>
  mutate(
    Trait_Count = as.integer(Trait_Count))

m1 <- glm(Trait_Count ~ Country, 
          data = data_trait, 
          family = binomial())
#I chose a generalized linear model (GLM) with a binomial distribution and a logit link function. 
#This choice was made because trait count is a binary variable that contains only 0s and 1s, indicating whether
#a trait is present for a species in a given country. The binomial GLM models the probability that functional feeding traits differ among countries. 
#This model assumes that Trait_Count is a function of Country and that each country has its own intercept. This model has the potential to identify geography's influence 
#in functional roles, providing insight on how environmental conditions shape mangrove niches. 

m2 <- glm(Trait_Count ~ Country + Feeding_Trait,
          data = data_trait, 
          family = binomial())

#In this GLM, I included feeding Trait to account for differences in how common or rare each trait is, independent of geography.
#This model evaluates both variation between feeding traits and how those traits differ across countries. Trait count is modeled as a function of both country and feeding trait.
#Each country has its own intercept representing its baseline probability of trait expression, and each feeding trait has its own intercept shift reflecting its inherent frequency. 
#Overall, this model assesses how geographic location and trait commonness shape the distribution of feeding roles in mangrove ecosystems.

m3 <- glm(Trait_Count ~ Country * Feeding_Trait,
          data = data_trait, 
          family = binomial())

#In this model an interaction between country and feeding trait is added to test if the effect of geography on trait presence depends
#on the type of feeding trait. Different countries may favor certain feeding niches due to differences in habitat structure, productivity,
#tidal regime, or species composition. This model assumes that trait count is jointly a function of country, feeding trait, and their interaction.
#Each country has its own intercept, each feeding trait has its own baseline probability of being expressed, and the interaction term allows the probability 
#of expressing a trait to vary depending on the specific combination of country and feeding trait.

library(bbmle)
AICtab(m1, m2, m3)

#AIC comparison indicated that Model 3, which includes the country and feeding trait interaction, provided the best fit to the data. These results suggest that the relationship between 
#geography and trait expression depends on the specific feeding trait being considered. Different countries do not simply differ in overall trait frequencies, they differ in which traits
#are expressed. This indicates that functional roles in mangrove ecosystems are shaped by interactions between geographic context and trait identity. 


# How does species richness varies across tidal ranges? --------------------
#Biological question: How does species richness varies across tidal ranges?
data_m

data_richness <- data_m |>
  group_by(`site.#`, Tidal.Regime) |>
  summarize(
    species_richness = n(),
    .groups = "drop"
  )
data_richness

spec_m1 <- lm(species_richness ~ Tidal.Regime, data = data_richness)
summary(spec_m1)

#I chose a linear model to evaluate whether species richness varies across tidal regimes. Species richness in this dataset is a continuous count summarized at the site level, which makes
#a linear model an appropriate starting point to compare mean richness across tidal regimes. In this model, species richness is treated as a function of tidal regime, and each tidal category 
#is allowed to have its own intercept representing its average species richness. I included tidal regime as the predictor because the biological question asks whether hydrodynamic differences
#influence the number of species present at each mangrove site. Tidal regimes can alter inundation frequency, sediment dynamics, nutrient availability, and habitat complexity which can shape 
#the species richness of crustaceans and mollusks in mangrove ecosystems.

spec_m2 <- glm(species_richness ~ Tidal.Regime,
          family = poisson(),
          data = data_richness)
summary(spec_m2)

#I used a Poisson generalized linear model because species richness is a count variable, and Poisson models are specifically designed for count data with non-negative integer values.In this model,
#species richness is treated as a function of tidal regime, and each tidal category has its own intercept representing its baseline expected richness on the log scale. This model evaluates how
#expected species richness differs across micro, meso, and macro tidal systems. If tidal regime is significant in the Poisson GLM, it suggests that hydrodynamic energy and water-movement patterns 
#play a direct role in mangrove species richness. 

library(MASS)

spec_m3 <- glm.nb(species_richness ~ Tidal.Regime,
             data = data_richness)
summary(spec_m3)

#I used a negative binomial GLM to identify if species richness varies across tidal regimes. This model can show overdispersion. Tidal regime was included as the predictor because the biological 
#question focuses on whether hydrodynamic differences influence biodiversity in mangrove sites. This model assumes that species richness is a function of tidal regime, with each tidal category having 
#its own intercept. The inclusion of a dispersion parameter allows the model to capture extra variability. The negative binomial GLM evaluates whether hydrodynamic intensity predicts how many species 
#a mangrove site can support, while acknowledging that natural ecosystems contain more variability. 

AICtab(spec_m1, spec_m2, spec_m3)

#Model 3 shows the best fit to the species richness data. This is because the negative binomial GLM captures the variation in the species richness across tidal regimes. This shows that hydrodynamic conditions
#within the micro, meso, macro tidal regimes influence biodiversity in mangrove ecosystems worldwide. 