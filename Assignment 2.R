#1. Binomial - Binomial would be the best fit as the data is discrete since it is
# counting the survivors within a forest plot. Binomial is often used for this type of data
#2. Poisson - Poisson is the best fit for discrete(count) data which is why I believe this would be the best fit. 
#3. Negative binomial/Poisson - Species richness is a count variable. Depending on the
#variability of the data negative binomial or poisson would be the best fit. If variability is high, negative binomial 
#would be the best fit. 
#4. Gamma - Age is a continuous variable which often times is skewed as well. This is why gamma would probably be the best fit
#5. Lognormal/Normal - Body size is a continuous variable. If the data is roughly symmetrical normal distribution can be used
#However, if it is skewed lognormal would be the best fit. 
#6. Lognormal - Population growth rate is a continuous variable which often times are skewed
# This is why I think lognormal would be the best fit. 
#7. Beta - This data is continuous, beta distribution is used for proportions, which is why I chose it
#8. Gamma - Biomass is a continuous positive variable that is often skewed. Gamma works well with skewed positive data
#9. Negative binomial - Seed counts per plant is a discrete variable. I believe that it has great variability
# which is why I think negative binomial would be the best fit
#10. Gamma - Time to germination is a continuous, positive, and can often times be skewed. 
#This is why I think gamma is the best fit.
#11. Gamma - This data is continuous, distance is positive and can be skewed. This is why
# I think Gamma is the best fist
#12. Poisson/Neg. Binomial - This data is discrete, as it is counting visits per flower. For this 
#type of data, poisson or negative binomial is the best fit. If the variation is high, negative binomial is the best fit. 
#13. Normal/Lognormal - Leaf area is continuous and positive. If the data is symmetrical 
# normal distribution would be the best fist. If the data is more skewed lognormal would be a better fit
#14. Normal/Lognormal - Environmental variables are continuous and depending on the data 
# (if it is skewed or not) normal distribution or lognormal would be the best fit. 
#15. Poisson/Neg. Binomial - This data is discrete. If there is little variability in the data
# Poisson is the best fit. If there is a lot of variability, negative binomial is better. 
#16. Poisson/Neg. Binomial - This data is discrete as we are counting fish that are present
# If there is little variability in the data poisson is the best fit. If there is a lot of 
#variability, negative binomial is better. 
#17. Beta - This data is a proportion and it is continuous.Beta is the best fit since it is designed for proportions. 
#18. Binomial - This data is discrete. Larval settlement success is a success/failure system which is why binomial is the best fit.
#19. Gamma - This data is continuous, positive and could be skewed. This is why I think Gamma 
#would be the best fit. 
#20. Beta. This data is a proportion. Beta distribution works the best for proportions which is why I chose it.  

library(tidyverse)
library(readxl)
data_m = read_xlsx("C:/Users/estef/Downloads/Cannicci_et_al_Final_dataset_4.0.xlsx")
data_m  #Most of the data is categorical
library(dplyr)
data_m[is.na(data_m)] <- 0
data_c <- table(data_m$Species, data_m$Country) #Counts
data_c

# Numeric value of interest: species total 
species_totals <- rowSums(data_c)

hist(species_totals, breaks=20, col="pink",
     main="Distribution of total counts per species",
     xlab="Total count", ylab="Number of species")

# The histogram shows that most species have low counts and a few species have high counts.
# The histogram is right-skewed and the data is discrete
# Because of this I think a Poisson or negative binomial distribution would be the best fit. 

# Numeric value of interest: species total 
country_totals <- colSums(data_c)

# Histogram
hist(country_totals, breaks=10, col="#5bc0be",
     main="Distribution of total counts per country",
     xlab="Total count", ylab="Number of countries")

# The histogram shows counts vary widely between countries and it is right-skewed. Since the data used
# is discrete I believe a Poisson/Negative Binomial distribution would be the best fit. 

#Since my data is mostly categorical, counts had to be done to apply distributions. Poisson is the
#recommended distribution to use with counts which are discrete data. The negative binomial distribution 
#can also be used, and it accounts for more variation in the data. 
