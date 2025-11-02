library(emdbook)
library(ggplot2)
library(readr)
library(tidyverse)
library(stats4)
library(bbmle)

#PART 1

# 1a ----------------------------------------------------------------------

# This model is focused on explaining how often papers are cited as a function of different factors.
# r_scripts_available is a binary variable, where papers either share their code (1) or don't (0)
# age_y is the age, in years, of a publication
# open access is a binary variable that tells whether the paper is free to access (1) or not (0)


#load and reformat the data

citation_data <- readr::read_rds("https://github.com/bmaitner/R_citations/raw/refs/heads/main/data/cite_data.RDS") %>%
  mutate(age_y = 2022-year) %>%
  mutate(r_scripts_available = case_when(r_scripts_available == "yes" ~ 1,
                                         r_scripts_available == "no" ~ 0)) %>%
  mutate(citations = as.numeric(citations),
         open_access = as.numeric(open_access)) %>%
  ungroup()%>%
  select(r_scripts_available,citations,open_access,age_y) %>%
  na.omit()

# plot if you like 

citation_data %>%
  ggplot(mapping = aes(x = age_y,y = citations))+
  geom_point()

# Fit a full model

cites.fit <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int +
                                      rsa*r_scripts_available^d +
                                      oa*open_access^c),
                  start = list(a=0.17,
                               int=1,
                               rsa=0.1,
                               oa=0.1,
                               b=1,
                               c=1,
                               d=1),
                  data = citation_data)

# AIC initial
AIC(cites.fit)

#Altering Model 
cites.fit2 <- mle2(
  citations ~ dpois(lambda = exp(int + a*log(age_y + 1) + rsa*r_scripts_available)),
  start = list(a=0.1,
               int=0,
               rsa=0.1),
  data = citation_data)

AIC(cites.fit2)

#Altering Model
cites.fit3 <- mle2(
  citations ~ dpois(lambda = exp(int + a*log(age_y + 1))),
  start = list(a=0.1,
               int=0),
  data = citation_data)

AIC(cites.fit3)

#Altering Model
cites.fit4 <- mle2(
  citations ~ dpois(lambda = exp(int + a*log(age_y + 1) + rsa*r_scripts_available + oa*open_access + i1*log(age_y + 1)*open_access)),
  start = list(a=0.1,
               int=0,
               rsa=0.1, 
               oa=0.1, 
               i1=0.05),
  data = citation_data)

AIC(cites.fit4)

AICtab(cites.fit,cites.fit2,cites.fit3,cites.fit4)

#Cites.fit4 is the model that fits the best as the AIC table states. This model adds an interaction between publication
#age and open-access status. The other models are either too simple or too complex, which do not accurately showcase the data. 

# 1b ----------------------------------------------------------------------

# This model is focused on what determines rates of R code sharing by authors.
# r_scripts_available is a binary variable, where papers either share their code (1) or don't (0)
# year is the year of publication (relative to 2010).
# open_access is a binary variable that tells whether the paper is free to access (1) or not (0)
# data_available is a binary variable that tells whether the data are publicly available (1) or not (0)


code_data <- readr::read_rds("https://github.com/bmaitner/R_citations/raw/refs/heads/main/data/cite_data.RDS") %>%
  mutate(r_scripts_available = case_when(r_scripts_available == "yes" ~ 1,
                                         r_scripts_available == "no" ~ 0)) %>%
  mutate(data_available = case_when(data_available == "yes" ~ 1,
                                    data_available == "no" ~ 0)) %>%
  
  mutate(citations = as.numeric(citations),
         open_access = as.numeric(open_access)) %>%
  mutate(year = year-2010)


# note that for this function I use a logistic transform to ensure the probability stays between 0 and 1 during optimization
# 


sharing.fit <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year^b +
                                               d * data_available^e +
                                               o * open_access^p
                               )),
  start = list(int = 0,
               y = 0,
               b = 1,
               d = 0,
               e = 1,
               o = 0,
               p = 1),
  data = code_data)

sharing.fit
AIC(sharing.fit)

sharing.fit2 <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year +
                                               d * data_available^e +
                                               o * open_access
                               )),
  start = list(int = 0,
               y = 0,
               d = 0,
               e = 1,
               o = 0),
  data = code_data)

#Altering Model
sharing.fit3 <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year +
                                               d * data_available)),
  start = list(int = 0,
               y = 0,
               d = 0),
  data = code_data)

#Altering Model
sharing.fit4 <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int + y*year + d*data_available + o*open_access + i1*year*data_available)),
  start = list(int = 0,
               y = 0,
               d = 0,
               o = 0,
               i1 = 0),
  data = code_data)

AICtab(sharing.fit, sharing.fit2, sharing.fit3, sharing.fit4)

#Sharing.fit4 is the best fit since it has the lowest AIC score. The first model has parameters that do not
#help the data. This model adds a parameter of interaction between the year and the data availability, adding more complexity to the model, helping 
#to find a better fit. 

# 1c ----------------------------------------------------------------------

# This model attempts to explain size variation in the wings of birds
# Wing.length is mean adult wing length
# Mass is mean adult body mass
# Range.size is the area of the geographic range of each species
# Order1 is a categorical variable that lists the taxonomic Order each species fall into.

# Note that I provide two ways to load the avonet dataset in case the csv file won't load for some of you.

avonet <- read_rds("https://github.com/bmaitner/Statistical_ecology_course/raw/refs/heads/main/data/Avonet/AVONET1_BirdLife.rds") %>%
  select(Order1, Wing.Length, Mass, Range.Size) %>%
  na.omit()

avonet  <- read.csv("https://github.com/bmaitner/Statistical_ecology_course/raw/refs/heads/main/data/Avonet/AVONET1_BirdLife.csv") %>%
  select(Order1, Wing.Length, Mass, Range.Size) %>%
  na.omit()

avonet %>%
  ggplot(mapping = aes(y=Wing.Length,x=Mass))+
  geom_point()

# Note: there is a lot of data here, it may take a while to fit the full model

avonet.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass)^b +
                                          rs*Range.Size,
                                        sdlog = sd),
                   start = list(m = 1,
                                b = 1,
                                sd = 1,
                                int = 0,
                                rs = 10),
                   data = avonet,
                   parameters = list(int ~ Order1))

avonet.fit
#Alter Model
avonet.fit2 <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass) +
                                          rs*Range.Size),
                   start = list(m = 1,
                                int = 0,
                                rs = 10),
                   data = avonet,
                   parameters = list(int ~ Order1))
confint(avonet.fit2)

#Alter Model
avonet.fit3 <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass)^b +
                                          Range.Size,
                                        sdlog = sd),
                   start = list(m = 15,
                                b = 1,
                                sd = 1,
                                int = 0),
                   data = avonet,
                   parameters = list(int ~ Order1))

#Alter Model
avonet.fit4 <- mle2(
  Wing.Length ~ dlnorm(meanlog = int + m*log(Mass) + rs*Range.Size + i1*log(Mass)*Range.Size, sdlog = sd),
  start = list(m = 1, sd = 1, int = 0, rs = 10, i1 = 0),
  data = avonet,
  parameters = list(int ~ Order1))

avonet.fit4
AICtab(avonet.fit, avonet.fit2, avonet.fit3, avonet.fit4)

#Avonet.fit2 is the best fit because of its simple nature. The other models add more complex parameters that
#loosely match the relationship between wing length, body mass, and size. The AIC table confirms that Avonet.fit2
#is the best fit as it has the lowest score. 

#PART 2
library(tidyverse)
library(readxl)
data_m = read_xlsx("C:/Users/estef/Downloads/Cannicci_et_al_Final_dataset_4.0.xlsx")
data_m
library(dplyr)
data_m[is.na(data_m)] <- 0

#Variable - Feeding behavior 
burrow.fit1 <- mle2(
  Burrowing ~ dbinom(size = 1,
                     prob = plogis(int + 
                                     r1*Longitude + 
                                     r2*Latitude +
                                     h1*(Habitat == "Inter_F") + 
                                     h2*(Habitat == "Inter_M") + 
                                     p1*(Phylum == "Crustacea") + 
                                     p2*(Phylum == "Mollusca"))),
  start = list(int = 0, r1 = 0, r2 = 0, h1 = 0, h2 = 0, p1 = 0, p2 = 0),
  data = data_m
)
burrow.fit1
burrow.fit2 <- mle2(
  Burrowing ~ dbinom(size = 1,
                     prob = plogis(int + r1*Longitude + r2*Latitude)),
  start = list(int = 0, r1 = 0, r2 = 0),
  data = data_m
)

burrow.fit3 <- mle2(
  Burrowing ~ dbinom(size = 1,
                     prob = plogis(int + 
                                     h1*(Habitat == "Inter_F") + 
                                     h2*(Habitat == "Inter_M") +
                                     p1*(Phylum == "Crustacea") + 
                                     p2*(Phylum == "Mollusca"))),
  start = list(int = 0, h1 = 0, h2 = 0, p1 = 0, p2 = 0),
  data = data_m
)

AICtab(burrow.fit1, burrow.fit2, burrow.fit3)
burrow.fit1
#The best model fit for the feeding trait variable is burrow.fit1. Burrow.fit1 has the lowest AIC between
#all of the models tested. Burrow.fit1 has a lot of parameters which increase the complexity of the model
#expressing the relationships of the data better. 