#2017-01-11

# Introduction to brms (alternative to map2stan)

install.packages("devtools")
library(devtools)
install_github("paul-buerkner/brms")

library(brms)
library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
# I get errors.  Try updateing version of R
# https://andreacirilloblog.wordpress.com/2015/10/22/updater-package-update-r-version-with-a-function-on-mac-osx/
#will let you update R frm Rstudio

require(devtools)
install_github('andreacirilloac/updateR')
library(updateR)
updateR(admin_password = "/slithY/")
# OK, I now have R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"

# try again

rstan_options(auto_write = TRUE)  # now it works
options(mc.cores = parallel::detectCores())
tomato <- read.csv("./Assignment_Chapter_09/TomatoR2CSHL.csv" )
summary(tomato)
head(tomato)

# now fit a model of hy length as a function of trt and species, with shelf as random effect

hyp1 <- brm(hyp ~ trt + species + (1|shelf),
            data = tomato, 
            prior = set_prior("normal(0,10)", class = "b")
            )

summary(hyp1)  # what does 'thin = 1' mean?
plot(hyp1, ask = FALSE)  # what does 'b' mean here?  slope est?
launch_shiny(hyp1)
# I dont' understand all this. treedepth?

# Specifying priors
# intercept = overall intercept
# b class is for coeff on fixed effects
# sigma class is overall SD
# sd class is for SD of random effect terms

# in above example, treatment and all species get same prior (all are class b)

# now same model but differnet priors for species and treatment

hyp2 <- brm(hyp ~ trt + species + (1|shelf),
            data = tomato,
            prior = c(set_prior("normal(0,10)",class="b"), # sets prior for all b coefficients not otherwise specified
                      set_prior("normal(0,5)",class="b", coef = "trtL")) #sets a different prior for the "trtL" coefficient
) 

summary(hyp2)
plot(hyp2, ask = FALSE)

# see all possible priors

get_prior(hyp ~ trt + species + (1|shelf),
          data = tomato)
### WHAT Is that first row?

hyp2$prior

# just for completeness, set same priors but one by one

hyp3 <- brm(hyp ~ trt + species + (1|shelf),
            data = tomato,
            prior = c(
              set_prior("normal(33,10)",class="Intercept"), # prior for the intercept
              set_prior("normal(0,10)",class="b"), # sets prior for all b coefficients not otherwise specified
              set_prior("normal(0,5)",class="b", coef = "trtL"), #set prior for "trtL"
              set_prior("cauchy(0,1)", class = "sigma"), #half cauchy prior for sigma
              set_prior("normal(0,1)", class = "sd", group = "shelf") #prior for variation due to shelf
            ) 
)

compare(hyp1,hyp2, hyp3)  # doesn't work.  no WAIC method.  And 
# doesn't work because 'different number of rows"

#  Stan parametrs

hyp4 <- brm(hyp ~ trt + species + (1|shelf),
            data = tomato,
            prior = set_prior("normal(0,10)",class="b"), 
            chains = 4, #the default anyway
            iter=4000,
            warmup = 1000,
            cores = 2
)  # still get warnings about divergent transitions (??)
summary(hyp4)
plot(hyp4)

# now a model with interactions

hyp5 <- brm(hyp ~ trt * species + (1|shelf), #trt by species interaction
            data = tomato,
            prior = set_prior("normal(0,10)",class="b")
)
summary(hyp5)
plot(hyp5, ask = FALSE)
 # compare models

waic(hyp1, hyp5)
loo(hyp1, hyp5)

# SE is close to the difference between the models.

## OK, now set a unique intercept for each species (not dealing with shelf as random effect here)

hyp6 <- brm(hyp ~ species + trt,
            prior = set_prior("normal(0,10)",class="b"),
            data = tomato)

hyp7 <- brm(hyp ~ 0 + species + trt,
            prior = c(
              set_prior("normal(33,10)", class = "b"), #average species height
              set_prior("normal(0,10)", class = "b", coef = "trtL")  # just for treatment
            ),
            data=tomato)

summary(hyp6)
summary(hyp7)  # likes this better; larger Eff.Sample numbers

loo(hyp6, hyp7)  # but the models are just about identirical

# Non Gaussian models

germination <- read.csv("../Ch10_2016-10-21/LironDataTime1.csv")
summary(germination)
head(germination)
germination$Germination2 <- ifelse(germination$germ=="Yes",1,0) #need numeric response
germination$Temperature_b <- germination$temp - min(germination$temp) #baseline it
head(germination)
summary(germination)

germ1 <- brm(Germination2 | trials(1) ~ species + Temperature_b,
             family = "binomial",
             prior=set_prior("normal(0,10)", class="b"),
             data = germination
)

summary(germ1)

germ2 <- brm(Germination2  ~ species + Temperature_b,
             family = "bernoulli",
             prior=set_prior("normal(0,10)", class="b"),
             data = germination
) 

summary(germ2)
waic(germ1, germ2)  # same

# Hypothesis testing

summary(hyp1) # hyp ~ treatment + species, shelf as random effect

hypothesis(hyp1, "trtL = 0 ")
plot(hypothesis(hyp1, "trtL = 0 ")) # pretty slick!! 

hypothesis(hyp1, "trtL >0")
plot(hypothesis(hyp1, "trtL >0"))

# Evidence ratio??  WHAT IS THAT??

hypothesis(hyp1, "speciesS.pennellii-speciesS.habrochaites = 0")

plot(hypothesis(hyp1, "speciesS.pennellii-speciesS.habrochaites = 0"))
# THere is a differnece between the species

## PRETTY SLICK!

# comments:  group level effects are the things you don't care about
#  population level effects are the things you want to estimate