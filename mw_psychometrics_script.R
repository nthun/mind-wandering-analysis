options(encoding = "UTF-8")
library(tidyverse)
library(magrittr)
library(psych)
library(broom)
library(normtest)
library(scales)

############## Read data
raw_df <- read_csv2("data/MW_validation_healthy_subjects.csv")




############## Transform data
# Drop unnecessary variables and transform values to numeric
mw <- 
    raw_df %>% 
    select(mw_ = starts_with("MINDWANDERING")) %>% 
    drop_na()

############## Calculate scales
# Define scales using the psych package
mw_keys <- make.keys(mw, list(
    mind_wandering = c("mw_1", "mw_2", "mw_3", "mw_4", "mw_5")))


# Reliability statistics
scoreItems(keys = mw_keys, items = mw, impute = "none")

# Calculate ese scales for each participant
mw_scales <-
    scoreItems(keys = mw_keys, items = mw) %>%  # Extract average scores
    use_series(scores) %>% 
    as.vector() %>%
    data_frame(mw = .)

################# Check normality
# Density plot with simulated normal curve overplotted
mw_scales %>% 
    ggplot() +
    aes(mw) +
    geom_density(data = rnorm(100000, mean(mw_scales$mw), sd(mw_scales$mw)) %>% data_frame(mw = .), aes(mw), color = "red", linetype = "dotted", size = 1.2) + # Simulated normal dist
    geom_density(fill = "grey40", alpha = .7)


# Check notmality using sw and ajb tests
mw_scales %>% 
    do(sw = shapiro.test(.$mw) %>% tidy(), # Run tests per groups/scales
       ajb = ajb.norm.test(.$mw) %>% tidy()) %>% # tidy() puts output into a nice df
    unnest(sw, ajb, .sep = "_") # Unpack all results from the nested outputs


# Confirmatory factor analysis using 1 factor

library(lavaan)
library(semPlot)
mw_model <- "
mw =~ mw_1 + mw_2 + mw_3 + mw_4 + mw_5 "

mw_cfa <- cfa(data = mw, mw_model, "std")
summary(mw_cfa, fit.measures = TRUE)

semPaths(mw_cfa, whatLabels = 'std', ehat = 'est', curveAdjacent = TRUE)

# Exploratory factor analysis
factanal(mw, factors = 1)

fa(mw, nfactors = 1, rotate = "oblique")


eigen(cor(mw))
