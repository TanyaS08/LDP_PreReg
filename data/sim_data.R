#' Analysis script for Simulated Cypress Hill Bata populations
#' before and after introduction of WNS. This includes 1) simulating
#' some data. 2) A paired t-test and 3) some figure generation


# packages needed

library(here)
library(dplyr)
library(ggplot2)
library(tvthemes)
library(tidymodels)
library(pwr)

#### Generate 'Random' data ####

# this creates reproducibility for random number generators
set.seed(16) 

# some random vales for initial abundance, 
# and sensitivity to WNS for three bat species

init_abundance = 
  tribble(
    ~species, ~init_abund, ~sensitivity,
    "Myotis lucifugus",1500, -0.9, # very
    "Eptesicus fuscus",   1000, -0.1,# not
    "Lasionycteris noctivagans", 500, 0.4 # increase
  )

# specifies standard deviation for the normal simulations (this allows for some noise)
std_dev = 200

# sampling years pre WNS

years_pre = c(2010, 2020)

# sampling years post WNS

years_post = c(2021, 2025, 2030)

### create pre WNS pop numbers

# we assume stable population numbers with some variation
# hence simulating abundance as a roemal distibution
# around the initial abundance
# values rounded to whole numbers becuase count data

pre_wns_abundance = 
  rbind(
    # Myotis
    tibble(abundance = round(rnorm(n = length(years_pre), 
                                   mean = as.numeric(init_abundance[1,2]),
                                   sd = std_dev)),
           year = years_pre,
           species = rep(pull(init_abundance[1,1]),
                         length(years_pre))),
    # Eptesicus
    tibble(abundance = round(rnorm(n = length(years_pre), 
                                   mean = as.numeric(init_abundance[2,2]),
                                   sd = std_dev)),
           year = years_pre,
           species = rep(pull(init_abundance[2,1]),
                         length(years_pre))),
    # Lasionycteris
    tibble(abundance = round(rnorm(n = length(years_pre), 
                                   mean = as.numeric(init_abundance[3,2]),
                                   sd = std_dev)),
           year = years_pre,
           species = rep(pull(init_abundance[3,1]),
                         length(years_pre)))
  )

## create post WNS pop numbers

# population numbers are simualted as expnential
# growth or decay (based on their hypothesised sensitivity)
# jitter is added once again for some random 'noise'
# values rounded to whole numbers becuase count data

post_wns_abundance = 
  rbind(
    # Myotis
    tibble(abundance = round(
      pull(pre_wns_abundance[2,1]) * exp(as.numeric(init_abundance[1,3]) * 1:length(years_post)) 
      %>% jitter(10)
    ),
    year = years_post,
    species = rep(pull(init_abundance[1,1]),
                  length(years_post))),
    # Eptesicus
    tibble(abundance = round(
      pull(pre_wns_abundance[4,1]) * exp(as.numeric(init_abundance[2,3]) * 1:length(years_post)) 
      %>% jitter(10)
    ),
    year = years_post,
    species = rep(pull(init_abundance[2,1]),
                  length(years_post))),
    # Lasionycteris
    tibble(abundance = round(
      pull(pre_wns_abundance[6,1]) * exp(as.numeric(init_abundance[3,3]) * 1:length(years_post)) 
      %>% jitter(10)
    ),
    year = years_post,
    species = rep(pull(init_abundance[3,1]),
                  length(years_post)))
  )


# Combine dataframes

abundance = 
  rbind(
    pre_wns_abundance,
    post_wns_abundance
  ) %>%
  group_by(year)

#### Analysis ####

# t-test

abundance %>%
  # add a 'pre' and 'post' WNS marker
  mutate(sampling = if_else(year < 2020,
                            "pre",
                            "post")) %>%
  t_test(formula = abundance ~ sampling,
         order = c("pre", "post"),
         alternative = "two-sided")

#### Plotting ####

# 

p = ggplot(abundance) +
  # line for WNS year
  geom_vline(aes(xintercept = 2020)) +
  geom_line(aes(x = year,
                  y = abundance,
                  colour = species)) +
  geom_point(aes(x = year,
                 y = abundance,
                 colour = species)) +
  labs(
    x = "Year",
    y = "Abundance",
  ) +
  scale_color_westeros(palette = "Tyrell",
                       name = "Species") +
  theme_bw()

p2 = 
  abundance %>%
  # add a 'pre' and 'post' WNS marker
  mutate(sampling = if_else(year < 2020,
                            "pre",
                            "post")) %>%
  ggplot() +
  geom_boxplot(aes(x = sampling,
                   y = abundance)) +
  labs(
    x = "Sampling Period",
    y = "Abundance",
  ) +
  scale_color_westeros(palette = "Tyrell",
                       name = "Species") +
  theme_bw()