
library(here)
library(dplyr)
library(ggplot2)
library(tibble)

# this creates reproducibility for random number generators
set.seed(16) 

# species initial abundance 

init_abundance = 
  tribble(
    ~species, ~init_abund,
    "Myotis lucifugus",   2000,
    "sp_2",   1000,
    "Lasionycteris noctivagans",   500,
    "sp_4",   700
  )

std_dev = 200

# years pre WNS

years_pre = c(1990, 2000, 2010, 2020)

# years post WNS

years_post = c(2021, 2025, 2030)

# create pre WNS pop numbers

pre_wns_abundance = 
  rbind(
    # sp1
    tibble(abundance = round(rnorm(n = length(years_pre), 
                                   mean = as.numeric(init_abundance[1,2]),
                                   sd = std_dev)),
           year = years_pre,
           species = rep(pull(init_abundance[1,1]),
                         length(years_pre))),
    # sp2
    tibble(abundance = round(rnorm(n = length(years_pre), 
                                   mean = as.numeric(init_abundance[2,2]),
                                   sd = std_dev)),
           year = years_pre,
           species = rep(pull(init_abundance[2,1]),
                         length(years_pre))),
    # sp3
    tibble(abundance = round(rnorm(n = length(years_pre), 
                                   mean = as.numeric(init_abundance[3,2]),
                                   sd = std_dev)),
           year = years_pre,
           species = rep(pull(init_abundance[3,1]),
                         length(years_pre))),
    # sp4
    tibble(abundance = round(rnorm(n = length(years_pre), 
                                   mean = as.numeric(init_abundance[4,2]),
                                   sd = std_dev)),
           year = years_pre,
           species = rep(pull(init_abundance[4,1]),
                         length(years_pre)))
  )

# WNS sensitivity

wns_sensitivity = 
  tribble(
    ~species, ~init_abund,
    "Myotis lucifugus",   -0.9, # very
    "sp_2",   -0.1,# not
    "Lasionycteris noctivagans",   0.4, # increase
    "sp_4",   0 #unaffected
  )

# create post WNS pop numbers

post_wns_abundance = 
  rbind(
    # sp1
    tibble(abundance = round(
      pull(pre_wns_abundance[4,1]) * exp(as.numeric(wns_sensitivity[1,2]) * 1:length(years_post)) 
      %>% jitter(10)
    ),
    year = years_post,
    species = rep(pull(init_abundance[1,1]),
                  length(years_post))),
    # sp2
    tibble(abundance = round(
      pull(pre_wns_abundance[8,1]) * exp(as.numeric(wns_sensitivity[2,2]) * 1:length(years_post)) 
      %>% jitter(10)
    ),
    year = years_post,
    species = rep(pull(init_abundance[2,1]),
                  length(years_post))),
    # sp3
    tibble(abundance = round(
      pull(pre_wns_abundance[12,1]) * exp(as.numeric(wns_sensitivity[3,2]) * 1:length(years_post)) 
      %>% jitter(10)
    ),
    year = years_post,
    species = rep(pull(init_abundance[3,1]),
                  length(years_post))),
    # sp4
    tibble(abundance = round(
      rnorm(n = length(years_post), 
            mean = as.numeric(pre_wns_abundance[16,1]),
            sd = std_dev)
    ),
    year = years_post,
    species = rep(pull(init_abundance[4,1]),
                  length(years_post))
    )
  )


# Combine dataframes

abundance = 
  rbind(
    pre_wns_abundance,
    post_wns_abundance
  )

p = ggplot(abundance) +
  geom_line(aes(x = year,
                  y = abundance,
                  colour = species)) +
  geom_point(aes(x = year,
                 y = abundance,
                 colour = species)) +
  geom_vline(aes(xintercept = 2020)) +
  labs(
    x = "Species",
    y = "Abundance"
  ) +
  theme_bw()
