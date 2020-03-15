#### Open data and code in sports science analysis
# Author: David Borg
# Contributor: Joshua J Bon
# Data collection and processing: David Borg, Brenton J Baguley
#

## Simple random sample to choose articles to survey.

d = read.csv("total-articles-after-title-screen.csv")

## Choose random sample
# Select 400
Number = sort(sample(x = d$Number, size = 400, replace = F, set.seed(123)))
sample = data.frame(Number)

selected <- merge(sample, d, by="Number")

# procedure to sample, complete once.
#write.csv(selected, file = "random-sample-400-data-extraction.csv")

## Calculate proportions and CIS from articles:

#install.packages(c("binom","dplyr"))
library(binom)
library(dplyr)

#### Calculate proportions for all 299 eligble studies
###
d = read.csv("random-sample-400-data-extraction.csv")
d <- d %>% filter(Eligble == "1")
table(d$Topic_area) # How many articles

# Shared data
table(d$Data_shared, useNA = 'ifany')
successes <- sum(d$Data_shared == 1, na.rm = T)
trials <- sum(!is.na(d$Data_shared))
binom.confint(x = successes, n = trials, conf.level = 0.95, methods = "exact")

# Shared code
table(d$Code_shared, useNA = 'ifany')
# !!comment to explain NAs
# Zero shared code

# Data available upon request
table(d$Data_available_upon_request, useNA = 'ifany')
# !!comment to explain NAs
successes <- sum(d$Data_available_upon_request == 1, na.rm = T)
trials <- sum(!is.na(d$Data_available_upon_request))
binom.confint(x = successes, n = trials, conf.level = 0.95, methods = "exact")

# Statistical software used
table(d$Software, useNA = 'ifany')
trials <- sum(!is.na(d$Software)) # this was 298, but now 299?
R <- binom.confint(x = sum(d$Software == "R"), n = trials, conf.level = 0.95, methods = "exact"); R
SAS <- binom.confint(x = sum(d$Software == "SAS"), n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = sum(d$Software == "SPSS"), n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = sum(d$Software == "Stata"), n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = sum(d$Software == "Other"), n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = sum(d$Software == "Not_stated"), n = trials, conf.level = 0.95, methods = "exact"); NS

# Continue editing here:

#### Proportions for 'sport and exercise science' topic area
###
d = read.csv("random-sample-400-data-extraction.csv")
d <- d %>% filter(Eligble == "1")
d <- d %>% filter(d$Topic_area == "Sport/Exercise")
table(d$Topic_area) # How many articles

# Shared data
table(d$Data_shared)
successes <- 8
trials <- 148
binom.confint(x = successes, n = trials, conf.level = 0.95, methods = "exact")

# Shared code
table(d$Code_shared) # Zero shared code

# Data available upon request
table(d$Data_available_upon_request)
successes <- 2
trials <- 140
binom.confint(x = successes, n = trials, conf.level = 0.95, methods = "exact")

# Statistical software used
table(d$Software)
trials <- 148
R <- binom.confint(x = 10, n = trials, conf.level = 0.95, methods = "exact"); R
SAS <- binom.confint(x = 4, n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = 72, n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = 6, n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = 29, n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = 27, n = trials, conf.level = 0.95, methods = "exact"); NS


#### Proportions for 'sports medicine' topic area
###
d = read.csv("random-sample-400-data-extraction.csv")
d <- d %>% filter(Eligble == "1")
d <- d %>% filter(d$Topic_area == "Medicine")
table(d$Topic_area) # How many articles

# Shared data
table(d$Data_shared)
successes <- 5
trials <- 137
binom.confint(x = successes, n = trials, conf.level = 0.95, methods = "exact")

# Shared code
table(d$Code_shared) # Zero shared code

# Data available upon request
table(d$Data_available_upon_request)
successes <- 3
trials <- 132
binom.confint(x = successes, n = trials, conf.level = 0.95, methods = "exact")

# Statistical software used
table(d$Software)
trials <- 136
R <- binom.confint(x = 15, n = trials, conf.level = 0.95, methods = "exact"); R
SAS <- binom.confint(x = 13, n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = 52, n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = 7, n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = 15, n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = 35, n = trials, conf.level = 0.95, methods = "exact"); NS


#### Proportions for 'rehabilitation' topic area
###
d = read.csv("random-sample-400-data-extraction.csv")
d <- d %>% filter(Eligble == "1")
d <- d %>% filter(d$Topic_area == "Rehab")
table(d$Topic_area) # How many articles

# Shared data, code and no data availability statement
table(d$Data_shared) # Zero shared data and code and no data availability 

# Statistical software used
table(d$Software)
trials <- 14
# Zero for 'R'
SAS <- binom.confint(x = 1, n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = 3, n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = 6, n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = 2, n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = 2, n = trials, conf.level = 0.95, methods = "exact"); NS


