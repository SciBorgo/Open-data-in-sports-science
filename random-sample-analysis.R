#### Open data and code in sports science analysis
# Author: David N Borg
# Contributor: Joshua J Bon
# Data collection and processing: David N Borg, Brenton J Baguley
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

## Calculate proportions and CIs from articles:

#install.packages(c("binom","dplyr"))
library(binom)
library(dplyr)



#### Calculate proportions for all 299 eligble studies
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
# !!comment to explain NA
trials <- sum(!is.na(d$Software)) # n = 298
R <- binom.confint(x = sum(d$Software == "R", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); R
SAS <- binom.confint(x = sum(d$Software == "SAS", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = sum(d$Software == "SPSS", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = sum(d$Software == "Stata", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = sum(d$Software == "Other", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = sum(d$Software == "Not_stated", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); NS



#### Proportions for 'sport and exercise science' topic area
d = read.csv("random-sample-400-data-extraction.csv")
d <- d %>% filter(Eligble == "1")
d <- d %>% filter(d$Topic_area == "Sport/Exercise")

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
trials <- sum(!is.na(d$Software))
R <- binom.confint(x = sum(d$Software == "R", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); R
SAS <- binom.confint(x = sum(d$Software == "SAS", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = sum(d$Software == "SPSS", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = sum(d$Software == "Stata", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = sum(d$Software == "Other", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = sum(d$Software == "Not_stated", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); NS



#### Proportions for 'sports medicine' topic area
d = read.csv("random-sample-400-data-extraction.csv")
d <- d %>% filter(Eligble == "1")
d <- d %>% filter(d$Topic_area == "Medicine")

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
trials <- sum(!is.na(d$Software))
R <- binom.confint(x = sum(d$Software == "R", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); R
SAS <- binom.confint(x = sum(d$Software == "SAS", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = sum(d$Software == "SPSS", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = sum(d$Software == "Stata", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = sum(d$Software == "Other", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = sum(d$Software == "Not_stated", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); NS



#### Proportions for 'rehabilitation' topic area
d = read.csv("random-sample-400-data-extraction.csv")
d <- d %>% filter(Eligble == "1")
d <- d %>% filter(d$Topic_area == "Rehab")

# Shared data
table(d$Data_shared, useNA = 'ifany')
# Zero shared data

# Shared code
table(d$Code_shared, useNA = 'ifany')
# Zero shared code

# Data available upon request
table(d$Data_available_upon_request, useNA = 'ifany')
# Zero included a data availability statement

# Statistical software used
table(d$Software, useNA = 'ifany')
trials <- sum(!is.na(d$Software))
# Zero used R ## R <- binom.confint(x = sum(d$Software == "R", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); R
SAS <- binom.confint(x = sum(d$Software == "SAS", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = sum(d$Software == "SPSS", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = sum(d$Software == "Stata", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = sum(d$Software == "Other", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = sum(d$Software == "Not_stated", na.rm = T), n = trials, conf.level = 0.95, methods = "exact"); NS



