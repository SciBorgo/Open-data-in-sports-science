

#### Open data and code in sports science analysis

d = read.csv("total-articles-after-title-screen.csv")

# Select 400
Number = sort(sample(x = d$Number, size = 400, replace = F, set.seed(123)))
sample = data.frame(Number)

selected <- merge(sample, d, by="Number")
#write.csv(selected, file = "random-sample-400-data-extraction.csv")

#install.packages(c("binom","dplyr"))
library(binom)
library(dplyr)

#### Calculate proportions for all 299 eligble studies
###
d = read.csv("random-sample-400-data-extraction.csv")
d <- d %>% filter(Eligble == "1")
table(d$Topic_area) # How many articles

# Shared data
table(d$Data_shared)
successes <- 13
trials <- 299
binom.confint(x = successes, n = trials, conf.level = 0.95, methods = "exact")

# Shared code
table(d$Code_shared) # Zero shared code

# Data available upon request
table(d$Data_available_upon_request)
successes <- 5
trials <- 286
binom.confint(x = successes, n = trials, conf.level = 0.95, methods = "exact")

# Statistical software used
table(d$Software)
trials <- 298
R <- binom.confint(x = 25, n = trials, conf.level = 0.95, methods = "exact"); R
SAS <- binom.confint(x = 18, n = trials, conf.level = 0.95, methods = "exact"); SAS
SPSS <- binom.confint(x = 127, n = trials, conf.level = 0.95, methods = "exact"); SPSS
Stata <- binom.confint(x = 19, n = trials, conf.level = 0.95, methods = "exact"); Stata
Other <- binom.confint(x = 46, n = trials, conf.level = 0.95, methods = "exact"); Other
NS <- binom.confint(x = 64, n = trials, conf.level = 0.95, methods = "exact"); NS


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


