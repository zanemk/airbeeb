rm(list = ls()))
gc()

# setup -------------------------------------------------------------------

library(dplyr)

set.seed(98136)

setwd("C:\\Users\\Zane Kelly\\Data\\")

#Nb. the raw data uses NULL to indicate missing values (these might be interesting themselves at some point)
dat <- read.delim("airbnb_session_data.txt", sep = "|", stringsAsFactors = F, na.strings = "NULL")


# basic desc --------------------------------------------------------------
table(table(unique(dat$id_visitor)))
dat <- dat %>% arrange(id_visitor, dim_session_number)
dat %>% group_by(id_visitor) %>% tally()
# 630 visitors made 7756 visits to the site
# who's the biggest recidivist?
glimpse(dat[dat$dim_session_number == max(dat$dim_session_number),])
# 702, wow

