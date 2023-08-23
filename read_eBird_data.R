library(lubridate)
library(tidyverse)

rawpath = "ebd_IN-CT_relJun-2023.txt"

preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","EXOTIC.CODE",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

data = data %>%
  # remove unapproved records and records of escapees
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  filter(!EXOTIC.CODE %in% c("X"))

imp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
        "LOCALITY.ID","LOCALITY.TYPE","COUNTY",
        "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED",
        "OBSERVER.ID","PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","group.id","SAMPLING.EVENT.IDENTIFIER")

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

data = data %>%
  # create a column "group.id" which can help remove duplicate checklists
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  dplyr::select(all_of(imp)) %>%
  # other useful columns
  # set date, add month, year and day columns using package LUBRIDATE
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month], 
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  # add number of species/list length column (no.sp), for list length analyses (lla)
  group_by(group.id) %>% 
  mutate(no.sp = n_distinct(COMMON.NAME)) %>%
  ungroup() %>%
  rename(DISTRICT = COUNTY)

save(data, file = "rawdata_CT.RData")
