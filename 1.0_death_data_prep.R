
# read in libraries ------------------------------------------------------------
library(tidyverse)

# read in data sources ---------------------------------------------------------

# cdc death data
death = read.delim(
  "E:/ds_projects/data/cdc/Underlying Cause of Death, 1999-2018.txt"
)

# COVID-19 deaths
covid = read.csv(
  "E:/ds_projects/data/cdc/covid_deaths_usafacts.csv",
  stringsAsFactors = F
)

# clean death data -------------------------------------------------------------

# make all column names upper case
colnames(death) = toupper(colnames(death))

# create clean integer month column
death$MONTH = as.integer(gsub(".*([0-9]{2})$", "\\1", death$MONTH.CODE))

# make STATE column the one with integers
death$STATE = death$STATE.CODE

# final processing step for death data
death = death %>%
  # remove the year totals
  filter(NOTES != "Total") %>%
  # remove the extra columns
  select(STATE, YEAR, MONTH, DEATHS)

# clean COVID death data -------------------------------------------------------

# set up some columns we want for after
covid$YEAR = 2020
covid$STATE = covid$stateFIPS
covid = covid[-c(1,2,3,4)]

# more extensive prep
covid = covid %>%
  # convert from wide to long format
  gather("DATE", "COVID", -STATE, -YEAR) %>%
  # extract the month and day from the new date column
  mutate(MONTH = as.integer(gsub("X([0-9]).*", "\\1", DATE)),
         DAY = as.integer(gsub("X[0-9]\\.([0-9][0-9]?).*", "\\1", DATE))) %>%
  # group up by state, month, and day for summarize
  group_by(STATE, YEAR, MONTH, DAY) %>%
  # add up the daily death totals for each state
  summarise(COVID = sum(COVID, na.rm = T)) %>%
  # regroup and arrange by day to get the reverse out cum sum
  group_by(STATE) %>%
  arrange(STATE, YEAR, MONTH, DAY) %>%
  # original data is cum sum - get daily differences
  mutate(COVID = COVID - lag(COVID)) %>%
  # new grouping for monthly totals
  group_by(STATE, MONTH) %>%
  # add up daily numbers within month to get monthly totals
  summarize(COVID = sum(COVID, na.rm = T))


