## # # # # ###
##   wrangle##
## lahman_df##
## # # # # ###

library(dplyr)
library(ggplot2)
library(magrittr)
library(RSQLite)
library(Lahman) # stats

## what's in Lahman?
?Lahman # view Lahman in Help

## let's look at Batting?
data(Batting) # attach data
## what's in Lahman>Batting?
tbl_vars(Batting) # view variables

## easily pull the columns you want
Batting <- select(tbl(lahman_df(), "Batting")
                  , yearID, playerID, teamID
                  , AB, HR)

## dplyr
hr_rate <- Batting %>% # select all of Batting
    filter(HR > 0 & AB > 150) %>% # only keep records above 0 HR & 150 AB
    mutate(hr_rate = AB/HR) %>% # create rate variable
    group_by(playerID) %>% # group_by playerID
    summarise(count = n()
              , mean = mean(hr_rate)) %>% # take the mean of each player's hr_rate
    filter(count > 3) %>% # only keep records above 3 (years)
    arrange(mean) # order by mean



lahman_s <- lahman_sqlite()
batting <- tbl(lahman_s, "Batting")