## # # # # # # # ###
##         wrangle##
## mlbgameday_data##
## # # # # # # # ###

library(dplyr)
library(ggplot2)
library(magrittr)
library(pitchRx)
library(lubridate)
library(RSQLite)

options(max.print = 100)

## using pitchRx to scrape mlbgameday data for analysis
## establish connection to the data
db <- src_sqlite("pitchFx.sqlite3", create = TRUE) # declare source db, if none
#                                                               #then create one

## scrape the appropriate data using start & end dates
# scrape(start = "2014-04-01", end = "2014-10-30", connect = db$con) # pitchRx
#                   #function to scrape http://gd2.mlb.com/components/game/mlb/~
#                                                     # places data tables in db

## scrape the appropriate data using diff suffix
# files <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")
# scrape(start="2008-01-01", end=Sys.Date(), suffix=files, connect=db$con)


## auto updates ##
# update_db(src_sqlite, end = Sys.Date() - 1)
## auto updates ##

# > db # take a look at the db
# src:  sqlite 3.8.6 [pitchFx.sqlite3]
# tbls: action, action_export, atbat, atbat_export, pitch, pitch_export, po
#                                                           #, po_export, runner

## expose a table from db
atbat <- tbl(db, "atbat")

# > atbat # take a look at atbat
# Source: sqlite 3.8.6 [pitchFx.sqlite3]
# From: atbat [169,369 x 28] # not a df; just what can be queried
#
#    pitcher batter num b s o start_tfs       start_tfs_zulu stand b_height p_throws
# 1   425844 543281   1 1 0 1    000805 2014-06-02T00:08:05Z     R      5-8        R
# 2   425844 543281  14 1 0 3    004218 2014-06-02T00:42:18Z     R      5-8        R
# 3   425844 543281  37 2 1 2    014202 2014-06-02T01:42:02Z     R      5-8        R
# 4   425844 431145  23 2 3 3    010426 2014-06-02T01:04:26Z     R     5-10        R
# 5   425844 435522  38 3 2 3    014358 2014-06-02T01:43:58Z     L      6-3        R
# 6   425844 435522   2 0 1 2    000900 2014-06-02T00:09:00Z     L      6-3        R
# 7   425844 435522  20 0 3 1    005906 2014-06-02T00:59:06Z     L      6-3        R
# 8   425844 425549  31 2 1 3    012836 2014-06-02T01:28:36Z     R      6-1        R
# 9   425844 457705  21 0 0 1    010119 2014-06-02T01:01:19Z     R     5-10        R
#  [ reached getOption("max.print") -- omitted 2 rows ]
# Variables not shown: atbat_des (chr), atbat_des_es (chr), event (chr), score (chr), home_team_runs (chr), away_team_runs (chr), url (chr), inning_side
#   (chr), inning (dbl), next_ (chr), event2 (chr), event3 (chr), batter_name (chr), pitcher_name (chr), event4 (chr), gameday_link (chr), date (chr)

## query your table from your db
d_atbat <- select(tbl(db, "atbat"), date, start_tfs_zulu, stand, batter_name,
                    p_throws, pitcher_name, event, inning, inning_side,
                    home_team_runs, away_team_runs, gameday_link)

## subset the query
home_runs <- filter(d_atbat, event == "Home Run")

## convert to data frame
home_runs <- as.data.frame(home_runs) %>%
    arrange(date)

## wrangle date/time

## clean start_tfs_zulu w/ lubridate
home_runs$start_tfs_zulu <- ymd_hms(home_runs$start_tfs_zulu) # strips each part
#                                            # and puts back together in POSIXct
home_runs <- rename(home_runs, time = start_tfs_zulu) # rename, new = old

home_runs %<>% mutate(hour = hour(home_runs$time)
                      ,minute = minute(home_runs$time)
                      ,month = month(home_runs$time)
                      ,day = day(home_runs$time))

## wrangle stadiums
## substring the gameday_link
home_runs %<>% mutate(stadium = substr(gameday_link, 23L, 25L)) # start, end

################################################################################
## stadium thoughts
## plots

sta_HR <- home_runs %>%
    group_by(stadium, inning, inning_side, month, day) %>%
    summarise(count = n()
              , startinn = min(time)
              , endinn = max(time)) %>%
    ungroup() %>%
    arrange(startinn, stadium)

avg_per_day_per_stadium <- sta_HR %>%
    group_by(stadium, month, day) %>%
    summarise(count = n()) %>%
    group_by(stadium) %>%
    summarise(mean = mean(count)) %>%
    arrange(desc(mean))

avg_home_pdps <- sta_HR %>%
    filter(inning_side == "bottom") %>%
    group_by(stadium, month, day) %>%
    summarise(count = n()) %>%
    group_by(stadium) %>%
    summarise(hr = n()
        , mean = mean(count)) %>%
    arrange(desc(mean))

avg_away_pdps <- sta_HR %>%
    filter(inning_side == "top") %>%
    group_by(stadium, month, day) %>%
    summarise(count = n()) %>%
    group_by(stadium) %>%
    summarise(hr = n()
        , mean = mean(count)) %>%
    arrange(desc(mean))

homesta_HR <- sta_HR %>%
    filter(inning_side == "bottom")

yy <- ggplot(sta_HR, aes(x = meanHRbyHR, y = count))
yy + geom_point(aes(color = stadium, size = count))




################################################################################
## time analysis
## plots
time_HR <- home_runs %>%
    group_by(hour, inning, inning_side, gameday_link) %>%
    summarise(HR = n())

hh <- ggplot(time_HR, aes(x = hour, y = HR))
hh + geom_bar(stat = "identity", aes(fill = inning))

#or

mtime_HR <- home_runs %>%
    group_by(hour, inning, inning_side, gameday_link) %>%
    summarise(HR = n()) %>%
    group_by(hour) %>%
    summarise(count = n()
              , meanHR = mean(HR))

mhh <- ggplot(mtime_HR, aes(x = hour, y = meanHR))
mhh <- geom_bar(stat = "identity")


hf <- qplot(x = hour, y = meanHR, data = mtime_HR)
hf + geom_bar(stat = "identity")
################################################################################
## inning analysis
## plots
inng_HR <- home_runs %>%
    group_by(inning, inning_side, gameday_link) %>%
    summarise(HR = n())

gg <- ggplot(inng_HR, aes(x = inning, y = HR))
gg + geom_bar(stat = "identity", aes(fill = inning_side))

# or

cc <- ggplot(inng_HR, aes(x = gameday_link, y = HR))
cc + geom_bar(stat = "identity", aes(fill = inning_side))

################################################################################


