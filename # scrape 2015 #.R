## # # # # # # # # # # ##
##              wrangle##
## 2015_mlbgameday_data##
## # # # # # # # # # # ##

library(dplyr)
library(ggplot2)
library(magrittr)
library(pitchRx)
library(lubridate)
library(RSQLite)

options(max.print = 100)

#------------------------------------------------------------------------------#
## setup in dropbox folder

setwd("C:/Users/manlike-fox/Dropbox/personal/pastime")
## using pitchRx to scrape mlbgameday data for analysis
## establish connection to the data
db <- src_sqlite("pitchFx.sqlite3", create = TRUE) # declare source db, if none
#                                                               #then create one

# # scrape the appropriate data using start & end dates
# scrape(start = "2015-04-05", end = "2015-04-05", connect = db$con) # pitchRx
#                   #function to scrape http://gd2.mlb.com/components/game/mlb/~
#                                                     # places data tables in db

# scrape the appropriate data using diff suffix
files <- c("inning/inning_all.xml", "miniscoreboard.xml"
           , "players.xml", "inning/inning_hit.xml")
scrape(start="2015-04-05", end=Sys.Date(), suffix=files, connect=db$con)

#------------------------------------------------------------------------------#
## auto update

files <- c("inning/inning_all.xml", "miniscoreboard.xml"
           , "players.xml", "inning/inning_hit.xml") # mlb tables
update_db(db$con, end = Sys.Date() - 1, suffix=files) # update

#------------------------------------------------------------------------------#
## look around the db (source)

db
# src:  sqlite 3.8.6 [pitchFx.sqlite3]
# tbls: action, atbat, coach, game, hip, media, pitch, player, po, runner, umpire

## expose a table from db
atbat <- tbl(db, "atbat")

## look around the table
atbat
# Source: sqlite 3.8.6 [pitchFx.sqlite3]
# From: atbat [1,581 x 32]
# 
# pitcher batter num b s o start_tfs       start_tfs_zulu stand b_height p_throws
# 1   452657 572761   1 2 2 1    001658 2015-04-06T00:16:58Z     L      6-3        L
# 2   452657 572761  14 0 0 2    004611 2015-04-06T00:46:11Z     L      6-3        L
# 3   452657 425877  10 1 3 1    003655 2015-04-06T00:36:55Z     R     5-11        L
# 4   452657 425794  13 0 3 2    004259 2015-04-06T00:42:59Z     R      6-7        L
# 5   452657 572761  35 3 2 0    014046 2015-04-06T01:40:46Z     L      6-3        L
# 6   452657 518792   2 1 0 1    001947 2015-04-06T00:19:47Z     L      6-5        L
# 7   452657 518792  20 0 0 0    010006 2015-04-06T01:00:06Z     L      6-5        L
# 8   452657 543939  11 4 2 1    003818 2015-04-06T00:38:18Z     L      5-9        L
# 9   452657 571431  23 2 0 3    010605 2015-04-06T01:06:05Z     L      6-3        L
# [ reached getOption("max.print") -- omitted 2 rows ]
# Variables not shown: atbat_des (chr), atbat_des_es (chr), event (chr), score (chr), home_team_runs (chr),
# away_team_runs (chr), url (chr), inning_side (chr), inning (dbl), next_ (chr), event2 (chr), event3 (chr),
# batter_name (chr), pitcher_name (chr), event4 (chr), gameday_link (chr), date (chr), event_num (chr), event_es
# (chr), play_guid (chr), event2_es (chr)

# expose a table
stats <- tbl(db, "player")

# look around the table
stats
# Source: sqlite 3.8.6 [pitchFx.sqlite3]
# From: player [1,279 x 27]
# 
# id  first       last num      boxname rl position status team_abbrev team_id parent_team_abbrev parent_team_id
# 1  279571   Matt    Belisle  37      Belisle  R        P      A         STL     138                STL            138
# 2  329092  Randy     Choate  36       Choate  L        P      A         STL     138                STL            138
# 3  407793   John     Lackey  41       Lackey  R        P      A         STL     138                STL            138
# 4  407812   Matt   Holliday   7     Holliday  R       LF      A         STL     138                STL            138
# 5  425509 Jhonny    Peralta  27      Peralta  R       SS      A         STL     138                STL            138
# 6  425794   Adam Wainwright  50   Wainwright  R        P      A         STL     138                STL            138
# 7  425877 Yadier     Molina   4    Molina, Y  R        C      A         STL     138                STL            138
# 8  445055    Jon        Jay  19          Jay  L       CF      A         STL     138                STL            138
# [ reached getOption("max.print") -- omitted 3 rows ]
# Variables not shown: avg (dbl), hr (dbl), rbi (dbl), url (chr), name_abbrev (chr), type (chr), name (chr),
# current_position (chr), bat_order (dbl), game_position (chr), wins (dbl), losses (dbl), era (dbl), gameday_link
# (chr), bats (chr)
#------------------------------------------------------------------------------#
## query the db (source)

## query your table from your db
q_atbat <- select(tbl(db, "atbat"), date, start_tfs_zulu, stand, batter_name,
                  p_throws, pitcher_name, event, inning, inning_side,
                  home_team_runs, away_team_runs, gameday_link)

## subset the query
strikeouts <- filter(q_atbat, event == "Strikeout")

# or 

## subset the query
homeruns <- filter(q_atbat, event == "Home Run")

## query your table from your db
q_playerHR <- select(tbl(db, "player"), boxname, rl, team_abbrev
                     , bat_order, position
                     , avg, hr, rbi
                     , gameday_link)

#------------------------------------------------------------------------------#
## convert to dataframe

df_atbat <- as.data.frame(q_atbat) %>%
    arrange(desc(date))


## convert subsets
Ks <- as.data.frame(strikeouts) %>%
    arrange(desc(date))

# or

HRs <- as.data.frame(homeruns) %>%
    arrange(desc(date))


## convert
df_stats <- as.data.frame(q_playerHR) %>%
    arrange(boxname, team_abbrev)

#------------------------------------------------------------------------------#
## wrangle

df_atbat %<>%
    mutate(time = ymd_hms(start_tfs_zulu)) %>%
    mutate(hour = hour(time)
           ,minute = minute(time)
           ,month = month(time)
           ,day = day(time)) %>%
    mutate(stadium = substr(gameday_link, 23L, 25L))

#------------------------------------------------------------------------------#
## filtering

HRs <- filter(df_atbat, event == "Home Run")
Ks <- filter(df_atbat, event == "Strikeout")
