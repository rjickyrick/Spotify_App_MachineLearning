#2 - CLEAN DATA
library(tidyverse)
library(dlookr)

#Read in the data gathered in Step 1 
tracks <- readRDS('tracks.rds')

#Explore the data to find any missing values
describe(tracks)

#There appears to be one rogue NA record in the data set
missing <- tracks %>%
  filter(is.na(tracks$danceability)) 
glimpse(missing)
rm(missing)

tracks <- tracks %>%
  filter(!is.na(tracks$danceability))

#Are all tracks unique? 
length(unique(tracks$track.id))

#Get artist names
for (i in 1:nrow(tracks)) {(
  tracks$artist[i] <- tracks$track.artists[[i]]$name
)}
rm(i)

#Any missing records?
table(is.na(tracks$artist))

#Which songs have appeared more than once? 
tracks %>%
  group_by(track.name, artist) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

tracks %>%
  filter(artist == "Christina Perri" & track.name == "A Thousand Years") %>%
  select(c(artist, track.name, playlist_year_id))

tracks %>%
  filter(artist == "Imagine Dragons" & track.name == "Demons") %>%
  select(c(artist, track.name, playlist_year_id))

##KEEP ONLY FIRST TRACK FOR DUPLICATES
tracks <- tracks %>%
  group_by(track.name, artist) %>%
  slice(1) %>%
  ungroup()

##CHECK TO SEE IF ANY TRACKS APPEAR MORE THAN ONCE..
tracks %>%
  group_by(track.name, artist) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

#Are all tracks unique? 
length(unique(tracks$track.id))

#CHECK THAT PREVIOUSLY DUPLICATED TRACKS ARE NOW REMOVED, KEEPING EARLIEST
tracks %>%
  filter(artist == "Christina Perri" & track.name == "A Thousand Years") %>%
  select(c(artist, track.name, playlist_year_id))
tracks %>%
  filter(artist == "Imagine Dragons" & track.name == "Demons") %>%
  select(c(artist, track.name, playlist_year_id))

#Which Artists have appeared the most
tracks %>%
  group_by(artist) %>%
  count() %>%
  filter(n > 13) %>%
  arrange(desc(n)) %>%
  ggplot() +
  aes(reorder(artist, n), n) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Most appearances in Top of YEAR lists: 1970:2019") +
  xlab("") +
  ylab("")

##REORDER AND SELECT VARIABLES
tracks_new <- tracks[, c(62, 63, 36, 17, 6:7, 9, 11:16, 31, 19, 10, 8, 59:61)]
glimpse(tracks_new)

#CHANGE YEAR TO DATE
#library(lubridate)
#tracks_new$playlist_year_id <- as.Date(as.character(tracks_new$playlist_year_id), format = "%Y")
#tracks_new$playlist_year_id <- year(tracks_new$playlist_year_id)
#glimpse(tracks_new)

#CREATE DECADE VARIABLE
tracks_new$decade <- floor(tracks_new$playlist_year_id / 10) * 10
tracks_new <- tracks_new[, c(21, 1:20)]


glimpse(tracks_new)

##Write out data
write_rds(tracks_new, 'tracks_clean.rds')