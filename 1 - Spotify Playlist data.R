#1 Get Spotify Playlist Data

## REQUIRED PACKAGES
library(spotifyr)
library(tidyverse)

#Set Spotify API key/secret
#NB: Obtain your own credentials from https://developer.spotify.com
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
#Get access
access_token <- get_spotify_access_token()

##READ IN PLAYLIST LINK DATA
playlists <- read.csv("data/Spotify_Playlist_links.csv")
playlists$user <- "spotify"

##EXTRACT PLAYLIST IDs FROM LINKS
playlists$id <- str_remove(playlists$Link, "https://open.spotify.com/playlist/")
playlists$id <- gsub("?si.*", "", playlists$id)
playlists$id <- substr(playlists$id,1,nchar(playlists$id)-1)

##GET TRACK FEATURES FOR 1st PLAYLIST
username <- playlists$user[1]
playlist_id <- playlists$id[1]
tracks <- get_playlist_audio_features(username, playlist_id)
tracks$playlist_year_id <- playlists$Year[1]


##GET TRACK FEATURES FOR THE REST
for (i in 2:nrow(playlists)) {
  username <- playlists$user[[i]]
  playlist_id <- playlists$id[[i]]
  playlist_year_id <- playlists$Year[[i]]
  print(paste("Getting playlist for ", playlist_year_id, sep = " "))
  tracks_new <- get_playlist_audio_features(username, playlist_id)
  tracks_new$playlist_year_id <- playlist_year_id
  tracks <- rbind(tracks, tracks_new)
  rm(tracks_new, playlist_year_id)
  print(paste("Waiting", 5, "seconds before getting next playist", sep = " "))
  Sys.sleep(5)
}

glimpse(tracks)

##WRITE OUT THIS DATA 
write_rds(tracks, "tracks.rds")





