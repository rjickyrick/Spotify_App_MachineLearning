#6 TRY OUT MODEL 
library(tidymodels)
library(tidyverse)
library(spotifyr)

#Read in Model 
spotify_fit <- readRDS("old_new_model.rds")
#Read in orginal data set
tracks <- readRDS('tracks.rds')
#Create ids to check searches for songs in the original data set
track_ids <- tracks$track.id

#Set Spotify API key/secret
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
#Get access
access_token <- get_spotify_access_token()

#Create predict_year function
predict_year <- function(link) {
  spotify_url_start <- "https://open.spotify.com/track/"
  if (str_detect(link, spotify_url_start) == FALSE)
  {print("The link you have entered is not a valid Spotify URL")}
  else {
    link <- str_remove(link, "https://open.spotify.com/track/")
    link <- gsub("?si.*", "", link)
    link <- substr(link,1,nchar(link)-1)
    track <- get_track(link)
    features <- get_track_audio_features(link)
    track_id <- features$id
    features$decade <- NA
    features$playlist_year_id <- NA
    features$artist <- track$artists$name[[1]]
    features$track.name <- track$name[[1]]
    features <- features[, c(19:22, 1:2, 4, 6:11, 17)]
    features$when <- NA 
    features <- features %>%
      rename(track.duration_ms = duration_ms)
    #result <- predict(spotify_fit, features)
    result <- 
      augment(spotify_fit, features)
    if (track_id %in% track_ids) {
      print(paste(features$track.name, "by", features$artist, "was part of the original set data set used to train this model. No results to show.", sep = " "))
    } else if (result$.pred_class == "old")
    {
      print(paste("The model suggests there is a", paste(round(sum(result$.pred_old * 100), digits =2), "%", sep = ""), "likelihood that", features$track.name, 
                  "by", features$artist, "is from before 1995", sep = " "))
    } else 
    {
      print(paste("The model suggests there is a", paste(round(sum(result$.pred_new * 100), digits =2), "%", sep = ""),"likelihood that", features$track.name, 
                  "by", features$artist, "is from after 1995", sep = " "))  
    }
  }
}


#Create links to test
#Song from before 1970 - Bob Dylan - Like a Rolling Stone
link1 <- "https://open.spotify.com/track/3AhXZa8sUQht0UEdBJgpGc?si=b478812d3cb1408f"
#Song from after 2019 - Olivia Rodrigo - drivers licence
link2 <- "https://open.spotify.com/track/5wANPM4fQCJwkGd4rN57mH?si=8ddca7e6855f4177"
#Song that was part of the original data set to train model - David Essex - Rock On
link3 <- "https://open.spotify.com/track/0uPIwcT6OdJ5BAJdYkxVp9?si=7841a1fb0efd4241"
#Invalid link
link4 <- "https://github.com"

#Test function
predict_year(link1)
predict_year(link2)
predict_year(link3)
predict_year(link4)

#FOR USE IN SHINY APP..

#Create Spotify embed code for searches similar to Links 1:3, null for Link 4
create_embed <- function(link) {
  spotify_url_start <- "https://open.spotify.com/track/"
  if (str_detect(link, spotify_url_start) == FALSE) {
    code <- "" 
  }
  else {
    link <- str_remove(link, "https://open.spotify.com/track/")
    link <- gsub("?si.*", "", link)
    link <- substr(link,1,nchar(link)-1)
    code <- paste0("<iframe width='100%' height='75' src='https://open.spotify.com/embed/track/", link ,"' frameborder='0' allowfullscreen></iframe>")
  }
}

#Test function
print(create_embed(link1))
print(create_embed(link2))
print(create_embed(link3))
print(create_embed(link4))