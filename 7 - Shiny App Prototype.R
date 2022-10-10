#7 - Create (Basic) Shiny App

#LOAD PACKAGES
library(shiny)
library(tidymodels)
library(tidyverse)
library(spotifyr)
library(shinyWidgets)
library(stringi)

#Read in Model 
spotify_fit <- readRDS("data/old_new_model.rds")

#Read in track IDs
tracks <- readRDS('track_ids.rds')
track_ids <- tracks$track.id

#Set Spotify API key/secret
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
#Get access
access_token <- get_spotify_access_token()


#Create Predict Year Function
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

#### UI SIDE #####
ui <- fluidPage(
  
  ##CREATE INPUTS
  fluidRow(
    column(12,
           textInput("text", "Enter Spotify Song Link:", value = "", width = "100%"),
           actionButton("runScript", "Run Prediction"),
    )),
  ##CREATE OUTPUTS
  h3(textOutput("result"))
  
  
)

### SERVER SIDE ###
server <- function(input, output, session) {
  
  ##Store Spotify link as reactive element  
  link <- reactive({
    input$text
  })
  
  ##Wait until 'Run Script' button is clicked before executing predict_year 
  observeEvent(input$runScript, { # "runScript" is an action button
    link <- link()
    result <- predict_year(link)
    song_link <- create_embed(link)
    output$result <- renderText(result)
  })
  
}

## Run App ##
shinyApp(ui, server)
