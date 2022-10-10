# Spotify, Machine Learning and Shiny

**A workflow that gathers data from the Spotify API, creates a simple Logistic Regression model, and then produces a Shiny app enabling users to enter songs and see the results of predictions.**

![](images/spot_model.png)

A Machine Learning model trained on ~5,000 hit songs recorded between 1970 and 2019. The aim was to classify songs as being from either before or after the mid-point year of 1995. The model uses metrics available from Spotify's API - including tempo, duration, 'valence', 'acousticness', etc. - to make predictions. You can see a live, working version of the app this workflow creates [here](https://craigfots.shinyapps.io/Spotify_LogReg_1995/). 

This repo contains all the code required to recreate the app, from gathering Spotify data to development of the Shiny app. However, before proceeding you will first need to obtain your own credentials for the [Spotify API](https://developer.spotify.com). 

The contents of the repo are as follows:

**1 - Get Spotify Playlist Data**

Retrieves data on ~5,000 songs from the Spotify API. These songs are taken from Spotify's in-house 'Top Hits of {YEAR}' playlists for the years 1970 to 2019.

**2 - Clean Data**

Performs some basic tidying and cleaning of the data set generated in step 1.

**3 - EDA**

A number of exploratory visualsations, looking for the appearance of any linear trends in Spotify metrics over time.

**4 - Build Logistic Regression Model**

Using the TidyModels package, a basic Logstics Regression model is trained on a subset of the data gathered above.

**6 - Evaluate Model**

Explore ROC and other metrics to see how well (or otherwise!) the model performs.

**6 - Create Predictions**

Generate predictions on live data using the model produced in step 4.

**7 - Shiny App Prototype**

Build a local version of a Shiny app that enables users to use the model via an interface.

**8 - Shiny App**

A folder containing all the elements required to style the app, split out functions into separate scripts, and add descriptive text.

