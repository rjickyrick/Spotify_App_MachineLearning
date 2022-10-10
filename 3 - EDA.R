#3 EDA 

#Load packages
library(tidyverse)

#Load data
tracks_clean <- readRDS('tracks_clean.rds')

##BASIC SUMMARIES

#Number of songs per year after data cleaning
tracks_clean %>%
  group_by(playlist_year_id) %>%
  count() %>%
  ggplot() +
  aes(playlist_year_id, n) +
  geom_bar(stat = "identity", fill = "#1DB954") +
  theme_minimal() +
  ggtitle("Number of songs in each Spotify 'Top Hits of Year' playlists - 1970:2019") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) 

##Average duration of songs per year
tracks_clean %>%
  group_by(playlist_year_id) %>%
  summarise_at(vars(track.duration_ms), list(duration = mean)) %>%
  ggplot() +
  aes(playlist_year_id, duration) +
  geom_line(size = 2, colour = "#1DB954") +
  theme_minimal() +
  ggtitle("Average track duration by year in Spotify 'Top Hits of {Year}' playlists - 1970:2019") +
  geom_smooth(method = "lm", colour = "#1DB954") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

## Explore some of the Spotify Metrics
tracks_clean %>%
  group_by(playlist_year_id) %>%
  summarise_at(vars(valence), list(valence_mean = mean)) %>%
  ggplot() +
  aes(playlist_year_id, valence_mean) +
  geom_line(size = 2, colour = "#1DB954") +
  theme_minimal() +
  ggtitle("Average track 'Happiness' by year in Spotify 'Top Hits of {Year}' playlists - 1970:2019") +
  geom_smooth(method = "lm", colour = "#1DB954") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

tracks_clean %>%
  group_by(playlist_year_id) %>%
  summarise_at(vars(speechiness), list(speechiness_mean = mean)) %>%
  ggplot() +
  aes(playlist_year_id, speechiness_mean) +
  geom_line(size = 2, colour = "#1DB954") +
  theme_minimal() +
  ggtitle("Average track 'Speechiness' by year in Spotify 'Top Hits of {Year}' playlists - 1970:2019") +
  geom_smooth(method = "lm", colour = "#1DB954") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) 

##LET'S LOOK AT THESE TOGETHER 

#REMOVE THE LAST THREE COLUMNS (19:21) - THESE ARE TEXT VERSION OF COLS 17:18
spotify_metrics <- tracks_clean[, c(1:18)]
glimpse(spotify_metrics)

#DEFINE A MIN-MAX NORMALISATION
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#Normalise values
spotify_norm <- as.data.frame(lapply(spotify_metrics[6:18], min_max_norm))
spotify_norm <- cbind(spotify_metrics[, 1:5], spotify_norm)

#Pivot Longer for visualisation
spotify_metrics_tidy <- spotify_norm %>%
  pivot_longer(-c(decade, playlist_year_id, artist, track.name, track.id), names_to = "metric", values_to = "rating")

#install.packages('jcolors')
#See: https://jaredhuling.org/jcolors/
library(jcolors)

#SPOTIFY METRICS BY DECADES
spotify_metrics_tidy %>%
  filter(metric != "time_signature") %>%
  group_by(decade, metric) %>%
  summarise_at(vars(rating), list(rating_mean = mean)) %>%
  ggplot(aes(decade, rating_mean, color = metric)) +
  scale_color_jcolors(palette = "pal8") +
  geom_line(size = 2) +
  facet_wrap(~metric) +
  theme_minimal() +
  ggtitle("Spotify Metrics - Average by Decade - 1970s:2010s") +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

##SPOTIFY METRICS BY YEARS
spotify_metrics_tidy %>%
  filter(metric != "time_signature") %>%
  group_by(playlist_year_id, metric) %>%
  summarise_at(vars(rating), list(rating_mean = mean)) %>%
  ggplot(aes(playlist_year_id, rating_mean, color = metric)) +
  scale_color_jcolors(palette = "pal8") +
  geom_line(size = 2) +
  facet_wrap(~metric) +
  theme_minimal() +
  ggtitle("Spotify Metrics - Average by Year - 1970:2019") +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

##LINEAR TRENDS
spotify_metrics_tidy %>%
  filter(metric != "time_signature") %>%
  group_by(playlist_year_id, metric) %>%
  summarise_at(vars(rating), list(rating_mean = mean)) %>%
  ggplot(aes(playlist_year_id, rating_mean, color = metric)) +
  scale_color_jcolors(palette = "pal8") +
  geom_smooth(method = "lm", size = 2) +
  facet_wrap(~metric) +
  theme_minimal() +
  ggtitle("Spotify Metrics - Linear Trends - 1970:2019") +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 


##KEYS 
tracks_clean %>%
  group_by(decade, key_name) %>%
  count() %>%
  ggplot() +
  aes(decade, n, colour = key_name) %>%
  geom_line(alpha = .75, size = 2) +
  scale_color_jcolors(palette = "pal8") +
  theme_minimal() +
  ggtitle("Spotify Metrics - Song Keys by Decade - 1970s:2010s") +
  theme(legend.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

tracks_clean %>%
  group_by(decade, key_name) %>%
  count() %>%
  ggplot() +
  aes(decade, n, colour = key_name) %>%
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_jcolors(palette = "pal8") +
  theme_minimal() +
  ggtitle("Spotify Metrics - Song Keys Trends by Decade - 1970s:2010s") +
  theme(legend.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

library(stringi)
tracks_clean %>%
  mutate(sharp = ifelse(str_detect(key_name, "#"), "Black Keys", "White Keys")) %>%
  group_by(decade, key_name, sharp) %>%
  count() %>%
  ggplot() +
  aes(decade, n, colour = sharp) %>%
  geom_smooth(method = "lm", se = FALSE, alpha = .75, size = 2) +
  scale_color_jcolors(palette = "pal8") +
  theme_minimal() +
  ggtitle("Spotify Metrics - Key Type Trends - 1970:2019") +
  theme(legend.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 


##MODE 
tracks_clean %>%
  group_by(playlist_year_id, mode_name) %>%
  count() %>%
  ggplot() +
  aes(playlist_year_id, n, colour = mode_name) %>%
  geom_line(size = 2) +
  scale_color_jcolors(palette = "pal8") +
  theme_minimal() +
  ggtitle("Spotify Metrics - Manjor and Minor Key - 1970:2019") +
  theme(legend.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

##TEMPO
tracks_clean %>%
  group_by(playlist_year_id) %>%
  summarise_at(vars(tempo), list(tempo_ave = mean)) %>%
  ggplot() +
  aes(playlist_year_id, tempo_ave) %>%
  geom_line(size = 2, colour = "#1DB954") +
  theme_minimal() +
  ggtitle("Average tempo Spotify 'Top Hits of {Year}' playlists - 1970:2019") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())

##TIME SIGNATURE
unique(tracks_clean$time_signature)
tracks_clean %>%
  group_by(time_signature, decade) %>%
  count() %>%
  arrange(decade)


#WRITE IT ALL OUT
write_rds(spotify_norm, "spotify_norm.rds")
write_rds(spotify_metrics, "spotify_metrics.rds")
write_rds(tracks_clean, "tracks_clean.rds")