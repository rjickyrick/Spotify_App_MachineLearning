#5 Evaluate Model

###EVALUATE MODEL
spotify_aug %>%
  roc_curve(truth = when, .pred_new) %>% 
  autoplot()

spotify_aug %>% 
  roc_auc(truth = when, .pred_new)

##WHERE ARE THE INCORRECT ONES? 
library(jcolors)

#Predictions versus truth
spotify_aug$truth <- spotify_aug$when == spotify_aug$.pred_class

#Visualise
spotify_aug %>%
  mutate(truth = ifelse(spotify_aug$truth == TRUE, "Correct", "Wrong")) %>%
  group_by(playlist_year_id, truth) %>%
  count() %>%
  ggplot() +
  aes(playlist_year_id, n, colour = truth) +
  geom_line(size = 2) +
  scale_color_jcolors(palette = "pal8") +
  geom_line(size = 2) +
  theme_minimal() +
  ggtitle("Logistic Regression Predictions") +
  theme(legend.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

##WHAT IS THE CONFIDENCE
spotify_aug %>%
  mutate(truth = ifelse(spotify_aug$.pred_class == spotify_aug$when, "Correct", "Wrong")) %>%
  #filter(truth == "Wrong") %>%
  ggplot() +
  aes(playlist_year_id, sum(.pred_old/.pred_old), colour = truth) +
  geom_jitter()

##MOST CONFIDENT CORRECT PREDICTIONS - POST 1995
spotify_aug %>%
  filter(truth == TRUE) %>%
  arrange(desc(.pred_new)) %>%
  select(c(decade, playlist_year_id, artist, track.name, when, .pred_class, .pred_new)) %>%
  head(20)

##MOST CONFIDENT CORRECT PREDICTIONS - PRE 1995
spotify_aug %>%
  filter(truth == TRUE) %>%
  arrange(desc(.pred_old)) %>%
  select(c(decade, playlist_year_id, artist, track.name, when, .pred_class)) %>%
  head(20)

##MOST CONFIDENT INCORRECT PREDICTIONS - POST 1995
spotify_aug %>%
  filter(truth == FALSE) %>%
  mutate(from_95 = playlist_year_id - 1995) %>%
  ggplot() +
  aes(from_95, .pred_new - .pred_old, colour = factor(decade)) %>%
  geom_point()

##WRITE OUT MODEL
write_rds <- spotify_fit('old_new_model.rds')