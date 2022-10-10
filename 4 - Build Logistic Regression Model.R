##DATA
spotify_norm <- readRDS("spotify_norm.rds")
spotify_metrics <- readRDS("spotify_metrics.rds")
tracks_clean <- readRDS("tracks_clean.rds")

glimpse(spotify_metrics)

##NEED TO MAKE THE PREDICTOR VARIABLE A FACTOR
#Pre 1995 or Post 1995
spotify_metrics$when <- as.factor(ifelse(spotify_metrics$playlist_year_id >= 1995, "new", "old"))
spotify_metrics %>%
  ggplot() +
  aes(playlist_year_id, valence) +
  geom_point(aes(colour = when)) +
  theme_minimal()

glimpse(spotify_metrics)

#SPLIT DATA INTO TRAIN AND TEST 
set.seed(222)
# Put 3/4 of the data into the training set, based on when 
data_split <- initial_split(spotify_metrics, prop = 3/4, strata = when)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

train_data %>%
  group_by(decade) %>%
  count()

test_data %>%
  group_by(decade) %>%
  count()

## CREATE RECIPE AND ROLES 
spotify_rec <- 
  recipe(when ~ ., data = train_data) %>%  #2 args: 1) formula, 2) data
  update_role(decade, playlist_year_id, artist, track.name, track.id, new_role = "ID") #give these variables role as ID, not predict

#Show new roles: variables are either outcome, predictor, or ID
summary(spotify_rec)

#MODEL
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

#WORKFLOW
spotify_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(spotify_rec)

#TRAIN MODEL
spotify_fit <- 
  spotify_wflow %>% 
  fit(data = train_data)

#EXTRACT ELEMENTS OF THE FITTED MODEL 
#EG Model Coefficients
spotify_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

## USE A TRAINED WORKFLOW TO PREDICT
predict(spotify_fit, test_data)

## PREDICT WITH AUGMENT GIVES PROBABILITIES
spotify_aug <- 
  augment(spotify_fit, test_data)
glimpse(spotify_aug)

##WRITE OUT MODEL
write_rds <- spotify_fit('old_new_model.rds')
