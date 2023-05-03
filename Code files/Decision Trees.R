## API-222 Final Project: Tree-based methods
## Code by Pranav Bhargava
## Code is based on past notes by TFs Laura Morris, Emily Mower, and Amy Wickett

################################################################################ 
##------------------------------ Decision Trees --------------------------------
#install.packages("tree")
library(ISLR)
library(tree)
library(readr)

setwd("/Users/pranavbhargava/Library/CloudStorage/OneDrive-HarvardUniversity/API 222 Final Project/Data")
spotify_data <- read_csv("Spotify_Youtube.csv")
spotify_data <- na.omit(spotify_data) #removing observations with NA values

#Creating our variable of interest -streams on spotify per Youtube View
spotify_data <- spotify_data%>%
  mutate(streams_per_views = Stream/Views)%>%
  mutate(views_per_stream = Views/Stream)%>%
  mutate(likes_per_views = Likes/Views)

## Let's again split the data into training and test sets
set.seed(222)  
train <- sample(seq(nrow(spotify_data)),
                round(nrow(spotify_data) * 0.8))
train <- sort(train)
test <- which(!(seq(nrow(spotify_data)) %in% train))


#STREAMS PER VIEWS
## We can now train a decision tree using the function tree()
?tree
spotify_tree = tree(streams_per_views ~ Danceability + Energy + Key + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, spotify_data,subset = train)

## Plot the results
plot(spotify_tree)
text(spotify_tree)

## Calculate the MSE for the Predicted Values
spotify_preds_test <- predict(spotify_tree, newdata = spotify_data[test,])
spotify_preds_train <- predict(spotify_tree, newdata = spotify_data[train,]) 


## Create a helper function to calculate MSEP
msep_func <- function(predictions, true_vals) {
  MSEP <- mean((predictions - true_vals)^2)
  return(MSEP)
}

##Evaluate training model performance
print(msep_func(predictions = spotify_preds_train,
                true_vals = spotify_data[train,]$streams_per_views))

## Evaluate model performance
print(msep_func(predictions = spotify_preds_test, 
                true_vals = spotify_data[test,]$streams_per_views))


#VIEWS/STREAMS
## We can now train a decision tree using the function tree()
?tree
spotify_tree = tree(views_per_stream ~ Danceability + Energy + Key + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, spotify_data,subset = train)

## Plot the results
#plot(spotify_tree)
#text(spotify_tree)

## Calculate the MSE for the Predicted Values
spotify_preds_test <- predict(spotify_tree, newdata = spotify_data[test,])
spotify_preds_train <- predict(spotify_tree, newdata = spotify_data[train,]) 


## Create a helper function to calculate MSEP
msep_func <- function(predictions, true_vals) {
  MSEP <- mean((predictions - true_vals)^2)
  return(MSEP)
}

##Evaluate training model performance
print(msep_func(predictions = spotify_preds_train,
                true_vals = spotify_data[train,]$views_per_stream))

## Evaluate model performance
print(msep_func(predictions = spotify_preds_test, 
                true_vals = spotify_data[test,]$views_per_stream))


#LIKES/VIEWS
## We can now train a decision tree using the function tree()
?tree
spotify_tree = tree(likes_per_views ~ Danceability + Energy + Key + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, spotify_data,subset = train)

## Plot the results
#plot(spotify_tree)
#text(spotify_tree)

## Calculate the MSE for the Predicted Values
spotify_preds_test <- predict(spotify_tree, newdata = spotify_data[test,])
spotify_preds_train <- predict(spotify_tree, newdata = spotify_data[train,]) 


## Create a helper function to calculate MSEP
msep_func <- function(predictions, true_vals) {
  MSEP <- mean((predictions - true_vals)^2)
  return(MSEP)
}

##Evaluate training model performance
print(msep_func(predictions = spotify_preds_train,
                true_vals = spotify_data[train,]$likes_per_views))

## Evaluate model performance
print(msep_func(predictions = spotify_preds_test, 
                true_vals = spotify_data[test,]$likes_per_views))



