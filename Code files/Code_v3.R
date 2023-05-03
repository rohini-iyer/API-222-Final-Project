setwd("~/OneDrive - Harvard University/Semester 4/API 222/Final project")

# Load all libraries
library(readr)
library(ggplot2)
library(MASS)
library(randomForest)
library(gbm)
library(FNN)

# Load the data
music <- read_csv("~/OneDrive - Harvard University/Semester 4/API 222/Final project/Spotify_Youtube.csv")
View(music)

# Counting rows that have missing data
nrow(music[!complete.cases(music),])
music <- na.omit(music)

# Creating new outcome variables
music$streams_per_view <- music$Stream / music$Views
music$views_per_stream <- music$Views / music$Stream
music$likes_per_view <- music$Likes / music$Views

# Dividing data into test and training datasets
set.seed(222)

train <- sample(seq(nrow(music)),
                round(nrow(music) * 0.8))
train <- sort(train)
# train_data <- music[train,]

test <- which(!(seq(nrow(music)) %in% train))
# test_data <- music[test,]

####### Views per stream #######
## Random forests
set.seed(222)

rf.music <- randomForest(views_per_stream ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                         importance = TRUE, n.trees = 5000)

# Predictions
yhat.rf <- predict(rf.music, newdata = music[-train ,])
yhat.rf.t <- predict(rf.music, newdata = music[-test ,])
music.train = music[train, ]
music.test = music[test, "views_per_stream"]
# as.data.frame(as.numeric

## Mean squared error of prediction
mean((yhat.rf.t - music.train$views_per_stream)^2)
mean((yhat.rf - music.test$views_per_stream)^2)

## Bagging model
set.seed(222)

bag.music <- randomForest(views_per_stream ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                          mtry = 10, importance = TRUE)
bag.music$importance

# Now let's make some predictions 
yhat.bag <- predict(bag.music, newdata = music[-train,])
yhat.bag.t <- predict(bag.music, newdata = music[-test,])

mean((yhat.bag.t - music.train$views_per_stream)^2)
mean((yhat.bag - music.test$views_per_stream)^2)

## Boosting model
set.seed(222)

boost.music <- gbm(views_per_stream ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                   distribution = "gaussian", n.trees = 5000, 
                   interaction.depth = 4)
summary(boost.music)  

# Now let's make some predictions 
yhat.boost <- predict(boost.music, newdata = music[-train ,], 
                      n.trees = 5000)
yhat.boost.t <- predict(boost.music, newdata = music[-test ,], 
                        n.trees = 5000)

mean((yhat.boost.t - music.train$views_per_stream)^2)
mean((yhat.boost - music.test$views_per_stream)^2)

## Linear regression
set.seed(222)

linear <- lm(views_per_stream ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]))
summary(linear)

yhat.linear <- predict(linear, newdata = music[-train ,])
yhat.linear.t <- predict(linear, newdata = music[-test ,])

music.test = music[test, "views_per_stream"]

mean((yhat.linear.t - music.train$views_per_stream)^2)
mean((yhat.linear - music.test$views_per_stream)^2)

## KNN regression
set.seed(222)

str(music)
non_numeric_cols <- sapply(music, function(x) !is.numeric(x))

# Finding best K for predicting views
# Define the range of K values to test
k_guesses <- 1:100
# Initialize a tracker for the MSE values for each K
mse_res <- NULL

# Now loop through all the values
music.train = music[train,]
music.test = music[test,]

music.train <- music.train[, !non_numeric_cols]
music.test <- music.test[, !non_numeric_cols]

music.train <- music.train[, -c(1, 12, 13, 14, 15, 16, 17, 19)]
music.test <- music.test[, -c(1, 12, 13, 14, 15, 16, 17, 19)]

for(i in 1:length(k_guesses)){
  # For each value, run the model using the current K guess
  knn_reg_music <- knn.reg(music.train[, -c(11)],
                           music.test[, -c(11)],
                           music.train$views_per_stream,
                           k = k_guesses[i]) # key line here
  # The MSE
  mse_knn <- mean((knn_reg_music$pred - music.test$views_per_stream)^2)
  # Now update the tracker
  mse_res[i] <- mse_knn
}
# Now plot the results
plot(x = k_guesses, y = mse_res, main = "MSE vs. K for views per stream", xlab = "K", ylab = "MSE")
which.min(mse_res)

# Use the k shortlisted above in the KNN regression model
knn_reg_music <- knn.reg(music.train[, -c(11)],
                         music.test[, -c(11)],
                         music.train$views_per_stream,
                         k = 6)
# The MSE
music.test = music[test, "views_per_stream"]
mean((knn_reg_music$pred - music.train$views_per_stream)^2)
mean((knn_reg_music$pred - music.test$views_per_stream)^2)

####### Streams per view #######
## Random forests
set.seed(222)

rf.music <- randomForest(streams_per_view ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                         importance = TRUE, n.trees = 5000)

# Predictions
yhat.rf <- predict(rf.music, newdata = music[-train ,])
yhat.rf.t <- predict(rf.music, newdata = music[-test ,])
music.test = music[test, "streams_per_view"]
# as.data.frame(as.numeric(

## Mean squared error of prediction
mean((yhat.rf.t - music.train$streams_per_view)^2)
mean((yhat.rf - music.test$streams_per_view)^2)

## Bagging model
set.seed(222)

bag.music <- randomForest(streams_per_view ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                          mtry = 10, importance = TRUE)
bag.music$importance

# Now let's make some predictions 
yhat.bag <- predict(bag.music, newdata = music[-train,])
yhat.bag.t <- predict(bag.music, newdata = music[-test,])

mean((yhat.bag.t - music.train$streams_per_view)^2)
mean((yhat.bag - music.test$streams_per_view)^2)

## Boosting model
set.seed(222)

boost.music <- gbm(streams_per_view ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                   distribution = "gaussian", n.trees = 5000, 
                   interaction.depth = 4)
summary(boost.music)  

# Now let's make some predictions 
yhat.boost <- predict(boost.music, newdata = music[-train ,], 
                      n.trees = 5000)
yhat.boost.t <- predict(boost.music, newdata = music[-test ,], 
                        n.trees = 5000)

mean((yhat.boost.t - music.train$streams_per_view)^2)
mean((yhat.boost - music.test$streams_per_view)^2)

## Linear regression
set.seed(222)

linear <- lm(streams_per_view ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]))
summary(linear)

yhat.linear <- predict(linear, newdata = music[-train ,])
yhat.linear.t <- predict(linear, newdata = music[-test ,])

music.test = music[test, "streams_per_view"]
mean((yhat.linear.t - music.train$streams_per_view)^2)
mean((yhat.linear - music.test$streams_per_view)^2)

## KNN regression
set.seed(222)

str(music)
non_numeric_cols <- sapply(music, function(x) !is.numeric(x))

# Finding best K for predicting views
# Define the range of K values to test
k_guesses <- 1:100
# Initialize a tracker for the MSE values for each K
mse_res <- NULL

# Now loop through all the values
music.train = music[train,]
music.test = music[test,]

music.train <- music.train[, !non_numeric_cols]
music.test <- music.test[, !non_numeric_cols]

music.train <- music.train[, -c(1, 12, 13, 14, 15, 16, 18, 19)]
music.test <- music.test[, -c(1, 12, 13, 14, 15, 16, 18, 19)]

for(i in 1:length(k_guesses)){
  # For each value, run the model using the current K guess
  knn_reg_music <- knn.reg(music.train[, -c(11)],
                           music.test[, -c(11)],
                           music.train$streams_per_view,
                           k = k_guesses[i]) # key line here
  # The MSE
  mse_knn <- mean((knn_reg_music$pred - music.test$streams_per_view)^2)
  # Now update the tracker
  mse_res[i] <- mse_knn
}
# Now plot the results
plot(x = k_guesses, y = mse_res, main = "MSE vs. K for streams_per_view", xlab = "K", ylab = "MSE")
which.min(mse_res)

# Use the k shortlisted above in the KNN regression model
knn_reg_music <- knn.reg(music.train[, -c(11)],
                         music.test[, -c(11)],
                         music.train$streams_per_view,
                         k = 1)
# The MSE
music.test = music[test, "streams_per_view"]
mean((knn_reg_music$pred - music.train$streams_per_view)^2)
mean((knn_reg_music$pred - music.test$streams_per_view)^2)

####### Likes per view #######
## Random forests
set.seed(222)

rf.music <- randomForest(likes_per_view ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                         importance = TRUE, n.trees = 5000)

# Predictions
yhat.rf <- predict(rf.music, newdata = music[-train ,])
yhat.rf.t <- predict(rf.music, newdata = music[-test ,])
music.test = music[test, "likes_per_view"]
# as.data.frame(as.numeric(

## Mean squared error of prediction
mean((yhat.rf.t - music.train$likes_per_view)^2)
mean((yhat.rf - music.test$likes_per_view)^2)

## Bagging model
set.seed(222)

bag.music <- randomForest(likes_per_view ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                          mtry = 10, importance = TRUE)
bag.music$importance

# Now let's make some predictions 
yhat.bag <- predict(bag.music, newdata = music[-train,])
yhat.bag.t <- predict(bag.music, newdata = music[-test,])

mean((yhat.bag.t - music.train$likes_per_view)^2)
mean((yhat.bag - music.test$likes_per_view)^2)

## Boosting model
set.seed(222)

boost.music <- gbm(likes_per_view ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]), 
                   distribution = "gaussian", n.trees = 5000, 
                   interaction.depth = 4)
summary(boost.music)  

# Now let's make some predictions 
yhat.boost <- predict(boost.music, newdata = music[-train ,], 
                      n.trees = 5000)
yhat.boost.t <- predict(boost.music, newdata = music[-test ,], 
                        n.trees = 5000)

mean((yhat.boost.t - music.train$likes_per_view)^2)
mean((yhat.boost - music.test$likes_per_view)^2)

## Linear regression
set.seed(222)

linear <- lm(likes_per_view ~ Danceability + Key + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo, data = data.frame(music[-test,]))
summary(linear)

yhat.linear <- predict(linear, newdata = music[-train ,])
yhat.linear.t <- predict(linear, newdata = music[-test ,])

music.test = music[test, "likes_per_view"]
mean((yhat.linear.t - music.train$likes_per_view)^2)
mean((yhat.linear - music.test$likes_per_view)^2)

## KNN regression
set.seed(222)

str(music)
non_numeric_cols <- sapply(music, function(x) !is.numeric(x))

# Finding best K for predicting views
# Define the range of K values to test
k_guesses <- 1:100
# Initialize a tracker for the MSE values for each K
mse_res <- NULL

# Now loop through all the values
music.train = music[train,]
music.test = music[test,]

music.train <- music.train[, !non_numeric_cols]
music.test <- music.test[, !non_numeric_cols]

music.train <- music.train[, -c(1, 12, 13, 14, 15, 16, 17, 18)]
music.test <- music.test[, -c(1, 12, 13, 14, 15, 16, 17, 18)]

for(i in 1:length(k_guesses)){
  # For each value, run the model using the current K guess
  knn_reg_music <- knn.reg(music.train[, -c(11)],
                           music.test[, -c(11)],
                           music.train$likes_per_view,
                           k = k_guesses[i]) # key line here
  # The MSE
  mse_knn <- mean((knn_reg_music$pred - music.test$likes_per_view)^2)
  # Now update the tracker
  mse_res[i] <- mse_knn
}
# Now plot the results
plot(x = k_guesses, y = mse_res, main = "MSE vs. K for likes_per_view", xlab = "K", ylab = "MSE")
which.min(mse_res)

# Use the k shortlisted above in the KNN regression model
knn_reg_music <- knn.reg(music.train[, -c(11)],
                         music.test[, -c(11)],
                         music.train$likes_per_view,
                         k = 92)
# The MSE
music.test = music[test, "likes_per_view"]
mean((knn_reg_music$pred - music.train$likes_per_view)^2)
mean((knn_reg_music$pred - music.test$likes_per_view)^2)

# Trial analysis
ggplot(music, aes(x = Speechiness, y = streams_per_view)) + 
  geom_point() +
  ylim(0, 100) +
  geom_smooth(method="lm", se=FALSE)

str(music)
summary(music$streams_per_view)
summary(music$views_per_stream)
