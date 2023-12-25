File_path<-file.path(getwd(),"musicdataset.csv")
File_path
music_data<-read.csv("musicdataset.csv")
music_data
sum(is.na(music_data))
library(dplyr)
library(tidyr)
library(stringr)
library(proxy)
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)
library(tidyverse)
####
print(head(music_data))
print(tail(music_data))
print(summary(music_data))
print(str(music_data))
music_genre<-unique(music_data$Top.Genre)
print(music_genre)
genre_count<-music_data %>%
  count(Top.Genre,sort=TRUE)
print(genre_count)
genre_count_plot <- music_data %>%
  group_by(Top.Genre) %>%
  summarize(n = n()) %>%
  filter(n > 10) %>%
  ggplot(aes(x = Top.Genre, y = n)) +
  geom_col()
print(genre_count_plot)
year_count_plot<-music_data %>%
  count(Year,sort=TRUE) %>%
  ggplot(aes(x=Year,y=n))+geom_col()
print(year_count_plot)
artist_sort<-music_data %>%
  count(Artist,sort=TRUE) %>%
  ggplot(aes(x=n))+geom_density()
artist_sort
top_10_artist<-head(artist_sort,10)
top_10_artist %>%
  ggplot(aes(x=n))+geom_col()
music_data %>%
  select(Title,Beats.Per.Minute..BPM.,Danceability,Length..Duration.,Popularity) %>%
  group_by(Title) %>%
  summarise_all(sum) %>%
  ggplot(aes(x=Length..Duration.,y=Popularity))+geom_jitter()
correlation_matrix<-cor(music_data[,c("Year","Energy","Danceability","Loudness..dB.","Liveness","Length..Duration.","Popularity")])
print(correlation_matrix)
heatmap(correlation_matrix, 
        cmap = ggplot2::scale_fill_viridis_c(), 
        cexRow = 0.8, 
        cexCol = 0.8, 
        margins = c(10, 10))
plot(music_data$Liveness,music_data$Popularity)
ggplot(data = music_data, aes(x = Danceability, y = Energy)) +
  geom_point() +
  xlab("Danceability") +
  ylab("Energy") +
  ggtitle("Danceability vs. Energy")
boxplot(Popularity~Liveness,data=music_data)
artist_count_plot <- music_data %>%
  group_by(Artist) %>%
  summarize(n = n()) %>%
  filter(n > 6) %>%
  ggplot(aes(x = Artist, y = n)) +
  geom_col()
print(artist_count_plot)
any_missing <- any(is.na(music_data))
any_missing
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")
######################################################################################
######################################################################################
music_data$keywords <- str_split_fixed(music_data$Title, "\\s+", n = 12) %>%
  + apply(., 1, function(x) paste(unique(x), collapse = " "))
genres <- unique(unlist(strsplit(as.character(music_data$Top.Genre), ",")))
genre_matrix <- sapply(genres, function(Top.Genre) grepl(Top.Genre, music_data$Top.Genre,
                                                         ignore.case = TRUE))
feature_matrix <- cbind(music_data$keywords, genre_matrix)
View(genre_matrix)
View(feature_matrix)
song_profiles <- cbind(music_data[, c("Index", "Title")], genre_matrix)
favorite_genres <- c("pop","modern rock")
user_profile <- data.frame(title = "User Profile", genre_matrix = as.numeric(genres %in%
                                                                               favorite_genres))
similarity_scores <- proxy::simil(feature_matrix, user_profile$genre_matrix, method =
                                    "cosine")
user_genres <- c("pop","modern rock")
# Compute the user profile based on favorite genres
user_profile <- data.frame(genre_matrix = as.numeric(colnames(feature_matrix) %in%
                                                       favorite_genres))

# Create a matrix from the user_profile column to match the dimensions of feature_matrix
user_profile_matrix <- matrix(user_profile$genre_matrix, ncol = ncol(feature_matrix), byrow =
                                TRUE)
similarity_scores <- proxy::simil(feature_matrix, user_profile_matrix, method = "cosine")
N <- 25
data_recommendations <- cbind(data, similarity = similarity_scores)
View(user_profile_matrix)
View(user_profile)
View(song_profiles)
# Convert feature_matrix to matrix type
feature_matrix <- as.matrix(feature_matrix)

# Convert user_profile_matrix to matrix type
user_profile_matrix <- as.matrix(user_profile_matrix)

# Convert missing values to 0
feature_matrix[is.na(feature_matrix)] <- 0
user_profile_matrix[is.na(user_profile_matrix)] <- 0
similarity_scores <- proxy::simil(feature_matrix, user_profile_matrix, method = "cosine")
similarity_matrix <- as.matrix(similarity_scores)
similarity_df <- data.frame(Index = 1:nrow(similarity_matrix), similarity_scores =
                              as.vector(similarity_matrix))
data_recommendations <- merge(data, similarity_df, by = "Index")
sorted_recommendations <-
  unique(data_recommendations[order(data_recommendations$similarity_scores, decreasing =
                                      TRUE), ])
top_songs <- sorted_recommendations[1:N, c("Index", "Title", "Artist", "Top.Genre","Popularity")]
print(top_songs)
View(top_songs)
#####################################################################################
#####################################################################################
artists <- unique(unlist(strsplit(as.character(music_data$Artist), ",")))
artist_matrix <- sapply(artists, function(Artist) grepl(Artist, music_data$Artist,ignore.case = TRUE))
real_matrix <- cbind(music_data$keywords, artist_matrix)
print(artist_matrix)
View(artist_matrix)
View(real_matrix)
songs <- cbind(music_data[, c("Index", "Title")], artist_matrix)
favorite_artists<- c("Coldplay", "Adele")
user <- data.frame(title = "User Profile", artist_matrix = as.numeric(artists %in%favorite_artists))
similarity_score_1 <- proxy::simil(real_matrix,user$artist_matrix, method ="cosine")
user_artists <- c("Coldplay", "Adele")
user<- data.frame(artist_matrix = as.numeric(colnames(real_matrix) %in%favorite_artists))
user_matrix <- matrix(user$artist_matrix, ncol = ncol(real_matrix), byrow = TRUE)
similarity_score_1 <- proxy::simil(real_matrix, user_matrix, method = "cosine")
N1 <- 15
data_recommendation_1 <- cbind(data, similarity = similarity_score_1)
View(user_matrix)
View(user)
View(songs)
real_matrix <- as.matrix(real_matrix)
user_matrix <- as.matrix(user_matrix)
real_matrix[is.na(real_matrix)] <- 0
user_matrix[is.na(user_matrix)] <- 0
similarity_score_1 <- proxy::simil(real_matrix, user_matrix, method = "cosine")
similarities_matrix_1 <- as.matrix(similarity_score_1)
similarities_df_1 <- data.frame(Index = 1:nrow(similarities_matrix_1), similarity_score_1 =as.vector(similarities_matrix_1))
data_recommendation_1 <- merge(music_data, similarities_df_1, by = "Index")
sorted_tracks <-unique(data_recommendation_1[order(data_recommendation_1$similarity_score_1, decreasing =TRUE), ])
top_tracks <- sorted_tracks [1:N1, c("Index", "Title", "Artist", "Top.Genre","Popularity")]
print(top_tracks)
View(top_tracks)
