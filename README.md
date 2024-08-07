# Music-Recommendation-System
The purpose of this project is to develop a music recommendation system based on user preferences and song features. The system aims to provide personalized song recommendations to users based on their favourite genres and artists.
The recommendation model used in this project is based on content-based recommender. Content- based recommender is a commonly used technique in recommendation systems that uses the commonly used method cosine similarity method.

Feature Extraction: Identify the relevant features or attributes that you want to use for similarity calculation. These features will be used to create a profile for each item (in this case, songs) in the dataset. For example, you may choose to use attributes like genre, artist, and song duration.

Vectorization: Convert the extracted features into a numerical representation for each song. This can be done using various methods such as one-hot encoding or numerical scaling. The goal is to represent each song as a vector in a multi-dimensional feature space.

Before building the recommendation model, the music dataset underwent preprocessing steps to transform it into a suitable format for collaborative filtering. This included extracting keywords from song titles and creating a binary matrix to indicate the presence of each music genre. The resulting feature matrix represented the features of each song in terms of keywords and genre presence. The data was transformed into a suitable format for modeling or algorithm implementation. Genre and artist data was converted to genre matrix and artist matrix. Further a similarity matrix was created.

User Profile: To generate personalized recommendations, the model required information about the user's preferences. In this case, the user's favorite music genres were defined and used to create a user profile matrix. The user profile matrix represented the user's preferences in terms of genre presence, aligning with the feature matrix.

Similarity Calculations:
The next step involved calculating the similarity between the feature matrix and the user profile matrix. Similarity scores were computed using the cosine similarity measure, which measures the cosine of the angle between two vectors. It provides a measure of similarity between two vectors, with values ranging from -1 to 1, where 1 indicates perfect similarity.
Recommendation Generation:
Once the similarity scores were calculated, they were merged with the original dataset to associate each song with its corresponding similarity score. The dataset was then sorted based on the similarity scores in descending order. The top N recommended songs were selected from the sorted dataset and presented to the user as personalized recommendations.
In addition to genre-based recommendations, artist-based recommendations were also provided. The process for artist-based recommendations was similar to genre-based recommendations. The presence of each artist in the dataset was identified, and a user profile matrix representing the user's favorite artists was created. Similarity scores were computed using the cosine similarity measure, and the dataset was sorted based on these scores to generate artist-based recommendations.
