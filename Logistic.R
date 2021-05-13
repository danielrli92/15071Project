library(tidyr)
library(tidyverse)
library(dplyr)
library(caret)
library(caTools)
library(ROCR)
library(ggplot2)
library(dplyr)
library(rpart.plot)
install.packages('e1071', dependencies=TRUE)

#LOAD DATA ----

Billboard <- read.csv("charts.csv")
SpotifyTracks <- read.csv("tracks.csv")
Databyartist<- read.csv("data_by_artist_o.csv")
Databygenre<- read.csv("data_by_genres_o.csv")
Databyyear<- read.csv("data_by_year_o.csv")
Artists<- read.csv("artists.csv")
Datao<- read.csv("data_o.csv")

#CLEAN DATA ----

#CLEAN SPOTIFY DATA 

#Add the Genre to Spotify Tracks 
colnames(Artists)[1] <- "id_artists"
Artists <- Artists[-c(4,5)]
SpotifyTracks$id_artists = substr(SpotifyTracks$id_artists,1,nchar(SpotifyTracks$id_artists)-2)
SpotifyTracks$id_artists = substr(SpotifyTracks$id_artists,3,nchar(SpotifyTracks$id_artists))
tracksandgenres <-merge(SpotifyTracks,Artists,by="id_artists")

#Filter Spotify Data to have only tracks that containt "Pop" 
popgenre <- filter(tracksandgenres,grepl('pop', genres))

#Clean Spotify data by removing their IDs
spotifytrackspop <- popgenre[-c(1,2)]

#CLEAN BILLBOARD DATA 

##Cleaning Billboard Data, specifically artist to match the artist in Spotify Data
#Separating Artist column based on separator words
separated <- separate(Billboard, artist, c("a", "b", "c", "d","e"), sep = "featuring|Featuring|and|And|&")
#remove spaces after
separated[c ("a", "b","c","d")] <- lapply(separated[c ("a", "b","c","d")], trimws)

#combine artist columns with commas between like Spotify data
combined <- Billboard
combined$artist <- paste0("'", separated$a, "', '", separated$b, "', '", separated$c, "', '", separated$d, "'")

#remove everything after NAs
combined$artist <-gsub("NA.*","",combined$artist)

#remove last quotations and commas
combined$artist = substr(combined$artist,1,nchar(combined$artist)-3)
combined$artist = paste0("[", combined$artist,"]")

#Rename columns to match Spotify data columns
colnames(combined)[3] <- "name"
colnames(combined)[4] <- "artists"

#Rename data set 
billboard=combined

#ADDING BILLBOARD DATA TO SPOTIFY TRACKS DATA ----


#Merge billboard information to spotify 
billboard$billboard = 1
spotify_billboard = merge(x=spotifytrackspop, y=billboard, by = c("name","artists"), all.x=TRUE)

spotify_billboard=distinct(spotify_billboard,name,.keep_all = TRUE)
spotify_billboard[is.na(spotify_billboard)] <- 0

summary(spotify_billboard)

#Log for if it charts
set.seed(896)
idx <- createDataPartition(spotify_billboard$billboard, p = 0.70, list = FALSE)
train <- spotify_billboard[idx,]
test <- spotify_billboard[-idx,]


logModel = glm(billboard ~ 
                danceability+
                duration_ms+
                explicit+
                energy+
                key+
                loudness+
                mode+
                speechiness+
                acousticness+
                instrumentalness+
                liveness+
                valence+
                tempo+
                time_signature,
                data = train,
                family = binomial,
               )
summary(logModel)

pred <- predict(logModel, newdata=test, type="response")
head(pred)

threshPred = (pred > .10)
table(threshPred)
table(test$billboard)

confusionMatrix = table(test$billboard, threshPred)
truePos = confusionMatrix[2,2]/sum(confusionMatrix[2,])
falsePos = confusionMatrix[1,2]/sum(confusionMatrix[1,])
accuracy = sum(diag(confusionMatrix)) / sum(confusionMatrix)
accuracy

performance(prediction((pred), test$billboard), "auc")@y.values[[1]]
rocr.pred <- prediction(pred, test$billboard)

plot(performance(rocr.pred,"tpr","fpr"))
abline(0,1)
AUC = as.numeric(performance(rocr.pred, "auc")@y.values)
AUC
