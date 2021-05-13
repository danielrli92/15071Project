library(tidyr)
library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

setwd("E:/MBA/15.071 Analytics Edge/Project")
Billboard <- read.csv("charts.csv")
SpotifyTracks <- read.csv("tracks.csv")

#Add the Genre to Spotify Tracks 
Artists<- read.csv("artists.csv")
colnames(Artists)[1] <- "id_artists"
Artists <- Artists[-c(4,5)]
SpotifyTracks$id_artists = substr(SpotifyTracks$id_artists,1,nchar(SpotifyTracks$id_artists)-2)
SpotifyTracks$id_artists = substr(SpotifyTracks$id_artists,3,nchar(SpotifyTracks$id_artists))
tracksandgenres <-merge(SpotifyTracks,Artists,by="id_artists")

#Only pop songs
popgenre <- filter(tracksandgenres,grepl('pop', genres))


##Cleaning Billboard Data, specifically artist to match the artist in Spotify Data
#Separating Artist column based on separator words
separated <- separate(Billboard, artist, c("a", "b", "c", "d","e"), sep = "featuring|Featuring|and|And|&")
#remove spaces after
separated[c ("a", "b","c","d")] <- lapply(seperated[c ("a", "b","c","d")], trimws)

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

##Clean Spotify data by removing their keys
spotifyMinus <- popgenre[-c(1,2)]

#Merge Spotify information to billboard
merged <- inner_join(combined, spotifyMinus, by = c("name", "artists"))

#Merge songs based on max weeks.on.board
maxWeeks <- merge(aggregate(weeks.on.board ~ name, merged, max), merged)
maxWeeks <- unique(maxWeeks)
maxWeeks <- maxWeeks[!duplicated(maxWeeks[c("name", "artists")]),]

spotify_billboard = merge(x=spotifyMinus, y=maxWeeks, by = c("name","artists"), all.x=TRUE)

maxWeeks$billboard = 1
spotifySub <- spotifyMinus
spotifySub<- spotifyMinus[ !(spotifySub$name %in% maxWeeks$name) & !(spotifySub$artist %in% maxWeeks$artist), ]
spotifySub$billboard = 0
spotifySub$weeks.on.board = NA
spotifySub$date = NA
spotifySub$rank = NA
spotifySub$last.week = NA
spotifySub$peak.rank = NA
master <- rbind(spotifySub, maxWeeks)

#Only look at past year 2000 songs
post2000 = filter(master, master$release_date>="1999-12-31")


#Split based on year
ordered = post2000[order(post2000$date),]
test <- ordered[1:786,]
train <- ordered[787:2621,]

#Split 70-30
set.seed(896)
idx <- createDataPartition(maxWeeks$weeks.on.board, p = 0.70, list = FALSE)
train <- maxWeeks[idx,]
test <- maxWeeks[-idx,]

#Linear Model for weeks on board
linearModel = lm(data = train, weeks.on.board ~ 
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
                   time_signature)

summary(linearModel)


predictmodel <- predict(linearModel, newdata = test)
residuals <- test$weeks.on.board - predictmodel #finding residuals
SSR <- sum((residuals)^2)
baseline <- mean(test$weeks.on.board)
SST <- sum((test$weeks.on.board - baseline)^2)
OSR <- 1 - SSR / SST
OSR

#Linear Model for peak rank

set.seed(896)
idx <- createDataPartition(maxWeeks$peak.rank, p = 0.70, list = FALSE)
train <- maxWeeks[idx,]
test <- maxWeeks[-idx,]

linearModel = lm(data = train, peak.rank ~ 
                   danceability+
                   #duration_ms+
                   explicit+
                   energy+
                   #key+
                   loudness+
                   #mode+
                   speechiness+
                   #acousticness+
                   instrumentalness+
                   #liveness+
                   valence+
                   #tempo+
                   time_signature)

summary(linearModel)


predictmodel <- predict(linearModel, newdata = test)
residuals <- test$peak.rank - predictmodel #finding residuals
SSR <- sum((residuals)^2)
baseline <- mean(test$peak.rank)
SST <- sum((test$peak.rank - baseline)^2)
OSR <- 1 - SSR / SST
OSR
