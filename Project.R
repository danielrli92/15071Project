library(tidyr)
library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

setwd("E:/MBA/15.071 Analytics Edge/Project")
Billboard <- read.csv("charts.csv")
SpotifyTracks <- read.csv("tracks.csv")


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
spotifyMinus <- SpotifyTracks[-c(1,7)]

#Merge Spotify information to billboard
merged <- inner_join(combined, spotifyMinus, by = c("name", "artists"))

#Merge songs based on max weeks.on.board
maxWeeks <- merge(aggregate(weeks.on.board ~ name, merged, max), merged)
maxWeeks <- unique(maxWeeks)
maxWeeks <- maxWeeks[!duplicated(maxWeeks[c("name", "artists")]),]

post2000 = filter(maxWeeks, maxWeeks$date>="1999-12-31")

ordered = post2000[order(post2000$date),]
test <- ordered[1:786,]
train <- ordered[787:2621,]

#Need to remove multiple entries, it seems Spotify has songs multiple times (maybe live performances, different versions), 
#Average columns and merge based on song/name
set.seed(896)
idx <- createDataPartition(post2000$weeks.on.board, p = 0.70, list = FALSE)
train <- maxWeeks[idx,]
test <- maxWeeks[-idx,]

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
