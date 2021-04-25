
library(tidyr)
library(tidyverse)
library(dplyr)

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

#Need to remove multiple entries, it seems Spotify has songs multiple times (maybe live performances, different versions), 
#Average columns and merge based on song/name

linearModel = lm(data = maxWeeks, weeks.on.board ~ .)
summary(linearModel)
