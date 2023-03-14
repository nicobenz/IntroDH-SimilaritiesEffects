library(readxl)


# jaccard similarity to calculate phoneme similarity based on feature vectors
jaccard_similarity <- function(a, b) {
  intersection <- sum(a & b)
  union <- sum(a | b)
  return (intersection / union)
}

###
### generate phonetic similarities
###

# load phonetic features from csv file
features <- read.csv("data/phonetic_features.csv")

# extract names and delete from features df
phonemes <- features$PHONEMES
features <- features[,-1]

# convert to matrix for jaccard similarity
features <- as.matrix(features)

# apply jaccard similarity function on all combinations of phonemes
phonetic_jaccard_similarities <- apply(features, 1, function(a) {
  apply(features, 1, function(b) {
    jaccard_similarity(a, b)
  })
})

# save for later
colnames(phonetic_jaccard_similarities) <- phonemes
rownames(phonetic_jaccard_similarities) <- phonemes
write.csv(phonetic_jaccard_similarities, 
          file = "tables/phonetic_jaccard_similarities.csv", 
          row.names = TRUE)


###
### process visual similarities
###

# load data from Simpson(2012)
visual_matrix <- read_excel("data/visual_similarity_Simpson2012.xlsx",
                            sheet = 3)

# drop 'a' character with wrong font from df
visual_matrix <- visual_matrix[-c(1:52),]

# drop 'a' character from other rows
i <- 1
while (i <= nrow(visual_matrix)) {
  visual_matrix <- visual_matrix[-i,]
  i <- i + 51
}

# extract visual similarity scores from df
visual_jaccard_similarities <- matrix(nrow = length(phonemes), ncol = length(phonemes))
for (i in 1:length(phonemes)) {
  for (j in 1:length(phonemes)) {
    query <- visual_matrix[which(visual_matrix$Letter1 == phonemes[i] & visual_matrix$Letter2 == phonemes[j]),]
    if (nrow(query) != 0) {
      visual_sim <- (as.numeric(query[["Value"]]) - 1)/6  # linear change to 0-1 from 1-7 scale
      visual_jaccard_similarities[i,j] <- visual_sim
    } else {
      visual_jaccard_similarities[i,j] <- 1  # use 1 for identical characters because Simpson (2012) did not include identity
    }
  }
}

# save for later
colnames(visual_jaccard_similarities) <- phonemes
rownames(visual_jaccard_similarities) <- phonemes
write.csv(visual_jaccard_similarities, 
          file = "tables/visual_jaccard_similarities.csv", 
          row.names = TRUE)
