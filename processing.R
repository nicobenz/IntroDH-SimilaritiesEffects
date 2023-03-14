library(readxl)
library(ggplot2)
source("preprocessing.R")

###
### functions
###

# function to extract values for response/target combinations
get_xy_values <- function(df) {
  values <- data.frame(
    x = numeric(), y = numeric(), 
    response = character(), target = character())
  # counter for correct appending when skipping rows
  count = 1
  for (i in 1:nrow(df)) {
    response <- as.character(df[i, "Response"])
    target <- as.character(df[i, "Target"])
    # check for valid response/target combinations
    if (target != response &&
        response != "-" &&
        response != "timeout" &&
        response %in% phonemes &&
        target %in% phonemes) {
      y <- visual_similarity[response, target]
      x <- phonetic_similarity[response, target]
      # append values to data frame
      values[count, ] <- c(x, y, response, target)
      count <- count + 1
    } else {
      # skip invalid rows
    }
  }
  return (values)
}

# associate values with their correct buckets for heatmap plotting
get_bracket_index <- function(val) {
  if (val <= 0.1) {
    return(1)
  } else if (val <= 0.2) {
    return(2)
  } else if (val <= 0.3) {
    return(3)
  } else if (val <= 0.4) {
    return(4)
  } else if (val <= 0.5) {
    return(5)
  } else if (val <= 0.6) {
    return(6)
  } else if (val <= 0.7) {
    return(7)
  } else if (val <= 0.8) {
    return(8)
  } else if (val <= 0.9) {
    return(9)
  } else if (val <= 1.0) {
    return(10)
  }
}

# preprocess data to plot in heatmap
get_plotting_df <- function(df, size=10) {
  heatmap_matrix <- matrix(0, nrow = 10, ncol = 10, byrow = TRUE)
  
  for (i in 1:nrow(df)) {
    y <- get_bracket_index(df[i, "x"])
    x <- get_bracket_index(df[i, "y"])
    # increment heat map buckets
    heatmap_matrix[x, y] = heatmap_matrix[x, y] + 1
  }
  heatmap_df <- reshape2::melt(heatmap_matrix) # reshape for better plotting
  
  return(heatmap_df)
}

# calculate p values for heatmap labels
get_p_value_df <- function(observed_values, expected_values, direction) {
  p_value_df <- data.frame(observed_values)
  # create a new column in observed_data to store the p-values
  p_value_df$p_values <- NA
  for (i in 1:nrow(observed_values)) {
    observed <- observed_values$value[i]
  
    # increment count if expected value is >= (or <=) observed value
    count <- 0
    for (j in 1:length(expected_values)) {
      expected <- expected_values[[j]]$value[i]
      if (direction == "positive") {
        if (expected >= observed) {
          count <- count + 1
        }
      } else if (direction == "negative") {
        if (expected <= observed) {
          count <- count + 1
        }
      }
    }
    # p value depends on count value in relation to length of expected values
    p_value = count / (length(expected_values))
    observed_values$p_values[i] = p_value
  }
  return(observed_values)
}

# output glyph combinations for specified heatmap cube
query_heatmap <- function(xy_vector, values) {
  result_list <- list()
  for (xy in xy_vector) {
    # create upper and lower bounds of cube
    x_upper_bound = xy[2] / 10
    x_lower_bound = x_upper_bound - 0.1
    
    y_upper_bound = xy[1] / 10
    y_lower_bound = y_upper_bound - 0.1
    result <- data.frame(response = character(), target = character(), x_y = character())
    count = 1
    
    for (i in 1:nrow(values)) {
      # check for values between bounds
      if (values$x[i] > x_lower_bound && values$x[i] <= x_upper_bound &&
          values$y[i] > y_lower_bound && values$y[i] <= y_upper_bound) {
        result[count, "response"] <- values$response[i]
        result[count, "target"] <- values$target[i]
        result[count, "x_y"] <- paste0(xy[1], "_", xy[2])
        count = count + 1
      }
    }
    result <- result[!duplicated(result), ]
    result_list <- c(result_list, list(result))
  }
  result <- do.call(rbind, result_list)
  return(result)
}


###
### content
###

# read similarity values
phonetic_similarity <- read.csv(
  "tables/phonetic_jaccard_similarities.csv",
  row.names = 1)
visual_similarity <- read.csv(
  "tables/visual_jaccard_similarities.csv",
  row.names = 1)

# get all possible combinations of glyphs for plotting
all_combinations <- data.frame(x = numeric(), y = numeric())
count <- 1
for (i in 1:nrow(phonetic_similarity)) {
  for (j in 1:nrow(visual_similarity)) {
    if (i != j) {
      y <- visual_similarity[i, j]
      x <- phonetic_similarity[i, j]
      all_combinations[count, ] <- c(x, y)
      count <- count + 1
    } else {
      # do nothing to skip combinations of same glyph
    }
  }
}

# scatter plot for distribution of possible errors
plot <- ggplot(all_combinations,aes(x = x, y = y)) +
  geom_point(color = "cornflowerblue") +
  labs(x = "Visual similarity", y = "Phonetic similarity")

# save for use in latex
ggsave("plots/all_glyph_combinations.pdf", plot, width = 12, height = 10, units = "cm", device = "pdf")

# read main data
data <- read_excel("data/data.xlsx")

# get values of visual and phonetic similarity for every valid error
xy_values <- get_xy_values(data)

# reshape
observed_values <- get_plotting_df(xy_values)

# create random combinations as a baseline
set.seed(123)  # seed for reproducibility
n_simulations <- 10000 # specifies number of repetitions for permutation test
# simulations with n = 10000 took 10+ min on my machine, so reduced value for testing is advised

# empty list to store dataframe of every simulation
simulations_collected <- list()
# create n_simulations simulations
for (i in 1:n_simulations) {
  baseline <- data.frame(matrix(ncol = 2, nrow = nrow(xy_values)))
  colnames(baseline) <- c("Response", "Target")
  
  # create randomly selected letter combination of same length as real data
  for (j in 1:nrow(xy_values)) {
    random1 <- sample(phonemes, 1)
    random2 <- sample(phonemes[phonemes != random1], 1)
    baseline[j, "Response"] <- random1 
    baseline[j, "Target"] <- random2
  }
  # perform operations like done with real data
  random_xy_values <- get_xy_values(baseline)
  random_heatmap_df <- get_plotting_df(random_xy_values)

  # store in list
  simulations_collected <- c(simulations_collected, list(random_heatmap_df))
}

# calculate mean of each cell over all simulations
simulations_mean <- Reduce("+", simulations_collected) / length(simulations_collected)

# create residual value df (amount of values over/under baseline)
residual_values <- data.frame(observed_values)
residual_values$value <- residual_values$value - simulations_mean$value

# calculate p values
pos_p_values <- get_p_value_df(observed_values, simulations_collected, "positive")
neg_p_values <- get_p_value_df(observed_values, simulations_collected, "negative")

# combine p-values
all_p_values <- data.frame(matrix(nrow = 10, ncol = 10))
colnames(all_p_values) <- c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5",
                            "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
rownames(all_p_values) <- c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5",
                            "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")
for (i in 1:10) {
  for (j in 1:10) {
    pos_p <- subset(pos_p_values, Var1 == j & Var2 == i)
    pos_p <- pos_p$p_values
    neg_p <- subset(neg_p_values, Var1 == j & Var2 == i)
    neg_p <- neg_p$p_values
    
    all_p_values[i, j] <- paste0(pos_p, ", ", neg_p)
  }
}

# save p values
write.csv(all_p_values, file = "tables/p_values.csv", row.names = TRUE)

# create df of labels to report p-values following APA standard
residual_values$p_label <- NA
for (i in 1:nrow(residual_values)) {
  if (pos_p_values$p_values[i] < neg_p_values$p_values[i]) {
    p_value <- pos_p_values$p_values[i]
  } else {
    p_value <- neg_p_values$p_values[i]
  }
  if (p_value < 0.001) {
    # p values under 0.001 have to be reported as p < .001
    residual_values$p_label[i] <- "p < .001"
  } else if (p_value < 0.01) {
    # p values under 0.01 have to be reported rounded to 3 digits after decimal
    p_label <- as.character(round(p_value, 3))
    #p_label <- format(p_value, nsmall = 3)
    p_label <- gsub("0.", "p = .", p_label, fixed = TRUE)
    residual_values$p_label[i] <- p_label
  } else if (p_value <= 0.05) {
    # p values under 0.05 have to be reported rounded to 2 digits after decimal
    p_label <- as.character(round(p_value, 2))
    #p_label <- format(p_value, nsmall = 2)
    p_label <- gsub("0.", "p = .", p_label, fixed = TRUE)
    residual_values$p_label[i] <- p_label
  } else {
    # ns
    residual_values$p_label[i] <- ""
  }
}

# plot heatmap
plot <- ggplot(residual_values, aes(x = Var1, y = Var2, fill = value, label = p_label)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightseagreen", mid = "white", high = "cornflowerblue", midpoint = 0) + 
  labs(x = "Visual similarity", y = "Phonetic similarity") + 
  scale_x_continuous(breaks = seq(0.5, 10.5, 1), labels = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0.5, 10.5, 1), labels = seq(0, 1, 0.1)) + 
  geom_text(aes(label = p_label), hjust = 0.5, vjust = 0.5, size = 2)

print(plot) # needed to see plot in r studio

# save heatmap
ggsave("plots/heatmap.pdf", plot, width = 14, height = 10, units = "cm", device = "pdf")

# vector of positions of significant cubes in heatmap
query_cells <- list(
  c(1,1),
  c(5,8),
  c(6,7),
  c(7,7),
  c(8,4),
  c(9,3),
  c(9,9)
  )

# query significant cubes and save corresponding letter combinations
significant_combinations <- query_heatmap(query_cells, xy_values)
write.csv(significant_combinations, file = "tables/significant_combinations.csv", row.names = TRUE)
