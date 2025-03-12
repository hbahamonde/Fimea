cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Fimea/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# loadings
p_load(foreign)
dat <- read.spss("/Volumes/INVEST Data/Fimea/Harmonised data/Lääkebarometri2021_internet_panel.sav", to.data.frame=TRUE)

# Translating Data

# install.packages("devtools")
# devtools::install_github("zumbov2/deeplr")
# library(deeplr)

p_load(deeplr)


# Load required packages
p_load(dplyr, progress)

# Function to translate text with progress bar
translate_text <- function(text, pb) {
  if (!is.na(text) && is.character(text) && nzchar(text)) {  # Ensure it is a character
    tryCatch({
      result <- deeplr::translate(text = text,
                                  source_lang = "FI",
                                  target_lang = "EN",
                                  auth_key = "048783fe-31b8-4d6c-bfbc-0a5fd51f9c46")
      if (!pb$finished) pb$tick()  # Only tick if progress is not finished
      return(result)
    }, error = function(e) {
      warning(paste("Translation error:", e$message))
      if (!pb$finished) pb$tick()  # Prevent over-ticking
      return(text) # Return original text if translation fails
    })
  } else {
    if (!pb$finished) pb$tick()  # Update progress bar for NA or non-character values
    return(text)
  }
}

# Ensure all columns are character type before translation
dat <- dat %>%
  mutate(across(where(is.factor), as.character)) %>%  # Convert factors to characters
  mutate(across(where(is.numeric), as.character))    # Convert numbers to characters

# Correctly count the number of translatable elements (only character and non-NA values)
num_elements <- sum(sapply(dat, function(col) sum(!is.na(col) & is.character(col))))

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Translating [:bar] :percent ETA: :eta",
  total = num_elements,
  clear = FALSE,
  width = 60
)

# TEMP (just to see if the translating code works)
# set.seed(123)  # Ensure reproducibility
# dat <- sample_n(dat, 10)

# Apply translation function with progress bar (only on character columns)
# dat <- dat %>%
#  mutate(across(where(is.character), ~ sapply(., function(x) translate_text(x, pb))))

# Ensure progress bar is marked as finished
if (!pb$finished) pb$terminate()

# save translated data
# write.csv(dat, "/Users/hectorbahamonde/research/Fimea/dat.csv", row.names = FALSE)
# save(dat, file="/Users/hectorbahamonde/research/Fimea/dat.RData")


# loadings
##
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Fimea/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# loadings
load("/Users/hectorbahamonde/research/Fimea/dat.RData")

# Ensure the necessary libraries are loaded
p_load(deeplr, progress)

# Function to translate text with progress bar
translate_text <- function(text, pb) {
  if (!is.na(text) && is.character(text) && nzchar(text)) {  # Ensure it is a character
    tryCatch({
      result <- deeplr::translate(text = text,
                                  source_lang = "FI",
                                  target_lang = "EN",
                                  auth_key = "048783fe-31b8-4d6c-bfbc-0a5fd51f9c46")
      if (!pb$finished) pb$tick()  # Only tick if progress is not finished
      return(result)
    }, error = function(e) {
      warning(paste("Translation error:", e$message))
      if (!pb$finished) pb$tick()  # Prevent over-ticking
      return(text) # Return original text if translation fails
    })
  } else {
    if (!pb$finished) pb$tick()  # Update progress bar for NA or non-character values
    return(text)
  }
}

# Get codebook
codebook <- attributes(dat)$variable.labels

# Correctly count the number of translatable elements (only character and non-NA values)
num_elements <- sum(sapply(codebook, function(col) sum(!is.na(col) & is.character(col))))

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Translating [:bar] :percent ETA: :eta",
  total = num_elements,
  clear = FALSE,
  width = 60
)

# Apply translation function with progress bar (only on character elements)
codebook <- sapply(codebook, function(x) translate_text(x, pb))

# Ensure progress bar is marked as finished
if (!pb$finished) pb$terminate()

# Apply codebook
attributes(dat)$variable.labels <- codebook

# delete metadata in Finnish
for (col in names(dat)) {
  attr(dat[[col]], "names") <- NULL  # Remove the "names" attribute
  attr(dat[[col]], "labels") <- NULL # Remove any "labels" attributes if present
  attr(dat[[col]], "var.label") <- NULL # Remove other possible labels
}

# Recode ind var

dat$M6_6_KEHYSC = as.factor(dat$M6_6_KEHYSC)
dat$M6_6_KEHYSB = as.factor(dat$M6_6_KEHYSB)

p_load(dplyr, stringr)

# b
dat <- dat %>%
  mutate(M6_6_KEHYSB = str_replace(
    M6_6_KEHYSB, 
    "Lääke pitäisi ottaa käyttöön yhteiskunnan varoin, jos yritys alentaa sen hintaa", 
    "A medicine should be introduced at public expense if a company lowers its price"
  ))

# c
dat <- dat %>%
  mutate(M6_6_KEHYSC = str_replace(
    M6_6_KEHYSC, 
    "Lääke pitäisi ottaa käyttöön yhteiskunnan varoin yrityksen pyytämästä hinnasta riippumatta ", 
    "The medicine should be made available at public expense, regardless of the price charged by the company"
  ))

# generate outcome variable
dat <- dat %>%
  mutate(
    outcome = coalesce(M6_6_KEHYSB, M6_6_KEHYSC, M6_6_KEHYSD) # Capture the first non-NA value
  )

# recode outcome variable
p_load(dplyr, forcats)

dat <- dat %>%
  mutate(
    outcome = na_if(outcome, "I don't know"), # Replace "I don't know" with NA
    outcome = factor(outcome, levels = c(
      "The medicine should be made available at public expense, regardless of the price charged by the company", # Level 3
      "A medicine should be introduced at public expense if a company lowers its price", # Level 2
      "The medicine should not be introduced with social funding" # Level 1
    ))
  )

# recode 
p_load(dplyr)

dat <- dat %>% rename(Frame = M1_versio)
dat$Frame = as.factor(dat$Frame)

# save translated data
# write.csv(dat, "/Users/hectorbahamonde/research/Fimea/dat.csv", row.names = FALSE)
# save(dat, file="/Users/hectorbahamonde/research/Fimea/dat.RData")


#########
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Fimea/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# loadings
load("/Users/hectorbahamonde/research/Fimea/dat.RData")


# recoding
p_load(dplyr)
dat <- dat %>%
  mutate(outcome = factor(outcome, 
                          levels = c(
                            "The medicine should not be introduced with social funding",  # Least supportive (1)
                            "A medicine should be introduced at public expense if a company lowers its price",  # Middle (2)
                            "The medicine should be made available at public expense, regardless of the price charged by the company"  # Most supportive (3)
                          ), ordered = TRUE))

dat <- dat %>%
  mutate(Frame = relevel(Frame, ref = "Frame B"))  # Set "Frame B" as reference instead of "Frame A"


# Remove rows where outcome is NA
p_load(dplyr)
dat <- dat %>% filter(!is.na(outcome))
dat <- dat %>% filter(Frame != "Frame A") %>% mutate(Frame = fct_drop(Frame))  # Drop unused levels


# models

# Fit the ordinal logistic regression model
p_load(MASS)
model <- polr(outcome ~ Frame + M1_1 + M1_2_1 + M1_3 + M1_5, data = dat, Hess = TRUE)
summary(model)

# Generate predicted probabilities for a predictor
p_load(ggplot2, ggeffects)
predicted_probs <- ggpredict(model, terms = "Frame")

# plot
p_load(ggplot2)
ggplot(predicted_probs, 
       aes(x = x, y = predicted, color = response.level)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.5), width = 0.3) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Keep legend at the bottom
    legend.direction = "vertical",  # Stack legend levels vertically
    legend.key.height = unit(0.5, "cm")  # Increase spacing between legend items
  ) +
  guides(colour = guide_legend(title = "", ncol = 1))
