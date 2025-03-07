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
dat <- dat %>%
  mutate(across(where(is.character), ~ sapply(., function(x) translate_text(x, pb))))

# Ensure progress bar is marked as finished
if (!pb$finished) pb$terminate()

# save translated data
write.csv(dat, "/Users/hectorbahamonde/research/Fimea/dat.csv", row.names = FALSE)
save(dat, file="/Users/hectorbahamonde/research/Fimea/dat.RData")
