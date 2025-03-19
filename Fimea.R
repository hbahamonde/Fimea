#########
# Framing Experiment
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

# Remove rows
p_load(tidyverse)
dat <- dat %>% filter(!is.na(outcome))
dat <- dat %>% filter(Frame != "Frame A") %>% mutate(Frame = fct_drop(Frame))  # Drop unused levels

# Recode the Frame variable
dat$Frame <- factor(dat$Frame, 
                    levels = c("Frame B", "Frame C", "Frame D"),  # Questionnaires
                    labels = c("Control", "Rule of Rescue", "Utility Maximizing"))

# Set "Control" as the reference category
dat$Frame <- relevel(dat$Frame, ref = "Control")


# models

# Fit the ordinal logistic regression model
p_load(MASS)
model <- polr(outcome ~ Frame + M1_1 + M1_2_1 + M1_3 + M1_5, data = dat, Hess = TRUE)
#model <- polr(outcome ~ Frame + M2_1 + M2_2 + M1_1 + M1_2_1 + M1_3 + M1_5, data = dat, Hess = TRUE)
# summary(model)

# Generate predicted probabilities for a predictor
p_load(ggeffects)
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
  guides(colour = guide_legend(title = "", ncol = 1)) + 
  labs(x = "Frame", y = "Predicted Probabilities")

#########
# Decision Making Experiment
#########
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Fimea/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# loadings
load("/Users/hectorbahamonde/research/Fimea/dat.RData")

# From Katri
# The hypotheses are broadly that 
## 1. Framing Exp: framing affects how individuals answer the decision making task.
## 2. Dec Mak Exp: having to do the decision making task (vs. not having to do it) affects general opinions and trust. 

# "Frame A" (no dec making task but yes questions) and "Frame B" (yes dec mak task and yes questions)
## Right comparison is A (dec mak) vs B (no dec mak)
## Can't compare A v B+C+D because C and D do have other framings (rule of resq. and ut. max). THis might bias respondents.

# keep Frame A and B only
p_load(tidyverse)
dat <- dat %>% filter(Frame %in% c("Frame A", "Frame B")) %>% mutate(Frame = fct_drop(Frame))  # Drop unused levels

# re-factoring
p_load(tidyverse)
dat <- dat %>%
  filter(Frame %in% c("Frame A", "Frame B")) %>%
  mutate(Frame = recode(Frame, 
                        "Frame A" = "Without Task", 
                        "Frame B" = "With Task"),
         Frame = factor(Frame, levels = c("Without Task", "With Task"))) # Convert to factor with reference


# recoding
dat$M6_7_1 <- na_if(dat$M6_7_1, "I don't know")
dat$M6_7_2 <- na_if(dat$M6_7_2, "I don't know")

p_load(dplyr, stringr)

# M6_7_1
dat <- dat %>%
  mutate(M6_7_1 = str_replace(M6_7_1, "Jokseenkin eri mieltä", "Somewhat disagree")
         )

# M6_7_2
dat <- dat %>% mutate(M6_7_2 = str_replace(M6_7_2, "Jokseenkin eri mieltä", "Somewhat disagree"))
dat <- dat %>% mutate(M6_7_2 = str_replace(M6_7_2, "Jokseenkin samaa mieltä", "Somewhat agree"))

# re-factor ordered
dat$M6_7_1 <- factor(dat$M6_7_1, 
                     levels = c("Somewhat disagree", "Somewhat agree", "Totally disagree", "Totally agree"), 
                     ordered = TRUE)

dat$M6_7_2 <- factor(dat$M6_7_2, 
                     levels = c("Somewhat disagree", "Somewhat agree", "Totally disagree", "Totally agree"), 
                     ordered = TRUE
                     )

# to numeric
dat$M6_8 <- as.numeric(sub("^([0-9]+).*", "\\1", dat$M6_8))


# M6_7_1 M6_7_2 M6_8

# models
m1 <- lm(M6_8 ~ Frame + M2_1 + M2_2 + M1_1 + M1_2_1 + M1_3 + M1_5, data = dat)
summary(m1)

p_load(MASS)
model <- polr(M6_7_1 ~ Frame + M1_1 + M1_2_1 + M1_3 + M1_5, data = dat, Hess = TRUE)

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
  guides(colour = guide_legend(title = "", ncol = 1)) + 
  labs(x = "Frame", y = "Predicted Probabilities")
