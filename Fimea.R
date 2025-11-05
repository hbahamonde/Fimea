#########
# Framing Experiment
#########


## ---- loadings
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

# recoding
dat$M1_1 = as.factor(dat$M1_1)
dat$M1_10 = as.factor(dat$M1_10) # income
dat$M1_2_1 = as.numeric(dat$M1_2_1) # Age
dat$M1_5 = as.factor(dat$M1_5) # Occupation (includes retired)
dat$M2_2 = as.factor(dat$M2_2) # Illness that affect function to work
dat$M2_5 = as.factor(dat$M2_5) # Have illness that entitles Kela compensation
dat$M2_6_0 = as.factor(dat$M2_6_0) # Has a long-term illness 
dat$M2_11 = as.factor(dat$M2_11) # how much spends on medicine
dat$PAINOKERROIN = as.numeric(dat$PAINOKERROIN) # weight
dat$M1_3 = as.factor(dat$M1_3) # marital status 
dat$M1_9 = as.factor(dat$M1_9) # region
dat$M2_1 = as.factor(dat$M2_1) # how's your health today
dat$M2_13 = as.factor(dat$M2_13) # had financial problems to buy medicines last year

# recode income
dat$income_group <- forcats::fct_collapse(dat$M1_10,
                                          "Low" = c("Up to 1000 €", "1001-2000 €"),  # use exact output from levels(dat$M1_10)
                                          "Middle" = c("2001-3000 €", "3001-4000 €"),
                                          "High" = c("4001-5000 €", "5001-8000 €", "Over 8000"),
                                          "Other/Unknown" = c("I do not want to answer", "I don't know"))

# Set "Control" as the reference category
dat$Frame <- relevel(dat$Frame, ref = "Control")

# Recode spending on medicine
dat$M2_11.r <- NA  # initialize new variable

dat$M2_11.r[dat$M2_11 %in% c(
  "I do not take medicines prescribed by my doctor",
  "All 100 €"
)] <- "low"

dat$M2_11.r[dat$M2_11 %in% c(
  "100-299 €", "300-599 €"
)] <- "mid"

dat$M2_11.r[dat$M2_11 == "600 € or more"] <- "high"

dat$M2_11.r[dat$M2_11 == "I don't know"] <- NA  # optional, already NA if not matched

dat$M2_11.r = as.factor(dat$M2_11.r)

# recode age
dat$age_groups <- cut(
  dat$M1_2_1,
  breaks = c(17, 34, 59, 79),  # make sure lower bound includes 18
  labels = c("young", "middle", "old"),
  right = TRUE,
  include.lowest = TRUE
  )

dat$age_groups <- factor(dat$age_groups, levels = c("young", "middle", "old"))

# recode problems last year var
dat$M2_13_binary <- ifelse(
  dat$M2_13 == "There have been no problems at all",
  "no_problems",
  "problems"
  )

dat$M2_13_binary <- factor(dat$M2_13_binary, levels = c("no_problems", "problems"))
## ---- 


# models

# “rule of rescue”
## intuitive obligation to rescue people from death at any cost.
## Frame 2a: “There is no cure for this particular type of cancer. 
## The new medicine is a possible option for patients who have already 
## received multiple treatments and for whom the remaining options are limited”

#  “utility maximizing”
## rational principle of using resources in the way that they produce the 
## most health.
## Frame 2b: “The funds available to healthcare are finite. The adoption 
## of the new medicine means that the funds used to pay for it will mean 
## cuts elsewhere in healthcare”

# Fit the ordinal logistic regression model
p_load(MASS)
# Ordered Logistic or Probit Regression
model <- polr(outcome ~ Frame + 
                # must have
                M1_1 + # Gender
                M1_2_1 + # Age
                income_group + # income
                # controls
                M1_9+
                M2_5 + # Have illness that entitles Kela compensation
                M2_11  # how much spends on medicine
              # M2_6_0 + # Has a long-term illness 
              # M1_3 +  # marital status 
              #M2_2 + # Illness that affect function to work
              # M1_5 + # Occupation (includes retired)
              , 
              data = dat, 
              method = c("logistic"), # probit / logistic // probit was giving me predictions OUTSIDE of the range of the C.I.
              Hess = TRUE,
              weights = PAINOKERROIN)
# working model
# model <- polr(outcome ~ Frame + M1_1 + M1_2_1 + M1_3 + M1_5, data = dat, Hess = TRUE, weights = PAINOKERROIN)
# summary(model)

# Generate predicted probabilities for a predictor
p_load(ggeffects)
predicted_probs <- ggpredict(model, terms = "Frame")

# plot
p_load(ggplot2)
dodge <- position_dodge(width = 0.3)

ggplot(predicted_probs, 
       aes(x = x, y = predicted, color = response.level, 
           group = response.level)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                  position = dodge) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(colour = guide_legend(title = "", ncol = 1)) + 
  labs(x = "Frame", y = "Predicted Probabilities")


compare_estimates_ci <- function(est1, ci1, est2, ci2, level = c(0.90, 0.95)) {
  # Convert confidence intervals to standard errors
  z_values <- qnorm(1 - (1 - level) / 2)
  se1 <- (ci1[2] - ci1[1]) / (2 * qnorm(0.975))  # assumes 95% CI input
  se2 <- (ci2[2] - ci2[1]) / (2 * qnorm(0.975))
  
  diff <- est2 - est1
  se_diff <- sqrt(se1^2 + se2^2)
  
  results <- sapply(z_values, function(z) {
    z_score <- diff / se_diff
    is_significant <- abs(z_score) > z
    p_value <- 2 * (1 - pnorm(abs(z_score)))
    c(z_score = round(z_score, 3), 
      p_value = round(p_value, 4), 
      significant = is_significant)
  })
  
  colnames(results) <- paste0(level * 100, "% CI")
  return(as.data.frame(t(results)))
}

compare_estimates_ci(
  est1 = 0.27, 
  ci1 = c(0.22, 0.32), 
  est2 = 0.35, 
  ci2 = c(0.28, 0.43)
)


# Models

library(MASS)

build_sequential_models <- function(base_formula, controls, data, outcome_var, weights_var = NULL, method = "logistic") {
  models <- list()
  
  for (i in seq_along(controls)) {
    control_terms <- paste(controls[1:i], collapse = " + ")
    full_formula <- as.formula(paste(outcome_var, "~", base_formula, "+", control_terms))
    
    model_name <- paste0("m.", i)
    
    models[[model_name]] <- polr(
      formula = full_formula,
      data = data,
      method = method,
      Hess = TRUE,
      weights = if (!is.null(weights_var)) data[[weights_var]] else NULL
    )
  }
  
  # Add final model with all controls
  full_control_terms <- paste(controls, collapse = " + ")
  full_formula <- as.formula(paste(outcome_var, "~", base_formula, "+", full_control_terms))
  models[["m.final"]] <- polr(
    formula = full_formula,
    data = data,
    method = method,
    Hess = TRUE,
    weights = if (!is.null(weights_var)) data[[weights_var]] else NULL
  )
  
  return(models)
}

# Required base and control variable definitions
base_formula <- "Frame + M1_1 + M1_2_1 + M1_10 + 0.32"
controls <- c("M2_5", "M2_11", "M2_6_0", "M1_3", "M2_2", "M1_5")
outcome_var <- "outcome"

# Run the function
models <- build_sequential_models(
  base_formula = base_formula,
  controls = controls,
  data = dat,
  outcome_var = outcome_var,
  weights_var = "PAINOKERROIN",
  method = "logistic"
)

# table
p_load(texreg)
screenreg(models, 
          #omit.coef = "_2_",
          scalebox = 0.1,
          booktabs = TRUE, 
          use.packages = TRUE)











# Interaction (income_group)
# 1. Framing effects are inelastic to income groups, e.g., both rich and poor react similarly to the frames.

model <- polr(outcome ~ Frame*income_group + 
                M1_1 + # Gender
                M1_2_1 + # Age
                # M2_6_0 + # Has a long-term illness 
                # M1_3 +  # marital status 
                M2_2, # Illness that affect function to work
                # M1_5 + # Occupation (includes retired)
                #M2_11 + # how much spends on medicine
                #income_group, # income
              data = dat, 
              method = c("logistic"), # probit / logistic // probit was giving me predictions OUTSIDE of the range of the C.I.
              Hess = TRUE,
              weights = PAINOKERROIN)

predicted_probs <- ggpredict(model, terms = c("Frame", "income_group"))

ggplot(predicted_probs, 
       aes(x = x, y = predicted, color = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.5), width = 0.3) + 
  geom_line(position = position_dodge(width = 0.5), aes(group = group)) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~response.level) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(colour = guide_legend(title = "income_group", ncol = 1)) + 
  labs(x = "Frame", y = "Predicted Probabilities")

# Interaction (M2_13)
# 1. Framing effects are inelastic to income groups, e.g., both rich and poor react similarly to the frames.

model <- polr(outcome ~ Frame*M2_13 + 
                M1_1 + # Gender
                M1_2_1 + # Age
                # M2_6_0 + # Has a long-term illness 
                # M1_3 +  # marital status 
                #M2_2 + # Illness that affect function to work
              # M1_5 + # Occupation (includes retired)
              #M2_11 + # how much spends on medicine
                M2_1, # income
              data = dat, 
              method = c("logistic"), # probit / logistic // probit was giving me predictions OUTSIDE of the range of the C.I.
              Hess = TRUE,
              weights = PAINOKERROIN)

predicted_probs <- ggpredict(model, terms = c("Frame", "M2_13"))

ggplot(predicted_probs, 
       aes(x = x, y = predicted, color = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.5), width = 0.3) + 
  geom_line(position = position_dodge(width = 0.5), aes(group = group)) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~response.level) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(colour = guide_legend(title = "M2_13", ncol = 1)) + 
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



################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
abstract.c = as.character(c("Abstract here."))
writeLines(abstract.c, fileConn)
close(fileConn)
## ----




## ---- abstract.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----
