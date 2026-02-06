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
                            "The medicine should not be introduced with social funding",
                            "A medicine should be introduced at public expense if a company lowers its price",
                            "The medicine should be made available at public expense, regardless of the price charged by the company"
                          ),
                          labels = c(
                            "Reject public funding",
                            "Conditional funding (if price is reduced)",
                            "Unconditional public funding"
                          ),
                          ordered = TRUE
  ))
# Remove rows
p_load(tidyverse)
dat <- dat %>% filter(!is.na(outcome))
dat <- dat %>% filter(Frame != "Frame A") %>% mutate(Frame = fct_drop(Frame))  # Drop unused levels

# Recode the Frame variable
p_load(dplyr, forcats)

dat <- dat %>%
  filter(!is.na(outcome)) %>%
  filter(Frame %in% c("Frame B", "Frame C", "Frame D")) %>%
  mutate(
    Frame = factor(Frame,
                   levels = c("Frame B", "Frame C", "Frame D"),
                   labels = c("Control", "Loss (rescue) frame", "Gains (stewardship) frame")
    )
  )


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
# dat$Frame <- relevel(dat$Frame, ref = "Control")

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
p_load(ggeffects, ggplot2, stringr)

# 90% CI (alpha = 0.10)
predicted_probs <- ggpredict(model, terms = "Frame", ci.lvl = 0.90)

# plot
dodge <- position_dodge(width = 0.3)

pred_plot <- ggplot(predicted_probs,
                    aes(x = x, y = predicted, color = response.level,
                        group = response.level)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = dodge) +
  scale_color_discrete(
    name = NULL,
    labels = function(x) stringr::str_wrap(x, width = 70)
  ) +
  coord_flip() +   # rotate the whole plot
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.box.spacing = unit(0.3, "cm"),
    aspect.ratio = 1,
    panel.border = element_rect(fill = NA, linewidth = 0.8)
  ) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(x = "Frame", y = "Predicted Probabilities")


# Compare stats

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


## Table

# Nice labels (edit as needed)
coef_map <- c(
  # Frames
  "FrameLoss (rescue) frame"         = "Loss (rescue) frame (vs. control)",
  "FrameGains (stewardship) frame"   = "Gains (stewardship) frame (vs. control)",  
  # Gender
  "M1_1Male"                = "Male (vs. female)",
  
  # Age
  "M1_2_1"                  = "Age",
  
  # Income
  "income_groupMiddle"      = "Income: middle (vs. low)",
  "income_groupHigh"        = "Income: high (vs. low)",
  "income_groupOther/Unknown" = "Income: other/unknown",
  
  # Region / education (whatever M1_9 actually is)
  "M1_9"                    = "Region",
  
  # Kela compensation
  "M2_5Yes"                 = "Eligible for Kela reimbursement",
  
  # Medicine spending
  "M2_11mid"                = "Medicine spending: medium",
  "M2_11high"               = "Medicine spending: high"
)


#dat$Frame <- relevel(factor(dat$Frame), ref = "B")

p_load(MASS, modelsummary, broom, dplyr, tibble)

dir.create("build", showWarnings = FALSE, recursive = TRUE)

tab_tex <- modelsummary::msummary(
  list("Ordinal logit" = model),
  coef_map = coef_map,
  exponentiate = TRUE,
  statistic = "conf.int",
  conf_level = 0.90,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|RMSE|F|R2|Adj|Within|Between|Std.Errors",
  output = "latex_tabular"
)

writeLines(enc2utf8(as.character(tab_tex)), "build/table_model.tex", useBytes = TRUE)

## ---- 


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
abstract.c = as.character(c("Governments often insulate authority over costly and controversial healthcare funding decisions from day-to-day electoral politics, relying on technical assessments and objective criteria to justify those decisions. Yet public responses to these decisions remain highly sensitive to how identical choices are presented. This article examines whether institutional insulation constrains framing effects on mass preferences, or whether citizens’ evaluations of state funding decisions remain elastic to framing even when authority is formally insulated from electoral politics. Drawing on prospect theory, we argue that preferences over healthcare spending are reference-point dependent, such that citizens respond differently when funding decisions are framed as avoiding salient losses versus preserving existing gains. We test this argument using a population-based survey experiment embedded in the 2021 Finnish Medicines Barometer, in which respondents evaluate an identical clinical vignette involving a novel, high-cost cancer medicine with uncertain benefits and are randomly assigned to alternative frames emphasizing last-resort rescue or stewardship of finite collective resources. Holding clinical evidence, costs, and uncertainty constant, we find substantial and systematic framing effects on mass preferences over public funding. Support for funding increases when decisions are framed as loss avoidance and decreases when framed as gain preservation, despite identical policy content. These findings demonstrate that institutional insulation from electoral politics does not neutralize framing effects on mass preferences. Instead, decisions justified through technical criteria remain politically consequential because public communication and interpretive frames shape how citizens evaluate distributive choices. By integrating research on framing with the political economy of delegation, this article explains why distributive healthcare policies remain politically contested even when citizens are not the formal decision-makers and highlights the behavioral foundations of public responses to state action under conditions of risk, scarcity, and institutional insulation."))
close(fileConn)
## ----




## ---- abstract.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----
