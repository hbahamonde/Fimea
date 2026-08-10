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
## original dataset
## p_load(haven)
## dat2 <- read_sav("/Users/hectorbahamonde/research/Fimea/original_dataset/Lääkebarometri2021_internet_panel.sav")


# recoding
p_load(dplyr)
dat <- dat %>%
  mutate(
    outcome = dplyr::coalesce(
      outcome,
      if_else(M6_6_KEHYSB == "I don't know", "I don't know", NA_character_),
      if_else(M6_6_KEHYSC == "I don't know", "I don't know", NA_character_),
      if_else(M6_6_KEHYSD == "I don't know", "I don't know", NA_character_)
    )
  )

# recoding (now safe to factorize outcome)
p_load(dplyr)

dat <- dat %>%
  mutate(outcome = factor(outcome,
                          levels = c(
                            "The medicine should not be introduced with social funding",
                            "A medicine should be introduced at public expense if a company lowers its price",
                            "The medicine should be made available at public expense, regardless of the price charged by the company",
                            "I don't know"
                          ),
                          labels = c(
                            "Reject public funding",
                            "Conditional funding (if price is reduced)",
                            "Unconditional public funding",
                            "I don't know"
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
                   labels = c("Control", "Loss (rescue) frame", "Gains (health maximisation) frame")
    )
  )


# recoding
dat$M1_1 <- factor(dat$M1_1, levels = c("Female", "Male", "Other"))
dat$M1_1 = as.factor(dat$M1_1)
# Keep only Female/Male (drop "Other" + any NA)
dat <- dat %>%
  dplyr::filter(M1_1 %in% c("Female", "Male")) %>%
  dplyr::mutate(M1_1 = droplevels(M1_1))
dat$M1_10 = as.factor(dat$M1_10) # income
# Recode income (M1_10) into broader groups
dat <- dat %>%
  dplyr::mutate(
    income_group = forcats::fct_collapse(
      M1_10,
      "Up to 3000 €"   = c("Up to 1000 €", "1001-2000 €", "2001-3000 €"),
      "3001-5000 €"    = c("3001-4000 €", "4001-5000 €"),
      "5001+ €"        = c("5001-8000 €", "Over 8000"),
      "Missing/No answer" = c("I do not want to answer", "I don't know")
    ),
    income_group = factor(
      income_group,
      levels = c("Up to 3000 €", "3001-5000 €", "5001+ €", "Missing/No answer")
    )
  )
dat$M1_2_1 = as.numeric(dat$M1_2_1) # Age
dat$M1_5 = as.factor(dat$M1_5) # Occupation (includes retired)
dat$M2_2 = as.factor(dat$M2_2) # Illness that affect function to work
dat$M2_5 = as.factor(dat$M2_5) # Have illness that entitles Kela compensation
dat$M2_6_0 = as.factor(dat$M2_6_0) # Has a long-term illness 
dat$M2_11 = as.factor(dat$M2_11) # how much spends on medicine
dat$PAINOKERROIN = as.numeric(dat$PAINOKERROIN) # weight
dat$M1_3 = as.factor(dat$M1_3) # marital status 
dat$M1_9 = as.factor(dat$M1_9) # region
# Recode region (M1_9) to NUTS2 (Finland)
dat <- dat %>%
  dplyr::mutate(
    M1_9 = dplyr::case_when(
      M1_9 == "Uusimaa" ~ "Helsinki-Uusimaa",
      M1_9 %in% c("Southwest Finland", "Kanta-Häme", "Päijät-Häme", "Kymenlaakso", "South Karelia") ~ "Southern Finland",
      M1_9 %in% c("Satakunta", "Pirkanmaa", "Central Finland", "South Ostrobothnia", "Ostrobothnia") ~ "Western Finland",
      M1_9 %in% c("Etelä-Savo", "North Savo", "North Karelia", "Central Ostrobothnia", "North Ostrobothnia", "Kainuu", "Lapland") ~ "Northern and Eastern Finland",
      M1_9 == "Åland" ~ "Åland",
      TRUE ~ NA_character_
    ),
    M1_9 = factor(
      M1_9,
      levels = c("Helsinki-Uusimaa", "Southern Finland", "Western Finland", "Northern and Eastern Finland", "Åland")
    )
  )


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


# recode problems last year var
dat$M2_13_binary <- ifelse(
  dat$M2_13 == "There have been no problems at all",
  "no_problems",
  "problems"
  )

dat$M2_13_binary <- factor(dat$M2_13_binary, levels = c("no_problems", "problems"))

# Order of the Don't know:
dat <- dat %>%
  mutate(
    outcome_ord = case_when(
      outcome == "Reject public funding" ~ "Reject public funding",
      outcome == "Conditional funding (if price is reduced)" ~ "Conditional funding (if price is reduced)",
      outcome == "Unconditional public funding" ~ "Unconditional public funding",
      outcome == "I don't know" ~ NA_character_
    ),
    outcome_ord = factor(
      outcome_ord,
      levels = c(
        "Reject public funding",
        "Conditional funding (if price is reduced)",
        "Unconditional public funding"
      ),
      ordered = TRUE
    )
  )

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
dat <- droplevels(dat)
model <- polr(
  outcome_ord ~ Frame +
    M1_1 +
    M1_2_1 +
    income_group +
    M1_9 +
    M2_5 +
    M2_11,
  data = dat,
  method = "logistic",
  Hess = TRUE,
  weights = PAINOKERROIN
  )

# Test the proportional-odds (parallel-slopes) assumption used by polr.
# nominal_test() fits a separate non-proportional effect for each term;
# a small p-value indicates that the common-slope restriction is violated.
p_load(ordinal)

model.proportional_odds_test <- ordinal::clm(
  formula = formula(model),
  data = dat,
  weights = PAINOKERROIN,
  link = "logit",
  Hess = TRUE
)

proportional_odds_test <- ordinal::nominal_test(
  model.proportional_odds_test
)
print(proportional_odds_test)

proportional_odds_test_table <- data.frame(
  term = rownames(proportional_odds_test),
  as.data.frame(proportional_odds_test, check.names = FALSE),
  row.names = NULL,
  check.names = FALSE
)

dir.create("build", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(
  proportional_odds_test_table,
  "build/proportional_odds_test.csv",
  row.names = FALSE
)

# working model
# model <- polr(outcome ~ Frame + M1_1 + M1_2_1 + M1_3 + M1_5, data = dat, Hess = TRUE, weights = PAINOKERROIN)
# summary(model)

# Generate predicted probabilities for a predictor
p_load(ggeffects, ggplot2, stringr)

# 90% CI (alpha = 0.10)
predicted_probs <- ggpredict(model, terms = "Frame", ci.lvl = 0.90)

# force legend order so "I don't know" is last
predicted_probs$response.level <- factor(
  predicted_probs$response.level,
  levels = c(
    setdiff(as.character(unique(predicted_probs$response.level)), "I don't know"),
    "I don't know"
  )
)

# plot
dodge <- position_dodge(width = 0.3)

pred_plot <- ggplot(predicted_probs,
                    aes(x = x, y = predicted, color = response.level,
                        group = response.level)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = dodge) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Reject public funding" = "red",
      "Conditional funding (if price is reduced)" = "goldenrod2",
      "Unconditional public funding" = "green"
    ),
    breaks = c(
      "Reject public funding",
      "Conditional funding (if price is reduced)",
      "Unconditional public funding"
    ),
    labels = function(x) stringr::str_wrap(x, width = 70)
  ) +
  coord_flip() +
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

# compare_estimates_ci(
#  est1 = 0.27, 
#  ci1 = c(0.22, 0.32), 
#  est2 = 0.35, 
#  ci2 = c(0.28, 0.43)
#)


## Table

p_load(MASS, nnet, modelsummary, broom, dplyr, tibble)

dir.create("build", showWarnings = FALSE, recursive = TRUE)

# estimation sample actually used by polr
mf <- model.frame(model)
w_used <- model.weights(mf)

# multinomial logit (I removed “I don’t know” from the ordinal model but will keep it in the multinomial model below)
model.multinomial <- nnet::multinom(
  relevel(factor(outcome, ordered = FALSE), ref = "Reject public funding") ~
    Frame +
    M1_1 +
    M1_2_1 +
    income_group +
    M1_9 +
    M2_5 +
    M2_11,
  data = dat,
  weights = PAINOKERROIN,
  Hess = TRUE,
  trace = FALSE
)

#
# Figure 2 analogue from the multinomial model (including "I don't know")
p_load(marginaleffects)

# Use the same covariate profile as ggpredict() in Figure 2: factor
# covariates at their reference/modal categories and age at its weighted mean.
multinomial_prediction_grid <- marginaleffects::datagrid(
  model = model.multinomial,
  Frame = levels(model.frame(model.multinomial)$Frame),
  M1_2_1 = weighted.mean(
    model.frame(model.multinomial)$M1_2_1,
    model.multinomial$weights,
    na.rm = TRUE
  )
)

multinomial_predicted_probs <- marginaleffects::predictions(
  model.multinomial,
  newdata = multinomial_prediction_grid,
  conf_level = 0.95
) %>%
  as.data.frame() %>%
  dplyr::mutate(
    response.level = factor(
      as.character(group),
      levels = c(
        "Reject public funding",
        "Conditional funding (if price is reduced)",
        "Unconditional public funding",
        "I don't know"
      )
    )
  )

# Exact values underlying Figure 2 (also exported below as a Word table).
figure2_probability_table <- multinomial_predicted_probs %>%
  dplyr::transmute(
    Frame = as.character(Frame),
    Outcome = as.character(response.level),
    `Predicted probability` = round(estimate, 2),
    `95% CI lower` = round(conf.low, 2),
    `95% CI upper` = round(conf.high, 2)
  )

utils::write.csv(
  figure2_probability_table,
  "build/table_figure2_predicted_probabilities.csv",
  row.names = FALSE
)

multinomial_dodge <- position_dodge(width = 0.3)

pred_plot_multinomial <- ggplot(
  multinomial_predicted_probs,
  aes(
    x = Frame,
    y = estimate,
    color = response.level,
    group = response.level
  )
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = multinomial_dodge
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Reject public funding" = "red",
      "Conditional funding (if price is reduced)" = "goldenrod2",
      "Unconditional public funding" = "green",
      "I don't know" = "steelblue4"
    ),
    breaks = c(
      "Reject public funding",
      "Conditional funding (if price is reduced)",
      "Unconditional public funding",
      "I don't know"
    ),
    labels = function(x) stringr::str_wrap(x, width = 70)
  ) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.box.spacing = unit(0.3, "cm"),
    aspect.ratio = 1,
    panel.border = element_rect(fill = NA, linewidth = 0.8)
  ) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(x = "Frame", y = "Predicted probabilities")

print(pred_plot_multinomial)

ggsave(
  "build/figure2_multinomial.pdf",
  plot = pred_plot_multinomial,
  width = 7.5,
  height = 5.5
)

# Word-ready numerical table supporting Figure 2.
# Running the Figure 2 section therefore produces both the figure and the
# table of exact predicted probabilities requested by the reviewer.
p_load(flextable, officer)

figure2_probability_word_data <- figure2_probability_table %>%
  dplyr::transmute(
    Frame,
    Outcome,
    `Predicted probability` = sprintf("%.2f", `Predicted probability`),
    `95% confidence interval` = sprintf(
      "[%.2f, %.2f]",
      `95% CI lower`,
      `95% CI upper`
    )
  )

figure2_probability_word <- flextable::flextable(
  figure2_probability_word_data
) %>%
  flextable::theme_booktabs() %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 8.5, part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::align(j = 3:4, align = "center", part = "all") %>%
  flextable::valign(valign = "center", part = "all") %>%
  flextable::padding(padding = 3, part = "all") %>%
  flextable::width(j = 1, width = 1.8) %>%
  flextable::width(j = 2, width = 2.7) %>%
  flextable::width(j = 3, width = 1.0) %>%
  flextable::width(j = 4, width = 1.2) %>%
  flextable::set_table_properties(
    layout = "fixed",
    opts_word = list(split = FALSE, repeat_headers = TRUE)
  )

flextable::save_as_docx(
  values = list(figure2_probability_word),
  path = "build/table_figure2_predicted_probabilities.docx",
  align = "center"
)

# Sensitivity figure: full-sample multinomial model versus the ordered-logit
# model that excludes "I don't know" responses. For a like-for-like comparison,
# multinomial probabilities for the three funding outcomes are conditional on a
# substantive response. The "I don't know" panel retains its full-sample
# probability and therefore contains only the multinomial estimate.
ordinal_comparison_raw <- marginaleffects::predictions(
  model,
  newdata = multinomial_prediction_grid,
  conf_level = 0.95
)

multinomial_comparison_raw <- marginaleffects::predictions(
  model.multinomial,
  newdata = multinomial_prediction_grid,
  conf_level = 0.95
)

# Delta-method transformation of P(Y = k) into
# P(Y = k | Y != "I don't know") for the three substantive outcomes.
multinomial_comparison_df <- as.data.frame(multinomial_comparison_raw)
multinomial_comparison_vcov <- vcov(multinomial_comparison_raw)
multinomial_comparison_jacobian <- matrix(
  0,
  nrow = nrow(multinomial_comparison_df),
  ncol = nrow(multinomial_comparison_df)
)
multinomial_comparison_estimate <- numeric(nrow(multinomial_comparison_df))

for (row_id in seq_len(nrow(multinomial_comparison_df))) {
  frame_id <- multinomial_comparison_df$Frame[row_id]
  dk_id <- which(
    multinomial_comparison_df$Frame == frame_id &
      as.character(multinomial_comparison_df$group) == "I don't know"
  )

  if (as.character(multinomial_comparison_df$group[row_id]) == "I don't know") {
    multinomial_comparison_estimate[row_id] <-
      multinomial_comparison_df$estimate[row_id]
    multinomial_comparison_jacobian[row_id, row_id] <- 1
  } else {
    substantive_probability <- 1 - multinomial_comparison_df$estimate[dk_id]
    multinomial_comparison_estimate[row_id] <-
      multinomial_comparison_df$estimate[row_id] / substantive_probability
    multinomial_comparison_jacobian[row_id, row_id] <-
      1 / substantive_probability
    multinomial_comparison_jacobian[row_id, dk_id] <-
      multinomial_comparison_df$estimate[row_id] /
      substantive_probability^2
  }
}

multinomial_comparison_transformed_vcov <-
  multinomial_comparison_jacobian %*%
  multinomial_comparison_vcov %*%
  t(multinomial_comparison_jacobian)

multinomial_comparison_se <- sqrt(
  pmax(diag(multinomial_comparison_transformed_vcov), 0)
)

comparison_outcome_levels <- c(
  "Reject public funding",
  "Conditional funding (if price is reduced)",
  "Unconditional public funding",
  "I don't know"
)

comparison_specification_levels <- c(
  "Multinomial logit (includes Don't know)",
  "Ordered logit (excludes Don't know)"
)

multinomial_comparison_probs <- multinomial_comparison_df %>%
  dplyr::transmute(
    Frame,
    response.level = factor(as.character(group),
                            levels = comparison_outcome_levels),
    estimate = multinomial_comparison_estimate,
    conf.low = pmax(0, estimate - qnorm(0.975) * multinomial_comparison_se),
    conf.high = pmin(1, estimate + qnorm(0.975) * multinomial_comparison_se),
    specification = "Multinomial logit (includes Don't know)"
  )

ordinal_comparison_probs <- as.data.frame(ordinal_comparison_raw) %>%
  dplyr::transmute(
    Frame,
    response.level = factor(as.character(group),
                            levels = comparison_outcome_levels),
    estimate,
    conf.low,
    conf.high,
    specification = "Ordered logit (excludes Don't know)"
  )

model_comparison_probs <- dplyr::bind_rows(
  multinomial_comparison_probs,
  ordinal_comparison_probs
) %>%
  dplyr::mutate(
    specification = factor(
      specification,
      levels = comparison_specification_levels
    )
  )

comparison_facet_labels <- c(
  "Reject public funding" = "Reject public funding",
  "Conditional funding (if price is reduced)" =
    "Conditional funding\n(if price is reduced)",
  "Unconditional public funding" = "Unconditional public funding",
  "I don't know" = "I don't know"
)

comparison_dodge <- position_dodge(width = 0.32)
comparison_probability_limit <- min(
  1,
  ceiling(max(model_comparison_probs$conf.high, na.rm = TRUE) * 20) / 20
)

pred_plot_model_comparison <- ggplot(
  model_comparison_probs,
  aes(
    x = Frame,
    y = estimate,
    color = response.level,
    shape = specification,
    group = specification
  )
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = comparison_dodge,
    linewidth = 0.45
  ) +
  facet_wrap(
    ~ response.level,
    ncol = 2,
    labeller = ggplot2::as_labeller(comparison_facet_labels)
  ) +
  scale_color_manual(
    values = c(
      "Reject public funding" = "red",
      "Conditional funding (if price is reduced)" = "goldenrod2",
      "Unconditional public funding" = "green",
      "I don't know" = "steelblue4"
    ),
    guide = "none"
  ) +
  scale_shape_manual(
    name = NULL,
    values = c(
      "Multinomial logit (includes Don't know)" = 16,
      "Ordered logit (excludes Don't know)" = 17
    )
  ) +
  scale_y_continuous(
    limits = c(0, comparison_probability_limit),
    breaks = seq(0, comparison_probability_limit, by = 0.10),
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.65, "cm"),
    legend.box.spacing = unit(0.25, "cm"),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, linewidth = 0.7)
  ) +
  guides(shape = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(x = "Frame", y = "Predicted probabilities")

print(pred_plot_model_comparison)

ggsave(
  "build/figure2_model_comparison.pdf",
  plot = pred_plot_model_comparison,
  width = 8.5,
  height = 6.2
)
ggsave(
  "build/figure2_model_comparison.png",
  plot = pred_plot_model_comparison,
  width = 8.5,
  height = 6.2,
  dpi = 300
)


# Table

# Full regression table requested by Reviewer 1: all frame coefficients,
# covariates, multinomial intercepts, ordinal thresholds, sample sizes, fit
# statistics, weighting details, and 95% confidence intervals.
z_95 <- qnorm(0.975)

# Multinomial coefficients are reported as relative risk ratios.
mn_tidy <- broom::tidy(model.multinomial) %>%
  dplyr::mutate(
    p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
    conf.low = estimate - z_95 * std.error,
    conf.high = estimate + z_95 * std.error,
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    term = paste0(y.level, ": ", term)
  ) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

model.multinomial.ms <- list(
  tidy = mn_tidy,
  glance = data.frame(
    `Num.Obs.` = nrow(model.frame(model.multinomial)),
    check.names = FALSE
  )
)
class(model.multinomial.ms) <- "modelsummary_list"

# Ordinal predictor coefficients are odds ratios. Thresholds are retained on
# their original logit scale because exponentiating cutpoints is not meaningful.
ord_tidy <- broom::tidy(model) %>%
  dplyr::mutate(
    is_threshold = coef.type == "scale",
    conf.low = estimate - z_95 * std.error,
    conf.high = estimate + z_95 * std.error,
    p.value = dplyr::if_else(
      is_threshold,
      NA_real_,
      2 * pnorm(abs(statistic), lower.tail = FALSE)
    ),
    estimate = dplyr::if_else(is_threshold, estimate, exp(estimate)),
    conf.low = dplyr::if_else(is_threshold, conf.low, exp(conf.low)),
    conf.high = dplyr::if_else(is_threshold, conf.high, exp(conf.high))
  ) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

model.ordinal.ms <- list(
  tidy = ord_tidy,
  glance = data.frame(
    `Num.Obs.` = nrow(model.frame(model)),
    check.names = FALSE
  )
)
class(model.ordinal.ms) <- "modelsummary_list"

# Human-readable labels for every coefficient and both ordinal thresholds.
coefficient_labels <- c(
  "(Intercept)" = "Intercept",
  "FrameLoss (rescue) frame" = "Loss (rescue) frame (ref: control)",
  "FrameGains (health maximisation) frame" =
    "Gains (health maximisation) frame (ref: control)",
  "M1_1Male" = "Male (ref: female)",
  "M1_2_1" = "Age (years)",
  "income_group3001-5000 €" = "Income: 3001-5000 € (ref: up to 3000 €)",
  "income_group5001+ €" = "Income: 5001+ € (ref: up to 3000 €)",
  "income_groupMissing/No answer" =
    "Income: missing/no answer (ref: up to 3000 €)",
  "M1_9Southern Finland" = "Region: Southern Finland (ref: Helsinki-Uusimaa)",
  "M1_9Western Finland" = "Region: Western Finland (ref: Helsinki-Uusimaa)",
  "M1_9Northern and Eastern Finland" =
    "Region: Northern and Eastern Finland (ref: Helsinki-Uusimaa)",
  "M2_5Yes" = "Eligible for Kela reimbursement: yes (ref: no)",
  "M2_11300-599 €" = "Medicine expenditure: 300-599 € (ref: 100-299 €)",
  "M2_11600 € or more" =
    "Medicine expenditure: 600 € or more (ref: 100-299 €)",
  "M2_11All 100 €" = "Medicine expenditure: up to 100 € (ref: 100-299 €)",
  "M2_11I do not take medicines prescribed by my doctor" =
    "Medicine expenditure: no prescribed medicines (ref: 100-299 €)",
  "M2_11I don't know" =
    "Medicine expenditure: don't know (ref: 100-299 €)",
  "Reject public funding|Conditional funding (if price is reduced)" =
    "Threshold: reject | conditional funding",
  "Conditional funding (if price is reduced)|Unconditional public funding" =
    "Threshold: conditional | unconditional funding"
)

multinomial_outcome_labels <- c(
  "Conditional funding (if price is reduced)" = "Conditional vs. reject",
  "Unconditional public funding" = "Unconditional vs. reject",
  "I don't know" = "Don't know vs. reject"
)

mn_coef_map <- broom::tidy(model.multinomial) %>%
  dplyr::transmute(
    term_key = paste0(y.level, ": ", term),
    term_label = paste0(
      unname(multinomial_outcome_labels[y.level]),
      ": ",
      dplyr::coalesce(unname(coefficient_labels[term]), term)
    )
  )

ord_coef_map <- broom::tidy(model) %>%
  dplyr::transmute(
    term_key = term,
    term_label = dplyr::coalesce(unname(coefficient_labels[term]), term)
  )

# Multinomial rows appear first because it is the main specification; the
# ordered-logit sensitivity estimates and thresholds follow.
coef_map <- c(
  stats::setNames(mn_coef_map$term_label, mn_coef_map$term_key),
  stats::setNames(ord_coef_map$term_label, ord_coef_map$term_key)
)

# Fit statistics and weighting information displayed in both output formats.
n_obs_ord <- nrow(model.frame(model))
n_obs_mn <- nrow(model.frame(model.multinomial))

add_gof <- data.frame(
  term = c("Num.Obs.", "Log.Lik.", "AIC", "BIC", "Survey weights"),
  `Multinomial logit` = c(
    format(n_obs_mn, big.mark = ",", scientific = FALSE, trim = TRUE),
    sprintf("%.2f", as.numeric(logLik(model.multinomial))),
    sprintf("%.2f", AIC(model.multinomial)),
    sprintf("%.2f", BIC(model.multinomial)),
    "Yes (PAINOKERROIN)"
  ),
  `Ordinal logit` = c(
    format(n_obs_ord, big.mark = ",", scientific = FALSE, trim = TRUE),
    sprintf("%.2f", as.numeric(logLik(model))),
    sprintf("%.2f", AIC(model)),
    sprintf("%.2f", BIC(model)),
    "Yes (PAINOKERROIN)"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

tab_tex <- modelsummary::msummary(
  list(
    "Multinomial logit" = model.multinomial.ms,
    "Ordinal logit" = model.ordinal.ms
  ),
  coef_map = coef_map,
  statistic = "conf.int",
  stars = TRUE,
  gof_omit = "Num\\.Obs\\.|Observations|AIC|BIC|Log\\.Lik|RMSE|F|R2|Adj|Within|Between|Std\\.Errors",
  add_rows = add_gof,
  output = "latex_tabular"
)

writeLines(enc2utf8(as.character(tab_tex)), "build/table_model.tex", useBytes = TRUE)
## ----

p_load(flextable, officer)

format_model_cell <- function(estimate, conf.low, conf.high, p.value) {
  stars <- dplyr::case_when(
    is.na(p.value) ~ "",
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.10 ~ "+",
    TRUE ~ ""
  )
  sprintf(
    "%.3f%s\n[%.3f, %.3f]",
    estimate,
    stars,
    conf.low,
    conf.high
  )
}

mn_word_cells <- mn_tidy %>%
  dplyr::transmute(
    term_key = term,
    `Multinomial logit` = format_model_cell(
      estimate,
      conf.low,
      conf.high,
      p.value
    )
  )

ord_word_cells <- ord_tidy %>%
  dplyr::transmute(
    term_key = term,
    `Ordinal logit` = format_model_cell(
      estimate,
      conf.low,
      conf.high,
      p.value
    )
  )

table_model_word_data <- data.frame(
  term_key = names(coef_map),
  Parameter = unname(coef_map),
  stringsAsFactors = FALSE
) %>%
  dplyr::left_join(mn_word_cells, by = "term_key") %>%
  dplyr::left_join(ord_word_cells, by = "term_key") %>%
  dplyr::mutate(
    `Multinomial logit` = dplyr::coalesce(`Multinomial logit`, ""),
    `Ordinal logit` = dplyr::coalesce(`Ordinal logit`, "")
  ) %>%
  dplyr::select(Parameter, `Multinomial logit`, `Ordinal logit`)

table_model_word_data <- dplyr::bind_rows(
  table_model_word_data,
  data.frame(
    Parameter = add_gof$term,
    `Multinomial logit` = add_gof$`Multinomial logit`,
    `Ordinal logit` = add_gof$`Ordinal logit`,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
)

tab_word <- flextable::flextable(table_model_word_data)

tab_word <- tab_word %>%
  flextable::theme_booktabs() %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 8, part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::align(j = 2:3, align = "center", part = "all") %>%
  flextable::valign(valign = "center", part = "all") %>%
  flextable::padding(padding = 2, part = "all") %>%
  flextable::width(j = 1, width = 6.2) %>%
  flextable::width(j = 2:3, width = 1.7) %>%
  flextable::set_table_properties(
    layout = "fixed",
    opts_word = list(split = FALSE, repeat_headers = TRUE)
  )

table_model_section <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape"),
  page_margins = officer::page_mar(
    top = 0.5,
    bottom = 0.5,
    left = 0.5,
    right = 0.5
  )
)

flextable::save_as_docx(
  values = list(tab_word),
  path = "build/table_model.docx",
  pr_section = table_model_section,
  align = "center"
)

## ---- summary_table
p_load(dplyr, modelsummary, tidyr, tibble)

dir.create("build", showWarnings = FALSE, recursive = TRUE)

sum_vars <- c("Frame", "M1_1", "M1_2_1", "M1_10", "M1_9", "M2_5", "M2_11")

# Estimation-relevant sample (complete cases on listed vars + outcome + weights)
dat_s <- dat %>%
  dplyr::select(dplyr::all_of(c("PAINOKERROIN", "outcome", sum_vars))) %>%
  dplyr::filter(!is.na(outcome)) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(sum_vars), ~ !is.na(.))) %>%
  dplyr::mutate(
    PAINOKERROIN = suppressWarnings(as.numeric(PAINOKERROIN)),
    M1_2_1       = suppressWarnings(as.numeric(M1_2_1))
  ) %>%
  dplyr::filter(is.finite(PAINOKERROIN), PAINOKERROIN > 0, is.finite(M1_2_1))

# weights (numeric, already filtered)
w <- dat_s$PAINOKERROIN

# weighted mean/sd (numeric-safe)
w_mean <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  sum(w * x) / sum(w)
}

w_sd <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  mu <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}

# Age row (numeric) — keep column types consistent for bind_rows()
age_tbl <- tibble::tibble(
  Variable = "M1_2_1",
  Level    = NA_character_,
  N        = length(dat_s$M1_2_1),
  Percent  = NA_real_,
  Mean     = round(w_mean(dat_s$M1_2_1, w), 2),
  SD       = round(w_sd(dat_s$M1_2_1, w), 2),
  Min      = suppressWarnings(min(dat_s$M1_2_1, na.rm = TRUE)),
  Max      = suppressWarnings(max(dat_s$M1_2_1, na.rm = TRUE))
)

# weighted frequency table for categorical vars
wfreq_tbl <- function(df, var, wvar = "PAINOKERROIN") {
  v <- df[[var]]
  if (!is.factor(v)) v <- factor(v)
  
  df2 <- df %>% dplyr::mutate(.v = v)
  tot_w <- sum(df2[[wvar]], na.rm = TRUE)
  
  df2 %>%
    dplyr::group_by(.v) %>%
    dplyr::summarise(
      N = dplyr::n(),
      wsum = sum(.data[[wvar]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Percent  = round(100 * wsum / tot_w, 2),
      Variable = var,
      Level    = as.character(.v),
      Mean = NA_real_,
      SD   = NA_real_,
      Min  = NA_real_,
      Max  = NA_real_
    ) %>%
    dplyr::select(Variable, Level, N, Percent, Mean, SD, Min, Max)
}

cat_vars <- c("Frame", "M1_1", "M1_10", "M1_9", "M2_5", "M2_11")
cat_tbl <- dplyr::bind_rows(lapply(cat_vars, function(v) wfreq_tbl(dat_s, v)))

# Combine (categoricals first, then age)
sum_df <- dplyr::bind_rows(cat_tbl, age_tbl) %>%
  dplyr::mutate(
    Variable = dplyr::recode(
      Variable,
      Frame = "Frame",
      M1_1  = "Gender (M1_1)",
      M1_10 = "Income (M1_10)",
      M1_9  = "Region (M1_9)",
      M2_5  = "Kela reimbursement eligibility (M2_5)",
      M2_11 = "Medicine spending (M2_11)",
      M1_2_1 = "Age"
    )
  )

# Export to LaTeX
tab_sum_tex <- modelsummary::datasummary_df(
  sum_df,
  title = "Summary statistics (Framing experiment)",
  notes = "Percentages are weighted using PAINOKERROIN; N is unweighted. Estimation sample: complete cases on listed variables.",
  output = "latex_tabular"
)

writeLines(
  enc2utf8(as.character(tab_sum_tex)),
  "/Users/hectorbahamonde/research/Fimea/build/table_summary.tex",
  useBytes = TRUE
)
## ----


## ---- table_1OA_unweighted

# Table 1OA: unweighted sensitivity models requested by Reviewer 1.
# These specifications use the same outcomes, covariates, and weight-eligible
# estimation sample as the weighted models, but PAINOKERROIN is not applied in
# model estimation.
pacman::p_load(
  MASS,
  nnet,
  modelsummary,
  broom,
  dplyr,
  tibble,
  flextable,
  officer
)

dat_unweighted <- dat %>%
  dplyr::mutate(
    PAINOKERROIN = suppressWarnings(as.numeric(PAINOKERROIN))
  ) %>%
  dplyr::filter(
    is.finite(PAINOKERROIN),
    PAINOKERROIN > 0
  )

model.multinomial.unweighted <- nnet::multinom(
  relevel(
    factor(outcome, ordered = FALSE),
    ref = "Reject public funding"
  ) ~ Frame + M1_1 + M1_2_1 + income_group + M1_9 + M2_5 + M2_11,
  data = dat_unweighted,
  Hess = TRUE,
  trace = FALSE
)

model.unweighted <- MASS::polr(
  outcome_ord ~
    Frame + M1_1 + M1_2_1 + income_group + M1_9 + M2_5 + M2_11,
  data = dat_unweighted,
  method = "logistic",
  Hess = TRUE
)

z_95_unweighted <- stats::qnorm(0.975)

# Multinomial coefficients are reported as relative risk ratios.
mn_unweighted_tidy <- broom::tidy(model.multinomial.unweighted) %>%
  dplyr::mutate(
    p.value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
    conf.low = estimate - z_95_unweighted * std.error,
    conf.high = estimate + z_95_unweighted * std.error,
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    term = paste0(y.level, ": ", term)
  ) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

model.multinomial.unweighted.ms <- list(
  tidy = mn_unweighted_tidy,
  glance = data.frame(
    `Num.Obs.` = nrow(model.frame(model.multinomial.unweighted)),
    check.names = FALSE
  )
)
class(model.multinomial.unweighted.ms) <- "modelsummary_list"

# Ordinal predictor coefficients are odds ratios; thresholds remain on the
# original logit scale.
ord_unweighted_tidy <- broom::tidy(model.unweighted) %>%
  dplyr::mutate(
    is_threshold = coef.type == "scale",
    conf.low = estimate - z_95_unweighted * std.error,
    conf.high = estimate + z_95_unweighted * std.error,
    p.value = dplyr::if_else(
      is_threshold,
      NA_real_,
      2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    ),
    estimate = dplyr::if_else(is_threshold, estimate, exp(estimate)),
    conf.low = dplyr::if_else(is_threshold, conf.low, exp(conf.low)),
    conf.high = dplyr::if_else(is_threshold, conf.high, exp(conf.high))
  ) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

model.ordinal.unweighted.ms <- list(
  tidy = ord_unweighted_tidy,
  glance = data.frame(
    `Num.Obs.` = nrow(model.frame(model.unweighted)),
    check.names = FALSE
  )
)
class(model.ordinal.unweighted.ms) <- "modelsummary_list"

# Use the same human-readable parameter labels and row order as Table 3.
mn_unweighted_coef_map <- broom::tidy(model.multinomial.unweighted) %>%
  dplyr::transmute(
    term_key = paste0(y.level, ": ", term),
    term_label = paste0(
      unname(multinomial_outcome_labels[y.level]),
      ": ",
      dplyr::coalesce(unname(coefficient_labels[term]), term)
    )
  )

ord_unweighted_coef_map <- broom::tidy(model.unweighted) %>%
  dplyr::transmute(
    term_key = term,
    term_label = dplyr::coalesce(
      unname(coefficient_labels[term]),
      term
    )
  )

coef_map_unweighted <- c(
  stats::setNames(
    mn_unweighted_coef_map$term_label,
    mn_unweighted_coef_map$term_key
  ),
  stats::setNames(
    ord_unweighted_coef_map$term_label,
    ord_unweighted_coef_map$term_key
  )
)

n_obs_mn_unweighted <- nrow(model.frame(model.multinomial.unweighted))
n_obs_ord_unweighted <- nrow(model.frame(model.unweighted))

add_gof_unweighted <- data.frame(
  term = c("Num.Obs.", "Log.Lik.", "AIC", "BIC", "Survey weights"),
  `Multinomial logit` = c(
    format(
      n_obs_mn_unweighted,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    sprintf("%.2f", as.numeric(logLik(model.multinomial.unweighted))),
    sprintf("%.2f", AIC(model.multinomial.unweighted)),
    sprintf("%.2f", BIC(model.multinomial.unweighted)),
    "No (unweighted)"
  ),
  `Ordinal logit` = c(
    format(
      n_obs_ord_unweighted,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    sprintf("%.2f", as.numeric(logLik(model.unweighted))),
    sprintf("%.2f", AIC(model.unweighted)),
    sprintf("%.2f", BIC(model.unweighted)),
    "No (unweighted)"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# LaTeX output for the Online Appendix.
table_1OA_tex <- modelsummary::msummary(
  list(
    "Multinomial logit" = model.multinomial.unweighted.ms,
    "Ordinal logit" = model.ordinal.unweighted.ms
  ),
  coef_map = coef_map_unweighted,
  statistic = "conf.int",
  stars = TRUE,
  gof_omit = paste0(
    "Num\\.Obs\\.|Observations|AIC|BIC|Log\\.Lik|RMSE|F|R2|Adj|",
    "Within|Between|Std\\.Errors"
  ),
  add_rows = add_gof_unweighted,
  output = "latex_tabular"
)

writeLines(
  enc2utf8(as.character(table_1OA_tex)),
  "build/table_1OA_unweighted.tex",
  useBytes = TRUE
)

# Word output: estimates and 95% confidence intervals appear in the same cell
# so individual parameter rows cannot split across pages.
format_model_cell_1OA <- function(
  estimate,
  conf.low,
  conf.high,
  p.value
) {
  stars <- dplyr::case_when(
    is.na(p.value) ~ "",
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.10 ~ "+",
    TRUE ~ ""
  )
  sprintf(
    "%.3f%s\n[%.3f, %.3f]",
    estimate,
    stars,
    conf.low,
    conf.high
  )
}

mn_unweighted_word_cells <- mn_unweighted_tidy %>%
  dplyr::transmute(
    term_key = term,
    `Multinomial logit` = format_model_cell_1OA(
      estimate,
      conf.low,
      conf.high,
      p.value
    )
  )

ord_unweighted_word_cells <- ord_unweighted_tidy %>%
  dplyr::transmute(
    term_key = term,
    `Ordinal logit` = format_model_cell_1OA(
      estimate,
      conf.low,
      conf.high,
      p.value
    )
  )

table_1OA_word_data <- data.frame(
  term_key = names(coef_map_unweighted),
  Parameter = unname(coef_map_unweighted),
  stringsAsFactors = FALSE
) %>%
  dplyr::left_join(mn_unweighted_word_cells, by = "term_key") %>%
  dplyr::left_join(ord_unweighted_word_cells, by = "term_key") %>%
  dplyr::mutate(
    `Multinomial logit` = dplyr::coalesce(`Multinomial logit`, ""),
    `Ordinal logit` = dplyr::coalesce(`Ordinal logit`, "")
  ) %>%
  dplyr::select(Parameter, `Multinomial logit`, `Ordinal logit`)

table_1OA_word_data <- dplyr::bind_rows(
  table_1OA_word_data,
  data.frame(
    Parameter = add_gof_unweighted$term,
    `Multinomial logit` = add_gof_unweighted$`Multinomial logit`,
    `Ordinal logit` = add_gof_unweighted$`Ordinal logit`,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
)

table_1OA_word <- flextable::flextable(table_1OA_word_data) %>%
  flextable::theme_booktabs() %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 8, part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::align(j = 2:3, align = "center", part = "all") %>%
  flextable::valign(valign = "center", part = "all") %>%
  flextable::padding(padding = 2, part = "all") %>%
  flextable::width(j = 1, width = 6.2) %>%
  flextable::width(j = 2:3, width = 1.7) %>%
  flextable::set_table_properties(
    layout = "fixed",
    opts_word = list(split = FALSE, repeat_headers = TRUE)
  )

table_1OA_section <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape"),
  page_margins = officer::page_mar(
    top = 0.5,
    bottom = 0.5,
    left = 0.5,
    right = 0.5
  )
)

flextable::save_as_docx(
  values = list(table_1OA_word),
  path = "build/table_1OA_unweighted.docx",
  pr_section = table_1OA_section,
  align = "center"
)


## ---- balance_plot

pacman::p_load(dplyr, tidyr, forcats, ggplot2, patchwork, stringr, tibble)

# IMPORTANT: do NOT reload dat.RData here; use the already-recoded `dat`
# If you insist on reloading, you must re-run the same recodes again.

dat_plot <- dat %>%
  dplyr::mutate(
    PAINOKERROIN = suppressWarnings(as.numeric(PAINOKERROIN)),
    M1_2_1       = suppressWarnings(as.numeric(M1_2_1)),
    FramePlot    = factor(Frame, levels = levels(Frame))  # Frame already recoded above
  ) %>%
  dplyr::filter(
    is.finite(PAINOKERROIN), PAINOKERROIN > 0,
    !is.na(FramePlot)
  ) %>%
  droplevels()

# Use your NEW variables
vars_cat <- c("M1_1", "income_group", "M1_9", "M2_5", "M2_11")

panel_map <- c(
  M1_1         = "Gender",
  income_group = "Income",
  M1_9         = "Region",
  M2_5         = "Kela reimbursement eligibility",
  M2_11        = "Medicine spending"
)

frame_levels <- levels(dat_plot$FramePlot)

base_small <- 6.6

shrink_theme <- ggplot2::theme_minimal(base_size = base_small) +
  ggplot2::theme(
    plot.title   = ggplot2::element_text(size = base_small * 1.15),
    axis.text    = ggplot2::element_text(size = base_small),
    axis.title   = ggplot2::element_text(size = base_small),
    legend.text  = ggplot2::element_text(size = base_small),
    legend.title = ggplot2::element_text(size = base_small)
  )

scale_fill_frames <- ggplot2::scale_fill_discrete(drop = FALSE, name = NULL)

age_df <- dat_plot %>%
  dplyr::filter(is.finite(M1_2_1)) %>%
  dplyr::mutate(FramePlot = factor(FramePlot, levels = frame_levels)) %>%
  dplyr::select(FramePlot, M1_2_1, PAINOKERROIN)

p_age <- ggplot2::ggplot(
  age_df,
  ggplot2::aes(x = M1_2_1, weight = PAINOKERROIN, fill = FramePlot)
) +
  ggplot2::geom_density(alpha = 0.25, linewidth = 0, adjust = 1) +
  ggplot2::geom_density(
    ggplot2::aes(color = FramePlot),
    fill = NA, linewidth = 0.7, adjust = 1,
    show.legend = FALSE
  ) +
  ggplot2::labs(title = "Age (weighted density)", x = NULL, y = "Density") +
  ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3)) +
  scale_fill_frames +
  shrink_theme +
  ggplot2::theme(
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 5.5),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 4))
  )

wprop_by_frame <- function(df, var, wvar = "PAINOKERROIN") {
  df %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    dplyr::mutate(
      FramePlot = factor(FramePlot, levels = frame_levels),
      Level = as.character(.data[[var]])
    ) %>%
    dplyr::group_by(FramePlot, Level) %>%
    dplyr::summarise(wsum = sum(.data[[wvar]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(FramePlot) %>%
    dplyr::mutate(value = 100 * wsum / sum(wsum)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Panel     = unname(panel_map[var]),
      FramePlot = FramePlot,
      Level     = Level,
      value     = as.numeric(value)
    )
}

cat_df <- dplyr::bind_rows(lapply(vars_cat, function(v) wprop_by_frame(dat_plot, v)))

make_panel <- function(panel, wrap_width = 28) {
  dfp <- cat_df %>% dplyr::filter(Panel == panel)
  
  ggplot2::ggplot(dfp, ggplot2::aes(y = forcats::fct_rev(Level), x = value, fill = FramePlot)) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.7,
      show.legend = FALSE
    ) +
    ggplot2::labs(title = panel, x = NULL, y = NULL) +
    ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, wrap_width)) +
    scale_fill_frames +
    shrink_theme +
    ggplot2::theme(legend.position = "none")
}

p_gender <- make_panel("Gender", 22)
p_income <- make_panel("Income", 28)
p_kela   <- make_panel("Kela reimbursement eligibility", 30)
p_spend  <- make_panel("Medicine spending", 30)
p_region <- make_panel("Region", 28)

sum_plot <- (p_gender | p_income) /
  (p_kela   | p_spend)  /
  (p_region)            /
  (patchwork::free(p_age, side = "l"))

## ----


## Revise and Resubmit

## ---- raw_outcome_distribution_by_frame ----


# Plot that shows distribution of answers, includes DK's, by treatment arm.
pacman::p_load(dplyr, tidyr, forcats, ggplot2, stringr, scales, tibble)

outcome_by_frame <- dat %>%
  dplyr::filter(!is.na(Frame), !is.na(outcome)) %>%
  dplyr::mutate(
    Frame = factor(Frame, levels = levels(dat$Frame)),
    outcome = factor(outcome, levels = levels(dat$outcome), ordered = FALSE)
  ) %>%
  dplyr::count(Frame, outcome, .drop = FALSE, name = "n") %>%
  dplyr::group_by(Frame) %>%
  dplyr::mutate(
    frame_n = sum(n),
    percent = 100 * n / frame_n
  ) %>%
  dplyr::ungroup()

outcome_dodge <- ggplot2::position_dodge(width = 0.82)

raw_outcome_plot <- ggplot2::ggplot(
  outcome_by_frame,
  ggplot2::aes(x = outcome, y = percent, fill = Frame)
) +
  ggplot2::geom_col(
    position = outcome_dodge,
    width = 0.74,
    color = "white",
    linewidth = 0.25
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = percent + 1,
      label = sprintf("%.1f%% (n=%d)", percent, n)
    ),
    position = outcome_dodge,
    hjust = 0,
    size = 3
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 34),
    drop = FALSE
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 60, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = ggplot2::expansion(mult = c(0, 0.22))
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Control" = "#7F7F7F",
      "Loss (rescue) frame" = "#D55E00",
      "Gains (health maximisation) frame" = "#0072B2"
    ),
    drop = FALSE,
    name = NULL
  ) +
  ggplot2::labs(
    title = "Raw outcome distribution by experimental arm",
    x = NULL,
    y = "Respondents within experimental arm",
    caption = stringr::str_wrap(
      paste0(
        "Unweighted percentages; raw n shown in parentheses. "),
      width = 110
    )
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9),
    plot.caption = ggplot2::element_text(hjust = 0, size = 7.5, lineheight = 1.05),
    axis.text.y = ggplot2::element_text(size = 9),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 8),
    plot.margin = ggplot2::margin(8, 42, 8, 8)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

print(raw_outcome_plot)

dir.create("build", showWarnings = FALSE, recursive = TRUE)

ggplot2::ggsave(
  filename = "build/figure_raw_outcome_by_frame.pdf",
  plot = raw_outcome_plot,
  width = 8.2,
  height = 6,
  units = "in",
  device = grDevices::cairo_pdf
)

ggsave(
  "build/figure_raw_outcome_by_frame.png",
  plot = raw_outcome_plot,
  width = 8.2,
  height = 6,
  dpi = 300
)


## ----



################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
abstract.c = as.character(c("Governments often insulate costly and controversial healthcare funding decisions from day-to-day electoral politics by delegating authority to technical bodies that rely on formal assessments and objective criteria. Yet public responses to these decisions may still depend on how identical choices are presented. This article examines whether institutional insulation constrains framing effects on mass preferences, or whether citizens remain sensitive to framing even when authority is formally insulated from electoral politics. Drawing on prospect theory, we argue that preferences over high-cost cancer medicines are reference-point dependent: citizens respond differently when decisions are framed as avoiding salient losses rather than preserving existing gains. We test this argument with a population-based survey experiment embedded in the 2021 Finnish Medicines Barometer. Respondents evaluated an identical clinical vignette about a novel, high-cost cancer medicine with uncertain benefits and were randomly assigned to alternative frames emphasizing either last-resort rescue or stewardship of finite collective resources. Holding clinical evidence, costs, and uncertainty constant, we find substantial and systematic framing effects. Support for funding rises under loss-avoidance framing and falls under gain-preservation framing, despite identical policy content. The findings suggest that institutional insulation does not neutralize framing effects. Technical justification alone does not depoliticize distributive choices in healthcare."))
writeLines(abstract.c, fileConn)
close(fileConn)
## ----




## ---- abstract.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----
