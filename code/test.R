set.seed(19910930)
gusto <- readr::read_csv(
  file = "data/gusto.csv",
  show_col_types = FALSE
)

model <- glm(
  formula = outcome ~ treatment,
  family = "binomial",
  data = gusto
)

gusto_with_sex_confounder <- gusto |>
  dplyr::mutate(
    linear_predictor_treatment = log(0.987) + log(.4) * sex_male,
    new_treatment = rbinom(nrow(gusto), 1, plogis(linear_predictor_treatment)),
    linear_predictor_outcome = log(.146) + log(.4) * sex_male - 0.1586183 * new_treatment,
    new_outcome = rbinom(nrow(gusto), 1, plogis(linear_predictor_outcome))
  )

model_rct <- glm(
  formula = outcome ~ treatment,
  family = "binomial",
  data = gusto_with_sex_confounder
)

model_with_sex_confounder <- glm(
  formula = new_outcome ~ new_treatment,
  family = "binomial",
  data = gusto_with_sex_confounder
)

model_with_sex_adjustment <- glm(
  formula = new_outcome ~ new_treatment + sex_male,
  family = "binomial",
  data = gusto_with_sex_confounder
)

odds_ratio_rct <- exp(model_rct$coefficients["treatment"])
odds_ratio_with_sex_confounder <- exp(model_with_sex_confounder$coefficients["new_treatment"])
odds_ratio_with_sex_adjustment <- exp(model_with_sex_adjustment$coefficients["new_treatment"])
#
# c(
#   rct = odds_ratio_rct,
#   confounding = odds_ratio_with_sex_confounder,
#   adjusted = odds_ratio_with_sex_adjustment
# )


gusto_with_multiple_confounders <- gusto |>
  dplyr::mutate(
    linear_predictor_treatment = -.34 + log(.4) * sex_male + log(.9) * (age - mean(age)) + log(1.05) * (sysbp - mean(sysbp)),
    new_treatment = rbinom(nrow(gusto), 1, plogis(linear_predictor_treatment)),
    linear_predictor_outcome = -3 + log(.4) * sex_male + log(1.1) * (age - mean(age)) + log(1.05) * (sysbp - mean(sysbp)) - 0.1586183 * new_treatment,
    new_outcome = rbinom(nrow(gusto), 1, plogis(linear_predictor_outcome))
  )

c(mean(gusto_with_multiple_confounders$new_outcome), mean(gusto_with_multiple_confounders$outcome))

model_unadjusted_multiple_confounders <- glm(
  new_outcome ~ new_treatment,
  data = gusto_with_multiple_confounders,
  family = "binomial"
)

matching_on_ps <- MatchIt::matchit(
  new_treatment ~ age + sex_male + sysbp,
  data = gusto_with_multiple_confounders,
  method = "nearest",
  caliper = .2
)

matched_sample <- MatchIt::match.data(matching_on_ps)

model_adjusted_multiple_confounders <- glm(
  new_outcome ~ new_treatment,
  data = matched_sample,
  family = "binomial"
)
