set.seed(19910930)
gusto <- readr::read_csv("data/gusto.csv")

model <- glm(
  formula = outcome ~ treatment,
  family = "binomial",
  data = gusto
)

# odds_ratio <- exp(model$coefficients["treatment"])
# confidence_intervals <- exp(confint(model))

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

c(
  rct = odds_ratio_rct,
  confounding = odds_ratio_with_sex_confounder,
  adjusted = odds_ratio_with_sex_adjustment
)
