set.seed(19910930)
gusto <- readr::read_csv( file = "data/gusto.csv", show_col_types = FALSE )

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

# ----

matching_on_ps_missing <- MatchIt::matchit(
  new_treatment ~ age + sex_male,
  data = gusto_with_multiple_confounders,
  method = "nearest",
  caliper = .2
)

matched_sample_missing <- MatchIt::match.data(matching_on_ps_missing)

model_adjusted_multiple_confounders_missing <- glm(
  new_outcome ~ new_treatment,
  data = matched_sample_missing,
  family = "binomial"
)

if (!file.exists("data/negative_controls.csv")) {

  log_odds_ratio_missing <- se_log_odds_ratio_missing <- rep(0, 200)
  log_odds_ratio <- se_log_odds_ratio <- rep(0, 200)

  for (i in 1:200) {

    gusto_with_negative_control <- gusto_with_multiple_confounders |>
      dplyr::mutate(
        linear_predictor_of_negative_control = runif(1, -5, -1) +
          runif(1, -.4, .4) * sex_male +
          runif(1, -.1, .1) * (age - mean(age)) +
          runif(1, -.1, .1) * (sysbp - mean(sysbp)),
        negative_control := rbinom(
          n = nrow(gusto_with_multiple_confounders),
          size = 1,
          prob = plogis(linear_predictor_of_negative_control)
        )
      )

    matching_on_ps_negative_control <- MatchIt::matchit(
      new_treatment ~ sex_male + age + sysbp,
      data = gusto_with_negative_control,
      method = "nearest",
      caliper = .2
    )

    matching_on_ps_negative_control_missing <- MatchIt::matchit(
      new_treatment ~ sex_male + age,
      data = gusto_with_negative_control,
      method = "nearest",
      caliper = .2
    )

    matched_sample_negative_control <- MatchIt::match.data(matching_on_ps_negative_control)
    matched_sample_negative_control_missing <- MatchIt::match.data(matching_on_ps_negative_control_missing)

    model_negative_control_missing <- glm(
      negative_control ~ new_treatment,
      data = matched_sample_negative_control_missing,
      family = "binomial"
    )

    model_negative_control <- glm(
      negative_control ~ new_treatment,
      data = matched_sample_negative_control,
      family = "binomial"
    )

    log_odds_ratio[i] <- model_negative_control$coefficients[2]
    se_log_odds_ratio[i] <- sqrt(diag(vcov(model_negative_control)))[2]
    log_odds_ratio_missing[i] <- model_negative_control_missing$coefficients[2]
    se_log_odds_ratio_missing[i] <- sqrt(diag(vcov(model_negative_control_missing)))[2]

  }

  readr::write_csv(
    x = data.frame(
      log_odds_ratio = log_odds_ratio,
      se_log_odds_ratio = se_log_odds_ratio,
      log_odds_ratio_missing = log_odds_ratio_missing,
      se_log_odds_ratio_missing = se_log_odds_ratio_missing
    ),
    file = "data/negative_controls.csv"
  )
}

