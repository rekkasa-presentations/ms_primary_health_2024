if (!file.exists("data/gusto.csv")) {
  gusto_url <- "https://github.com/resplab/predtools/raw/refs/heads/master/data/gusto.rda"
  load(url(gusto_url))
  gusto |>
    dplyr::filter(!is.na(tpa)) |>
    dplyr::mutate(
      sex_male = ifelse(sex == "male", 1, 0)
    ) |>
    dplyr::rename(
      "outcome" = "day30",
      "treatment" = "tpa"
    ) |>
    dplyr::select(outcome, treatment, sex_male, Killip, age,sysbp, pulse, pmi, miloc) |>
    readr::write_csv("data/gusto.csv")
}

