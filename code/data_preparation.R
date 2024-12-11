gusto_url <- "https://github.com/resplab/predtools/raw/refs/heads/master/data/gusto.rda"
load(url(gusto_url))
gusto |>
  dplyr::filter(!is.na(tpa)) |>
  dplyr::rename(
    "outcome" = "day30",
    "treatment" = "tpa"
  ) |>
  dplyr::select(outcome, treatment, sex, Killip, age,sysbp, pulse, pmi, miloc) |>
  readr::write_csv("data/gusto.csv")
