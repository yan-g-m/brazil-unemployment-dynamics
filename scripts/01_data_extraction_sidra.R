# ==============================================================================
# Objetivo: Coletar e tratar dados da PNAD Contínua via API SIDRA do IBGE:
#           1. Série Mensal por Trimestres Móveis (Brasil Agregado) - Tabela 6381
#           2. Série Trimestral Fechada (27 Unidades da Federação) - Tabela 4099
# Autor: Yan Galante Maestre
# Data: 2026-10-27
# Dependências: pacman, sidrar, tidyverse, lubridate, janitor, here
# ==============================================================================

pacman::p_load(sidrar, tidyverse, lubridate, janitor, here)

# ------------------------------------------------------------------------------
# 1. Coleta da Série Mensal (Trimestres Móveis) - Brasil Agregado
# Intuição econômica: A série mensalizada oferece maior densidade temporal 
# (~170 observações) para modelar a dinâmica temporal fina e o ajuste X-13.
# ------------------------------------------------------------------------------
message("Coletando série mensal por trimestres móveis (Brasil)...")

df_br_raw <- sidrar::get_sidra(
  x = 6381,
  variable = 4099,
  period = "all",
  geo = "Brazil"
)

# Salva o dado bruto para auditoria e reprodutibilidade
readr::write_rds(
  df_br_raw,
  file = here::here("data", "raw", "sidra_br_monthly_raw.rds")
)

df_brazil_monthly <- df_br_raw |>
  janitor::clean_names() |>
  dplyr::select(
    period_code = trimestre_movel_codigo,
    unemployment_rate = valor
  ) |>
  dplyr::mutate(
    year = as.integer(stringr::str_sub(period_code, 1, 4)),
    month = as.integer(stringr::str_sub(period_code, 5, 6)),
    date = lubridate::make_date(year = year, month = month, day = 1),
    unemployment_rate = as.numeric(unemployment_rate)
  ) |>
  dplyr::filter(!is.na(unemployment_rate)) |>
  dplyr::arrange(date) |>
  dplyr::select(date, year, month, unemployment_rate)

# Salva a série tratada
readr::write_rds(
  df_brazil_monthly,
  file = here::here("data", "processed", "brazil_monthly_unemployment.rds")
)
message("Série mensal do Brasil processada e salva em data/processed/.")

# ------------------------------------------------------------------------------
# 2. Coleta da Série Trimestral Fechada - 27 Unidades da Federação (UFs)
# Intuição econômica: A expansão amostral do IBGE só permite inferência 
# estatisticamente robusta nos estados para os trimestres fechados.
# ------------------------------------------------------------------------------
message("Coletando séries trimestrais fechadas por UF...")

df_states_raw <- sidrar::get_sidra(
  x = 4099,
  variable = 4099,
  period = "all",
  geo = "State"
)

# Salva o dado bruto regional
readr::write_rds(
  df_states_raw,
  file = here::here("data", "raw", "sidra_states_quarterly_raw.rds")
)

df_states_quarterly <- df_states_raw |>
  janitor::clean_names() |>
  dplyr::select(
    state_code = unidade_da_federacao_codigo,
    state_name = unidade_da_federacao,
    quarter_code = trimestre_codigo,
    unemployment_rate = valor
  ) |>
  dplyr::mutate(
    year = as.integer(stringr::str_sub(quarter_code, 1, 4)),
    quarter = as.integer(stringr::str_sub(quarter_code, 5, 6)),
    month = quarter * 3,
    date = lubridate::make_date(year = year, month = month, day = 1),
    unemployment_rate = as.numeric(unemployment_rate),
    state_code = as.character(state_code),
    state_name = as.character(state_name)
  ) |>
  dplyr::filter(!is.na(unemployment_rate)) |>
  dplyr::arrange(state_code, date) |>
  dplyr::select(state_code, state_name, date, year, quarter, unemployment_rate)

# Salva as séries estaduais tratadas
readr::write_rds(
  df_states_quarterly,
  file = here::here("data", "processed", "states_quarterly_unemployment.rds")
)
message("Séries trimestrais por UF processadas e salvas em data/processed/.")