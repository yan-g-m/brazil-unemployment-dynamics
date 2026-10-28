# ==============================================================================
# Objetivo: Análise regional da taxa de desemprego das 27 UFs (PNADC Trimestral)
#           - Sumário descritivo transversal e ranking das 27 UFs
#           - Seleção empírica de 4 casos representativos de heterogeneidade
#           - Decomposição sazonal em painel via STL
#           - Matriz de composição setorial contextual (indústria, serviços, agro)
#           - Perfil sazonal comparativo integrado (Brasil vs. Estados)
# Autor: Yan Galante Maestre
# Data: 2026-10-27
# Dependências: pacman, tidyverse, seasonal, scales, patchwork, here, lubridate
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, seasonal, scales, patchwork, here, lubridate
)

# ------------------------------------------------------------------------------
# 1. Importação e Preparação dos Dados Trimestrais por UF
# ------------------------------------------------------------------------------
df_states <- readr::read_rds(here::here("data", "processed", "states_quarterly_unemployment.rds"))

# ------------------------------------------------------------------------------
# 2. Métricas Quantitativas e Resumo Estatístico das 27 UFs
# ------------------------------------------------------------------------------
df_uf_summary <- df_states |>
  dplyr::group_by(state_code, state_name) |>
  dplyr::summarise(
    mean_rate   = mean(unemployment_rate, na.rm = TRUE),
    sd_rate     = sd(unemployment_rate, na.rm = TRUE),
    min_rate    = min(unemployment_rate, na.rm = TRUE),
    max_rate    = max(unemployment_rate, na.rm = TRUE),
    range_rate  = max_rate - min_rate,
    latest_rate = dplyr::last(unemployment_rate),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(mean_rate))

# Exportação da tabela com as 27 UFs
readr::write_csv(
  df_uf_summary,
  here::here("outputs", "tables", "table_states_descriptive_summary.csv")
)

# Seleção empírica dos 4 casos de destaque:
# 1. Teto estrutural (maior taxa média)
# 2. Piso estrutural (menor taxa média)
# 3. Maior volatilidade histórica (desvio-padrão)
# 4. São Paulo (polo dinâmico central)
top_mean_uf <- df_uf_summary |> dplyr::slice_max(mean_rate, n = 1) |> dplyr::pull(state_name)
min_mean_uf <- df_uf_summary |> dplyr::slice_min(mean_rate, n = 1) |> dplyr::pull(state_name)
top_sd_uf   <- df_uf_summary |> 
  dplyr::filter(!state_name %in% c(top_mean_uf, min_mean_uf)) |> 
  dplyr::slice_max(sd_rate, n = 1) |> 
  dplyr::pull(state_name)

selected_ufs <- unique(c(top_mean_uf, min_mean_uf, top_sd_uf, "São Paulo"))

message("Estados selecionados empiricamente: ", paste(selected_ufs, collapse = ", "))

# ------------------------------------------------------------------------------
# 3. Visualização Transversal: Ranking Histórico das 27 UFs
# ------------------------------------------------------------------------------
fig_states_ranking <- ggplot2::ggplot(
  df_uf_summary, 
  ggplot2::aes(x = stats::reorder(state_name, mean_rate), y = mean_rate)
) +
  ggplot2::geom_col(fill = "#1c3d5a", width = 0.7) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = pmax(0, mean_rate - sd_rate), ymax = mean_rate + sd_rate),
    width = 0.25, 
    color = "#85144b", 
    linewidth = 0.6
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::labs(
    title = "Brazilian States: Historical Average Unemployment Rate (2012-2026)",
    subtitle = "Mean rate with ±1 standard deviation error bars (PNADC Trimestral)",
    x = NULL,
    y = "Average Unemployment Rate (%)"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    axis.text.y = ggplot2::element_text(size = 9)
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "figure_03_states_unemployment_ranking.png"),
  plot = fig_states_ranking,
  width = 9,
  height = 9,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 4. Trajetória Temporal Comparativa dos Casos Selecionados
# ------------------------------------------------------------------------------
df_selected_trajectories <- df_states |>
  dplyr::filter(state_name %in% selected_ufs)

fig_states_time_series <- ggplot2::ggplot(
  df_selected_trajectories, 
  ggplot2::aes(x = date, y = unemployment_rate, color = state_name)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1) +
  ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::scale_color_manual(
    values = c(
      "Bahia"          = "#e41a1c",
      "Rio de Janeiro" = "#377eb8",
      "São Paulo"      = "#984ea3",
      "Santa Catarina" = "#4daf4a"
    )
  ) +
  ggplot2::labs(
    title = "Regional Divergence: Unemployment Dynamics Across Selected States",
    subtitle = "Quarterly series comparing structural extremes and economic hubs (2012-2026)",
    x = "Year",
    y = "Unemployment Rate (%)",
    color = "State"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "figure_04_selected_states_trajectories.png"),
  plot = fig_states_time_series,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 5. Decomposição Sazonal Trimestral (STL) para os Estados Selecionados
# ------------------------------------------------------------------------------
decompose_state_stl <- function(data_subset) {
  s_date    <- min(data_subset$date)
  s_year    <- lubridate::year(s_date)
  s_quarter <- lubridate::quarter(s_date)
  
  ts_state <- stats::ts(
    data_subset$unemployment_rate, 
    start = c(s_year, s_quarter), 
    frequency = 4
  )
  
  stl_fit <- stats::stl(ts_state, s.window = "periodic", robust = TRUE)
  stl_components <- as.data.frame(stl_fit$time.series)
  
  data_subset |>
    dplyr::mutate(
      seasonal_component  = stl_components$seasonal,
      trend_component     = stl_components$trend,
      remainder           = stl_components$remainder,
      seasonally_adjusted = unemployment_rate - seasonal_component
    )
}

df_selected_decomposed <- df_selected_trajectories |>
  dplyr::group_split(state_name) |>
  purrr::map_dfr(decompose_state_stl)

readr::write_rds(
  df_selected_decomposed,
  here::here("data", "processed", "states_selected_decomposed.rds")
)

# ------------------------------------------------------------------------------
# 6. Painel Comparativo das Trajetórias Dessazonalizadas e Tendências (Facets)
# ------------------------------------------------------------------------------
fig_states_decomposed_comparison <- ggplot2::ggplot(
  df_selected_decomposed, 
  ggplot2::aes(x = date)
) +
  ggplot2::geom_line(
    ggplot2::aes(y = unemployment_rate, color = "Raw Rate"), 
    alpha = 0.35, 
    linewidth = 0.6
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = seasonally_adjusted, color = "Seasonally Adjusted"), 
    linewidth = 0.8
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = trend_component, color = "Trend-Cycle"), 
    linewidth = 0.9, 
    linetype = "dashed"
  ) +
  ggplot2::facet_wrap(~ state_name, scales = "free_y", ncol = 2) +
  ggplot2::scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::scale_color_manual(
    values = c("Raw Rate" = "gray50", "Seasonally Adjusted" = "#104e8b", "Trend-Cycle" = "#b22222")
  ) +
  ggplot2::labs(
    title = "Structural and Cyclical Trends by State (STL Decomposition)",
    subtitle = "Comparing raw rates, seasonally adjusted trajectories, and underlying trend-cycles",
    x = "Year",
    y = "Rate (%)",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "figure_05_states_decomposed_facets.png"),
  plot = fig_states_decomposed_comparison,
  width = 11,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 7. Composição Setorial Automatizada via API SIDRA (Tabela 4097 - PNADC)
# Extração 100% programática sem inputs manuais
# ------------------------------------------------------------------------------
message("Baixando dados setoriais da PNADC via API SIDRA (Tabela 4097)...")

# Códigos das UFs selecionadas + Brasil Agregado
# 1 = Brasil, 29 = Bahia, 33 = Rio de Janeiro, 35 = São Paulo, 42 = Santa Catarina
sidra_sectoral_raw <- sidrar::get_sidra(
  x = 4097,
  variable = 4090, # Pessoas ocupadas (mil pessoas)
  geo = "State",
  geo.filter = list("State" = c(29, 33, 35, 42)),
  classific = "c48", # Grupamentos de atividade
  category = list("c48" = "all"),
  period = "last 20" # Últimos 5 anos para compor a média estrutural recente
)

sidra_sectoral_br <- sidrar::get_sidra(
  x = 4097,
  variable = 4090,
  geo = "Brazil",
  classific = "c48",
  category = list("c48" = "all"),
  period = "last 20"
)

# Unificação das extrações
df_sectoral_raw <- dplyr::bind_rows(
  sidra_sectoral_raw |> dplyr::rename(geography = `Unidade da Federação`),
  sidra_sectoral_br  |> dplyr::rename(geography = `Brasil`)
)

# Agregação nos 3 macrosetores clássicos
df_sectoral_aggregated <- df_sectoral_raw |>
  dplyr::rename(
    activity = `Grupamento de atividade no trabalho principal`,
    employed = `Valor`
  ) |>
  dplyr::filter(
    !activity %in% c("Total", "Atividades mal especificadas")
  ) |>
  dplyr::mutate(
    macro_sector = dplyr::case_when(
      activity %in% c("Agricultura, pecuária, produção florestal, pesca e aquicultura") ~ "Agriculture",
      activity %in% c("Indústria geral", "Construção") ~ "Industry_Construction",
      TRUE ~ "Services_Commerce"
    )
  ) |>
  dplyr::group_by(geography, macro_sector) |>
  dplyr::summarise(total_employed = sum(employed, na.rm = TRUE), .groups = "drop_last") |>
  dplyr::mutate(sector_share_pct = (total_employed / sum(total_employed)) * 100) |>
  dplyr::ungroup()

# Transformação para formato tabular transversal
table_sectoral_composition <- df_sectoral_aggregated |>
  dplyr::select(geography, macro_sector, sector_share_pct) |>
  tidyr::pivot_wider(names_from = macro_sector, values_from = sector_share_pct) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

readr::write_csv(
  table_sectoral_composition,
  here::here("outputs", "tables", "table_sectoral_composition_states.csv")
)
message("Tabela setorial automatizada salva em outputs/tables/table_sectoral_composition_states.csv.")

# ------------------------------------------------------------------------------
# 8. Visualização Híbrida da Sazonalidade em Pontos Percentuais (p.p.)
# Comparativo Brasil Mensal vs. Estados Trimestrais na Mesma Métrica
# ------------------------------------------------------------------------------

# 1. Carrega os dados decompostos mensais do Brasil
df_brazil_decomposed <- readr::read_rds(here::here("data", "processed", "brazil_monthly_decomposed.rds"))

# Painel Superior: Brasil Mensal em PONTOS PERCENTUAIS (p.p.)
# Desvio sazonal = Taxa Bruta - Taxa Dessazonalizada
p_br_seasonality <- df_brazil_decomposed |>
  dplyr::mutate(
    month_lbl = lubridate::month(date, label = TRUE, abbr = TRUE),
    year_grp  = as.factor(lubridate::year(date)),
    swing_pp  = unemployment_rate - unemployment_sa
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = month_lbl, y = swing_pp)) +
  # Linhas anuais individuais (dispersão empírica histórica real)
  ggplot2::geom_line(ggplot2::aes(group = year_grp), color = "gray75", linewidth = 0.45, alpha = 0.7) +
  # Média mensal histórica
  ggplot2::stat_summary(fun = mean, geom = "line", ggplot2::aes(group = 1), color = "#104e8b", linewidth = 1.3) +
  ggplot2::stat_summary(fun = mean, geom = "point", color = "#b22222", size = 2.2) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  ggplot2::scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = 0.1), " p.p.")) +
  ggplot2::labs(
    title = "A. Brazil: Recurring Monthly Seasonal Profile (PNADC 3-Month Moving Average)",
    subtitle = "Absolute seasonal deviation from trend-cycle in percentage points (Grey lines = Individual years; Blue line = Historical mean)",
    x = "Month",
    y = "Seasonal Impact (p.p.)"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

# 2. Carrega os dados dos Estados e calcula médias trimestrais em p.p.
df_selected_decomposed <- readr::read_rds(here::here("data", "processed", "states_selected_decomposed.rds"))

df_states_seasonality <- df_selected_decomposed |>
  dplyr::mutate(quarter_lbl = paste0("Q", lubridate::quarter(date))) |>
  dplyr::group_by(state_name, quarter_lbl) |>
  dplyr::summarise(
    seasonal_swing_pp = mean(seasonal_component, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Agrega a série do Brasil para periodicidade trimestral (média do desvio em p.p.)
df_brazil_quarterly_swing <- df_brazil_decomposed |>
  dplyr::mutate(
    quarter_lbl = paste0("Q", lubridate::quarter(date)),
    swing_pp    = unemployment_rate - unemployment_sa
  ) |>
  dplyr::group_by(quarter_lbl) |>
  dplyr::summarise(
    seasonal_swing_pp = mean(swing_pp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(state_name = "Brazil (Aggregate)")

# 4. Base integrada de sazonalidade
df_all_seasonality <- dplyr::bind_rows(df_states_seasonality, df_brazil_quarterly_swing)

# Painel Inferior: Estados Selecionados vs. Brasil em PONTOS PERCENTUAIS (p.p.)
p_states_seasonality <- ggplot2::ggplot(
  df_all_seasonality, 
  ggplot2::aes(x = quarter_lbl, y = seasonal_swing_pp, color = state_name, group = state_name, linetype = state_name)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "gray70", linewidth = 0.5) +
  ggplot2::geom_line(ggplot2::aes(linewidth = ifelse(state_name == "Brazil (Aggregate)", 1.2, 0.9))) +
  ggplot2::geom_point(ggplot2::aes(size = ifelse(state_name == "Brazil (Aggregate)", 2.8, 2.0))) +
  ggplot2::scale_color_manual(
    values = c(
      "Brazil (Aggregate)" = "black",
      "Bahia"              = "#e41a1c",
      "Rio de Janeiro"     = "#377eb8",
      "São Paulo"          = "#984ea3",
      "Santa Catarina"     = "#4daf4a"
    )
  ) +
  ggplot2::scale_linetype_manual(
    values = c(
      "Brazil (Aggregate)" = "dashed",
      "Bahia"              = "solid",
      "Rio de Janeiro"     = "solid",
      "São Paulo"          = "solid",
      "Santa Catarina"     = "solid"
    )
  ) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = 0.1), " p.p.")) +
  ggplot2::scale_linewidth_identity() +
  ggplot2::scale_size_identity() +
  ggplot2::labs(
    title = "B. Quarterly Comparison: Brazil vs. Selected States",
    subtitle = "Absolute seasonal deviation from trend-cycle in percentage points (Dashed black line = Brazil Aggregate)",
    x = "Quarter",
    y = "Seasonal Impact (p.p.)",
    color = "Geography",
    linetype = "Geography"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

# 5. Composição com Patchwork e gravação em alta resolução
fig_seasonality_hybrid <- p_br_seasonality / p_states_seasonality

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "figure_06_seasonal_profiles.png"),
  plot = fig_seasonality_hybrid,
  width = 10,
  height = 9,
  dpi = 300
)

message("Figura 06 gerada com sucesso em outputs/figures/figure_06_seasonal_profiles.png.")