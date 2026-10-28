# ==============================================================================
# Objetivo: Diagnóstico Econométrico, Modelagem SARIMA e Decomposição Estrutural
#           da Taxa de Desemprego no Brasil (PNADC/IBGE)
# Autor: Yan Galante Maestre
# Data: 2026-10-27
# Metodologia: Testes de Raiz Unitária (ADF, KPSS, ZA), SARIMA/SEATS e Testes de Resíduos
# Dependências: pacman, tidyverse, tseries, urca, seasonal, FinTS, patchwork, here
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, tseries, urca, seasonal, FinTS, patchwork, here
)

# ------------------------------------------------------------------------------
# 1. Importação dos Dados Tratados do Brasil
# ------------------------------------------------------------------------------
df_brazil <- readr::read_rds(here::here("data", "processed", "brazil_monthly_unemployment.rds"))

start_date <- min(df_brazil$date)
start_year <- lubridate::year(start_date)
start_month <- lubridate::month(start_date)

ts_unemployment <- stats::ts(
  df_brazil$unemployment_rate,
  start = c(start_year, start_month),
  frequency = 12
)

# ------------------------------------------------------------------------------
# 2. Diagnóstico Exploratório e Propriedades Estocásticas
# ------------------------------------------------------------------------------
p_ts <- ggplot2::ggplot(df_brazil, ggplot2::aes(x = date, y = unemployment_rate)) +
  ggplot2::geom_line(color = "#104e8b", linewidth = 0.8) +
  ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::labs(
    title = "A. Unemployment Rate in Brazil (PNADC 3-Month Moving Average)",
    x = NULL, 
    y = "Unemployment Rate (%)"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

p_acf <- forecast::ggAcf(ts_unemployment, lag.max = 36) +
  ggplot2::labs(
    title = "B. Autocorrelation Function (ACF)", 
    x = "Lag (Months)",
    y = "ACF"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

p_pacf <- forecast::ggPacf(ts_unemployment, lag.max = 36) +
  ggplot2::labs(
    title = "C. Partial Autocorrelation Function (PACF)", 
    x = "Lag (Months)",
    y = "PACF"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

fig_exploratory <- p_ts / (p_acf + p_pacf)

ggplot2::ggsave(
  here::here("outputs", "figures", "figure_01_exploratory_diagnostics.png"),
  plot = fig_exploratory, 
  width = 10, 
  height = 7, 
  dpi = 300
)

# ------------------------------------------------------------------------------
# 3. Testes Formais de Raiz Unitária e Quebra Estrutural (Corrigido Definitivo)
# ------------------------------------------------------------------------------
test_adf  <- urca::ur.df(ts_unemployment, type = "trend", selectlags = "AIC")
test_kpss <- urca::ur.kpss(ts_unemployment, type = "tau", lags = "short")
test_za   <- urca::ur.za(ts_unemployment, model = "both", lag = 2)

# Extração das estatísticas de teste (escalares numéricos puros)
stat_adf  <- as.numeric(test_adf@teststat[1])
stat_kpss <- as.numeric(test_kpss@teststat[1])
stat_za   <- as.numeric(test_za@teststat)

# Extração dos valores críticos a 5% por posição indexada
cval_adf_5pct  <- as.numeric(test_adf@cval[1, 2])   # Coluna 2 = 5pct
cval_kpss_5pct <- as.numeric(test_kpss@cval[1, 2])  # Coluna 2 = 5pct
cval_za_5pct   <- as.numeric(test_za@cval[2])       # Elemento 2 = 5% (-5.08)

# Validação defensiva: se cval_za_5pct for NA, fixa o valor teórico canónico (-5.08)
if (is.na(cval_za_5pct)) cval_za_5pct <- -5.08

table_unit_root_tests <- tibble::tibble(
  Test = c(
    "ADF (com tendência)",
    "KPSS (com tendência)",
    "Zivot-Andrews (intercepto e tendência)"
  ),
  Null_Hypothesis = c(
    "Presença de Raiz Unitária",
    "Estacionariedade em Tendência",
    "Raiz Unitária com Quebra Estrutural"
  ),
  Test_Statistic = c(stat_adf, stat_kpss, stat_za),
  Critical_Value_5pct = c(cval_adf_5pct, cval_kpss_5pct, cval_za_5pct),
  Decision = c(
    ifelse(stat_adf < cval_adf_5pct, "Rejeita H0", "Não Rejeita H0"),
    ifelse(stat_kpss > cval_kpss_5pct, "Rejeita H0", "Não Rejeita H0"),
    ifelse(stat_za < cval_za_5pct, "Rejeita H0", "Não Rejeita H0")
  )
)

readr::write_csv(
  table_unit_root_tests,
  here::here("outputs", "tables", "table_unit_root_tests_brazil.csv")
)

message(">> [OK] table_unit_root_tests_brazil.csv gravado")

# ------------------------------------------------------------------------------
# 4. Ajustamento Sazonal via X-13ARIMA-SEATS com Módulo TRAMO
# ------------------------------------------------------------------------------
model_x13 <- seasonal::seas(
  ts_unemployment,
  transform.function = "log",
  regression.aictest = NULL,
  outlier = list(types = c("ao", "ls", "tc"))
)

# Extração das componentes estruturais
ts_sa    <- seasonal::final(model_x13)
ts_trend <- seasonal::trend(model_x13)
ts_seas  <- seasonal::series(model_x13, "seats.seasonal")
ts_irreg <- seasonal::irregular(model_x13)

df_brazil_decomposed <- df_brazil |>
  dplyr::mutate(
    unemployment_sa    = as.numeric(ts_sa),
    unemployment_trend = as.numeric(ts_trend),
    seasonal_factor    = as.numeric(ts_seas),
    irregular_factor   = as.numeric(ts_irreg)
  )

readr::write_rds(
  df_brazil_decomposed,
  here::here("data", "processed", "brazil_monthly_decomposed.rds")
)

# ------------------------------------------------------------------------------
# 5. Diagnóstico Residual e Validação Econométrica
# ------------------------------------------------------------------------------
residuos_x13 <- stats::residuals(model_x13)

lb_12 <- stats::Box.test(residuos_x13, lag = 12, type = "Ljung-Box", fitdf = 2)
lb_24 <- stats::Box.test(residuos_x13, lag = 24, type = "Ljung-Box", fitdf = 2)
arch_test <- FinTS::ArchTest(as.numeric(na.omit(residuos_x13)), lags = 12)
jb_test   <- tseries::jarque.bera.test(as.numeric(na.omit(residuos_x13)))

table_residual_diagnostics <- tibble::tibble(
  Diagnostic_Test = c("Ljung-Box (Lag 12)", "Ljung-Box (Lag 24)", "ARCH-LM (Lag 12)", "Jarque-Bera"),
  Null_Hypothesis = c("Ausência de Autocorrelação", "Ausência de Autocorrelação", "Homocedasticidade Condicional", "Normalidade dos Erros"),
  Test_Statistic  = c(lb_12$statistic, lb_24$statistic, arch_test$statistic, jb_test$statistic),
  P_Value         = c(lb_12$p.value, lb_24$p.value, arch_test$p.value, jb_test$p.value),
  Decision_5pct   = c(
    ifelse(lb_12$p.value > 0.05, "Não Rejeita H0 (Válido)", "Rejeita H0"),
    ifelse(lb_24$p.value > 0.05, "Não Rejeita H0 (Válido)", "Rejeita H0"),
    ifelse(arch_test$p.value > 0.05, "Não Rejeita H0 (Válido)", "Rejeita H0"),
    ifelse(jb_test$p.value > 0.05, "Não Rejeita H0 (Válido)", "Rejeita H0")
  )
)

readr::write_csv(
  table_residual_diagnostics,
  here::here("outputs", "tables", "table_residual_diagnostics_brazil.csv")
)

# ------------------------------------------------------------------------------
# 5.1. Diagnóstico Visual de Normalidade dos Resíduos (Figura Adicional)
# ------------------------------------------------------------------------------
residuos_log <- log(df_brazil_decomposed$irregular_factor) |> na.omit()
df_res <- tibble::tibble(residuos = as.numeric(residuos_log))

p_hist <- ggplot2::ggplot(df_res, ggplot2::aes(x = residuos)) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = after_stat(density)),
    bins = 25, fill = "#1c3d5a", color = "white", alpha = 0.8
  ) +
  ggplot2::geom_density(ggplot2::aes(color = "Empirical Density"), linewidth = 1) +
  ggplot2::stat_function(
    fun = dnorm,
    args = list(mean = mean(df_res$residuos), sd = sd(df_res$residuos)),
    ggplot2::aes(color = "Theoretical Normal"),
    linetype = "dashed", linewidth = 1
  ) +
  ggplot2::scale_color_manual(
    values = c("Empirical Density" = "#e41a1c", "Theoretical Normal" = "black")
  ) +
  ggplot2::labs(
    title = "A. Residual Distribution vs. Normal Curve",
    x = "Irregular Component (log residuals)",
    y = "Density",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "top", plot.title = ggplot2::element_text(face = "bold"))

p_qq <- ggplot2::ggplot(df_res, ggplot2::aes(sample = residuos)) +
  ggplot2::stat_qq(color = "#104e8b", size = 1.8, alpha = 0.7) +
  ggplot2::stat_qq_line(color = "#e41a1c", linewidth = 1, linetype = "dashed") +
  ggplot2::labs(
    title = "B. Normal Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

fig_residual_normality <- p_hist + p_qq

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "figure_v2_residual_normality.png"),
  plot = fig_residual_normality,
  width = 10,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 6. Teste Formal de Dummies e Auditoria de Estabilidade (Covid-19)
# ------------------------------------------------------------------------------
df_reg <- df_brazil |>
  dplyr::mutate(
    d_log_rate     = c(NA, diff(log(unemployment_rate))),
    d12_d_log_rate = c(rep(NA, 12), diff(d_log_rate, lag = 12)),
    lag_ar1        = dplyr::lag(d12_d_log_rate, 1),
    dummy_pulse    = ifelse(date >= as.Date("2020-03-01") & date <= as.Date("2020-06-01"), 1, 0),
    dummy_level    = ifelse(date >= as.Date("2020-03-01"), 1, 0)
  ) |>
  na.omit()

lm_base  <- stats::lm(d12_d_log_rate ~ lag_ar1, data = df_reg)
lm_dummy <- stats::lm(d12_d_log_rate ~ lag_ar1 + dummy_pulse + dummy_level, data = df_reg)
anova_covid <- stats::anova(lm_base, lm_dummy)

table_covid_dummy_test <- tibble::tibble(
  Model = c("Base SARIMA (sem dummies)", "SARIMA com Dummies Covid (Pulso e Nível)"),
  Res_Df = anova_covid$Res.Df,
  RSS = anova_covid$RSS,
  F_Statistic = c(NA, anova_covid$F[2]),
  P_Value = c(NA, anova_covid$`Pr(>F)`[2])
)

readr::write_csv(
  table_covid_dummy_test,
  here::here("outputs", "tables", "table_covid_dummy_test.csv")
)

# ------------------------------------------------------------------------------
# 7. Figura de Decomposição Estrutural
# ------------------------------------------------------------------------------
p1 <- ggplot2::ggplot(df_brazil_decomposed, ggplot2::aes(x = date)) +
  ggplot2::geom_line(ggplot2::aes(y = unemployment_rate, color = "Original (Raw)"), linewidth = 0.6) +
  ggplot2::geom_line(ggplot2::aes(y = unemployment_sa, color = "Seasonally Adjusted (SA)"), linewidth = 0.8) +
  ggplot2::geom_line(ggplot2::aes(y = unemployment_trend, color = "Trend-Cycle"), linewidth = 0.9, linetype = "dashed") +
  ggplot2::scale_color_manual(
    values = c(
      "Original (Raw)" = "gray60", 
      "Seasonally Adjusted (SA)" = "#104e8b", 
      "Trend-Cycle" = "#b22222"
    )
  ) +
  ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::labs(
    title = "Brazil: Unemployment Rate Decomposition (X-13ARIMA-SEATS)",
    subtitle = "Monthly 3-Month Moving Average (Mar/2012 - Aug/2026)",
    x = NULL, 
    y = "Rate (%)", 
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold")
  )

p2 <- ggplot2::ggplot(df_brazil_decomposed, ggplot2::aes(x = date, y = seasonal_factor)) +
  ggplot2::geom_line(color = "#2e8b57", linewidth = 0.7) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "gray40") +
  ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ggplot2::labs(
    title = "Seasonal Factor (Multiplicative / Ratio to Moving Average)", 
    x = NULL, 
    y = "Factor"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

p3 <- ggplot2::ggplot(df_brazil_decomposed, ggplot2::aes(x = date, y = irregular_factor)) +
  ggplot2::geom_line(color = "gray30", linewidth = 0.6) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "gray40") +
  ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ggplot2::labs(
    title = "Irregular Component (Noise)", 
    x = "Year", 
    y = "Factor"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

fig_decomposition <- patchwork::wrap_plots(p1, p2, p3, ncol = 1)

ggplot2::ggsave(
  here::here("outputs", "figures", "figure_02_structural_decomposition_brazil.png"),
  plot = fig_decomposition, 
  width = 10, 
  height = 9, 
  dpi = 300
)

# ------------------------------------------------------------------------------
# 8. Visualização Executiva: Tendência Recente e Projeção
# ------------------------------------------------------------------------------

# 1. Extração do último ponto e parâmetros de projeção
ultimo_dado <- df_brazil_decomposed |> dplyr::slice_tail(n = 1)
data_base   <- ultimo_dado$date
taxa_trend  <- ultimo_dado$unemployment_trend

# Fatores sazonais médios históricos do X-13 (Setembro a Dezembro)
df_fatores_fim_ano <- df_brazil_decomposed |>
  dplyr::mutate(month_num = lubridate::month(date)) |>
  dplyr::filter(month_num %in% c(9, 10, 11, 12)) |>
  dplyr::group_by(month_num) |>
  dplyr::summarise(
    mean_factor = mean(seasonal_factor, na.rm = TRUE),
    min_factor  = min(seasonal_factor, na.rm = TRUE),
    max_factor  = max(seasonal_factor, na.rm = TRUE),
    .groups = "drop"
  )

# Datas de projeção estritamente até Dezembro de 2026
datas_projecao <- seq.Date(
  from = lubridate::floor_date(data_base, "month") + months(1), 
  to = as.Date("2026-12-01"),
  by = "1 month"
)

df_forecast <- tibble::tibble(
  date       = datas_projecao,
  month_num  = lubridate::month(datas_projecao),
  trend_rate = taxa_trend
) |>
  dplyr::left_join(df_fatores_fim_ano, by = "month_num") |>
  dplyr::mutate(
    unemployment_rate = trend_rate * mean_factor,
    rate_lower        = trend_rate * min_factor,
    rate_upper        = trend_rate * max_factor,
    series_type       = "Seasonal Projection"
  )

# Base recente (2024 em diante) para foco visual limpo
df_zoom <- df_brazil_decomposed |>
  dplyr::filter(date >= as.Date("2024-01-01")) |>
  dplyr::select(date, unemployment_rate, unemployment_trend) |>
  dplyr::mutate(series_type = "Observed")

ponto_conexao <- df_zoom |> 
  dplyr::slice_tail(n = 1) |> 
  dplyr::mutate(
    rate_lower  = unemployment_rate,
    rate_upper  = unemployment_rate,
    series_type = "Seasonal Projection"
  )

df_forecast_plot <- dplyr::bind_rows(ponto_conexao, df_forecast)
ponto_final_dez  <- df_forecast |> dplyr::slice_tail(n = 1)

# 2. Construção Gráfica Limpa e Minimalista
fig_executiva <- ggplot2::ggplot() +
  # Linha da taxa observada real (cinza)
  ggplot2::geom_line(
    data = df_zoom, 
    ggplot2::aes(x = date, y = unemployment_rate, color = "Raw Rate (Observed)"), 
    linewidth = 0.85
  ) +
  # Linha da tendência-ciclo purificada (vermelha)
  ggplot2::geom_line(
    data = df_zoom, 
    ggplot2::aes(x = date, y = unemployment_trend, color = "Trend-Cycle (Structural Baseline)"), 
    linewidth = 1.3
  ) +
  # Ribbon de incerteza sazonal
  ggplot2::geom_ribbon(
    data = df_forecast_plot,
    ggplot2::aes(x = date, ymin = rate_lower, ymax = rate_upper),
    fill = "#104e8b", alpha = 0.15
  ) +
  # Trajetória tracejada da projeção
  ggplot2::geom_line(
    data = df_forecast_plot,
    ggplot2::aes(x = date, y = unemployment_rate, color = "Seasonal Projection (End-of-Year Dip)"),
    linetype = "dashed", linewidth = 1.1
  ) +
  # Marcador pontual em Dezembro/2026
  ggplot2::geom_point(
    data = ponto_final_dez,
    ggplot2::aes(x = date, y = unemployment_rate),
    color = "#104e8b", size = 3
  ) +
  # Rótulo explicativo alinhado com margem interna à esquerda do ponto (sem cortes)
  ggplot2::annotate(
    "label",
    x = ponto_final_dez$date,
    y = ponto_final_dez$unemployment_rate - 0.35,
    label = paste0(
      "Expected Dec/2026: ~", 
      scales::number(ponto_final_dez$unemployment_rate, accuracy = 0.1), "%\n",
      "(Seasonal swing, not structural drop)"
    ),
    hjust = 1,
    fill = "#f8f9fa", color = "#104e8b", fontface = "bold", size = 3.2,
    label.padding = ggplot2::unit(0.4, "lines"),
    label.r = ggplot2::unit(0.2, "lines")
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Raw Rate (Observed)"                  = "gray40",
      "Trend-Cycle (Structural Baseline)"    = "#b22222",
      "Seasonal Projection (End-of-Year Dip)" = "#104e8b"
    )
  ) +
  # Escala temporal encerrando estritamente em Dezembro/2026
  ggplot2::scale_x_date(
    date_breaks = "3 months", 
    date_labels = "%b/%Y",
    limits = c(as.Date("2024-01-01"), as.Date("2026-12-01")),
    expand = ggplot2::expansion(mult = c(0.02, 0.07))
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    breaks = seq(4.5, 8.5, by = 0.5),
    limits = c(4.5, 8.2)
  ) +
  ggplot2::labs(
    title = "Brazil Unemployment Dynamics: Current Structural Tightness and Year-End Outlook",
    subtitle = "Decomposing underlying structural trends from seasonal hiring swings (PNADC / X-13ARIMA-SEATS)",
    x = NULL,
    y = "Unemployment Rate (%)",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

# Exportação em alta resolução
ggplot2::ggsave(
  filename = here::here("outputs", "figures", "figure_07_trend_and_forecast_bridge.png"),
  plot = fig_executiva,
  width = 10,
  height = 6,
  dpi = 300
)

message("Figura 07 atualizada e salva com sucesso.")