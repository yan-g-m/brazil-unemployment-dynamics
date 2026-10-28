# ==============================================================================
# Objetivo: Estruturar os diretórios do projeto para governança e reprodutibilidade
# Autor: Yan Galante Maestre
# Data: 2026-10-27
# Dependências: fs, here
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(fs, here)

# Criação das pastas de dados, scripts, saídas e relatórios
pastas_projeto <- c(
  here::here("data", "raw"),
  here::here("data", "processed"),
  here::here("scripts"),
  here::here("outputs", "tables"),
  here::here("outputs", "figures"),
  here::here("reports")
)

fs::dir_create(pastas_projeto)
message("Diretórios do projeto estruturados com sucesso via here::here().")