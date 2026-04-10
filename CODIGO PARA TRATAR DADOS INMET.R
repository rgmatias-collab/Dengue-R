install.packages("janitor")
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(stringr)
library(janitor)

pasta_entrada <- "C:/Users/rgmat/OneDrive/Documentos/espacialização - ba/METEOROLOGIA BA/METEOROLOGIA BA/2024"
pasta_saida   <- "C:/Users/rgmat/OneDrive/Documentos/espacialização - ba/METEOROLOGIA BA/Meorologia Ba - Tratadas/2024"


dir.create(pasta_saida, showWarnings = FALSE)

arquivos <- list.files(
  pasta_entrada,
  pattern = "\\.csv$|\\.CSV$",
  full.names = TRUE
)

limpar_inmet <- function(arquivo){
  
  dados <- read_csv2(
    arquivo,
    skip = 8,
    show_col_types = FALSE
  )
  
  # Padronizar nomes das colunas
  dados <- clean_names(dados)
  
  # Ver nomes das colunas (debug se necessário)
  # print(names(dados))
  
  dados <- dados %>%
    select(
      data = contains("data"),
      precipitacao = contains("precipit"),
      temperatura = contains("bulbo_seco"),
      umidade = contains("umidade_relativa")
    )
  
  # remover linhas com -9999
  dados <- dados %>%
    filter(!if_any(everything(), ~ . == -9999))
  
  # converter data
  dados <- dados %>%
    mutate(data = ymd(data))
  
  # média diária
  dados_diarios <- dados %>%
    group_by(data) %>%
    summarise(
      precipitacao_mm = mean(precipitacao, na.rm = TRUE),
      temperatura_c = mean(temperatura, na.rm = TRUE),
      umidade_relativa = mean(umidade, na.rm = TRUE),
      .groups = "drop"
    )
  
  nome_saida <- file.path(
    pasta_saida,
    paste0("tratado_", basename(arquivo))
  )
  
  write_csv(dados_diarios, nome_saida)
  
}

map(arquivos, limpar_inmet)

