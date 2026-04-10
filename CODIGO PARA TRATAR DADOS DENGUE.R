# Instalar pacotes se necessário
install.packages(c("foreign","dplyr","writexl"))

library(foreign)
library(dplyr)
library(writexl)

# Caminho do DBF
arquivo <- "C:/Users/rgmat/OneDrive/Documentos/espacialização - ba/DENG-BA/Deng-Br-2021/DENGBR21.dbf"

# Ler DBF
dados <- read.dbf(arquivo, as.is = TRUE)

# Garantir que o código do município seja character
dados$ID_MN_RESI <- as.character(dados$ID_MN_RESI)

# Tabela com municípios e códigos IBGE
municipios <- data.frame(
  cidade = c(
    "Salvador",
    "Barreiras",
    "Ilheus",
    "Feira_de_Santana",
    "Vitoria_da_Conquista",
    "Porto_Seguro"
     ),
  codigo = as.character(c(
    292740,
    290320,
    291360,
    291080,
    293330,
    292530
  ))
)

# Pasta de saída
pasta_saida <- "C:/Users/rgmat/OneDrive/Documentos/espacialização - ba/DENG-BA/casos de dengue - municipios/2021"

dir.create(pasta_saida, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# GERAR PLANILHA POR MUNICÍPIO
# ---------------------------

for(i in 1:nrow(municipios)){
  
  codigo <- municipios$codigo[i]
  cidade <- municipios$cidade[i]
  
  dados_mun <- dados %>%
    filter(ID_MN_RESI == codigo) %>%
    select(
      id_municipio = ID_MN_RESI,
      id_estado = SG_UF,
      data_notificacao = DT_NOTIFIC
    )
  
  caminho_saida <- paste0(pasta_saida, "/", cidade, ".xlsx")
  
  write_xlsx(dados_mun, caminho_saida)
}

# ---------------------------
# PLANILHA RESUMO COM CONTAGEM
# ---------------------------

resumo <- dados %>%
  filter(ID_MN_RESI %in% municipios$codigo) %>%
  count(ID_MN_RESI, name = "quantidade_registros") %>%
  left_join(municipios, by = c("ID_MN_RESI" = "codigo")) %>%
  select(
    cidade,
    id_municipio = ID_MN_RESI,
    quantidade_registros
  )

# Salvar resumo
write_xlsx(
  resumo,
  paste0(pasta_saida, "/resumo_quantidade_municipios.xlsx")
)

