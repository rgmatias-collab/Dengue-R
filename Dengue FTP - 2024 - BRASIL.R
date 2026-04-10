# ==============================
# AUMENTAR TEMPO DE DOWNLOAD
# ==============================

options(timeout = 6000)

# ==============================
# URL DO DATASUS
# ==============================

url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/DENGBR24.dbc"

# ==============================
# PASTA DE DESTINO
# ==============================

pasta_destino <- "C:/Users/rgmat/OneDrive/Documentos/espacialização - ba/DENG-BA/Deng-Br-2024"

dir.create(pasta_destino, showWarnings = FALSE, recursive = TRUE)

# ==============================
# CAMINHO FINAL DO ARQUIVO
# ==============================

arquivo_destino <- file.path(pasta_destino, "DENGBR24.dbc")

# ==============================
# DOWNLOAD
# ==============================

cat("Baixando SINAN Dengue 2024...\n")

download.file(
  url,
  destfile = arquivo_destino,
  mode = "wb",
  method = "libcurl"
)

cat("Download concluído!\n")
cat("Arquivo salvo em:\n")
print(arquivo_destino)

