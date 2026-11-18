# INICIAR_APP.R
# Alternativa ao botão "Run App": execute no RStudio com
#   source("INICIAR_APP.R", encoding = "UTF-8")
# O próprio app.R instala os pacotes que faltarem.

arquivo_app <- file.path(getwd(), "app.R")
if (!file.exists(arquivo_app)) {
  stop("Não encontrei app.R nesta pasta. Use Session > Set Working Directory > Choose Directory e escolha a pasta do app.")
}
shiny::runApp(getwd(), launch.browser = TRUE)
