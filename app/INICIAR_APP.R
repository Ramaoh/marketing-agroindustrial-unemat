# INICIAR_APP.R
# Execute este arquivo no RStudio com: source("INICIAR_APP.R", encoding="UTF-8")

arquivo_app <- file.path(getwd(), "app.R")
if (!file.exists(arquivo_app)) {
  stop("Não encontrei app.R. Abra o diretório Campanha_Agroindustrial_RStudio no RStudio.")
}

pacotes <- c("shiny", "DT", "httr", "jsonlite", "readr", "dplyr", "shinyjs")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly=TRUE)]
if (length(faltantes)) {
  install.packages(faltantes, repos="https://cloud.r-project.org")
}

shiny::runApp(getwd(), launch.browser=TRUE)
