# ============================================================
# SISTEMA DE CAMPANHAS DE MARKETING AGROINDUSTRIAL
# Aplicativo Shiny - WhatsApp, Instagram e Facebook (Meta APIs)
# ============================================================

options(shiny.maxRequestSize = 10 * 1024^2)

pacotes <- c("shiny", "DT", "httr", "jsonlite", "readr", "dplyr", "shinyjs")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) {
  stop(
    "Instale os pacotes antes de iniciar: install.packages(c(",
    paste(sprintf('"%s"', faltantes), collapse = ", "),
    "))"
  )
}

library(shiny)
library(DT)
library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(shinyjs)

# ---------------- CONFIGURAÇÕES E ARQUIVOS ----------------
PASTA_DADOS <- "dados"
PASTA_MIDIA <- file.path(PASTA_DADOS, "midia")
PASTA_LOGS  <- file.path(PASTA_DADOS, "logs")
dir.create(PASTA_DADOS, showWarnings = FALSE, recursive = TRUE)
dir.create(PASTA_MIDIA, showWarnings = FALSE, recursive = TRUE)
dir.create(PASTA_LOGS, showWarnings = FALSE, recursive = TRUE)

ARQ_CONTATOS <- file.path(PASTA_DADOS, "contatos.csv")
ARQ_PRODUTOS <- file.path(PASTA_DADOS, "produtos.csv")
ARQ_CAMPANHAS <- file.path(PASTA_DADOS, "campanhas.csv")
ARQ_LOG <- file.path(PASTA_LOGS, "historico_envios.csv")

campos_contatos <- c("nome", "telefone", "instagram_id", "facebook_id",
                     "consentimento", "whatsapp_optin", "instagram_optin",
                     "facebook_optin", "observacao")
campos_produtos <- c("produto", "categoria", "descricao", "preco", "unidade",
                     "imagem", "link", "ativo")
campos_campanhas <- c("data_criacao", "nome", "produto", "objetivo",
                      "publico", "mensagem", "chamada", "link", "imagem",
                      "canais", "status")

inicializar_csv <- function(arquivo, campos) {
  if (!file.exists(arquivo)) {
    utils::write.csv(as.data.frame(setNames(replicate(length(campos), character(0),
      simplify = FALSE), campos)), arquivo, row.names = FALSE, fileEncoding = "UTF-8")
  }
}
inicializar_csv(ARQ_CONTATOS, campos_contatos)
inicializar_csv(ARQ_PRODUTOS, campos_produtos)
inicializar_csv(ARQ_CAMPANHAS, campos_campanhas)
if (!file.exists(ARQ_LOG)) {
  utils::write.csv(data.frame(data_hora=character(), campanha=character(),
    canal=character(), destinatario=character(), resultado=character(),
    detalhe=character()), ARQ_LOG, row.names=FALSE, fileEncoding="UTF-8")
}

ler_csv <- function(arquivo, campos) {
  tryCatch({
    x <- readr::read_csv(arquivo, show_col_types = FALSE, na = character(),
                         locale = locale(encoding = "UTF-8"))
    for (nm in setdiff(campos, names(x))) x[[nm]] <- ""
    as.data.frame(x[, campos, drop=FALSE], stringsAsFactors=FALSE)
  }, error=function(e) as.data.frame(setNames(replicate(length(campos),
      character(0), simplify=FALSE), campos)))
}
gravar_csv <- function(x, arquivo) {
  readr::write_csv(as.data.frame(x), arquivo, na = "")
}
env <- function(nome, padrao="") {
  valor <- Sys.getenv(nome, unset=padrao)
  trimws(valor)
}
api_version <- function() {
  v <- env("META_API_VERSION", "v27.0")
  if (!grepl("^v[0-9]+\\.[0-9]+$", v)) "v27.0" else v
}
graph_url <- function(path) paste0("https://graph.facebook.com/", api_version(), "/", path)

registrar <- function(campanha, canal, destinatario, resultado, detalhe="") {
  linha <- data.frame(data_hora=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    campanha=campanha, canal=canal, destinatario=destinatario,
    resultado=resultado, detalhe=substr(detalhe, 1, 500),
    stringsAsFactors=FALSE)
  atual <- tryCatch(read.csv(ARQ_LOG, stringsAsFactors=FALSE, fileEncoding="UTF-8"),
                    error=function(e) data.frame())
  gravar_csv(bind_rows(atual, linha), ARQ_LOG)
}

resposta_meta <- function(resp) {
  txt <- content(resp, as="text", encoding="UTF-8")
  if (http_error(resp)) {
    obj <- tryCatch(fromJSON(txt), error=function(e) list())
    msg <- if (!is.null(obj$error$message)) obj$error$message else txt
    return(list(ok=FALSE, detalhe=paste0("HTTP ", status_code(resp), ": ", msg)))
  }
  list(ok=TRUE, detalhe=txt)
}

# ---------------- INTEGRAÇÕES META ----------------
# WhatsApp: envio de modelo aprovado (categoria MARKETING).
# O nome/idioma do modelo e o ID do telefone são definidos no .Renviron.
enviar_whatsapp_template <- function(telefone, campanha, produto, chamada) {
  token <- env("META_ACCESS_TOKEN")
  phone_id <- env("WA_PHONE_NUMBER_ID")
  template <- env("WA_TEMPLATE_NAME")
  idioma <- env("WA_TEMPLATE_LANG", "pt_BR")
  if (!nzchar(token) || !nzchar(phone_id) || !nzchar(template)) {
    return(list(ok=FALSE, detalhe="Configure META_ACCESS_TOKEN, WA_PHONE_NUMBER_ID e WA_TEMPLATE_NAME no .Renviron."))
  }
  numero <- gsub("[^0-9]", "", telefone)
  if (nchar(numero) < 10) return(list(ok=FALSE, detalhe="Telefone inválido. Use código do país + DDD + número."))
  # Modelo esperado: 3 variáveis posicionais no corpo:
  # {{1}} nome da campanha; {{2}} produto; {{3}} chamada.
  corpo <- list(list(type="body", parameters=list(
    list(type="text", text=substr(campanha, 1, 60)),
    list(type="text", text=substr(produto, 1, 60)),
    list(type="text", text=substr(chamada, 1, 60))
  )))
  payload <- list(messaging_product="whatsapp", recipient_type="individual",
    to=numero, type="template",
    template=list(name=template, language=list(code=idioma), components=corpo))
  resp <- tryCatch(POST(graph_url(paste0(phone_id, "/messages")),
    add_headers(Authorization=paste("Bearer", token)),
    content_type_json(), body=payload, encode="json", timeout(30)),
    error=function(e) NULL)
  if (is.null(resp)) return(list(ok=FALSE, detalhe="Falha de conexão com a API do WhatsApp."))
  resposta_meta(resp)
}

# Instagram: mensagem individual para Instagram-scoped ID elegível.
# A pessoa precisa ter iniciado/interagido com a conta profissional,
# e a mensagem deve respeitar a janela e as políticas vigentes.
enviar_instagram_dm <- function(ig_id, mensagem) {
  token <- env("META_ACCESS_TOKEN")
  if (!nzchar(token) || !nzchar(ig_id)) return(list(ok=FALSE, detalhe="Token ou Instagram-scoped ID ausente."))
  payload <- list(recipient=list(id=ig_id), message=list(text=mensagem))
  resp <- tryCatch(POST(graph_url(paste0(env("IG_USER_ID"), "/messages")),
    add_headers(Authorization=paste("Bearer", token)),
    content_type_json(), body=payload, encode="json", timeout(30)),
    error=function(e) NULL)
  if (is.null(resp)) return(list(ok=FALSE, detalhe="Falha de conexão com a API do Instagram."))
  resposta_meta(resp)
}

# Facebook Messenger: mensagem individual para Page-scoped ID elegível.
enviar_facebook_dm <- function(fb_id, mensagem) {
  token <- env("FB_PAGE_ACCESS_TOKEN", env("META_ACCESS_TOKEN"))
  page_id <- env("FB_PAGE_ID")
  if (!nzchar(token) || !nzchar(page_id) || !nzchar(fb_id))
    return(list(ok=FALSE, detalhe="Configure FB_PAGE_ID, token da Página e ID do destinatário."))
  payload <- list(recipient=list(id=fb_id), messaging_type="RESPONSE",
                  message=list(text=mensagem))
  resp <- tryCatch(POST(graph_url(paste0(page_id, "/messages")),
    add_headers(Authorization=paste("Bearer", token)),
    content_type_json(), body=payload, encode="json", timeout(30)),
    error=function(e) NULL)
  if (is.null(resp)) return(list(ok=FALSE, detalhe="Falha de conexão com o Messenger."))
  resposta_meta(resp)
}

# Publicação no Facebook Page Feed (texto/link).
publicar_facebook <- function(mensagem, link="") {
  token <- env("FB_PAGE_ACCESS_TOKEN", env("META_ACCESS_TOKEN"))
  page_id <- env("FB_PAGE_ID")
  if (!nzchar(token) || !nzchar(page_id))
    return(list(ok=FALSE, detalhe="Configure FB_PAGE_ID e token de acesso da Página."))
  payload <- list(message=mensagem)
  if (nzchar(link)) payload$link <- link
  resp <- tryCatch(POST(graph_url(paste0(page_id, "/feed")),
    add_headers(Authorization=paste("Bearer", token)),
    body=payload, encode="form", timeout(30)),
    error=function(e) NULL)
  if (is.null(resp)) return(list(ok=FALSE, detalhe="Falha de conexão com o Facebook."))
  resposta_meta(resp)
}

# Publicação Instagram: exige URL pública HTTPS da imagem.
# Etapa 1 cria container; etapa 2 publica o container.
publicar_instagram <- function(mensagem, imagem_url) {
  token <- env("META_ACCESS_TOKEN")
  ig_id <- env("IG_USER_ID")
  if (!nzchar(token) || !nzchar(ig_id))
    return(list(ok=FALSE, detalhe="Configure IG_USER_ID e META_ACCESS_TOKEN."))
  if (!grepl("^https://", imagem_url))
    return(list(ok=FALSE, detalhe="O Instagram exige uma URL HTTPS pública para a imagem."))
  r1 <- tryCatch(POST(graph_url(paste0(ig_id, "/media")),
    add_headers(Authorization=paste("Bearer", token)),
    body=list(image_url=imagem_url, caption=mensagem),
    encode="form", timeout(30)), error=function(e) NULL)
  if (is.null(r1)) return(list(ok=FALSE, detalhe="Falha ao criar mídia no Instagram."))
  a <- resposta_meta(r1)
  if (!a$ok) return(a)
  obj <- tryCatch(fromJSON(content(r1, as="text", encoding="UTF-8")),
                  error=function(e) list())
  creation_id <- obj$id
  if (is.null(creation_id) || !nzchar(creation_id))
    return(list(ok=FALSE, detalhe="A Meta não retornou o ID do conteúdo."))
  r2 <- tryCatch(POST(graph_url(paste0(ig_id, "/media_publish")),
    add_headers(Authorization=paste("Bearer", token)),
    body=list(creation_id=creation_id), encode="form", timeout(30)),
    error=function(e) NULL)
  if (is.null(r2)) return(list(ok=FALSE, detalhe="Falha ao publicar conteúdo no Instagram."))
  resposta_meta(r2)
}

# ---------------- INTERFACE ----------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$title("Campanhas de Marketing Agroindustrial"),
    tags$style(HTML("
      body { background:#f5f7f6; color:#26332c; }
      .topo { background:#174d35; color:white; padding:22px; margin:-15px -15px 20px -15px; }
      .topo h2 { margin:0; font-weight:700; }
      .sub { opacity:.88; margin-top:6px; }
      .well { background:white; border:1px solid #e1e8e3; border-radius:10px; box-shadow:0 2px 8px #00000008; }
      .btn-primary { background:#17633e; border-color:#17633e; }
      .btn-primary:hover { background:#104b2e; }
      .caixa { background:white; border:1px solid #e0e8e2; padding:16px; border-radius:10px; margin-bottom:14px; }
      .status-ok { color:#17633e; font-weight:bold; }
      .status-off { color:#a35b16; font-weight:bold; }
      .small-note { color:#68756d; font-size:12px; }
      .nav-tabs>li>a { font-weight:600; }
    "))
  ),
  div(class="topo",
    h2("Sistema de Campanhas de Marketing Agroindustrial"),
    div(class="sub", "Planejamento • Pesquisa • Comunicação • Publicação • Resultados")
  ),
  fluidRow(
    column(12,
      div(class="caixa",
        strong("Modo de operação: "),
        span("Planejamento e simulação disponíveis imediatamente. Envio/publicação dependem das credenciais e permissões oficiais da Meta."),
        br(), span(class="small-note",
          "Use somente contatos com consentimento e respeite as regras de cada canal. O aplicativo não realiza disparos em massa indiscriminados.")
      )
    )
  ),
  tabsetPanel(id="abas",
    tabPanel("Painel", br(),
      fluidRow(
        column(3, div(class="caixa", h4("Produtos"), h2(textOutput("n_produtos", inline=TRUE)))),
        column(3, div(class="caixa", h4("Contatos"), h2(textOutput("n_contatos", inline=TRUE)))),
        column(3, div(class="caixa", h4("Campanhas"), h2(textOutput("n_campanhas", inline=TRUE)))),
        column(3, div(class="caixa", h4("Envios registrados"), h2(textOutput("n_envios", inline=TRUE))))
      ),
      div(class="caixa", h4("Últimas campanhas"), DTOutput("tb_campanhas_painel")),
      div(class="caixa", h4("Situação das integrações"),
          uiOutput("status_integracoes"))
    ),
    tabPanel("Pesquisa de mercado", br(),
      fluidRow(
        column(4,
          div(class="caixa", h4("Dados da pesquisa"),
            p(class="small-note", "Importe as respostas do questionário (CSV, uma linha por respondente) ou carregue a base de exemplo (dados fictícios, apenas para aula)."),
            fileInput("pesq_arquivo", "Importar respostas (CSV)", accept=".csv"),
            actionButton("pesq_exemplo", "Carregar base de exemplo"),
            br(), br(),
            downloadButton("baixar_questionario", "Baixar roteiro do questionário"),
            hr(),
            selectInput("pesq_var", "Variável para analisar", choices=NULL),
            selectInput("pesq_cruz", "Cruzar com (opcional)", choices=NULL),
            strong(textOutput("pesq_n"))
          )
        ),
        column(8,
          div(class="caixa", h4("Distribuição de respostas"),
            plotOutput("pesq_grafico", height="320px"),
            tableOutput("pesq_tabela")),
          div(class="caixa", h4("Cruzamento de variáveis"),
            p(class="small-note", "Categórica × categórica: % na linha. Numérica × categórica: média por grupo."),
            tableOutput("pesq_cruzada")),
          div(class="caixa", h4("Do dado à campanha"),
            uiOutput("pesq_insights"))
        )
      )
    ),
    tabPanel("Produtos", br(),
      fluidRow(
        column(5,
          div(class="caixa", h4("Cadastrar produto"),
            textInput("p_nome", "Nome do produto"),
            selectInput("p_categoria", "Categoria",
              c("Farinha e derivados", "Frutas e polpas", "Leite e derivados",
                "Carnes e derivados", "Mel e derivados", "Grãos e cereais",
                "Hortaliças", "Outro")),
            textAreaInput("p_descricao", "Descrição e diferenciais", rows=3),
            numericInput("p_preco", "Preço (R$)", value=0, min=0, step=.01),
            textInput("p_unidade", "Unidade (ex.: embalagem 500 g)", "unidade"),
            textInput("p_link", "Link de compra ou catálogo (opcional)"),
            fileInput("p_imagem", "Imagem do produto", accept=c("image/png","image/jpeg","image/webp")),
            checkboxInput("p_ativo", "Produto ativo", TRUE),
            actionButton("salvar_produto", "Salvar produto", class="btn-primary")
          )
        ),
        column(7, div(class="caixa", h4("Produtos cadastrados"), DTOutput("tb_produtos")))
      )
    ),
    tabPanel("Contatos", br(),
      fluidRow(
        column(5,
          div(class="caixa", h4("Cadastrar contato"),
            textInput("c_nome", "Nome do cliente/contato"),
            textInput("c_telefone", "WhatsApp com código do país (ex.: 5565999999999)"),
            textInput("c_ig", "Instagram-scoped ID (se disponível)"),
            textInput("c_fb", "Facebook Page-scoped ID (se disponível)"),
            checkboxInput("c_consent", "Consentimento geral registrado", FALSE),
            checkboxInput("c_wa", "Autorizou receber mensagens pelo WhatsApp", FALSE),
            checkboxInput("c_igopt", "Interagiu/está elegível para mensagem no Instagram", FALSE),
            checkboxInput("c_fbopt", "Interagiu/está elegível para mensagem no Messenger", FALSE),
            textAreaInput("c_obs", "Observações / origem do consentimento", rows=2),
            actionButton("salvar_contato", "Salvar contato", class="btn-primary"),
            br(), br(),
            fileInput("importar_contatos", "Importar contatos CSV", accept=".csv"),
            downloadButton("baixar_modelo_contatos", "Baixar modelo CSV")
          )
        ),
        column(7, div(class="caixa", h4("Base de contatos"), DTOutput("tb_contatos"),
          p(class="small-note", "Não cadastre listas compradas. Registre a origem e a autorização de cada contato. IDs do Instagram/Facebook são identificadores de destinatários obtidos por interação permitida, não nomes de usuário.")
        ))
      )
    ),
    tabPanel("Criar campanha", br(),
      fluidRow(
        column(5,
          div(class="caixa", h4("Dados da campanha"),
            textInput("camp_nome", "Nome da campanha", "Sabores da Nossa Terra"),
            selectInput("camp_produto", "Produto", choices=NULL),
            selectInput("camp_objetivo", "Objetivo",
              c("Lançamento de produto", "Aumentar vendas", "Divulgar marca",
                "Conquistar clientes", "Promover oferta", "Fidelizar clientes")),
            textInput("camp_publico", "Público-alvo", "Consumidores da região"),
            textAreaInput("camp_beneficio", "Principal benefício/diferencial",
              "Qualidade, origem regional e valorização dos produtores.", rows=2),
            textInput("camp_oferta", "Oferta/preço (opcional)"),
            textInput("camp_chamada", "Chamada para ação (CTA)", "Peça pelo WhatsApp!"),
            textInput("camp_link", "Link de compra / catálogo"),
            fileInput("camp_imagem", "Imagem para divulgação", accept=c("image/png","image/jpeg","image/webp")),
            checkboxGroupInput("camp_canais", "Canais da campanha",
              choices=c("WhatsApp"="WhatsApp", "Instagram"="Instagram",
                        "Facebook"="Facebook"),
              selected=c("WhatsApp","Instagram","Facebook")),
            actionButton("gerar_texto", "Gerar texto publicitário"),
            actionButton("salvar_campanha", "Salvar campanha", class="btn-primary")
          )
        ),
        column(7,
          div(class="caixa", h4("Texto publicitário"),
            textAreaInput("camp_mensagem", "Revise e edite a mensagem", rows=12,
              placeholder="O texto da campanha aparecerá aqui..."),
            p(class="small-note", "Personalize o texto para cada rede. Inclua informações verdadeiras sobre preço, disponibilidade, origem e características do produto."),
            actionButton("copiar_instrucao", "Preparar texto para copiar")
          ),
          div(class="caixa", h4("Prévia da campanha"),
            uiOutput("previa_campanha"))
        )
      )
    ),
    tabPanel("Publicar e enviar", br(),
      div(class="caixa",
        h4("1. Selecione uma campanha salva"),
        selectInput("camp_envio", "Campanha", choices=NULL),
        p(class="small-note", "O envio de mensagens é individual e condicionado à elegibilidade do destinatário. Publicações em Instagram exigem imagem em URL HTTPS pública.")
      ),
      fluidRow(
        column(6, div(class="caixa",
          h4("2. Publicar nas páginas"),
          checkboxInput("pub_ig", "Publicar no Instagram", FALSE),
          textInput("url_imagem_publica", "URL HTTPS pública da imagem (Instagram)"),
          checkboxInput("pub_fb", "Publicar no Facebook", FALSE),
          actionButton("publicar_paginas", "Publicar conteúdo selecionado", class="btn-primary"),
          br(), br(), verbatimTextOutput("resultado_publicacao")
        )),
        column(6, div(class="caixa",
          h4("3. Enviar mensagens individuais"),
          selectInput("canal_dm", "Canal", c("WhatsApp","Instagram","Facebook")),
          radioButtons("grupo_envio", "Destinatários",
            c("Todos os contatos elegíveis"="todos", "Selecionar um contato"="um"),
            selected="todos"),
          uiOutput("seletor_contato"),
          checkboxInput("confirmo_envio",
            "Confirmo que os destinatários autorizaram o contato e que a mensagem atende às regras do canal.", FALSE),
          actionButton("enviar_mensagens", "Enviar mensagens elegíveis", class="btn-primary"),
          br(), br(), verbatimTextOutput("resultado_envio")
        ))
      ),
      div(class="caixa", h4("Regras importantes"),
        tags$ul(
          tags$li("WhatsApp: campanhas iniciadas pela empresa devem usar modelo de mensagem aprovado pela Meta; o modelo configurado neste aplicativo deve conter três variáveis no corpo: campanha, produto e chamada."),
          tags$li("Instagram e Messenger: mensagens diretas só podem ser enviadas a pessoas elegíveis, normalmente após interação com a conta/Página e dentro da janela permitida."),
          tags$li("Não use o aplicativo para spam, listas compradas, mensagens repetitivas ou contatos sem autorização."),
          tags$li("Teste primeiro com um contato próprio e revise o conteúdo antes de enviar.")
        )
      )
    ),
    tabPanel("Relatórios", br(),
      div(class="caixa", h4("Histórico de envios e publicações"), DTOutput("tb_logs"),
          downloadButton("baixar_logs", "Baixar histórico CSV")),
      div(class="caixa", h4("Resumo por canal"), DTOutput("tb_resumo"))
    ),
    tabPanel("Configuração Meta", br(),
      div(class="caixa",
        h4("Configuração das credenciais"),
        p("As credenciais devem ficar no arquivo .Renviron, nunca dentro do código compartilhado ou em planilhas de contatos."),
        tableOutput("tb_config"),
        h4("Variáveis necessárias"),
        tags$pre("META_ACCESS_TOKEN=SEU_TOKEN\nWA_PHONE_NUMBER_ID=ID_DO_TELEFONE\nWA_TEMPLATE_NAME=nome_do_modelo_aprovado\nWA_TEMPLATE_LANG=pt_BR\nIG_USER_ID=ID_DA_CONTA_PROFISSIONAL\nFB_PAGE_ID=ID_DA_PAGINA\nFB_PAGE_ACCESS_TOKEN=TOKEN_DA_PAGINA\nMETA_API_VERSION=v27.0"),
        p("Depois de editar o .Renviron, reinicie o RStudio/R para carregar as variáveis."),
        tags$ol(
          tags$li("Crie/acesse um aplicativo em Meta for Developers."),
          tags$li("Adicione os produtos WhatsApp, Instagram e Messenger conforme o canal."),
          tags$li("Vincule a conta profissional do Instagram à Página do Facebook."),
          tags$li("Configure permissões, tokens e revisão do aplicativo conforme o uso pretendido."),
          tags$li("No WhatsApp Manager, crie e obtenha aprovação de um modelo de mensagem da categoria Marketing com três variáveis no corpo."),
          tags$li("Para Instagram e Messenger, obtenha os IDs de destinatários por fluxos autorizados de interação/webhooks.")
        ),
        p("Documentação oficial:"),
        tags$ul(
          tags$li(tags$a(href="https://developers.facebook.com/documentation/business-messaging/whatsapp/overview", target="_blank", "WhatsApp Business Platform")),
          tags$li(tags$a(href="https://developers.facebook.com/documentation/business-messaging/instagram-messaging", target="_blank", "Instagram Messaging")),
          tags$li(tags$a(href="https://developers.facebook.com/docs/messenger-platform/", target="_blank", "Messenger Platform"))
        )
      )
    )
  ),
  tags$footer(class="small-note",
    "Universidade do Estado de Mato Grosso – UNEMAT • Curso de Engenharia de Produção Agroindustrial • Disciplina de Marketing • Prof. Dr. Ramão Humberto Martins Manvailer • Sistema didático")
)

# ---------------- SERVIDOR ----------------
server <- function(input, output, session) {
  rv <- reactiveValues(
    produtos=ler_csv(ARQ_PRODUTOS, campos_produtos),
    contatos=ler_csv(ARQ_CONTATOS, campos_contatos),
    campanhas=ler_csv(ARQ_CAMPANHAS, campos_campanhas),
    resultado_pub="Aguardando publicação.",
    resultado_env="Aguardando envio.",
    pesquisa=NULL
  )

  # ---------------- PESQUISA DE MERCADO ----------------
  ler_pesquisa <- function(arquivo) {
    tryCatch(utils::read.csv(arquivo, stringsAsFactors=FALSE, fileEncoding="UTF-8",
                             check.names=FALSE, na.strings=c("", "NA")),
             error=function(e) NULL)
  }
  eh_numerica <- function(v) is.numeric(v) && length(unique(stats::na.omit(v))) > 6

  observeEvent(input$pesq_arquivo, {
    x <- ler_pesquisa(input$pesq_arquivo$datapath)
    if (is.null(x) || !nrow(x)) {
      showNotification("Não foi possível ler o CSV. Use separador vírgula e codificação UTF-8.", type="error")
    } else {
      rv$pesquisa <- x
      showNotification(paste("Pesquisa carregada:", nrow(x), "respondentes."), type="message")
    }
  })
  observeEvent(input$pesq_exemplo, {
    arq <- file.path(PASTA_DADOS, "exemplo_pesquisa.csv")
    if (!file.exists(arq)) {
      showNotification("Arquivo dados/exemplo_pesquisa.csv não encontrado.", type="error")
      return(invisible(NULL))
    }
    rv$pesquisa <- ler_pesquisa(arq)
    showNotification("Base de exemplo carregada (dados fictícios).", type="message")
  })
  observe({
    x <- rv$pesquisa
    vars <- if (is.null(x)) character(0) else setdiff(names(x), "id")
    updateSelectInput(session, "pesq_var", choices=vars)
    updateSelectInput(session, "pesq_cruz", choices=c("(nenhuma)"="", vars))
  })
  output$pesq_n <- renderText({
    if (is.null(rv$pesquisa)) "Nenhuma base carregada." else paste0("Respondentes: ", nrow(rv$pesquisa))
  })
  output$pesq_grafico <- renderPlot({
    x <- rv$pesquisa
    req(x, input$pesq_var, input$pesq_var %in% names(x))
    v <- x[[input$pesq_var]]
    if (eh_numerica(v)) {
      par(mar=c(4, 4, 2, 1))
      hist(v, col="#17633e", border="white", main=input$pesq_var,
           xlab=input$pesq_var, ylab="Respondentes")
    } else {
      t <- sort(table(v), decreasing=FALSE)
      par(mar=c(4, 12, 2, 1))
      barplot(t, horiz=TRUE, las=1, col="#17633e", border=NA,
              main=input$pesq_var, xlab="Respondentes", cex.names=0.85)
    }
  })
  output$pesq_tabela <- renderTable({
    x <- rv$pesquisa
    req(x, input$pesq_var, input$pesq_var %in% names(x))
    v <- x[[input$pesq_var]]
    if (eh_numerica(v)) {
      s <- summary(v)
      data.frame(Estatística=c("Mínimo", "1º quartil", "Mediana", "Média", "3º quartil", "Máximo"),
                 Valor=round(as.numeric(s[1:6]), 2), check.names=FALSE)
    } else {
      t <- sort(table(v), decreasing=TRUE)
      data.frame(Resposta=names(t), Frequência=as.integer(t),
                 Percentual=sprintf("%.1f%%", 100 * as.numeric(t) / sum(t)),
                 check.names=FALSE)
    }
  })
  output$pesq_cruzada <- renderTable({
    x <- rv$pesquisa
    req(x, input$pesq_var, input$pesq_cruz, nzchar(input$pesq_cruz),
        input$pesq_var %in% names(x), input$pesq_cruz %in% names(x),
        input$pesq_cruz != input$pesq_var)
    a <- x[[input$pesq_var]]; b <- x[[input$pesq_cruz]]
    if (eh_numerica(a) && eh_numerica(b)) {
      data.frame(Indicador="Correlação de Pearson",
                 Valor=round(stats::cor(a, b, use="complete.obs"), 3))
    } else if (eh_numerica(a) || eh_numerica(b)) {
      num <- if (eh_numerica(a)) a else b
      grp <- as.character(if (eh_numerica(a)) b else a)
      medias <- tapply(num, grp, function(z) mean(z, na.rm=TRUE))
      ns <- tapply(num, grp, function(z) sum(!is.na(z)))
      data.frame(Grupo=names(medias), Média=round(as.numeric(medias), 2),
                 n=as.integer(ns[names(medias)]), check.names=FALSE)
    } else {
      t <- prop.table(table(a, b), 1) * 100
      m <- as.data.frame.matrix(round(t, 1))
      cbind(Grupo=rownames(m), m)
    }
  })
  output$pesq_insights <- renderUI({
    x <- rv$pesquisa
    if (is.null(x)) return(p(class="small-note", "Carregue uma base para ver as sugestões."))
    mais_citado <- function(col) {
      if (!col %in% names(x)) return(NA_character_)
      t <- sort(table(x[[col]]), decreasing=TRUE)
      if (!length(t)) return(NA_character_)
      sprintf("%s (%.0f%%)", names(t)[1], 100 * t[[1]] / sum(t))
    }
    regras <- list(
      c("atributo_principal", "Atributo mais valorizado", "deve ser o argumento central da mensagem (benefício)."),
      c("canal_info", "Canal onde o público se informa", "deve ser o canal prioritário da campanha."),
      c("local_compra", "Local de compra preferido", "indica onde a oferta precisa estar disponível (praça)."),
      c("barreira_compra", "Principal barreira de compra", "deve ser respondida na mensagem ou na oferta."),
      c("formato_conteudo", "Formato de conteúdo preferido", "orienta o tipo de peça (vídeo, foto, receita etc.).")
    )
    itens <- lapply(regras, function(r) {
      v <- mais_citado(r[1])
      if (is.na(v)) NULL else tags$li(strong(r[2], ": "), v, " → ", r[3])
    })
    dp <- if ("disposicao_pagar_kg" %in% names(x) && is.numeric(x$disposicao_pagar_kg)) {
      tags$li(strong("Disposição a pagar (mediana): "),
              sprintf("R$ %.2f por kg", stats::median(x$disposicao_pagar_kg, na.rm=TRUE)),
              " → compare com o custo e o preço praticado antes de definir a oferta.")
    } else NULL
    tagList(tags$ul(itens, dp),
      p(class="small-note", "Resultados de amostra por conveniência descrevem os respondentes, não toda a população. Use-os como hipóteses para a campanha."))
  })
  output$baixar_questionario <- downloadHandler(
    filename=function() "roteiro_questionario_pesquisa.txt",
    content=function(file) {
      arq <- file.path(PASTA_DADOS, "roteiro_questionario.txt")
      if (file.exists(arq)) file.copy(arq, file) else writeLines("Roteiro não encontrado.", file)
    }
  )

  atualizar_escolhas <- function() {
    produtos <- rv$produtos$produto[rv$produtos$ativo %in% c("TRUE","true","1","Sim")]
    updateSelectInput(session, "camp_produto",
      choices=if (length(produtos)) produtos else character(0))
    nomes <- if (nrow(rv$campanhas)) paste0(seq_len(nrow(rv$campanhas)), " - ", rv$campanhas$nome) else character(0)
    updateSelectInput(session, "camp_envio", choices=nomes)
  }
  observe({ atualizar_escolhas() })

  output$n_produtos <- renderText(nrow(rv$produtos))
  output$n_contatos <- renderText(nrow(rv$contatos))
  output$n_campanhas <- renderText(nrow(rv$campanhas))
  output$n_envios <- renderText(tryCatch(nrow(read.csv(ARQ_LOG)), error=function(e) 0))

  output$status_integracoes <- renderUI({
    status <- list(
      "WhatsApp" = nzchar(env("META_ACCESS_TOKEN")) && nzchar(env("WA_PHONE_NUMBER_ID")) && nzchar(env("WA_TEMPLATE_NAME")),
      "Instagram" = nzchar(env("META_ACCESS_TOKEN")) && nzchar(env("IG_USER_ID")),
      "Facebook" = nzchar(env("FB_PAGE_ACCESS_TOKEN", env("META_ACCESS_TOKEN"))) && nzchar(env("FB_PAGE_ID"))
    )
    tagList(lapply(names(status), function(nm) {
      div(if (status[[nm]]) class="status-ok" else class="status-off",
          paste0(if (status[[nm]]) "● " else "○ ", nm,
                 if (status[[nm]]) " — credenciais detectadas" else " — configuração pendente"))
    }))
  })

  output$tb_config <- renderTable({
    data.frame(Variável=c("Token Meta","ID telefone WhatsApp","Modelo WhatsApp",
      "ID Instagram profissional","ID Página Facebook","Token Página Facebook"),
      Situação=c(
        ifelse(nzchar(env("META_ACCESS_TOKEN")),"Detectado","Pendente"),
        ifelse(nzchar(env("WA_PHONE_NUMBER_ID")),"Detectado","Pendente"),
        ifelse(nzchar(env("WA_TEMPLATE_NAME")),"Detectado","Pendente"),
        ifelse(nzchar(env("IG_USER_ID")),"Detectado","Pendente"),
        ifelse(nzchar(env("FB_PAGE_ID")),"Detectado","Pendente"),
        ifelse(nzchar(env("FB_PAGE_ACCESS_TOKEN")),"Detectado","Pendente")
      ), check.names=FALSE)
  })

  output$tb_produtos <- renderDT(datatable(rv$produtos, options=list(pageLength=5, scrollX=TRUE)))
  output$tb_contatos <- renderDT(datatable(rv$contatos, options=list(pageLength=7, scrollX=TRUE)))
  output$tb_campanhas <- renderDT(datatable(rv$campanhas, options=list(pageLength=5, scrollX=TRUE)))
  output$tb_campanhas_painel <- renderDT(datatable(head(rv$campanhas, 5),
    options=list(dom="t", pageLength=5, scrollX=TRUE)))
  output$tb_logs <- renderDT({
    x <- tryCatch(read.csv(ARQ_LOG, stringsAsFactors=FALSE, fileEncoding="UTF-8"),
                  error=function(e) data.frame())
    datatable(x, options=list(pageLength=10, scrollX=TRUE))
  })
  output$tb_resumo <- renderDT({
    x <- tryCatch(read.csv(ARQ_LOG, stringsAsFactors=FALSE), error=function(e) data.frame())
    if (!nrow(x)) return(datatable(data.frame(Informacao="Nenhum envio registrado.")))
    datatable(as.data.frame(table(Canal=x$canal, Resultado=x$resultado)),
      options=list(dom="t", pageLength=10))
  })
  output$baixar_logs <- downloadHandler(
    filename=function() paste0("historico_campanhas_", Sys.Date(), ".csv"),
    content=function(file) file.copy(ARQ_LOG, file)
  )
  output$baixar_modelo_contatos <- downloadHandler(
    filename=function() "modelo_contatos.csv",
    content=function(file) {
      modelo <- data.frame(nome="Cliente Exemplo", telefone="5565999999999",
        instagram_id="", facebook_id="", consentimento="FALSE",
        whatsapp_optin="FALSE", instagram_optin="FALSE", facebook_optin="FALSE",
        observacao="Registrar origem e data do consentimento")
      write.csv(modelo, file, row.names=FALSE, fileEncoding="UTF-8")
    }
  )

  observeEvent(input$salvar_produto, {
    req(nzchar(trimws(input$p_nome)))
    img <- ""
    if (!is.null(input$p_imagem)) {
      ext <- tools::file_ext(input$p_imagem$name)
      img <- file.path(PASTA_MIDIA, paste0("produto_", as.integer(Sys.time()), ".", ext))
      file.copy(input$p_imagem$datapath, img, overwrite=TRUE)
    }
    novo <- data.frame(produto=input$p_nome, categoria=input$p_categoria,
      descricao=input$p_descricao, preco=as.character(input$p_preco),
      unidade=input$p_unidade, imagem=img, link=input$p_link,
      ativo=as.character(input$p_ativo), stringsAsFactors=FALSE)
    rv$produtos <- bind_rows(rv$produtos, novo)
    gravar_csv(rv$produtos, ARQ_PRODUTOS)
    atualizar_escolhas()
    showNotification("Produto cadastrado.", type="message")
  })

  observeEvent(input$salvar_contato, {
    req(nzchar(trimws(input$c_nome)))
    novo <- data.frame(nome=input$c_nome, telefone=input$c_telefone,
      instagram_id=input$c_ig, facebook_id=input$c_fb,
      consentimento=as.character(input$c_consent),
      whatsapp_optin=as.character(input$c_wa),
      instagram_optin=as.character(input$c_igopt),
      facebook_optin=as.character(input$c_fbopt),
      observacao=input$c_obs, stringsAsFactors=FALSE)
    rv$contatos <- bind_rows(rv$contatos, novo)
    gravar_csv(rv$contatos, ARQ_CONTATOS)
    showNotification("Contato cadastrado.", type="message")
  })

  observeEvent(input$importar_contatos, {
    req(input$importar_contatos$datapath)
    x <- tryCatch(read.csv(input$importar_contatos$datapath,
      stringsAsFactors=FALSE, fileEncoding="UTF-8"), error=function(e) NULL)
    if (is.null(x) || !all(campos_contatos %in% names(x))) {
      showNotification("CSV inválido. Baixe o modelo e mantenha os nomes das colunas.", type="error")
    } else {
      rv$contatos <- bind_rows(rv$contatos, x[, campos_contatos])
      gravar_csv(rv$contatos, ARQ_CONTATOS)
      showNotification("Contatos importados.", type="message")
    }
  })

  observeEvent(input$gerar_texto, {
    produto <- input$camp_produto
    if (is.null(produto) || !nzchar(produto)) produto <- "nosso produto"
    oferta <- if (nzchar(input$camp_oferta)) paste0("\nOferta: ", input$camp_oferta, ".") else ""
    msg <- paste0("🌱 ", input$camp_nome, "\n\n",
      "Conheça ", produto, "!\n",
      input$camp_beneficio, oferta, "\n\n",
      input$camp_chamada,
      if (nzchar(input$camp_link)) paste0("\n", input$camp_link) else "",
      "\n\nSe não desejar receber novas comunicações, avise-nos.")
    updateTextAreaInput(session, "camp_mensagem", value=msg)
  })

  observeEvent(input$salvar_campanha, {
    req(nzchar(trimws(input$camp_nome)), nzchar(trimws(input$camp_mensagem)))
    img <- ""
    if (!is.null(input$camp_imagem)) {
      ext <- tools::file_ext(input$camp_imagem$name)
      img <- file.path(PASTA_MIDIA, paste0("campanha_", as.integer(Sys.time()), ".", ext))
      file.copy(input$camp_imagem$datapath, img, overwrite=TRUE)
    }
    novo <- data.frame(data_criacao=as.character(Sys.Date()), nome=input$camp_nome,
      produto=ifelse(is.null(input$camp_produto), "", input$camp_produto),
      objetivo=input$camp_objetivo, publico=input$camp_publico,
      mensagem=input$camp_mensagem, chamada=input$camp_chamada,
      link=input$camp_link, imagem=img,
      canais=paste(input$camp_canais, collapse="; "), status="Planejada",
      stringsAsFactors=FALSE)
    rv$campanhas <- bind_rows(rv$campanhas, novo)
    gravar_csv(rv$campanhas, ARQ_CAMPANHAS)
    atualizar_escolhas()
    showNotification("Campanha salva com sucesso.", type="message")
  })

  output$previa_campanha <- renderUI({
    div(style="white-space:pre-wrap;background:#f5f8f5;padding:14px;border-radius:8px;",
        if (nzchar(input$camp_mensagem)) input$camp_mensagem else "A prévia aparecerá aqui.")
  })

  output$seletor_contato <- renderUI({
    if (identical(input$grupo_envio, "um")) {
      selectInput("contato_unico", "Contato",
        choices=setNames(seq_len(nrow(rv$contatos)), rv$contatos$nome))
    } else NULL
  })

  campanha_selecionada <- reactive({
    req(input$camp_envio, nrow(rv$campanhas)>0)
    idx <- as.integer(sub(" .*", "", input$camp_envio))
    validate(need(!is.na(idx) && idx>=1 && idx<=nrow(rv$campanhas), "Selecione uma campanha válida."))
    rv$campanhas[idx, , drop=FALSE]
  })

  observeEvent(input$publicar_paginas, {
    req(input$pub_ig || input$pub_fb)
    c <- campanha_selecionada()
    res <- character()
    if (isTRUE(input$pub_ig)) {
      if (!nzchar(input$url_imagem_publica)) {
        res <- c(res, "Instagram: informe uma URL HTTPS pública da imagem.")
      } else {
        r <- publicar_instagram(c$mensagem, input$url_imagem_publica)
        registrar(c$nome, "Instagram publicação", env("IG_USER_ID"),
                  ifelse(r$ok,"Sucesso","Falha"), r$detalhe)
        res <- c(res, paste("Instagram:", ifelse(r$ok,"Publicado","Falha"), r$detalhe))
      }
    }
    if (isTRUE(input$pub_fb)) {
      r <- publicar_facebook(c$mensagem, c$link)
      registrar(c$nome, "Facebook publicação", env("FB_PAGE_ID"),
                ifelse(r$ok,"Sucesso","Falha"), r$detalhe)
      res <- c(res, paste("Facebook:", ifelse(r$ok,"Publicado","Falha"), r$detalhe))
    }
    rv$resultado_pub <- paste(res, collapse="\n")
  })
  output$resultado_publicacao <- renderText(rv$resultado_pub)

  observeEvent(input$enviar_mensagens, {
    req(isTRUE(input$confirmo_envio))
    c <- campanha_selecionada()
    contatos <- rv$contatos
    if (identical(input$grupo_envio, "um")) {
      req(input$contato_unico)
      contatos <- contatos[as.integer(input$contato_unico), , drop=FALSE]
    }
    canal <- input$canal_dm
    elegivel <- rep(FALSE, nrow(contatos))
    if (canal=="WhatsApp") {
      elegivel <- tolower(contatos$consentimento)=="true" &
                  tolower(contatos$whatsapp_optin)=="true" &
                  nzchar(contatos$telefone)
    } else if (canal=="Instagram") {
      elegivel <- tolower(contatos$consentimento)=="true" &
                  tolower(contatos$instagram_optin)=="true" &
                  nzchar(contatos$instagram_id)
    } else {
      elegivel <- tolower(contatos$consentimento)=="true" &
                  tolower(contatos$facebook_optin)=="true" &
                  nzchar(contatos$facebook_id)
    }
    alvos <- contatos[elegivel, , drop=FALSE]
    if (!nrow(alvos)) {
      rv$resultado_env <- "Nenhum contato elegível. Verifique consentimento, opt-in e identificador do canal."
    } else {
      saidas <- character()
      for (i in seq_len(nrow(alvos))) {
        if (canal=="WhatsApp") {
          r <- enviar_whatsapp_template(alvos$telefone[i], c$nome, c$produto, c$chamada)
          dest <- alvos$telefone[i]
        } else if (canal=="Instagram") {
          r <- enviar_instagram_dm(alvos$instagram_id[i], c$mensagem)
          dest <- alvos$instagram_id[i]
        } else {
          r <- enviar_facebook_dm(alvos$facebook_id[i], c$mensagem)
          dest <- alvos$facebook_id[i]
        }
        registrar(c$nome, canal, dest, ifelse(r$ok,"Sucesso","Falha"), r$detalhe)
        saidas <- c(saidas, paste(alvos$nome[i], "—", ifelse(r$ok,"OK","ERRO"), r$detalhe))
        Sys.sleep(0.3)
      }
      rv$resultado_env <- paste0("Processados ", nrow(alvos), " contato(s):\n",
                                 paste(saidas, collapse="\n"))
    }
  })
  output$resultado_envio <- renderText(rv$resultado_env)

  observeEvent(input$copiar_instrucao, {
    showNotification("Selecione o texto na caixa e use Ctrl+C para copiar.", type="message")
  })
}

shinyApp(ui, server)
