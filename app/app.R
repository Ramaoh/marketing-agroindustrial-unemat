# ============================================================
# SISTEMA DE CAMPANHAS DE MARKETING AGROINDUSTRIAL
# Aplicativo Shiny - WhatsApp, Instagram e Facebook (Meta APIs)
# ============================================================

options(shiny.maxRequestSize = 10 * 1024^2)

pacotes <- c("shiny", "DT", "httr", "jsonlite", "readr", "dplyr", "shinyjs", "png", "jpeg", "webp")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) {
  message("Instalando pacotes que faltam: ", paste(faltantes, collapse = ", "))
  install.packages(faltantes, repos = "https://cloud.r-project.org")
  faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltantes)) stop(
    "Não foi possível instalar: ", paste(faltantes, collapse = ", "),
    ". Verifique a internet e rode install.packages(c(",
    paste(sprintf('"%s"', faltantes), collapse = ", "), "))")
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
# permite que o navegador mostre as imagens salvas em dados/midia
shiny::addResourcePath("midia", normalizePath(PASTA_MIDIA, winslash = "/"))
# imagem embutida (data URI): aparece em qualquer pasta, sem depender de links
img_data_uri <- function(caminho) {
  if (is.null(caminho) || is.na(caminho) || !nzchar(caminho) || !file.exists(caminho)) return(NA_character_)
  ext <- tolower(tools::file_ext(caminho))
  mime <- switch(ext, png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
                 webp = "image/webp", gif = "image/gif", "image/jpeg")
  bin <- readBin(caminho, "raw", file.info(caminho)$size)
  paste0("data:", mime, ";base64,", jsonlite::base64_enc(bin))
}
VERSAO_APP <- "versão 18/11/2026 – v6 (WhatsApp, Facebook e Instagram)"

# ---------------- ARQUIVOS DE EXEMPLO EMBUTIDOS ----------------
# Garantem que a base de exemplo, o roteiro e os produtos existam mesmo
# quando o app.R é executado sozinho, fora da pasta original.
EXEMPLO_PESQUISA <- r"---(id,faixa_etaria,renda_familiar,local_compra,frequencia_compra,atributo_principal,conhece_produtor_local,canal_info,formato_conteudo,barreira_compra,disposicao_pagar_kg,intencao_compra,satisfacao_atual
1,18 a 24,2 a 4 SM,Feira livre,Raramente,Qualidade/sabor,Sim,Instagram,Receita,Dúvida sobre higiene,8.50,4,3
2,35 a 44,Até 2 SM,Feira livre,Semanal,Higiene e selo de inspeção,Sim,Indicação de conhecidos,História do produtor,Não conheço a marca,6.50,4,4
3,60 ou mais,Acima de 8 SM,Direto do produtor,Raramente,Preço,Sim,WhatsApp,Receita,Não encontro onde comprar,11.00,5,3
4,18 a 24,4 a 8 SM,Supermercado,Quinzenal,Preço,Não,Instagram,Vídeo curto,Preço alto,10.00,3,2
5,35 a 44,Acima de 8 SM,Supermercado,Quinzenal,Embalagem,Não,Facebook,Vídeo curto,Não encontro onde comprar,16.00,4,3
6,18 a 24,2 a 4 SM,Feira livre,Quinzenal,Qualidade/sabor,Não,Instagram,Foto com preço,Preço alto,8.00,5,2
7,35 a 44,2 a 4 SM,Feira livre,Quinzenal,Higiene e selo de inspeção,Não,Instagram,Promoção/cupom,Preço alto,11.00,4,4
8,35 a 44,2 a 4 SM,Feira livre,Quinzenal,Embalagem,Sim,Instagram,Receita,Dúvida sobre higiene,12.00,5,2
9,60 ou mais,4 a 8 SM,Supermercado,Raramente,Qualidade/sabor,Não,Indicação de conhecidos,Receita,Preço alto,18.00,2,4
10,18 a 24,Até 2 SM,Feira livre,Semanal,Origem regional,Não,Instagram,Vídeo curto,Não encontro onde comprar,9.50,2,2
11,18 a 24,4 a 8 SM,Supermercado,Mensal,Higiene e selo de inspeção,Não,Instagram,Receita,Não encontro onde comprar,12.50,5,3
12,35 a 44,4 a 8 SM,Supermercado,Quinzenal,Origem regional,Não,Indicação de conhecidos,Receita,Não encontro onde comprar,10.50,3,1
13,45 a 59,Acima de 8 SM,Supermercado,Quinzenal,Higiene e selo de inspeção,Não,WhatsApp,Vídeo curto,Não encontro onde comprar,16.00,3,3
14,45 a 59,Até 2 SM,Feira livre,Quinzenal,Preço,Sim,WhatsApp,Promoção/cupom,Não conheço a marca,10.00,4,3
15,35 a 44,2 a 4 SM,Supermercado,Mensal,Origem regional,Sim,Instagram,Foto com preço,Nenhuma,15.00,4,4
16,45 a 59,Até 2 SM,Direto do produtor,Raramente,Qualidade/sabor,Não,Indicação de conhecidos,Receita,Nenhuma,8.00,5,5
17,25 a 34,2 a 4 SM,Feira livre,Semanal,Qualidade/sabor,Não,Instagram,Foto com preço,Dúvida sobre higiene,10.00,4,3
18,18 a 24,4 a 8 SM,Direto do produtor,Mensal,Higiene e selo de inspeção,Sim,Rádio local,Promoção/cupom,Não conheço a marca,15.50,3,4
19,45 a 59,2 a 4 SM,Mercearia do bairro,Quinzenal,Qualidade/sabor,Não,Facebook,Receita,Não encontro onde comprar,11.00,2,3
20,25 a 34,Até 2 SM,Feira livre,Semanal,Preço,Não,WhatsApp,História do produtor,Dúvida sobre higiene,8.00,4,3
21,35 a 44,2 a 4 SM,Supermercado,Mensal,Qualidade/sabor,Sim,WhatsApp,Vídeo curto,Preço alto,14.00,3,3
22,18 a 24,Até 2 SM,Supermercado,Mensal,Preço,Não,Instagram,História do produtor,Não conheço a marca,8.50,3,2
23,60 ou mais,2 a 4 SM,Supermercado,Quinzenal,Embalagem,Não,Facebook,Promoção/cupom,Não conheço a marca,13.00,3,4
24,45 a 59,2 a 4 SM,Supermercado,Mensal,Qualidade/sabor,Não,Indicação de conhecidos,Foto com preço,Dúvida sobre higiene,11.00,3,5
25,35 a 44,2 a 4 SM,Feira livre,Mensal,Qualidade/sabor,Não,WhatsApp,Foto com preço,Preço alto,10.00,2,4
26,25 a 34,2 a 4 SM,Supermercado,Quinzenal,Qualidade/sabor,Sim,Instagram,História do produtor,Nenhuma,12.00,4,2
27,25 a 34,Até 2 SM,Supermercado,Quinzenal,Preço,Sim,Instagram,Promoção/cupom,Não conheço a marca,9.50,3,2
28,35 a 44,Até 2 SM,Feira livre,Mensal,Preço,Não,WhatsApp,Promoção/cupom,Preço alto,6.00,4,3
29,45 a 59,Acima de 8 SM,Mercearia do bairro,Quinzenal,Embalagem,Não,WhatsApp,Promoção/cupom,Preço alto,16.00,4,2
30,35 a 44,4 a 8 SM,Feira livre,Semanal,Origem regional,Sim,Rádio local,Promoção/cupom,Dúvida sobre higiene,13.50,5,2
31,45 a 59,2 a 4 SM,Supermercado,Quinzenal,Origem regional,Sim,Indicação de conhecidos,Promoção/cupom,Não encontro onde comprar,13.50,5,2
32,60 ou mais,2 a 4 SM,Mercearia do bairro,Quinzenal,Origem regional,Sim,Facebook,Vídeo curto,Dúvida sobre higiene,12.00,4,4
33,18 a 24,2 a 4 SM,Direto do produtor,Semanal,Qualidade/sabor,Não,Instagram,Promoção/cupom,Não encontro onde comprar,11.50,3,2
34,35 a 44,4 a 8 SM,Mercearia do bairro,Raramente,Embalagem,Sim,Facebook,Foto com preço,Não conheço a marca,12.00,5,5
35,35 a 44,4 a 8 SM,Feira livre,Semanal,Preço,Sim,WhatsApp,Foto com preço,Não conheço a marca,16.00,5,1
36,45 a 59,2 a 4 SM,Feira livre,Mensal,Preço,Não,WhatsApp,Receita,Dúvida sobre higiene,11.00,3,2
37,35 a 44,Até 2 SM,Direto do produtor,Semanal,Preço,Sim,Indicação de conhecidos,Foto com preço,Preço alto,7.00,3,4
38,45 a 59,2 a 4 SM,Supermercado,Mensal,Qualidade/sabor,Não,WhatsApp,História do produtor,Preço alto,10.00,3,2
39,18 a 24,4 a 8 SM,Feira livre,Semanal,Qualidade/sabor,Não,Instagram,Promoção/cupom,Preço alto,15.00,4,3
40,35 a 44,Até 2 SM,Supermercado,Quinzenal,Preço,Sim,Indicação de conhecidos,História do produtor,Preço alto,13.50,3,1
41,60 ou mais,4 a 8 SM,Mercearia do bairro,Mensal,Preço,Sim,WhatsApp,Foto com preço,Nenhuma,12.50,4,3
42,25 a 34,2 a 4 SM,Feira livre,Mensal,Preço,Não,Instagram,Vídeo curto,Não encontro onde comprar,10.00,5,4
43,25 a 34,Até 2 SM,Supermercado,Quinzenal,Preço,Não,WhatsApp,História do produtor,Não conheço a marca,9.50,4,3
44,35 a 44,2 a 4 SM,Supermercado,Quinzenal,Preço,Não,Facebook,Receita,Não encontro onde comprar,11.50,3,4
45,25 a 34,4 a 8 SM,Mercearia do bairro,Raramente,Higiene e selo de inspeção,Não,Instagram,Foto com preço,Não conheço a marca,13.50,5,3
46,35 a 44,Até 2 SM,Direto do produtor,Quinzenal,Origem regional,Sim,WhatsApp,Promoção/cupom,Preço alto,10.50,4,3
47,25 a 34,Até 2 SM,Direto do produtor,Mensal,Preço,Não,Instagram,Foto com preço,Não encontro onde comprar,6.50,4,3
48,25 a 34,Até 2 SM,Mercearia do bairro,Raramente,Qualidade/sabor,Sim,Rádio local,Promoção/cupom,Não conheço a marca,11.00,4,3
49,35 a 44,Até 2 SM,Feira livre,Semanal,Preço,Não,Facebook,História do produtor,Não encontro onde comprar,10.00,3,2
50,25 a 34,Acima de 8 SM,Supermercado,Mensal,Preço,Sim,Instagram,Promoção/cupom,Dúvida sobre higiene,13.50,5,4
51,45 a 59,Até 2 SM,Feira livre,Semanal,Origem regional,Não,WhatsApp,Foto com preço,Não conheço a marca,10.00,4,3
52,25 a 34,Até 2 SM,Feira livre,Quinzenal,Preço,Sim,WhatsApp,Foto com preço,Não conheço a marca,9.00,3,4
53,18 a 24,2 a 4 SM,Direto do produtor,Semanal,Qualidade/sabor,Sim,Instagram,História do produtor,Dúvida sobre higiene,11.50,5,4
54,45 a 59,2 a 4 SM,Feira livre,Mensal,Qualidade/sabor,Sim,Rádio local,Foto com preço,Não encontro onde comprar,12.50,5,3
55,35 a 44,4 a 8 SM,Direto do produtor,Raramente,Origem regional,Sim,Rádio local,Receita,Não encontro onde comprar,17.50,3,3
56,45 a 59,2 a 4 SM,Supermercado,Semanal,Origem regional,Sim,Rádio local,Promoção/cupom,Não encontro onde comprar,13.50,5,3
57,60 ou mais,4 a 8 SM,Supermercado,Quinzenal,Qualidade/sabor,Não,Rádio local,Promoção/cupom,Preço alto,15.50,4,1
58,35 a 44,2 a 4 SM,Feira livre,Raramente,Origem regional,Sim,WhatsApp,História do produtor,Preço alto,11.50,3,5
59,35 a 44,2 a 4 SM,Supermercado,Quinzenal,Qualidade/sabor,Não,WhatsApp,Promoção/cupom,Não conheço a marca,9.50,4,2
60,35 a 44,Até 2 SM,Mercearia do bairro,Semanal,Qualidade/sabor,Sim,Facebook,Foto com preço,Nenhuma,8.50,4,3
61,35 a 44,Até 2 SM,Mercearia do bairro,Raramente,Origem regional,Sim,Instagram,Foto com preço,Nenhuma,10.50,3,1
62,25 a 34,2 a 4 SM,Feira livre,Quinzenal,Higiene e selo de inspeção,Não,Instagram,Foto com preço,Não encontro onde comprar,11.00,4,3
63,18 a 24,2 a 4 SM,Supermercado,Quinzenal,Preço,Sim,Instagram,História do produtor,Não encontro onde comprar,9.00,3,4
64,25 a 34,Até 2 SM,Supermercado,Semanal,Preço,Sim,WhatsApp,Promoção/cupom,Preço alto,8.00,4,3
65,25 a 34,4 a 8 SM,Feira livre,Raramente,Qualidade/sabor,Sim,Indicação de conhecidos,Vídeo curto,Dúvida sobre higiene,14.50,3,4
66,18 a 24,Acima de 8 SM,Mercearia do bairro,Mensal,Higiene e selo de inspeção,Não,Indicação de conhecidos,Promoção/cupom,Nenhuma,14.00,5,3
67,60 ou mais,Até 2 SM,Supermercado,Mensal,Preço,Sim,Facebook,Vídeo curto,Nenhuma,11.00,5,2
68,35 a 44,2 a 4 SM,Feira livre,Semanal,Preço,Não,Instagram,Foto com preço,Não encontro onde comprar,10.00,3,3
69,45 a 59,Até 2 SM,Supermercado,Mensal,Preço,Sim,WhatsApp,História do produtor,Não encontro onde comprar,10.00,5,3
70,60 ou mais,4 a 8 SM,Direto do produtor,Mensal,Preço,Não,Facebook,Foto com preço,Preço alto,11.50,3,3
71,25 a 34,Acima de 8 SM,Feira livre,Quinzenal,Preço,Não,Instagram,História do produtor,Não conheço a marca,13.50,3,3
72,25 a 34,2 a 4 SM,Direto do produtor,Mensal,Qualidade/sabor,Não,Rádio local,Foto com preço,Dúvida sobre higiene,12.50,3,2
73,25 a 34,2 a 4 SM,Mercearia do bairro,Mensal,Preço,Não,WhatsApp,Vídeo curto,Não conheço a marca,10.00,4,3
74,25 a 34,Até 2 SM,Feira livre,Semanal,Preço,Sim,Indicação de conhecidos,Foto com preço,Não encontro onde comprar,10.50,4,4
75,45 a 59,4 a 8 SM,Direto do produtor,Quinzenal,Origem regional,Sim,WhatsApp,Foto com preço,Não encontro onde comprar,14.50,4,4
76,45 a 59,4 a 8 SM,Feira livre,Raramente,Origem regional,Sim,WhatsApp,Foto com preço,Não conheço a marca,17.50,4,2
77,35 a 44,2 a 4 SM,Feira livre,Raramente,Origem regional,Sim,Rádio local,Foto com preço,Preço alto,15.50,4,3
78,35 a 44,2 a 4 SM,Feira livre,Quinzenal,Preço,Sim,WhatsApp,Vídeo curto,Não conheço a marca,11.50,4,4
79,25 a 34,Acima de 8 SM,Feira livre,Mensal,Higiene e selo de inspeção,Não,Instagram,História do produtor,Nenhuma,12.00,4,3
80,25 a 34,2 a 4 SM,Feira livre,Mensal,Higiene e selo de inspeção,Não,Indicação de conhecidos,Receita,Preço alto,13.00,4,2
81,18 a 24,Até 2 SM,Supermercado,Mensal,Preço,Não,Instagram,Vídeo curto,Não encontro onde comprar,9.00,4,3
82,35 a 44,2 a 4 SM,Mercearia do bairro,Mensal,Preço,Sim,Rádio local,Vídeo curto,Não encontro onde comprar,11.50,4,4
83,35 a 44,Até 2 SM,Supermercado,Mensal,Higiene e selo de inspeção,Não,Instagram,Receita,Não encontro onde comprar,9.00,4,1
84,18 a 24,Acima de 8 SM,Direto do produtor,Quinzenal,Embalagem,Sim,Instagram,Foto com preço,Nenhuma,16.00,4,3
85,35 a 44,4 a 8 SM,Mercearia do bairro,Quinzenal,Preço,Sim,Indicação de conhecidos,Receita,Preço alto,12.00,4,4
86,45 a 59,Até 2 SM,Direto do produtor,Mensal,Preço,Sim,Indicação de conhecidos,Foto com preço,Não conheço a marca,6.50,3,3
87,25 a 34,4 a 8 SM,Mercearia do bairro,Quinzenal,Higiene e selo de inspeção,Não,WhatsApp,Foto com preço,Não encontro onde comprar,12.00,3,3
88,45 a 59,Até 2 SM,Mercearia do bairro,Mensal,Preço,Sim,Facebook,Promoção/cupom,Preço alto,11.50,4,3
89,60 ou mais,2 a 4 SM,Supermercado,Quinzenal,Embalagem,Não,Facebook,Foto com preço,Preço alto,10.00,3,4
90,45 a 59,2 a 4 SM,Feira livre,Mensal,Preço,Sim,Indicação de conhecidos,Foto com preço,Preço alto,11.50,3,4
91,25 a 34,2 a 4 SM,Feira livre,Raramente,Qualidade/sabor,Não,Indicação de conhecidos,Vídeo curto,Preço alto,11.50,4,4
92,18 a 24,Até 2 SM,Supermercado,Semanal,Origem regional,Não,Rádio local,Receita,Não encontro onde comprar,9.00,3,2
93,35 a 44,Até 2 SM,Supermercado,Quinzenal,Qualidade/sabor,Não,Rádio local,História do produtor,Não encontro onde comprar,7.00,3,3
94,60 ou mais,2 a 4 SM,Supermercado,Mensal,Qualidade/sabor,Não,Facebook,Promoção/cupom,Preço alto,12.00,4,3
95,18 a 24,2 a 4 SM,Mercearia do bairro,Semanal,Preço,Sim,Instagram,Receita,Dúvida sobre higiene,8.00,5,3
96,45 a 59,4 a 8 SM,Feira livre,Quinzenal,Higiene e selo de inspeção,Não,WhatsApp,Foto com preço,Dúvida sobre higiene,12.00,4,3
97,18 a 24,2 a 4 SM,Supermercado,Quinzenal,Qualidade/sabor,Não,Instagram,História do produtor,Não encontro onde comprar,12.00,4,4
98,18 a 24,2 a 4 SM,Direto do produtor,Quinzenal,Preço,Não,WhatsApp,Vídeo curto,Não conheço a marca,11.50,3,2
99,35 a 44,2 a 4 SM,Supermercado,Mensal,Embalagem,Não,Rádio local,Receita,Não conheço a marca,10.50,5,1
100,25 a 34,Acima de 8 SM,Direto do produtor,Quinzenal,Embalagem,Não,WhatsApp,Foto com preço,Nenhuma,14.50,5,3
101,25 a 34,Até 2 SM,Mercearia do bairro,Mensal,Qualidade/sabor,Não,Instagram,Promoção/cupom,Não conheço a marca,11.50,2,3
102,45 a 59,2 a 4 SM,Mercearia do bairro,Semanal,Embalagem,Sim,Facebook,Foto com preço,Não encontro onde comprar,13.50,5,3
103,45 a 59,2 a 4 SM,Feira livre,Semanal,Higiene e selo de inspeção,Não,Instagram,História do produtor,Não conheço a marca,14.00,4,4
104,60 ou mais,2 a 4 SM,Feira livre,Mensal,Preço,Não,Facebook,Promoção/cupom,Dúvida sobre higiene,9.50,2,4
105,18 a 24,2 a 4 SM,Feira livre,Raramente,Preço,Sim,Indicação de conhecidos,Promoção/cupom,Nenhuma,10.50,5,3
106,60 ou mais,2 a 4 SM,Direto do produtor,Semanal,Higiene e selo de inspeção,Não,WhatsApp,Foto com preço,Dúvida sobre higiene,12.50,3,2
107,45 a 59,2 a 4 SM,Supermercado,Quinzenal,Preço,Sim,Instagram,Vídeo curto,Preço alto,9.50,4,2
108,45 a 59,Acima de 8 SM,Supermercado,Quinzenal,Origem regional,Não,Indicação de conhecidos,Promoção/cupom,Preço alto,16.50,4,4
109,25 a 34,Até 2 SM,Supermercado,Mensal,Qualidade/sabor,Não,Instagram,Promoção/cupom,Não encontro onde comprar,9.00,5,4
110,18 a 24,Até 2 SM,Direto do produtor,Mensal,Preço,Não,Facebook,Promoção/cupom,Preço alto,7.50,3,2
111,18 a 24,4 a 8 SM,Supermercado,Semanal,Higiene e selo de inspeção,Sim,Instagram,Receita,Preço alto,14.00,5,3
112,60 ou mais,2 a 4 SM,Supermercado,Quinzenal,Preço,Não,Indicação de conhecidos,História do produtor,Dúvida sobre higiene,13.00,5,4
113,25 a 34,Até 2 SM,Mercearia do bairro,Mensal,Preço,Não,WhatsApp,Receita,Não encontro onde comprar,12.50,2,4
114,18 a 24,Até 2 SM,Supermercado,Semanal,Embalagem,Não,WhatsApp,História do produtor,Não conheço a marca,10.00,1,3
115,18 a 24,2 a 4 SM,Supermercado,Quinzenal,Embalagem,Não,Instagram,História do produtor,Não encontro onde comprar,9.00,4,4
116,25 a 34,Até 2 SM,Supermercado,Semanal,Qualidade/sabor,Não,Rádio local,Vídeo curto,Preço alto,6.50,3,3
117,18 a 24,Até 2 SM,Supermercado,Quinzenal,Qualidade/sabor,Não,WhatsApp,Promoção/cupom,Não encontro onde comprar,6.00,3,5
118,25 a 34,Até 2 SM,Direto do produtor,Mensal,Origem regional,Não,Instagram,Vídeo curto,Preço alto,14.00,2,3
119,35 a 44,2 a 4 SM,Supermercado,Mensal,Qualidade/sabor,Não,WhatsApp,Receita,Dúvida sobre higiene,14.00,4,4
120,25 a 34,4 a 8 SM,Supermercado,Mensal,Higiene e selo de inspeção,Não,Indicação de conhecidos,Promoção/cupom,Nenhuma,14.50,3,2)---"
ROTEIRO_QUESTIONARIO <- r"---(ROTEIRO DE QUESTIONÁRIO – PESQUISA DE MERCADO PARA PRODUTO AGROINDUSTRIAL
Universidade do Estado de Mato Grosso – UNEMAT
Curso de Engenharia de Produção Agroindustrial – Disciplina de Marketing
Prof. Dr. Ramão Humberto Martins Manvailer

Objetivo: identificar o perfil do consumidor, os atributos valorizados, os canais de
informação e as barreiras de compra de um produto agroindustrial regional
(ex.: farinha de mandioca artesanal), para orientar a criação de uma campanha.

Termo de abertura (ler ao respondente):
"Esta pesquisa faz parte de uma atividade acadêmica da UNEMAT. A participação é
voluntária e anônima. Os dados serão usados apenas para fins didáticos.
Você aceita participar?"  ( ) Sim  ( ) Não -> encerrar

Cada pergunta corresponde a uma coluna do arquivo CSV (nome entre colchetes).
Mantenha os nomes das colunas para que o aplicativo gere as sugestões automáticas.

1. [faixa_etaria] Qual a sua idade?
   18 a 24 | 25 a 34 | 35 a 44 | 45 a 59 | 60 ou mais

2. [renda_familiar] Qual a renda familiar mensal aproximada? (SM = salário mínimo)
   Até 2 SM | 2 a 4 SM | 4 a 8 SM | Acima de 8 SM

3. [local_compra] Onde você costuma comprar este produto?
   Supermercado | Feira livre | Direto do produtor | Mercearia do bairro

4. [frequencia_compra] Com que frequência compra?
   Semanal | Quinzenal | Mensal | Raramente

5. [atributo_principal] O que MAIS pesa na sua escolha? (marque só uma)
   Qualidade/sabor | Preço | Origem regional | Higiene e selo de inspeção | Embalagem

6. [conhece_produtor_local] Você conhece algum produtor/agroindústria local deste produto?
   Sim | Não

7. [canal_info] Por onde você mais fica sabendo de novos produtos de alimentação?
   WhatsApp | Instagram | Facebook | Indicação de conhecidos | Rádio local

8. [formato_conteudo] Que tipo de conteúdo mais chama sua atenção?
   Vídeo curto | Foto com preço | Receita | História do produtor | Promoção/cupom

9. [barreira_compra] O que mais impede você de comprar de agroindústrias locais?
   Não encontro onde comprar | Preço alto | Não conheço a marca | Dúvida sobre higiene | Nenhuma

10. [disposicao_pagar_kg] Quanto pagaria, no máximo, por 1 kg de um produto artesanal
    de boa qualidade? (valor em R$, use ponto como separador decimal, ex.: 12.50)

11. [intencao_compra] De 1 (nada provável) a 5 (muito provável), qual a chance de você
    experimentar o produto da agroindústria local no próximo mês?

12. [satisfacao_atual] De 1 (muito insatisfeito) a 5 (muito satisfeito), qual sua
    satisfação com o produto que você compra hoje?

Orientações de aplicação
- Aplique em formulário on-line (Google Forms/Microsoft Forms) ou entrevista presencial.
- Exporte as respostas para CSV, renomeie os cabeçalhos para os nomes entre colchetes
  e inclua uma primeira coluna "id" (1, 2, 3...).
- Não colete nome, telefone ou CPF na pesquisa: ela é anônima (LGPD). Quem quiser
  receber ofertas deve se cadastrar separadamente, com consentimento registrado.
- Registre: público-alvo, local e data da coleta, tipo de amostra e número de respondentes.)---"
PRODUTOS_EXEMPLO <- r"---(produto,categoria,descricao,preco,unidade,imagem,link,ativo
Farinha de mandioca artesanal,Farinha e derivados,"Torrada em forno a lenha, produção familiar de Barra do Bugres",12.90,pacote 1 kg,,,TRUE
Polpa de cupuaçu,Frutas e polpas,"Polpa congelada, sem conservantes",9.50,pacote 500 g,,,TRUE
Mel silvestre,Mel e derivados,Mel de florada nativa do cerrado,28.00,pote 500 g,,,TRUE)---"
gravar_texto <- function(texto, arquivo) {
  con <- file(arquivo, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(texto, con)
}
if (!file.exists(file.path(PASTA_DADOS, "exemplo_pesquisa.csv")))
  gravar_texto(EXEMPLO_PESQUISA, file.path(PASTA_DADOS, "exemplo_pesquisa.csv"))
if (!file.exists(file.path(PASTA_DADOS, "roteiro_questionario.txt")))
  gravar_texto(ROTEIRO_QUESTIONARIO, file.path(PASTA_DADOS, "roteiro_questionario.txt"))
if (!file.exists(file.path(PASTA_DADOS, "produtos.csv")))
  gravar_texto(PRODUTOS_EXEMPLO, file.path(PASTA_DADOS, "produtos.csv"))

if (file.exists(".Renviron")) readRenviron(".Renviron")

ARQ_CONTATOS <- file.path(PASTA_DADOS, "contatos.csv")
ARQ_PRODUTOS <- file.path(PASTA_DADOS, "produtos.csv")
ARQ_CAMPANHAS <- file.path(PASTA_DADOS, "campanhas.csv")
ARQ_LOG <- file.path(PASTA_LOGS, "historico_envios.csv")
ARQ_IDENT <- file.path(PASTA_DADOS, "identificacao.csv")
campos_ident <- c("equipe", "integrantes", "turma", "agroindustria", "atualizado_em")

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
inicializar_csv(ARQ_IDENT, campos_ident)
if (!file.exists(ARQ_LOG)) {
  utils::write.csv(data.frame(data_hora=character(), campanha=character(),
    canal=character(), destinatario=character(), resultado=character(),
    detalhe=character()), ARQ_LOG, row.names=FALSE, fileEncoding="UTF-8")
}

# Cópia de segurança diária dos dados do aluno (dados/backup/AAAA-MM-DD).
# Nunca apaga nem sobrescreve nada: só copia.
local({
  arqs <- c(ARQ_CONTATOS, ARQ_PRODUTOS, ARQ_CAMPANHAS, ARQ_LOG, ARQ_IDENT)
  arqs <- arqs[file.exists(arqs)]
  destino <- file.path(PASTA_DADOS, "backup", format(Sys.Date(), "%Y-%m-%d"))
  if (length(arqs) && !dir.exists(destino)) {
    dir.create(destino, recursive = TRUE, showWarnings = FALSE)
    file.copy(arqs, destino, overwrite = FALSE)
  }
})

ler_csv <- function(arquivo, campos) {
  tryCatch({
    x <- readr::read_csv(arquivo, show_col_types = FALSE, na = character(),
                         col_types = readr::cols(.default = readr::col_character()),
                         locale = locale(encoding = "UTF-8"))
    for (nm in setdiff(campos, names(x))) x[[nm]] <- ""
    as.data.frame(x[, campos, drop=FALSE], stringsAsFactors=FALSE)
  }, error=function(e) as.data.frame(setNames(replicate(length(campos),
      character(0), simplify=FALSE), campos)))
}
gravar_csv <- function(x, arquivo) {
  readr::write_csv(as.data.frame(x), arquivo, na = "")
}
`%||%` <- function(a, b) if (is.null(a)) b else a

env <- function(nome, padrao="") {
  valor <- Sys.getenv(nome, unset=padrao)
  trimws(valor)
}
api_version <- function() {
  v <- env("META_API_VERSION", "v26.0")
  if (!grepl("^v[0-9]+\\.[0-9]+$", v)) "v26.0" else v
}
graph_url <- function(path) paste0("https://graph.facebook.com/", api_version(), "/", path)

registrar <- function(campanha, canal, destinatario, resultado, detalhe="") {
  linha <- data.frame(data_hora=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    campanha=campanha, canal=canal, destinatario=destinatario,
    resultado=resultado, detalhe=substr(detalhe, 1, 500),
    stringsAsFactors=FALSE)
  atual <- tryCatch(read.csv(ARQ_LOG, stringsAsFactors=FALSE, fileEncoding="UTF-8",
                             colClasses="character"),
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

# ---------------- RELATÓRIO EM PDF (somente R base, sem pacotes extras) ----------------
gerar_relatorio_pdf <- function(arquivo, log, campanhas, contatos, produtos, pesquisa = NULL, ident = NULL) {
  verde <- "#174D35"; verde2 <- "#17633E"; cinza <- "#5B6B62"; claro <- "#EEF4F0"; ouro <- "#C98A1B"; tinta <- "#1E2A22"
  # remove caracteres que a fonte do PDF não imprime (emojis etc.)
  limpar <- function(x) {
    x <- as.character(x); x[is.na(x)] <- ""
    x <- enc2utf8(x)
    gsub("[^\\x{0009}\\x{000A}\\x{0020}-\\x{00FF}]", "", x, perl = TRUE)
  }
  # quebra um texto em linhas que cabem em 'largura' unidades
  quebrar <- function(txt, largura, cex) {
    n <- max(8, floor((largura - 2) * 1.32 * 0.72 / cex))
    partes <- strsplit(limpar(txt), "\n", fixed = TRUE)[[1]]
    if (!length(partes)) return("")
    out <- unlist(lapply(partes, function(p) { w <- strwrap(p, width = n); if (length(w)) w else "" }))
    if (!length(out)) "" else out
  }
  alt_linha <- function(cex) 2.3 * cex / 0.72

  id1 <- function(campo) if (!is.null(ident) && nrow(ident) && campo %in% names(ident)) limpar(ident[[campo]][1]) else ""
  equipe_txt <- id1("equipe")
  integrantes <- trimws(strsplit(id1("integrantes"), "\n", fixed = TRUE)[[1]]); integrantes <- integrantes[nzchar(integrantes)]
  grDevices::pdf(arquivo, paper = "a4", width = 8.27, height = 11.69, family = "Helvetica")
  on.exit(grDevices::dev.off(), add = TRUE)
  pagina <- 0
  titulo_atual <- ""
  nova_pagina <- function(titulo = titulo_atual) {
    titulo_atual <<- titulo
    pagina <<- pagina + 1
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new(); graphics::plot.window(xlim = c(0, 100), ylim = c(0, 140), xaxs = "i", yaxs = "i")
    graphics::rect(0, 128, 100, 140, col = verde, border = NA)
    graphics::text(4, 136.2, "Universidade do Estado de Mato Grosso - UNEMAT", adj = 0, col = "white", cex = 0.85, font = 2)
    graphics::text(4, 132.6, "Curso de Engenharia de Produção Agroindustrial - Disciplina de Marketing", adj = 0, col = "white", cex = 0.75)
    graphics::text(4, 129.6, "Prof. Dr. Ramão Humberto Martins Manvailer", adj = 0, col = "white", cex = 0.75)
    graphics::text(4, 123, titulo, adj = 0, col = verde, cex = 1.35, font = 2)
    graphics::text(96, 3, paste("Página", pagina), adj = 1, col = cinza, cex = 0.7)
    graphics::text(4, 3, paste0(if (nzchar(equipe_txt)) paste0("Equipe: ", equipe_txt, "  |  ") else "",
                                "Gerado em ", format(Sys.time(), "%d/%m/%Y %H:%M"), "  |  Sistema de Campanhas de Marketing Agroindustrial"),
                   adj = 0, col = cinza, cex = 0.7)
    117
  }
  garantir <- function(y, h) if (y - h < 9) nova_pagina() else y
  secao <- function(y, txt) { y <- garantir(y, 14); graphics::text(4, y, limpar(txt), adj = 0, col = verde2, cex = 1.05, font = 2); y - 4.5 }
  paragrafo <- function(y, txt, cex = 0.75, x = 4, largura = 92, cor = tinta, font = 1) {
    for (l in quebrar(txt, largura, cex)) { y <- garantir(y, alt_linha(cex)); graphics::text(x, y, l, adj = 0, cex = cex, col = cor, font = font); y <- y - alt_linha(cex) }
    y
  }
  tabela <- function(y, df, larguras, cex = 0.72) {
    xs <- 4 + c(0, cumsum(larguras))[seq_along(larguras)]
    lh <- alt_linha(cex)
    cabecalho <- function(y) {
      graphics::rect(4, y - 1.9, 4 + sum(larguras), y + 1.9, col = verde, border = NA)
      for (j in seq_along(larguras)) graphics::text(xs[j] + 1, y, limpar(names(df)[j]), adj = 0, col = "white", cex = cex, font = 2)
      y - 4
    }
    y <- garantir(y, 10); y <- cabecalho(y)
    if (!nrow(df)) { graphics::text(5, y, "Nenhum registro.", adj = 0, col = cinza, cex = cex, font = 3); return(y - 5) }
    for (i in seq_len(nrow(df))) {
      celulas <- lapply(seq_along(larguras), function(j) quebrar(df[i, j], larguras[j], cex))
      h <- max(vapply(celulas, length, integer(1))) * lh + 1.2
      if (y - h < 9) { y <- nova_pagina(); y <- cabecalho(y) }
      if (i %% 2 == 0) graphics::rect(4, y - h + 1.6, 4 + sum(larguras), y + 1.8, col = claro, border = NA)
      for (j in seq_along(larguras)) for (k in seq_along(celulas[[j]]))
        graphics::text(xs[j] + 1, y - (k - 1) * lh, celulas[[j]][k], adj = 0, col = tinta, cex = cex)
      y <- y - h
    }
    y - 3
  }
  # lê PNG/JPG e desenha dentro de uma caixa (x, y = canto superior esquerdo)
  # identifica o formato pelo conteúdo (não pela extensão) e lê a imagem
  erros_img <- character()
  formato_img <- function(caminho) {
    b <- tryCatch(readBin(caminho, "raw", 12), error = function(e) raw(0))
    tem <- function(ini, assinatura) { a <- as.raw(assinatura); length(b) >= ini + length(a) - 1 && identical(b[ini:(ini + length(a) - 1)], a) }
    if (tem(1, c(0x89, 0x50, 0x4E, 0x47))) return("png")
    if (tem(1, c(0xFF, 0xD8, 0xFF))) return("jpeg")
    if (tem(1, charToRaw("RIFF")) && tem(9, charToRaw("WEBP"))) return("webp")
    if (tem(1, charToRaw("GIF8"))) return("gif")
    "desconhecido"
  }
  ler_img <- function(caminho, nome = basename(caminho)) {
    caminho <- as.character(caminho)
    if (!length(caminho) || is.na(caminho) || !nzchar(caminho)) return(NULL)
    if (!file.exists(caminho)) { erros_img <<- c(erros_img, paste0(nome, ": arquivo da foto não encontrado nesta pasta")); return(NULL) }
    fmt <- formato_img(caminho)
    img <- tryCatch(switch(fmt,
      png  = png::readPNG(caminho),
      jpeg = jpeg::readJPEG(caminho),
      webp = webp::read_webp(caminho, numeric = TRUE),
      stop("formato ", fmt, " não suportado no PDF (use JPG ou PNG)")),
      error = function(e) { erros_img <<- c(erros_img, paste0(nome, ": ", conditionMessage(e))); NULL })
    if (is.null(img)) return(NULL)
    if (is.integer(img) || max(img, na.rm = TRUE) > 1) img <- img / 255
    img
  }
  desenhar_img <- function(img, x, y, maxw, maxh) {
    d <- dim(img); prop <- (d[1] / d[2]) * (8.27 / 100) / (11.69 / 140)
    w <- maxw; h <- w * prop
    if (h > maxh) { h <- maxh; w <- h / prop }
    graphics::rasterImage(img, x, y - h, x + w, y)
    graphics::rect(x, y - h, x + w, y, border = "#D3DED7")
    h
  }
  df_vazio <- function(nomes) { d <- as.data.frame(setNames(replicate(length(nomes), character(0), simplify = FALSE), nomes)); names(d) <- nomes; d }

  # ---------- 1. Resumo ----------
  y <- nova_pagina("Relatório da campanha")
  # identificação da equipe
  if (nzchar(equipe_txt)) {
    linhas_id <- list(list("Equipe", equipe_txt, 1.0, 2))
    if (nzchar(id1("agroindustria"))) linhas_id[[length(linhas_id) + 1]] <- list("Agroindústria / produto", id1("agroindustria"), 0.8, 1)
    if (nzchar(id1("turma")))         linhas_id[[length(linhas_id) + 1]] <- list("Turma", id1("turma"), 0.8, 1)
    if (length(integrantes))          linhas_id[[length(linhas_id) + 1]] <- list(paste0("Integrantes (", length(integrantes), ")"),
                                                                                  paste(integrantes, collapse = "\n"), 0.8, 1)
    blocos <- lapply(linhas_id, function(l) quebrar(l[[2]], 64, l[[3]]))
    h <- sum(mapply(function(b, l) length(b) * alt_linha(l[[3]]), blocos, linhas_id)) + 3.5
    graphics::rect(4, y - h + 2, 96, y + 2.5, col = claro, border = NA)
    yy <- y
    for (k in seq_along(linhas_id)) {
      l <- linhas_id[[k]]
      graphics::text(6, yy, paste0(l[[1]], ":"), adj = 0, cex = 0.78, font = 2, col = cinza)
      for (t in blocos[[k]]) { graphics::text(33, yy, t, adj = 0, cex = l[[3]], font = l[[4]], col = tinta); yy <- yy - alt_linha(l[[3]]) }
    }
    y <- y - h - 3
  } else {
    graphics::rect(4, y - 4.5, 96, y + 2.5, col = "#FFF4E0", border = NA)
    graphics::text(6, y - 1, "Equipe não identificada: preencha a Identificação da equipe na aba Painel e gere o relatório de novo.",
                   adj = 0, cex = 0.78, font = 2, col = ouro)
    y <- y - 10
  }
  ok <- if (nrow(log)) sum(log$resultado == "Sucesso", na.rm = TRUE) else 0
  falhas <- if (nrow(log)) sum(log$resultado != "Sucesso", na.rm = TRUE) else 0
  autorizados <- if (nrow(contatos)) sum(tolower(contatos$consentimento) == "true", na.rm = TRUE) else 0
  cards <- list(c(nrow(produtos), "produtos"), c(nrow(campanhas), "campanhas"), c(autorizados, "contatos autorizados"), c(ok, "envios com sucesso"))
  for (k in seq_along(cards)) {
    x0 <- 4 + (k - 1) * 23.3
    graphics::rect(x0, y - 11, x0 + 21.5, y + 1, col = claro, border = NA)
    graphics::text(x0 + 10.75, y - 3.5, cards[[k]][1], cex = 2, font = 2, col = verde)
    graphics::text(x0 + 10.75, y - 8.5, cards[[k]][2], cex = 0.72, col = cinza)
  }
  y <- y - 15
  if (falhas > 0) y <- paragrafo(y, paste0("Envios com falha: ", falhas, " (detalhes no histórico)."), cor = ouro, font = 2)
  y <- y - 3

  # ---------- 2. Produtos ----------
  y <- secao(y, "Produtos cadastrados")
  pr <- if (nrow(produtos)) data.frame(Produto = produtos$produto, Categoria = produtos$categoria, Descricao = produtos$descricao,
        `Preço (R$)` = produtos$preco, Unidade = produtos$unidade, check.names = FALSE) else df_vazio(c("Produto","Categoria","Descricao","Preço (R$)","Unidade"))
  names(pr)[3] <- "Descrição"
  y <- tabela(y, pr, c(22, 17, 28, 11, 14))
  if (nrow(produtos)) {
    fotos <- lapply(seq_len(nrow(produtos)), function(i) list(nome = produtos$produto[i],
                                                              img = ler_img(produtos$imagem[i], produtos$produto[i])))
    com_img <- Filter(function(f) !is.null(f$img), fotos)
    if (length(com_img)) {
      y <- garantir(y, 10); graphics::text(4, y, "Fotos dos produtos", adj = 0, cex = 0.85, font = 2, col = tinta); y <- y - 4
      for (k in seq_along(com_img)) {
        col <- (k - 1) %% 3
        if (col == 0) y <- garantir(y, 36)
        x0 <- 4 + col * 31
        desenhar_img(com_img[[k]]$img, x0, y, 29, 28)
        graphics::text(x0, y - 30.5, limpar(com_img[[k]]$nome), adj = 0, cex = 0.65, col = cinza)
        if (col == 2 || k == length(com_img)) y <- y - 35
      }
    }
    if (length(erros_img)) for (e in unique(erros_img)) y <- paragrafo(y, paste("Foto não incluída -", e), cex = 0.65, cor = ouro, font = 3)
    y <- y - 2
  }

  # ---------- 3. Campanhas (completas) ----------
  y <- secao(y, "Campanhas")
  if (!nrow(campanhas)) y <- paragrafo(y, "Nenhuma campanha salva.", cor = cinza, font = 3)
  for (i in seq_len(nrow(campanhas))) {
    c1 <- campanhas[i, , drop = FALSE]
    y <- garantir(y, 30)
    graphics::rect(4, y - 2.2, 96, y + 2.2, col = claro, border = NA)
    graphics::text(5, y, limpar(paste0(i, ". ", c1$nome)), adj = 0, cex = 0.9, font = 2, col = verde)
    graphics::text(95, y, limpar(c1$data_criacao), adj = 1, cex = 0.7, col = cinza)
    y <- y - 5
    campos <- list(c("Produto", c1$produto), c("Objetivo", c1$objetivo), c("Público-alvo", c1$publico),
                   c("Chamada para ação", c1$chamada), c("Canais", c1$canais), c("Link", c1$link), c("Status", c1$status))
    for (cp in campos) if (nzchar(limpar(cp[2]))) {
      linhas <- quebrar(cp[2], 70, 0.75)
      y <- garantir(y, length(linhas) * alt_linha(0.75))
      graphics::text(5, y, paste0(cp[1], ":"), adj = 0, cex = 0.75, font = 2, col = tinta)
      for (k in seq_along(linhas)) { graphics::text(26, y, linhas[k], adj = 0, cex = 0.75, col = tinta); y <- y - alt_linha(0.75) }
    }
    y <- garantir(y, 8)
    graphics::text(5, y, "Texto da campanha:", adj = 0, cex = 0.75, font = 2, col = tinta); y <- y - alt_linha(0.75)
    y <- paragrafo(y, c1$mensagem, cex = 0.75, x = 7, largura = 88, cor = "#33433A")
    n_err <- length(erros_img)
    img <- ler_img(c1$imagem, paste("imagem da campanha", c1$nome))
    if (is.null(img) && !nzchar(limpar(c1$imagem)) && nrow(produtos)) {   # sem imagem própria: usa a foto do produto
      cam <- produtos$imagem[produtos$produto == c1$produto]
      if (length(cam)) img <- ler_img(cam[1], c1$produto)
    }
    if (!is.null(img)) {
      y <- y - 2; y <- garantir(y, 48)
      graphics::text(5, y, "Imagem da campanha:", adj = 0, cex = 0.75, font = 2, col = tinta); y <- y - 3
      y <- y - desenhar_img(img, 7, y, 50, 42) - 2
    } else if (length(erros_img) > n_err) {
      for (e in erros_img[(n_err + 1):length(erros_img)]) y <- paragrafo(y, paste("Imagem não incluída -", e), cex = 0.65, cor = ouro, font = 3)
    }
    y <- y - 4
  }

  # ---------- 4. Contatos ----------
  y <- secao(y, "Contatos")
  sn <- function(v) ifelse(tolower(limpar(v)) == "true", "sim", "não")
  ct <- if (nrow(contatos)) data.frame(Nome = contatos$nome, WhatsApp = contatos$telefone, Consentimento = sn(contatos$consentimento),
        `Opt-in WhatsApp` = sn(contatos$whatsapp_optin), Origem = contatos$observacao, check.names = FALSE)
        else df_vazio(c("Nome","WhatsApp","Consentimento","Opt-in WhatsApp","Origem"))
  y <- tabela(y, ct, c(22, 18, 15, 15, 22))

  # ---------- 5. Resumo por canal ----------
  if (nrow(log)) {
    y <- secao(y, "Resumo por canal")
    canais <- sort(unique(log$canal))
    rs <- data.frame(Canal = canais,
                     Sucesso = vapply(canais, function(c) sum(log$canal == c & log$resultado == "Sucesso"), numeric(1)),
                     Falha = vapply(canais, function(c) sum(log$canal == c & log$resultado != "Sucesso"), numeric(1)),
                     check.names = FALSE, row.names = NULL)
    y <- tabela(y, rs, c(40, 20, 20))
    mx <- max(rs$Sucesso + rs$Falha, 1)
    y <- garantir(y, 6 + 4.5 * nrow(rs))
    for (i in seq_len(nrow(rs))) {
      graphics::text(4, y, limpar(rs$Canal[i]), adj = 0, cex = 0.72)
      w1 <- 50 * rs$Sucesso[i] / mx; w2 <- 50 * rs$Falha[i] / mx
      if (w1 > 0) graphics::rect(40, y - 1.4, 40 + w1, y + 1.4, col = verde2, border = NA)
      if (w2 > 0) graphics::rect(40 + w1, y - 1.4, 40 + w1 + w2, y + 1.4, col = ouro, border = NA)
      graphics::text(42 + w1 + w2, y, paste(rs$Sucesso[i] + rs$Falha[i]), adj = 0, cex = 0.72)
      y <- y - 4.5
    }
    graphics::rect(40, y - 0.8, 42, y + 0.8, col = verde2, border = NA); graphics::text(43, y, "sucesso", adj = 0, cex = 0.65, col = cinza)
    graphics::rect(55, y - 0.8, 57, y + 0.8, col = ouro, border = NA); graphics::text(58, y, "falha", adj = 0, cex = 0.65, col = cinza)
    y <- y - 7
  }

  # ---------- 6. Pesquisa ----------
  if (!is.null(pesquisa) && is.data.frame(pesquisa) && nrow(pesquisa)) {
    y <- secao(y, paste0("Pesquisa de mercado (", nrow(pesquisa), " respondentes)"))
    regras <- list(c("atributo_principal", "Atributo mais valorizado"), c("canal_info", "Canal onde o público se informa"),
                   c("local_compra", "Local de compra preferido"), c("barreira_compra", "Principal barreira de compra"),
                   c("formato_conteudo", "Formato de conteúdo preferido"))
    linhas <- list()
    for (r in regras) if (r[1] %in% names(pesquisa)) {
      t <- sort(table(pesquisa[[r[1]]]), decreasing = TRUE)
      if (length(t)) linhas[[length(linhas) + 1]] <- c(r[2], sprintf("%s (%.0f%%)", names(t)[1], 100 * t[[1]] / sum(t)))
    }
    if ("disposicao_pagar_kg" %in% names(pesquisa)) {
      v <- suppressWarnings(as.numeric(pesquisa$disposicao_pagar_kg))
      if (any(!is.na(v))) linhas[[length(linhas) + 1]] <- c("Disposição a pagar (mediana)",
        paste0("R$ ", formatC(stats::median(v, na.rm = TRUE), format = "f", digits = 2, decimal.mark = ","), " por kg"))
    }
    if (length(linhas)) {
      ps <- data.frame(Indicador = vapply(linhas, `[`, "", 1), Resultado = vapply(linhas, `[`, "", 2), check.names = FALSE)
      y <- tabela(y, ps, c(40, 52))
    }
  }

  # ---------- 7. Histórico completo ----------
  y <- nova_pagina("Histórico de envios e publicações")
  hs <- if (nrow(log)) data.frame(`Data e hora` = log$data_hora, Campanha = log$campanha, Canal = log$canal,
                                  Destino = log$destinatario, Resultado = log$resultado, Detalhe = log$detalhe, check.names = FALSE)
        else df_vazio(c("Data e hora", "Campanha", "Canal", "Destino", "Resultado", "Detalhe"))
  tabela(y, hs, c(15, 15, 12, 14, 10, 26), cex = 0.62)
  invisible(arquivo)
}

# ---------------- INTEGRAÇÕES META ----------------
# WhatsApp: envio de modelo aprovado (categoria MARKETING).
# O nome/idioma do modelo e o ID do telefone são definidos no .Renviron.
enviar_whatsapp_template <- function(telefone, campanha, produto, chamada) {
  token <- env("META_ACCESS_TOKEN")
  phone_id <- env("WA_PHONE_NUMBER_ID")
  template <- env("WA_TEMPLATE_NAME", "hello_world")
  if (!nzchar(template)) template <- "hello_world"
  idioma <- env("WA_TEMPLATE_LANG", if (identical(template, "hello_world")) "en_US" else "pt_BR")
  n_vars <- suppressWarnings(as.integer(env("WA_TEMPLATE_VARS", if (identical(template, "hello_world")) "0" else "3")))
  if (is.na(n_vars)) n_vars <- 3L
  if (!nzchar(token) || !nzchar(phone_id) || !nzchar(template)) {
    return(list(ok=FALSE, detalhe="Configure token, ID do telefone e nome do modelo na aba Configuração Meta."))
  }
  numero <- gsub("[^0-9]", "", telefone)
  if (nchar(numero) < 10) return(list(ok=FALSE, detalhe="Telefone inválido. Use código do país + DDD + número."))
  # Modelo esperado: 3 variáveis posicionais no corpo:
  # {{1}} nome da campanha; {{2}} produto; {{3}} chamada.
  # Modelos sem variáveis (ex.: hello_world do número de teste) vão sem components.
  valores <- c(campanha, produto, chamada)
  modelo <- list(name=template, language=list(code=idioma))
  if (n_vars > 0) {
    valores <- valores[seq_len(min(n_vars, 3))]
    modelo$components <- list(list(type="body", parameters=lapply(valores,
      function(v) list(type="text", text=substr(ifelse(nzchar(v), v, "-"), 1, 60)))))
  }
  payload <- list(messaging_product="whatsapp", recipient_type="individual",
    to=numero, type="template", template=modelo)
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
    div(class="sub", "Planejamento • Pesquisa • Comunicação • Publicação • Resultados", span(style="opacity:.7;margin-left:12px;font-size:12px;", VERSAO_APP))
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
      div(class="caixa", h4("Identificação da equipe"),
        p(class="small-note", "Aparece na primeira página e no rodapé de todas as páginas do relatório em PDF, no nome do arquivo baixado e vai junto no 'Salvar meu trabalho'."),
        fluidRow(
          column(4, textInput("ident_equipe", "Nome da equipe *", placeholder = "ex.: Equipe Sabor do Bugres", width = "100%")),
          column(4, textInput("ident_agro", "Agroindústria / produto trabalhado", placeholder = "ex.: Farinha de mandioca artesanal", width = "100%")),
          column(4, textInput("ident_turma", "Turma / semestre", placeholder = "ex.: Marketing - 2026/2", width = "100%"))
        ),
        textAreaInput("ident_integrantes", "Integrantes * (um nome completo por linha)", rows = 4, width = "100%",
                      placeholder = "Maria da Silva\nJoão Pereira\n..."),
        actionButton("salvar_ident", "Salvar identificação", class = "btn-primary"),
        div(style = "margin-top:8px;", uiOutput("ident_status"))
      ),
      fluidRow(
        column(3, div(class="caixa", h4("Produtos"), h2(textOutput("n_produtos", inline=TRUE)))),
        column(3, div(class="caixa", h4("Contatos"), h2(textOutput("n_contatos", inline=TRUE)))),
        column(3, div(class="caixa", h4("Campanhas"), h2(textOutput("n_campanhas", inline=TRUE)))),
        column(3, div(class="caixa", h4("Envios registrados"), h2(textOutput("n_envios", inline=TRUE))))
      ),
      div(class="caixa", h4("Meu trabalho: salvar e continuar na próxima aula"),
        fluidRow(
          column(6,
            p(strong("1. Ao final da aula:"), " baixe o arquivo com tudo o que você fez (produtos, contatos, campanhas, histórico e pesquisa carregada). Guarde no pen drive, e-mail ou Google Drive."),
            downloadButton("baixar_trabalho", "Salvar meu trabalho", class="btn-primary")
          ),
          column(6,
            p(strong("2. Na próxima aula:"), " importe o arquivo salvo para continuar de onde parou, mesmo em outro computador."),
            fileInput("importar_trabalho", NULL, accept=".rds", buttonLabel="Continuar trabalho salvo...",
                      placeholder="arquivo meu_trabalho_....rds")
          )
        ),
        p(class="small-note", "Ao importar, o app guarda antes uma cópia do que estava nesta pasta (dados/backup). As imagens enviadas não vão no arquivo."),
        hr(),
        p(strong("Sumiu o que você tinha feito?"), " Isso acontece quando o app é aberto de outra pasta (Downloads, WhatsApp, arquivo .zip). Os dados não foram apagados: clique abaixo para procurá-los neste computador e trazê-los de volta."),
        actionButton("procurar_dados", "Recuperar dados anteriores deste computador"),
        br(), br(),
        uiOutput("ui_recuperar")
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
        column(7, div(class="caixa", h4("Produtos cadastrados"), DTOutput("tb_produtos"),
          p(class="small-note", strong("Para editar:"), " dê dois cliques na célula, corrija e aperte Enter (Ctrl+Enter em textos longos). Para excluir: clique na linha (ela fica destacada) e use o botão abaixo."),
          actionButton("excluir_produtos", "Excluir produto(s) selecionado(s)", class="btn-danger btn-sm"),
          p(class="small-note", "Para trocar a foto: no formulário ao lado, digite o mesmo nome do produto, escolha a nova foto e clique em Salvar produto.")))
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
          p(class="small-note", strong("Para editar:"), " dê dois cliques na célula, corrija e aperte Enter (Ctrl+Enter em textos longos). Para excluir: clique na linha (ela fica destacada) e use o botão abaixo."),
          actionButton("excluir_contatos", "Excluir contato(s) selecionado(s)", class="btn-danger btn-sm"),
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
            uiOutput("previa_campanha")),
          div(class="caixa", h4("Campanhas salvas"),
            DTOutput("tb_campanhas"),
            p(class="small-note", strong("Para editar:"), " dê dois cliques na célula, corrija e aperte Enter (Ctrl+Enter em textos longos). Para excluir: clique na linha (ela fica destacada) e use o botão abaixo."),
            actionButton("abrir_campanha", "Abrir selecionada no formulário para editar"),
            actionButton("excluir_campanhas", "Excluir campanha(s) selecionada(s)", class="btn-danger btn-sm"),
            p(class="small-note", "Ao abrir no formulário, altere o que quiser e clique em Salvar campanha: a campanha com o mesmo nome é atualizada."))
        )
      )
    ),
    tabPanel("Publicar e enviar", br(),
      div(class="caixa",
        h4("1. Selecione uma campanha salva"),
        selectInput("camp_envio", "Campanha", choices=NULL),
        p(class="small-note", "O envio de mensagens é individual e condicionado à elegibilidade do destinatário. Publicações em Instagram exigem imagem em URL HTTPS pública.")
      ),
      div(class="caixa", style="border:2px solid #1F8F55;",
        h4("2. Enviar pelo WhatsApp (recomendado para a aula)"),
        p("Cada botão abre o ", strong("WhatsApp do seu celular ou computador"), " com o texto da campanha já escrito para o contato. ",
          "Confira o texto e toque em ", strong("enviar"), ". O envio é registrado no histórico e no relatório em PDF."),
        p(class="small-note", "Aparecem só os contatos com consentimento e autorização de WhatsApp marcados (aba Contatos). Use o número com 55 + DDD."),
        radioButtons("wa_modo", "Abrir com:", inline = TRUE,
          choices = c("WhatsApp instalado no computador (direto)" = "app",
                      "WhatsApp Web no navegador (direto)" = "web",
                      "Celular / página de escolha do WhatsApp" = "wame"),
          selected = "app"),
        p(class="small-note", "Na primeira vez, o navegador pergunta se pode abrir o WhatsApp: marque ", strong("Sempre permitir"), " e clique em Abrir. Depois disso abre direto."),
        uiOutput("wa_links"),
        hr(),
        h4("Enviar para vários contatos de uma vez (lista de transmissão)"),
        p("Na lista de transmissão do WhatsApp, você envia ", strong("uma única vez"), " e cada contato recebe a mensagem no privado. ",
          "Só recebe quem tem o seu número salvo na agenda."),
        tags$ol(
          tags$li("Clique em ", strong("Baixar contatos (.vcf)"), " e abra o arquivo no celular para salvar os contatos na agenda (se ainda não estiverem salvos)."),
          tags$li("No WhatsApp do celular: ", strong("⋮ → Nova transmissão"), " (no WhatsApp Business: ", strong("Ferramentas comerciais → Listas de transmissão"), ") e marque os contatos da lista abaixo."),
          tags$li("Clique em ", strong("Copiar texto da campanha"), ", cole na transmissão e envie uma vez."),
          tags$li("Volte aqui e clique em ", strong("Registrar envio por transmissão"), " para o envio entrar no histórico e no relatório.")
        ),
        uiOutput("wa_transmissao")
      ),
      div(class="caixa", style="border:2px solid #1877F2;",
        h4("3. Publicar no Facebook e no Instagram (recomendado para a aula)"),
        p("O ", strong("Meta Business Suite"), " publica na sua Página do Facebook e no Instagram ao mesmo tempo, pelo computador. ",
          "O app prepara o texto e a imagem; você cola, publica e registra aqui."),
        tags$ol(
          tags$li("Clique em ", strong("Copiar texto"), " e em ", strong("Baixar imagem"), "."),
          tags$li("Clique em ", strong("Abrir Meta Business Suite"), " → ", strong("Criar publicação"), ". Marque a Página do Facebook e a conta do Instagram."),
          tags$li("Cole o texto (Ctrl+V), adicione a imagem baixada e clique em ", strong("Publicar"), "."),
          tags$li("Volte aqui e clique em ", strong("Registrar"), " para entrar no histórico e no relatório em PDF.")
        ),
        p(class="small-note", "Precisa de uma Página do Facebook (pode ser de teste, com o nome da agroindústria) e, para o Instagram, de uma conta profissional ligada a essa Página. Sem Página, use 'Abrir Facebook' para publicar no seu perfil."),
        uiOutput("redes_publicar")
      ),
      fluidRow(
        column(6, div(class="caixa",
          h4("4. Publicação automática pela API (exige credenciais da Meta)"),
          checkboxInput("pub_ig", "Publicar no Instagram", FALSE),
          textInput("url_imagem_publica", "URL HTTPS pública da imagem (Instagram)"),
          checkboxInput("pub_fb", "Publicar no Facebook", FALSE),
          actionButton("publicar_paginas", "Publicar conteúdo selecionado", class="btn-primary"),
          br(), br(), verbatimTextOutput("resultado_publicacao")
        )),
        column(6, div(class="caixa",
          h4("5. Envio automático pela API (exige número comercial da Meta)"),
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
          div(style="margin:6px 0 10px;", uiOutput("ident_aviso_rel")),
          downloadButton("baixar_pdf", "Baixar relatório em PDF", class="btn-primary"),
          downloadButton("baixar_logs", "Baixar planilha (CSV)")),
      div(class="caixa", h4("Resumo por canal"), DTOutput("tb_resumo"))
    ),
    tabPanel("Configuração Meta", br(),
      div(class="caixa",
        h4("Credenciais (modo de teste da Meta)"),
        p("Preencha com os dados do SEU aplicativo em developers.facebook.com. Nunca compartilhe o token em grupos ou no GitHub."),
        fluidRow(
          column(6,
            passwordInput("cfg_token", "Token de acesso (META_ACCESS_TOKEN)"),
            textInput("cfg_phone", "ID do número de telefone do WhatsApp (Phone number ID)"),
            textInput("cfg_tpl", "Nome do modelo do WhatsApp", "hello_world"),
            fluidRow(
              column(6, textInput("cfg_lang", "Idioma do modelo", "en_US")),
              column(6, selectInput("cfg_vars", "Variáveis no corpo", c("0 (hello_world)"="0", "3 (campanha, produto, chamada)"="3")))
            )
          ),
          column(6,
            textInput("cfg_page", "ID da Página do Facebook (opcional)"),
            passwordInput("cfg_page_token", "Token da Página (opcional)"),
            textInput("cfg_ig", "ID da conta profissional do Instagram (opcional)"),
            textInput("cfg_ver", "Versão da API", "v26.0")
          )
        ),
        actionButton("cfg_aplicar", "Usar nesta sessão", class="btn-primary"),
        actionButton("cfg_salvar", "Salvar neste computador (.Renviron)"),
        p(class="small-note", "'Usar nesta sessão' vale até fechar o R. 'Salvar' grava o arquivo .Renviron na pasta do app; não envie esse arquivo para o GitHub."),
        tableOutput("tb_config"),
        h4("Variáveis necessárias"),
        tags$pre("META_ACCESS_TOKEN=SEU_TOKEN\nWA_PHONE_NUMBER_ID=ID_DO_TELEFONE\nWA_TEMPLATE_NAME=nome_do_modelo_aprovado\nWA_TEMPLATE_LANG=pt_BR\nIG_USER_ID=ID_DA_CONTA_PROFISSIONAL\nFB_PAGE_ID=ID_DA_PAGINA\nFB_PAGE_ACCESS_TOKEN=TOKEN_DA_PAGINA\nMETA_API_VERSION=v26.0"),
        p("O arquivo .Renviron da pasta do app é carregado automaticamente ao iniciar."),
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
    ident=ler_csv(ARQ_IDENT, campos_ident),
    resultado_pub="Aguardando publicação.",
    resultado_env="Aguardando envio.",
    pesquisa=NULL,
    cfg=0
  )

  # ---------------- IDENTIFICAÇÃO DA EQUIPE ----------------
  preencher_ident <- function() {
    i <- isolate(rv$ident)
    v <- function(campo) if (nrow(i) && campo %in% names(i)) { x <- i[[campo]][1]; if (is.na(x)) "" else x } else ""
    updateTextInput(session, "ident_equipe", value = v("equipe"))
    updateTextInput(session, "ident_agro", value = v("agroindustria"))
    updateTextInput(session, "ident_turma", value = v("turma"))
    updateTextAreaInput(session, "ident_integrantes", value = v("integrantes"))
  }
  session$onFlushed(preencher_ident, once = TRUE)
  observeEvent(input$salvar_ident, {
    equipe <- trimws(input$ident_equipe)
    integ <- trimws(strsplit(input$ident_integrantes %||% "", "\n", fixed = TRUE)[[1]])
    integ <- integ[nzchar(integ)]
    if (!nzchar(equipe) || !length(integ)) {
      showNotification("Preencha o nome da equipe e pelo menos um integrante.", type = "error")
      return(invisible(NULL))
    }
    rv$ident <- data.frame(equipe = equipe, integrantes = paste(integ, collapse = "\n"),
                           turma = trimws(input$ident_turma), agroindustria = trimws(input$ident_agro),
                           atualizado_em = format(Sys.time(), "%d/%m/%Y %H:%M"), stringsAsFactors = FALSE)
    gravar_csv(rv$ident, ARQ_IDENT)
    preencher_ident()
    showNotification(sprintf("Identificação salva: %s (%d integrante(s)).", equipe, length(integ)), type = "message")
  })
  ident_ok <- reactive({ i <- rv$ident; nrow(i) > 0 && !is.na(i$equipe[1]) && nzchar(trimws(i$equipe[1])) })
  status_ident <- function() {
    if (!ident_ok()) return(span(class = "status-off", "○ Equipe ainda não identificada: preencha e clique em Salvar identificação (aba Painel)."))
    i <- rv$ident
    n <- length(Filter(nzchar, trimws(strsplit(i$integrantes[1], "\n", fixed = TRUE)[[1]])))
    span(class = "status-ok", sprintf("● %s | %d integrante(s) | salvo em %s", i$equipe[1], n, i$atualizado_em[1]))
  }
  output$ident_status <- renderUI(status_ident())
  output$ident_aviso_rel <- renderUI(status_ident())

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
  log_reativo <- reactiveFileReader(1000, session, ARQ_LOG, function(f)
    tryCatch(read.csv(f, stringsAsFactors=FALSE, fileEncoding="UTF-8", colClasses="character"),
             error=function(e) data.frame()))
  output$n_envios <- renderText(nrow(log_reativo()))

  # ---------------- SALVAR / CONTINUAR TRABALHO ----------------
  CAMPOS_LOG <- c("data_hora", "campanha", "canal", "destinatario", "resultado", "detalhe")
  padronizar <- function(df, campos) {
    df <- if (is.null(df)) data.frame() else as.data.frame(df, stringsAsFactors=FALSE)
    for (nm in setdiff(campos, names(df))) df[[nm]] <- rep("", nrow(df))
    df <- df[, campos, drop=FALSE]
    df[] <- lapply(df, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
    df
  }
  output$baixar_trabalho <- downloadHandler(
    filename=function() paste0("meu_trabalho_campanha_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".rds"),
    content=function(file) {
      saveRDS(list(app="campanhas_agro", versao=1, salvo_em=format(Sys.time(), "%d/%m/%Y %H:%M"),
                   produtos=rv$produtos, contatos=rv$contatos, campanhas=rv$campanhas,
                   log=padronizar(log_reativo(), CAMPOS_LOG), pesquisa=rv$pesquisa,
                   ident=rv$ident), file)
    }
  )
  # ---------------- RECUPERAR DADOS DE OUTRAS PASTAS ----------------
  pastas_encontradas <- reactiveVal(data.frame())
  observeEvent(input$procurar_dados, {
    home <- Sys.getenv("USERPROFILE"); if (!nzchar(home)) home <- path.expand("~")
    lad <- Sys.getenv("LOCALAPPDATA")
    locais <- c(file.path(home, c("Downloads", "Desktop", "Documents", "OneDrive")),
                if (nzchar(lad)) file.path(lad, "Temp"),
                if (nzchar(lad) && dir.exists(file.path(lad, "Packages")))
                  grep("WhatsApp", list.dirs(file.path(lad, "Packages"), recursive = FALSE), value = TRUE))
    locais <- unique(locais[dir.exists(locais)])
    id <- showNotification("Procurando dados anteriores neste computador... aguarde.", duration = NULL, type = "message")
    on.exit(removeNotification(id), add = TRUE)
    arqs <- unique(unlist(lapply(locais, function(d) tryCatch(
      list.files(d, pattern = "^campanhas\\.csv$", recursive = TRUE, full.names = TRUE),
      error = function(e) character(0)))))
    atual <- normalizePath(PASTA_DADOS, winslash = "/", mustWork = FALSE)
    pastas <- unique(normalizePath(dirname(arqs), winslash = "/", mustWork = FALSE))
    pastas <- pastas[pastas != atual & !grepl("/backup/", pastas) & basename(pastas) == "dados"]
    conta <- function(f) if (file.exists(f)) tryCatch(nrow(utils::read.csv(f, colClasses = "character")), error = function(e) 0) else 0
    if (length(pastas)) {
      info <- data.frame(pasta = pastas,
        campanhas = vapply(file.path(pastas, "campanhas.csv"), conta, numeric(1)),
        contatos  = vapply(file.path(pastas, "contatos.csv"),  conta, numeric(1)),
        produtos  = vapply(file.path(pastas, "produtos.csv"),  conta, numeric(1)),
        quando    = file.mtime(file.path(pastas, "campanhas.csv")),
        stringsAsFactors = FALSE, row.names = NULL)
      info <- info[info$campanhas + info$contatos > 0, , drop = FALSE]
      info <- info[order(info$quando, decreasing = TRUE), , drop = FALSE]
    } else info <- data.frame()
    pastas_encontradas(info)
    if (!nrow(info)) showNotification("Não encontrei dados anteriores com campanhas ou contatos neste computador.", type = "warning", duration = 8)
  })
  output$ui_recuperar <- renderUI({
    info <- pastas_encontradas()
    if (!nrow(info)) return(NULL)
    rotulos <- sprintf("%s | %d campanha(s), %d contato(s), %d produto(s) | %s",
                       format(info$quando, "%d/%m/%Y %H:%M"), info$campanhas, info$contatos, info$produtos, info$pasta)
    tagList(
      selectInput("pasta_recuperar", "Dados encontrados (o mais recente aparece primeiro)", choices = setNames(info$pasta, rotulos), width = "100%"),
      actionButton("confirmar_recuperar", "Trazer estes dados para cá", class = "btn-primary"),
      p(class = "small-note", "Antes de trazer, o app guarda uma cópia do que está aqui agora (dados/backup).")
    )
  })
  observeEvent(input$confirmar_recuperar, {
    origem <- input$pasta_recuperar
    req(origem, dir.exists(origem))
    bk <- file.path(PASTA_DADOS, "backup", paste0("antes_recuperacao_", format(Sys.time(), "%Y-%m-%d_%H%M%S")))
    arqs <- c(ARQ_CONTATOS, ARQ_PRODUTOS, ARQ_CAMPANHAS, ARQ_LOG, ARQ_IDENT); arqs <- arqs[file.exists(arqs)]
    if (length(arqs)) { dir.create(bk, recursive = TRUE, showWarnings = FALSE); file.copy(arqs, bk) }
    for (nm in c("contatos.csv", "produtos.csv", "campanhas.csv", "identificacao.csv")) {
      f <- file.path(origem, nm); if (file.exists(f)) file.copy(f, file.path(PASTA_DADOS, nm), overwrite = TRUE)
    }
    f <- file.path(origem, "logs", "historico_envios.csv"); if (file.exists(f)) file.copy(f, ARQ_LOG, overwrite = TRUE)
    midia <- list.files(file.path(origem, "midia"), full.names = TRUE)
    if (length(midia)) file.copy(midia, PASTA_MIDIA, overwrite = FALSE)
    rv$produtos  <- ler_csv(ARQ_PRODUTOS,  campos_produtos)
    rv$contatos  <- ler_csv(ARQ_CONTATOS,  campos_contatos)
    rv$campanhas <- ler_csv(ARQ_CAMPANHAS, campos_campanhas)
    rv$ident     <- ler_csv(ARQ_IDENT, campos_ident); preencher_ident()
    atualizar_escolhas()
    pastas_encontradas(data.frame())
    showNotification(sprintf("Dados recuperados: %d produto(s), %d contato(s), %d campanha(s). Agora clique em 'Salvar meu trabalho' para guardar uma cópia.",
                             nrow(rv$produtos), nrow(rv$contatos), nrow(rv$campanhas)), type = "message", duration = 10)
  })

  observeEvent(input$importar_trabalho, {
    x <- tryCatch(readRDS(input$importar_trabalho$datapath), error=function(e) NULL)
    if (is.null(x) || !is.list(x) || !identical(x$app, "campanhas_agro")) {
      showNotification("Arquivo inválido. Use o arquivo .rds baixado pelo botão 'Salvar meu trabalho'.", type="error")
      return(invisible(NULL))
    }
    # cópia de segurança do que existe agora, antes de substituir
    destino <- file.path(PASTA_DADOS, "backup", paste0("antes_importacao_", format(Sys.time(), "%Y-%m-%d_%H%M%S")))
    arqs <- c(ARQ_CONTATOS, ARQ_PRODUTOS, ARQ_CAMPANHAS, ARQ_LOG, ARQ_IDENT)
    arqs <- arqs[file.exists(arqs)]
    if (length(arqs)) { dir.create(destino, recursive=TRUE, showWarnings=FALSE); file.copy(arqs, destino) }
    rv$produtos  <- padronizar(x$produtos,  campos_produtos);  gravar_csv(rv$produtos,  ARQ_PRODUTOS)
    rv$contatos  <- padronizar(x$contatos,  campos_contatos);  gravar_csv(rv$contatos,  ARQ_CONTATOS)
    rv$campanhas <- padronizar(x$campanhas, campos_campanhas); gravar_csv(rv$campanhas, ARQ_CAMPANHAS)
    gravar_csv(padronizar(x$log, CAMPOS_LOG), ARQ_LOG)
    if (!is.null(x$ident) && NROW(x$ident)) {
      rv$ident <- padronizar(x$ident, campos_ident); gravar_csv(rv$ident, ARQ_IDENT); preencher_ident()
    }
    if (!is.null(x$pesquisa) && is.data.frame(x$pesquisa) && nrow(x$pesquisa)) rv$pesquisa <- x$pesquisa
    atualizar_escolhas()
    showNotification(sprintf("Trabalho de %s importado: %d produto(s), %d contato(s), %d campanha(s), %d registro(s) de envio.",
      ifelse(is.null(x$salvo_em), "data desconhecida", x$salvo_em), nrow(rv$produtos), nrow(rv$contatos),
      nrow(rv$campanhas), nrow(padronizar(x$log, CAMPOS_LOG))), type="message", duration=8)
  })

  cfg_campos <- c(META_ACCESS_TOKEN="cfg_token", WA_PHONE_NUMBER_ID="cfg_phone",
    WA_TEMPLATE_NAME="cfg_tpl", WA_TEMPLATE_LANG="cfg_lang", WA_TEMPLATE_VARS="cfg_vars",
    FB_PAGE_ID="cfg_page", FB_PAGE_ACCESS_TOKEN="cfg_page_token", IG_USER_ID="cfg_ig",
    META_API_VERSION="cfg_ver")
  cfg_valores <- function() {
    v <- vapply(cfg_campos, function(id) trimws(as.character(input[[id]] %||% "")), character(1))
    v[nzchar(v)]
  }
  observeEvent(input$cfg_aplicar, {
    v <- cfg_valores()
    if (length(v)) do.call(Sys.setenv, as.list(v))
    rv$cfg <- (rv$cfg %||% 0) + 1
    showNotification(paste("Credenciais aplicadas nesta sessão:", length(v), "campo(s)."), type="message")
  })
  observeEvent(input$cfg_salvar, {
    v <- cfg_valores()
    if (!length(v)) { showNotification("Nada para salvar.", type="warning"); return(invisible(NULL)) }
    do.call(Sys.setenv, as.list(v))
    existentes <- if (file.exists(".Renviron")) readLines(".Renviron", warn=FALSE) else character(0)
    existentes <- existentes[!sub("=.*", "", existentes) %in% names(v)]
    writeLines(c(existentes, paste0(names(v), "=", v)), ".Renviron")
    rv$cfg <- (rv$cfg %||% 0) + 1
    showNotification("Credenciais salvas em .Renviron nesta pasta.", type="message")
  })

  output$status_integracoes <- renderUI({
    rv$cfg
    status <- list(
      "WhatsApp" = nzchar(env("META_ACCESS_TOKEN")) && nzchar(env("WA_PHONE_NUMBER_ID")),
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
    rv$cfg
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

  img_tag <- function(caminho, altura = 60) {
    caminho <- as.character(caminho); caminho[is.na(caminho)] <- ""
    vapply(caminho, function(cm) {
      if (!nzchar(cm)) return("(sem foto)")
      uri <- img_data_uri(cm)
      if (is.na(uri)) return("(foto não encontrada nesta pasta)")
      sprintf('<img src="%s" style="height:%dpx;max-width:120px;object-fit:cover;border-radius:6px">', uri, altura)
    }, character(1), USE.NAMES = FALSE)
  }
  output$tb_produtos <- renderDT({
    x <- rv$produtos
    x <- data.frame(Foto = img_tag(x$imagem), x[, setdiff(names(x), "imagem"), drop = FALSE],
                    check.names = FALSE, stringsAsFactors = FALSE)
    datatable(x, escape = -1, rownames = FALSE, selection = "multiple",
              editable = list(target = "cell", disable = list(columns = 0)),
              options = list(pageLength = 5, scrollX = TRUE))
  })
  output$tb_contatos <- renderDT(datatable(rv$contatos, rownames = FALSE, selection = "multiple",
    editable = list(target = "cell"), options = list(pageLength = 7, scrollX = TRUE)))
  output$tb_campanhas <- renderDT({
    x <- rv$campanhas
    bloq <- which(names(x) %in% c("data_criacao", "imagem")) - 1
    datatable(x, rownames = FALSE, selection = "multiple",
              editable = list(target = "cell", disable = list(columns = bloq)),
              options = list(pageLength = 5, scrollX = TRUE))
  })

  # ---------------- EDIÇÃO E EXCLUSÃO NAS TABELAS ----------------
  aplicar_edicao <- function(df, info, colunas_exibidas, editaveis) {
    for (k in seq_len(nrow(info))) {
      i <- info$row[k]; nm <- colunas_exibidas[info$col[k] + 1]
      if (!is.na(nm) && nm %in% editaveis && i >= 1 && i <= nrow(df)) df[i, nm] <- as.character(info$value[k])
    }
    df
  }
  observeEvent(input$tb_produtos_cell_edit, {
    cols <- c("Foto", setdiff(names(rv$produtos), "imagem"))
    rv$produtos <- aplicar_edicao(rv$produtos, input$tb_produtos_cell_edit, cols, setdiff(campos_produtos, "imagem"))
    gravar_csv(rv$produtos, ARQ_PRODUTOS); atualizar_escolhas()
    showNotification("Produto alterado e salvo.", type = "message")
  })
  observeEvent(input$tb_contatos_cell_edit, {
    rv$contatos <- aplicar_edicao(rv$contatos, input$tb_contatos_cell_edit, names(rv$contatos), campos_contatos)
    gravar_csv(rv$contatos, ARQ_CONTATOS)
    showNotification("Contato alterado e salvo.", type = "message")
  })
  observeEvent(input$tb_campanhas_cell_edit, {
    rv$campanhas <- aplicar_edicao(rv$campanhas, input$tb_campanhas_cell_edit, names(rv$campanhas),
                                   setdiff(campos_campanhas, c("data_criacao", "imagem")))
    gravar_csv(rv$campanhas, ARQ_CAMPANHAS); atualizar_escolhas()
    showNotification("Campanha alterada e salva.", type = "message")
  })
  configurar_exclusao <- function(tabela, botao, rotulo, obter, definir) {
    pendentes <- reactiveVal(integer())
    observeEvent(input[[botao]], {
      sel <- input[[paste0(tabela, "_rows_selected")]]
      if (!length(sel)) {
        showNotification("Clique primeiro na linha que deseja excluir (ela fica destacada).", type = "warning")
        return(invisible(NULL))
      }
      pendentes(sel)
      showModal(modalDialog(title = "Confirmar exclusão",
        sprintf("Excluir %d %s? O app guarda uma cópia diária em dados/backup.", length(sel), rotulo),
        footer = tagList(modalButton("Cancelar"), actionButton(paste0("conf_", botao), "Excluir", class = "btn-danger"))))
    })
    observeEvent(input[[paste0("conf_", botao)]], {
      x <- obter(); sel <- pendentes(); sel <- sel[sel >= 1 & sel <= nrow(x)]
      if (length(sel)) definir(x[-sel, , drop = FALSE])
      removeModal()
      showNotification(sprintf("%d %s excluído(s).", length(sel), rotulo), type = "message")
    })
  }
  configurar_exclusao("tb_produtos", "excluir_produtos", "produto(s)", function() rv$produtos,
    function(x) { rv$produtos <- x; gravar_csv(x, ARQ_PRODUTOS); atualizar_escolhas() })
  configurar_exclusao("tb_contatos", "excluir_contatos", "contato(s)", function() rv$contatos,
    function(x) { rv$contatos <- x; gravar_csv(x, ARQ_CONTATOS) })
  configurar_exclusao("tb_campanhas", "excluir_campanhas", "campanha(s)", function() rv$campanhas,
    function(x) { rv$campanhas <- x; gravar_csv(x, ARQ_CAMPANHAS); atualizar_escolhas() })
  observeEvent(input$abrir_campanha, {
    sel <- input$tb_campanhas_rows_selected
    if (!length(sel)) { showNotification("Clique primeiro na campanha que deseja abrir.", type = "warning"); return(invisible(NULL)) }
    c1 <- rv$campanhas[sel[1], , drop = FALSE]
    updateTextInput(session, "camp_nome", value = c1$nome)
    if (c1$produto %in% rv$produtos$produto) updateSelectInput(session, "camp_produto", selected = c1$produto)
    updateSelectInput(session, "camp_objetivo", selected = c1$objetivo)
    updateTextInput(session, "camp_publico", value = c1$publico)
    updateTextInput(session, "camp_chamada", value = c1$chamada)
    updateTextInput(session, "camp_link", value = c1$link)
    updateCheckboxGroupInput(session, "camp_canais", selected = trimws(strsplit(c1$canais, ";")[[1]]))
    updateTextAreaInput(session, "camp_mensagem", value = c1$mensagem)
    showNotification("Campanha aberta no formulário. Altere e clique em Salvar campanha.", type = "message")
  })
  output$tb_campanhas_painel <- renderDT(datatable(head(rv$campanhas, 5),
    options=list(dom="t", pageLength=5, scrollX=TRUE)))
  output$tb_logs <- renderDT({
    x <- log_reativo()
    datatable(x, options=list(pageLength=10, scrollX=TRUE))
  })
  output$tb_resumo <- renderDT({
    x <- log_reativo()
    if (!nrow(x)) return(datatable(data.frame(Informacao="Nenhum envio registrado.")))
    datatable(as.data.frame(table(Canal=x$canal, Resultado=x$resultado)),
      options=list(dom="t", pageLength=10))
  })
  output$baixar_pdf <- downloadHandler(
    filename=function() {
      eq <- if (ident_ok()) rv$ident$equipe[1] else "sem_identificacao"
      eq <- tryCatch(iconv(eq, "UTF-8", "ASCII//TRANSLIT", sub = ""), error = function(e) eq)
      eq <- gsub("[^A-Za-z0-9]+", "_", ifelse(is.na(eq), "equipe", eq)); eq <- gsub("^_|_$", "", eq)
      paste0("relatorio_", if (nzchar(eq)) eq else "equipe", "_", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
    },
    content=function(file) {
      log <- tryCatch(read.csv(ARQ_LOG, stringsAsFactors=FALSE, fileEncoding="UTF-8", colClasses="character"),
                      error=function(e) data.frame())
      for (nm in c("data_hora","campanha","canal","destinatario","resultado","detalhe"))
        if (!nm %in% names(log)) log[[nm]] <- character(nrow(log))
      gerar_relatorio_pdf(file, log, rv$campanhas, rv$contatos, rv$produtos, rv$pesquisa, rv$ident)
    },
    contentType="application/pdf"
  )
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
    nome <- trimws(input$p_nome)
    img <- ""
    if (!is.null(input$p_imagem) && file.exists(input$p_imagem$datapath)) {
      ext <- tolower(tools::file_ext(input$p_imagem$name)); if (!nzchar(ext)) ext <- "jpg"
      img <- file.path(PASTA_MIDIA, paste0("produto_", format(Sys.time(), "%Y%m%d%H%M%OS3"), ".", ext))
      img <- gsub("[.](?=[0-9]{3}[.])", "", img, perl = TRUE)
      ok <- file.copy(input$p_imagem$datapath, img, overwrite = TRUE)
      if (!isTRUE(ok)) { img <- ""; showNotification("Não foi possível gravar a imagem. Tente de novo.", type = "error") }
    }
    novo <- data.frame(produto=nome, categoria=input$p_categoria,
      descricao=input$p_descricao, preco=as.character(input$p_preco),
      unidade=input$p_unidade, imagem=img, link=input$p_link,
      ativo=as.character(input$p_ativo), stringsAsFactors=FALSE)
    existe <- which(tolower(trimws(rv$produtos$produto)) == tolower(nome))
    if (length(existe)) {
      i <- existe[1]
      if (!nzchar(img)) novo$imagem <- rv$produtos$imagem[i]   # sem foto nova: mantém a anterior
      x <- rv$produtos; x[i, names(novo)] <- novo[1, ]; rv$produtos <- x
      msg <- if (nzchar(img)) "Produto atualizado com a foto." else "Produto atualizado."
    } else {
      rv$produtos <- bind_rows(rv$produtos, novo)
      msg <- if (nzchar(img)) "Produto cadastrado com foto." else "Produto cadastrado (sem foto)."
    }
    gravar_csv(rv$produtos, ARQ_PRODUTOS)
    atualizar_escolhas()
    showNotification(msg, type="message")
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
      stringsAsFactors=FALSE, fileEncoding="UTF-8", colClasses="character"), error=function(e) NULL)
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
    msg <- paste0("*", input$camp_nome, "*", "\n\n",
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
    existe <- which(tolower(trimws(rv$campanhas$nome)) == tolower(trimws(input$camp_nome)))
    if (length(existe)) {
      i <- existe[1]
      novo$data_criacao <- rv$campanhas$data_criacao[i]
      novo$status <- rv$campanhas$status[i]
      if (!nzchar(img)) novo$imagem <- rv$campanhas$imagem[i]
      x <- rv$campanhas; x[i, names(novo)] <- novo[1, ]; rv$campanhas <- x
      msg <- "Campanha atualizada."
    } else {
      rv$campanhas <- bind_rows(rv$campanhas, novo)
      msg <- "Campanha salva com sucesso."
    }
    gravar_csv(rv$campanhas, ARQ_CAMPANHAS)
    atualizar_escolhas()
    showNotification(msg, type="message")
  })

  output$previa_campanha <- renderUI({
    # imagem: a enviada nesta campanha ou, se não houver, a foto do produto escolhido
    img <- NULL
    if (!is.null(input$camp_imagem) && file.exists(input$camp_imagem$datapath)) {
      tipo <- input$camp_imagem$type; if (is.null(tipo) || !nzchar(tipo)) tipo <- "image/jpeg"
      dados <- readBin(input$camp_imagem$datapath, "raw", file.info(input$camp_imagem$datapath)$size)
      img <- tags$img(src = paste0("data:", tipo, ";base64,", jsonlite::base64_enc(dados)),
                      style = "max-width:100%;max-height:260px;border-radius:8px;margin-bottom:10px;")
    } else if (!is.null(input$camp_produto) && nzchar(input$camp_produto)) {
      cam <- rv$produtos$imagem[rv$produtos$produto == input$camp_produto]
      cam <- cam[nzchar(cam) & file.exists(cam)]
      if (length(cam)) img <- tags$img(src = img_data_uri(cam[1]),
                                       style = "max-width:100%;max-height:260px;border-radius:8px;margin-bottom:10px;")
    }
    div(style="background:#f5f8f5;padding:14px;border-radius:8px;",
        img,
        div(style="white-space:pre-wrap;", if (nzchar(input$camp_mensagem)) input$camp_mensagem else "A prévia aparecerá aqui."))
  })

  output$seletor_contato <- renderUI({
    if (identical(input$grupo_envio, "um")) {
      selectInput("contato_unico", "Contato",
        choices=setNames(seq_len(nrow(rv$contatos)), rv$contatos$nome))
    } else NULL
  })

  campanha_selecionada <- reactive({
    req(input$camp_envio, nrow(rv$campanhas)>0)
    idx <- suppressWarnings(as.integer(sub(" .*", "", input$camp_envio)))
    if (is.na(idx) || idx < 1 || idx > nrow(rv$campanhas)) {
      showNotification("Selecione uma campanha salva no campo Campanha.", type = "warning")
      req(FALSE)
    }
    rv$campanhas[idx, , drop=FALSE]
  })

  observeEvent(input$publicar_paginas, {
    if (!nrow(rv$campanhas) || !nzchar(input$camp_envio %||% "")) {
      showNotification("Salve uma campanha (aba Criar campanha) e selecione-a no campo Campanha.", type = "warning"); return(invisible(NULL)) }
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
    if (!nrow(rv$campanhas) || !nzchar(input$camp_envio %||% "")) {
      showNotification("Salve uma campanha (aba Criar campanha) e selecione-a no campo Campanha.", type = "warning"); return(invisible(NULL)) }
    if (!isTRUE(input$confirmo_envio)) {
      showNotification("Marque a caixa de confirmação antes de enviar.", type = "warning"); return(invisible(NULL)) }
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

  # ---------------- ENVIO PELO WHATSAPP DO ALUNO (link wa.me) ----------------
  output$wa_links <- renderUI({
    if (!nrow(rv$campanhas) || is.null(input$camp_envio) || !nzchar(input$camp_envio))
      return(p(class = "status-off", "Salve uma campanha (aba Criar campanha) e selecione-a acima."))
    idx <- suppressWarnings(as.integer(sub(" .*", "", input$camp_envio)))
    if (is.na(idx) || idx < 1 || idx > nrow(rv$campanhas)) return(NULL)
    camp <- rv$campanhas[idx, , drop = FALSE]
    ct <- rv$contatos
    tel <- gsub("[^0-9]", "", ct$telefone)
    ok <- tolower(ct$consentimento) == "true" & tolower(ct$whatsapp_optin) == "true" & nchar(tel) >= 12
    if (!any(ok)) return(p(class = "status-off", "Nenhum contato autorizado. Na aba Contatos, cadastre o número (ex.: 5565999999999) e marque as duas autorizações."))
    # codifica byte a byte em UTF-8 (preserva emojis e acentos no Windows)
    url_utf8 <- function(x) {
      b <- as.integer(charToRaw(enc2utf8(as.character(x))))
      ok <- (b >= 48 & b <= 57) | (b >= 65 & b <= 90) | (b >= 97 & b <= 122) | b %in% c(45L, 46L, 95L, 126L)
      paste0(ifelse(ok, vapply(b, function(z) rawToChar(as.raw(z)), ""), sprintf("%%%02X", b)), collapse = "")
    }
    # remove símbolos estragados (bytes inválidos e o caractere de substituição) antes de enviar
    limpar_msg <- function(x) {
      x <- as.character(x); x[is.na(x)] <- ""
      x <- iconv(enc2utf8(x), "UTF-8", "UTF-8", sub = "")
      x[is.na(x)] <- ""
      # mantém só letras, acentos, números, pontuação e quebras de linha (remove emojis e símbolos estragados)
      x <- gsub("[^\\x{0009}\\x{000A}\\x{000D}\\x{0020}-\\x{007E}\\x{00A0}-\\x{00FF}]", "", x, perl = TRUE)
      sub("^[[:space:]]+", "", x)
    }
    texto <- url_utf8(limpar_msg(camp$mensagem))
    tags$table(class = "table", style = "margin-bottom:0;",
      tags$tbody(lapply(which(ok), function(i) {
        js <- sprintf("Shiny.setInputValue('wa_click', {nome: %s, tel: '%s', camp: %s, n: Math.random()});",
                      jsonlite::toJSON(ct$nome[i], auto_unbox = TRUE), tel[i],
                      jsonlite::toJSON(camp$nome, auto_unbox = TRUE))
        tags$tr(
          tags$td(strong(ct$nome[i]), br(), span(class = "small-note", tel[i])),
          tags$td(style = "text-align:right;vertical-align:middle;",
            tags$a(class = "btn", style = "background:#1F8F55;color:#fff;font-weight:bold;",
                   href = switch(input$wa_modo %||% "app",
                     app  = paste0("whatsapp://send?phone=", tel[i], "&text=", texto),
                     web  = paste0("https://web.whatsapp.com/send?phone=", tel[i], "&text=", texto),
                     paste0("https://wa.me/", tel[i], "?text=", texto)),
                   target = "_blank", rel = "noopener", onclick = js,
                   icon("whatsapp"), " Abrir no WhatsApp"))
        )
      })))
  })
  # ---------------- LISTA DE TRANSMISSÃO ----------------
  dados_transmissao <- reactive({
    if (!nrow(rv$campanhas) || is.null(input$camp_envio) || !nzchar(input$camp_envio)) return(NULL)
    idx <- suppressWarnings(as.integer(sub(" .*", "", input$camp_envio)))
    if (is.na(idx) || idx < 1 || idx > nrow(rv$campanhas)) return(NULL)
    ct <- rv$contatos
    tel <- gsub("[^0-9]", "", ct$telefone)
    ok <- tolower(ct$consentimento) == "true" & tolower(ct$whatsapp_optin) == "true" & nchar(tel) >= 12
    txt <- as.character(rv$campanhas$mensagem[idx]); txt[is.na(txt)] <- ""
    txt <- iconv(enc2utf8(txt), "UTF-8", "UTF-8", sub = ""); txt[is.na(txt)] <- ""
    txt <- gsub("[^\\x{0009}\\x{000A}\\x{000D}\\x{0020}-\\x{007E}\\x{00A0}-\\x{00FF}]", "", txt, perl = TRUE)
    list(campanha = rv$campanhas$nome[idx], texto = sub("^[[:space:]]+", "", txt),
         nomes = ct$nome[ok], tels = tel[ok])
  })
  output$wa_transmissao <- renderUI({
    d <- dados_transmissao()
    if (is.null(d)) return(NULL)
    if (!length(d$tels)) return(p(class = "status-off", "Nenhum contato autorizado para a lista de transmissão."))
    fmt <- function(t) paste0("+", substr(t, 1, 2), " ", substr(t, 3, 4), " ", substr(t, 5, nchar(t)))
    lista <- paste0(d$nomes, " - ", fmt(d$tels), collapse = "\n")
    js_copiar <- "function(id, btn){var el=document.getElementById(id);el.select();el.setSelectionRange(0,999999);
      var ok=function(){var t=btn.innerText;btn.innerText='Copiado!';setTimeout(function(){btn.innerText=t;},1500);};
      if(navigator.clipboard&&window.isSecureContext){navigator.clipboard.writeText(el.value).then(ok,function(){document.execCommand('copy');ok();});}
      else{document.execCommand('copy');ok();}}"
    tagList(
      tags$script(HTML(paste0("window.copiarCampo = ", js_copiar, ";"))),
      fluidRow(
        column(6,
          strong(sprintf("Texto da campanha \"%s\"", d$campanha)),
          tags$textarea(id = "tx_campanha_copiar", readonly = NA, rows = 8,
                        style = "width:100%;font-size:13px;margin:6px 0;", d$texto),
          tags$button(class = "btn btn-primary", type = "button",
                      onclick = "copiarCampo('tx_campanha_copiar', this)", icon("copy"), " Copiar texto da campanha")
        ),
        column(6,
          strong(sprintf("Contatos autorizados (%d)", length(d$tels))),
          tags$textarea(id = "tx_contatos_copiar", readonly = NA, rows = 8,
                        style = "width:100%;font-size:13px;margin:6px 0;", lista),
          tags$button(class = "btn btn-default", type = "button",
                      onclick = "copiarCampo('tx_contatos_copiar', this)", icon("copy"), " Copiar lista"),
          downloadButton("baixar_vcf", "Baixar contatos (.vcf)")
        )
      ),
      br(),
      actionButton("registrar_transmissao", "Registrar envio por transmissão", class = "btn-success", icon = icon("check"))
    )
  })
  output$baixar_vcf <- downloadHandler(
    filename = function() paste0("contatos_autorizados_", format(Sys.Date(), "%Y-%m-%d"), ".vcf"),
    content = function(file) {
      d <- dados_transmissao()
      if (is.null(d) || !length(d$tels)) { writeLines("", file); return(invisible(NULL)) }
      linhas <- unlist(lapply(seq_along(d$tels), function(i) c("BEGIN:VCARD", "VERSION:3.0",
        paste0("FN:", d$nomes[i]), paste0("N:", d$nomes[i], ";;;;"),
        paste0("TEL;TYPE=CELL:+", d$tels[i]), "END:VCARD")))
      con <- file(file, open = "w", encoding = "UTF-8"); on.exit(close(con)); writeLines(linhas, con)
    },
    contentType = "text/vcard"
  )
  observeEvent(input$registrar_transmissao, {
    d <- dados_transmissao()
    if (is.null(d) || !length(d$tels)) { showNotification("Selecione uma campanha com contatos autorizados.", type = "warning"); return(invisible(NULL)) }
    for (i in seq_along(d$tels))
      registrar(d$campanha, "WhatsApp (transmissão)", d$tels[i], "Sucesso", paste("Enviado por lista de transmissão para", d$nomes[i]))
    showNotification(sprintf("Envio por transmissão registrado para %d contato(s).", length(d$tels)), type = "message")
  })

  # ---------------- FACEBOOK E INSTAGRAM (publicação pelo aluno) ----------------
  imagem_campanha <- reactive({
    if (!nrow(rv$campanhas) || is.null(input$camp_envio) || !nzchar(input$camp_envio)) return(NULL)
    idx <- suppressWarnings(as.integer(sub(" .*", "", input$camp_envio)))
    if (is.na(idx) || idx < 1 || idx > nrow(rv$campanhas)) return(NULL)
    cam <- rv$campanhas$imagem[idx]
    if (is.na(cam) || !nzchar(cam) || !file.exists(cam)) {
      cam <- rv$produtos$imagem[rv$produtos$produto == rv$campanhas$produto[idx]]
      cam <- cam[!is.na(cam) & nzchar(cam) & file.exists(cam)]
      cam <- if (length(cam)) cam[1] else NA_character_
    }
    if (is.na(cam)) NULL else cam
  })
  output$redes_publicar <- renderUI({
    d <- dados_transmissao()
    if (is.null(d)) return(p(class = "status-off", "Salve uma campanha (aba Criar campanha) e selecione-a no campo Campanha, no topo."))
    img <- imagem_campanha()
    js_copiar <- "function(id, btn){var el=document.getElementById(id);el.select();el.setSelectionRange(0,999999);
      var ok=function(){var t=btn.innerText;btn.innerText='Copiado!';setTimeout(function(){btn.innerText=t;},1500);};
      if(navigator.clipboard&&window.isSecureContext){navigator.clipboard.writeText(el.value).then(ok,function(){document.execCommand('copy');ok();});}
      else{document.execCommand('copy');ok();}}"
    tagList(
      tags$script(HTML(paste0("window.copiarCampo = ", js_copiar, ";"))),
      fluidRow(
        column(7,
          strong("Texto da publicação"),
          tags$textarea(id = "tx_redes_copiar", readonly = NA, rows = 9,
                        style = "width:100%;font-size:13px;margin:6px 0;",
                        gsub("*", "", d$texto, fixed = TRUE)),   # asteriscos só viram negrito no WhatsApp
          tags$button(class = "btn btn-primary", type = "button",
                      onclick = "copiarCampo('tx_redes_copiar', this)", icon("copy"), " Copiar texto")
        ),
        column(5,
          strong("Imagem"),
          if (!is.null(img)) div(style = "margin:6px 0;",
              tags$img(src = img_data_uri(img), style = "max-width:100%;max-height:180px;border-radius:8px;"))
          else p(class = "status-off", style = "margin:6px 0;", "Sem imagem: cadastre uma foto do produto ou da campanha. O Instagram exige imagem."),
          if (!is.null(img)) downloadButton("baixar_img_redes", "Baixar imagem")
        )
      ),
      div(style = "margin-top:12px;display:flex;gap:8px;flex-wrap:wrap;",
        tags$a(class = "btn", style = "background:#1877F2;color:#fff;font-weight:bold;",
               href = "https://business.facebook.com/latest/composer", target = "_blank", rel = "noopener",
               icon("facebook"), " Abrir Meta Business Suite (Facebook + Instagram)"),
        tags$a(class = "btn btn-default", href = "https://www.facebook.com/", target = "_blank", rel = "noopener", "Abrir Facebook"),
        tags$a(class = "btn btn-default", href = "https://www.instagram.com/", target = "_blank", rel = "noopener", "Abrir Instagram")
      ),
      div(style = "margin-top:12px;display:flex;gap:8px;flex-wrap:wrap;",
        actionButton("reg_pub_fb", "Registrar: publiquei no Facebook", class = "btn-success", icon = icon("check")),
        actionButton("reg_pub_ig", "Registrar: publiquei no Instagram", class = "btn-success", icon = icon("check"))
      )
    )
  })
  output$baixar_img_redes <- downloadHandler(
    filename = function() {
      img <- imagem_campanha(); ext <- if (is.null(img)) "jpg" else tolower(tools::file_ext(img))
      d <- dados_transmissao(); nm <- if (is.null(d)) "campanha" else d$campanha
      nm <- tryCatch(iconv(nm, "UTF-8", "ASCII//TRANSLIT", sub = ""), error = function(e) "campanha")
      nm <- gsub("[^A-Za-z0-9]+", "_", nm)
      paste0("imagem_", nm, ".", ext)
    },
    content = function(file) { img <- imagem_campanha(); if (!is.null(img)) file.copy(img, file) }
  )
  observeEvent(input$reg_pub_fb, {
    d <- dados_transmissao(); req(d)
    registrar(d$campanha, "Facebook (publicação)", "Página/perfil do aluno", "Sucesso", "Publicado pelo aluno (Meta Business Suite/Facebook)")
    showNotification("Publicação no Facebook registrada.", type = "message")
  })
  observeEvent(input$reg_pub_ig, {
    d <- dados_transmissao(); req(d)
    registrar(d$campanha, "Instagram (publicação)", "Conta do aluno", "Sucesso", "Publicado pelo aluno (Meta Business Suite/Instagram)")
    showNotification("Publicação no Instagram registrada.", type = "message")
  })

  observeEvent(input$wa_click, {
    w <- input$wa_click
    registrar(w$camp, "WhatsApp (link)", w$tel, "Sucesso", paste("Mensagem aberta no WhatsApp do aluno para", w$nome))
    showNotification(paste("WhatsApp aberto para", w$nome, "- confira o texto e toque em enviar."), type = "message", duration = 6)
  })

  observeEvent(input$copiar_instrucao, {
    showNotification("Selecione o texto na caixa e use Ctrl+C para copiar.", type="message")
  })
}

shinyApp(ui, server)
