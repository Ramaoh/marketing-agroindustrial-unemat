# Marketing Agroindustrial – Pesquisa e Campanhas

**Universidade do Estado de Mato Grosso – UNEMAT**
Curso de Engenharia de Produção Agroindustrial · Disciplina de Marketing
Prof. Dr. Ramão Humberto Martins Manvailer

Material didático para duas aulas sobre **pesquisa de mercado**, **campanhas de marketing** e **especificidades do marketing no setor agroindustrial**, com um aplicativo Shiny (R) para a prática.

## Conteúdo

| Pasta / arquivo | Descrição |
|---|---|
| `app/` | Aplicativo Shiny *Sistema de Campanhas de Marketing Agroindustrial* |
| `app/dados/exemplo_pesquisa.csv` | Base de exemplo com 120 respostas **fictícias** |
| `app/dados/roteiro_questionario.txt` | Roteiro do questionário (12 perguntas) |
| `materiais/Aulas_Pesquisa_Campanhas_Marketing_Agroindustrial.docx` | Plano e texto-base das duas aulas (professor) |
| `materiais/Plano_Campanha_EXEMPLO_RESOLVIDO.docx` | Plano de campanha completo, feito com a base de exemplo |
| `materiais/Plano_Campanha_MODELO_para_preencher.docx` | Modelo para os grupos preencherem |

## Como executar o aplicativo

1. Baixe o repositório (**Code → Download ZIP**) e **extraia** o arquivo.
2. No RStudio, defina a pasta `app` como diretório de trabalho (*Session → Set Working Directory → Choose Directory…*).
3. No Console:
   ```r
   source("INICIAR_APP.R", encoding = "UTF-8")
   ```
   Os pacotes necessários (`shiny`, `DT`, `httr`, `jsonlite`, `readr`, `dplyr`, `shinyjs`) são instalados na primeira execução.

O planejamento, a pesquisa e a simulação funcionam sem credenciais. A integração com WhatsApp, Instagram e Facebook exige credenciais oficiais da Meta (ver `app/LEIA-ME.txt` e `app/.Renviron.example`).

## Segurança e LGPD

- **Nunca** envie o arquivo `.Renviron` (tokens) nem bases de contatos reais para o repositório. O `.gitignore` já os exclui.
- Use somente contatos com consentimento registrado. Não use listas compradas.
