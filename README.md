# Marketing Agroindustrial – Pesquisa e Campanhas

**Universidade do Estado de Mato Grosso – UNEMAT**
Curso de Engenharia de Produção Agroindustrial · Disciplina de Marketing
Prof. Dr. Ramão Humberto Martins Manvailer

Material didático sobre **pesquisa de mercado**, **campanhas de marketing** e **especificidades do marketing no setor agroindustrial**, com um aplicativo Shiny (R) para a prática.

## Conteúdo

| Pasta / arquivo | Descrição |
|---|---|
| `app/app.R` | Aplicativo *Sistema de Campanhas de Marketing Agroindustrial* (versão v6) |
| `app/dados/` | Base de exemplo da pesquisa (120 respostas **fictícias**), roteiro do questionário e produtos de exemplo |
| `materiais/Aulas_Pesquisa_Campanhas_Marketing_Agroindustrial.docx` | Plano e texto-base das duas aulas (professor) |
| `materiais/Plano_Campanha_Exemplo_Resolvido.pptx` | Apresentação do exemplo resolvido |
| `materiais/Plano_Campanha_EXEMPLO_RESOLVIDO.docx` | Plano de campanha completo, feito com a base de exemplo |
| `materiais/Plano_Campanha_MODELO_para_preencher.docx` | Modelo para as equipes preencherem |
| `materiais/Guia_do_Aluno_App_Campanhas.docx` | Guia do aluno: passo a passo do aplicativo |
| `privacidade.html` e `exclusao-de-dados.html` | Política de privacidade e instruções de exclusão de dados |

## Como executar o aplicativo

1. Baixe o repositório (**Code → Download ZIP**) e **extraia** o arquivo (não abra o app de dentro do ZIP).
2. Abra `app/app.R` no RStudio e clique em **Run App**.
3. Na primeira execução, o app instala sozinho os pacotes que faltarem (é preciso internet).
4. No topo do app deve aparecer **"versão 18/11/2026 – v6"**.

Mantenha sempre o `app.R` na **mesma pasta**: os dados da equipe ficam na pasta `dados/`, ao lado dele.

## O que o aplicativo faz

- **Painel:** identificação da equipe, salvar o trabalho e continuar na próxima aula, recuperar dados.
- **Pesquisa de mercado:** análise da base de exemplo ou da pesquisa da equipe (frequências, cruzamentos, "do dado à campanha").
- **Produtos, Contatos e Criar campanha:** cadastro com fotos, edição e exclusão.
- **Publicar e enviar:**
  - WhatsApp: botão que abre a conversa com o texto pronto (o aluno confirma o envio) e apoio à lista de transmissão;
  - Facebook e Instagram: texto e imagem prontos para publicar pelo Meta Business Suite.
- **Relatórios:** histórico e relatório em PDF com a identificação da equipe.

O envio e a publicação **automáticos** pela API da Meta continuam disponíveis no app, mas exigem credenciais e número comercial próprios (ver `app/LEIA-ME.txt`).

## Segurança e LGPD

- **Nunca** envie tokens, o arquivo `.Renviron` ou bases de contatos reais para o repositório.
- Use somente contatos com consentimento registrado. Não use listas compradas.
