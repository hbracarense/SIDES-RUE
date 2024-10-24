library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(timevis)
library(tidyverse)
library(readxl)
library(maptools)     
library(spdep)          
library(cartography)    
library(tmap)           
library(leaflet)
library(rgdal)
library(rgeos)
library(sp)
library(DT)
library(rlang)
library(writexl)
library(plotly)
library(data.table)
library(apputils)

#Dados - geral-----------------------------------------------------
df_estabelecimentos <- read_excel("indicadores_ppsus.xlsx", sheet = "estabelecimento")
df_micro <- read_excel("indicadores_ppsus.xlsx", sheet = "micro")
df_macro <- read_excel("indicadores_ppsus.xlsx", sheet = "macro")
df_ibge <- read_excel("indicadores_ppsus.xlsx", sheet = "ibge")
#Dados - "Marco regulatório"---------------------------------------
timedata <- readRDS("timedata.rds")

groups <- data.frame(id = c(1,2), content = c("Federal", "Estadual"))

#Dados - Mapas - aba "Estabelecimentos"_---------------------------
mg_shp <- readOGR("www", "mg", stringsAsFactors=FALSE, encoding="UTF-8")
mg_micros_shp <- readOGR("www", "mg_micros", stringsAsFactors=FALSE, encoding="UTF-8")
mg_macros_shp <- readOGR("www", "mg_macros", stringsAsFactors=FALSE, encoding="UTF-8")

typeIcons <- iconList(blue = makeIcon("www/blue.png", iconWidth = 24, iconHeight =32),
                      green = makeIcon("www/green.png", iconWidth = 24, iconHeight =32),
                      red = makeIcon("www/red.png", iconWidth = 24, iconHeight =32))

df_estabelecimentos_final <- df_estabelecimentos %>% filter(Tipologia == "Habilitados IAM/AVC"|Tipologia == "Não Habilitados"|Tipologia == "Rede Resposta")
df_estabelecimentos_final <- df_estabelecimentos_final %>% mutate(cor = ifelse(Tipologia == "Habilitados IAM/AVC", "green",
                                                                               ifelse(Tipologia == "Rede Resposta", "blue", "red")))

coordenadas_municipios <- data.frame(coordinates(mg_shp))
coordenadas_municipios$cidades <- mg_shp@data$NM_MUN

coordenadas_micros <- data.frame(coordinates(mg_micros_shp))
coordenadas_micros$microrregiao <- mg_micros_shp@data$n_micro

coordenadas_macros <- data.frame(coordinates(mg_macros_shp))
coordenadas_macros$macrorregiao <- mg_macros_shp@data$mg_micros_

#Dados - mapas - aba "Tipologia"-----------------------------------
paleta <- c("#D7191C", "#ED6E43", "#FDBA6F", "#FEE8A5", "#E6F4A7", "#B3DF75", "#6ABC58", "#1A9641")

#Dados - gráficos - aba "Indicadores"------------------------------
icone_total <- apputils::icon(list(src = "icone_total.png", width = "80px"), lib = "local")
icone_evasao <- apputils::icon(list(src = "icone_evasao.png", width = "80px"), lib = "local")
icone_tx_transf <- apputils::icon(list(src = "icone_tx_transf.png", width = "80px"), lib = "local")
cores <- c('#CED2CC', '#23282D', '#4CB5F5', '#1F3F49', '#D32D41', '#6AB187') 

#UI - Aba "Apresentação"-------------------------------------------
tab_apresentacao <- tabPanel("Apresentação",
                             fluidRow(
                               column(12,
                                      align = 'center',
                                      h1("Bem-vindo ao SIDES-RUE!"),
                                      box(
                                        width = 1,
                                        solidHeader = TRUE,
                                        p("Sem sombra", style ="text-align: justify;", style = "color: white;", style = "font-size:15px;"),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        p("Sem sombra", style ="text-align: justify;", style = "color: white;", style = "font-size:15px;")
                                      ),
                                      box(
                                        width = 10,
                                        solidHeader = TRUE,
                                        br(),
                                        p("Esse Sistema de Informações apresenta indicadores associados ao Desempenho da Rede de Urgência e Emergência (RUE) do Estado de Minas Gerais. Nessa primeira versão priorizamos as linhas de cuidado do Infarto Agudo do Miocárdio (IAM) e do Acidente Vascular Cerebral (AVC). O sistema está organizado em quatro abas além dessa apresentação. A primeira aba traz o marco regulatório atinente à construção dessas linhas de cuidado, incluindo a regulamentação federal e estadual. A segunda apresenta a caracterização dos estabelecimentos hospitalares segundo o status de habilitação ou credenciamento na rede. Em seguida propomos uma tipologia das microrregiões de saúde que foi construída a partir de indicadores de oferta, desempenho e taxa de ocupação hospitalar. Por fim, a última aba apresenta os indicadores de desempenho da rede do IAM e AVC considerando as micro e macrorregiões de saúde. Para informações técnicas mais detalhadas, consulte a nota metodológica disponível para ",em('download'),' ao final desta página.', style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                        p("Esse sistema foi construído a partir do projeto de pesquisa desenvolvido com financiamento do Edital Nº 003/2020 - PROGRAMA DE PESQUISA PARA O SUS: GESTÃO COMPARTILHADA EM SAÚDE-PPSUS financiado pela FAPEMIG. O projeto foi desenvolvido a partir de uma parceria com a Secretaria Estadual de Saúde do Estado de Minas Gerais (SES-MG).", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                        br(),
                                        p(strong('Equipe de pesquisa'), style ="text-align: right;", style = "color: black;", style = "font-size:15px;"),
                                        p('Mônica Viegas   ',strong('|'),'   Kenya Noronha', style ="text-align: right;", style = "color: black;", style = "font-size:15px;"),
                                        p('Lucas Carvalho   ',strong('|'),'   Cristina Guimarães', style ="text-align: right;", style = "color: black;", style = "font-size:15px;"),
                                        p('Daniel Nogueira   ',strong('|'),'   Henrique Bracarense', style ="text-align: right;", style = "color: black;", style = "font-size:15px;"),
                                        p('Marcelo Fleury   ',strong('|'),'   Maria Rigotti', style ="text-align: right;", style = "color: black;", style = "font-size:15px;"),
                                        p('Pedro Benner   ',strong('|'),'   Clara Sanna', style ="text-align: right;", style = "color: black;", style = "font-size:15px;"),
                                        br(),
                                        p(strong("Última atualização: "), "23 de outubro de 2024.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                        img(src = "banner.jpeg", height = '100%', width = '100%'),
                                        br(),
                                        br(),
                                        fluidRow(      div(
                                          style = "position:relative; left:calc(0%);",
                                          downloadButton(
                                            outputId = "download_nota",
                                            label = "Nota metodológica",
                                            style = "color: #fff; background-color: #222222; border-color: #fff;"
                                          ) 
                                        ))
                                      ),
                                      box(
                                        width = 1,
                                        solidHeader = TRUE,
                                        p("Sem sombra", style ="text-align: justify;", style = "color: white;", style = "font-size:15px;"),
                                        br(),
                                        br(),                            
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        p("Sem sombra", style ="text-align: justify;", style = "color: white;", style = "font-size:15px;")
                                      )
                               )
                               
                             )
)

#UI - Aba "Marco regulatório"--------------------------------------
tab_marco <- tabPanel("Marco regulatório",
                      fluidRow(
                        box(width = 12, solidHeader = TRUE, withSpinner(timevisOutput("marco_output"), type = '8'))
                      ),
                      fluidRow(box(width = 4, solidHeader = TRUE, style = "border-radius: 15px; background-color: #f2f0eb",
                                   column(width = 6,
                                          div(style="display:inline-block", radioButtons("nivel_input", HTML(paste("Nível", span("LINHA",style = "color:#f2f0eb"), sep = "<br/>")), c("Todas as regulações", "Estadual", "Federal")))),
                                   column(width = 6,
                                          conditionalPanel(
                                            condition = 'input.nivel_input == "Todas as regulações"',
                                            div(style="display:inline-block", radioButtons("regulamentacoes_all_input", "Objeto", c("Todas as regulações", "IAM", "AVC", "Queimados", "Trauma", "Rede de Resposta", "Valora Minas", "Outros/Misto")))
                                          ),
                                          conditionalPanel(
                                            condition = 'input.nivel_input == "Estadual"',
                                            div(style="display:inline-block", radioButtons("regulamentacoes_mg_input", "Objeto", c("Todas as regulações", "IAM", "AVC", "Queimados", "Rede de Resposta", "Valora Minas", "Outros/Misto")))
                                          ),
                                          conditionalPanel(
                                            condition = 'input.nivel_input == "Federal"',
                                            div(style="display:inline-block", radioButtons("regulamentacoes_br_input", "Objeto", c("Todas as regulações", "IAM", "AVC", "Queimados", "Trauma","Outros/Misto")))
                                          )
              
                                   )

                      ),
                      box(width = 6, solidHeader = TRUE,
                          p("A linha temporal do marco regulatório contempla as portarias ministeriais, em âmbito federal, e deliberações e resoluções estaduais atinentes às linha de cuidado IAM, AVC, Queimados e Trauma, bem como à Rede de Resposta e ao Valora Minas, no intervalo 1998-2024.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                          p("O marco inicial se dá em 2004, com a criação da Política Nacional de Atenção Cardiovascular de Alta Complexidade.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                          p("Selecione os filtros desejados ao lado e clique nos ícones para acessar o conteúdo dos instrumentos.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                      )
                      )
                      
)

#UI - Aba "Estabelecimentos"--------------------------------------
tab_estabelecimentos <- tabPanel("Estabelecimentos",
                                 fluidRow(box(width = 12, solidHeader = TRUE, style = "background-color: #ffffff",
                                              withSpinner(leafletOutput("mapa_estabelecimentos"), type ='8')),
                                          box(width = 3, solidHeader = TRUE, style = "border-radius: 15px; background-color: #f2f0eb",
                                              conditionalPanel(
                                                condition = "typeof input.mapa_estabelecimentos_polygon == 'undefined'||input.mapa_estabelecimentos_polygon == 'Municípios'",
                                                selectizeInput(
                                                  inputId = "municipio_input",
                                                  label = "Município",
                                                  choices = sort(c("--Todos os municípios--", coordenadas_municipios$cidades)),
                                                  selected = "--Todos os municípios--"
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.mapa_estabelecimentos_polygon == 'Microrregiões'",
                                                selectInput(
                                                  inputId = "microrregiao_input",
                                                  label = "Microrregião",
                                                  choices = sort(c("--Todas as microrregiões--", coordenadas_micros$microrregiao)),
                                                  selected = "--Todas as microrregiões--"
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.mapa_estabelecimentos_polygon == 'Macrorregiões'",
                                                selectInput(
                                                  inputId = "macrorregiao_input",
                                                  label = "Macrorregião",
                                                  choices = sort(c("--Todas as macrorregiões--", coordenadas_macros$macrorregiao)),
                                                  selected = "--Todas as macrorregiões--"
                                                )
                                              )
                                          ),
                                          box(width = 3, solidHeader = TRUE,
                                              p("Pesquise os estabelecimentos habilitados para IAM/AVC, não habilitados e pertencentes à Rede  Resposta em 2019, por município, micro e macrorregião, com visualização espacial no mapa acima e sintetização na tabela abaixo.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                              p("Estatísticas descritivas são apresentadas à direita.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                              p("Ao final da página, é possível realizar download das informações obtidas em formato .xlsx.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;")
                                          ),
                                          column(
                                            width = 6,
                                            tags$h4("Caracterização"),
                                            box(width = 12,             
                                                status = "warning",
                                                solidHeader = FALSE,
                                                conditionalPanel(
                                                  condition = "typeof input.mapa_estabelecimentos_polygon == 'undefined'||input.mapa_estabelecimentos_polygon == 'Municípios'",
                                                  tableOutput("caracterizacao_municipios")),
                                                conditionalPanel(
                                                  condition = "input.mapa_estabelecimentos_polygon == 'Microrregiões'",
                                                  tableOutput("caracterizacao_microrregioes")),
                                                conditionalPanel(
                                                  condition = "input.mapa_estabelecimentos_polygon == 'Macrorregiões'",
                                                  tableOutput("caracterizacao_macrorregioes")))
                                          ),
                                          
                                          
                                 ),
                                 tags$h1("Dados dos estabelecimentos"),
                                 fluidRow(
                                   box(width = 12,             
                                       status = "warning",
                                       solidHeader = FALSE,
                                       conditionalPanel(
                                         condition = "typeof input.mapa_estabelecimentos_polygon == 'undefined'||input.mapa_estabelecimentos_polygon == 'Municípios'",
                                         DT::dataTableOutput("tabela_municipios", width = '100%')),
                                       conditionalPanel(
                                         condition = "input.mapa_estabelecimentos_polygon == 'Microrregiões'",
                                         DT::dataTableOutput("tabela_microrregioes", width = '100%')),
                                       conditionalPanel(
                                         condition = "input.mapa_estabelecimentos_polygon == 'Macrorregiões'",
                                         DT::dataTableOutput("tabela_macrorregioes", width = '100%'))
                                   )
                                   
                                 ),
                                 fluidRow(      div(
                                   style = "position:relative; left:calc(45%);",
                                   downloadButton(
                                     outputId = "download_estabelecimentos",
                                     label = "Download",
                                     style = "color: #fff; background-color: #222222; border-color: #fff;"
                                   ) 
                                 ))
)

#UI - Aba "Tipologia"---------------------------------------------
linebreaks <- function(n){HTML(strrep(br(), n))}

tab_tipologia <- tabPanel("Tipologia",
                          fluidRow(box(width = 7, withSpinner(leafletOutput("mapa_tipologia"), type ='8')),
                                   box(width = 5, solidHeader = TRUE, 
                                       p("A tipologia de microrregiões foi construída a partir de um índice composto por três dimensões:", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                       linebreaks(2),
                                       p(HTML('&emsp;'),"-Oferta de leitos hospitalares (Alta/Baixa oferta)", style ="text-align: left;", style = "color: black;", style = "font-size:18px;"),
                                       p(HTML('&emsp;'),"-Taxa de ocupação hospitalar (Adequada/Ineficiente/Sobrecarga)", style ="text-align: left;", style = "color: black;", style = "font-size:18px;"),
                                       p(HTML('&emsp;'),"-Desempenho da rede IAM/AVC (Alto/Baixo desempenho)", style ="text-align: left;", style = "color: black;", style = "font-size:18px;"),
                                       linebreaks(2),
                                       p("Desta forma, são gerados nove tipos classificatórios possíveis.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                       p("Clique nas microrregiões do mapa interativo ao lado para conhecer seus pares e a categoria a qual pertencem.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"))),
                          conditionalPanel(
                            condition = 'typeof input.mapa_tipologia_shape_click != "undefined"',
                            fluidRow(column(12, align ="center",div(style = "position:relative; left:calc(17.5%);",uiOutput("tipo_box"))))
                          ))

#UI - Aba "Macrorregiões"-----------------------------------------
tab_macrorregioes <- tabPanel("Macrorregiões",
                              sidebarLayout(
                                sidebarPanel(
                                  selectizeInput(
                                    inputId = "macro_input",
                                    label = "Macrorregião",
                                    choices = sort(df_macro$nomemacro),
                                    multiple = TRUE, 
                                    options = list(
                                      placeholder = "Escolha uma macrorregião",
                                      maxItems = 1)
                                  ),
                                  conditionalPanel(
                                    condition = 'input.macro_comparacao_input != 0 && input.macro_input != ""',
                                    uiOutput("macro_comparacao_ui")
                                  ),
                                  radioButtons(
                                    inputId = "macro_enfermidade",
                                    label = "Enfermidade",
                                    choices = c("Todas as enfermidades", "IAM", "AVC"),
                                    selected = "Todas as enfermidades"
                                  ),
                                  conditionalPanel(
                                    condition = 'input.macro_input != ""',
                                    checkboxInput(inputId = "macro_comparacao_input", label = "Comparar com outras macrorregiões", value = FALSE) 
                                  ),
                                  div(
                                    style = "position:relative; left:calc(37.5%);",
                                    actionButton(
                                      inputId = "exibicao_macro",
                                      label = textOutput("texto_botao_macro"),
                                      style = "color: #fff; background-color: #222222; border-color: #fff;"
                                    )
                                  )
                                ),
                                mainPanel(
                                  conditionalPanel(
                                    condition = "input.exibicao_macro == 0",
                                    fluidRow(
                                      box(title = NULL,
                                          solidHeader = TRUE,
                                          width = 9,
                                          p("Nesta aba, são exibidos os indicadores calculados em nível macrorregional e de alta complexidade.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                          p("Estes são considerados nos aspectos de:", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),strong("-Oferta"), style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),HTML('&emsp;'),"Hospitais e leitos disponíveis por 10.000 habitantes acima de 45 anos, por habilitação e natureza (SUS/privado). ", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),strong("-Demanda"), style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),HTML('&emsp;'),"Distribuição das solicitações de internação por tipo e status do estabelecimento porta de entrada. ", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),strong("-Ocupação"), style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),HTML('&emsp;'),"Taxa de ocupação dos hospitais, segundo tipologia.", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p("Para a demanda, possibilita-se a distinção entre o somatório das duas enfermidades e IAM/AVC separadamente. Com a seleção deste último filtro, tem-se uma grandeza adicional:", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),strong("-Desempenho"), style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),HTML('&emsp;'),"Taxas de atendimento no tempo recomendado, evasão, transferência total e por tipo de habilitação da porta de entrada, além da distribuição dos pacientes transferidos. Distâncias médias percorridas entre residência/porta de entrada e porta de entrada/destino, dentro e fora da macrorregião original.", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p("A função de comparação permite que os indicadores das macrorregiões sejam comparados entre si.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                          p("Ao fim da página, as informações visualizadas são disponibilizadas para download em formato .xlsx.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;")
                                      )
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.exibicao_macro > 0",
                                    tags$div(
                                      tags$h1("Oferta"),
                                      fluidRow(
                                        box(width = 12,
                                            solidHeader = FALSE,
                                            status = 'warning',
                                            withSpinner(leafletOutput("mapa_macro"), type ='8'))
                                      ),
                                      uiOutput("ui_oferta_macro"),
                                      tags$h1("Demanda"),
                                      uiOutput("ui_demanda_macro"),
                                      tags$h1("Ocupação hospitalar"),
                                      uiOutput("ui_ocupacao_macro"),
                                      uiOutput("ui_desempenho_macro"),
                                      fluidRow(      div(
                                        style = "position:relative; left:calc(45%);",
                                        linebreaks(2),
                                        downloadButton(
                                          outputId = "download_macro",
                                          label = "Download",
                                          style = "color: #fff; background-color: #222222; border-color: #fff;"
                                        ) 
                                      )
                                      )
                                    )
                                  )
                                )
                              ))

#UI - Aba "Microrregiões"-----------------------------------------
tab_microrregioes <- tabPanel("Microrregiões",
                              sidebarLayout(
                                sidebarPanel(
                                  selectizeInput(
                                    inputId = "micro_input",
                                    label = "Microrregião",
                                    choices = sort(df_micro$nomemicro),
                                    multiple = TRUE, 
                                    options = list(
                                      placeholder = "Escolha uma microrregião",
                                      maxItems = 1)
                                  ),
                                  conditionalPanel(
                                    condition = 'input.micro_comparacao_input != 0 && input.micro_input != ""',
                                    uiOutput("micro_tipo_comparacao_ui"),
                                    uiOutput("micro_comparacao_ui")
                                  ),
                                  radioButtons(
                                    inputId = "micro_enfermidade",
                                    label = "Enfermidade",
                                    choices = c("Todas as enfermidades", "IAM", "AVC"),
                                    selected = "Todas as enfermidades"
                                  ),
                                  conditionalPanel(
                                    condition = 'input.micro_input != ""',
                                    checkboxInput(inputId = "micro_comparacao_input", label = "Comparar com outras microrregiões", value = FALSE)
                                  ),
                                  div(
                                    style = "position:relative; left:calc(37.5%);",
                                    actionButton(
                                      inputId = "exibicao_micro",
                                      label = textOutput("texto_botao_micro"),
                                      style = "color: #fff; background-color: #222222; border-color: #fff;"
                                    )
                                  )
                                ),
                                mainPanel(
                                  conditionalPanel(
                                    condition = "input.exibicao_micro == 0",
                                    fluidRow(
                                      box(title = NULL,
                                          solidHeader = TRUE,
                                          width = 10,
                                          p("Nesta aba, são exibidos os indicadores calculados em nível microrregional e de média complexidade.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                          p("Estes são considerados nos aspectos de:", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),strong("-Oferta"), style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),HTML('&emsp;'),"Hospitais e leitos disponíveis por 10.000 habitantes acima de 45 anos, por habilitação e natureza (SUS/privado). ", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),strong("-Demanda"), style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),HTML('&emsp;'),"Distribuição das solicitações de internação por tipo e status do estabelecimento porta de entrada. ", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),strong("-Ocupação"), style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),HTML('&emsp;'),"Taxa de ocupação dos hospitais, segundo tipologia.", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p("Para a demanda, possibilita-se a distinção entre o somatório das duas enfermidades e IAM/AVC separadamente. Com a seleção deste último filtro, tem-se uma grandeza adicional:", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),strong("-Desempenho"), style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p(HTML('&emsp;'),HTML('&emsp;'),"Taxas de atendimento no tempo recomendado, evasão, transferência total e por tipo de habilitação da porta de entrada, além da distribuição dos pacientes transferidos. Distâncias médias percorridas entre residência/porta de entrada e porta de entrada/destino, dentro e fora da microrregião original.", style ="text-align: left;", style = "color: black;", style = "font-size:15px;"),
                                          p("A função de comparação permite que os indicadores das microrregiões sejam comparados entre si, seja dentre todas as microrregiões ou apenas aquelas da mesma macrorregião ou tipo, conforme tipologia desenvolvida.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;"),
                                          p("Ao fim da página, as informações visualizadas são disponibilizadas para download em formato .xlsx.", style ="text-align: justify;", style = "color: black;", style = "font-size:15px;")
                                      )
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.exibicao_micro > 0",
                                    tags$div(
                                      tags$h1("Oferta"),
                                      fluidRow(
                                        box(width = 12,
                                            solidHeader = FALSE,
                                            status = 'warning',
                                            withSpinner(leafletOutput("mapa_micro"), type ='8'))
                                      ),
                                      uiOutput("ui_oferta_micro"),
                                      tags$h1("Demanda"),
                                      uiOutput("ui_demanda_micro"),
                                      tags$h1("Ocupação hospitalar"),
                                      uiOutput("ui_ocupacao_micro"),
                                      uiOutput("ui_desempenho_micro"),
                                      fluidRow(      div(
                                        style = "position:relative; left:calc(45%);",
                                        linebreaks(2),
                                        downloadButton(
                                          outputId = "download_micro",
                                          label = "Download",
                                          style = "color: #fff; background-color: #222222; border-color: #fff;"
                                        ) 
                                      )
                                      )
                                    )
                                  )
                                )
                              ))


#UI - Abas de indicadores-----------------------------------------
tabs_indicadores <- navbarMenu("Indicadores",
                               tab_macrorregioes,
                               tab_microrregioes
)
#UI - geral-------------------------------------------------------
ui <- navbarPage(title = img("SIDES-RUE", src="logo.jpeg", height = '40px', width = '120px',align = 'center'),
                 theme = shinytheme("cosmo"),
                 useShinyjs(),
                 tags$head(
                   tags$style(HTML("
                                    .navbar-nav > li > a, .navbar-brand {
                            padding-top:4px !important; 
                            padding-bottom:0 !important;
                            height: 45px;
                            }
                           .navbar {min-height:45px !important;}
                                    
                                    .item11   { border-color: #23282D; color: white; background-color: #23282D; }
                                    .item12 { border-color: #CED2CC; color: black; background-color: #CED2CC; }
                                    .item13 { border-color: #6AB187; color: white; background-color: #6AB187; }
                                    .item15 { border-color: #B3C100; color: black; background-color: #B3C100; }
                                    .item16 { border-color: #D32D41; color: white; background-color: #D32D41; }
                                    .item21   { border-color: #23282D; color: white; background-color: #23282D; }
                                    .item22 { border-color: #CED2CC; color: black; background-color: #CED2CC; }
                                    .item23 { border-color: #6AB187; color: white; background-color: #6AB187; }
                                    .item24 { border-color: #4CB5F5; color: black; background-color: #4CB5F5; }
                                    .item25 { border-color: #B3C100; color: black; background-color: #B3C100; }
                                    .item27 { border-color: #1F3F49; color: white; background-color: #1F3F49; }
                                    
                                    .box.box-solid.box-warning>.box-header {
                                    color:#ffffff;
                                    background:#000000
                                    }
                                    
                                    .box.box-solid.box-warning{
                                    border-bottom-color:#000000;
                                    border-left-color:#000000;
                                    border-right-color:#000000;
                                    border-top-color:#000000;
                                    }
                                    
                                    .box.box-warning>.box-header {
                                    color:#000000;
                                    background:#00000
                                    }

                                    .box.box-warning{
                                    border-bottom-color:#000000;
                                    border-left-color:#000000;
                                    border-right-color:#000000;
                                    border-top-color:#000000;
                                    }
                    ")),
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   )
                 ),
                 header = tagList(useShinydashboard()),
                 tab_apresentacao,
                 tab_marco,
                 tab_estabelecimentos,
                 tab_tipologia,
                 tabs_indicadores
                 
)

#Server - Geral-----------------------------------------------

server <- function(input, output, session){
  #Download da aba "Marco regulatório"----------------------------------------
  
  output$download_portaria <- downloadHandler(
    filename = function(){
      switch(input$marco_output_selected,
             "1" = {
               "portaria_3432.pdf"
             },
             "2" = {
               "portaria_210.pdf"
             },
             "3" = {
               "portaria_1169.pdf"
             },
             "4" = {
               "portaria_123.pdf"
             },
             "5" = {
               "portaria_630.pdf"
             },
             "6" = {
               "portaria_719.pdf"
             },
             "7" = {
               "portaria_1600.pdf"
             },
             "8" = {
               "portaria_2395.pdf"
             },
             "9" = {
               "portaria_2994.pdf"
             },
             "10" = {
               "portaria_664.pdf"
             },
             "11" = {
               "portaria_665.pdf"
             },
             "12" = {
               "portaria_433.pdf"
             },
             "13" = {
               "portaria_483.pdf"
             },
             "14" = {
               "portaria_2777.pdf"
             },
             "15" = {
               "portaria_800.pdf"
             },
             "16" = {
               "portaria_484.pdf"
             },
             "17" = {
               "portaria_2911.pdf"
             },
             "18" = {
               "deliberacao_2832.pdf"
             },
             "19" = {
               "resolucao_6502.pdf"
             },
             "20" = {
               "portaria_1846.pdf"
             },
             "21" = {
               "deliberacao_2933.pdf"
             },
             "22" = {
               "resolucao_6713.pdf"
             },
             "23" = {
               "resolucao_6905.pdf"
             },
             "24" = {
               "portaria_3408.pdf"
             },
             "25" = {
               "deliberacao_3150.pdf"
             },
             "26" = {
               "deliberacao_3160.pdf" 
             },
             "27" = {
               "deliberacao_3209.pdf"
             },
             "28" = {
               "deliberacao_3245.pdf"
             },
             "29" = {
               "deliberacao_3263.pdf"
             },
             "30" = {
               "deliberacao_3304.pdf"
             },
             "31" = {
               "resolucao_7444.pdf"
             },
             "32" = {
               "deliberacao_3386.pdf"
             },
             "33" = {
               "deliberacao_3410.pdf"
             },
             "34" = {
               "deliberacao_3415.pdf"
             },
             "35" = {
               "portaria_104.pdf"
             },
             "36" = {
               "portaria_1099.pdf"
             },
             "37" = {
               "portaria_1374.pdf"
             },
             "38" = {
               "portaria_3670.pdf"
             },
             "39" = {
               "portaria_845.pdf"
             },
             "40" = {
               "portaria_1174.pdf"
             },
             "41" = {
               "portaria_1383.pdf"
             },
             "42" = {
               "portaria_2478.pdf"
             },
             "43" = {
               "portaria_2569.pdf"
             },
             "44" = {
               "portaria_3478.pdf"
             },
             "45" = {
               "portaria_95.pdf"
             },
             "46" = {
               "portaria_104_2009.pdf"
             },
             "47" = {
               "portaria_1366.pdf"
             },
             "48" = {
               "portaria_1365.pdf"
             },
             "49" = {
               "portaria_1678.pdf"
             },
             "50" = {
               "portaria_701.pdf"
             },
             "51" = {
               "portaria_1816.pdf"
             },
             "52" = {
               "portaria_1750.pdf"
             },
             "53" = {
               "portaria_888.pdf"
             },
             "54" = {
               "portaria_3568.pdf"
             },
             "55" = {
               "portaria_3410.pdf"
             },
             "56" = {
               "portaria_1202.pdf"
             },
             "57" = {
               "deliberacao_4509.pdf"
             },
             "58" = {
               "resolucao_9210.pdf"
             },
             "59" = {
               "deliberacao_4408.pdf"
             },
             "60" = {
               "deliberacao_3812.pdf"
             },
             "61" = {
               "deliberacao_3812.pdf"
             },
             "62" = {
               "deliberacao_3733.pdf"
             },
             "63" = {
               "deliberacao_3786.pdf"
             },
             "64" = {
               "deliberacao_3914.pdf"
             },
             "65" = {
               "deliberacao_3982.pdf"
             },
             "66" = {
               "deliberacao_3782.pdf"
             },
             "67" = {
               "deliberacao_3910.pdf"
             },
             "68" = {
               "deliberacao_4183.pdf"
             },
             "69" = {
               "deliberacao_4403.pdf"
             },
             "70" = {
               "deliberacao_4359.pdf"
             },
             "71" = {
               "deliberacao_4406.pdf"
             },
             "72" = {
               "deliberacao_3533.pdf"
             },
             "73" = {
               "deliberacao_3851.pdf"
             },
             "74" = {
               "deliberacao_3844.pdf"
             },
             "75" = {
               "deliberacao_3844.pdf"
             },
             "76" = {
               "deliberacao_3977.pdf"
             },
             "77" = {
               "resolucao_8301.pdf"
             },
             "78" = {
               "deliberacao_4282.pdf"
             },
             "79" = {
               "resolucao_6714.pdf"
             },
             "80" = {
               "deliberacao_3416.pdf"
             },
             "81" = {
               "portaria_1273.pdf"
             },
             "82" = {
               "deliberacao_3536.pdf"
             },
             "83" = {
               "deliberacao_3532.pdf"
             },
             "84" = {
               "deliberacao_3625.pdf"
             },
             "85" = {
               "deliberacao_3624.pdf"
             },
             "86" = {
               "deliberacao_3677.pdf"
             },
             "87" = {
               "deliberacao_3416.pdf"
             },
             "88" = {
               "deliberacao_3594.pdf"
             },
             "89" = {
               "deliberacao_3673.pdf"
             },
             "90" = {
               "deliberacao_4124.pdf"
             },
             "91" = {
               "resolucao_8636.pdf"
             },
             "92" = {
               "deliberacao_4151.pdf"
             },
             "93" = {
               "resolucao_8694.pdf"
             },
             "94" = {
               "deliberacao_4296.pdf"
             },
             "95" = {
               "deliberacao_4326.pdf"
             },
             "96" = {
               "deliberacao_4405.pdf"
             },
             "97" = {
               "resolucao_9210.pdf"
             },
             "98" = {
               "deliberacao_4509.pdf"
             },
             "99" = {
               "deliberacao_3909.pdf"
             },
             "100" = {
               "portaria_1996.pdf"
             },
             "101" = {
               "portaria_29.pdf"
             },
             "102" = {
               "portaria_3128.pdf"
             },
             "103" = {
               "portaria_2211.pdf"
             },
             "104" = {
               "portaria_3438.pdf"
             },
             "105" = {
               "deliberacao_3763.pdf"
             },
             "106" = {
               "resolucao_9074.pdf"
             },
             "107" = {
               "deliberacao_4044.pdf"
             },
             "108" = {
               "deliberacao_4505.pdf"
             },
             "109" = {
               "resolucao_9112.pdf"
             },
             "110" = {
               "deliberacao_4439.pdf"
             },
             "111" = {
               "deliberacao_4408.pdf"
             },
             "112" = {
               "resolucao_8983.pdf"
             },
             "113" = {
               "resolucao_8984.pdf"
             },
             "114" = {
               "deliberacao_4050.pdf"
             },
             "115" = {
               "deliberacao_3951.pdf"
             },
             "116" = {
               "resolucao_8301.pdf"
             },
             "117" = {
               "resolucao_8357.pdf"
             },
             "118" = {
               "deliberacao_3906.pdf"
             },
             "119" = {
               "deliberacao_3909.pdf"
             })
    },
    content = function(file) {
      switch(input$marco_output_selected,
             "1" = {
               file.copy("www/portaria_3432.pdf", file)
             },
             "2" = {
               file.copy("www/portaria_210.pdf", file)
             },
             "3" = {
               file.copy("www/portaria_1169.pdf", file)
             },
             "4" = {
               file.copy("www/portaria_123.pdf", file)
             },
             "5" = {
               file.copy("www/portaria_630.pdf", file)
             },
             "6" = {
               file.copy("www/portaria_719.pdf", file)
             },
             "7" = {
               file.copy("www/portaria_1600.pdf", file)
             },
             "8" = {
               file.copy("www/portaria_2395.pdf", file)
             },
             "9" = {
               file.copy("www/portaria_2994.pdf", file)
             },
             "10" = {
               file.copy("www/portaria_664.pdf", file)
             },
             "11" = {
               file.copy("www/portaria_665.pdf", file)
             },
             "12" = {
               file.copy("www/portaria_433.pdf", file)
             },
             "13" = {
               file.copy("www/portaria_483.pdf", file)
             },
             "14" = {
               file.copy("www/portaria_2777.pdf", file)
             },
             "15" = {
               file.copy("www/portaria_800.pdf", file)
             },
             "16" = {
               file.copy("www/portaria_484.pdf", file)
             },
             "17" = {
               file.copy("www/portaria_2911.pdf", file)
             },
             "18" = {
               file.copy("www/deliberacao_2832.pdf", file)
             },
             "19" = {
               file.copy("www/resolucao_6502.pdf", file)
             },
             "20" = {
               file.copy("www/portaria_1846.pdf", file)
             },
             "21" = {
               file.copy("www/deliberacao_2933.pdf", file)
             },
             "22" = {
               file.copy("www/resolucao_6713.pdf", file)
             },
             "23" = {
               file.copy("www/resolucao_6905.pdf", file)
             },
             "24" = {
               file.copy("www/portaria_3408.pdf", file)
             },
             "25" = {
               file.copy("www/deliberacao_3150.pdf", file)
             },
             "26" = {
               file.copy("www/deliberacao_3160.pdf", file)
             },
             "27" = {
               file.copy("www/deliberacao_3209.pdf", file)
             },
             "28" = {
               file.copy("www/deliberacao_3245.pdf", file)
             },
             "29" = {
               file.copy("www/deliberacao_3263.pdf", file)
             },
             "30" = {
               file.copy("www/deliberacao_3304.pdf", file)
             },
             "31" = {
               file.copy("www/resolucao_7444.pdf", file)
             },
             "32" = {
               file.copy("www/deliberacao_3386.pdf", file)
             },
             "33" = {
               file.copy("www/deliberacao_3410.pdf", file)
             },
             "34" = {
               file.copy("www/deliberacao_3415.pdf", file)
             },
             "35" = {
               file.copy("www/portaria_104.pdf", file)
             },
             "36" = {
               file.copy("www/portaria_1099.pdf", file)
             },
             "37" = {
               file.copy("www/portaria_1374.pdf", file)
             },
             "38" = {
               file.copy("www/portaria_3670.pdf", file)
             },
             "39" = {
               file.copy("www/portaria_845.pdf", file)
             },
             "40" = {
               file.copy("www/portaria_1174.pdf", file)
             },
             "41" = {
               file.copy("www/portaria_1383.pdf", file)
             },
             "42" = {
               file.copy("www/portaria_2478.pdf", file)
             },
             "43" = {
               file.copy("www/portaria_2569.pdf", file)
             },
             "44" = {
               file.copy("www/portaria_3478.pdf", file)
             },
             "45" = {
               file.copy("www/portaria_95.pdf", file)
             },
             "46" = {
               file.copy("www/portaria_104_2009.pdf", file)
             },
             "47" = {
               file.copy("www/portaria_1366.pdf", file)
             },
             "48" = {
               file.copy("www/portaria_1365.pdf", file)
             },
             "49" = {
               file.copy("www/portaria_1678.pdf", file)
             },
             "50" = {
               file.copy("www/portaria_701.pdf", file)
             },
             "51" = {
               file.copy("www/portaria_1816.pdf", file)
             },
             "52" = {
               file.copy("www/portaria_1750.pdf", file)
             },
             "53" = {
               file.copy("www/portaria_888.pdf", file)
             },
             "54" = {
               file.copy("www/portaria_3568.pdf", file)
             },
             "55" = {
               file.copy("www/portaria_3410.pdf", file)
             },
             "56" = {
               file.copy("www/portaria_1202.pdf", file)
             },
             "57" = {
               file.copy("www/deliberacao_4509.pdf", file)
             },
             "58" = {
               file.copy("www/resolucao_9210.pdf", file)
             },
             "59" = {
               file.copy("www/deliberacao_4408.pdf", file)
             },
             "60" = {
               file.copy("www/deliberacao_3812.pdf", file)
             },
             "61" = {
               file.copy("www/deliberacao_3812.pdf", file)
             },
             "62" = {
               file.copy("www/deliberacao_3733.pdf", file)
             },
             "63" = {
               file.copy("www/deliberacao_3786.pdf", file)
             },
             "64" = {
               file.copy("www/deliberacao_3914.pdf", file)
             },
             "65" = {
               file.copy("www/deliberacao_3982.pdf", file)
             },
             "66" = {
               file.copy("www/deliberacao_3782.pdf", file)
             },
             "67" = {
               file.copy("www/deliberacao_3910.pdf", file)
             },
             "68" = {
               file.copy("www/deliberacao_4183.pdf", file)
             },
             "69" = {
               file.copy("www/deliberacao_4403.pdf", file)
             },
             "70" = {
               file.copy("www/deliberacao_4359.pdf", file)
             },
             "71" = {
               file.copy("www/deliberacao_4406.pdf", file)
             },
             "72" = {
               file.copy("www/deliberacao_3533.pdf", file)
             },
             "73" = {
               file.copy("www/deliberacao_3851.pdf", file)
             },
             "74" = {
               file.copy("www/deliberacao_3844.pdf", file)
             },
             "75" = {
               file.copy("www/deliberacao_3844.pdf", file)
             },
             "76" = {
               file.copy("www/deliberacao_3977.pdf", file)
             },
             "77" = {
               file.copy("www/resolucao_8301.pdf", file)
             },
             "78" = {
               file.copy("www/deliberacao_4282.pdf", file)
             },
             "79" = {
               file.copy("www/resolucao_6714.pdf", file)
             },
             "80" = {
               file.copy("www/deliberacao_3416.pdf", file)
             },
             "81" = {
               file.copy("www/portaria_1273.pdf", file)
             },
             "82" = {
               file.copy("www/deliberacao_3536.pdf", file)
             },
             "83" = {
               file.copy("www/deliberacao_3532.pdf", file)
             },
             "84" = {
               file.copy("www/deliberacao_3625.pdf", file)
             },
             "85" = {
               file.copy("www/deliberacao_3624.pdf", file)
             },
             "86" = {
               file.copy("www/deliberacao_3677.pdf", file)
             },
             "87" = {
               file.copy("www/deliberacao_3416.pdf", file)
             },
             "88" = {
               file.copy("www/deliberacao_3594.pdf", file)
             },
             "89" = {
               file.copy("www/deliberacao_3673.pdf", file)
             },
             "90" = {
               file.copy("www/deliberacao_4124.pdf", file)
             },
             "91" = {
               file.copy("www/resolucao_8636.pdf", file)
             },
             "92" = {
               file.copy("www/deliberacao_4151.pdf", file)
             },
             "93" = {
               file.copy("www/resolucao_8694.pdf", file)
             },
             "94" = {
               file.copy("www/deliberacao_4296.pdf", file)
             },
             "95" = {
               file.copy("www/deliberacao_4326.pdf", file)
             },
             "96" = {
               file.copy("www/deliberacao_4405.pdf", file)
             },
             "97" = {
               file.copy("www/resolucao_9210.pdf", file)
             },
             "98" = {
               file.copy("www/deliberacao_4509.pdf", file)
             },
             "99" = {
               file.copy("www/deliberacao_3909.pdf", file)
             },
             "100" = {
               file.copy("www/portaria_1996.pdf", file)
             },
             "101" = {
               file.copy("www/portaria_29.pdf", file)
             },
             "102" = {
               file.copy("www/portaria_3128.pdf", file)
             },
             "103" = {
               file.copy("www/portaria_2211.pdf", file)
             },
             "104" = {
               file.copy("www/portaria_3438.pdf", file)
             },
             "105" = {
               file.copy("www/deliberacao_3763.pdf", file)
             },
             "106" = {
               file.copy("www/resolucao_9074.pdf", file)
             },
             "107" = {
               file.copy("www/deliberacao_4044.pdf", file)
             },
             "108" = {
               file.copy("www/deliberacao_4505.pdf", file)
             },
             "109" = {
               file.copy("www/resolucao_9112.pdf", file)
             },
             "110" = {
               file.copy("www/deliberacao_4439.pdf", file)
             },
             "111" = {
               file.copy("www/deliberacao_4408.pdf", file)
             },
             "112" = {
               file.copy("www/resolucao_8983.pdf", file)
             },
             "113" = {
               file.copy("www/resolucao_8984.pdf", file)
             },
             "114" = {
               file.copy("www/deliberacao_4050.pdf", file)
             },
             "115" = {
               file.copy("www/deliberacao_3951.pdf", file)
             },
             "116" = {
               file.copy("www/resolucao_8301.pdf", file)
             },
             "117" = {
               file.copy("www/resolucao_8357.pdf", file)
             },
             "118" = {
               file.copy("www/deliberacao_3906.pdf", file)
             },
             "119" = {
               file.copy("www/deliberacao_3909.pdf", file)
             })
    }
  )
  
  #Linha do tempo da aba "Marco regulatório"-------------------------------
  output$marco_output <- renderTimevis({
    switch(input$nivel_input,
           "Todas as regulações" = {
             switch(input$regulamentacoes_all_input,
                    "Todas as regulações" = {df <- timedata
                                            centro <- "1996-06-30"},
                    "IAM" = {df <- timedata %>% filter(subgroup == '1.2'|subgroup == '2.2')
                             centro <- "2003-06-30"},
                    "AVC" = {df <- timedata %>% filter(subgroup == '1.3'|subgroup == '2.3')
                             centro <- "2010-06-30"},
                    "Queimados" = {df <- timedata %>% filter(subgroup == '1.5'|subgroup == '2.5')
                             centro <- "1999-06-30"},
                    "Trauma" = {df <- timedata %>% filter(subgroup == '1.6')
                                centro <- "2004-06-30"},
                    "Rede de Resposta" = {df <- timedata %>% filter(subgroup == '2.4')
                                centro <- "2019-06-30"},
                    "Valora Minas" = {df <- timedata %>% filter(subgroup == '2.7')
                                centro <- "2021-06-30"},
                    "Outros/Misto" = {df <- timedata %>% filter(subgroup == '1.1'|subgroup == '2.1')
                                 centro <- "1998-06-30"})
           },
           "Estadual" = {
             switch(input$regulamentacoes_mg_input,
                    "Todas as regulações" = {df <- timedata %>% filter(group == '2')
                    centro <- "2017-06-30"},
                    "IAM" = {df <- timedata %>% filter(subgroup == '2.2')
                    centro <- "2022-06-30"},
                    "AVC" = {df <- timedata %>% filter(subgroup == '2.3')
                    centro <- "2020-06-30"},
                    "Queimados" = {df <- timedata %>% filter(subgroup == '2.5')
                    centro <- "2022-06-30"},
                    "Trauma" = {df <- timedata %>% filter(subgroup == '1.6')
                    centro <- "2004-06-30"},
                    "Rede de Resposta" = {df <- timedata %>% filter(subgroup == '2.4')
                    centro <- "2019-06-30"},
                    "Valora Minas" = {df <- timedata %>% filter(subgroup == '2.7')
                    centro <- "2021-06-30"},
                    "Outros/Misto" = {df <- timedata %>% filter(subgroup == '2.1')
                    centro <- "2021-06-30"})
           },
           "Federal" = {
             switch(input$regulamentacoes_br_input,
                    "Todas as regulações" = {df <- timedata %>% filter(group == '1')
                    centro <- "1996-06-30"},
                    "IAM" = {df <- timedata %>% filter(subgroup == '1.2')
                    centro <- "2003-06-30"},
                    "AVC" = {df <- timedata %>% filter(subgroup == '1.3')
                    centro <- "2010-06-30"},
                    "Queimados" = {df <- timedata %>% filter(subgroup == '1.5')
                    centro <- "1999-06-30"},
                    "Trauma" = {df <- timedata %>% filter(subgroup == '1.6')
                    centro <- "2004-06-30"},
                    "Outros/Misto" = {df <- timedata %>% filter(subgroup == '1.1')
                    centro <- "1998-06-30"})
           })
    df = data.frame(df)
    linha_tempo <- timevis(data = df, groups = groups)
    linha_tempo <- linha_tempo %>% centerTime(centro)
    linha_tempo
  })
  
  #Função de rearranjamento da data-------------------------------------------
  data_rearranjo <- function(data){
    ano = substring(data,1,4)
    mes = substring(data,6,7)
    switch(mes,
           "01" = {
             mes <- "janeiro"
           },
           "02" = {
             mes <- "fevereiro"
           },
           "03" = {
             mes <- "março"
           },
           "04" = {
             mes <- "abril"
           },
           "05" = {
             mes <- "maio"
           },
           "06" = {
             mes <- "junho"
           },
           "07" = {
             mes <- "julho"
           },
           "08" = {
             mes <- "agosto"
           },
           "09" = {
             mes <- "setembro"
           },
           "10" = {
             mes <- "outubro"
           },
           "11" = {
             mes <- "novembro"
           },
           "12" = {
             mes <- "dezembro"
           })
    dia = substring(data,9,10)
    
    if(as.numeric(dia) < 10){
      if(dia == "01"){
        dia <- "1º"
      } else{
        dia <- substr(dia,2,2)
      }
    }
    data_reajustada = paste(dia,"de",mes,"de",ano)
    return(data_reajustada)
  }
  
  #Pop-up da aba "Marco regulatório"
  observeEvent(input$marco_output_selected, {
    data = data_rearranjo(timedata %>% filter(id == input$marco_output_selected) %>% pull(start))
    
    
    showModal(modalDialog(
      title = timedata %>% filter(id == input$marco_output_selected) %>% pull(content),
      p("Data:",data, style ="text-align: justify;"),
      p("Especialidade:",timedata %>% filter(id == input$marco_output_selected) %>% pull(area), style ="text-align: justify;"),
      p("Tema:",timedata %>% filter(id == input$marco_output_selected) %>% pull(theme), style ="text-align: justify;"),
      if(!is.na(timedata %>% filter(id == input$marco_output_selected) %>% pull(complement))){
        p("Complemento:",timedata %>% filter(id == input$marco_output_selected) %>% pull(complement), style ="text-align: justify;")
      },
      div(
        style = "position:relative; left:calc(37.5%);",
        downloadButton(
          outputId = "download_portaria",
          label = "Download",
          style = "color: #fff; background-color: #222222; border-color: #fff;"
        ) 
      ),
      easyClose = FALSE,
      footer = modalButton("Fechar")
    ))
  })
  
  #Mapas da aba "Estabelecimentos"-------------------------------------------
  latitude_inicial <- -18.10
  longitude_inicial <- -44.38
  zoom_inicial  <- 4.5
  
  grupos_lista <- c("<img src= 'green.png' height = '32' width = '24'> Habilitados IAM/AVC", "<img src='blue.png' height = '32' width = '24'> Rede Resposta","<img src= 'red.png' height = '32' width = '24'> Não Habilitados")
  
  inputs <- reactiveValues(municipio = NULL, microrregiao = NULL, macrorregiao = NULL, estabelecimentos = NULL)
  
  output$mapa_estabelecimentos <- renderLeaflet({
    leaflet() %>% 
      setView(lat = latitude_inicial, lng = longitude_inicial, zoom = zoom_inicial) %>%
      addTiles() %>% addPolygons(data = mg_shp, color = "#444444", weight = 1, smoothFactor = 0.5,
                                 opacity = 1.0, fillOpacity = 0,
                                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                     bringToFront = TRUE), group = "Municípios") %>%
      addPolygons(data = mg_micros_shp, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE), group = "Microrregiões") %>%
      addPolygons(data = mg_macros_shp, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE), group = "Macrorregiões") %>%
      addMarkers(df_estabelecimentos_final$lon[df_estabelecimentos_final$Tipologia == 'Habilitados IAM/AVC'], df_estabelecimentos_final$lat[df_estabelecimentos_final$Tipologia == 'Habilitados IAM/AVC'], icon= typeIcons[df_estabelecimentos_final$cor[df_estabelecimentos_final$Tipologia == 'Habilitados IAM/AVC']], group = "<img src= 'green.png' height = '32' width = '24'> Habilitados IAM/AVC", label=df_estabelecimentos_final$no_fantasia[df_estabelecimentos_final$Tipologia == 'Habilitados IAM/AVC']) %>% 
      addMarkers(df_estabelecimentos_final$lon[df_estabelecimentos_final$Tipologia == 'Rede Resposta'], df_estabelecimentos_final$lat[df_estabelecimentos_final$Tipologia == 'Rede Resposta'], icon= typeIcons[df_estabelecimentos_final$cor[df_estabelecimentos_final$Tipologia == 'Rede Resposta']], group = "<img src='blue.png' height = '32' width = '24'> Rede Resposta", label=df_estabelecimentos_final$no_fantasia[df_estabelecimentos_final$Tipologia == 'Rede Resposta']) %>% 
      addMarkers(df_estabelecimentos_final$lon[df_estabelecimentos_final$Tipologia == 'Não Habilitados'], df_estabelecimentos_final$lat[df_estabelecimentos_final$Tipologia == 'Não Habilitados'], icon= typeIcons[df_estabelecimentos_final$cor[df_estabelecimentos_final$Tipologia == 'Não Habilitados']], group = "<img src= 'red.png' height = '32' width = '24'> Não Habilitados", label=df_estabelecimentos_final$no_fantasia[df_estabelecimentos_final$Tipologia == 'Não Habilitados']) %>%
      addLayersControl(baseGroups = c("Municípios","Microrregiões","Macrorregiões"),
                       overlayGroups = grupos_lista,
                       options = layersControlOptions(collapsed = FALSE))  %>%
      htmlwidgets::onRender("
      function(el, x) {
        var mapa = this;
        mapa.on('baselayerchange',
          function (e) {
            Shiny.onInputChange('mapa_estabelecimentos_polygon', e.layer.groupname)
        })
    }")
    
    
  })
  
  observeEvent(input$municipio_input,
               {inputs$municipio <-input$municipio_input
               if(inputs$municipio != "--Todos os municípios--"){
                 leafletProxy("mapa_estabelecimentos") %>% fitBounds(0.99*coordenadas_municipios$X1[coordenadas_municipios$cidades == inputs$municipio], 0.99*coordenadas_municipios$X2[coordenadas_municipios$cidades == inputs$municipio],
                                                                     1.01*coordenadas_municipios$X1[coordenadas_municipios$cidades == inputs$municipio], 1.01*coordenadas_municipios$X2[coordenadas_municipios$cidades == inputs$municipio])
               } else{
                 leafletProxy("mapa_estabelecimentos") %>% setView(lng = longitude_inicial, lat = latitude_inicial, zoom = zoom_inicial)
               }
               })
  
  observeEvent(input$microrregiao_input,
               {inputs$microrregiao <-input$microrregiao_input
               if(inputs$microrregiao != "--Todas as microrregiões--"){
                 leafletProxy("mapa_estabelecimentos") %>% fitBounds(0.98*coordenadas_micros$X1[coordenadas_micros$microrregiao == inputs$microrregiao], 0.98*coordenadas_micros$X2[coordenadas_micros$microrregiao == inputs$microrregiao],
                                                                     1.02*coordenadas_micros$X1[coordenadas_micros$microrregiao == inputs$microrregiao], 1.02*coordenadas_micros$X2[coordenadas_micros$microrregiao == inputs$microrregiao])
               } else{
                 leafletProxy("mapa_estabelecimentos") %>% setView(lng = longitude_inicial, lat = latitude_inicial, zoom = zoom_inicial)
               }
               })
  
  observeEvent(input$macrorregiao_input,
               {inputs$macrorregiao <-input$macrorregiao_input
               if(input$macrorregiao_input != "--Todas as macrorregiões--"){
                 leafletProxy("mapa_estabelecimentos") %>% fitBounds(0.95*coordenadas_macros$X1[coordenadas_macros$macrorregiao == input$macrorregiao_input], 0.95*coordenadas_macros$X2[coordenadas_macros$macrorregiao == input$macrorregiao_input],
                                                                     1.05*coordenadas_macros$X1[coordenadas_macros$macrorregiao == input$macrorregiao_input], 1.05*coordenadas_macros$X2[coordenadas_macros$macrorregiao == input$macrorregiao_input])
               } else{
                 leafletProxy("mapa_estabelecimentos") %>% setView(lng = longitude_inicial, lat = latitude_inicial, zoom = zoom_inicial)
               }
               })
  
  observeEvent(input$mapa_estabelecimentos_groups,
               inputs$estabelecimentos <- grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )])
  
  observeEvent(input$mapa_estabelecimentos_polygon,{
    leafletProxy("mapa_estabelecimentos") %>% setView(lng = longitude_inicial, lat = latitude_inicial, zoom = zoom_inicial)
    
    switch(input$mapa_estabelecimentos_polygon,
           "Municípios" = {
             updateSelectInput(session,
                               "microrregiao_input",
                               selected = "--Todas as microrregiões--"
             )
             updateSelectInput(session,
                               "macrorregiao_input",
                               selected = "--Todas as macrorregiões--"
             )
           },
           "Microrregiões" = {
             updateSelectizeInput(session,
                                  "municipio_input",
                                  selected = "--Todos os municípios--"
             )
             updateSelectInput(session,
                               "macrorregiao_input",
                               selected = "--Todas as macrorregiões--"
             )
           },
           "Macrorregiões" = {
             updateSelectizeInput(session,
                                  "municipio_input",
                                  selected = "--Todos os municípios--"
             )
             updateSelectInput(session,
                               "microrregiao_input",
                               selected = "--Todas as microrregiões--"
             )
           }
    )
  })
  
  #Tabela de exibição da aba "Estabelecimentos"-------------------------------
  tabela_filtrada <- function(entrada, condicao, coluna){
    if(entrada == condicao){
      if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 3 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[1] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][2] == grupos_lista[2] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][3] == grupos_lista[3]){
        tabela <- df_estabelecimentos_final
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 2 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[1] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][2] == grupos_lista[2]){
        tabela <- df_estabelecimentos_final %>% filter(Tipologia != 'Não Habilitados')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 2 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[1] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][2] == grupos_lista[3]){
        tabela <- df_estabelecimentos_final %>% filter(Tipologia != 'Rede Resposta')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 2 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[2] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][2] == grupos_lista[3]){
        tabela <- df_estabelecimentos_final %>% filter(Tipologia != 'Habilitados IAM/AVC')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 1 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[1]){
        tabela <- df_estabelecimentos_final %>% filter(Tipologia == 'Habilitados IAM/AVC')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 1 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[2]){
        tabela <- df_estabelecimentos_final %>% filter(Tipologia == 'Rede Resposta')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 1 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups)][1] == grupos_lista[3]){
        tabela <- df_estabelecimentos_final %>% filter(Tipologia == 'Não Habilitados')
      } else{
        tabela <- df_estabelecimentos_final %>% filter(Tipologia != 'Habilitados IAM/AVC' & Tipologia != 'Rede Resposta' & Tipologia != 'Não Habilitados')
      }
    } else{
      if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 3 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[1] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][2] == grupos_lista[2] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][3] == grupos_lista[3]){
        tabela <- df_estabelecimentos_final %>% filter(!!sym(coluna) == entrada)
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 2 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[1] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][2] == grupos_lista[2]){
        tabela <- df_estabelecimentos_final %>% filter(!!sym(coluna) == entrada & Tipologia != 'Não Habilitados')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 2 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[1] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][2] == grupos_lista[3]){
        tabela <- df_estabelecimentos_final %>% filter(!!sym(coluna) == entrada & Tipologia != 'Rede Resposta')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 2 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[2] && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][2] == grupos_lista[3]){
        tabela <- df_estabelecimentos_final %>% filter(!!sym(coluna) == entrada & Tipologia != 'Habilitados IAM/AVC')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 1 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[1]){
        tabela <- df_estabelecimentos_final %>% filter(!!sym(coluna) == entrada & Tipologia == 'Habilitados IAM/AVC')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 1 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )][1] == grupos_lista[2]){
        tabela <- df_estabelecimentos_final %>% filter(!!sym(coluna) == entrada & Tipologia == 'Rede Resposta')
      } else if(length(grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups )]) == 1 && grupos_lista[which(grupos_lista %in% input$mapa_estabelecimentos_groups)][1] == grupos_lista[3]){
        tabela <- df_estabelecimentos_final %>% filter(!!sym(coluna) == entrada & Tipologia == 'Não Habilitados')
      } else{
        tabela <- df_estabelecimentos_final %>% filter(!!sym(coluna) == entrada & Tipologia != 'Habilitados IAM/AVC' & Tipologia != 'Rede Resposta' & Tipologia != 'Não Habilitados')
      }
    }
    
    tabela <- tabela[,c(1,2,4,6,8,9,10,11,47,44)]
    tabela$tx_ocup2019 <- format(round(tabela$tx_ocup2019, 2), nsmall = 2)
    colnames(tabela) = c("CNES", "Nome fantasia", "Município", "Microrregião", "Macrorregião", "Tipologia", "Tipo do estabelecimento", "Leitos SUS", "Porte","Taxa de ocupação (%, 2019)")
    
    return(tabela)
  }
  
  tabela_filtrada_municipios <- reactive({
    tabela <- tabela_filtrada(inputs$municipio, "--Todos os municípios--", 'nome_municipio')
  })
  
  tabela_filtrada_microrregioes <- reactive({
    tabela <- tabela_filtrada(inputs$microrregiao, "--Todas as microrregiões--", 'micro')
  })
  
  tabela_filtrada_macrorregioes <- reactive({
    tabela <- tabela_filtrada(inputs$macrorregiao, "--Todas as macrorregiões--", 'macro')
  })
  
  output$tabela_municipios <- DT::renderDataTable(
    tabela_filtrada_municipios(),
    options = list(scrollX = '400px',
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$tabela_microrregioes <- DT::renderDataTable(
    tabela_filtrada_microrregioes(),
    options = list(scrollX = '400px',
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$tabela_macrorregioes <- DT::renderDataTable(
    tabela_filtrada_macrorregioes(),
    options = list(scrollX = '400px',
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  #Tabela de descritivas - Aba "Estabelecimentos"
  descritivas_estabelecimentos <- function(tabela_entrada){
    grupo_hospitais <- c("Habilitados IAM/AVC", "Rede Resposta", "Não Habilitados", "Total")
    numero_hospitais <- vector()
    numero_hospitais$h <- nrow(tabela_entrada %>% filter(Tipologia == "Habilitados IAM/AVC"))
    numero_hospitais$rr <- nrow(tabela_entrada %>% filter(Tipologia == "Rede Resposta"))
    numero_hospitais$nh <- nrow(tabela_entrada %>% filter(Tipologia == "Não Habilitados"))
    numero_hospitais$t <- numero_hospitais$h + numero_hospitais$rr + numero_hospitais$nh
    
    porcentagem_hospitais <- vector()
    porcentagem_hospitais$h <- 0
    porcentagem_hospitais$rr <- 0
    porcentagem_hospitais$nh <- 0
    
    ifelse(numero_hospitais$t == 0,
           {porcentagem_hospitais$h <-0
           porcentagem_hospitais$rr <-0
           porcentagem_hospitais$nh <-0},
           {porcentagem_hospitais$h <- (numero_hospitais$h/numero_hospitais$t)*100
           porcentagem_hospitais$rr <- (numero_hospitais$rr/numero_hospitais$t)*100
           porcentagem_hospitais$nh <- (numero_hospitais$nh/numero_hospitais$t)*100})
    porcentagem_hospitais$t <- porcentagem_hospitais$h + porcentagem_hospitais$rr + porcentagem_hospitais$nh
    numero_hospitais <-format(round(as.numeric(numero_hospitais), 0), nsmall = 0)
    
    leitos_n <- vector()
    leitos_n$h <- sum(tabela_entrada %>% filter(Tipologia == "Habilitados IAM/AVC") %>% pull("Leitos SUS"))
    leitos_n$rr <- sum(tabela_entrada %>% filter(Tipologia == "Rede Resposta") %>% pull("Leitos SUS"))
    leitos_n$nh <- sum(tabela_entrada %>% filter(Tipologia == "Não Habilitados") %>% pull("Leitos SUS"))
    leitos_n$t <- leitos_n$h + leitos_n$rr + leitos_n$nh
    
    leitos_p <- vector()
    leitos_p$h <- 0
    leitos_p$rr <- 0
    leitos_p$nh <- 0
    
    ifelse(leitos_n$t == 0,
           {leitos_p$h <-0
           leitos_p$rr <-0
           leitos_p$nh <-0},
           {leitos_p$h <- (leitos_n$h/leitos_n$t)*100
           leitos_p$rr <- (leitos_n$rr/leitos_n$t)*100
           leitos_p$nh <- (leitos_n$nh/leitos_n$t)*100})
    
    leitos_p$t <- leitos_p$h + leitos_p$rr + leitos_p$nh
    leitos_n <- format(round(as.numeric(leitos_n),0),nsmall=0)
    
    pequeno_n<- vector()
    pequeno_n$h <- tabela_entrada %>% filter(Tipologia == "Habilitados IAM/AVC" & Porte == "Pequeno") %>% count()
    pequeno_n$rr <- tabela_entrada %>% filter(Tipologia == "Rede Resposta" & Porte == "Pequeno") %>% count()
    pequeno_n$nh <- tabela_entrada %>% filter(Tipologia == "Não Habilitados" & Porte == "Pequeno") %>% count()
    pequeno_n$t <- pequeno_n$h + pequeno_n$rr + pequeno_n$nh
    
    pequeno_p <- vector()
    pequeno_p$h <- 0
    pequeno_p$rr <- 0
    pequeno_p$nh <- 0
    
    ifelse(pequeno_n$t == 0,
           {pequeno_p$h <-0
           pequeno_p$rr <-0
           pequeno_p$nh <-0},
           {pequeno_p$h <- (pequeno_n$h/pequeno_n$t)*100
           pequeno_p$rr <- (pequeno_n$rr/pequeno_n$t)*100
           pequeno_p$nh <- (pequeno_n$nh/pequeno_n$t)*100})
    pequeno_p$t <- pequeno_p$h + pequeno_p$rr + pequeno_p$nh
    
    medio_n<- vector()
    medio_n$h <- tabela_entrada %>% filter(Tipologia == "Habilitados IAM/AVC" & Porte == "Médio") %>% count()
    medio_n$rr <- tabela_entrada %>% filter(Tipologia == "Rede Resposta" & Porte == "Médio") %>% count()
    medio_n$nh <- tabela_entrada %>% filter(Tipologia == "Não Habilitados" & Porte == "Médio") %>% count()
    medio_n$t <- medio_n$h + medio_n$rr + medio_n$nh
    
    medio_p <- vector()
    medio_p$h <- 0
    medio_p$rr <- 0
    medio_p$nh <- 0
    
    ifelse(medio_n$t == 0,
           {medio_p$h <-0
           medio_p$rr <-0
           medio_p$nh <-0},
           {medio_p$h <- (medio_n$h/medio_n$t)*100
           medio_p$rr <- (medio_n$rr/medio_n$t)*100
           medio_p$nh <- (medio_n$nh/medio_n$t)*100})
    medio_p$t <- medio_p$h + medio_p$rr + medio_p$nh
    
    grande_n<- vector()
    grande_n$h <- tabela_entrada %>% filter(Tipologia == "Habilitados IAM/AVC" & Porte == "Grande") %>% count()
    grande_n$rr <- tabela_entrada %>% filter(Tipologia == "Rede Resposta" & Porte == "Grande") %>% count()
    grande_n$nh <- tabela_entrada %>% filter(Tipologia == "Não Habilitados" & Porte == "Grande") %>% count()
    grande_n$t <- grande_n$h + grande_n$rr + grande_n$nh
    
    grande_p <- vector()
    grande_p$h <- 0
    grande_p$rr <- 0
    grande_p$nh <- 0
    
    ifelse(grande_n$t == 0,
           {grande_p$h <-0
           grande_p$rr <-0
           grande_p$nh <-0},
           {grande_p$h <- (grande_n$h/grande_n$t)*100
           grande_p$rr <- (grande_n$rr/grande_n$t)*100
           grande_p$nh <- (grande_n$nh/grande_n$t)*100})
    grande_p$t <- grande_p$h + grande_p$rr + grande_p$nh
    
    tabela <- cbind(grupo_hospitais, numero_hospitais, porcentagem_hospitais, leitos_n, leitos_p, pequeno_p, medio_p, grande_p)
    colnames(tabela) <- c("Grupo de hospitais", "Número", "Proporção (%)", "Leitos SUS", "Leitos SUS (%)", "Pequeno porte (%)", "Médio porte (%)", "Grande porte (%)")
    return(tabela)
  }
  
  output$caracterizacao_municipios <- renderTable({
    descritivas_estabelecimentos(tabela_filtrada_municipios())
  })
  
  output$caracterizacao_microrregioes <- renderTable({
    descritivas_estabelecimentos(tabela_filtrada_microrregioes())
  })
  
  output$caracterizacao_macrorregioes <- renderTable({
    descritivas_estabelecimentos(tabela_filtrada_macrorregioes())
  })
  
  #Download - Aba "Estabelecimentos"-----------------------------------------
  
  arquivo_download <- reactive({
    if(length(input$mapa_estabelecimentos_polygon) == 0||input$mapa_estabelecimentos_polygon == "Municípios"){
      tabela_filtrada_municipios()
      
    } else if(input$mapa_estabelecimentos_polygon == "Microrregiões"){
      tabela_filtrada_microrregioes()
      
    } else if(input$mapa_estabelecimentos_polygon == "Macrorregiões"){
      tabela_filtrada_macrorregioes()
    }
  })
  
  output$download_estabelecimentos <- downloadHandler(
    filename = function(){
      paste('pesquisa_estabelecimentos', Sys.Date(),'.xlsx',sep='')
    },
    content = function(file){
      write_xlsx(arquivo_download(), file)
    }
  )
  
  
  #Mapa - Aba "Tipologia"----------------------------------------------------
  output$mapa_tipologia <- renderLeaflet({
    data_map <- merge(mg_micros_shp, df_micro[,c("codmicro","tipologia_micro_oferta_ocup_desemp")], by.x = "c_micro", by.y = 'codmicro')
    proj4string(data_map) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
    Encoding(data_map$n_micro) <- "UTF-8"
    
    pal <- colorFactor(rev(paleta), data_map$tipologia_micro_oferta_ocup_desemp)
    
    leaflet(data = data_map, options = leafletOptions(doubleClickZoom= FALSE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(opacity = 1), group = "ESRI World Imagery") %>%
      addPolygons(fillColor = ~pal(tipologia_micro_oferta_ocup_desemp), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1,
                  layerId = ~as.character(c_micro),
                  popup = ~n_micro) %>%
      addLegend("bottomright", pal = pal, values = ~tipologia_micro_oferta_ocup_desemp,
                labels = "Tipologia",
                title = "Tipologia")
    
  })
  
  clicklist <- reactiveValues(ids = vector())
  
  observeEvent(input$mapa_tipologia_shape_click,
               {
                 click_id <- input$mapa_tipologia_shape_click$id
                 clicklist$ids <- c(clicklist$ids, click_id)
                 tipo <- df_micro %>% filter(codmicro == click_id) %>% pull(tipologia_micro_oferta_ocup_desemp)
                 df_hl <- df_micro
                 df_hl$cor <- ""
                 df_hl$espessura <- ""
                 for(i in 1:nrow(df_hl)){
                   if(df_hl$tipologia_micro_oferta_ocup_desemp[i] == tipo){
                     df_hl$cor[i] <- "#FFFFFF"
                     df_hl$espessura[i] <- 3
                   } else{
                     df_hl$cor[i] <- "#BDBDC3"
                     df_hl$espessura[i] <- 0
                   }
                 }
                 data_map <- merge(mg_micros_shp, df_hl[,c("codmicro","cor", "espessura")], by.x = "c_micro", by.y = 'codmicro')
                 proj4string(data_map) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
                 Encoding(data_map$n_micro) <- "UTF-8"
                 
                 if(length(clicklist$ids) == 1){
                   leafletProxy("mapa_tipologia") %>% addPolylines(data = data_map,
                                                                   color = ~cor,
                                                                   opacity = 1,
                                                                   weight = ~espessura,
                                                                   layerId = ~paste(1,c_micro)
                   )
                 } else{
                   ultima_id <- paste(1,clicklist$ids[(length(clicklist$ids)-1)])
                   leafletProxy("mapa_tipologia") %>% removeShape(ultima_id)%>% addPolylines(data = data_map,
                                                                                             color = ~cor,
                                                                                             opacity = 1,
                                                                                             weight = ~espessura,
                                                                                             layerId = ~paste(1,c_micro)
                   )
                 }
                 
               })
  
  output$tipo_box <- renderUI({
    cor_funcao <- function(tipo){
      switch(tipo,
             "Alta, Adequada, Alto" = {
               cor <- "#1a9641"
             },
             "Alta, Ineficiente, Alto" = {
               cor <- "#6abc58"
             },
             "Alta, Ineficiente, Baixo" = {
               cor <- "#b3df75"
             },
             "Alta, Sobrecarga, Alto" = {
               cor <- "#e6f4a7"
             },
             "Baixa, Adequada, Alto" = {
               cor <- "#fee8a5"
             },
             "Baixa, Adequada, Baixo" = {
               cor <- "#fdba6f"
             },
             "Baixa, Ineficiente, Alto" = {
               cor <- "#ed6e43"
             },
             "Baixa, Ineficiente, Baixo" = {
               cor <- "#d7191c"
             }
      )
      estilo <- paste("border-radius: 15px; background-color:",cor)
      return(estilo)
    }
    tipo <- df_micro %>% filter(codmicro == input$mapa_tipologia_shape_click$id) %>% pull(tipologia_micro_oferta_ocup_desemp)
    microrregioes <- paste(sort(df_micro %>% filter(tipologia_micro_oferta_ocup_desemp == tipo) %>% pull(nomemicro)), collapse = ", ")
    box(width = 8, solidHeader = TRUE, style = cor_funcao(tipo),
        p(strong(tipo), style ="text-align: center;", style = "color: black;", style = "font-size:26px;"),
        p("Classificação", style ="text-align: center;", style = "color: black;", style = "font-size:18px;"),
        p(" "),
        p(strong("Microrregiões: "), microrregioes, style ="text-align: justify;", style = "color: black;", style = "font-size:18px;")
    )
  })
  
  #Abas de indicadores------------------------------------------------------
  #Geral--------------------------------------------------------------------
  
  #Geração do mapa das abas "Indicadores"
  legenda_mapa <- function(mapa, df_1, df_2, df_3){
    
    if(nrow(df_1) == 0 && nrow(df_2)==0){
      legenda_html <- "<img src= 'blue.png' height = '32' width = '24'> Rede Resposta"
      mapa_final <- mapa %>% addMarkers(df_3, lng = df_3$lon, lat = df_3$lat, icon= typeIcons[df_3$cor], label= df_3$no_fantasia)
    } else if(nrow(df_1) == 0 && nrow(df_3)==0){
      legenda_html <- "<img src= 'green.png' height = '32' width = '24'> Habilitados IAM/AVC"
      mapa_final <- mapa %>% addMarkers(df_2, lng = df_2$lon, lat = df_2$lat, icon= typeIcons[df_2$cor], label= df_2$no_fantasia)
    } else if(nrow(df_2) == 0 && nrow(df_3)==0){
      legenda_html <- "<img src= 'red.png' height = '32' width = '24'> Não Habilitados"
      mapa_final <- mapa %>% addMarkers(df_1, lng = df_1$lon, lat = df_1$lat, icon= typeIcons[df_1$cor], label= df_1$no_fantasia)
    } else if(nrow(df_1) == 0){
      legenda_html <- "<img src= 'green.png' height = '32' width = '24'> Habilitados IAM/AVC</br><img src= 'blue.png' height = '32' width = '24'> Rede Resposta"
      
      mapa_final <- mapa %>% addMarkers(df_2, lng = df_2$lon, lat = df_2$lat, icon= typeIcons[df_2$cor], label= df_2$no_fantasia) %>% addMarkers(df_3, lng = df_3$lon, lat = df_3$lat, icon= typeIcons[df_3$cor], label= df_3$no_fantasia)
      
    } else if(nrow(df_2) == 0){
      legenda_html <- "<img src= 'blue.png' height = '32' width = '24'> Rede Resposta</br><img src= 'red.png' height = '32' width = '24'> Não Habilitados"
      
      mapa_final <- mapa %>% addMarkers(df_1, lng = df_1$lon, lat = df_1$lat, icon= typeIcons[df_1$cor], label= df_1$no_fantasia) %>% addMarkers(df_3, lng = df_3$lon, lat = df_3$lat, icon= typeIcons[df_3$cor], label= df_3$no_fantasia)
    } else if(nrow(df_3) == 0){
      legenda_html <- "<img src= 'green.png' height = '32' width = '24'> Habilitados IAM/AVC</br><img src= 'red.png' height = '32' width = '24'> Não Habilitados"
      
      mapa_final <- mapa %>% addMarkers(df_1, lng = df_1$lon, lat = df_1$lat, icon= typeIcons[df_1$cor], label= df_1$no_fantasia) %>% addMarkers(df_2, lng = df_2$lon, lat = df_2$lat, icon= typeIcons[df_2$cor], label= df_2$no_fantasia)
    } else if(nrow(df_1) != 0 && nrow(df_2) !=0 && nrow(df_3) !=0){
      legenda_html <- "<img src= 'green.png' height = '32' width = '24'> Habilitados IAM/AVC</br><img src='blue.png' height = '32' width = '24'> Rede Resposta</br><img src= 'red.png' height = '32' width = '24'> Não Habilitados"
      
      mapa_final <- mapa %>% addMarkers(df_1, lng = df_1$lon, lat = df_1$lat, icon= typeIcons[df_1$cor], label= df_1$no_fantasia) %>% 
        addMarkers(df_2, lng = df_2$lon, lat = df_2$lat, icon= typeIcons[df_2$cor], label= df_2$no_fantasia) %>%
        addMarkers(df_3, lng = df_3$lon, lat = df_3$lat, icon= typeIcons[df_3$cor], label= df_3$no_fantasia)
    } else if(nrow(df_1) == 0 && nrow(df_2) ==0 && nrow(df_3) ==0){
      legenda_html = NULL
      
      mapa_final <- mapa
    }
    
    dados_mapa <- list(mapa = mapa_final, legenda = legenda_html)
    
    return(dados_mapa)
  }
  
  mapas_indicadores <- function(df_coordenadas, coluna_df_coordenadas, input, coluna_df_estabelecimentos, zoom_entrada, data_shp_1, data_shp_2, coluna_label, input_comparacao, input_unidades_comparacao){
    if(input_comparacao == 0){
      longitude <-  reactive(df_coordenadas %>% filter(!!sym(coluna_df_coordenadas) == input) %>% pull(X1))
      latitude <- reactive(df_coordenadas %>% filter(!!sym(coluna_df_coordenadas) == input) %>% pull(X2))
      
      df_mapa <- df_estabelecimentos_final %>% filter(!!sym(coluna_df_estabelecimentos) == input)
      
      variavel_plot <- leaflet(options = leafletOptions(minZoom = zoom_entrada,
                                                        maxZoom = zoom_entrada)) %>%
        setView(lng = longitude(), lat = latitude(), zoom = zoom_entrada) %>%
        setMaxBounds( lng1 = 0.99*longitude()
                      , lat1 = 0.99*latitude()
                      , lng2 = 1.01*longitude()
                      , lat2 = 1.01*latitude() ) %>%
        addTiles()
    } else{
      df_mapa <- df_estabelecimentos_final %>% filter(!!sym(coluna_df_estabelecimentos) %in% c(input, input_unidades_comparacao))
      
      variavel_plot <- leaflet() %>% addTiles()
    }
    
    df_1 <- df_mapa %>% filter(Tipologia == 'Não Habilitados')  
    df_2 <- df_mapa %>% filter(Tipologia == 'Habilitados IAM/AVC')
    df_3 <- df_mapa %>% filter(Tipologia == 'Rede Resposta')
    
    variavel_plot <- variavel_plot %>% addPolylines(data = data_shp_1,
                                                    color = "#000000",
                                                    opacity = 1,
                                                    weight = 5) %>%
      addPolygons(data = data_shp_2,
                  fillColor = "#FEE8A5",
                  fillOpacity = 0.4,
                  stroke = TRUE,
                  smoothFactor = 0.5,
                  color = "#000000",
                  opacity = 1,
                  weight = 2,
                  label = coluna_label)
    
    dados = legenda_mapa(variavel_plot, df_1, df_2, df_3)
    variavel_plot = dados$mapa
    
    variavel_plot <- variavel_plot %>%
      addControl(html = dados$legenda, position = "bottomright")
    
    return(variavel_plot)
  }
  
  #Cards dos indicadores de oferta
  texto_card <- function(df_regiao, coluna_df_regiao, coluna_df_estabelecimentos, input, tipo_hospitais, tipologia, coluna_taxa){
    hospitais <- df_regiao %>% filter(!!sym(coluna_df_regiao) == input) %>% pull(!!sym(tipo_hospitais))
    leitos <- sum(df_estabelecimentos_final %>% filter(!!sym(coluna_df_estabelecimentos) == input & Tipologia == tipologia) %>% pull(leitos_SUS))
    taxa <- format(round(df_regiao %>% filter(!!sym(coluna_df_regiao) == input) %>% pull(!!sym(coluna_taxa)),2), nsmall = 2, decimal.mark = ",")
    
    texto_html <- ifelse(hospitais == 0,
                         "<ul><li> Nenhum hospital na região! </li></ul>",
                         paste("<ul><li><b>",hospitais,"</b>",ifelse(hospitais == 1, "hospital</li>","hospitais</li>"),
                               "<li><b>",leitos,"</b>",ifelse(leitos > 1, "leitos</li>","leito</li>"),
                               "<li><b>",taxa,"</b>leitos por 10.000 habitantes acima de 45 anos</li></ul>")
                         
    )
    return(texto_html)
  }
  
  #Gráfico dos indicadores de demanda - Tipo de estabelecimento
  grafico_tipo_estab_pe <- function(df, coluna_df, input,input_enfermidade){
    switch(input_enfermidade,
           "Todas as enfermidades" = {
             df_tipo <- df %>% filter(!!sym(coluna_df) == input) %>% select (!!sym(coluna_df),
                                                                             n_solic_central_AVC, n_solic_central_IAM,
                                                                             n_solic_clinica_AVC, n_solic_clinica_IAM,
                                                                             n_solic_hospital_AVC, n_solic_hospital_IAM,
                                                                             n_solic_pa_AVC, n_solic_pa_IAM,
                                                                             n_solic_ubs_AVC, n_solic_ubs_IAM,
                                                                             n_solic_unimista_AVC,n_solic_unimista_IAM) %>% mutate("Central" = n_solic_central_AVC + n_solic_central_IAM,
                                                                                                                                   "Clínica" = n_solic_clinica_AVC + n_solic_clinica_IAM,
                                                                                                                                   "Hospital" = n_solic_hospital_AVC + n_solic_hospital_IAM,
                                                                                                                                   "Pronto Atendimento" = n_solic_pa_AVC + n_solic_pa_IAM,
                                                                                                                                   "UBS" = n_solic_ubs_AVC + n_solic_ubs_IAM,
                                                                                                                                   "Unidade Mista" = n_solic_unimista_AVC + n_solic_unimista_IAM,
                                                                                                                                   n_solic_central_AVC = NULL, 
                                                                                                                                   n_solic_central_IAM = NULL,
                                                                                                                                   n_solic_clinica_AVC = NULL, 
                                                                                                                                   n_solic_clinica_IAM = NULL,
                                                                                                                                   n_solic_hospital_AVC = NULL, 
                                                                                                                                   n_solic_hospital_IAM = NULL,
                                                                                                                                   n_solic_pa_AVC = NULL,
                                                                                                                                   n_solic_pa_IAM = NULL,
                                                                                                                                   n_solic_ubs_AVC = NULL, 
                                                                                                                                   n_solic_ubs_IAM = NULL,
                                                                                                                                   n_solic_unimista_AVC = NULL,
                                                                                                                                   n_solic_unimista_IAM = NULL) %>% pivot_longer(cols = c(-!!sym(coluna_df)),names_to = "categoria") %>% select(-c(!!sym(coluna_df)))
             
           },
           "IAM" = {
             df_tipo <- df %>% filter(!!sym(coluna_df) == input) %>% select (!!sym(coluna_df),
                                                                             n_solic_central_IAM,
                                                                             n_solic_clinica_IAM,
                                                                             n_solic_hospital_IAM,
                                                                             n_solic_pa_IAM,
                                                                             n_solic_ubs_IAM,
                                                                             n_solic_unimista_IAM) %>% mutate("Central" = n_solic_central_IAM,
                                                                                                              "Clínica" = n_solic_clinica_IAM,
                                                                                                              "Hospital" = n_solic_hospital_IAM,
                                                                                                              "Pronto Atendimento" = n_solic_pa_IAM,
                                                                                                              "UBS" = n_solic_ubs_IAM,
                                                                                                              "Unidade Mista" = n_solic_unimista_IAM,
                                                                                                              n_solic_central_IAM = NULL,
                                                                                                              n_solic_clinica_IAM = NULL,
                                                                                                              n_solic_hospital_IAM = NULL,
                                                                                                              n_solic_pa_IAM = NULL,
                                                                                                              n_solic_ubs_IAM = NULL,
                                                                                                              n_solic_unimista_IAM = NULL) %>% pivot_longer(cols = c(-!!sym(coluna_df)),names_to = "categoria") %>% select(-c(!!sym(coluna_df)))
           },
           "AVC" = {
             df_tipo <- df %>% filter(!!sym(coluna_df) == input) %>% select (!!sym(coluna_df),
                                                                             n_solic_central_AVC,
                                                                             n_solic_clinica_AVC,
                                                                             n_solic_hospital_AVC,
                                                                             n_solic_pa_AVC,
                                                                             n_solic_ubs_AVC,
                                                                             n_solic_unimista_AVC) %>% mutate("Central" = n_solic_central_AVC,
                                                                                                              "Clínica" = n_solic_clinica_AVC,
                                                                                                              "Hospital" = n_solic_hospital_AVC,
                                                                                                              "Pronto Atendimento" = n_solic_pa_AVC,
                                                                                                              "UBS" = n_solic_ubs_AVC,
                                                                                                              "Unidade Mista" = n_solic_unimista_AVC,
                                                                                                              n_solic_central_AVC = NULL,
                                                                                                              n_solic_clinica_AVC = NULL,
                                                                                                              n_solic_hospital_AVC = NULL,
                                                                                                              n_solic_pa_AVC = NULL,
                                                                                                              n_solic_ubs_AVC = NULL,
                                                                                                              n_solic_unimista_AVC = NULL) %>% pivot_longer(cols = c(-!!sym(coluna_df)),names_to = "categoria") %>% select(-c(!!sym(coluna_df)))
           }
    )
    
    
    grafico <-df_tipo %>% plot_ly(labels = ~categoria, values = ~value,
                                  textinfo = 'percent',
                                  hoverinfo = 'text',
                                  text = ~paste(value,ifelse(value>1,'solicitações','solicitação')),
                                  marker = list(colors = cores,
                                                line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.8) %>%
      layout(showlegend = TRUE,
             legend = list(orientation = 'h'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    return(grafico)
  }
  
  #Gráfico dos indicadores de demanda - Habilitacao de estabelecimento
  grafico_hab_estab_pe <- function(df, coluna_df, input,input_enfermidade){
    switch(input_enfermidade,
           "Todas as enfermidades" = {
             df_tipo <- df %>% filter(!!sym(coluna_df) == input) %>% select (!!sym(coluna_df),
                                                                             n_solic_hab_AVC, n_solic_hab_IAM,
                                                                             n_solic_rr_AVC, n_solic_rr_IAM,
                                                                             n_solic_nhab_AVC, n_solic_nhab_IAM,
                                                                             n_solic_outros_AVC, n_solic_outros_IAM) %>% mutate("Habilitados IAM/AVC" = n_solic_hab_AVC + n_solic_hab_IAM,
                                                                                                                                "Rede Resposta" = n_solic_rr_AVC + n_solic_rr_IAM,
                                                                                                                                "Não Habilitados" = n_solic_nhab_AVC + n_solic_nhab_IAM,
                                                                                                                                "Outros" = n_solic_outros_AVC + n_solic_outros_IAM,
                                                                                                                                n_solic_hab_AVC = NULL, 
                                                                                                                                n_solic_hab_IAM = NULL,
                                                                                                                                n_solic_rr_AVC = NULL, 
                                                                                                                                n_solic_rr_IAM = NULL,
                                                                                                                                n_solic_nhab_AVC = NULL,
                                                                                                                                n_solic_nhab_IAM = NULL,
                                                                                                                                n_solic_outros_AVC = NULL,
                                                                                                                                n_solic_outros_IAM = NULL) %>% pivot_longer(cols = c(-!!sym(coluna_df)),names_to = "categoria") %>% select(-c(!!sym(coluna_df)))
             
           },
           "IAM" = {
             df_tipo <- df %>% filter(!!sym(coluna_df) == input) %>% select (!!sym(coluna_df),
                                                                             n_solic_hab_IAM,
                                                                             n_solic_rr_IAM,
                                                                             n_solic_nhab_IAM,
                                                                             n_solic_outros_IAM) %>% mutate("Habilitados IAM/AVC" = n_solic_hab_IAM,
                                                                                                            "Rede Resposta" = n_solic_rr_IAM,
                                                                                                            "Não Habilitados" = n_solic_nhab_IAM,
                                                                                                            "Outros" = n_solic_outros_IAM,
                                                                                                            n_solic_hab_IAM = NULL,
                                                                                                            n_solic_rr_IAM = NULL,
                                                                                                            n_solic_nhab_IAM = NULL, 
                                                                                                            n_solic_outros_IAM = NULL) %>% pivot_longer(cols = c(-!!sym(coluna_df)),names_to = "categoria") %>% select(-c(!!sym(coluna_df)))
           },
           "AVC" = {
             df_tipo <- df %>% filter(!!sym(coluna_df) == input) %>% select (!!sym(coluna_df),
                                                                             n_solic_hab_AVC,
                                                                             n_solic_rr_AVC,
                                                                             n_solic_nhab_AVC,
                                                                             n_solic_outros_AVC) %>% mutate("Habilitados IAM/AVC" = n_solic_hab_AVC,
                                                                                                            "Rede Resposta" = n_solic_rr_AVC,
                                                                                                            "Não Habilitados" = n_solic_nhab_AVC,
                                                                                                            "Outros" = n_solic_outros_AVC,
                                                                                                            n_solic_hab_AVC = NULL,
                                                                                                            n_solic_rr_AVC = NULL,
                                                                                                            n_solic_nhab_AVC = NULL, 
                                                                                                            n_solic_outros_AVC = NULL) %>% pivot_longer(cols = c(-!!sym(coluna_df)),names_to = "categoria") %>% select(-c(!!sym(coluna_df)))
           }
    )
    
    
    grafico <-df_tipo %>% plot_ly(labels = ~categoria, values = ~value,
                                  textinfo = 'percent',
                                  hoverinfo = 'text',
                                  text = ~paste(value,ifelse(value>1,'solicitações','solicitação')),
                                  marker = list(colors = cores,
                                                line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.8) %>%
      layout(showlegend = TRUE,
             legend = list(orientation = 'h'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    return(grafico)
  }
  
  #Gráfico - ocupação
  grafico_ocupacao <- function(df, coluna_df, input){
    df_ocup <- df %>% filter(!!sym(coluna_df) == input) %>% select(!!sym(coluna_df), ocup_hab, ocup_rr, ocup_naohab) %>% mutate("Habilitados IAM/AVC" = ocup_hab,
                                                                                                                                "Rede Resposta" = ocup_rr,
                                                                                                                                "Não Habilitados" = ocup_naohab,
                                                                                                                                ocup_hab = NULL,
                                                                                                                                ocup_rr = NULL,
                                                                                                                                ocup_naohab = NULL) %>% pivot_longer(cols = c(-!!sym(coluna_df)),names_to = "tipo") %>% select(-c(!!sym(coluna_df)))
    
    df_ocup[is.na(df_ocup)] <- 0
    grafico <- plot_ly(df_ocup) %>%
      add_bars(x= ~value, y = ~as.factor(tipo), type = 'bar', orientation = 'h',
               width = c(0.4,0.4,0.4),
               marker = list(color = c('#1F3F49','#23282D','#6AB187')),
               name = "Ocupação",
               hovertemplate = ~paste(format(round(value,2), nsmall = 2, decimal.mark = ","),"%")) %>%
      layout(xaxis = list(title = "Taxa de ocupação (%)"),
             yaxis = list(title = "",
                          categoryorder = 'array',
                          categoryarray = c('Não Habilitados', 'Rede Resposta', 'Habilitados IAM/AVC')))                                                                                                                       
    
    
    return(grafico)
  }
  
  #Gráfico - tempo adqueado
  grafico_tempo_adeq <- function(df, coluna_df, input, col_adeq, col_adeq_diff){
    df_tempo <- df %>% filter(!!sym(coluna_df) == input) %>% select(!!sym(coluna_df),
                                                                    !!sym(col_adeq),
                                                                    !!sym(col_adeq_diff)) %>% pivot_longer(cols = c(-!!sym(coluna_df)),names_to = "tipo") %>% select(-c(!!sym(coluna_df)))
    
    
    df_tempo['tipo'] <-c("Tempo adequado", "Com transferência")
    
    df_tempo[is.na(df_tempo)] <- 0
    grafico <- plot_ly(df_tempo) %>%
      add_bars(x= ~value*100, y = ~as.factor(tipo), type = 'bar', orientation = 'h',
               width = c(0.4,0.4),
               marker = list(color = c('#1F3F49','#6AB187')),
               name = "Atendimentos",
               hovertemplate = ~paste(format(round(value*100,2), nsmall = 2, decimal.mark = ","),"%")) %>%
      layout(xaxis = list(title = "Taxa de atendimentos (%)"),
             yaxis = list(title = "",
                          range = list(-0.5,1.5),
                          categoryorder = 'array',
                          categoryarray = c("Com transferência","Tempo adequado")))                                                
    
    
    return(grafico)
  }
  
  #Gráfico de número de transferências - IAM/AVC
  grafico_transferencia <- function(df, regiao, input, hab, rr, naohab){
    df_transf <- df %>% filter(!!sym(regiao) == input) %>% select(!!sym(regiao),
                                                                  !!sym(hab),
                                                                  !!sym(rr),
                                                                  !!sym(naohab)) %>% pivot_longer(cols = c(-!!sym(regiao)),names_to = "tipo") %>% select(-c(!!sym(regiao)))
    
    df_transf['tipo'] <- c("Habilitados IAM/AVC", "Rede Resposta", "Não Habilitados")
    df_transf[is.na(df_transf)] <- 0
    grafico <-df_transf %>% plot_ly(labels = ~tipo, values = ~value,
                                    textinfo = 'percent',
                                    hoverinfo = 'text',
                                    text = ~paste(value,ifelse(value>1,'transferências','transferência')),
                                    marker = list(colors = cores,
                                                  line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.8) %>%
      layout(showlegend = TRUE,
             legend = list(orientation = 'h'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(grafico)
  }
  
  #Gráfico de taxa de transferências - IAM/AVC
  grafico_tx_transf <- function(df, regiao, input, tx_hab, tx_rr, tx_nhab, tx_outros){
    df_tx <- df %>% filter(!!sym(regiao) == input) %>% select(!!sym(regiao),
                                                              !!sym(tx_hab),
                                                              !!sym(tx_rr),
                                                              !!sym(tx_nhab),
                                                              !!sym(tx_outros)) %>% pivot_longer(cols = c(-!!sym(regiao)),names_to = "tipo") %>% select(-c(!!sym(regiao)))
    
    df_tx['tipo'] <- c("Habilitados IAM/AVC", "Rede Resposta", "Não Habilitados", "Outros")
    df_tx[is.na(df_tx)] <- 0
    grafico <- plot_ly(df_tx) %>%
      add_bars(x= ~value*100, y = ~as.factor(tipo), type = 'bar', orientation = 'h',
               width = c(0.4,0.4,0.4,0.4),
               marker = list(color = c('#1F3F49','#23282D','#6AB187','#CED2CC')),
               name = "Transferências",
               hovertemplate = ~paste(format(round(value*100,2), nsmall = 2, decimal.mark = ","),"%")) %>%
      layout(#bargap = 0.1,
        xaxis = list(title = "Taxa de transferência (%)"),
        yaxis = list(title = "",
                     categoryorder = 'array',
                     categoryarray = c('Outros','Não Habilitados', 'Rede Resposta', 'Habilitados IAM/AVC')))                                                                                                                       
    
    
    
    return(grafico)
  }
  
  #Gráfico de distâncias percorridas----------------------------------------
  criacao_vetor <- function(distancia){
    vetor_distancia <- vector()
    
    if(is.na(distancia)){
      distancia <- 0
    }
    
    for(i in 1:ceiling(distancia/10)){
      if(i < ceiling(distancia/10)){
        vetor_distancia[i] <- 10
      } else{
        vetor_distancia[i] <- (distancia/10)%%1
      }
    }
    return(vetor_distancia)
  }
  
  grafico_distancia <- function(df, regiao, input, residencia_pe, pe_destino, regiao_diferente_residencia_pe, regiao_diferente_pe_destino, frase){
    y <- "Distância"
    
    vetor_distancia <- criacao_vetor(df %>% filter(!!sym(regiao) == input) %>% pull(!!sym(residencia_pe)))
    vetor_distancia2 <- criacao_vetor(df %>% filter(!!sym(regiao) == input) %>% pull(!!sym(pe_destino)))
    vetor_distancia3 <- criacao_vetor(df %>% filter(!!sym(regiao) == input) %>% pull(!!sym(regiao_diferente_residencia_pe)))
    vetor_distancia4 <- criacao_vetor(df %>% filter(!!sym(regiao) == input) %>% pull(!!sym(regiao_diferente_pe_destino)))
    
    data <- data.frame(y, vetor_distancia)
    data2 <- data.frame(y, vetor_distancia2)
    data3 <- data.frame(y, vetor_distancia3)
    data4 <- data.frame(y, vetor_distancia4)
    
    grafico <- plot_ly(data4, x = ~vetor_distancia4, y = 0.25, type = 'bar', orientation = 'h', name = "Distância",
                       hovertemplate = ~paste(format(round(sum(vetor_distancia4),2),nsmall = 2, decimal.mark = ","),"km"),
                       marker = list(color = '#D32D41',
                                     line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
      add_trace(data3, x = ~vetor_distancia3, y = 0.5, name = "Distância", hovertemplate = ~paste(format(round(sum(vetor_distancia3),2),nsmall = 2, decimal.mark = ","),"km"), marker = list(color = '#6AB187')) %>%
      add_trace(data2, x = ~vetor_distancia2, y = 0.75, name = "Distância", hovertemplate = ~paste(format(round(sum(vetor_distancia2),2),nsmall = 2, decimal.mark = ","),"km"), marker = list(color = '#D32D41')) %>%
      add_trace(data, x = ~vetor_distancia, y = 1, name = "Distância", hovertemplate = ~paste(format(round(sum(vetor_distancia),2),nsmall = 2, decimal.mark = ","),"km"), marker = list(color = '#6AB187')) %>%
      add_trace(data4, x= ~sum(vetor_distancia4), y=0.25,type='scatter', mode='markers', name = "Distância", hovertemplate = ~paste(format(round(sum(vetor_distancia4),2),nsmall = 2, decimal.mark = ","),"km"), marker=list(size=20, symbol = 'x', color = '#D32D41')) %>%
      add_trace(data3, x= ~sum(vetor_distancia3), y=0.5,type='scatter', mode='markers', name = "Distância", hovertemplate = ~paste(format(round(sum(vetor_distancia3),2),nsmall = 2, decimal.mark = ","),"km"), marker=list(size=20, symbol = 'x', color = '#6AB187')) %>%
      add_trace(data2, x= ~sum(vetor_distancia2), y=0.75,type='scatter', mode='markers', name = "Distância", hovertemplate = ~paste(format(round(sum(vetor_distancia2),2),nsmall = 2, decimal.mark = ","),"km"), marker=list(size=20, symbol = 'x', color = '#D32D41')) %>%
      add_trace(data, x= ~sum(vetor_distancia), y=1,type='scatter', mode='markers', name = "Distância", hovertemplate = ~paste(format(round(sum(vetor_distancia),2),nsmall = 2, decimal.mark = ","),"km"), marker=list(size=20, symbol = 'x', color = '#6AB187')) %>%
      add_trace(data4, x= ~(sum(vetor_distancia4)+1), y=0.28,type='scatter', mode='text', text = ~paste(format(round(sum(vetor_distancia4),2),nsmall = 2, decimal.mark = ","),"km"), textposition = 'top center', textfont=list(color = '#000000', size = 16), marker = list(color = 'red', opacity = 0)) %>%
      add_trace(data3, x= ~(sum(vetor_distancia3)+1), y=0.53,type='scatter', mode='text', text = ~paste(format(round(sum(vetor_distancia3),2),nsmall = 2, decimal.mark = ","),"km"), textposition = 'top center', textfont=list(color = '#000000', size = 16), marker = list(color = 'red', opacity = 0)) %>%
      add_trace(data2, x= ~(sum(vetor_distancia2)+1), y=0.78,type='scatter', mode='text', text = ~paste(format(round(sum(vetor_distancia2),2),nsmall = 2, decimal.mark = ","),"km"), textposition = 'top center', textfont=list(color = '#000000', size = 16), marker = list(color = 'red', opacity = 0)) %>%
      add_trace(data, x= ~(sum(vetor_distancia)+1), y=1.03,type='scatter', mode='text', text = ~paste(format(round(sum(vetor_distancia),2),nsmall = 2, decimal.mark = ","),"km"), textposition = 'top center', textfont=list(color = '#000000', size = 16), marker = list(color = 'red', opacity = 0)) %>%
      layout(bargap = 0.9,
             xaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE),
             barmode = 'stack',
             showlegend = FALSE) %>%
      add_annotations(text = "Residência/", x = -0.005, xref = 'paper', y = 0.95, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = "Porta de entrada", x = -0.005, xref = 'paper', y = 0.88, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = "Porta de entrada/", x = -0.005, xref = 'paper', y = 0.65, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = "Destino", x = -0.005, xref = 'paper', y = 0.58, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = "Residência/", x = -0.005, xref = 'paper', y = 0.45, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = "Porta de entrada", x = -0.005, xref = 'paper', y = 0.38, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = frase, x = -0.005, xref = 'paper', y = 0.27, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = "Porta de entrada/", x = -0.005, xref = 'paper', y = 0.15, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = "Destino", x = -0.005, xref = 'paper', y = 0.08, yref = 'paper', showarrow = FALSE) %>%
      add_annotations(text = frase, x = -0.005, xref = 'paper', y = 0.01, yref = 'paper', showarrow = FALSE)
    return(grafico)
  }
  
  
  #Funções de geração das tabelas de comparação
  funcao_numeros_taxa <- function(x){
    numero <- format(round(x,2), nsmall = 2, decimal.mark = ",")
    
    return(numero)
  }
  
  funcao_numeros_absolutos <- function(x){
    numero <- format(round(x,0), nsmall = 0, big.mark = ".", decimal.mark = ",")
    
    return(numero)
  }
  
  
  comparacao_oferta_tabela <- function(df, regiao, input){
    df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                  n_habilitados,
                                                                  n_rr,
                                                                  n_naohab,
                                                                  taxa_leitos_sus_hab,
                                                                  taxa_leitos_sus_rr,
                                                                  taxa_leitos_sus_naohab,
                                                                  tx_leito_nsus,
                                                                  pop_tot,
                                                                  pop_45mais)
    df_comp[is.na(df_comp)] <- 0
    
    df_comp[,c("taxa_leitos_sus_hab", "taxa_leitos_sus_rr", "taxa_leitos_sus_naohab", "tx_leito_nsus")] <- lapply(df_comp[,c("taxa_leitos_sus_hab", "taxa_leitos_sus_rr", "taxa_leitos_sus_naohab", "tx_leito_nsus")], funcao_numeros_taxa)
    
    df_comp[,c("pop_tot", "pop_45mais")] <- lapply(df_comp[,c("pop_tot", "pop_45mais")], funcao_numeros_absolutos)
    
    if(regiao == "nomemacro"){
      nome_col <- "Macrorregião"
    } else{
      nome_col <- "Microrregião"
    }
    
    colnames(df_comp) <- c(nome_col, "Hospitais - Habilitados IAM/AVC", "Hospitais - Rede Resposta", 'Hospitais - Não Habilitados', "Leitos/10.000 hab. > 45 anos (Habilitados IAM/AVC)", "Leitos/10.000 hab. > 45 anos (Rede Resposta)", "Leitos/10.000 hab. > 45 anos (Não Habilitados)", "Leitos/10.000 hab. > 45 anos (Privados)", "População total", "População acima de 45 anos")
    
    return(df_comp)
  }
  
  comparacao_demanda_tipo_tabela <- function(df, regiao, enfermidade, input){
    switch(enfermidade,
           "Todas as enfermidades" = {
             df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select (!!sym(regiao),
                                                                            n_solic_central_AVC, n_solic_central_IAM,
                                                                            n_solic_clinica_AVC, n_solic_clinica_IAM,
                                                                            n_solic_hospital_AVC, n_solic_hospital_IAM,
                                                                            n_solic_pa_AVC, n_solic_pa_IAM,
                                                                            n_solic_ubs_AVC, n_solic_ubs_IAM,
                                                                            n_solic_unimista_AVC,n_solic_unimista_IAM) %>% mutate("Central" = n_solic_central_AVC + n_solic_central_IAM,
                                                                                                                                  "Clínica" = n_solic_clinica_AVC + n_solic_clinica_IAM,
                                                                                                                                  "Hospital" = n_solic_hospital_AVC + n_solic_hospital_IAM,
                                                                                                                                  "Pronto Atendimento" = n_solic_pa_AVC + n_solic_pa_IAM,
                                                                                                                                  "UBS" = n_solic_ubs_AVC + n_solic_ubs_IAM,
                                                                                                                                  "Unidade Mista" = n_solic_unimista_AVC + n_solic_unimista_IAM,
                                                                                                                                  n_solic_central_AVC = NULL, 
                                                                                                                                  n_solic_central_IAM = NULL,
                                                                                                                                  n_solic_clinica_AVC = NULL, 
                                                                                                                                  n_solic_clinica_IAM = NULL,
                                                                                                                                  n_solic_hospital_AVC = NULL, 
                                                                                                                                  n_solic_hospital_IAM = NULL,
                                                                                                                                  n_solic_pa_AVC = NULL,
                                                                                                                                  n_solic_pa_IAM = NULL,
                                                                                                                                  n_solic_ubs_AVC = NULL, 
                                                                                                                                  n_solic_ubs_IAM = NULL,
                                                                                                                                  n_solic_unimista_AVC = NULL,
                                                                                                                                  n_solic_unimista_IAM = NULL)
           },
           "IAM" = {
             df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select (!!sym(regiao),
                                                                            n_solic_central_IAM,
                                                                            n_solic_clinica_IAM,
                                                                            n_solic_hospital_IAM,
                                                                            n_solic_pa_IAM,
                                                                            n_solic_ubs_IAM,
                                                                            n_solic_unimista_IAM) %>% mutate("Central" = n_solic_central_IAM,
                                                                                                             "Clínica" = n_solic_clinica_IAM,
                                                                                                             "Hospital" = n_solic_hospital_IAM,
                                                                                                             "Pronto Atendimento" = n_solic_pa_IAM,
                                                                                                             "UBS" = n_solic_ubs_IAM,
                                                                                                             "Unidade Mista" = n_solic_unimista_IAM,
                                                                                                             n_solic_central_IAM = NULL,
                                                                                                             n_solic_clinica_IAM = NULL,
                                                                                                             n_solic_hospital_IAM = NULL,
                                                                                                             n_solic_pa_IAM = NULL,
                                                                                                             n_solic_ubs_IAM = NULL,
                                                                                                             n_solic_unimista_IAM = NULL)
             
           },
           "AVC" = {
             df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select (!!sym(regiao),
                                                                            n_solic_central_AVC,
                                                                            n_solic_clinica_AVC,
                                                                            n_solic_hospital_AVC,
                                                                            n_solic_pa_AVC,
                                                                            n_solic_ubs_AVC,
                                                                            n_solic_unimista_AVC) %>% mutate("Central" = n_solic_central_AVC,
                                                                                                             "Clínica" = n_solic_clinica_AVC,
                                                                                                             "Hospital" = n_solic_hospital_AVC,
                                                                                                             "Pronto Atendimento" = n_solic_pa_AVC,
                                                                                                             "UBS" = n_solic_ubs_AVC,
                                                                                                             "Unidade Mista" = n_solic_unimista_AVC,
                                                                                                             n_solic_central_AVC = NULL,
                                                                                                             n_solic_clinica_AVC = NULL,
                                                                                                             n_solic_hospital_AVC = NULL,
                                                                                                             n_solic_pa_AVC = NULL,
                                                                                                             n_solic_ubs_AVC = NULL,
                                                                                                             n_solic_unimista_AVC = NULL)
           }
           
    )
    
    if(regiao == "nomemacro"){
      names(df_comp)[names(df_comp) == "nomemacro"] <- "Macrorregião"
    } else{
      names(df_comp)[names(df_comp) == "nomemicro"] <- "Microrregião"
    }
    
    df_comp[is.na(df_comp)] <- 0
    df_comp[,c("Central", "Clínica", "Hospital", "Pronto Atendimento", "UBS", "Unidade Mista")] <- lapply(df_comp[,c("Central", "Clínica", "Hospital", "Pronto Atendimento", "UBS", "Unidade Mista")], funcao_numeros_absolutos)
    
    return(df_comp)
  }
  
  comparacao_demanda_hab_tabela <- function(df, regiao, enfermidade, input){
    switch(enfermidade,
           "Todas as enfermidades" = {
             df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select (!!sym(regiao),
                                                                            n_solic_hab_AVC, n_solic_hab_IAM,
                                                                            n_solic_rr_AVC, n_solic_rr_IAM,
                                                                            n_solic_nhab_AVC, n_solic_nhab_IAM,
                                                                            n_solic_outros_AVC, n_solic_outros_IAM) %>% mutate("Habilitados IAM/AVC" = n_solic_hab_AVC + n_solic_hab_IAM,
                                                                                                                               "Rede Resposta" = n_solic_rr_AVC + n_solic_rr_IAM,
                                                                                                                               "Não Habilitados" = n_solic_nhab_AVC + n_solic_nhab_IAM,
                                                                                                                               "Outros" = n_solic_outros_AVC + n_solic_outros_IAM,
                                                                                                                               n_solic_hab_AVC = NULL, 
                                                                                                                               n_solic_hab_IAM = NULL,
                                                                                                                               n_solic_rr_AVC = NULL, 
                                                                                                                               n_solic_rr_IAM = NULL,
                                                                                                                               n_solic_nhab_AVC = NULL,
                                                                                                                               n_solic_nhab_IAM = NULL,
                                                                                                                               n_solic_outros_AVC = NULL,
                                                                                                                               n_solic_outros_IAM = NULL)
             
           },
           "IAM" = {
             df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select (!!sym(regiao),
                                                                            n_solic_hab_IAM,
                                                                            n_solic_rr_IAM,
                                                                            n_solic_nhab_IAM,
                                                                            n_solic_outros_IAM) %>% mutate("Habilitados IAM/AVC" = n_solic_hab_IAM,
                                                                                                           "Rede Resposta" = n_solic_rr_IAM,
                                                                                                           "Não Habilitados" = n_solic_nhab_IAM,
                                                                                                           "Outros" = n_solic_outros_IAM,
                                                                                                           n_solic_hab_IAM = NULL,
                                                                                                           n_solic_rr_IAM = NULL,
                                                                                                           n_solic_nhab_IAM = NULL, 
                                                                                                           n_solic_outros_IAM = NULL)
           },
           "AVC" = {
             df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select (!!sym(regiao),
                                                                            n_solic_hab_AVC,
                                                                            n_solic_rr_AVC,
                                                                            n_solic_nhab_AVC,
                                                                            n_solic_outros_AVC) %>% mutate("Habilitados IAM/AVC" = n_solic_hab_AVC,
                                                                                                           "Rede Resposta" = n_solic_rr_AVC,
                                                                                                           "Não Habilitados" = n_solic_nhab_AVC,
                                                                                                           "Outros" = n_solic_outros_AVC,
                                                                                                           n_solic_hab_AVC = NULL,
                                                                                                           n_solic_rr_AVC = NULL,
                                                                                                           n_solic_nhab_AVC = NULL, 
                                                                                                           n_solic_outros_AVC = NULL)
           }
    )
    
    
    if(regiao == "nomemacro"){
      names(df_comp)[names(df_comp) == "nomemacro"] <- "Macrorregião"
    } else{
      names(df_comp)[names(df_comp) == "nomemicro"] <- "Microrregião"
    }
    
    df_comp[is.na(df_comp)] <- 0
    df_comp[,c("Habilitados IAM/AVC", "Rede Resposta", "Não Habilitados", "Outros")] <- lapply(df_comp[,c("Habilitados IAM/AVC", "Rede Resposta", "Não Habilitados", "Outros")], funcao_numeros_absolutos)
    return(df_comp)
  }
  
  comparacao_ocupacao_tabela <- function(df, regiao, input){
    df_comp <- df %>% filter(!!sym(regiao) %in% input) %>%select(!!sym(regiao),
                                                                 ocup_hab,
                                                                 ocup_rr,
                                                                 ocup_naohab,
                                                                 ocup_tot)
    
    df_comp[is.na(df_comp)] <- 0
    df_comp[,c("ocup_hab", "ocup_rr","ocup_naohab", "ocup_tot")] <- lapply(df_comp[,c("ocup_hab", "ocup_rr","ocup_naohab", "ocup_tot")], funcao_numeros_taxa)
    
    if(regiao == "nomemacro"){
      nome_col <- "Macrorregião"
    } else{
      nome_col <- "Microrregião"
    }
    
    colnames(df_comp) <- c(nome_col, "Habilitados IAM/AVC (%)", "Rede Resposta (%)", 'Não Habilitados (%)', "Total (%)")
    
    return(df_comp)
  }
  
  comparacao_desempenho_taxas_tabela <- function(df, regiao, enfermidade, input){
    if(regiao == "nomemacro"){
      if(enfermidade == "IAM"){
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      tx_tempo_adeq_alta_IAM,
                                                                      tx_tempo_adeq_alta_macrodif_IAM,
                                                                      tx_evasao_alta_IAM,
                                                                      tx_transf_estab_alta_hab_IAM,
                                                                      tx_transf_estab_alta_rr_IAM,
                                                                      tx_transf_estab_alta_nhab_IAM,
                                                                      tx_transf_estab_alta_outros_IAM,
                                                                      tx_transf_estab_alta_total_IAM)
      }else{
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      tx_tempo_adeq_alta_AVC,
                                                                      tx_tempo_adeq_alta_macrodif_AVC,
                                                                      tx_evasao_alta_AVC,
                                                                      tx_transf_estab_alta_hab_AVC,
                                                                      tx_transf_estab_alta_rr_AVC,
                                                                      tx_transf_estab_alta_nhab_AVC,
                                                                      tx_transf_estab_alta_outros_AVC,
                                                                      tx_transf_estab_alta_total_AVC)
      }
      
      
    }else{
      if(enfermidade == "IAM"){
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      tx_tempo_adeq_media_IAM,
                                                                      tx_tempo_adeq_media_microdif_IAM,
                                                                      tx_evasao_media_IAM,
                                                                      tx_transf_estab_media_hab_IAM,
                                                                      tx_transf_estab_media_rr_IAM,
                                                                      tx_transf_estab_media_nhab_IAM,
                                                                      tx_transf_estab_media_outros_IAM,
                                                                      tx_transf_estab_media_total_IAM)
      }else{
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      tx_tempo_adeq_media_AVC,
                                                                      tx_tempo_adeq_media_microdif_AVC,
                                                                      tx_evasao_media_AVC,
                                                                      tx_transf_estab_media_hab_AVC,
                                                                      tx_transf_estab_media_rr_AVC,
                                                                      tx_transf_estab_media_nhab_AVC,
                                                                      tx_transf_estab_media_outros_AVC,
                                                                      tx_transf_estab_media_total_AVC)
      }
      
    }
    df_comp[is.na(df_comp)] <- 0
    df_comp[,c(2:9)] <- df_comp[,c(2:9)]*100
    df_comp[,c(2:9)] <- lapply(df_comp[,c(2:9)], funcao_numeros_taxa)
    
    if(regiao == "nomemacro"){
      nome_col_1 <- "Macrorregião"
      nome_col_2 <- "Atendimentos no tempo adequado (outra macrorregião)"
    } else{
      nome_col_1 <- "Microrregião"
      nome_col_2 <- "Atendimentos no tempo adequado (outra microrregião)"
    }
    
    colnames(df_comp) <- c(nome_col_1, "Atendimentos no tempo adequado", nome_col_2, "Evasão", "Transferência (Habilitados IAM/AVC)", "Transferência (Rede Resposta)", "Transferência (Não Habilitados)", "Transferência (Outros)", "Transferência (Total)")
    
    return(df_comp)
  }
  
  comparacao_desempenho_transfs_tabela <- function(df, regiao, enfermidade, input){
    if(regiao == "nomemacro"){
      if(enfermidade == "IAM"){
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      n_transf_alta_destino_hab_IAM,
                                                                      n_transf_alta_destino_rr_IAM,
                                                                      n_transf_alta_destino_nhab_IAM)
      }else{
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      n_transf_alta_destino_hab_AVC,
                                                                      n_transf_alta_destino_rr_AVC,
                                                                      n_transf_alta_destino_nhab_AVC)
      }
      
      
    }else{
      if(enfermidade == "IAM"){
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      n_transf_media_destino_hab_IAM,
                                                                      n_transf_media_destino_rr_IAM,
                                                                      n_transf_media_destino_nhab_IAM)
      }else{
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      n_transf_media_destino_hab_AVC,
                                                                      n_transf_media_destino_rr_AVC,
                                                                      n_transf_media_destino_nhab_AVC)
      }
    }
    
    df_comp[is.na(df_comp)] <- 0
    df_comp[,c(2:4)] <- lapply(df_comp[,c(2:4)], funcao_numeros_absolutos)
    
    if(regiao == "nomemacro"){
      nome_col <- "Macrorregião"
    } else{
      nome_col <- "Microrregião"
    }
    
    colnames(df_comp) <- c(nome_col,"Habilitados IAM/AVC", "Rede Resposta", "Não Habilitados")
    
    return(df_comp)
  }
  
  comparacao_desempenho_distancias_tabela <- function(df, regiao, enfermidade, input){
    if(regiao == "nomemacro"){
      if(enfermidade == "IAM"){
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      dist_paciente_origem_alta_IAM,
                                                                      dist_origem_destino_alta_IAM,
                                                                      dist_paciente_origem_alta_macrodif_IAM,
                                                                      dist_origem_destino_alta_macrodif_IAM)
      }else{
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      dist_paciente_origem_alta_IAM,
                                                                      dist_origem_destino_alta_IAM,
                                                                      dist_paciente_origem_alta_macrodif_IAM,
                                                                      dist_origem_destino_alta_macrodif_IAM)
      }
      
      
    }else{
      if(enfermidade == "IAM"){
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      dist_paciente_origem_media_IAM,
                                                                      dist_origem_destino_media_IAM,
                                                                      dist_paciente_origem_media_microdif_IAM,
                                                                      dist_origem_destino_media_microdif_IAM)
      }else{
        df_comp <- df %>% filter(!!sym(regiao) %in% input) %>% select(!!sym(regiao),
                                                                      dist_paciente_origem_media_IAM,
                                                                      dist_origem_destino_media_IAM,
                                                                      dist_paciente_origem_media_microdif_IAM,
                                                                      dist_origem_destino_media_microdif_IAM)
      }
    }
    
    df_comp[is.na(df_comp)] <- 0
    df_comp[,c(2:5)] <- lapply(df_comp[,c(2:5)], funcao_numeros_taxa)
    
    if(regiao == "nomemacro"){
      nome_col_1 <- "Macrorregião"
      nome_col_2 <- "Residência/Porta de entrada (macro diferente)"
      nome_col_3 <- "Porta de entrada/Destino (macro diferente)"
    } else{
      nome_col_1 <- "Microrregião"
      nome_col_2 <- "Residência/Porta de entrada (micro diferente)"
      nome_col_3 <- "Porta de entrada/Destino (micro diferente)"
      
    }
    
    colnames(df_comp) <- c(nome_col_1,"Residência/Porta de entrada", "Porta de entrada/Destino", nome_col_2, nome_col_3)
    
    return(df_comp)
  }
  
  #Funções de download - aba Indicadores
  download_oferta_estabelecimentos <- function(regiao, input){
    df_dl <- df_estabelecimentos_final %>% filter(!!sym(regiao) %in% input) %>% select(macro,
                                                                                       micro,
                                                                                       nome_municipio,
                                                                                       CNES,
                                                                                       no_fantasia,
                                                                                       Tipo_estab,
                                                                                       Tipologia,
                                                                                       leitos_SUS)
    
    colnames(df_dl) <- c("Macrorregião", "Microrregião", "Município", "CNES", "Nome fantasia", "Tipo", "Tipologia", "Leitos SUS")
    return(df_dl)
  }
  
  #Macrorregiões------------------------------------------------------------
  
  ui_oferta_macro <- function(){
    if(input$macro_comparacao_input == 0){
      ui <- fluidRow(
        box(title = "Habilitados IAM/AVC",
            solidHeader = TRUE,
            status = "success",
            width = 3,
            height = '180px',
            uiOutput("texto_macro_h")),
        box(title = "Rede Resposta",
            solidHeader = TRUE,
            status = "primary",
            width = 3,
            height = '180px',
            uiOutput("texto_macro_rr")),
        box(title = "Não Habilitados",
            solidHeader = TRUE,
            status = "danger",
            width = 3,
            height = '180px',
            uiOutput("texto_macro_nh")),
        box(title = "Privados",
            solidHeader = TRUE,
            status = "warning",
            width = 3,
            height = '180px',
            uiOutput("texto_macro_nsus"))
      )
    } else{
      ui <- box(title = NULL,
                solidHeader = TRUE,
                width = 12,
                DT::dataTableOutput("comparacao_oferta_macro")
      )
    }
    return(ui)
  }
  ui_demanda_macro <- function(){
    if(input$macro_comparacao_input == 0){
      ui <- (fluidRow(
        box(title = "Tipo do estabelecimento porta de entrada",
            status = "warning",
            solidHeader = FALSE,
            width = 6,
            height = '425px',
            withSpinner(plotlyOutput("tipo_estabelecimentos_pe_macro"), type ='8')
        ),
        box(title = "Habilitação do estabelecimento porta de entrada",
            status = "warning",
            solidHeader = FALSE,
            width = 6,
            height = '425px',
            withSpinner(plotlyOutput("habilitacao_estabelecimentos_pe_macro"), type ='8')
        )
      ))
    } else{
      ui <- div(fluidRow(
        box(title = "Solicitações - Tipo do estabelecimento porta de entrada",
            solidHeader = FALSE,
            status = "warning",
            width = 12,
            DT::dataTableOutput("comparacao_demanda_tipo_macro")
        )
      ),
      fluidRow(
        box(title = "Solicitações - Habilitação do estabelecimento porta de entrada",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("comparacao_demanda_hab_macro")
        ) 
      ))
    }
    return(ui)
  }
  ui_ocupacao_macro <- function(){
    if(input$macro_comparacao_input == 0){
      ui <- fluidRow(
        box(title = NULL,
            status = "warning",
            solidHeader = FALSE,
            width = 8,
            height = '250px',
            withSpinner(plotlyOutput("ocupacao_macro", height = '250px', width = '100%'), type ='8')
        ),
        infoBoxOutput("tx_total_macro", width = 4)
      )
    } else{
      ui <- box(title = NULL,
                solidHeader = FALSE,
                status = "warning",
                width = 12,
                DT::dataTableOutput("comparacao_ocupacao_macro")
      )
    }
    return(ui)
  }
  ui_desempenho_macro <- function(){
    if(input$macro_comparacao_input == 0){
      ui <- div(
        tags$h1("Desempenho"),
        fluidRow(
          box(title = NULL,
              status = 'warning',
              solidHeader = FALSE,
              width = 6,
              height = '180px',
              withSpinner(plotlyOutput("tempo_adeq_macro", height = '180px', width = '100%'), type ='8')),
          column(width =6,
                 infoBoxOutput("tx_evasao_macro", width = 12),
                 infoBoxOutput("tx_transf_total_macro", width = 12),
          )
        ),
        fluidRow(
          box(title = "Número de transferências",
              solidHeader = TRUE,
              width = 6,
              height = '360px',
              withSpinner(plotlyOutput('numero_transf_macro', height = '360px'), type = '8')
          ),
          box(title = "Taxas de transferência (a partir da origem)",
              solidHeader = TRUE,
              width = 6,
              height = '340px',
              withSpinner(plotlyOutput('taxas_transf_macro', height = '310px', width = '100%'), type = '8')
          ),
        ),
        fluidRow(
          box(title = "Distância média percorrida pelo paciente",
              solidHeader = TRUE,
              width = 12,
              height = '300px',
              withSpinner(plotlyOutput('distancia_macro', height = '300px'), type = '8')
          )
        )
      )
    } else{
      ui <- div(
        tags$h1("Desempenho"),
        fluidRow(
          box(title = "Taxas (%)",
              solidHeader = FALSE,
              status = "warning",
              width = 12,
              DT::dataTableOutput("comparacao_desempenho_taxas_macro")
          )
        ),
        fluidRow(
          box(title = "Transferências por habilitação do destino",
              solidHeader = TRUE,
              width = 12,
              DT::dataTableOutput("comparacao_desempenho_transfs_macro")
          )
        ),
        fluidRow(
          box(title = "Distância média percorrida (km)",
              solidHeader = TRUE,
              width = 12,
              DT::dataTableOutput("comparacao_desempenho_distancia_macro")
          )
        )
      )
    } 
    return(ui)
  }
  
  observe({
    useShinyjs()
    if(input$macro_comparacao_input == 0){
      if(length(input$macro_input) == 0){
        disable("exibicao_macro")
      } else{
        enable("exibicao_macro")
      }
    } else{
      if(length(input$macro_input) == 0||length(input$macro_comp_input) == 0){
        disable("exibicao_macro")
      } else{
        enable("exibicao_macro")
      }
    }
    
    
  })
  
  output$macro_comparacao_ui <- renderUI({
    selectizeInput(
      inputId = "macro_comp_input",
      label = "Comparação",
      choices = sort(df_macro$nomemacro[!df_macro$nomemacro %in% input$macro_input]),
      options = list(
        placeholder = "Escolha as macrorregiões para comparação"),
      multiple = TRUE
    )
  })
  
  plots_macro <- reactiveValues(
    mapa = NULL,
    ui_oferta = NULL,
    ui_demanda = NULL,
    ui_ocupacao = NULL,
    ui_desempenho = NULL,
    texto_h = NULL,
    texto_rr = NULL,
    texto_nh = NULL,
    texto_nsus = NULL,
    tipo_estabelecimento_pe = NULL,
    habilitacao_estabelecimento_pe = NULL,
    ocupacao_grafico = NULL,
    taxa_total = NULL,
    tempo_adeq = NULL,
    taxa_evasao = NULL,
    numero_transf = NULL,
    tx_transf = NULL,
    tx_transf_total = NULL,
    distancias = NULL,
    comparacao_oferta = NULL,
    comparacao_demanda_tipo = NULL,
    comparacao_demanda_hab = NULL,
    comparacao_ocupacao = NULL,
    comparacao_desempenho_taxas = NULL,
    comparacao_desempenho_transfs = NULL,
    comparacao_desempenho_distancias = NULL,
    download_oferta_estab = NULL,
    download_oferta_indic = NULL,
    download_demanda_tipo = NULL,
    download_demanda_hab = NULL,
    download_ocupacao = NULL,
    download_desempenho_taxas = NULL,
    download_desempenho_transfs = NULL,
    download_desempenho_distancias = NULL
  )
  
  output$texto_botao_macro <- renderText({
    ifelse(input$exibicao_macro == 0, "Exibir", "Atualizar")
  })
  
  nivel_zoom <- function(macrorregiao){
    switch(macrorregiao,
           "Centro" = {zoom_final = 7.4},
           "Centro Sul" = {zoom_final = 8.4},
           "Jequitinhonha" = {zoom_final = 7.4},
           "Leste" = {zoom_final = 7.8},
           "Leste do Sul" = {zoom_final = 8.4},
           "Nordeste" = {zoom_final = 7.4},
           "Noroeste" = {zoom_final = 6.8},
           "Norte" = {zoom_final = 7},
           "Oeste" = {zoom_final = 8},
           "Sudeste" = {zoom_final = 8},
           "Sul" = {zoom_final = 7.4},
           "Triângulo do Norte" = {zoom_final = 8},
           "Triângulo do Sul" = {zoom_final = 7.8},
           "Vale do Aço" = {zoom_final = 8.5}
    )
    
    
    return(zoom_final)
  }
  
  
  
  observeEvent(input$exibicao_macro,{
    plots_macro$ui_oferta <- ui_oferta_macro()
    plots_macro$ui_demanda <- ui_demanda_macro()
    plots_macro$ui_ocupacao <- ui_ocupacao_macro()
    if(input$macro_enfermidade != 'Todas as enfermidades'){
      plots_macro$ui_desempenho <- ui_desempenho_macro()
      shinyjs::show("ui_desempenho_macro")
    } else{
      shinyjs::hide("ui_desempenho_macro")
    }
    
    if(input$macro_comparacao_input == 0){
      data_macro <- subset(mg_macros_shp, mg_micros_ == input$macro_input)
      data_micro <- subset(mg_micros_shp, n_macro == input$macro_input)
      
      plots_macro$texto_h <- HTML(texto_card(df_macro, "nomemacro", "macro", input$macro_input, "n_habilitados","Habilitados IAM/AVC", "taxa_leitos_sus_hab"))
      
      plots_macro$texto_rr <- HTML(texto_card(df_macro, "nomemacro", "macro", input$macro_input, "n_rr","Rede Resposta", "taxa_leitos_sus_rr"))
      
      plots_macro$texto_nh <- HTML(texto_card(df_macro, "nomemacro", "macro", input$macro_input, "n_naohab","Não Habilitados", "taxa_leitos_sus_naohab"))
      
      plots_macro$texto_nsus <- HTML(paste("<ul><li><b>",format(round(df_macro %>% filter(nomemacro == input$macro_input) %>% pull(tx_leito_nsus),2), nsmall = 2, decimal.mark = ","),"</b>leitos por 10.000 habitantes acima de 45 anos</li></ul>"))
      
      
      plots_macro$tipo_estabelecimento_pe <- grafico_tipo_estab_pe(df_macro,"nomemacro",input$macro_input, input$macro_enfermidade)
      
      plots_macro$habilitacao_estabelecimento_pe <- grafico_hab_estab_pe(df_macro,"nomemacro",input$macro_input, input$macro_enfermidade)
      
      plots_macro$ocupacao_grafico <- grafico_ocupacao(df_macro, "nomemacro", input$macro_input)
      
      plots_macro$taxa_total <- apputils::infoBox(title = "Taxa de ocupação total", value = paste(format(round(df_macro %>% filter(nomemacro == input$macro_input) %>% pull(ocup_tot),2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_total)
      
      plots_macro$download_oferta_estab <- download_oferta_estabelecimentos("macro", input$macro_input)
      plots_macro$download_oferta_indic <- comparacao_oferta_tabela(df_macro, "nomemacro",input$macro_input)
      plots_macro$download_demanda_tipo <- comparacao_demanda_tipo_tabela(df_macro, "nomemacro", input$macro_enfermidade,input$macro_input)
      plots_macro$download_demanda_hab <- comparacao_demanda_hab_tabela(df_macro, "nomemacro", input$macro_enfermidade,input$macro_input)
      plots_macro$download_ocupacao <- comparacao_ocupacao_tabela(df_macro, "nomemacro", input$macro_input)
      plots_macro$download_desempenho_taxas <- comparacao_desempenho_taxas_tabela(df_macro, "nomemacro", input$macro_enfermidade, input$macro_input)
      plots_macro$download_desempenho_transfs <- comparacao_desempenho_transfs_tabela(df_macro, "nomemacro", input$macro_enfermidade, input$macro_input)
      plots_macro$download_desempenho_distancias <- comparacao_desempenho_distancias_tabela(df_macro, "nomemacro", input$macro_enfermidade, input$macro_input)
      
      if(input$macro_enfermidade != "Todas as enfermidades"){
        if(input$macro_enfermidade == "IAM"){
          plots_macro$tempo_adeq <- grafico_tempo_adeq(df_macro,"nomemacro",input$macro_input,"tx_tempo_adeq_alta_IAM","tx_tempo_adeq_alta_macrodif_IAM")
          plots_macro$taxa_evasao <- apputils::infoBox(title = "Taxa de evasão", value = paste(format(round(df_macro %>% filter(nomemacro == input$macro_input) %>% pull(tx_evasao_alta_IAM)*100,2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_evasao)
          plots_macro$numero_transf <- grafico_transferencia(df_macro, "nomemacro", input$macro_input, "n_transf_alta_destino_hab_IAM", "n_transf_alta_destino_rr_IAM", "n_transf_alta_destino_nhab_IAM")
          plots_macro$tx_transf <- grafico_tx_transf(df_macro, "nomemacro", input$macro_input, "tx_transf_estab_alta_hab_IAM", "tx_transf_estab_alta_rr_IAM", "tx_transf_estab_alta_nhab_IAM", "tx_transf_estab_alta_outros_IAM")
          plots_macro$tx_transf_total <- apputils::infoBox(title = "Transferência total", value = paste(format(round(df_macro %>% filter(nomemacro == input$macro_input) %>% pull(tx_transf_estab_alta_total_IAM)*100,2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_tx_transf)
          plots_macro$distancias <- grafico_distancia(df_macro, "nomemacro",input$macro_input,"dist_paciente_origem_alta_IAM","dist_origem_destino_alta_IAM","dist_paciente_origem_alta_macrodif_IAM", "dist_origem_destino_alta_macrodif_IAM", "(macro diferente)")
        } else{
          plots_macro$tempo_adeq <- grafico_tempo_adeq(df_macro,"nomemacro",input$macro_input,"tx_tempo_adeq_alta_AVC","tx_tempo_adeq_alta_macrodif_AVC")
          plots_macro$taxa_evasao <- apputils::infoBox(title = "Taxa de evasão", value = paste(format(round(df_macro %>% filter(nomemacro == input$macro_input) %>% pull(tx_evasao_alta_AVC)*100,2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_evasao)
          plots_macro$numero_transf <- grafico_transferencia(df_macro, "nomemacro", input$macro_input, "n_transf_alta_destino_hab_AVC", "n_transf_alta_destino_rr_AVC", "n_transf_alta_destino_nhab_AVC")
          plots_macro$tx_transf <- grafico_tx_transf(df_macro, "nomemacro", input$macro_input, "tx_transf_estab_alta_hab_AVC", "tx_transf_estab_alta_rr_AVC", "tx_transf_estab_alta_nhab_AVC", "tx_transf_estab_alta_outros_AVC")
          plots_macro$tx_transf_total <- apputils::infoBox(title = "Transferência total", value = paste(format(round(df_macro %>% filter(nomemacro == input$macro_input) %>% pull(tx_transf_estab_alta_total_AVC)*100,2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_tx_transf)
          plots_macro$distancias <- grafico_distancia(df_macro, "nomemacro",input$macro_input,"dist_paciente_origem_alta_AVC","dist_origem_destino_alta_AVC","dist_paciente_origem_alta_macrodif_AVC", "dist_origem_destino_alta_macrodif_AVC", "(macro diferente)")
        }
      }
    } else{
      data_macro <- subset(mg_macros_shp, mg_micros_ %in% c(input$macro_input,input$macro_comp_input))
      data_micro <- subset(mg_micros_shp, n_macro %in% c(input$macro_input,input$macro_comp_input))
      
      plots_macro$comparacao_oferta <- comparacao_oferta_tabela(df_macro, "nomemacro",c(input$macro_input, input$macro_comp_input))
      plots_macro$comparacao_demanda_tipo <- comparacao_demanda_tipo_tabela(df_macro, "nomemacro", input$macro_enfermidade,c(input$macro_input, input$macro_comp_input))
      plots_macro$comparacao_demanda_hab <- comparacao_demanda_hab_tabela(df_macro, "nomemacro", input$macro_enfermidade,c(input$macro_input, input$macro_comp_input))
      plots_macro$comparacao_ocupacao <- comparacao_ocupacao_tabela(df_macro, "nomemacro", c(input$macro_input, input$macro_comp_input))
      plots_macro$comparacao_desempenho_taxas <- comparacao_desempenho_taxas_tabela(df_macro, "nomemacro", input$macro_enfermidade, c(input$macro_input, input$macro_comp_input))
      plots_macro$comparacao_desempenho_transfs <- comparacao_desempenho_transfs_tabela(df_macro, "nomemacro", input$macro_enfermidade, c(input$macro_input, input$macro_comp_input))
      plots_macro$comparacao_desempenho_distancias <- comparacao_desempenho_distancias_tabela(df_macro, "nomemacro", input$macro_enfermidade, c(input$macro_input, input$macro_comp_input))
      
      plots_macro$download_oferta_estab <- download_oferta_estabelecimentos("macro", c(input$macro_input, input$macro_comp_input))
      plots_macro$download_oferta_indic <- plots_macro$comparacao_oferta
      plots_macro$download_demanda_tipo <- plots_macro$comparacao_demanda_tipo
      plots_macro$download_demanda_hab <- plots_macro$comparacao_demanda_hab
      plots_macro$download_ocupacao <- plots_macro$comparacao_ocupacao
      plots_macro$download_desempenho_taxas <- plots_macro$comparacao_desempenho_taxas
      plots_macro$download_desempenho_transfs <- plots_macro$comparacao_desempenho_transfs
      plots_macro$download_desempenho_distancias <- plots_macro$comparacao_desempenho_distancias
    }
    
    
    
    zoom_n = nivel_zoom(input$macro_input)
    
    plots_macro$mapa <- mapas_indicadores(coordenadas_macros, "macrorregiao", input$macro_input, "macro", zoom_n, data_macro, data_micro, data_micro@data$n_micro, input$macro_comparacao_input, input$macro_comp_input)
    
  })
  
  output$ui_oferta_macro <- renderUI({
    if(is.null(plots_macro$ui_oferta)) return()
    plots_macro$ui_oferta
  })
  
  output$ui_demanda_macro <- renderUI({
    if(is.null(plots_macro$ui_demanda)) return()
    plots_macro$ui_demanda
  })
  
  output$ui_ocupacao_macro <- renderUI({
    if(is.null(plots_macro$ui_ocupacao)) return()
    plots_macro$ui_ocupacao
  })
  
  output$ui_desempenho_macro <- renderUI({
    if(is.null(plots_macro$ui_desempenho)) return()
    plots_macro$ui_desempenho
  })
  
  output$mapa_macro <- renderLeaflet({
    if(is.null(plots_macro$mapa)) return()
    plots_macro$mapa
  })
  
  output$texto_macro_h <- renderUI({
    if(is.null(plots_macro$texto_h)) return()
    plots_macro$texto_h
  })
  
  output$texto_macro_rr <- renderUI({
    if(is.null(plots_macro$texto_rr)) return()
    plots_macro$texto_rr
  })
  
  output$texto_macro_nh <- renderUI({
    if(is.null(plots_macro$texto_nh)) return()
    plots_macro$texto_nh
  })
  
  output$texto_macro_nsus <- renderUI({
    if(is.null(plots_macro$texto_nsus)) return()
    plots_macro$texto_nsus
  })
  
  output$tipo_estabelecimentos_pe_macro <- renderPlotly({
    if(is.null(plots_macro$tipo_estabelecimento_pe)) return()
    plots_macro$tipo_estabelecimento_pe
  })
  
  output$habilitacao_estabelecimentos_pe_macro <- renderPlotly({
    if(is.null(plots_macro$habilitacao_estabelecimento_pe)) return()
    plots_macro$habilitacao_estabelecimento_pe
  })
  
  output$ocupacao_macro <- renderPlotly({
    if(is.null(plots_macro$ocupacao_grafico)) return()
    plots_macro$ocupacao_grafico
  })
  
  output$tx_total_macro <- renderInfoBox({
    if(is.null(plots_macro$taxa_total)) return()
    plots_macro$taxa_total
  })
  
  output$tempo_adeq_macro <- renderPlotly({
    if(is.null(plots_macro$tempo_adeq)) return()
    plots_macro$tempo_adeq
  })
  
  output$tx_evasao_macro <- renderInfoBox({
    if(is.null(plots_macro$taxa_evasao)) return()
    plots_macro$taxa_evasao
  })
  
  output$numero_transf_macro <- renderPlotly({
    if(is.null(plots_macro$numero_transf)) return()
    plots_macro$numero_transf
  })
  
  output$taxas_transf_macro <- renderPlotly({
    if(is.null(plots_macro$tx_transf)) return()
    plots_macro$tx_transf
  })
  
  output$tx_transf_total_macro <- renderInfoBox({
    if(is.null(plots_macro$tx_transf_total)) return()
    plots_macro$tx_transf_total
  })
  
  output$distancia_macro <- renderPlotly({
    if(is.null(plots_macro$distancias)) return()
    plots_macro$distancias
  })
  
  output$comparacao_oferta_macro <- DT::renderDataTable(
    plots_macro$comparacao_oferta,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_demanda_tipo_macro <- DT::renderDataTable(
    plots_macro$comparacao_demanda_tipo,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_demanda_hab_macro <- DT::renderDataTable(
    plots_macro$comparacao_demanda_hab,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_ocupacao_macro <- DT::renderDataTable(
    plots_macro$comparacao_ocupacao,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_desempenho_taxas_macro <- DT::renderDataTable(
    plots_macro$comparacao_desempenho_taxas,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_desempenho_transfs_macro <- DT::renderDataTable(
    plots_macro$comparacao_desempenho_transfs,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_desempenho_distancia_macro <- DT::renderDataTable(
    plots_macro$comparacao_desempenho_distancias,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$download_macro <- downloadHandler(
    filename = function(){
      paste('pesquisa_indicadores_macro_', Sys.Date(),'.xlsx',sep='')
    },
    content = function(file){
      if(input$macro_enfermidade == "Todas as enfermidades"){
        write_xlsx(list("Oferta - Estabelecimentos" = plots_macro$download_oferta_estab,
                        "Oferta - Indicadores" = plots_macro$download_oferta_indic,
                        "Demanda - Tipo" = plots_macro$download_demanda_tipo,
                        "Demanda - Habilitação"= plots_macro$download_demanda_hab,
                        "Ocupação" = plots_macro$download_ocupacao), file)
      } else{
        write_xlsx(list("Oferta - Estabelecimentos" = plots_macro$download_oferta_estab,
                        "Oferta - Indicadores" = plots_macro$download_oferta_indic,
                        "Demanda - Tipo" = plots_macro$download_demanda_tipo,
                        "Demanda - Habilitação"= plots_macro$download_demanda_hab,
                        "Ocupação" = plots_macro$download_ocupacao,
                        "Desempenho - Taxas" = plots_macro$download_desempenho_taxas,
                        "Desempenho - Transferências" = plots_macro$download_desempenho_transfs,
                        "Desempenho - Distâncias" = plots_macro$download_desempenho_distancias), file)
      }
    }
    
  )
  
  
  
  #Microrregiões----------------------------------------------------------
  ui_oferta_micro <- function(){
    if(input$micro_comparacao_input == 0){
      ui <- fluidRow(
        box(title = "Habilitados IAM/AVC",
            solidHeader = TRUE,
            status = "success",
            width = 3,
            height = '180px',
            uiOutput("texto_micro_h")),
        box(title = "Rede Resposta",
            solidHeader = TRUE,
            status = "primary",
            width = 3,
            height = '180px',
            uiOutput("texto_micro_rr")),
        box(title = "Não Habilitados",
            solidHeader = TRUE,
            status = "danger",
            width = 3,
            height = '180px',
            uiOutput("texto_micro_nh")),
        box(title = "Privados",
            solidHeader = TRUE,
            status = "warning",
            width = 3,
            height = '180px',
            uiOutput("texto_micro_nsus"))
      )
    } else{
      ui <- box(title = NULL,
                solidHeader = TRUE,
                width = 12,
                DT::dataTableOutput("comparacao_oferta_micro")
      )
    }
    return(ui)
  }
  ui_demanda_micro <- function(){
    if(input$micro_comparacao_input == 0){
      ui <- (fluidRow(
        box(title = "Tipo do estabelecimento porta de entrada",
            status = "warning",
            solidHeader = FALSE,
            width = 6,
            height = '425px',
            withSpinner(plotlyOutput("tipo_estabelecimentos_pe_micro"), type ='8')
        ),
        box(title = "Habilitação do estabelecimento porta de entrada",
            status = "warning",
            solidHeader = FALSE,
            width = 6,
            height = '425px',
            withSpinner(plotlyOutput("habilitacao_estabelecimentos_pe_micro"), type ='8')
        )
      ))
    } else{
      ui <- div(fluidRow(
        box(title = "Solicitações - Tipo do estabelecimento porta de entrada",
            solidHeader = FALSE,
            status = "warning",
            width = 12,
            DT::dataTableOutput("comparacao_demanda_tipo_micro")
        )
      ),
      fluidRow(
        box(title = "Solicitações - Habilitação do estabelecimento porta de entrada",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("comparacao_demanda_hab_micro")
        ) 
      ))
    }
    return(ui)
  }
  ui_ocupacao_micro <- function(){
    if(input$micro_comparacao_input == 0){
      ui <- fluidRow(
        box(title = NULL,
            status = "warning",
            solidHeader = FALSE,
            width = 8,
            height = '250px',
            withSpinner(plotlyOutput("ocupacao_micro", height = '250px', width = '100%'), type ='8')
        ),
        infoBoxOutput("tx_total_micro", width = 4)
      )
    } else{
      ui <- box(title = NULL,
                solidHeader = FALSE,
                status = "warning",
                width = 12,
                DT::dataTableOutput("comparacao_ocupacao_micro")
      )
    }
    return(ui)
  }
  ui_desempenho_micro <- function(){
    if(input$micro_comparacao_input == 0){
      ui <- div(
        tags$h1("Desempenho"),
        fluidRow(
          box(title = NULL,
              status = 'warning',
              solidHeader = FALSE,
              width = 6,
              height = '180px',
              withSpinner(plotlyOutput("tempo_adeq_micro", height = '180px', width = '100%'), type ='8')),
          column(width =6,
                 infoBoxOutput("tx_evasao_micro", width = 12),
                 infoBoxOutput("tx_transf_total_micro", width = 12),
          )
        ),
        fluidRow(
          box(title = "Número de transferências",
              solidHeader = TRUE,
              width = 6,
              height = '360px',
              withSpinner(plotlyOutput('numero_transf_micro', height = '360px'), type = '8')
          ),
          box(title = "Taxas de transferência (a partir da origem)",
              solidHeader = TRUE,
              width = 6,
              height = '340px',
              withSpinner(plotlyOutput('taxas_transf_micro', height = '310px', width = '100%'), type = '8')
          ),
        ),
        fluidRow(
          box(title = "Distância média percorrida pelo paciente",
              solidHeader = TRUE,
              width = 12,
              height = '300px',
              withSpinner(plotlyOutput('distancia_micro', height = '300px'), type = '8')
          )
        )
      )
    } else{
      ui <- div(
        tags$h1("Desempenho"),
        fluidRow(
          box(title = "Taxas (%)",
              solidHeader = FALSE,
              status = "warning",
              width = 12,
              DT::dataTableOutput("comparacao_desempenho_taxas_micro")
          )
        ),
        fluidRow(
          box(title = "Transferências por habilitação do destino",
              solidHeader = TRUE,
              width = 12,
              DT::dataTableOutput("comparacao_desempenho_transfs_micro")
          )
        ),
        fluidRow(
          box(title = "Distância média percorrida (km)",
              solidHeader = TRUE,
              width = 12,
              DT::dataTableOutput("comparacao_desempenho_distancia_micro")
          )
        )
      )
    } 
    return(ui)
  }
  
  
  observe({
    useShinyjs()
    if(input$micro_comparacao_input == 0){
      if(length(input$micro_input) == 0){
        disable("exibicao_micro")
      } else{
        enable("exibicao_micro")
      }
    } else{
      if(length(input$micro_input) == 0||length(input$micro_comp_input) == 0){
        disable("exibicao_micro")
      } else{
        enable("exibicao_micro")
      }
    }
    
    
  })
  
  
  
  output$micro_tipo_comparacao_ui <- renderUI({
    if(input$micro_input != "Ipatinga"){
      escolhas_micro <- c("Todas as microrregiões", "Mesma macrorregião", "Mesmo tipo")
    } else{
      escolhas_micro <- c("Todas as microrregiões", "Mesma macrorregião")
    }
    
    radioButtons(
      inputId = "tipo_micro_input",
      label = "Comparação",
      choices = escolhas_micro,
    )
  })
  
  output$micro_comparacao_ui <- renderUI({
    req(input$tipo_micro_input)
    switch(input$tipo_micro_input,
           "Todas as microrregiões" = {
             lista_micro_comp <- sort(df_micro$nomemicro[!df_micro$nomemicro %in% input$micro_input])
           },
           "Mesma macrorregião" = {
             macro <- df_micro %>% filter(nomemicro == input$micro_input) %>% pull(codmacro)
             lista_micro_comp <- df_micro %>% filter(nomemicro != input$micro_input & codmacro == macro) %>% pull(nomemicro)
           },
           "Mesmo tipo" = {
             tipo <- df_micro %>% filter(nomemicro == input$micro_input) %>% pull(tipologia_micro_oferta_ocup_desemp)
             lista_micro_comp <- df_micro %>% filter(nomemicro != input$micro_input & tipologia_micro_oferta_ocup_desemp == tipo) %>% pull(nomemicro)
           }
    )
    
    selectizeInput(
      inputId = "micro_comp_input",
      label = NULL,
      choices = lista_micro_comp,
      options = list(
        placeholder = "Escolha as microrregiões para comparação"),
      multiple = TRUE
    )
  })
  
  output$texto_botao_micro <- renderText({
    ifelse(input$exibicao_micro == 0, "Exibir", "Atualizar")
  })
  
  plots_micro <- reactiveValues(
    mapa = NULL,
    ui_oferta = NULL,
    ui_demanda = NULL,
    ui_ocupacao = NULL,
    ui_desempenho = NULL,
    texto_h = NULL,
    texto_rr = NULL,
    texto_nh = NULL,
    texto_nsus = NULL,
    tipo_estabelecimento_pe = NULL,
    habilitacao_estabelecimento_pe = NULL,
    ocupacao_grafico = NULL,
    taxa_total = NULL,
    tempo_adeq = NULL,
    numero_transf = NULL,
    tx_transf = NULL,
    tx_transf_total = NULL,
    distancias = NULL,
    comparacao_oferta = NULL,
    comparacao_demanda_tipo = NULL,
    comparacao_demanda_hab = NULL,
    comparacao_ocupacao = NULL,
    comparacao_desempenho_taxas = NULL,
    comparacao_desempenho_transfs = NULL,
    comparacao_desempenho_distancias = NULL,
    download_oferta_estab = NULL,
    download_oferta_indic = NULL,
    download_demanda_tipo = NULL,
    download_demanda_hab = NULL,
    download_ocupacao = NULL,
    download_desempenho_taxas = NULL,
    download_desempenho_transfs = NULL,
    download_desempenho_distancias = NULL
  )
  
  observeEvent(input$exibicao_micro, {
    plots_micro$ui_oferta <- ui_oferta_micro()
    plots_micro$ui_demanda <- ui_demanda_micro()
    plots_micro$ui_ocupacao <- ui_ocupacao_micro()
    if(input$micro_enfermidade != 'Todas as enfermidades'){
      plots_micro$ui_desempenho <- ui_desempenho_micro()
      shinyjs::show("ui_desempenho_micro")
    } else{
      shinyjs::hide("ui_desempenho_micro")
    }
    
    if(input$micro_comparacao_input == 0){
      data_micro <- subset(mg_micros_shp, n_micro == input$micro_input)
      municipios_micro = df_ibge %>% filter(micro == input$micro_input) %>% pull(municipio)
      
      plots_micro$texto_h <- HTML(texto_card(df_micro, "nomemicro", "micro", input$micro_input, "n_habilitados","Habilitados IAM/AVC", "taxa_leitos_sus_hab"))
      
      plots_micro$texto_rr <- HTML(texto_card(df_micro, "nomemicro", "micro", input$micro_input, "n_rr","Rede Resposta", "taxa_leitos_sus_rr"))
      
      plots_micro$texto_nh <- HTML(texto_card(df_micro, "nomemicro", "micro", input$micro_input, "n_naohab","Não Habilitados", "taxa_leitos_sus_naohab"))
      
      plots_micro$texto_nsus <- HTML(paste("<ul><li><b>",format(round(df_micro %>% filter(nomemicro == input$micro_input) %>% pull(tx_leito_nsus),2), nsmall = 2, decimal.mark = ","),"</b>leitos por 10.000 habitantes acima de 45 anos</li></ul>"))
      
      plots_micro$tipo_estabelecimento_pe <- grafico_tipo_estab_pe(df_micro,"nomemicro",input$micro_input, input$micro_enfermidade)
      
      plots_micro$habilitacao_estabelecimento_pe <- grafico_hab_estab_pe(df_micro,"nomemicro",input$micro_input, input$micro_enfermidade)
      
      plots_micro$ocupacao_grafico <- grafico_ocupacao(df_micro, "nomemicro", input$micro_input)
      
      plots_micro$taxa_total <- apputils::infoBox(title = "Taxa de ocupação total", value = paste(format(round(df_micro %>% filter(nomemicro == input$micro_input) %>% pull(ocup_tot),2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_total)
      
      plots_micro$download_oferta_estab <- download_oferta_estabelecimentos("micro", input$micro_input)
      plots_micro$download_oferta_indic <- comparacao_oferta_tabela(df_micro, "nomemicro",input$micro_input)
      plots_micro$download_demanda_tipo <- comparacao_demanda_tipo_tabela(df_micro, "nomemicro", input$micro_enfermidade,input$micro_input)
      plots_micro$download_demanda_hab <- comparacao_demanda_hab_tabela(df_micro, "nomemicro", input$micro_enfermidade,input$micro_input)
      plots_micro$download_ocupacao <- comparacao_ocupacao_tabela(df_micro, "nomemicro", input$micro_input)
      plots_micro$download_desempenho_taxas <- comparacao_desempenho_taxas_tabela(df_micro, "nomemicro", input$micro_enfermidade, input$micro_input)
      plots_micro$download_desempenho_transfs <- comparacao_desempenho_transfs_tabela(df_micro, "nomemicro", input$micro_enfermidade, input$micro_input)
      plots_micro$download_desempenho_distancias <- comparacao_desempenho_distancias_tabela(df_micro, "nomemicro", input$micro_enfermidade, input$micro_input)
      
      if(input$micro_enfermidade != "Todas as enfermidades"){
        if(input$micro_enfermidade == "IAM"){
          plots_micro$tempo_adeq <- grafico_tempo_adeq(df_micro,"nomemicro",input$micro_input,"tx_tempo_adeq_media_IAM","tx_tempo_adeq_media_microdif_IAM")
          plots_micro$taxa_evasao <- apputils::infoBox(title = "Taxa de evasão", value = paste(format(round(df_micro %>% filter(nomemicro == input$micro_input) %>% pull(tx_evasao_media_IAM)*100,2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_evasao)
          plots_micro$numero_transf <- grafico_transferencia(df_micro, "nomemicro", input$micro_input, "n_transf_media_destino_hab_IAM", "n_transf_media_destino_rr_IAM", "n_transf_media_destino_nhab_IAM")
          plots_micro$tx_transf <- grafico_tx_transf(df_micro, "nomemicro", input$micro_input, "tx_transf_estab_media_hab_IAM", "tx_transf_estab_media_rr_IAM", "tx_transf_estab_media_nhab_IAM", "tx_transf_estab_media_outros_IAM")
          plots_micro$tx_transf_total <- apputils::infoBox(title = "Transferência total", value = paste(format(round(df_micro %>% filter(nomemicro == input$micro_input) %>% pull(tx_transf_estab_media_total_IAM)*100,2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_tx_transf)
          plots_micro$distancias <- grafico_distancia(df_micro, "nomemicro",input$micro_input,"dist_paciente_origem_media_IAM","dist_origem_destino_media_IAM","dist_paciente_origem_media_microdif_IAM", "dist_origem_destino_media_microdif_IAM", "(micro diferente)")
        } else{
          plots_micro$tempo_adeq <- grafico_tempo_adeq(df_micro,"nomemicro",input$micro_input,"tx_tempo_adeq_media_AVC","tx_tempo_adeq_media_microdif_AVC")
          plots_micro$taxa_evasao <- apputils::infoBox(title = "Taxa de evasão", value = paste(format(round(df_micro %>% filter(nomemicro == input$micro_input) %>% pull(tx_evasao_media_AVC)*100,2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_evasao)
          plots_micro$numero_transf <- grafico_transferencia(df_micro, "nomemicro", input$micro_input, "n_transf_media_destino_hab_AVC", "n_transf_media_destino_rr_AVC", "n_transf_media_destino_nhab_AVC")
          plots_micro$tx_transf <- grafico_tx_transf(df_micro, "nomemicro", input$micro_input, "tx_transf_estab_media_hab_AVC", "tx_transf_estab_media_rr_AVC", "tx_transf_estab_media_nhab_AVC", "tx_transf_estab_media_outros_AVC")
          plots_micro$tx_transf_total <- apputils::infoBox(title = "Transferência total", value = paste(format(round(df_micro %>% filter(nomemicro == input$micro_input) %>% pull(tx_transf_estab_media_total_AVC)*100,2), nsmall = 2, decimal.mark = ","),"%"), color = "white", icon = icone_tx_transf)
          plots_micro$distancias <- grafico_distancia(df_micro, "nomemicro",input$micro_input,"dist_paciente_origem_media_AVC","dist_origem_destino_media_AVC","dist_paciente_origem_media_microdif_AVC", "dist_origem_destino_media_microdif_AVC", "(micro diferente)")
        }
      }
      
    } else{
      data_micro <- subset(mg_micros_shp, n_micro %in% c(input$micro_input, input$micro_comp_input))
      municipios_micro = df_ibge %>% filter(micro %in% c(input$micro_input, input$micro_comp_input)) %>% pull(municipio)
      
      plots_micro$comparacao_oferta <- comparacao_oferta_tabela(df_micro, "nomemicro",c(input$micro_input, input$micro_comp_input))
      plots_micro$comparacao_demanda_tipo <- comparacao_demanda_tipo_tabela(df_micro, "nomemicro", input$micro_enfermidade,c(input$micro_input, input$micro_comp_input))
      plots_micro$comparacao_demanda_hab <- comparacao_demanda_hab_tabela(df_micro, "nomemicro", input$micro_enfermidade,c(input$micro_input, input$micro_comp_input))
      plots_micro$comparacao_ocupacao <- comparacao_ocupacao_tabela(df_micro, "nomemicro", c(input$micro_input, input$micro_comp_input))
      plots_micro$comparacao_desempenho_taxas <- comparacao_desempenho_taxas_tabela(df_micro, "nomemicro", input$micro_enfermidade, c(input$micro_input, input$micro_comp_input))
      plots_micro$comparacao_desempenho_transfs <- comparacao_desempenho_transfs_tabela(df_micro, "nomemicro", input$micro_enfermidade, c(input$micro_input, input$micro_comp_input))
      plots_micro$comparacao_desempenho_distancias <- comparacao_desempenho_distancias_tabela(df_micro, "nomemicro", input$micro_enfermidade, c(input$micro_input, input$micro_comp_input))
      
      
      plots_micro$download_oferta_estab <- download_oferta_estabelecimentos("micro", c(input$micro_input, input$micro_comp_input))
      plots_micro$download_oferta_indic <- plots_micro$comparacao_oferta
      plots_micro$download_demanda_tipo <- plots_micro$comparacao_demanda_tipo
      plots_micro$download_demanda_hab <- plots_micro$comparacao_demanda_hab
      plots_micro$download_ocupacao <- plots_micro$comparacao_ocupacao
      plots_micro$download_desempenho_taxas <- plots_micro$comparacao_desempenho_taxas
      plots_micro$download_desempenho_transfs <- plots_micro$comparacao_desempenho_transfs
      plots_micro$download_desempenho_distancias <- plots_micro$comparacao_desempenho_distancias
    }
    
    data_municipios <- subset(mg_shp, NM_MUN %in% municipios_micro)
    
    zoom_n <- ifelse(gArea(data_micro) < 0.2, 9.5,
                     ifelse((gArea(data_micro) >= 0.2 && gArea(data_micro) < 0.4), 9,
                            ifelse((gArea(data_micro) >= 0.4 && gArea(data_micro) < 0.6),8.5,
                                   ifelse((gArea(data_micro) >= 0.6 && gArea(data_micro) < 0.8),8,7.5))))
    
    plots_micro$mapa <- mapas_indicadores(coordenadas_micros, "microrregiao", input$micro_input, "micro", zoom_n, data_micro, data_municipios, data_municipios@data$NM_MUN, input$micro_comparacao_input, input$micro_comp_input)
  })
  
  output$ui_oferta_micro <- renderUI({
    if(is.null(plots_micro$ui_oferta)) return()
    plots_micro$ui_oferta
  })
  
  output$ui_demanda_micro <- renderUI({
    if(is.null(plots_micro$ui_demanda)) return()
    plots_micro$ui_demanda
  })
  
  output$ui_ocupacao_micro <- renderUI({
    if(is.null(plots_micro$ui_ocupacao)) return()
    plots_micro$ui_ocupacao
  })
  
  output$ui_desempenho_micro <- renderUI({
    if(is.null(plots_micro$ui_desempenho)) return()
    plots_micro$ui_desempenho
  })
  
  output$mapa_micro <- renderLeaflet({
    if(is.null(plots_micro$mapa)) return()
    plots_micro$mapa
  })
  
  output$texto_micro_h <- renderUI({
    if(is.null(plots_micro$texto_h)) return()
    plots_micro$texto_h
  })
  
  output$texto_micro_rr <- renderUI({
    if(is.null(plots_micro$texto_rr)) return()
    plots_micro$texto_rr
  })
  
  output$texto_micro_nh <- renderUI({
    if(is.null(plots_micro$texto_nh)) return()
    plots_micro$texto_nh
  })
  
  output$texto_micro_nsus <- renderUI({
    if(is.null(plots_micro$texto_nsus)) return()
    plots_micro$texto_nsus
  })
  
  output$tipo_estabelecimentos_pe_micro <- renderPlotly({
    if(is.null(plots_micro$tipo_estabelecimento_pe)) return()
    plots_micro$tipo_estabelecimento_pe
  })
  
  output$habilitacao_estabelecimentos_pe_micro <- renderPlotly({
    if(is.null(plots_micro$habilitacao_estabelecimento_pe)) return()
    plots_micro$habilitacao_estabelecimento_pe
  })
  
  output$ocupacao_micro <- renderPlotly({
    if(is.null(plots_micro$ocupacao_grafico)) return()
    plots_micro$ocupacao_grafico
  })
  
  output$tx_total_micro <- renderInfoBox({
    if(is.null(plots_micro$taxa_total)) return()
    plots_micro$taxa_total
  })
  
  output$tempo_adeq_micro <- renderPlotly({
    if(is.null(plots_micro$tempo_adeq)) return()
    plots_micro$tempo_adeq
  })
  
  output$tx_evasao_micro <- renderInfoBox({
    if(is.null(plots_micro$taxa_evasao)) return()
    plots_micro$taxa_evasao
  })
  
  output$numero_transf_micro <- renderPlotly({
    if(is.null(plots_micro$numero_transf)) return()
    plots_micro$numero_transf
  })
  
  output$taxas_transf_micro <- renderPlotly({
    if(is.null(plots_micro$tx_transf)) return()
    plots_micro$tx_transf
  })
  
  output$tx_transf_total_micro <- renderInfoBox({
    if(is.null(plots_micro$tx_transf_total)) return()
    plots_micro$tx_transf_total
  })
  
  output$distancia_micro <- renderPlotly({
    if(is.null(plots_micro$distancias)) return()
    plots_micro$distancias
  })
  
  output$comparacao_oferta_micro <- DT::renderDataTable(
    plots_micro$comparacao_oferta,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_demanda_tipo_micro <- DT::renderDataTable(
    plots_micro$comparacao_demanda_tipo,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_demanda_hab_micro <- DT::renderDataTable(
    plots_micro$comparacao_demanda_hab,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_ocupacao_micro <- DT::renderDataTable(
    plots_micro$comparacao_ocupacao,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_desempenho_taxas_micro <- DT::renderDataTable(
    plots_micro$comparacao_desempenho_taxas,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_desempenho_transfs_micro <- DT::renderDataTable(
    plots_micro$comparacao_desempenho_transfs,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$comparacao_desempenho_distancia_micro <- DT::renderDataTable(
    plots_micro$comparacao_desempenho_distancias,
    options = list(scrollX = '400px',
                   searching = FALSE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  output$download_micro <- downloadHandler(
    filename = function(){
      paste('pesquisa_indicadores_micro_', Sys.Date(),'.xlsx',sep='')
    },
    content = function(file){
      if(input$micro_enfermidade == "Todas as enfermidades"){
        write_xlsx(list("Oferta - Estabelecimentos" = plots_micro$download_oferta_estab,
                        "Oferta - Indicadores" = plots_micro$download_oferta_indic,
                        "Demanda - Tipo" = plots_micro$download_demanda_tipo,
                        "Demanda - Habilitação"= plots_micro$download_demanda_hab,
                        "Ocupação" = plots_micro$download_ocupacao), file)
      } else{
        write_xlsx(list("Oferta - Estabelecimentos" = plots_micro$download_oferta_estab,
                        "Oferta - Indicadores" = plots_micro$download_oferta_indic,
                        "Demanda - Tipo" = plots_micro$download_demanda_tipo,
                        "Demanda - Habilitação"= plots_micro$download_demanda_hab,
                        "Ocupação" = plots_micro$download_ocupacao,
                        "Desempenho - Taxas" = plots_micro$download_desempenho_taxas,
                        "Desempenho - Transferências" = plots_micro$download_desempenho_transfs,
                        "Desempenho - Distâncias" = plots_micro$download_desempenho_distancias), file)
      }
    }
    
  )
  
}

shinyApp(ui = ui, server = server)
