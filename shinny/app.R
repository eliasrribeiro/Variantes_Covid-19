library(shiny)
library(dplyr)
library(magrittr)
library(readxl)
library(shinydashboard)
library(questionr)
library(kableExtra)
library(ggplot2)
library(highcharter)
library(summarytools)
library(modelsummary)
library(abjData)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(reactable)
library(htmltools)
library(zoo)
library(plotly)
library(lubridate)
library(googlesheets4)
library(shinyjs)

# Carregando a base de dados ----

dados5 <- readRDS("dados6.rds")

#criando variável chamada variante(original,gama,delta,omicron)
in_gama <- as.Date("01-02-2021",format="%d-%m-%Y")
in_delta <- as.Date("01-08-2021",format="%d-%m-%Y")
in_ommi <- as.Date("01-01-2022",format="%d-%m-%Y")
dados5 <- dados5 %>% 
  mutate(variante = case_when(dt_sint < in_gama ~ "original",
                              dt_sint >= in_gama & dt_sint < in_delta ~ "gama",
                              dt_sint >= in_delta & dt_sint < in_ommi ~ "delta",
                              dt_sint >= in_ommi ~ "omicron")) %>% 
  mutate(month_year = paste(formatC(month(dt_sint), width=2, format="d", flag="0"),
                            year(dt_sint),sep="/")) %>% 
  mutate(mes = month(dt_sint))


dados5 <- dados5 %>% 
    mutate(vacina_cov = ifelse(variante == "original","não",vacina_cov))

dados5$variante <- factor(dados5$variante,
                          levels = c("original", "gama", "delta","omicron"))
  
dados5 <- dados5 %>% 
  filter(CLASSI_FIN == "5")

dados5 <- dados5 %>% 
  mutate(dt_1dose = as.Date(DOSE_1_COV, format = "%d/%m/%Y")) %>% 
  mutate(dt_2dose = as.Date(DOSE_2_COV, format = "%d/%m/%Y")) %>% 
  mutate(doses = case_when(
    vacina == "sim" & is.na(dt_1dose) 
    & is.na(dt_2dose) ~ "pelo menos uma dose",
    !is.na(dt_2dose) ~ "duas doses",
    !is.na(dt_1dose) & is.na(dt_2dose) ~ "pelo menos uma dose",
    TRUE ~ "não informado"))


# Alterações iniciais na base ----

## Reajuste de levels ----
dados5$faixa_et <- factor(dados5$faixa_et,
                          levels = c("<20", "20-34", ">=35"))

dados5$escol <- factor(dados5$escol,
                       levels = c("sem escol", "fund1", "fund2", "medio", "superior"))

dados5$suport_ven <- factor(dados5$suport_ven,
                            levels = c("não", "não invasivo", "invasivo"))

## Restante de alterações ----
dados5$raca_sel <- dados5$raca
dados5$raca_sel <-
  ifelse(is.na(dados5$raca_sel), "não informado", dados5$raca_sel)

dados5$vacina_cov_sel <-
  ifelse(is.na(dados5$vacina_cov), "nao informado", dados5$vacina_cov)

dados5$CLASSI_FIN <- as.factor(dados5$CLASSI_FIN)


dados5$DT_SIN_PRI <- dmy(dados5$DT_SIN_PRI)
dados5$DT_EVOLUCA <- dmy(dados5$DT_EVOLUCA)


sticky_style <-
  list(
    position = "sticky",
    left = 0,
    background = "#fff",
    zIndex = 1,
    borderRight = "1px solid #eee"
  )

# Dia de hoje ----
hoje <- Sys.Date()

# Que vamos usar para medidas resumo ----
media <- function(x)
  mean(x, na.rm = TRUE)
mediana <- function(x)
  median(x, na.rm = TRUE)
DP <- function(x)
  sd(x, na.rm = TRUE)
minimo <- function(x)
  base::min(x, na.rm = TRUE)
maximo <- function(x)
  base::max(x, na.rm = TRUE)
q25 <- function(x)
  stats::quantile(x, p = 0.25, na.rm = TRUE)
q75 <- function(x)
  stats::quantile(x, p = 0.75, na.rm = TRUE)
IQR <- function(x)
  round(q75(x) - q25(x), 2)
n <- function(x)
  round(sum(!is.na(x)), digits = 0)
faltantes <- function(x)
  round(sum(is.na(x)), digits = 0)


humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

table <- "responses"

appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# info for sharing this app on facebook/twitter ----
share <- list(title = "Inscrição na Newsletter")

# Custom dropdownMenu function for share icons ----
customSentence <- function(numItems, type) {
  paste("Feedback")
}

customSentence_share <- function(numItems, type) {
  paste("Gostou? Compartilhe!")
}

dropdownMenuCustom <- function (..., 
                                type = c("messages", "notifications", "tasks"), 
                                badgeStatus = "primary", 
                                icon = NULL,
                                .list = NULL, 
                                customSentence = customSentence){
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, 
                   messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), 
                   tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}

# User Interface ----
ui <-
  dashboardPage(
    title = "Análise Variantes COVID-19",
    dashboardHeader(
      title = strong('Análise Variantes COVID-19'),
      dropdownMenuCustom(
        type = 'message',
        customSentence = customSentence,
        messageItem(
          from = "observatorioobstetricobr@gmail.com",
          message =  "",
          icon = icon("envelope"),
          href = "mailto:observatorioobstetricobr@gmail.com"
        ),
        icon = icon('envelope')
      ),
      dropdownMenuCustom(
        type = 'message',
        customSentence = customSentence_share,
        icon = icon("share-alt"),
        messageItem(
          from = 'Twitter',
          message = "",
          icon = icon("twitter"),
          href = "https://twitter.com/intent/tweet?url=Observat%C3%B3rio%20Obst%C3%A9trico%20Brasileiro%20COVID-19%0Ahttps%3A%2F%2Fobservatorioobstetrico.shinyapps.io%2Fcovid_gesta_puerp_br%2F"
        ),
        messageItem(
          from = 'Facebook',
          message = "",
          icon = icon("facebook"),
          href = "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fobservatorioobstetrico.shinyapps.io%2Fcovid_gesta_puerp_br%2F"
        ),
        messageItem(
          from = 'WhatsApp',
          message = "",
          icon = icon("whatsapp-square"),
          href = "https://web.whatsapp.com/send?text=https%3A%2F%2Fobservatorioobstetrico.shinyapps.io%2Fcovid_gesta_puerp_br%2F"
        ),
        messageItem(
          from = 'LinkedIn',
          message = "",
          icon = icon("linkedin"),
          href = "http://www.linkedin.com/shareArticle?mini=true&url=https%3A%2F%2Fobservatorioobstetrico.shinyapps.io%2Fcovid_gesta_puerp_br%2F&title=Observat%C3%B3rio%20Obst%C3%A9trico%20Brasileiro%20COVID-19%20"
        )
      )
    ),
    dashboardSidebar(
      ## Menu ----
      sidebarMenu(
        style = "position: fixed; overflow: visible;",
        # menuItem("Início", tabName = "inicio"),
        # menuItem("Documentação", tabName = "doc"),
        # menuItem("Informações Gerais", tabName = "info"),
        menuItem("Análise", tabName = "tab_cruzada")
        # menuItem("Variáveis tempo", tabName = "tempo"),
        # menuItem("Tempo de atraso de notificação", tabName = "notificacao"),
        # menuItem("Casos por UF e município", tabName = "muni_casos"),
        # menuItem("Médias móveis", tabName = "graf"),
        # menuItem("Mapas", tabName = "mps_casos")
      )
    ),
    ### Item Inicio ----
    dashboardBody(
      tags$head(tags$style(
        HTML(
          '
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0A1E3C;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0A1E3C;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0A1E3C;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #0A1E3C;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #32A0FF;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #0A1E3C;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #32A0FF;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #32A0FF;
                              }
                              '
        ),
        HTML("hr {border-top: 1px solid #0A1E3C;}")
      )),
       tabItems(
      ### Item Análise cruzada ----
      tabItem(tabName = "tab_cruzada",
              fluidRow(
                box(
                  collapsible = TRUE,
                  width = 4,
                  title = "Selecione",
                  status = "primary",
                  solidHeader = FALSE,
                  h4(strong("Variáveis de interesse")),
                  selectInput(
                    inputId = "caracteristicas1",
                    label = "Característica na linha:",
                    choices = c(
                      "Ano do caso" = "ano",
                      "Classificação do caso" = "classi_fin1",
                      "Diagnóstico COVID-19" = "classi_covid",
                      "Momento gestacional" = "classi_gesta_puerp",
                      "Região do Brasil"  = "region",
                      "UF do Brasil"  = "SG_UF",
                      "Raça" = "raca",
                      "Escolaridade" = "escol",
                      "Faixa etária" = "faixa_et",
                      "Histórico de viagem" = "hist_viagem",
                      "Mudança município" = "mudou_muni",
                      "Zona da residência" = "zona",
                      "SG para SRAG" = "sg_para_srag",
                      "Infecção no hospital" = "inf_inter",
                      "Contato com suíno" = "cont_ave_suino",
                      "Vacina contra gripe" = "vacina",
                      "Antiviral" = "antiviral",
                      "Internação" = "hospital",
                      "Febre" = "febre",
                      "Tosse" = "tosse",
                      "Garganta" = "garganta",
                      "Dispinéia" = "dispneia",
                      "Desconforto respiratório" = "desc_resp",
                      "Saturação" = "saturacao",
                      "Diarréia" = "diarreia",
                      "Vômito" = "vomito",
                      "Dor abdominal" = "dor_abd",
                      "Fadiga" = "fadiga",
                      "Perda olfativa" = "perd_olft",
                      "Perda paladar" = "perd_pala",
                      "Cardiovascular" = "cardiopati",
                      "Hematológica" = "hematologi",
                      "Hepática" = "hepatica",
                      "Asma" = "asma",
                      "Diabetes" = "diabetes",
                      "Neuropatia" = "neuro",
                      "Pneumopatia" = "pneumopati",
                      "Imunodepressão" = "imunodepre",
                      "Renal" = "renal",
                      "Obesidade" = "obesidade",
                      "UTI" = "uti",
                      "Suporte ventilatório" = "suport_ven",
                      "Intubação S/N" = "intubacao_SN",
                      "Evolução" = "evolucao",
                      "Vacina" = "vacina_cov"
                    ),
                    selected = "evolucao",
                    width = "220px"
                  ),
                  # selectInput(
                  #   inputId = "caracteristicas2",
                  #   label = "Característica na coluna:",
                  #   choices = c(
                  #     "Ano do caso" = "ano",
                  #     "Classificação do caso" = "classi_fin1",
                  #     "Diagnóstico COVID-19" = "classi_covid",
                  #     "Momento gestacional" = "classi_gesta_puerp",
                  #     "Região do Brasil"  = "region",
                  #     "UF do Brasil"  = "SG_UF",
                  #     "Raça" = "raca",
                  #     "Escolaridade" = "escol",
                  #     "Faixa etária" = "faixa_et",
                  #     "Histórico de viagem" = "hist_viagem",
                  #     "Mudança município" = "mudou_muni",
                  #     "Zona da residência" = "zona",
                  #     "SG para SRAG" = "sg_para_srag",
                  #     "Infecção no hospital" = "inf_inter",
                  #     "Contato com suíno" = "cont_ave_suino",
                  #     "Vacina contra gripe" = "vacina",
                  #     "Antiviral" = "antiviral",
                  #     "Internação" = "hospital",
                  #     "Febre" = "febre",
                  #     "Tosse" = "tosse",
                  #     "Garganta" = "garganta",
                  #     "Dispinéia" = "dispneia",
                  #     "Desconforto respiratório" = "desc_resp",
                  #     "Saturação" = "saturacao",
                  #     "Diarréia" = "diarreia",
                  #     "Vômito" = "vomito",
                  #     "Dor abdominal" = "dor_abd",
                  #     "Fadiga" = "fadiga",
                  #     "Perda olfativa" = "perd_olft",
                  #     "Perda paladar" = "perd_pala",
                  #     "Cardiovascular" = "cardiopati",
                  #     "Hematológica" = "hematologi",
                  #     "Hepática" = "hepatica",
                  #     "Asma" = "asma",
                  #     "Diabetes" = "diabetes",
                  #     "Neuropatia" = "neuro",
                  #     "Pneumopatia" = "pneumopati",
                  #     "Imunodepressão" = "imunodepre",
                  #     "Renal" = "renal",
                  #     "Obesidade" = "obesidade",
                  #     "UTI" = "uti",
                  #     "Suporte ventilatório" = "suport_ven",
                  #     "Evolução" = "evolucao"
                  #   ),
                  #   selected = "evolucao",
                  #   width = "220px"
                  # ),
                  checkboxGroupInput(
                    inputId = "classivariante",
                    label = "Tipo de Variante de COVID-19:",
                    choices = c(
                      "Original" = "original",
                      "Gama" = "gama",
                      "Delta" = "delta",
                      "Omicron" = "omicron"
                    ),
                    selected = c("original","gama","delta","omicron")
                  ),
                  strong("Selecionar só casos válidos?"),
                  checkboxInput("na1", "Excluir casos faltantes?",
                                value = TRUE),
                  hr(),
                  h4(strong(
                    "Características gestantes e/ou puérperas"
                  )),
                  sliderInput(
                    inputId = "idade2",
                    label = "Intervalo de idade:",
                    min = min(dados5$idade_anos),
                    max = max(dados5$idade_anos),
                    value = c(min(dados5$idade_anos), max(dados5$idade_anos))
                  ),
                  checkboxGroupInput(
                    inputId = "GestantePuerpera2",
                    label = "Idade gestacional/puérpera:",
                    choices = c(
                      "1° trimestre" = "1tri",
                      "2° trimestre" = "2tri",
                      "3° trimestre" = "3tri",
                      "Idade gestacional ignorada" = "IG_ig",
                      "Puérpera" = "puerp"
                    ),
                    selected = c("1tri", "2tri", "3tri", "IG_ig", "puerp")
                  ),
                  hr(),
                  h4(strong("Diagnóstico")),
                  checkboxGroupInput(
                    inputId = "classiCovid2",
                    label = "Tipo de diagnóstico de COVID:",
                    choices = c(
                      "PCR" = "pcr",
                      "Antigênio" = "antigenio",
                      "Sorologia" = "sorologia",
                      "Outro" = "outro",
                      "Não confirmado ou não COVID-19" = "não"
                    ),
                    selected = c("pcr", "antigenio", "sorologia", "outro", "não")
                  ),
                  hr(),
                  h4(strong("Vacina")),
                  checkboxGroupInput(
                    inputId = "vacinacov",
                    label = "Tomou a vacina?:",
                    choices = c(
                      "Sim" = "sim",
                      "Não" = "não",
                      "Não informado" = "nao informado"
                    ),
                    selected = c("sim","não","nao informado")
                  ),
                  checkboxGroupInput(
                    inputId = "dosescov",
                    label = "Tomou quantas doses?:",
                    choices = c(
                      "Pelo menos uma dose" = "pelo menos uma dose",
                      "Duas doses" = "duas doses",
                      "Não informado" = "não informado"
                    ),
                    selected = c("pelo menos uma dose","duas doses","não informado")
                  )
                ),
                box(
                  width = 8,
                  status = "primary",
                  div(tabsetPanel(
                    tabPanel("Tabela cruzada",
                             highcharter::highchartOutput("plot11")),
                    tabPanel("Teste de Fisher",
                             verbatimTextOutput("print1")),
                    tabPanel("Grafico Geral",
                             highcharter::highchartOutput("plot12"))
                  )),
                  verbatimTextOutput("table1"),
                  h3(strong("Observação")),
                  p(
                    "<NA> na tabela acima indica os casos faltantes ou ignorados (não resposta) das variáveis em questão.
                  No gráfico, essa informação aparece na categoria com número (por exemplo, número 2)."
                  ),
                  p(
                    "Caso queira só analisar os casos válidos (sem considerar os casos faltantes), no canto superior esquerdo em 'Selecionar só casos válidos?',
                selecione o botão 'Excluir casos faltantes?'."
                  )
                )
              ))
    )))
    
# Server ----
server <- function(input, output, session) {
  
  ## base de dados com filtragem por inputs ----
  
  selectData2 <- reactive({
    dados5 %>%
      dplyr::filter(idade_anos >= input$idade2[1]) %>%
      dplyr::filter(idade_anos <= input$idade2[2]) %>%
      dplyr::filter(classi_gesta_puerp %in% input$GestantePuerpera2) %>%
      # dplyr::filter(CLASSI_FIN %in% input$confCovid2) %>%
      dplyr::filter(classi_covid %in% input$classiCovid2) %>%
      dplyr::filter(vacina_cov_sel %in% input$vacinacov) %>%
      dplyr::filter(variante %in% input$classivariante) %>%
      dplyr::filter(doses %in% input$dosescov) %>%
      {
        if (input$na1 == TRUE)
          dplyr::filter(., !is.na(get(input$caracteristicas1)))
        else
          dplyr::filter(., (!is.na(get(
            input$caracteristicas1
          )) | is.na(get(
            input$caracteristicas1
          ))))
      }
  })
  
  ### Gráfico e tabela cruzada ----
  dados_hc_aux <- reactive({
    selectData2() %>%
      count(var = .[["variante"]]) %>%
      mutate(ntot = n) %>%
      select(-n)
  })
  
  dados_hc <- reactive({
    selectData2() %>%
      count(var = .[["variante"]],
            var2 = .[[input$caracteristicas1]]) %>%
      full_join(dados_hc_aux(), by = "var") %>%
      mutate(porc = round((n / ntot) * 100, 2))
  })
  
  output$plot11 <- highcharter::renderHighchart({
    hchart(dados_hc(), type = "column",
           hcaes(x = var,
                 y = porc, group = var2)) %>%
      hc_xAxis(title = list(text = "Variantes")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  ### Gráfico geral por variável (vacina covid e variantes) ----
  
  tab1 <- reactive({
    if(input$caracteristicas1 == "evolucao"){
      selectData2() %>% 
        filter(evolucao=="obito") %>%
        with(.,data.frame(prop.table(table(vacina_cov,variante))))
    }
    else if(input$caracteristicas1 == "suport_ven" | input$caracteristicas1 == "intubacao_SN"){
      selectData2() %>% 
        filter(suport_ven=="invasivo") %>%
        with(.,data.frame(prop.table(table(vacina_cov,variante))))
    }
    else {
      selectData2() %>% 
        filter(get(input$caracteristicas1)=="sim") %>%
        with(.,data.frame(prop.table(table(vacina_cov,variante))))
    }
    })
    
  output$plot12 <- highcharter::renderHighchart({
      hchart(tab1(), type = "line",
           hcaes(x = variante,
                 y = Freq, group = vacina_cov,colour=vacina_cov)) %>%
      hc_xAxis(title = list(text = "Variantes")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  output$table1 <- renderPrint({
    st_options(headings = FALSE, display.labels = FALSE)
    with(
      selectData2(),
      summarytools::ctable(
        get(input$caracteristicas1),
        variante,
        prop = "c",
        headings = st_options("headings"),
        display.labels = st_options("display.labels"),
        useNA = "ifany",
        dnn = c(input$caracteristicas1, "Variantes"),
        chisq = TRUE
      )
    )
  })
  
  output$print1 <- renderPrint({
    with(selectData2(),
         fisher.test(variante,get(input$caracteristicas1),simulate.p.value = TRUE))
  })
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x)
      input[[x]])
    data <- c(data, timestamp = humanTime())
    data <- t(data)
    data
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
}

shinyApp(ui, server)
