  library(tidyverse)
  library(janitor)
  library(openxlsx)
  library(readxl)
  library(DBI)
  library(pool)
  library(RPostgres)
  library(stringr)
  library(collapse)
  library(gsubfn)
  library(glue)
  library(feather)
  library(splitstackshape)
  library(haven)
  library(lubridate)
  library(ggplot2)
  library(highcharter)
  library(bs4Dash)
  library(bslib)
  library(DT)
  library(flextable)
  library(fresh)
  library(memoise)
  library(openssl)
  library(plotly)
  library(scales)
  library(shinyalert)
  library(shinycssloaders)
  library(shinyFeedback)
  library(shinyFiles)
  library(shinyjs)
  library(shinymanager)
  library(shinyvalidate)
  library(shinyWidgets)
  library(sodium)
  library(sp)
  library(uuid)
  library(shadowtext)
  library(ggtext)
  library(data.table)
  library(shiny)
  library(RColorBrewer)
  shinyOptions(cache = cachem::cache_disk("./bind-cache"))
  set.seed(123)
  options(scipen = 9999)
  
  divisao_administrativa <- DBI::dbGetQuery(far_pool, "SELECT L.id AS admin_id, localidade, distrito, provincia FROM moz.localidades L LEFT JOIN moz.distritos D ON D.id = L.distrito")

  
  {
    db2 <- 'semea'
    host_db2 <- "semea.choww6kimn1s.eu-north-1.rds.amazonaws.com"
    db_port2 <- '5432'
    db_user2 <- "postgres"
    db_password2 <- "FAR;2030,"
    far_pool <- dbPool(RPostgres::Postgres(), dbname = db2, host=host_db2, port=db_port2, user=db_user2, password=db_password2)
  } ### credentials
  
  BLUE <- "#076fa2"
  RED <- "#E3120B"
  BLACK <- "#202020"
  GREY <- "grey50"  

{
title <- tags$a(href='https://finance.far.org',tags$img(src="FARFPNEW.png", height = '150', width = '240'), '', target="_blank")
user_image <- "https://img.icons8.com/color/96/000000/circled-user-male-skin-type-6--v1.png"
provincias  <- c('CABO DELGADO', 'GAZA', 'INHAMBANE', 'MANICA', 'MAPUTO', 'MAPUTO CIDADE', 'NAMPULA', 'NIASSA', 'SOFALA', 'TETE', 'ZAMBÉZIA')
} ########### LOAD DATASETS
  
  
  
{
two_decimals <- scales::label_comma(accuracy = .2, big.mark = ".", decimal.mark = ",")
zero_decimals <- function (numero){prettyNum(numero, big.mark = ",")}
grafico_barras <- function(data, categoria, valor, meta){
  data <- data %>% mutate(valores := {{valor}}) %>% 
    mutate(categoria := as.character({{categoria}}),
           valor := as.numeric({{valor}}),
           categoria := fct_reorder({{categoria}}, {{valor}}),
           fill := ifelse(valor == max({{valor}}), "Dark", "Light"))
  
  no_y_grid_plot <- ggplot(data, aes(valor, categoria)) +
    geom_col(aes(x = valor, y=categoria, fill  = fill))+
    theme_minimal(base_size = 14)+
    geom_text(
      data = data,
      mapping = aes(x = valor, y = categoria, label = valor),
      hjust = 1,
      nudge_x = -0.1,
      color = 'white',
      fontface = 'bold',
      size = 4.5
    ) +
    scale_fill_manual(values = c('#008000', '#A2AD9C'))+
    geom_text(
      data = data,
      mapping = aes(x = 0, y = categoria, label = categoria),
      hjust = 0,
      nudge_x = 0.25,
      color = 'white',
      fontface = 'bold',
      size = 4.5
    )+
    # geom_vline(xintercept=40,col = "red", lty=2) +
    # geom_vline(xintercept = 0) +
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(breaks = NULL) +
    labs(x = element_blank(), y = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), legend.position = "none")
  no_y_grid_plot
}

box_title_js <- '
  Shiny.addCustomMessageHandler("box_title", function(title) {
  if(title.includes("mpg")){
    colour = "red"
  } else {
    colour = "blue"
  }
    $("#box_plot h3").html(title)
    $("#box_plot .card-body").css("background-color", colour)
  });
'


initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#228B22', 'color': '#fff'});",
  "$(this.api().table().body()).css({'font-weight': 'normal'});",
  "}"
) 
}########### FUNCTIONS
{
N <- 20

x <- cumsum(rnorm(N)) + 0.5 * cumsum(runif(N))
x <- round(200*x)

df <- data.frame(x = sort(as.Date(Sys.time() - lubridate::days(1:N))), y = abs(x))

hc_theme_sparkline_vb <- function(...) {
 theme <- list(chart = list( backgroundColor = NULL,margins = c(0, 0, 0, 0),spacingTop = 0,spacingRight = 0, spacingBottom = 0, spacingLeft = 0, plotBorderWidth = 0, borderWidth = 0, style = list(overflow = "visible")),
    xAxis = list(visible = FALSE, endOnTick = FALSE, startOnTick = FALSE),
    yAxis = list(visible = FALSE, endOnTick = FALSE,  startOnTick = FALSE),
    tooltip = list(outside = FALSE, shadow = FALSE, borderColor = "transparent", botderWidth = 0, backgroundColor = "transparent", style = list(textOutline = "5px white")),
    plotOptions = list(series = list(marker = list(enabled = FALSE),
        lineWidth = 2, shadow = FALSE, fillOpacity = 0.25, color = "#FFFFFFBF",
        fillColor = list(linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0), stops = list(list(0.00, "#FFFFFF00"), list(0.50, "#FFFFFF7F"), list(1.00, "#FFFFFFFF"))))),
    credits = list(enabled = FALSE, text = ""))
  
  theme <- structure(theme, class = "hc_theme")
  if (length(list(...)) > 0) {theme <- hc_theme_merge(theme, hc_theme(...))}
  theme
}

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, icon = NULL, color = "aqua", width = 3, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-sm icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

hc <- hchart(df, "area", hcaes(x, y), name = "Participantes")  %>% hc_size(height = 20) %>% hc_credits(enabled = TRUE) %>% hc_add_theme(hc_theme_sparkline_vb()) 
hc2 <- hchart(df, "line", hcaes(x, y), name = "Beneficiários")  %>% hc_size(height = 20) %>% hc_credits(enabled = FALSE) %>% hc_add_theme(hc_theme_sparkline_vb()) 
hc3 <- hchart(df, "column", hcaes(x, y), name = "Agregados familiares")  %>% hc_size(height = 20) %>% hc_credits(enabled = FALSE) %>% hc_add_theme(hc_theme_sparkline_vb()) 


} ############ VALUE BOX FUNCTIONS

ui <- dashboardPage(
  dashboardHeader(title =  title, rightUi = userOutput("user_names")),
  dashboardSidebar(disable = FALSE, minified = F, collapsed = TRUE, sidebarMenu(
    menuItem("ASSISTÊNCIA", startExpanded = FALSE, icon = icon("motorcycle"),
             menuSubItem("Famílias", tabName = "farfp_outreach", icon = icon("arrows-down-to-people")),
             menuSubItem("Grupos", tabName = "op_msme", icon = icon("users-between-lines"))
             ),
    menuItem("INVESTIMENTOS", startExpanded = FALSE, icon = icon("industry"),
             menuSubItem("Infraestruturas", tabName = "technicians_trainings", icon = icon("warehouse")),
             menuSubItem("Equipamentos", tabName = "beneficiaries_trainings", icon = icon("tractor"))
                                 ))),
  dashboardBody(tags$head(tags$style("#test .modal-dialog {width: fit-content !important;}")),

    tabItems(
      tabItem("farfp_outreach", 
              fluidRow(
                valueBoxOutput("vbox", width = 2),
                valueBoxOutput("vbox2", width = 2),
                valueBoxOutput("vbox3", width = 2),
                valueBoxOutput("vbox4", width = 2),
                valueBoxOutput("vbox5", width = 2),
                valueBoxOutput("vbox6", width = 2)
                ),
              
              fluidRow(
                box(title = "PRODUTORES CAPACITADOS POR TÉCNICO", closable = TRUE, maximizable = TRUE, width = 6, 
                    height = "720px",  solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    plotOutput("familias_assistidas", height=700)),

                box(title = "PRODUTORES CAPACITADOS POR DISTRITO", closable = TRUE, maximizable = TRUE, width = 6, 
                    height = "720px",  solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    plotOutput("participantes", height=700)),
                
                box(title = "ANÁLISE DETALHADA DOS DADOS", closable = TRUE, maximizable = TRUE, width = 12, collapsed = TRUE,
                    height = "720px",  solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    DTOutput("analise_detalhada"))

                 )
              )
  
  )),
  
  footer = bs4DashFooter(left = a(href = "www.farfp.com", target = "_blank", "Direitos reservados ao FAR-FP"), right = paste0("Actualização: ", format(Sys.Date(), "%b"), " ", format(Sys.Date(), "%Y"))),
  controlbar = dashboardControlbar(id = "controlbar", collapsed = TRUE, overlay = TRUE,
                                   selectizeInput("selected_province", label = h5("Províncias"), c("Todas", provincias), selected = "Todas"),
                                   selectizeInput("selected_intervention", "Intervenções", c("Estudo de base" = "baseline", "Selecção  de beneficiarios" = "targeting", "Assistência" = "assistence", "Insumos" = "inputs", "Equipment" = "Equipamento")),
                                   selectizeInput("intervencao_selecionada", "Intervenções", choices = c("Selecionar"), selected = 'Todas')
                                   )
)

server <- function(input, output, session) {
  
  admin_ids <- reactive({
    if(input$selected_province != "Todas"){beneficiarios <- fsubset(beneficiarios, provincia == input$selected_province)}
    beneficiarios
  })
  
  
  output$user_names <- renderUser({
    dashboardUser(
      name = "PROCAVA",
      image = user_image, 
      title = "Oficial de M&A",
      subtitle = NULL,
      footer = NULL
    )
  })

  output$vbox <- renderValueBox({
    
    valor <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT people_id) FROM trained.pessoas T LEFT JOIN trained.eventos S ON S.id = T.session_id WHERE date_started between '2024-01-01' AND CURRENT_DATE")
    valor_24 <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT people_id) FROM trained.pessoas T LEFT JOIN trained.eventos S ON S.id = T.session_id WHERE date_started BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()")

    vb <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
      title = toupper("Famílias capacitadas ou assistidas"), sparkobj = hc2,
      info = "Corresponde ao número de agregados familiares assistidos que são representados por mulheres",
      icon = icon("people-roof"), width = 2, color = "green", href = NULL)
    vb})

  
  output$vbox2 <- renderValueBox({
    
    valor <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT tecnico) FROM trained.eventos WHERE date_started between '2024-01-01' AND CURRENT_DATE")
    valor_24 <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT tecnico) FROM trained.eventos WHERE date_started BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()")
    vb <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
      title = toupper("TÉCNICOS ENVOLVIDOS"), sparkobj = hc2,
      info = "Corresponde ao número de agregados familiares assistidos que são representados por mulheres",
      icon = icon("person-biking"), width = 2, color = "orange", href = NULL)
    vb})
  
    
  output$vbox3 <- renderValueBox({
    valor <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT people_id) FROM trained.pessoas T LEFT JOIN trained.eventos E ON E.id = T.session_id LEFT JOIN familias.sexo S ON S.person_id = T.people_id WHERE sexo IN ('Mulher', 'Feminino', 'F', 'Mulheres') AND date_started between '2024-01-01' AND CURRENT_DATE")
    valor_24 <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT people_id) FROM trained.pessoas T LEFT JOIN trained.eventos E ON E.id = T.session_id LEFT JOIN familias.sexo S ON S.person_id = T.people_id WHERE sexo IN ('Mulher', 'Feminino', 'F', 'Mulheres') AND date_started BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()")
    vb3 <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"), valor_24, " nas últimas 24 horas"),
      
                         title = toupper("Mulheres capacitadas"), sparkobj = hc3,
                         info = "Corresponde ao número de agregados familiares assistidos que são representados por mulheres",
                         icon = icon("venus"), width = 2, color = "yellow", href = NULL)
    vb3})
  
  output$vbox4 <- renderValueBox({
     
    valor <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT people_id) FROM trained.pessoas T LEFT JOIN trained.eventos S ON S.id = T.session_id LEFT JOIN familias.idades I ON I.person_id = T.people_id WHERE ano > 1989 AND date_started between '2024-01-01' AND CURRENT_DATE")
    valor_24 <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT people_id) FROM trained.pessoas T LEFT JOIN trained.eventos S ON S.id = T.session_id LEFT JOIN familias.idades I ON I.person_id = T.people_id WHERE ano > 1989 AND date_started BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()")
    
    vb4 <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
      title = toupper("Jovens capacitados"), sparkobj = hc3,
      info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
      icon = icon("children"), width = 2, color = "purple", href = NULL)
    
    vb4})
  
  
  output$vbox5 <- renderValueBox({
    
    valor <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT P.people_id|| member) FROM trained.pessoas T LEFT JOIN familias.pcd P ON P.people_id = T.people_id LEFT JOIN trained.eventos E ON E.id = T.session_id WHERE date_started between '2024-01-01' AND CURRENT_DATE")
    valor_24 <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT P.people_id|| member) FROM trained.pessoas T LEFT JOIN familias.pcd P ON P.people_id = T.people_id LEFT JOIN trained.eventos E ON E.id = T.session_id WHERE  date_started BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()")
    
    vb5 <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
        
                         title = toupper("Agentes comunais (PAC, PCC, VCO, PRON, DIF)"),
                         sparkobj = hc3,
                         # subtitle = tagList(HTML("&uarr;"), valor_24, " nas últimas 24 horas"),
                         info =     tags$span(tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "message")),
                         icon = icon("person-chalkboard"),
                         width = 2,
                         color = "fuchsia",
                         href = NULL)
  vb5})
  
  output$vbox6 <- renderValueBox({
    valor <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT P.people_id|| member) FROM trained.pessoas T LEFT JOIN familias.pcd P ON P.people_id = T.people_id LEFT JOIN trained.eventos E ON E.id = T.session_id WHERE date_started between '2024-01-01' AND CURRENT_DATE")
    valor_24 <- DBI::dbGetQuery(far_pool, "SELECT COUNT( DISTINCT P.people_id|| member) FROM trained.pessoas T LEFT JOIN familias.pcd P ON P.people_id = T.people_id LEFT JOIN trained.eventos E ON E.id = T.session_id WHERE  date_started BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()")
    
    vb6 <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
      
      title = toupper("Pessoas com Deficiência"),
      sparkobj = hc3,
      info = tags$span("Fixed ratio", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "message")),
      icon = icon("wheelchair"),
      width = 2, color = "olive", href = NULL)

    vb6})

  output$familias_assistidas <- renderPlot({
    
    
    agregados_familiares <- DBI::dbGetQuery(far_pool, "SELECT nome, count(distinct people_id) AS agregados 
                                            FROM trained.pessoas B 
                                            LEFT JOIN trained.eventos E ON E.id = B.session_id 
                                            LEFT JOIN pessoal.extensao X ON X.nuit::numeric = E.tecnico::numeric
                                            WHERE nome NOTNULL
                                            GROUP BY nome ORDER BY count(distinct people_id) DESC LIMIT 15") %>% 
      mutate(
        color = case_when(
          row_number() == 1 ~ "#008000",
          row_number() == 2 ~ "#32CD32",
          row_number() == 3 ~ "#7CFC00",
          row_number() >= 9 ~ "#FFC300",
          TRUE ~ "gray70"))

    agregados_familiares %>%  mutate(nome = fct_reorder(nome, agregados)) %>% 
      ggplot(aes(x = nome, y = agregados, fill = color)) +
      geom_col() +
      coord_flip()+
      geom_text(aes(label = agregados),  size = 8, hjust = 1.5,  vjust = 0.5) +
      scale_fill_identity(guide = "none") +
      theme_void()+
      theme(axis.text.y = element_text(size = 15, angle = 0, vjust=0, hjust=0))
  })
  
  output$participantes <- renderPlot({
    agregados_familiares <- DBI::dbGetQuery(far_pool, "SELECT distrito, count(distinct T.people_id) AS agregados 
                                            FROM trained.pessoas T LEFT JOIN familias.pessoas P ON P.id = T.people_id 
                                            LEFT JOIN moz.localidades L ON L.id = P.admin_id 
                                            GROUP BY distrito 
                                            ORDER BY count(distinct T.people_id) DESC LIMIT 15") %>% 
      mutate(
        color = case_when(
          row_number() == 1 ~ "#008000",
          row_number() == 2 ~ "#32CD32",
          row_number() == 3 ~ "#7CFC00",
          row_number() >= 9 ~ "#FFC300",
          TRUE ~ "gray70"))
    
    agregados_familiares %>% mutate(distrito = fct_reorder(distrito, agregados)) %>% 
      ggplot(aes(x = distrito, y = agregados, fill = color)) +
      geom_col() +
      coord_flip()+
      geom_text(aes(label = agregados),  size = 8, hjust = 1.5,  vjust = 0.5) +
      scale_fill_identity(guide = "none") +
      theme_void()+
      theme(axis.text.y = element_text(size = 15, angle = 0, vjust=0, hjust=0))
  })
  
  
  output$analise_detalhada <- renderDT({
    
    agregados_familiares <- DBI::dbGetQuery(far_pool, "SELECT topico, faixa_sexo, count(DISTINCT r.people_id) AS treinados FROM (SELECT provincia, posto, L.distrito, localidade, S.sexo,
    CASE WHEN ano ISNULL THEN 'Sem idade' WHEN ano > EXTRACT('YEAR' FROM CURRENT_DATE)-35 THEN 'Jovens' ELSE 'Adultos' END AS faixa_etaria,
    COALESCE(S.sexo || ' ' || CASE WHEN ano ISNULL THEN 'Sem idade' WHEN ano > EXTRACT('YEAR' FROM CURRENT_DATE)-35 THEN 'Jovens' ELSE 'Adultos' END, 'Outros') AS faixa_sexo,
    T.people_id, topic AS topico, COALESCE(X.nome, 'Não cadastrado') AS responsavel
    FROM trained.pessoas T  
    LEFT JOIN familias.pessoas P ON P.id = T.people_id 
    LEFT JOIN familias.sexo S ON S.person_id = P.id 
    LEFT JOIN familias.idades I ON I.person_id = P.id
    LEFT JOIN trained.eventos E ON E.id = T.session_id 
    LEFT JOIN pessoal.pessoal X ON X.nuit::numeric = E.tecnico::numeric 
    LEFT JOIN moz.localidades L ON L.id = P.admin_id
    LEFT JOIN moz.distritos D ON D.id = L.distrito
    LEFT JOIN moz.postos K ON K.id = L.posto_id
    LEFT JOIN trained.topic_codes AS tc ON tc.id = E.topic_id) r GROUP BY topico, faixa_sexo")
    
    agregados_familiares <- agregados_familiares %>% 
      pivot_wider(names_from = "faixa_sexo", values_from = "treinados") %>% 
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
      mutate_all(as.character)
    
    agregados_familiares[,2:ncol(agregados_familiares)] <- sapply(agregados_familiares[,2:ncol(agregados_familiares)],as.numeric)
    
    agregados_familiares <- agregados_familiares %>% adorn_totals("col")
    
    datatable(agregados_familiares, rownames= FALSE, options = list(pageLength = 10, initComplete = initComplete))
  })

}

shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
