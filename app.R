library(tidyverse)
library(shiny)
require(htmltools)
library(plotly)
require(DT)

ui <- fluidPage(includeCSS("www/style.css"),
                navbarPage("Rekap Kusapa",
                           tabPanel("Home",
                                    tags$div(
                                      column(class = "col-lg-6 col-md-12 col-sm-12",
                                             tags$h3("Jenis Kajian"),
                                             width = 6,dataTableOutput("table1")),
                                      column(class = "col-lg-6 col-md-12 col-sm-12", 
                                             tags$h3("Petugas"),
                                             width = 6,dataTableOutput("table2")),
                                    ),
                                    tags$div(
                                      column(class = "col-lg-4 col-md-12 col-sm-12",
                                             tags$h4("Popular Times"),
                                             uiOutput('select1'),
                                             plotlyOutput("graph2"), width = 4),
                                      column(class = "col-lg-8 col-md-12 col-sm-12",
                                             tags$h3("Jumlah Konsultasi per Bulan"),
                                             plotlyOutput("graph1"), width = 8),
                                    )),
                           tabPanel("Dataset",
                                    tags$div(
                                      column(width = 6,dataTableOutput("table3")),
                                      column(width = 6,
                                             uiOutput("text3"),
                                             tags$div(style = "overflow-y:scroll; max-height: 80rem",uiOutput("information_1")
                                             ))
                                    ))
                           )
                )

server <- function(input, output){
  #==Streams data====
#  chat <- eventReactive(input$update,{as_tibble(read.table("data/chat.txt", sep = ",", header = TRUE))},ignoreNULL = FALSE)
#  chat_fix <- eventReactive(input$update,{
#    chat() %>% filter(!is.na(internal_id_1) & is.na(internal_id_2)) %>%
#      select(chat_id,time_stamp, fullname, internal_id_1, petugas_1, group_id) %>%
#      rename(internal_id = internal_id_1, petugas = petugas_1)
#  },ignoreNULL = FALSE)
#  output$table_chat <- renderDataTable(datatable(chat_fix(), selection = "none", extensions = "Responsive"), server = TRUE)
  #=== Streams data2====
  load("chat.rda")
  chat <- chat %>% drop_na(employee)
#  chat_data <- eventReactive(input$update,{
#    readxl::read_xlsx("data/chat.xlsx") %>% filter(!is.na(employee)) %>%
#      mutate(time = as.POSIXct(format(time, tz = "Asia/Bangkok")),
#             endtime = as.POSIXct(format(endtime, tz = "Asia/Bangkok")),
#             closed = as.POSIXct(format(closed, tz = "Asia/Bangkok")))
#  },ignoreNULL = FALSE)
  

  #=== Per Jenis Konsultasi ====
  output$table1 <- renderDataTable(datatable(chat %>% filter(!is.na(group_id)) %>% group_by(group_id) %>% 
                                               summarise(N = n(), .groups = 'drop') %>%
                                               arrange(desc(N)), 
                                   selection = "none", extensions = "Responsive", options = list(scrollY = "250px")))
  
  #==== Per Petugas ====
  output$table2 <- renderDataTable(datatable(chat %>% filter(!is.na(employee)) %>% 
                                               group_by(employee) %>% summarise(N = n(), .groups = 'drop') %>%
                                               arrange(desc(N)), 
                                   selection = "none", extensions = "Responsive", options = list(scrollY = "250px")))
  #=== Ubah Tanggal ke bulan ====
  output$graph1 <- renderPlotly(
    ggplotly(ggplot(chat %>% dplyr::group_by(format(time, "%Y-%m")) %>% 
                      summarise(N = n(), .groups = 'drop') %>%
                      rename(month = `format(time, "%Y-%m")`) %>% 
                      arrange(month), aes(x=month, y=N, group=1)) + 
               geom_point(stat='summary', fun=sum) +
               stat_summary(fun=sum, geom="line") +
               theme_classic())
  )
  #=== Ubah Tanggal ke Hari =====
  #format(chat_data$time[1], '%u-%A')
  #(60*as.numeric(format(chat_data$time[1], '%H')) + as.numeric(chat_data$endtime[1]-chat_data$time[1]))/60
  
  day_chat <- eventReactive(input$update,{
    chat %>% 
      mutate(day = format(time, '%u-%A'), 
             time_duration = floor((60*as.numeric(format(time, '%H')) + as.numeric(format(time, '%M')))/60))
  },ignoreNULL = FALSE)
  output$select1 <- renderUI({
    selectInput('days','Pilih Hari', choices = c("1-Monday","2-Tuesday","3-Wednesday","4-Thursday","5-Friday","6-Saturday","7-Sunday"), selected = "1-Monday", multiple = FALSE)
  })
  
  output$graph2 <- renderPlotly(
    ggplotly(ggplot(data = day_chat() %>% filter(day %in% input$days) %>% group_by(time_duration) %>% summarise(N = n(), .groups = 'drop'), aes(x = time_duration, y = N)) + 
               geom_bar(stat = 'identity') + xlim(0,23) + ylim(0,1500) + theme_classic()
    )
  )
  
  #==== Data Percakapan ====
  output$table3 <- renderDataTable(datatable(chat[c("time","employee","customer","group_id")] %>% 
                                                              mutate(time = format(as.POSIXct(time, tz = "Asia/Bangkok"), format = '%Y-%m-%d %H:%M:%S')),
                                                            extensions = 'Buttons', selection = "single",escape = FALSE, 
                                                            options = list(pageLength = 20, dom = 'Bfrtip', extend = "collection", buttons = 'excel'),
                                                            filter = list(position = 'top', clear = FALSE)))
  output$text3 <- renderUI(
    if(length(input$table3_rows_selected)){
      HTML("<h3>Topik Konsultasi</h3>")
    }
  )
  output$information_1 <- renderUI(
    if(length(input$table3_rows_selected)){
      HTML(paste(chat[input$table3_rows_selected,"plaintext"]))
    }
  )
}

shinyApp(ui,server)
