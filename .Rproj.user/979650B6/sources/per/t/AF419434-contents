#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : data upload
#' @author Farid Azouaou
#' @param id  server module ID
#' @export
ghred_tisefka_UI <- function(id){
  ns <- NS(id)
fluidPage(
  fluidRow(
    column(width = 3,
           fileInput(inputId = ns("tisefka_file"), label = "Choose CSV File",
                     multiple = FALSE,
                     accept = c("csv")
           )),
    column(width= 3,
           shinyWidgets::radioGroupButtons(
             inputId = ns("tisefka_tala"),
             label = "Data Source :",
             choices = c(
               `<i class="fas fa-table"></i>` = "table", `<i class="fas fa-database"></i>` = "database"             ),
             status = "success",
             justified = TRUE,
             selected = "CSV"
           )
    ),
    column(width = 3, uiOutput(ns("excel_tawriqt")))
  ),
  #-----------date related settings
  #---- Help text
  uiOutput(ns("date_variable_help")),

  fluidRow(column(width = 3,
                  uiOutput(ns("date_variable"))),
           column(width = 3,
                  uiOutput(ns("SA_date_format")))
           ),
  #-----------Spread Data
  uiOutput(ns("duplicated_date_warning")),


  fluidRow(
           column(width = 3,
                  uiOutput(ns("handle_duplicate"))),
           column(width = 3,
                  uiOutput(ns("aggregate_param1"))),
           column(width = 3,
                  uiOutput(ns("tisefka_spread_var")))
  ),


  #---- Data Overview
  div(class = "col-xs-12 col-sm-12 col-md-12",
      shinydashboard::tabBox(width = 12, title = "Data Diagnosis",
                             tabPanel(title = "Overview",icon = icon("eye"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_view"))
                             ),
                             tabPanel(title = "Description",icon = icon("table"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_description"))
                             ),
                             tabPanel(title = "Outliers",icon = icon("table"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_outliers"))
                             )
      )
  )
)

}

#' Saldae dashboard module: upload data
#' @description upload rwa data and prepare it to be used for exploration and analysis
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use data upload
#' @param output output shinydashboard element
#' @param session shiny session
#' @return output objects to be displayed in corresponding UI module
#' @export

ghred_tisefka_mod <-function(input, output, session){

  file_tasetta <- reactive({
    req(input$tisefka_file)
    tools::file_ext(input$tisefka_file$name)
  })
  excel_tiwriqin <- reactive({
   req(file_tasetta())
   if(file_tasetta()=="csv")return(NULL)
   if(file_tasetta()=="xlsx"){
     readxl::excel_sheets(input$tisefka_file$datapath)
   }
  })
  output$excel_tawriqt <- renderUI({
    req(excel_tiwriqin())
    if(is.null(excel_tiwriqin()))return(NULL)
    shinyWidgets::pickerInput(
      inputId = session$ns("excel_tawriqt"),
      label = "Choose excel sheet:",
      choices = excel_tiwriqin(),
      options = list(
        style = "btn-primary"
      )
    )
  })

  tisefka <- reactive({
    req(file_tasetta())
    SaldaeDataExplorer::ghred_tisefka_aqerru(input_file = input$tisefka_file, tala = file_tasetta(), tawriqt = input$excel_tawriqt)
  })
  output$date_variable_help <- renderUI({
    req(tisefka())
    my_help_text <- h5("Data Successfully uploaded, you need to specify the variable to use for time (Date)", style = "color:green")
    helpText(my_help_text)
  })
  #------- select date variable
  output$date_variable <- renderUI({
    req(tisefka())
    dates_yellan <- colnames(tisefka())
    # dates_yellan <- text_similarity_f(target_text = dates_yellan, text_benchmark = "date")
    shinyWidgets::pickerInput(
      inputId = session$ns("date_variable"),
      label = "Choose Date variable:",
      choices = dates_yellan,
      options = list(
        style = "btn-primary"
      )
    )
  })
  output$SA_date_format <- renderUI({
    req(input$date_variable)
    shinyWidgets::pickerInput(
      inputId = session$ns("SA_date_format"),
      label = "Choose Date format:",
      choices = SaldaeDataExplorer::SA_date_format_yellan(),
      options = list(
        style = "btn-primary"
      )
    )
  })

  #---------- in case of duplicated dates (grouping is required)--------


  duplicates_yellan <- reactive({
    req(input$date_variable)
    # TRUE%in%(economics_long[,"date"]%>%base::duplicated())
    d_ukud <- SaldaeDataExplorer::IsDate(mydate = tisefka()[1,input$date_variable],SA_date_format = input$SA_date_format)
    if(d_ukud==FALSE)return(FALSE)
    TRUE%in%(tisefka()[,input$date_variable]%>%base::duplicated())
  })
  output$duplicated_date_warning <- renderUI({
    req(duplicates_yellan())
    if(duplicates_yellan()==FALSE)return(NULL)
    my_help_text <- h5("There are duplicated dates, please aggregate or spread your data", style = "color:orange")
    helpText(my_help_text)
  })

output$handle_duplicate <- renderUI({
    req(duplicates_yellan())
    handle_duplicate_choices <-c("Remove","Aggregate","Spread")
    if(duplicates_yellan()==FALSE)return(NULL)
    shinyWidgets::pickerInput(
      inputId = session$ns("handle_duplicate"),
      label = "Handle Duplicates",
      choices = handle_duplicate_choices,
      multiple = FALSE,
      options = list(
        style = "btn-primary"
      )
    )
})

output$aggregate_param1 <- renderUI({
  req(input$handle_duplicate)
  if(input$handle_duplicate=="Remove"){return(NULL)}
  else if(input$handle_duplicate=="Aggregate"){
    aggregation_metric <-c("Sum","Average","Maximum","Minimum","Median")
    shinyWidgets::pickerInput(
      inputId = session$ns("aggregate_duplicates"),
      label = "Aggregation Metric",
      choices = aggregation_metric,
      multiple = FALSE,
      options = list(
        style = "btn-primary"
      )
    )
  }else if(input$handle_duplicate=="Spread"){
    req(spread_yellan())
    if(is.null(spread_yellan()))return(NULL)
    shinyWidgets::pickerInput(
      inputId = session$ns("tisefka_spread"),
      label = "Spread Data",
      choices = spread_yellan(),
      multiple = TRUE,
      options = list(
        style = "btn-primary"
      )
    )
  }

})


#----------duplicates : spread
spread_yellan <- reactive({
    SaldaeDataExplorer::tisefka_spread_yella(tisefka = tisefka(), date_variable = input$date_variable)
})


output$tisefka_spread_var <- renderUI({
    req(input$handle_duplicate)
    if(input$handle_duplicate!="Spread")return(NULL)
    if(is.null(spread_yellan()))return(NULL)
    spread_value <- colnames(tisefka())
    spread_value <- spread_value[spread_value != input$date_variable]
    spread_value <- spread_value[spread_value != input$tisefka_spread]
    shinyWidgets::pickerInput(
      inputId = session$ns("tisefka_spread_var"),
      label = "spread value",
      choices = spread_value,
      multiple = TRUE,
      options = list(
        style = "btn-primary"
      )
    )
  })


tisefka_tizegzawin <- reactive({
    req(input$SA_date_format)
    aggregation_metric <- spread_key<-spread_value <- NULL
    handle_duplicate <-input$handle_duplicate
    if(is.null(handle_duplicate))handle_duplicate<-"No_duplicated_dates"
    if(handle_duplicate=="Spread"){
      spread_key    <-  input$tisefka_spread
      spread_value  <-  input$tisefka_spread_var
    }else if(handle_duplicate=="Aggregate"){
      aggregation_metric  <-  input$aggregate_duplicates
    }
    SaldaeDataExplorer::sbed_tisefka(tisefka = tisefka(), date_variable = input$date_variable, SA_date_format = input$SA_date_format, spread_key = spread_key,aggregation_metric = aggregation_metric, spread_value = spread_value)
})

#--------- display raw data
data_summary <- reactive({
  req(tisefka_tizegzawin())
    diag_output <- SaldaeDataExplorer::data_diagnosis_f(tisefka = tisefka_tizegzawin(), categoricals_ukud = NULL)
    return(diag_output)
})

numeric_variables <- reactive({
    req(data_summary())
    dat_diag <- data_summary()$diagnosis
    numericals <- dat_diag[grepl("numeric", dat_diag$types), "variables", drop = T]
    multi_integers <- dat_diag[grepl("integer", dat_diag$types), c("unique_count", "variables"), drop = T]
    if (nrow(multi_integers)) {
      multi_integers <- multi_integers[multi_integers["unique_count"] > 10, "variables", drop = T]
    } else {
      multi_integers <- NULL
    }
    return(c(numericals, multi_integers))
})

tisefka_overview <- reactive({
  req(data_summary())
  SaldaeDataExplorer::Handson_exploration(tisefka = tisefka_tizegzawin(), tisefka_report = data_summary(),numeric_variables = numeric_variables())
})

#-------------------------

output$tisefka_description <- rhandsontable::renderRHandsontable({
  req(data_summary())
  return(rhandsontable::rhandsontable(data_summary()$beschreibung, rowHeaders = NULL, width = 1000, height = 300))
})

output$tisefka_outliers <- rhandsontable::renderRHandsontable({
  req(tisefka_overview())
  return(rhandsontable::rhandsontable(data_summary()$outliers, rowHeaders = NULL, width = 1000, height = 300))
})
output$tisefka_view <- rhandsontable::renderRHandsontable({
  req(tisefka_overview())
  return(tisefka_overview())
})
ts_time_units <- reactive({
  return(SaldaeDataExplorer::possible_units_for_summary(time_vect = tisefka_tizegzawin()$date))
})

explore_output <- reactive({
  req(tisefka_overview())
  output <- list()
  output$tisefka_tizegzawin <- dplyr::tbl_df(tisefka_tizegzawin())
  output$tisefka_overview <- tisefka_overview()
  output$numeric_variables <- numeric_variables()
  output$ts_time_units <- ts_time_units()
  return(output)
})

}
