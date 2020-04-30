#' Saldae Clustering Time series
#' @description proceed to
#' @author Farid Azouaou
#' @param tisefka data containing a set time serie
#' @param
#' @return a list containing information about clustering results
#' @export

Saldae_cluserting_main <- function(tisefka = NULL,anomaly_detection= FALSE,num_clusters =  NULL,clust_distance = "dtw_basic" ,clust_algo = "partitional"){
  num_clusters<-as.numeric(num_clusters)
  start_time <- Sys.time()
  tisefka_date <- tisefka$date
  tisefka$date <- NULL
  tisefka <- purrr::map_df(.x= tisefka,~SaldaeDataExplorer::interp_na_value(ts_x = .x, interp_mode = "spline"))


  #. outliers correction
  if (anomaly_detection == TRUE) {
    tisefka <- SaldaeDataExplorer::anomaly_detection_nnegh(tisefka = tisefka, anomaly_mode = "anomalize", target_ts = target_variables)
  }

  tisefka <- purrr::map_df(.x= tisefka,~base::scale(x = .x))


  tisefka_clust <- dtwclust::tsclust(series = t(tisefka),type = clust_algo, k = num_clusters,distance =  clust_distance,centroid = "pam")

  clust_output <- list()
  clust_output[["tisefka_origin"]]<- tisefka%>%dplyr::mutate(date = tisefka_date)
  clust_output[["tisefka_clust"]] <- tisefka_clust

  end_time <- Sys.time()-start_time
  end_time
  return(clust_output)
}
# tisefka <- Foreign_Exchange_Rates
# tisefka$date <- tisefka$`Time Serie`
# tisefka$`Time Serie`<- NULL





#' Saldae Clustering Time series UI module
#' @description UI module for time series clustering
#' @author Farid Azouaou
#' @param id session ID
#' @return t.b.d
#' @export

SA_clustering_core_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width =2,
             uiOutput(ns("number_clusters")))
    )
  )
}


#' Saldae Clustering Time series SERVER module
#' @description SERVER module for time series clustering
#' @author Farid Azouaou
#' @param tisefka data containing a set time serie
#' @param input
#' @param output
#' @param session
#' @return a list containing information about clustering results
#' @export
SA_clustering_core_mod <- function(input, output, session,tisefka) {
  output$number_clusters <- renderUI({
    clusters_choices <- c(2:12)
    shinyWidgets::pickerInput(inputId = session$ns("number_clusters"),
                              choices = clusters_choices,
                              selected = 5)
  })
  tisefka_inu <- reactive({
    tisefka()$tisefka_tizegzawin
  })
 clust_results <- reactive({
    req(tisefka_inu())
    req(input$number_clusters)
    tisefka_clust<-tail(tisefka_inu(),700)
    print("start clustering")
    Saldae_cluserting_main(tisefka = tisefka_clust,num_clusters = input$number_clusters)
  })
}




