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



clustering_tisefka_mds <- function(tsclust_results = NULL){

  if(class(tsclust_results$tisefka_clust)=="PartitionalTSClusters"){
    ts_clust_mds = MASS::isoMDS(tsclust_results$tisefka_clust@distmat)$points
    colnames(ts_clust_mds) <- c("dist_x","dist_y")

    ts_clust_mds <- data.frame(ts_clust_mds,cluster = tsclust_results$tisefka_clust@cluster,ts_names = rownames(ts_clust_mds),check.names = FALSE)
    ts_clust_mds <- crosstalk::SharedData$new(ts_clust_mds)
  }
  return(ts_clust_mds)
}



#' Saldae Clustering Time series UI module
#' @description UI module for time series clustering
#' @author Farid Azouaou
#' @param id session ID
#' @return t.b.d
#' @export

SA_clustering_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width =2,
             uiOutput(ns("number_clusters")))
    ),
    fluidRow(
      column(5, d3scatter::d3scatterOutput(ns("clust_mds"))),
      column(7, plotOutput(ns("by_clusters")))
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
SA_clustering_mod <- function(input, output, session,tisefka) {
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

  ts_clust_mds <- reactive({clustering_tisefka_mds(tsclust_results = clust_results())})

  output$clust_mds <- d3scatter::renderD3scatter({
    req(ts_clust_mds())
    d3scatter::d3scatter(ts_clust_mds(), ~dist_x, ~dist_y,~factor(cluster), width="100%", height=250)
  })



  df_mds <- shiny::debounce({reactive({
    req(ts_clust_mds())
    ts_clust_mds()$data(withSelection = TRUE)})})

  output$by_clusters <- renderPlot({
    req(df_mds())
    if(!"selected_"%in%colnames(df_mds()))return(NULL)
    selected_variables<- df_mds()%>%dplyr::filter(selected_==TRUE)
    selected_variables<- paste(selected_variables$ts_names)
    my_clust_plot<-clust_results()$tisefka_origin%>%dplyr::select(c("date",selected_variables))%>%
      tidyr::gather("ts_name","value" ,-date)
    ggplot2::ggplot(data = my_clust_plot,mapping= ggplot2::aes(x = date,y = value,group=ts_name,colour=ts_name))+ggplot2::geom_line()
  })
  output$output_results<-reactive({
    return(clust_results)
  })
}




