
#' Saldae Clustering CrossTalk
#' @description UI crosstalk view
#' @author Farid Azouaou
#' @param tisefka clustering results
#' @param mds_CT MDS crosstalk ShareData object
#' @return t.b.d
#' @export

clustering_sekened_crosstalk <- function(mds_CT= NULL,tisefka= NULL){
  df_mds <- mds_CT$data(withSelection = TRUE, withFilter = TRUE)
  selected_variables<- df_mds%>%dplyr::filter(selected_==TRUE)
  selected_variables<- paste(selected_variables$ts_names)
  my_clust_plot<-tisefka%>%dplyr::select(c("date",selected_variables))%>%
    tidyr::gather("ts_name","value" ,-date)
  ggplot2::ggplot(data = my_clust_plot,mapping= ggplot2::aes(x = date,y = value,group=ts_name,colour=ts_name))+ggplot2::geom_line()
}



#' Saldae Clustering MDS 2D representation
#' @description Kruskal's Non-metric Multidimensional Scaling
#' @author Farid Azouaou
#' @param tsclust_results clustering results
#' @return t.b.d
#' @export
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
      column(5, d3scatter::d3scatterOutput(ns("clust_mds"))),
      column(7, plotOutput(ns("by_clusters")))
    )
  )
}


#' Saldae Clustering Time series SERVER module
#' @description SERVER module for time series clustering
#' @author Farid Azouaou
#' @param clust_results list containing clustering results
#' @param input
#' @param output
#' @param session
#' @return a list containing information about clustering results
#' @export
SA_clustering_mod <- function(input, output, session,clust_results) {
  mds_matrix <- reactive({clustering_tisefka_mds(tsclust_results = clust_results())})
  output$clust_mds <- d3scatter::renderD3scatter({
    req(mds_matrix())
    d3scatter::d3scatter(mds_matrix(), ~dist_x, ~dist_y,~factor(cluster), width="100%", height=250)
  })
}




