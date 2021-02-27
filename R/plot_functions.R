library(ggplot2)
library(magrittr)


#' @name ggimage
#' @param out Return of \link{ode.1D}
#' @param which_var which variable in \code{attributes(out)}
#' @param depth depth name for variable
#' @return A matrix
#' @importFrom stringr str_detect
#' @importFrom magrittr %>% %<>%
#' @importFrom stats setNames
#' @export
get_out <- function(out, which_var, depth = NULL) {

  atts <- as.list(attributes(out))
  N    <- atts$lengthvar[1] / atts$nspec

  if(is.null(depth)){
    depth_name <- 1:N
  } else {
    depth_name <- depth
  }

  col_ind <- pmatch(which_var, atts$ynames)

  if(is.na(col_ind)) {

    col_ind <- which(str_detect(colnames(out), which_var))
    dc = out[, c(1, col_ind)] %>%
      data.frame() %>%
      setNames(., c("times", depth_name))

  } else {

    dc = out[,-1][,((col_ind-1)*N+1):(col_ind*N)] %>%
      data.frame(times = out[,1], .) %>%
      setNames(., c("times", depth_name))

  }

  return(dc)

}

#' @name ggimage
#' @title plot and get result of run_model
#' @param trans Transformation of \link{scale_fill_gradientn}, like \code{"log10"}.
#' @param y.rev Should be y scale on the reverse scale?
#' @param contour.text Should contour text be printed?
#' @param sign_var Does variable have sign attribute?
#' @return ggplot list
#' @importFrom reshape2 melt
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 scale_y_continuous scale_y_reverse
#' @importFrom ggplot2 scale_x_continuous ggtitle
#' @importFrom ggplot2 ggplot aes geom_raster geom_contour labs theme_bw coord_cartesian
#' @importFrom metR geom_text_contour label_placement_flattest
#' @importFrom grDevices rainbow
#' @export
#'
ggimage <- function(out,
                    which_var = "Biomass",
                    trans = NULL, y.rev = TRUE,
                    contour.text = FALSE,
                    depth = NULL, sign_var = FALSE) {

  dc <- get_out(out, which_var, depth = depth)

  c_melt <- reshape2::melt(dc, id = "times",
                           variable.name = "depth",
                           value.name = "value")
  c_melt$depth %<>% as.vector() %>% as.numeric()

  if(is.null(trans)) {
    scale_tran <- scale_fill_gradientn(colours = rainbow(255, end = 0.7, rev = TRUE))
  } else {
    scale_tran <- scale_fill_gradientn(colours = rainbow(255, end = 0.7, rev = TRUE), trans = trans)
  }

  if(!y.rev) {
    scale_y <- scale_y_continuous()
  } else {
    scale_y <- scale_y_reverse()
  }

  if(!sign_var) {

    p <- ggplot(c_melt, aes(x=times, y=depth, fill = value, z= value)) +
      geom_raster() +
      geom_contour(color = "black", bins = 10) +
      scale_tran +
      scale_y +
      labs(fill = which_var) +
      theme_bw() +
      coord_cartesian(expand = FALSE)


    if(contour.text) {
      p <- p +  metR::geom_text_contour(size = 4, color = "black",
                                        label.placement = metR::label_placement_flattest())
    }

  } else {

    c_melt$value <- sign(c_melt$value)
    p <- ggplot(c_melt, aes(x=times, y=depth, fill = factor(value), z= value)) +
      geom_raster() +
      scale_y +
      labs(fill = which_var) +
      theme_bw() +
      coord_cartesian(expand = FALSE)
  }

  return(p)

}


#' @importFrom utils globalVariables
utils::globalVariables(c(".", "times", "value"))

