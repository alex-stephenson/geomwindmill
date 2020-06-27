#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

stat_windmill <- function(mapping = NULL, data = NULL, geom = "polygon",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, span_x = 1, ...) {
  ggplot2::layer(
    stat = StatWindmill, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, span_x = span_x, ...)
  )
}

StatWindmill <- ggplot2::ggproto("StatWindmill", ggplot2::Stat,
                        compute_group = function(data, scales, span_x = 1) {
                          blade_frame <- data.frame(
                            x_map=c(0.15,0.85,0.95,0.95,0.5,0.05,0.05),
                            y = c(0,0,0.45,0.8,1,0.8,0.45)
                          )
                          new_x <- (data$x - span_x/2) + (span_x * blade_frame$x_map)
                          new_y <- data$y * blade_frame$y
                          new_blade <- data.frame(x=new_x, y=new_y)
                          new_blade

                        },

                        required_aes = c("x", "y")
)

geom_windmill <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          rule = "evenodd",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatWindmill,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rule = rule,
      ...
    )
  )
}



