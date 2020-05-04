
#' Plot heatmap with plotly.
#'
#' \code{heatmap_plotly} plots heatmap with plotly.
#'
#' This is a function to plot heatmap using plotly package. Sensible defaults
#' are supplied to make the plot better conform with desired appearance.
#'
#' @param df A data.frame.
#' @param x_var A character. Character of the column to be used for x axis.
#' @param y_var A character. Character of the column to be used for y axis.
#' @param z_var A character. Character of the column to be used for color axis.
#'
#' @return A plotly object of heatmap type.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly layout
#'
#' @examples
#' \dontrun{
#' heatmap_plotly(df, "x_column", "y_column", "z_column")
#' }
#'
heatmap_plotly <-
  function(df,
           x_var,
           y_var,
           z_var)
  {
    p_heatmap <-
      df %>%
      plot_ly(type = "heatmap",
              x = ~get(x_var), # or eval(parse(text = x_var))
              y = ~get(y_var),
              z = ~get(z_var),
              colorbar=list(len = 0.25,
                            outlinewidth = 0,
                            title = list(text = z_var))) %>%
      layout(yaxis = list(title = y_var,
                          autorange = "reversed"),
             xaxis = list(title = x_var))

    return(p_heatmap)
  }
