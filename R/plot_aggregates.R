
#' Plot heatmap with plotly.
#'
#' \code{heatmap_plotly} plots heatmap with plotly.
#'
#' This is a function to plot heatmap using plotly package. Sensible defaults
#' are supplied to make the plot better conform with desired appearance.
#'
#' @param df A data.frame.
#' @param x A character. Character of the column to be used for x axis.
#' @param y A character. Character of the column to be used for y axis.
#' @param z A character. Character of the column to be used for color axis.
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
  function(df, x, y, z) {
    p <-
      plot_ly(type = "heatmap",
              x = df[[x]], # or eval(parse(text = x))
              y = df[[y]],
              z = df[[z]],
              colorbar=list(len = 0.25,
                            outlinewidth = 0,
                            title = list(text = z))) %>%
      layout(yaxis = list(title = y,
                          autorange = "reversed"),
             xaxis = list(title = x))
    return(p)
  }




#' Plot multi-trace subplots with plotly.
#'
#' \code{multi_trace_plotly} plots multi-trace subplots with plotly.
#'
#' This is a function to plot multi-trace subplots using plotly package.
#' Multiple dependent variables are plot agains independent variable. Dependent
#' variables can be organized/combined in different subplots. Groupby variable
#' and plot types can be controlled. The plots can be both vertical (default)
#' and horizontal orientations. It can return either subplots or a list of
#' plotly objects.
#'
#' @param df A data.frame.
#' @param in_var A character. Character of the column to be used for independent
#'   variable, typically x axis in "vertical" orientation.
#' @param de_var A character, or (named) charactor vector, or (named) list of
#'   character vector. The structure will determine how the dependent variables
#'   are arranged into multi-axis subplots, typically y axis in "vertical"
#'   orientation.
#' @param groupby_var A character, or character vector. Character of the
#'   column(s) to be used for grouping, currently implemented by linetype.
#' @param plot_type Named list for plotly type, mode, etc.
#' @param orientation "v" or "h", for "vertical" and "horizontal". v (vertical)
#'   is the common x/y arrangement e.g. top edge, h (horizontal) is the uncommon
#'    x/y arrangement e.g. right edge.
#' @param return_type "subplot" or "list". return_type should be subplot or list
#'    of plots.
#'
#' @return A plotly (subplot) object or a list of plotly objects depending on
#'   return_type.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom rlang arg_match
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' multi_trace_plotly(df, in_var = "x_column",
#'   de_var = c("y_column1", "y_column2"),
#'   groupby_var = "z_column",
#'   plot_type = list(type = "scatter", mode = "lines"),
#'   orientation = "v", return_type = "subplot")
#' }
#'
multi_trace_plotly <-
  function(df,
           in_var, # independent variable. Do not use x/y here is to allow vertical vs. horizontal representations
           de_var, # dependent variables
           groupby_var = NULL, # TODO, need to handle NULL value well
           plot_type = list(type = "scatter", mode = "lines"), # list(type = "scatter", mode = "lines", barmode = "overlay", etc.), group by independent variable
           orientation = c("v", "h"), # v or h. v (vertical) is the common x/y arrangement e.g. top edge, h (horizontal) is the uncommon x/y arrangement e.g. right edge
           return_type = c("subplot", "list") # return_type should be subplot or list of plots
  )
  {
    orientation <- arg_match(orientation)
    return_type <- arg_match(return_type)

    assert_that(!is.null(df))

    p <- rep(list(plot_ly()), length(de_var))

    for (i in seq_along(de_var)) {
      for (j in seq_along(de_var[[i]])) {
        if (orientation == "v") {
          x_col <- df[[in_var]]
          y_col <- df[[de_var[[i]][[j]]]]
          x_axis_title <- in_var
          x_axis <- list(title = x_axis_title)
          y_axis_title <- names(de_var)[[i]] %||% (de_var[[i]] %>% paste(collapse = "<br>"))
          y_axis <- list(title = y_axis_title)
        }
        if (orientation == "h") {
          y_col <- df[[in_var]]
          x_col <- df[[de_var[[i]][[j]]]]
          y_axis_title <- in_var
          x_axis <- list(title = x_axis_title)
          x_axis_title <- names(de_var)[[i]] %||% (de_var[[i]] %>% paste(collapse = "<br>"))
          y_axis <- list(title = y_axis_title,
                         autorange = "reversed")
        }
        subplot_title <- names(de_var)[[i]] %||% (de_var[[i]] %>% paste(collapse = "<br>"))

        linetype <- ifelse(is.null(groupby_var), "NULL",
                           "~interaction(mget(unlist(groupby_var, use.names = FALSE)))") # another hack (double-hacking) to get it work when groupby_var is NULL
        p[[i]] <-
          p[[i]] %>%
          add_trace(data = df, # even though I have changed the data representation in the plotly call to actual column data, this is still necessary as in "linetype" it requires the data component
                    x = x_col,
                    y = y_col, # after quite some test, ~get() or ~eval(parse(text = )) do not work!!
                    type = plot_type$type,
                    mode = plot_type$mode,
                    text = de_var[[i]][[j]],
                    color = de_var[[i]][[j]],
                    linetype = eval(parse(text = linetype))
          )
      }

      p[[i]] <-
        p[[i]] %>%
        layout(
          xaxis = x_axis,
          yaxis = y_axis,
          annotations = list(
            text = subplot_title,
            x = 0.5, y = 1,
            xref = "paper", yref = "paper", showarrow = FALSE,
            xanchor = "center"
          )
        )
    }

    if (return_type == "list") {
      return(p)
    }
    if (return_type == "subplot") {
      if (orientation == "v") {
        p <-
          subplot(p, nrows = length(p),
                  margin = 0, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
        return(p)
      }

      if (orientation == "h") {
        p <-
          subplot(p, nrows = 1,
                  margin = 0, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
        return(p)
      }
    }


  }
