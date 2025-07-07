InteractPlot4Lines <- function(df, x, y, group,
                               name       = "",
                               xlab       = x,
                               ylab       = y,
                               line_width = 1.3) {
  # 1) Ensure required packages are installed and loaded
  for (pkg in c("plotly", "RColorBrewer")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  
  # 2) Build a Set1-based color palette extended to n colors
  ngroups   <- length(unique(df[[group]]))
  maxcols   <- RColorBrewer::brewer.pal.info["Set1", "maxcolors"]
  base_cols <- RColorBrewer::brewer.pal(min(ngroups, maxcols), "Set1")
  color_vec <- grDevices::colorRampPalette(base_cols)(ngroups)
  
  # 3) Create interactive Plotly line plot
  plotly::plot_ly(
    df,
    x     = as.formula(paste0("~", x)),
    y     = as.formula(paste0("~", y)),
    color = as.formula(paste0("~", group)),
    colors= color_vec,
    type  = "scatter",
    mode  = "lines",
    line  = list(width = line_width),
    hoverinfo = "text",
    text = ~paste0(group,
                   "<br>", xlab, ": ", df[[x]],
                   "<br>", ylab, ": ", df[[y]])
  ) %>%
    plotly::layout(
      title = name,
      xaxis = list(
        title = xlab,
        dtick = 1,
        showgrid = TRUE
      ),
      yaxis = list(
        title = ylab,
        showgrid = TRUE
      ),
      plot_bgcolor = "white"
    )
}


