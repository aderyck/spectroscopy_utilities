InteractPlot4Spectra <- function(spectra, name = "") {
  # 1) Ensure required packages are installed and loaded
  for (pkg in c("plotly", "reshape2", "RColorBrewer")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  
  # 2) Convert input to matrix, get sample count, wavelengths, and IDs
  mat <- as.matrix(spectra)
  n   <- nrow(mat)
  wl  <- colnames(mat)
  if (is.null(wl)) wl <- seq_len(ncol(mat))
  ids <- seq_len(n)
  dimnames(mat) <- list(ID = ids, `Wavelength (nm)` = wl)
  
  # 3) Reshape data from wide to long format for plotting
  spec_frame <- reshape2::melt(
    mat,
    varnames   = c("ID", "Wavelength (nm)"),
    value.name = "Reflectance"
  )
  
  # 4) Build a Set1-based color palette extended to n colors
  maxcols      <- RColorBrewer::brewer.pal.info["Set1", "maxcolors"]
  base_colors  <- RColorBrewer::brewer.pal(min(n, maxcols), "Set1")
  color_vector <- grDevices::colorRampPalette(base_colors)(n)
  
  # 5) Create interactive Plotly line plot
  plotly::plot_ly(
    spec_frame,
    x          = ~as.numeric(`Wavelength (nm)`),
    y          = ~Reflectance,
    color      = ~factor(ID),
    colors     = color_vector,
    showlegend = TRUE
  ) %>%
    plotly::add_lines(line = list(width = 1.3)) %>%
    plotly::layout(
      title = name,
      plot_bgcolor = "white",
      xaxis = list(
        title    = "Wavelength (nm)",
        showline = FALSE,
        zeroline = FALSE,
        showgrid = TRUE
      ),
      yaxis = list(
        title    = "Reflectance",
        showline = FALSE,
        zeroline = FALSE,
        showgrid = TRUE
      )
    )
}







