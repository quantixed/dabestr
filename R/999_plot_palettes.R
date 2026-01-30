# Helper functions that deal with assignment of colour palettes for the overall plots
#
# Contains function `apply_palette`.

# Applies palettes to <ggplot> objects
# TODO add proper documentation.
apply_palette <- function(ggplot_object, palette_name, palette_values = NULL, num_groups) {
  # check palette is valid
  palette_name <- check_palette(palette_name, num_groups, palette_values)
  ggplot_object <- switch(
    palette_name,
    "npg" =
      ggplot_object + ggsci::scale_color_npg() + ggsci::scale_fill_npg(),
    "aaas" =
      ggplot_object + ggsci::scale_color_aaas() + ggsci::scale_fill_aaas(),
    "nejm" =
      ggplot_object + ggsci::scale_color_nejm() + ggsci::scale_fill_nejm(),
    "lancet" =
      ggplot_object + ggsci::scale_color_lancet() + ggsci::scale_fill_lancet(),
    "jama" =
      ggplot_object + ggsci::scale_color_jama() + ggsci::scale_fill_jama(),
    "jco" =
      ggplot_object + ggsci::scale_color_jco() + ggsci::scale_fill_jco(),
    "ucscgb" =
      ggplot_object + ggsci::scale_color_ucscgb() + ggsci::scale_fill_ucscgb(),
    "d3" =
      ggplot_object + ggsci::scale_color_d3() + ggsci::scale_fill_d3(),
    "locuszoom" =
      ggplot_object + ggsci::scale_color_locuszoom() + ggsci::scale_fill_locuszoom(),
    "igv" =
      ggplot_object + ggsci::scale_color_igv() + ggsci::scale_fill_igv(),
    "cosmic" =
      ggplot_object + ggsci::scale_color_cosmic() + ggsci::scale_fill_cosmic(),
    "uchicago" =
      ggplot_object + ggsci::scale_color_uchicago() + ggsci::scale_fill_uchicago(),
    "brewer" =
      ggplot_object + ggplot2::scale_color_brewer() + ggplot2::scale_fill_brewer(),
    "ordinal" =
      ggplot_object + ggplot2::scale_color_ordinal() + ggplot2::scale_fill_ordinal(),
    "viridis_d" =
      ggplot_object + ggplot2::scale_color_viridis_d() + ggplot2::scale_fill_viridis_d(),
    "manual" =
      ggplot_object + ggplot2::scale_color_manual(values = palette_values) + ggplot2::scale_fill_manual(values = palette_values),
    "default" =
      ggplot_object
  )
  
  return(ggplot_object)
}

get_palette_colours <- function(palette_name, num_colours, palette_values = NULL) {
  # check palette is valid
  palette_name <- check_palette(palette_name, num_colours, palette_values)
  # palette function by name
  colours <- switch(palette_name,
                    "npg" = ggsci::pal_npg()(num_colours),
                    "aaas" = ggsci::pal_aaas()(num_colours),
                    "nejm" = ggsci::pal_nejm()(num_colours),
                    "lancet" = ggsci::pal_lancet()(num_colours),
                    "jama" = ggsci::pal_jama()(num_colours),
                    "jco" = ggsci::pal_jco()(num_colours),
                    "ucscgb" = ggsci::pal_ucscgb()(num_colours),
                    "d3" = ggsci::pal_d3()(num_colours),
                    "locuszoom" = ggsci::pal_locuszoom()(num_colours),
                    "igv" = ggsci::pal_igv()(num_colours),
                    "cosmic" = ggsci::pal_cosmic()(num_colours),
                    "uchicago" = ggsci::pal_uchicago()(num_colours),
                    "brewer" = RColorBrewer::brewer.pal(num_colours, "Accent"),
                    "ordinal" = viridisLite::viridis(n = num_colours, option = "viridis"),
                    "viridis_d" = viridisLite::viridis(n = num_colours, option = "viridis"),
                    "manual" = palette_values
  )
  if(is.null(colours)) {
    colours <- rep("black", num_colours)
  }
  return(colours)
}

check_palette <- function(palette_name, num_groups, palette_values = NULL) {
  # list of max colours per palette
  max_colours <- list(
    "d3" = 10,
    "npg" = 10,
    "aaas" = 10,
    "nejm" = 10,
    "lancet" = 9,
    "jama" = 7,
    "jco" = 10,
    "ucscgb" = 26,
    "d3" = 20,
    "locuszoom" = 12,
    "igv" = 20,
    "cosmic" = 12,
    "uchicago" = 10,
    "brewer" = 8,
    "ordinal" = Inf,
    "viridis_d" = Inf,
    "manual" = Inf
  )
  # check num_groups does not exceed max colours
  if (palette_name %in% names(max_colours)) {
    if (num_groups > max_colours[[palette_name]]) {
      warning(paste0("The selected palette '", palette_name, "' may not have enough
 colours for ", num_groups, " groups. Switching to 'default' ggplot2 colours."))  
      palette_name <- "default"
    }
  }
  # check manual palette has enough colours
  if (palette_name == "manual") {
    if (is.null(palette_values) | length(palette_values) < num_groups) {
      warning("Manual palette selected but insufficient colours provided. Using default ggplot2 colours.")
      palette_name <- "default"
    }
  }
  
  return(palette_name)
}
