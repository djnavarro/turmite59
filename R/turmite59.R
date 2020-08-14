
#' Makes a heartbleed image
#'
#' @param shade hex code
#' @param file filename
#'
#' @export
turmite59 <- function(shade, file = NULL) {

  cat("setting up...\n")
  param <- make_settings(shade)

  cat("turmite wandering...\n")
  raster <- make_background(param)

  cat("dust heart beating...\n")
  dat <- make_hearts(param)

  cat("image rendering...\n")
  ggplot2::ggsave(
    filename = make_filename(file, shade),
    plot = make_ggplot(param, raster, dat),
    width = param$pixels_wide / 300,
    height = param$pixels_high / 300,
    dpi = 300
  )
}


# settings ----------------------------------------------------------------

sample_palette <- function(n) {
  pal <- paletteer::palettes_c_names
  ind <- sample(nrow(pal), 1)
  pnm <- paste0(pal[ind, 1], "::", pal[ind, 2])
  return(paletteer::paletteer_c(pnm, n))
}

make_settings <- function(shade) {

  # control RNG seed
  seed <- sum(grDevices::col2rgb(shade)) + 1
  set.seed(seed)

  # constants
  param <- list(
    seed = seed,
    ncols = 1000,
    alpha_base = .5,
    fill_base = shade,
    shift = c(.3, .4),
    grains_high = 2500,
    grains_wide = 2500,
    nsteps = 10000000,
    ss = 3
  )

  # derived values
  param$pixels_high = param$grains_high * 2
  param$pixels_wide = param$grains_wide * 2


  # variables
  param$heart_size <- stats::runif(1, min = .2, max = .5)
  param$n_slices <- sample(6:12, 1)
  param$palette_base <- sample_palette(param$ncols + 1)
  param$palette_heart <- grDevices::adjustcolor(
    col = param$palette_base,
    offset = c(0.5, 0.5, 0.5, 0),
    transform = diag(c(.7, .7, .7, 1))
  )

  return(param)
}


# turmite background ------------------------------------------------------

make_background <- function(param) {

  ar <- param$grains_high / param$grains_wide
  raster <- ambient::long_grid(
    x = seq(0, 1,  length.out = param$grains_wide),
    y = seq(0, ar, length.out = param$grains_high)
  )

  grid <- turmite(
    width = param$grains_wide,
    height = param$grains_high,
    iter = param$nsteps,
    step_size = param$ss
  )
  grid <- t(grid)
  inds <- 1 + ceiling(param$ncols * grid/param$nsteps)
  raster$shade <- param$palette_base[inds]

  return(raster)
}


# bleeding heart ----------------------------------------------------------

make_hearts <- function(param) {

  dat <- jasmines::use_seed(param$seed) %>%
    jasmines::entity_heart(grain = 1000, size = param$heart_size) %>%
    dplyr::mutate(ind = dplyr::row_number()) %>%
    jasmines::unfold_slice(
      iterations = param$n_slices,
      scale = .5 * 10^-24,
      scatter = TRUE,
      output1 = "id"
    ) %>%
    dplyr::mutate(
      x = x + stats::rnorm(dplyr::n())/500,
      y = y + stats::rnorm(dplyr::n())/500
    ) %>%
    jasmines::unfold_breeze(
      iterations = 100,
      scale = .0002,
      drift = .0005,
      fractal = ambient::ridged,
      octaves = 8
    ) %>%
    jasmines::unfold_inside() %>%
    dplyr::mutate(val = 1 + (inside>0)*ind)

  dat$val <- ambient::normalise(x = dat$val, to = c(1, param$ncols+1))
  dat$val <- round(dat$val)
  dat$shade <- param$palette_heart[dat$val]
  dat$x <- (dat$x + param$shift[1])
  dat$y <- (dat$y + param$shift[2])

  return(dat)
}



# plot and save -----------------------------------------------------------

make_filename <- function(file, shade) {
  if(is.null(file)) {
    file <- paste0("turmite_59_", gsub("#", "", shade), ".png")
  }
  return(file)
}

make_ggplot <- function(param, raster, dat) {

  ggplot2::ggplot(
    data = raster,
    mapping = ggplot2::aes(x, y, fill = shade)
  ) +

    # the raster object forms the background
    ggplot2::geom_raster(alpha = param$alpha_base) +

    # the heart is made of dust/points
    ggplot2::geom_point(
      data = dat,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        color = shade,
        alpha = exp(-(time -1)/20)
      ),
      inherit.aes = FALSE,
      show.legend = FALSE,
      size = .5
    ) +

    # bunch of settings...
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0,1)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = param$fill_base,
        color = param$fill_base
      )
    )
}


