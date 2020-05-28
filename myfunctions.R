## functions for the main script

library("tidyverse")

make_data <- function(
  fixef,  # vector of fixed effects: see model matrix X for order of coefficients
  analysis = "main",  # one of "main" or "colour"
  print_modelmatrix = FALSE
  ) {

  # data frame
  if (analysis == "main") {
    df <- tibble(
      group = factor(rep(c("blind", "sighted"), each = 3), levels = c("sighted", "blind")),
      property = rep(c("categorical", "multimodal", "visual"), 2)
      )
  } else if (analysis == "colour") {
    df <- tibble(
      group = factor(rep(c("blind", "sighted"), each = 2), levels = c("sighted", "blind")),
      property = rep(c("colour", "non-colour"), 2)
      )
  } else {
    stop("Invalid value for 'analysis'!")
  }

  # model matrix
  X <- model.matrix(~ group * property, df)

  if (print_modelmatrix) {
    print(df)
    print(X)
  }

  df$DV <- X %*% fixef

  df
}

plot_data <- function(df, DV_name, colour = FALSE) {
  p <- ggplot(df, aes(x = property,  y = DV, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab(DV_name) +
    theme_classic() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
      )
  if (colour) p <- p + xlab("type of visual property")
  p
}
