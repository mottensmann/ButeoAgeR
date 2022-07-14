#' Estimate the age of juvenile buzzards based on wing length
#'
#' @description Buzzard age is estimated based on a standard growth curve modeled from published data by
#' Bijlsma (Limosa 72, 1999).
#'
#' @details Estimations are based on a four degree polynomial fit
#'
#' @param df data frame containing data
#' @param wing column name for wing measurements
#' @param sex column name giving the sex of individuals or NULL
#' @param unit unit of values. By default in mm
#' @param .plot logical
#' @param .show_model logical
#' @param decimals decimals
#'
#' @references
#' Bijlsma, RG 1999: Sex determination of nestling Common Buzzards Buteo buteo. Limosa 72, 1.10
#'
#' @examples
#' ## 240 mm wing length
#' # buteo_age(data.frame(wing = c(240,240), sex = c("male","female")))
#' # buteo_age(data.frame(wing = c(240,240), sex = c(1,0)))
#'
#' @import ggplot2
#'
#' @export
#'
buteo_age <- function(df = NULL, wing = "wing", sex = "sex", unit = c("mm", "cm"), .plot = F, decimals = 2,
                      .show_model = T) {
  unit <- match.arg(unit)

  ## translate binary sex
  if (!is.null(sex)) {
    if (class(utils::type.convert(df[[sex]])) != 'factor') {
      df[[sex]][df[[sex]] == 1]  <- 'male'
      df[[sex]][df[[sex]] == 0] <- 'female'
    }
  }
  ## get standard growth data
  path <- system.file("extdata", "buzzard_wing.txt", package = "RaptoR")
  data <- read.table(path, header = T, skip = 1,
                     colClasses = c("integer", "factor", "factor",
                                    "numeric", "numeric", "integer",
                                    "factor", "factor", "factor"))

  if (unit == "cm") {
    data[["mean"]] <- data[["mean"]]/10
    data[["sd"]] <- data[["sd"]]/10
  }

  ## build models

  if (is.null(sex)) {
    #model <- stats::lm(age ~ mean, data)
    model <-  stats::lm(data$age ~ poly(data$mean, 4, raw = T))
    fit.val <- data.frame(age = stats::predict(model, data), mean = data[["mean"]])
  } else {
    #model <- stats::lm(age ~ mean + sex, data)
    model <-  stats::lm(data$age ~ poly(data$mean, 4, raw = T) + data$sex)
    fit.val <- data.frame(age = stats::predict(model, data), mean = data[["mean"]], sex = data[["sex"]])
  }
  if (isTRUE(.show_model)) print(model)

  ## plot
  if (.plot == T) {
    plot <- ggplot(data, aes(y = age, x = mean, col = sex)) +
      geom_point(alpha = .8) +
      geom_line(data = fit.val, size = 1,
                aes(y = age, x = mean, col = sex)) +
      theme_classic(base_size = 12) +
      ylab("Age [days since hatching]") +
      xlab(paste0("Wing length [", unit, "]")) +
      scale_color_discrete(name = "",
                           breaks = c("1", "0"),
                           labels = c("Male", "Female")) +
      ggtitle("Common Buzzard growth curve",
              subtitle = "data: Bijlsma, RG (1999): Limosa 72")

    # if (!is.null(sex)) {
    #   plot <- plot +
    #     annotate("text", y = 50, x = ifelse(unit == "mm", 70, 7), colour = "red",
    #              size = 2,
    #              label = paste0("y=",
    #                             "3.647e-01x-",
    #                             "2.211e-03x^2+",
    #                             "7.472e-06x^3-",
    #                             "8.162e-09x^4-",
    #                             "5.078e")) +
    #
    #     annotate("text", y = 47, x = ifelse(unit == "mm", 70, 7), colour = "blue",
    #              size = 2,
    #              label = paste0("y=",
    #                             "3.647e-01x-",
    #                             "2.211e-03x^2+",
    #                             "7.472e-06x^3-",
    #                             "8.162e-09x^4-",
    #                             "4.2263"))
    # } else {
    #   plot <- plot +
    #     annotate("text", y = 50 , x = ifelse(unit == "mm", 70, 7), colour = "black",
    #              size = 2,
    #              label = paste0("y=",
    #                             "3.689e-01x-",
    #                             "2.271e-03x^2+",
    #                             "7.771e-06x^3-",
    #                             "8.643e-09x^4-",
    #                             "4.725"))
    # }
    #

    out <- plot
  } else {
    ## build model
    if (is.null(sex)) {
      model <- stats::lm(age ~ mean, data)
      fit.val <- data.frame(age = stats::predict(model, data), mean = data[["mean"]])
      ## format data
      to_predict <- data.frame(mean = df[[wing]])
    } else {
      model <- stats::lm(age ~ mean + sex, data)
      fit.val <- data.frame(age = stats::predict(model, data),
                            mean = data[["mean"]], sex = data[["sex"]])
      ## format data
      to_predict <- data.frame(mean = df[[wing]],
                               sex = df[[sex]])
    }
    out <- stats::predict.lm(model, to_predict, se.fit = T)
  }

  return(out)
}

# rm(list = ls())
# df = data.frame(wing = c(240,240), sex = c("male","female"))
# wing = "wing"
# sex = "sex"
# unit = "cm"
# .plot = F
# decimals = 2
# .show_model = T
