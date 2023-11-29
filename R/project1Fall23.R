library(dplyr)
library(ggplot2)
library(magrittr)
library(gridExtra)

#' ntickets
#'
#' @param N
#' @param gamma
#' @param p
#'
#' @return a list of values (nd,nc,N,p,gamma). a layout with two plots (one discrete and one continuous)
#' @export
#'
#' @examples ntickets(200,0.02,0.95)
#' @examples ntickets(N=400,gamma=0.02,p=0.95)
#'
ntickets <- function(N,gamma,p){


  seatsnum <- N
  upperval <- seatsnum + 10
  p <- p
  gamma <- gamma
  overbookChance <- data.frame('Probability' = pbinom(seatsnum, seatsnum + 1:upperval, p, lower.tail = FALSE),'tickets_sold' = seatsnum + 1:upperval)
  index <- which.min(abs(overbookChance$Probability - gamma))
  nd <- overbookChance$tickets_sold[index]
  nc <- stats::approxfun(x = overbookChance$Probability, y = overbookChance$tickets_sold)(gamma)
  x_breaks <- seq(from = seatsnum + 1, to = upperval + 20, by = 2)

  plotD <- ggplot(overbookChance) +
    geom_point(aes(tickets_sold, Probability), size = 1, col = "red") +  # points
    geom_line(aes(tickets_sold, Probability), size = 0.5) +   # line to connect points
    geom_hline(yintercept = gamma, color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_vline(xintercept = nd, color = "red", linetype = "dashed", linewidth = 0.25) +
    theme_bw() +
    labs(x = "n",
         y = "Objective",
         title = "Objective Vs n to find optimal tickets sold",
         subtitle = paste(nd, "gamma =", gamma, ", N =", seatsnum, " discrete")) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(breaks = seq(0, 1, 1 - p), limits = c(0, 1)) +
    coord_cartesian(xlim = c(seatsnum + 1, upperval+20)) +
    theme(axis.text.x = element_text(size = 5), axis.text.y = element_text(size = 5))

  plotC <- ggplot(overbookChance) +
    geom_line(aes(tickets_sold, Probability), linewidth = 0.5) +
    geom_hline(yintercept = gamma, color = "blue", linetype = "dashed", linewidth = 0.25) +
    geom_vline(xintercept = nc, color = "blue", linetype = "dashed", linewidth = 0.25) +
    theme_bw() +
    labs(x = "n",
         y = "Objective ",
         title = "Objective Vs n to find optimal tickets sold",
         subtitle = paste(nc, "gamma =", gamma, ", N =", seatsnum, " continuous")) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(breaks = seq(0, 1, 1 - p), limits = c(0, 1)) +
    coord_cartesian(xlim = c(seatsnum + 1, upperval+20)) +
    theme(axis.text.x = element_text(size = 5), axis.text.y = element_text(size = 5))
  values_list <- list(nd = nd, nc = nc, gamma = gamma, N = seatsnum, p = p)

  layout <- grid.arrange(plotD, plotC, nrow = 2)

  return(list(values_list = values_list, layout = layout))
}
