#' Plot a Gantt-Chart
#'
#' @param excel_file The file containing the tasks and the corresponding dates
#' @param title Title of the plot
#' @param point_size Size of the points plotted in the Gantt-Chart
#' @param date_breaks
#' @param minor_breaks
#' @param save
#'
#' @return
#' @export
#'
#' @import tidyverse
#' @import openxlsx
#' @import assertthat
#' @import forcats
#'
#' @examples
ganttChart <- function(excel_file, title="Gantt-Chart", point_size=5, date_breaks = "4 weeks", minor_breaks = "1 week", save = FALSE){

  assertthat::assert_that(file.exists(excel_file))
  df <- openxlsx::read.xlsx(excel_file)

  df <- df %>% mutate(from = as.Date(from),
                      to = as.Date(to),
                      group = factor(group, levels = unique(df$group))
  ) %>%
    arrange(desc(group), desc(from)) %>%
    #arrange(desc(from)) %>%
    mutate(task = forcats::fct_inorder(task)) %>%
    pivot_longer(cols = c(from, to), values_to = "date") %>%
    mutate(pch = ifelse(status %in% c("beendet", "abgeschlossen") & name == "to", 15, 18),
           pch = ifelse(name == 'from', 32, pch))  # Am Anfang der Linie kein Punkt


  ggplot(data = df, aes(x=date, y=task, colour = group)) +
    geom_line(lwd=3, alpha=.7) +
    scale_shape_identity(aes(fill=group)) +
    geom_point(aes(group=group, shape=pch), size=point_size) +
    scale_x_date(position="top", date_breaks = date_breaks, minor_breaks = minor_breaks,
                 date_labels="%m/%y") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size=22)
    ) +
    ggtitle(title) +
    labs(y="", x = "", colour="Bereich") +
    ggsave("gantt-Diagramm.png", units = "cm", height = 17, width=32)
}
