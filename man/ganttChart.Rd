\name{ganttChart}
\alias{ganttChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a Gantt Chart
%%  ~~function to do ... ~~
}
\description{
This function reads in an excel file (.xlsx) with tasks and dates and creates a Gantt Chart
}
\usage{
ganttChart(excel_file, title = "Gantt-Chart", point_size = 5, date_breaks = "4 weeks", minor_breaks = "1 week", save = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{excel_file}{
  Specify a path to an excel-file (.xlsx) where the project data are located. This excel file needs to have at least four columns: "from", "to", "task", and "group".
%%     ~~Describe \code{excel_file} here~~
}
  \item{title}{
  The title displayed on top of the chart.
%%     ~~Describe \code{title} here~~
}
  \item{point_size}{
    Size of the points in the chart.
%%     ~~Describe \code{point_size} here~~
}
  \item{date_breaks}{
  Specify the intervals displayed on the x-axis. E.g. "4 weeks".
%%     ~~Describe \code{date_breaks} here~~
}
  \item{minor_breaks}{
  Specify the minor intervals display on the x.axis. E.g. "1 week".
%%     ~~Describe \code{minor_breaks} here~~
}
  \item{save}{
%%     ~~Describe \code{save} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (excel_file, title = "Gantt-Chart", point_size = 5,
    date_breaks = "4 weeks", minor_breaks = "1 week", save = FALSE)
{
    assertthat::assert_that(file.exists(here::here(excel_file)))
    df <- df \%>\% mutate(from = as.Date(from), to = as.Date(to),
        group = factor(group, levels = unique(df$group))) \%>\%
        arrange(desc(group), desc(from)) \%>\% mutate(task = forcats::fct_inorder(task)) \%>\%
        pivot_longer(cols = c(from, to), values_to = "date") \%>\%
        mutate(pch = ifelse(status \%in\% c("beendet", "abgeschlossen") &
            name == "to", 15, 18), pch = ifelse(name == "from",
            32, pch))
    ggplot(data = df, aes(x = date, y = task, colour = group)) +
        geom_line(lwd = 3, alpha = 0.7) + scale_shape_identity(aes(fill = group)) +
        geom_point(aes(group = group, shape = pch), size = point_size) +
        scale_x_date(position = "top", date_breaks = date_breaks,
            minor_breaks = minor_breaks, date_labels = "\%m/\%y") +
        theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(size = 22)) +
        ggtitle(title) + labs(y = "", x = "", colour = "Bereich") +
        ggsave("gantt-Diagramm.png", units = "cm", height = 17,
            width = 32)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
