\name{bobbins}
\alias{bobbins}
\title{Strength of yarn samples}
\description{
Strength of yarn samples from six bobbins
}
\format{
Data frame with 24 rows and two variables:
\describe{
 \item{\code{bobbin}}{(factor, with 6 levels)}
  \item{\code{strength}}{(numeric)}
  }
}
\examples{
aov(strength ~ bobbin, data = bobbins)
}
\keyword{datasets}
