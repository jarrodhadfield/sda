\name{gasUse}
\alias{gasUse}
\title{Effect of insulation on gas consumption}
\description{
A houseowner measured gas use and external temperature over two heating
seasons, one (lasting 26 weeks) before installation of wall
insulation, the other (lasting 30 weeks) after installation.
}
\format{
  Data frame with 56 rows and three columns:
\describe{
\item{\code{Insul}}{Factor with two levels, \code{before} or \code{after}
insulation.}
\item{\code{Temp}}{External temperature}
\item{\code{Gas}}{Weekly gas use (1000's of cubic feet)}
  }
}
\source{The whiteside dataset in the MASS package}
\examples{
head(gasUse)
}
\keyword{datasets}
