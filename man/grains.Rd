\name{grains}
\alias{grains}
\title{Size distribution of sand grains}
\description{
  Cumulative distribution of diameters in a sample of sand, obtained by
  passing the sample through a series of increasingly fine sieves.
}
\format{Data frame with two columns:
  \describe{
    \item{\code{diams}}{x = diameter (mm)}
    \item{\code{probs}}{Proportion of sample with diameter < x} 
  }
  }
\examples{
plot(probs ~ diams, type = 'l', data = grains)
}
\keyword{datasets}
