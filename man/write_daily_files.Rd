\name{write_daily_files}
\alias{write_daily_files}
\title{
  Split Daily Prices File
}
\description{
  Split large daily prices file into one file per PERMNO.
}
\usage{
write_daily_files(filename, outdir, verbose = TRUE)
}
\arguments{
  \item{filename}{
    character
}
  \item{outdir}{
    character
}
  \item{verbose}{
    logical
}
}
\details{
  Uses \code{\link{readLines}} to read the file.
}
\value{
  Mostly called for its side effect. Returns invisibly the number of
  lines written.
}
\author{
  Enrico Schumann
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
## to be added
}
