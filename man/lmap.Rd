% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmap.R
\name{lmap_lst}
\alias{lmap_lst}
\alias{lmap_lgl}
\alias{lmap_int}
\alias{lmap_dbl}
\alias{lmap_chr}
\alias{lmap_vec}
\alias{mclmap_lst}
\alias{mclmap_lgl}
\alias{mclmap_int}
\alias{mclmap_dbl}
\alias{mclmap_chr}
\alias{mclmap_vec}
\title{Apply a function to each subset of a list.}
\usage{
lmap_lst(.x, .f, mode = c("list", "integer", "double", "character", "logical"))

lmap_lgl(.x, .f)

lmap_int(.x, .f)

lmap_dbl(.x, .f)

lmap_chr(.x, .f)

lmap_vec(.x, .f, .ptype = NULL)

mclmap_lst(.x, .f, ...)

mclmap_lgl(.x, .f, ...)

mclmap_int(.x, .f, ...)

mclmap_dbl(.x, .f, ...)

mclmap_chr(.x, .f, ...)

mclmap_vec(.x, .f, ..., .ptype = NULL)
}
\arguments{
\item{.x}{A list.}

\item{.f}{A function to apply to each subset of \code{.x}.}

\item{mode}{Output type.}

\item{.ptype}{A prototype for the output vector.}

\item{...}{Additional arguments passed to \code{\link[parallel:mclapply]{parallel::mclapply()}}.}
}
\value{
A vector of the same length as \code{.x}.
The suffix of the function name denotes the output type.
\verb{*_vec()} applies \code{\link[purrr:list_simplify]{purrr::list_simplify()}} before returning.
}
\description{
\verb{lmap*()} are similar to \code{lapply()}, but they map over subset \code{.x[i]}
instead of element \code{.x[[i]]}.
It allows \code{.f} to access the attributes of the encapsulating list.
This is useful for operation on the \code{sfc} column in an \code{sf} data.frame.
\code{\link[purrr:lmap]{purrr::lmap()}} does similar things, but it forces the output to be a list.

\verb{mclmap*()} are parallelized versions of \verb{lmap*()}.
}
\examples{
x = list(a = 1.0, b = 2.0)
lmap_lst(x, unlist)

lmap_lgl(x, is.list)

lmap_int(x, length)

lmap_dbl(x, unlist)

lmap_chr(x, names)

lmap_vec(x, \(.x) .x[[1L]] + 2i)
}
\seealso{
\code{\link[=rowwise_map]{rowwise_map()}}
}
