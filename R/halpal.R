#' Color palettes using Halton sequences in CIE L*a*b* color space
#'
#' @param n The number of colors to include in the palette.
#' @param L_min The minimum value of L in the CIE L*a*b* color space.
#' @param L_max The maximum value of L in the CIE L*a*b* color space.
#' @param a_min The minimum value of a in the CIE L*a*b* color space.
#' @param a_max The maximum value of a in the CIE L*a*b* color space.
#' @param b_min The minimum value of b in the CIE L*a*b* color space.
#' @param b_max The maximum value of b in the CIE L*a*b* color space.
#' @param ... Further arguments passed to `halpal`.
#' @return A vector of colors.
#' @export
#' @examples
#' n <- 20
#' barplot(rep(1, n), col=tail(halpal(n+15), n), space=0, border=NA, axes=FALSE)
#'
#' n <- 20
#' set.seed(123)
#' intercept <- runif(n, -5, 5)
#' slope <- -(intercept/5)*runif(n, 0, 1)
#' dat <- expand.grid(ID=paste0("ID:", 1:n), x=1:10)
#' dat$y <- intercept + dat$x*slope + runif(nrow(dat), 0, 3)
#' ggplot(dat, aes(x=x, y=y, color=ID)) + geom_line(size=1.3) +
#'   scale_color_halpal() + theme_bw()
#' 
halpal <- function(
    n       = 20,
    L_min   = 45,
    L_max   = 80,
    a_min   = -60,
    a_max   = 80,
    b_min   = -60,
    b_max   = 80) {

    x <- randtoolbox::halton(n, 3)

    L <- x[,3]*(L_max - L_min) + L_min
    a <- x[,1]*(a_max - a_min) + a_min
    b <- x[,2]*(b_max - b_min) + b_min

    pal <- colorspace::LAB(L=L, A=a, B=b) 

    colorspace::hex(pal, fixup=T)
}

#' @rdname halpal
#' @export
scale_colour_halpal <- function(...) {
    ggplot2::discrete_scale(
        aesthetics="colour",
        scale_name="halpal",
        palette=function(n) halpal(n=n, ...))
}

#' @rdname halpal
#' @export
scale_color_halpal <- scale_colour_halpal

#' @rdname halpal
#' @export
scale_fill_halpal <- function(...) {
    ggplot2::discrete_scale(
        aesthetics="fill",
        scale_name="halpal",
        palette=function(n) halpal(n=n, ...))
}

