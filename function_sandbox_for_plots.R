# An example that generates PDF file from a function that creates a plot
## See http://nicercode.github.io/blog/2013-07-09-figure-functions/
make_pdf <- function(expr, filename, ..., verbose = TRUE) {
     if(verbose){
          message("Creating: ", filename)
     }
     pdf(file = filename, ...)
     on.exit(dev.off())
     eval.parent(substitute(expr))
}

chm <- raster("../NEONdata/D17-California/TEAK/2013/lidar/Teak_lidarCHM.tif")

#this is the function that creates the plots from chm
density_plot_from_chm <- function(chm, title, breaks) {
     density(chm, main = title, xlab = "Canopy height (m)")
     sapply(breaks, function(x) abline(v = x, col = "red"))
}

hist_plot_from_chm <- function(chm, title, breaks) {
     chm[ chm <= 0 ] <- NA
     hist(chm, main = title, xlab = "Canopy height (m)")
     #sapply(breaks, function(x) abline(v = x, col = "red"))
}


make_pdf(density_plot_from_chm(teak_chm, 
                               title = "Canopy heights for NEON TeaKettle",
         breaks = c(5,20,50)),
         filename = "teak_chm_density.pdf", width = 8, height = 6)

make_pdf(hist_plot_from_chm(teak_chm, 
                               title = "Canopy heights for NEON TeaKettle",
                               breaks = c(5,20,50)),
         filename = "teak_chm_hist.pdf", width = 8, height = 6)