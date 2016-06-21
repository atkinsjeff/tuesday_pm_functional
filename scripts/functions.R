# functions used in reclassifying and processing data


###############
# Density plot function from chm
density_plot_from_chm <- function(rast.in, title, bins) {
     density(rast.in, main = title, xlab = "Canopy height (m)")
     sapply(bins, function(x) abline(v = x, col = "red"))
}

# Histogram plot function, expects chm raster, title, and breaks
hist_plot_from_chm <- function(rast.in, title, bins) {
     #chm[ chm <= 0 ] <- NA
     hist(rast.in, main = title, xlab = "Canopy height (m)")
     sapply(bins, function(x) abline(v = x, col = "red"))
}


# Let's start a function that reclassifies a matrix from a set of breaks
create_height_class_matrix <- function(breaks) {
     # get the length of the breaks vector to figure out how many classes we have
     br.length <- length(breaks)
     
     # Initialize height class vector with 0
     height.class.m <- c(0)
     
     # for inputs of breaks that  = c(6, 30, 50)
     # We'd like to make something like this:
     
     # c(0, 6, 1,
     #   6, 30, 2,
     #   30, 50, 3,
     #   50, 1000, 4)
     
     for(i in 1:br.length) {
          height.class.m <- c(height.class.m, breaks[i - 1], breaks[i], i)
     }
     reclass.height.matrix <- matrix(height.class.m,
                                     ncol = 3,
                                     byrow = TRUE)
     reclass.height.matrix
}

##########################################
##########################################
# function to plot the reclassified raster

plot_reclassified_raster <- function(rast.in, site.name, breaks){
     # this is a tricky bit because we need to out the legend
     # outside of the plot region
     
     # Get colors for plotting
     bin.colors <- rev(terrain.colors(length(breaks)))
     
     # make room for a legend
     
     par(xpd = FALSE, mar = c(5.1, 4.1, 4.1, 4.5))
     
     # plot
     plot(rast.in,
          col = bin.colors,
          main = paste("Canopy height classes \n", site.name),
          legend = FALSE)
     
     # allow legend to plot outside of bounds
     par(xpd = TRUE)
     
     # legend x
     leg.x <- par()$usr[2] + 20
     
     # legend y
     leg.y <- par()$usr[4] + 50 - (abs(par()$usr[3] - par()$usr[4]) / 2) 
     
     # create legend text
     height.mat <- create_height_class_matrix(breaks)
     
     # initialize legend text
     legend.text <- c()
     
     for (i in 1:length(breaks)) {
          
          legend.text <- c(legend.text, 
                           paste0(height.mat[i, 1], "-", 
                                  height.mat[i, 2], " m"))
     }
     
     # create the legend
     legend(leg.x, leg.y,  # set x,y legend location
            legend = legend.text,  # make sure the order matches colors
            fill = bin.colors,
            bty = "n") # turn off border
     
     # turn off plotting outside of bounds
     par(xpd = FALSE)
}


##### Make PDF function
make_pdf <- function(expr, filename, ..., verbose = TRUE) {
     if(verbose){
          message("Creating: ", filename)
     }
     pdf(file = filename, ...)
     on.exit(dev.off())
     eval.parent(substitute(expr))
}

