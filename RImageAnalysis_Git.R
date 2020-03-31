# Tim Monko 01/25/2019-02/01/2019 ----
# Cell Distribution Statistics. This purpose is to take outputted distribution stuff from Matlab and do stats in R 

# Be sure to set proper working directory. 
# Shorcut to set: CTRL+SHFT+H. 
# To use the UI (on Mac) Session -> Set Working Directory -> Choose directory
# Reminder that CTRL+ALT+T runs a code "section" (a comment which ends with "----" begins a section)


# Load order means that later library calls will mask earlier ones if they have the same name. Use 'functionname:libraryname' where double colons (::) points to the desired package
library(tidyverse) # Includes ggplot2, tidyr, dplyr, stringr, readr, tibble, purrr, forcats
library(svglite) # For exporting plots as .svg graphics to use as vector format
library(magick) # A more general image altering package
library(imager) # For loading of stacks, I think to be used with magick
# library(EBImage) # This is probably the easiest and most targeted one, but is maybe jus8t a small version of ImageMagick
# library(shiny) # This is needed for some ofe xROI's functionality - maybe? 
library(xROI) # For drawing a pol\
library(rowr) # For editing of list into cols with diff lengths - maybe not needed

## (2) PREPPING IMAGES FOR ANALYSIS ----
# !!! CTL + SHFT + H to set WD  !!!!
rm(list = ls())
rescale <- 0.63492 # For 10X its 0.63492 (1/1.575)
flipROI <- 0 # If 1 then subtracts y's of the poly from the y-dims. To test check plot(combo_px) or plot(dist_tif) > addMask(ROI)

path <- getwd()
file.paths <- list.files(path = path, pattern = '\\.tif$') # I think $ at the end will search for all files with .tif appending the end, I'm not sure what the \\ is for
file.number <- length(file.paths)

dist <- image_read(file.paths[1])
names <- as.character(1:length(dist)) # Latter number is the number of slices needed to be added to the list

## (3) INTERACTIVE SECTION ----

# Create empty lists to deposit data into with the for loop (I read that lapply is better for this, but I can't figure out how to make it work)
cell.counts <- NULL
cell.counts[names] <- list(NULL)

centers.mx <- NULL
centers.mx[names] <- list(NULL)
centers.mx.scaled <- NULL
centers.mx.scaled[names] <- list(NULL)
centers.mx.rel <- NULL
centers.mx.rel[names] <- list(NULL)

centers.my <- NULL
centers.my[names] <- list(NULL)
centers.my.scaled <- NULL
centers.my.scaled[names] <- list(NULL)
centers.my.rel <- NULL
centers.my.rel[names] <- list(NULL)


for (im in 1:file.number) {
  dist <- image_read(file.paths[im]) # Uses the Magick STL library to load in a tiff image with all layers as needed
  dist.tif <- magick2cimg(dist) # converts the imported tiff file into a cimg for imager functions, keeps the layers - and color if a part of the image

  par(mfrow = c(1,1)) # Resets plot for dispalyed image to take up the full plot field
  plot(dist.tif, 1, main = file.paths[im]) # Second argument allows which layer to show, defaults to 1 if none entered. 'main' sets the title 

  px <- dist.tif > 0.5 # > 0.5 if White signal on Black background, < 0.5 if visa versa. 
    # Setting to 0.5 threshold will get rid of the 'blur' effect of straightening an image, and therefore cause no change to the original number of cells 
  poly <- drawPolygon(col = 'magenta') # Interactively draw a polygon on the displayed image and will get coordinates based off the graph

  # Establish the polygon in a relative matrix for 'rasterizeROI'
  poly.x.rel <- as.matrix((poly$x)/1360) # The maximum value for the figures dimensions. This is default to 1360 x 1036 on our microscope.
  poly.y.rel <- as.matrix((poly$y)/1036)
  poly.y.rel.flip <- as.matrix((1036-poly$y)/1036)
  poly.c <- matrix(c(poly.x.rel, poly.y.rel.flip), ncol = 2, byrow = FALSE)
  
  ROI <- rasterizeROI(poly.c, c(1360, 1036))
  #addMask(ROI, add = FALSE) # not necessary for code but used to visualize
  ROI.cimg <- as.cimg(t(ROI)) # Strangely, needs to be converted to make a "conformable array" although it looks redundant to go mat -> cimg -> mat

  # Convert to matrices/array for doing arithmetic isntead of using the cimg/pixset format
  ROI.mat <- as.matrix(ROI.cimg) 
  # ROI.flip <- 1 - ROI.mat

  min.x <- min(poly$x)
  max.x <- max(poly$x)
  min.y <- min(poly$y)
  max.y <- max(poly$y)

  for (slice in 1:length(dist)) { # length(dist) is equivalent to the number of slices in the image (dimension 3)
    px <- dist.tif[,,slice,] > 0.5
    px.cimg <- as.cimg(px)
    px.mat <- as.matrix(px.cimg)

    combo.mat <- (1-ROI.mat) + px.mat # Combine the two matrices into an image for analysis. Use 1-ROI.mat to flip the value of the ROI in order for the combo to allow > 1 to background the rest of the ROI that isn't cropped
    combo.crop <- combo.mat[(min.x:max.x), (min.y:max.y)]
    
    combo.binary <- combo.crop > 1 # Convert to binary
    combo.cimg <- as.cimg(combo.binary) # Numeric binary array to cimg
    combo.px <- as.pixset(combo.cimg) # Cimg to TRUE/FALSE pixset
  
    combo.lab <- label(combo.px) # Identifies each separate ROI with an identifying label (a different value on a grayscale)
    lab.df <- as.data.frame(combo.lab) %>% subset(value > 0) # Uses the unique value labels to create a data frame
    number.of.cells <- unique(lab.df$value) %>% max() # Prints the number of cells (ROIs) in the image
    print(number.of.cells) # Just nice to see the progress of the code
    cell.counts[[slice]] <- c(cell.counts[[slice]], number.of.cells) # Cell counts list
    
    centers <- group_by(lab.df, value) %>% summarise(mx = mean(x), my = mean(y)) # Find the mean x/y coordinate of each unique labeled ROI
    centers$slice <- slice

    # Lists of the data
    centers.mx[[slice]]        <- c(centers.mx[[slice]], centers$mx)
    centers.mx.scaled[[slice]] <- c(centers.mx.scaled[[slice]], centers$mx * rescale)
    centers.mx.rel[[slice]]    <- c(centers.mx.rel[[slice]], centers$mx/(max.x-min.y))
    centers.my[[slice]]        <- c(centers.my[[slice]], centers$my)
    centers.my.scaled[[slice]] <- c(centers.my.scaled[[slice]], centers$my * rescale)
    centers.my.rel[[slice]]    <- c(centers.my.rel[[slice]], 1-(centers$my/(max.y-min.y))) # This needs to be flipped because cIMG format has 0 on the vertical, therefore 0.9 is near the bottom but needs flipped to the top
  }
}

## (4) QUICK VISIAULIZATION----
par(mfrow = c(2,1)) # Adjust the plot for visualizing both the ROI and the histogram of the centers
plot(combo.px)
with(centers, points(mx,my,col="magenta"))
hist(unlist(centers.my.rel[2]))
## (5) SAVE to .Rdata (will save all the variables for later use) -----
base::save.image(file = 'envCKOtest.RData')

## (6) Loading in Environments for keeping groups of data together---- 
WT <- new.env()
load('envCKOtest.RData', envir = WT)
WTmed <- new.env()
#load('envWTmedial.RData', envir = WTmed)
CKO <- new.env()
load('envCKOtest.RData', envir = CKO)
CKOmed <- new.env()
#load('envCKOmiddle.RData', envir = CKO)

## For Joining WT and CKO data with no bins
# To join the variables from the environment to a single tibble q
# To add more environments of variables just add more unl, and change the geno and bin as necessary
unlWT <- unlist(WT$centers.my.rel[1])
tib.WT <- tibble(dist = unlWT, geno = 'Pax6', bin = 'All')
unlCKO <- unlist(CKO$centers.my.rel[1])
tib.CKO <- tibble(dist = unlCKO, geno = 'Tbr2', bin = 'All')

combo.dist <- bind_rows(tib.WT, tib.CKO)

## (7) Current plot method----
density.plot <- ggplot(data = combo.dist, aes(x = dist, fill = geno)) +
  theme_classic(base_size = 18) + 
  geom_density(kernel = 'gaussian', adjust = 0.3, alpha = 0.5) + #aes(y = ..count..)  for relative stuff
  labs(x = 'Relative Distance from VZ to Pia',
       y = 'Density of Cells',
       title = 'E16.5 Progenitor Distribution',
       fill = 'Marker') +
  scale_x_continuous(limits = c(0, 1),
                   breaks = seq(0, 1, 0.2)) +
  theme(legend.position = c(0.85, 0.7)) +
  geom_vline(xintercept = 0.65, linetype = 'dashed') + # Subplate
  #geom_vline(xintercept = 0.13, linetype = 'dashed') + # VZ border
  #geom_vline(xintercept = 0.05, linetype = 'dashed') + # Tbr2 dense lower
  #geom_vline(xintercept = 0.19, linetype = 'dashed') + # Tbr2 dense upper
  geom_vline(xintercept = 0.50, linetype = 'dashed') + # Tbr2 sparse upper
  geom_vline(xintercept = 0.64, linetype = 'dashed') + # NG1 lower E14.5
  geom_vline(xintercept = 0.77, linetype = 'dashed') # NG1 upper E14.5
density.plot



## (8) SAVE PLOT ----
ggsave(filename = 'Pax6Tbr2 S1 E16 EMBO rel.svg', device = 'svg', width = 5, height = 4, units = 'in')
ggsave(filename = 'Pax6Tbr2 S1 E16 EMBO rel.png', device = 'png', width = 5, height = 4, units = 'in')

## COLORFUL plot Method with normalizing the numbers ---- 
density.plot <- ggplot(data = combo.dist, aes(x = dist, group = geno, fill = geno)) + #use group = desc(geno) if need to change order of the bars
  theme_classic(base_size = 16) + 
  geom_density(kernel = 'gaussian', adjust = 0.3, alpha = 0.5, aes(y = ..count../1000)) +
  labs(x = 'Relative Distance from VZ to Pia',
       y = 'Density of Cells',
       title = 'Sall1 Distribution at E16.5 - Putative V1',
       fill = 'Genotype') +
  scale_fill_manual(values = c('magenta', 'green')) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  theme(legend.position = c(0.85, 0.7)) +
  geom_vline(xintercept = 0.65, linetype = 'dashed') + # Subplate
  #geom_vline(xintercept = 0.13, linetype = 'dashed') + # VZ border
  #geom_vline(xintercept = 0.05, linetype = 'dashed') + # Tbr2 dense lower
  geom_vline(xintercept = 0.19, linetype = 'dashed') + # Tbr2 dense upper
  geom_vline(xintercept = 0.50, linetype = 'dashed') # Tbr2 sparse upper
density.plot
## For Removing some bins and such----
combo.filter <- filter(combo.dist, dist < 0.65 & dist > 0.5)
filter.WT <- filter(combo.filter, geno == 'WT')
filter.CKO <- filter(combo.filter, geno == 'CKO')
kst <- ks.test(filter.WT$dist, filter.CKO$dist)
kst
t <- t.test(filter.WT$dist, filter.CKO$dist)
t


