# Tim Monko 03/14/2018 ----
# Cell Counting Statistics. This purpose is to use the databases from cell counting in organizing, grouping, and statting my data. 

# Be sure to set proper working directory. 
# Shorcut to set: CTRL+SHFT+H. 
# To use the UI (on Mac) Session -> Set Working Directory -> Choose directory
# Reminder that CTRL+ALT+T runs a code "section" (a comment which ends with "----" begins a section)

library(tidyverse) # Includes ggplot2, tidyr, dplyr, stringr, readr, tibble, purrr, forcats
library(broom)
library(ggpubr)
library(svglite)
rm(list = ls())


## VARIABLES TO DEFINE ----

rawdata <- read.csv("Gbx2P8PU1.csv") # Set the file that you want to read here
# MAC-OS, right click, hold ALT and "copy "...." as Pathname"
 
voi <- quo(density) #used for t-test and MANOVA analysis (as first grouping variable)
voi2 <- quo(IZ) #used for MANOVA analysis as the second grouping variable

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
grouped_var <- function(dataframe, var.of.interest) {
  #grouped_var2 allows the user to define the variable of interest about with the quo(arg) call. then the !! var.of.interest removes the special quosure to be evaluted in the functions, to be used only for upper level function calls. Otherwise use grouped_var original
  var3 <- 
    dataframe %>% # Calls the data.frame of interest
    drop_na(!! var.of.interest) %>% # Removes all rows with NAs,  could use filter(!is.na(Ctip2)), but drop_na allows for dropping based on multiple criteria
    group_by(Level, WT.cKO, Pair) %>% # Groups the rawdata according to a variable
    summarize(var.mean = mean(!! var.of.interest)) # Used for functions performed on the grouped data
  return(var3)
}
means_var <- function(grouped.var, WTcKO, levels) {
  subset(grouped.var, WT.cKO == WTcKO & Level == levels)
}
ttest_var <- function(means.1, means.2, equalvari = TRUE, paird = TRUE) {
  tidy(t.test(means.1$var.mean, means.2$var.mean, var.equal = equalvari, paired = paird))
}

# I am still unsure how to interpret a MANOVA, so this will not be used too much at this time 

## DATA MANIPULATION AND TTEST STATISTICS
###---###

group.var <- grouped_var(rawdata, voi) # Pools means for each grouping (in this case WTcKO and S1/V1 and Pair) for a certain variable 

means.WT.S1  <- means_var(group.var, 1, 'CM') # Distributes the means to a variable for calling on in a t-test
means.cKO.S1 <- means_var(group.var, 2, 'CM')
means.WT.V1  <- means_var(group.var, 1, 'CL')
means.cKO.V1 <- means_var(group.var, 2, 'CL')
means.WT.combo <- bind_rows(means.WT.S1, means.WT.V1) %>% 
  group_by(WT.cKO, Pair) %>%
  summarize(var.mean = sum(var.mean))
means.cKO.combo <- bind_rows(means.cKO.S1, means.cKO.V1) %>%
  group_by(WT.cKO, Pair) %>%
  summarize(var.mean = sum(var.mean))

p.S1 <- ttest_var(means.WT.S1, means.cKO.S1)
p.V1 <- ttest_var(means.WT.V1, means.cKO.V1)

p.combo.t <- tidy(t.test(means.WT.combo$var.mean, means.cKO.combo$var.mean, var.equal = TRUE, paired = TRUE))
p.combo.MW <- tidy(wilcox.test(means.WT.combo$var.mean, means.cKO.combo$var.mean, paired = TRUE))

means.S1 <- bind_rows(means.WT.S1, means.cKO.S1)
means.V1 <- bind_rows(means.WT.V1, means.cKO.V1)
means.combo <- bind_rows(means.WT.combo, means.cKO.combo)


p.S1
p.V1
p.combo.t
p.combo.MW

## MANOVA Statistics ---- 
####
#If running MANOVA, be sure to analyze above section 

group.var <- grouped_var2(rawdata, voi2)

means2.WT.S1  <- means_var(group.var2, 1, 'Cmed')
means2.cKO.S1 <- means_var(group.var2, 2, 'Cmed')
means2.WT.V1  <- means_var(group.var2, 1, 'CL')
means2.cKO.V1 <- means_var(group.var2, 2, 'CL')

p2.S1 <- ttest_var(means2.WT.S1, means2.cKO.S1)
p2.V1 <- ttest_var(means2.WT.V1, means2.cKO.V1)

means2.S1 <- bind_rows(means2.WT.S1, means2.cKO.S1)
means2.V1 <- bind_rows(means2.WT.V1, means2.cKO.V1)

manova.S1 <- manova(cbind(means.S1$var.mean, means2.S1$var.mean) ~ means.S1$WT.cKO)
# Output: p.value is at the end of the listing
tidy(manova.S1) 
# Output: "Response 1" = voi (1st variable of interest), Pr(>F) = p.value
#         "Response 2" = voi2
summary.aov(manova.S1)

manova.V1 <- manova(cbind(means.V1$var.mean, means2.V1$var.mean) ~ means.V1$WT.cKO)
tidy(manova.V1)
summary.aov(manova.V1)

p.S1
p.V1
p2.S1
p2.V1

## PLOTTING FOR PUBLISHINGS, WTIH FXN STYLE INPUTS ----
###---###
###---###

file.name.saveas <- "E16-Gbx2-S1-PH3apical.png"
plot.df <- means.combo
labs.title <- "S1"
labs.x <- "Genotype"
labs.y <- "Apical PH3+"
p.val <- p.V1$p.value

data.x <- plot.df$WT.cKO
data.y <- plot.df$var.mean
theme.size <- 24
plot.color <- plot.df$WT.cKO
line.group <- plot.df$Pair
p.size <- 3

plot.2 <- ggplot(plot.df, aes(x = data.x, y = data.y)) +
  theme_classic(base_size = theme.size) +
  geom_point(color = plot.color, 
             size = 3) + 
  labs(title = labs.title,
       x = labs.x,
       y = labs.y) + 
  geom_line(aes(group = line.group)) +
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("CTRL", "cKO"), 
                     limits = c(0.8,2.2)) #+
  #scale_y_continuous(limits = c(10,40))

plot.2
# Plot the graph with above parameters. Change the target if you want to do and store multiple graphs for a later multiplot function
## Old Plot Style ----
plot.1 <- ggplot(plot.df, aes(x = data.x, y = data.y)) +
  theme_classic(base_size = theme.size, base_family = 'Arial') +
  geom_point(color = plot.color, 
             size = 1.2) + 
  labs(title = labs.title,
       x = labs.x,
       y = labs.y) + 
  geom_line(aes(group = line.group)) +
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("WT", "cKO"), 
                     limits = c(0.8,2.2)) +
  scale_y_continuous(limits = c(0,1)) + 
  annotate("text", x = 1.5, y = max(data.y)+0.1,
           label = if (p.val > .001) {
             paste("p = ", formatC(p.val, digits = 3, format = "f"))
           } else {" p < .001"},
           size = p.size)

plot.1
## Newer Plot Style ----

plot.2 <- ggplot(plot.df, aes(x = data.x, y = data.y)) +
  theme_classic(base_size = theme.size) +
  geom_point(color = plot.color, 
             size = 3) + 
  labs(title = labs.title,
       x = labs.x,
       y = labs.y) + 
  geom_line(aes(group = line.group)) +
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("CTRL", "cKO"), 
                     limits = c(0.8,2.2)) +
  scale_y_continuous(limits = c(15,40))

plot.2
## For saving the most recently called plot ----

ggsave(filename = file.name.saveas, device = "png", width = 2.5, height = 4, units = "in")

## PLOTTING DOUBLE PLOTS FOR MANOVA ----
group.vars <- means.S1
group.vars$var2.mean <- means2.S1$var.mean

file.name.saveas <- "P8-Vgf-Cmed-PH3-Apical-Basal.eps"
plot.df <- means.V1
labs.title <- "V1"
labs.x <- "Apical"
labs.y <- "Basal"
p.val <- p.V1$p.value

data.x <- plot.df$WT.cKO
plot.df$WT.cKO <- plot.df$WT.cKO - .2
data.y <- plot.df$var.mean
theme.size <- 12
plot.color <- group.vars$WT.cKO
line.group <- plot.df$Pair
p.size <- 4


# Plot the graph with above parameters. Change the target if you want to do and store multiple graphs for a later multiplot function
plot.1 <- ggplot(data = group.vars, aes(x = var.mean, y = var2.mean)) +
  theme_classic(base_size = theme.size) +
  geom_point(color = plot.color) +
  labs(title = labs.title,
       x = labs.x,
       y = labs.y) + 
  geom_line(aes(group = line.group))

plot.1



#### ALTERNATIVE PLOTTING METHODS (old and no longer used)
