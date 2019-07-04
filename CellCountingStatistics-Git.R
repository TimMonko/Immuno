# Tim Monko 03/14/2018----
# Rewritten for GIT 06/07/2019
# Cell Counting Statistics. This purpose is to use the databases from cell counting in organizing, grouping, and statting my data. 

# Be sure to set proper working directory. 
# Shorcut to set: CTRL+SHFT+H. 
# To use the UI (on Mac) Session -> Set Working Directory -> Choose directory
# Reminder that CTRL+ALT+T runs a code "section" (a comment which ends with "----" begins a section)

library(tidyverse) # Includes ggplot2, tidyr, dplyr, stringr, readr, tibble, purrr, forcats, broom 
library(ggpubr)
# library(svglite)
library(broom)
rm(list = ls())

## ----
rm(list = ls())
rawdata <- read.csv("E16Sall1Tbr2.csv") # Set the file that you want to read here

## VARIABLES TO DEFINE ----

dep.variable <- quo(SVZ) #used for t-test and MANOVA analysis (as first grouping variable)
# dep.variable.2 <- quo(ROR) #used for MANOVA analysis as the second grouping variable
Bin1 <- 'Cmed' # Bin for separation, such as S1 or Caudal, or 1 etc
Bin2 <- 'Cmid' 

group.test <- rawdata %>% 
  filter(is.na(rawdata$Ifbad.1)) %>%
  drop_na(SVZ) %>%
  group_by(Level, WT.cKO, Pair)



grouped_var <- function(dataframe, var.of.interest) {
  #grouped_var2 allows the user to define the variable of interest about with the quo(arg) call. then the !! var.of.interest removes the special quosure to be evaluted in the functions, to be used only for upper level function calls. Otherwise use grouped_var original
  grouped.var <- 
    dataframe %>% # Calls the data.frame of interest
    filter(is.na(rawdata$Ifbad.1)) %>% 
    #filter(dataframe$Ifbad.1 != 1) %>%  ## deprecated? 
    drop_na(!! var.of.interest) %>% # Removes all rows with NAs,  could use filter(!is.na(Ctip2)), but drop_na allows for dropping based on multiple criteria
    group_by(Level, WT.cKO, Pair) %>% # Groups the rawdata according to a variable LEVEL may go here
    summarize(var.mean = mean(!! var.of.interest)) # Used for functions performed on the grouped data
  return(grouped.var)
}
subset_var <- function(grouped.var, WTcKO, Bin) {
  subset(grouped.var, WT.cKO == WTcKO & Level == Bin) # Subset may present an issue. Pay attention to S1.V1 
}
ttest_var <- function(means.1, means.2, equalvari = TRUE, paird = TRUE) {
  tidy(t.test(means.1$var.mean, means.2$var.mean, var.equal = equalvari, paired = paird))
}

wilcoxtest_var <- function(means.1, means.2) {
  tidy(wilcox.test(means.1$var.mean, means.2$var.mean, paired = TRUE))
}
## DATA MANIPULATION AND TTEST STATISTICS
###---###

group.var <- grouped_var(rawdata, dep.variable) # Pools means for each grouping (in this case WTcKO and S1/V1 and Pair) for a certain variable 

means.WT.Bin1  <- subset_var(group.var, 1, Bin1) # Distributes the means to a variable for calling on in a t-test
means.cKO.Bin1 <- subset_var(group.var, 2, Bin1)
means.WT.Bin2  <- subset_var(group.var, 1, Bin2)
means.cKO.Bin2 <- subset_var(group.var, 2, Bin2)

means.WT.comboBin <- bind_rows(means.WT.Bin1, means.WT.Bin2) %>% 
  group_by(WT.cKO, Pair) %>%
  summarize(var.mean = sum(var.mean))
means.cKO.comboBin <- bind_rows(means.cKO.Bin1, means.cKO.Bin2) %>%
  group_by(WT.cKO, Pair) %>%
  summarize(var.mean = sum(var.mean))

means.Bin1 <- bind_rows(means.WT.Bin1, means.cKO.Bin1) # These get combined for the purposes of making figures, so that they are grouped variables in one tibble 
means.Bin2 <- bind_rows(means.WT.Bin2, means.cKO.Bin2)
means.comboBin <- bind_rows(means.WT.comboBin, means.cKO.comboBin)

p.t.Bin1 <- ttest_var(means.WT.Bin1, means.cKO.Bin1) # Paired T-test 
p.t.Bin2 <- ttest_var(means.WT.Bin2, means.cKO.Bin2)
p.t.comboBin <- ttest_var(means.WT.comboBin, means.cKO.comboBin)
p.w.Bin1 <- wilcoxtest_var(means.WT.Bin1, means.cKO.Bin1) # This needs fixed 
p.w.Bin2 <- wilcoxtest_var(means.WT.Bin2, means.cKO.Bin2)
p.w.comboBin <- wilcoxtest_var(means.WT.comboBin, means.cKO.comboBin)

test.results <- bind_rows(
  't.Bin1' = p.t.Bin1, 
  't.Bin2' = p.t.Bin2,
  't.combo' = p.t.comboBin,
  'w.Bin1' = p.w.Bin1,
  'w.Bin2' = p.w.Bin2,
  'w.combo' = p.w.comboBin,
  .id = 'Test')

# PLOTS FOR QUICK VISUALIZATION

plot.Bin1 <- ggplot(means.Bin1, aes(x = WT.cKO, y = var.mean)) +
  theme_classic(base_size = 24) +
  geom_point(color = means.Bin1$WT.cKO, size = 1.2) +
  labs(title = 'Bin 1',
       y = 'Means') +   
  geom_line(aes(group = means.Bin1$Pair)) +
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("WT", "cKO"), 
                     limits = c(0.8,2.2)) +
  annotate("text", x = 1.5, y = max(means.Bin1$var.mean) + 0.01*max(means.Bin1$var.mean),
           label = if (p.t.Bin1$p.value > .001) {
             paste("p = ", formatC(p.t.Bin1$p.value, digits = 3, format = "f"))
           } else {" p < .001"},
           size = 3)


plot.Bin2 <- ggplot(means.Bin2, aes(x = WT.cKO, y = var.mean)) +
  theme_classic(base_size = 24) +
  geom_point(color = means.Bin2$WT.cKO, size = 1.2) +
  labs(title = 'Bin 2',
       y = 'Means') +   
  geom_line(aes(group = means.Bin2$Pair)) +
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("WT", "cKO"), 
                     limits = c(0.8,2.2)) +
  annotate("text", x = 1.5, y = max(means.Bin2$var.mean) + 0.01*max(means.Bin2$var.mean),
           label = if (p.t.Bin2$p.value > .001) {
             paste("p = ", formatC(p.t.Bin2$p.value, digits = 3, format = "f"))
           } else {" p < .001"},
           size = 3)

plot.comboBin <- ggplot(means.comboBin, aes(x = WT.cKO, y = var.mean)) +
  theme_classic(base_size = 24) +
  geom_point(color = means.comboBin$WT.cKO, size = 1.2) +
  labs(title = 'Bin Combo',
       y = 'Means') +   
  geom_line(aes(group = means.comboBin$Pair)) +
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("WT", "cKO"), 
                     limits = c(0.8,2.2)) +
  annotate("text", x = 1.5, y = max(means.comboBin$var.mean) + 0.01*max(means.comboBin$var.mean),
           label = if (p.t.comboBin$p.value > .001) {
             paste("p = ", formatC(p.t.comboBin$p.value, digits = 3, format = "f"))
           } else {" p < .001"},
           size = 3)

plot(plot.Bin1)
plot(plot.Bin2)
plot(plot.comboBin)

test.results

## PLOTTING FOR PUBLISHINGS, WTIH FXN STYLE INPUTS ----
###---###
###---###

file.name.saveas <- "P8-Gbx2-V1-CC3deep.svg"
plot.df <- means.Bin2
# Bin <- 2
labs.title <- "V1"
labs.x <- "Genotype"
labs.y <- "Pia-to-ventricle, μm"

data.x <- plot.df$WT.cKO
data.y <- plot.df$var.mean
theme.size <- 10
plot.color <- plot.df$WT.cKO
line.group <- plot.df$Pair
#p.size <- 3

# Plot the graph with above parameters. Change the target if you want to do and store multiple graphs for a later multiplot function
print.plot <- ggplot(plot.df, aes(x = data.x, y = data.y)) +
  theme_classic(base_size = theme.size) +
  geom_point(color = plot.color, 
             size = 1) + 
  labs(title = labs.title,
       x = labs.x,
       y = labs.y) + 
  geom_line(aes(group = line.group)) +
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("CTRL", "cKO"), 
                     limits = c(0.8,2.2)) 
#  annotate("text", x = 1.5, y = max(plot.df$var.mean),
#           label = if (test.results$p.value[Bin] < .05) {
#           paste("*") })

print.plot
## For saving the most recently called plot ----

ggsave(filename = file.name.saveas, device = "svg", width = 3, height = 4, units = "cm") # Yasushi likes 3x4 the best 


## OLD MANOVA CODE, KEPT INCASE NEEDED IN FUTURE ----
####
#If running MANOVA, be sure to analyze above section 

group.var2 <- grouped_var2(rawdata, voi2)

means2.WT.S1  <- means_var(group.var2, 1, 1)
means2.cKO.S1 <- means_var(group.var2, 2, 1)
means2.WT.V1  <- means_var(group.var2, 1, 2)
means2.cKO.V1 <- means_var(group.var2, 2, 2)

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
