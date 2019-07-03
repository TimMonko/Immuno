## Tim Monko 04/15/2019 ----
# Plotting of Cell Distribution Statistics, to be used after "RImageAnalysis" or on any cellular distribution data.
# Split from Previous RImageAnalysis to clean up the script and because they are largely independent when running, since data needs to be loaded into environments. 

# Be sure to set proper working directory. 
# Shorcut to set: CTRL+SHFT+H. 
# To use the UI (on Mac) Session -> Set Working Directory -> Choose directory
# Reminder that CTRL+ALT+T runs a code "section" (a comment which ends with "----" begins a section)

#Librarys 
library(tidyverse)

## Loading in Environments for keeping groups of data together ---- 
# .RData files can be loaded into environments, thus allowing a repetitively named data such as 'centers.my.rel' to be called on in the format of envName$variable.Name
# This feature is particularly useful for comparing data between groups that have been outputted after independent runs of a script, such as cell distribution data from 'RImageAnalysis'

WT <- new.env()
load('envWT.RData', envir = WT)
WTmed <- new.env()
#load('envWTmedial.RData', envir = WTmed)
CKO <- new.env()
load('envCKO.RData', envir = CKO)
CKOmed <- new.env()
#load('envCKOmiddle.RData', envir = CKO)

## For Joining WT and CKO data with no bins  ----
# To join the variables from the environment to a single tibble q
# To add more environments of variables just add more unl, and change the geno and bin as necessary
unlWT <- 1-unlist(WT$centers.my.rel[1])
tib.WT <- tibble(dist = unlWT, geno = 'WT', bin = 'All')

unlCKO <- 1-unlist(CKO$centers.my.rel[1])
tib.CKO <- tibble(dist = unlCKO, geno = 'CKO', bin = 'All')


combo.dist <- bind_rows(tib.WT, tib.CKO)

## OUTDATED, Kept for posterity ----
unl1 <- unlist(WT$centers.my.rel[1])
dist <- unl1
df1 <- tibble(dist)
df1 <- add_column(df1, geno = 'WT', bin = 'Mid')

unl2 <- unlist(CKO$centers.my.rel[1])
length(unl2) <- length(unl1)
dist <- unl1
df2 <- tibble(dist)
df2 <- add_column(df2, geno = 'cKO', bin = 'Mid')

unl3 <- unlist(WTmed$centers.my.rel[1])
length(unl3) <- length(unl1)
dist <- unl3
df3 <- tibble(dist)
df3 <- add_column(df3, geno = 'WT', bin = 'Med')

unl4 <- unlist(CKOmed$centers.my.rel[1])
length(unl4) <- length(unl1)
dist <- unl4
df4 <- tibble(dist)
df4 <- add_column(df4, geno = 'cKO', bin = 'Med')

combo.dist <- bind_rows(df1, df2)
combo.dist.allbins <- bind_rows(df1, df2, df3, df4)
# https://stackoverflow.com/questions/45098389/normalizing-y-axis-in-density-plots-in-r-ggplot-to-proportion-by-group


fill1 <- c('red', 'CKO')
kt1 <- ks.test(unl1, unl2)
## ----
## Current plot method----
density.plot <- ggplot(data = combo.dist, aes(x = dist, fill = geno)) +
  theme_classic(base_size = 16) + 
  geom_density(kernel = 'gaussian', adjust = 0.3, alpha = 0.5) +
  labs(x = 'Relative Distance from VZ to Pia',
       y = 'Density of Tbr2+ Cells',
       title = 'Tbr2 Distribution at E16.5 - Putative V1',
       fill = 'Genotype') +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  theme(legend.position = c(0.85, 0.7)) +
  geom_vline(xintercept = 0.65, linetype = 'dashed') + # Subplate
  #geom_vline(xintercept = 0.13, linetype = 'dashed') + # VZ border
  #geom_vline(xintercept = 0.05, linetype = 'dashed') + # Tbr2 dense lower
  geom_vline(xintercept = 0.19, linetype = 'dashed') + # Tbr2 dense upper
  geom_vline(xintercept = 0.50, linetype = 'dashed') # Tbr2 sparse upper
density.plot



## SAVE PLOT ----
ggsave(filename = 'Tbr2 V1 E16Vgf Dist Smaller2.svg', device = 'svg', width = 4, height = 3, units = 'in')

## For Removing some bins and such----
combo.filter <- filter(combo.dist, dist < 0.65 & dist > 0.5)
filter.WT <- filter(combo.filter, geno == 'WT')
filter.CKO <- filter(combo.filter, geno == 'CKO')
kst <- ks.test(filter.WT$dist, filter.CKO$dist)
kst
t <- t.test(filter.WT$dist, filter.CKO$dist)
t