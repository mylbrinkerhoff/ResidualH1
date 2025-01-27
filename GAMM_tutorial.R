###############################################################################
#
#
#   GAMM_tutorial.R
#
#   
#
#   M. Brinkerhoff  * UCSC  * 2024.05.10 (F)
#
###############################################################################

# Libraries that are required
library(ggplot2)
library(mgcv)
library(itsadug)
source("gamm_hacks.r")

# Loading Data
words.50 <- read.csv("words_50.csv")
traj <- read.csv("traj.csv")
traj.50 <- read.csv("traj_50.csv")
traj.random <- read.csv("traj_random.csv")

# Plotting to see what the raw data looks like
ggplot(words.50, aes(x = measurement.no,
                     y = f2,
                     group = traj,
                     alpha = duration)) +
         facet_grid(~word) +
         geom_line()

# model with separate smooths
words.50$word <- as.factor(words.50$word)
words.50.gam.sep <- bam(f2 ~ word + 
                          s(measurement.no, by = word, bs = "cr"),
                        data = words.50, method = "ML")
# This is a handy way to focus on a portion of the results
# However, you should use the summary() instead in most cases
summary.coefs(words.50.gam.sep) 
summary(words.50.gam.sep)

# model with smooth for A & difference smooth
words.50$word.ord <- as.ordered(words.50$word) 
contrasts(words.50$word.ord) <- "contr.treatment" 
words.50.gam.diff <- bam(f2 ~ word.ord + s(measurement.no, bs="cr") +
                           s(measurement.no, by=word.ord, bs="cr"),
                         data=words.50, 
                         method="ML")
summary.coefs(words.50.gam.diff)

# fitting a nested model w/o the difference smooth
words.50.gam.diff.0 <- bam(f2 ~ s(measurement.no, bs="cr"),
                           data = words.50, method = "ML")

# model comparison using the compareML() function from itsadug 
# this is very similar to the anova() function, but better suited 
# to models fitted using bam(); some parts of the output are suppressed
compareML(words.50.gam.diff, words.50.gam.diff.0, print.output = F)$table

# plot of the model predictions and the difference smooth
plot_smooth(words.50.gam.diff, view = "measurement.no",
            plot_all = "word.ord", rug = F)
plot_diff(words.50.gam.diff, view = "measurement.no",
          comp = list(word.ord=c("B","A")))

# Accounting for the 