# plot_results.R
# plot the results from the carry-over simulations
# March 2020
library(ggplot2)
library(dplyr)
library(gridExtra)

# get the data
file = 'sim.results.RData'
load(paste('data/', file, sep='')) # load simulations results from simulate_carry_over_women.R

# no increase in females, standard weights, close line to gap
filter(results, prop.male==0.70, w1==0.25, gap==0.3, success==0.2)

## plot results by gap
# a) number of women
to.plot = filter(results, prop.male==0.70, w1==0.25, success==0.2)
num.plot = ggplot(data=to.plot, aes(x=gap, y=counts.median, ymin=counts.Q1, ymax=counts.Q3))+
  geom_point(col='violetred4', size=4)+
  geom_errorbar(lwd=1.2, width=0, col='violetred4')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(breaks=c(5,10,15))+
  scale_x_continuous(breaks=seq(0.1,0.3,0.1))+
  ylab('Women funded in carry-over round')+
  xlab('Gap in score from funding line')
# b) success proportion
p.plot = ggplot(data=to.plot, aes(x=gap, y=winner.p))+
  geom_point(col='dark blue', size=4)+
  geom_line(lwd=1.1, col='dark blue')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0.1,0.3,0.1))+
  ylab('Proportion of women who were\nsuccessful in carry-over round')+
  xlab('Gap in score from funding line')

# 2-by-1 plot
grid.arrange(num.plot, p.plot, nrow = 1)
