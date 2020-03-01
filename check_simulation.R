# check_simulation.R
# check the function to simulate peer review scores by comparing it with the real data
# Feb 2020
library(MASS)
library(ggplot2)
source('sim_scores.R') # function to simulate scores

# get the observed data
load(file='data/original.RData')
original = mutate(data, score = 0.25*signif + 0.25*track + 0.5*quality) %>% # weighted score
  group_by(id) %>%
  mutate(overall = mean(score)) %>%
  ungroup() %>%
  mutate(simnum=0) # to help with plot

# calculate overall score per reviewers
original.scores = group_by(original, simnum, id) %>%
  summarise(mean = mean(overall)) %>%
  ungroup()
# add original funding line
funding.line = quantile(original.scores$mean, probs=0.8) # 20% funding

# get the estimates to inform the simulations
load('Z:/carryover/results.chains.RData')
alpha = alpha + 5 # add back mean

# run 50 simulations
N = 2899 # to match observed data
success = 0.2 # success proportion
rev = 3 # reviewers per applicant
prop.male = 0.6 # proportion of applicants that are male 
simulations = NULL
for (i in 1:50){
  # a) scores per reviewer
  scores = sim.scores(N=N, rev=rev) %>% # call function to simulate scores
    group_by(id) %>%
    mutate(overall = mean(score)) %>%
    ungroup() %>%
    mutate(simnum = i)
  simulated.scores = group_by(scores, simnum, id) %>%
    summarise(mean = mean(overall)) %>%
    ungroup()
  # b) overall score per applicant
  simulations = bind_rows(simulations, simulated.scores)
}

# compare using emprical CDF
eplot = ggplot(data=simulations, aes(x=sim.scores, group=factor(simnum))) +
  stat_ecdf(col=grey(0.5))+
  stat_ecdf(data=original.scores, aes(x=overall), col='red')+
  theme_bw()+
  theme(legend.position = 'none')+
  xlab('Overall score')+
  ylab('Cumulative proportion')
eplot

# export figure
jpeg('figures/compareSimECDF.jpg', width=4, height=4, units='in', res=400)
print(eplot)
dev.off()

# histograms
dplot = ggplot(data=simulations, aes(x=overall, group=factor(simnum))) +
  geom_density(col=grey(0.5))+
  geom_density(data=original, aes(x=overall), col='red')+
  theme_bw()+
  theme(legend.position = 'none')+
  xlab('Overall score')+
  ylab('Density')
dplot
# export figure
jpeg('figures/compareSimDensity.jpg', width=4, height=4, units='in', res=400)
print(dplot)
dev.off()