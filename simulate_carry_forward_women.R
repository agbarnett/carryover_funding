# simulate_carry_forward_women.R
# simulate the impact of carrying forward a score for female applicants in an annual funding round
# March 2020
library(dplyr)
library(MASS) # for random multivariate normal
library(stringr)
source('sim_scores.R') # function to simulate scores

# key numbers
n.simulated.models = 20 # number of simulated models from JAGS
success = 0.20 # success proportion
N = 1000 # total number of applicants
rev = 5 # reviewers per applicant (fewer ties in overall scores compared with 3)
gap = 0.3 # gap from funding line
prop.male = 0.65 # proportion of applicants that are male 
p.track.improve = 0.5 # probability that a reviewer increases the track record score for a carried-forward grant
N.sim = 500 # number of simulations to run
weights = c(0.25,0.25,0.5) # weights for significance (must add to 1), track record and scientific quality, standard = 25:25:50
#weights = c(0.33,0.33,0.34) # alternative weights with approximately equal weight for all three categories

# get the JAGS estimates to inform the simulations
load('data/results.chains.RData')
alpha = alpha + 5 # add back mean after centering from JAGS

## main loop
final.numbers = first.round.winners = NULL
for (s in 1:N.sim){
  # generate first round
  first.round = sim.scores(N=N, rev=rev) 
  # calculate score per person
  scores = group_by(first.round, gender, id) %>%
    summarise(overall = mean(score)) %>%
    ungroup() %>%
    arrange(-overall, id) %>% # rank high to low; any ties are randomly ordered by id
    mutate(rank = 1:n(),
           winner = rank <= success*N) # fixed number of winners
  # calculate funding line
  funding_line = filter(scores, winner==FALSE) %>%
    slice(1) 
  funding_line = funding_line$overall
  # success numbers by gender
  success.by.gender = group_by(scores, gender, winner) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(gender==0, winner==TRUE) %>% # just look at female numbers
    dplyr::select(-gender, -winner) %>%
    mutate(sim = s)
  first.round.winners = bind_rows(first.round.winners, success.by.gender)

  # find female applicants just below the funding line and carry forward
  just.outside = filter(scores, 
                        gender==0, 
                        overall <= funding_line, overall > funding_line - gap) %>%
    dplyr::select(id)
  carry.forward = left_join(just.outside, first.round, by='id') %>%
    mutate(track = track + rbinom(n=nrow(just.outside), size=1, prob=p.track.improve), # randomly improve track record 
           track = ifelse(track>=8, 7, track), # highest score is 7
           score = weights[1]*signif + weights[2]*track + weights[3]*quality, # recalculate score
           id = paste('carry', id, sep = '') ) # change their id
  
  # now simulate next year
  second.round = sim.scores(N=N, rev=rev) 
  combined = bind_rows(second.round, carry.forward) # add carried forward women
  # calculate score per person
  second.scores = group_by(combined, gender, id) %>%
    summarise(overall = mean(score)) %>%
    ungroup() %>%
    mutate(random.ties = runif(n())) %>% # for random times
    arrange(-overall, random.ties) %>% # rank high to low; any ties are randomly ordered
    mutate(rank = 1:n(),
           winner = rank <= success*N) # fixed number of winners because of fixed budget (needed to displace winners from first round)
  # check for carry-over winners
  check = filter(second.scores, str_detect(pattern='carry', string=id)) %>%
    group_by(winner) %>%
    summarise(sim = s,
              Count = n()) %>% # count the winners and losers
    ungroup()
  final.numbers = bind_rows(final.numbers, check)
} # end of main simulation loop

# add zero counts to carry-over winners
counts = tidyr::spread(final.numbers, winner, Count) %>%
  rename('winner' = `TRUE`,
         'loser' = `FALSE`) %>%
  mutate(winner = tidyr::replace_na(winner, 0),
         loser = tidyr::replace_na(loser, 0),
         n = winner + loser,
         prop = winner / (winner + loser)) %>% # success proportion for carry-over women
  dplyr::select(-sim)
# summary statistics for carry over winners
summary(counts)

# summary statistics for female winners in first round
summary(first.round.winners)

# append statistics to summary file
to.add = data.frame(success=success, N=N, gap=gap, rev=rev, prop.male=prop.male,
                    p.track.improve=p.track.improve, N.sim=N.sim,    
                    w1=weights[1], w2=weights[2], w3=weights[3],
                    counts.median = median(counts$winner), # female carry over winners
                    counts.Q1 = as.numeric(quantile(counts$winner, 0.25)),
                    counts.Q3 = as.numeric(quantile(counts$winner, 0.75)),
                    winner.p = mean(counts$prop), # proportion of women carried over who win funding
                    winner.median = median(first.round.winners$count), # female winners in first round
                    winner.Q1 = as.numeric(quantile(first.round.winners$count, 0.25)),
                    winner.Q3 = as.numeric(quantile(first.round.winners$count, 0.75))
                    
)
                    
## save the results                    
file = 'sim.results.RData'
# first results
if(length(dir('data', pattern = file)) == 0){
  results = to.add
  save(results, file=paste('data/', file, sep=''))
}
# appending
if(length(dir('data', pattern = file)) > 0){
  load(paste('data/', file, sep='')) # load previous results
  results = rbind(results, to.add)
  save(results, file=paste('data/', file, sep='')) # save updated results
}
