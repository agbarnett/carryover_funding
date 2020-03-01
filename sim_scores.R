# sim_scores.R
# function to simulate scores using multiple panel members
# who score significance, track record, and scientific quality
# Feb 2020

sim.scores = function(N, rev, weights=c(0.25,0.25,0.5)){
  n.simulated.models = ncol(alpha) # number of estimates from the simulated model
  # simulate error and person effect
  error = person.effect = NULL
  for (sim.num in 1:n.simulated.models){ # loop through different Bayesian estimates
    varcov.matrix = Sigma[,,sim.num]
    mu = alpha[,sim.num]
    sd = sigma.beta[sim.num]
    this.error = mvrnorm(mu=c(0,0,0), Sigma = varcov.matrix, n=ceiling((N*rev)/n.simulated.models)) # random correlated error using multivariate normal
    error = bind_rows(error, data.frame(this.error))
    this.person.effect = rnorm(ceiling(N/n.simulated.models), mean=0, sd=sd) # random intercept (person effect)
    person.effect = c(person.effect, this.person.effect)
  }
  error = error[1:(N*rev),] # remove few extras
  person.effect = person.effect[1:N] # remove few extras
  gender = rbinom(prob=prop.male, size = 1, n = N) # random gender
  
  sim = mutate(error, 
               id = 1+ floor((1:(N*rev)-1)/rev),
               gender = gender[id], 
               person.effect = person.effect[id],
               id = as.character(id), # helps with later conversion
               signif=mu[1], track=mu[2], quality=mu[3],
               signif = round(signif + person.effect + X1),
               track = round(track + person.effect + X2),
               quality = round(quality + person.effect + X3),
               signif = ifelse(signif>=8, 7, signif), # highest score is 7
               track = ifelse(track>=8, 7, track),
               quality = ifelse(quality>=8, 7, quality),
               score = weights[1]*signif + weights[2]*track + weights[3]*quality) # weighted score
  
  return(sim)
}
