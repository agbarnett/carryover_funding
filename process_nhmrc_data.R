# process_nhmrc_data.R
# process the NHMRC data for the simulation in simulate_carry_over_women.R
# run a bayesian model to estimate the application random intercepts and remaining correlation
# Feb 2020
library(readxl)
library(dplyr)
library(R2WinBUGS)
data = read_excel('../BMJ.analysis/2009 Projects - Assessor Scores and original 1sp scores.xls', skip=2)
names(data) = c('idx','signif','track','quality','role')
data = mutate(data, 
              id = as.numeric(as.factor(idx))) %>% # convert ID to integer
  dplyr::select(-idx)
save(data, file='data/original.RData')

# create external text file with bugs model
mfile = 'bugs.model.txt'
bugs = file(mfile, 'w')
cat('model{
    for (i in 1:N){
       matrix[i, 1:P] ~ dmnorm(mu[i, 1:P], Omega[1:P, 1:P]) # multivariate normal
       for (k in 1:3){
         mu[i, k] <- alpha[k] + beta.c[i] 
       }
       beta[i] ~ dnorm(0, tau.beta) # random applicant effect
       beta.c[i] <- beta[i] - mu.beta # centre on zero
    }
    Omega[1:P, 1:P] ~ dwish(R[1:P, 1:P], P)
    Sigma[1:P, 1:P] <- inverse(Omega[1:P, 1:P])
    for (k in 1:3){ # overall means
       alpha[k] ~ dnorm(0, 0.001)
    }
    tau.beta ~ dgamma(1, 1)
    sigma.beta <- 1/sqrt(tau.beta)
    mu.beta <- mean(beta[1:N])
}\n', file=bugs)
close(bugs)

# prepare the data
#data = sample_n(data, 500)
N = nrow(data)
P = 3 
matrix = as.matrix(dplyr::select(data,'signif','track','quality'))
bdata = list(N = N, P = P, R = diag(P), matrix=matrix)
inits = list(list(alpha=c(4.869784,5.118561,4.564460), Omega=diag(P), tau.beta=1), 
             list(alpha=c(4.869784,5.118561,4.564460), Omega=diag(P), tau.beta=1)) # initial values (two sets); use decent starting values for alpha

# run the regression model in BUGS
parms = c('alpha','Sigma', 'sigma.beta')
n.thin = 3
n.sample = 2000
bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=mfile,
                     n.chains=2, n.thin=n.thin, n.iter=n.sample*n.thin, debug=TRUE, DIC=FALSE,
                     bugs.directory="c:/Program Files/WinBUGS14")
bugs.results$summary

# save the parameter estimates needed for the simulation
Sigma = as.matrix(bugs.results$mean)
save(mu, Sigma, file='WinBUGS.Results.RData')

# export for JAGS
save(bdata, inits, mfile, parms, file='Z:/carryover/jagsready.RData', version = 2)
