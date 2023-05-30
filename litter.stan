data {
  int<lower=0> N;
  int r[N];
  int n[N];
  real m;
  real <lower=0> v;
  real <lower=0> d;
  real <lower=0> a;
}

parameters {
  real mu;
  real<lower=0> sigmasq;
  real b[N];
}

transformed parameters {
  real<lower=0> sigma;
  sigma = sqrt(sigmasq); 
}

model {
  mu ~ normal(m, sqrt(v)); 
  sigmasq ~ inv_gamma(d, a);
  b ~ normal(mu, sigma);
  r ~ binomial_logit(n, b);
}

generated quantities {
  real pop_mean = inv_logit(mu);
  real<lower=0,upper=1> theta[N];
  for (i in 1:N)
    theta[i] = inv_logit(b[i]);
}
