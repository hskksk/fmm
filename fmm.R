library(rstan)
rstan_options(auto_write=T)

model = "
data {
  int<lower=1> N;
  int<lower=1> k;
  real X[N];
}
parameters {
  simplex[k] theta;
  real mu[k];
  real sigma[k];
}
model {
  real ps[k];
  for (i in 1:k){
    mu[i] ~ normal(0, 1.0e+2);
    sigma[i] ~ inv_gamma(0.001, 0.001); 
  }
  for(i in 1:N){
    for(j in 1:k){
      ps[j] <- log(theta[j]) + normal_lpdf(X[i] | mu[j], sigma[j]);
    }
    increment_log_prob(log_sum_exp(ps));
  }
}
//generated quantities {
//  row_vector[k] tmp;
//  matrix[N,k] probs;
//  for (i in 1:N) {
//    for (j in 1:k) {
//      tmp[j] = theta[j] .* exp(normal_lpdf(X[i] | mu[j], sigma[j]));
//    }
//    probs[i,] = tmp / sum(tmp);
//  }
//}
"

smodel = stan_model(model_code=model, verbose=T)

x = c(rnorm(10, 3, 4), rnorm(20, 0,1), rnorm(40, -1, 2))
dat = list(X=x, N=length(x), k=3)
fit = sampling(smodel, data=dat)
