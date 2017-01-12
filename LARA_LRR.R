####------------------DSL Project: Opinion Mining---------------------####
####---------Code for Latent Aspect Rating Regression (LRR)-----------####

set.seed(1279)
require(MASS)
require(lbfgs)
library(Matrix)
require(OpenMx)

#alphahat is d*k matrix with intialized alphahat value
#aspectrate is calculated from intialized beta, and freq matrix of matrix where each element is matrix of K*N
#reviewrate is d*1 vector of reviews
#sigma, beta is intialized parameter

# List of Word Frequecy matrix coming from Aspect segmenter
freq<-list_Wd
k <- dim(freq[[1]])[1]
d<- length(freq)
n<- dim(freq[[1]])[2]

# delta is less than 1/pi
delta<-1.8
# intialization of mu (based on obsevations over rating)
mu <- matrix(rep(1/k),nrow = 1, ncol=k)
# intialization of Sigma (diagonal matrix here)
sigma<-diag(k)

# alternative
#sigma<-matrix(c(rnorm(k*k,mean=0,sd=10)), ncol=k, nrow=k)
#sigma <- forceSymmetric(sigma)

# pi value (from manual)
pi_val<-.5

# original reviewrate coming from data
#reviewrate<-c(rnorm(d,3.5,0.5))
reviewrate <- reviews$overall[1:d]

# Multivariate normal draws with mu and sigma
alphahat = mvrnorm(n = d, mu, sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
# making copy
alphahat_copy <- alphahat

# lambda
lambda<-2

# intialization of beta matrix (normal approximation)
beta<-matrix(c(rnorm(n*k,mean=0,sd=5)), ncol=n, nrow=k)
beta_copy <- beta


#################################cost functions and grad functions#########################################################

# Takes alphahat vector, returns scaler costalpha
costalpha <- function(alphahat)
{ 
  alpha_local <-matrix(0,nrow=1,ncol=k)
  costalpha<-0
  # alpha calculation
  for(j in 1:k)
  {
    alpha_local[j]<-exp(alphahat[j])/sum(exp(alphahat))
  }

  costalpha_local = (alpha_local%*%as.matrix(aspectrate[rev,])-reviewrate[rev])^2/delta
    +pi_val*sum((aspectrate[rev,]-reviewrate[rev])^2*alpha_local)
    +(alphahat-mu)%*%solve(sigma)%*%t(alphahat-mu)

  return(costalpha_local)
}

# Takes alphahat vector, returns gradient vector 
gradientalpha <- function(alphahat)
{
  grad_local <-matrix(0, nrow=1, ncol=k)
  alpha_local <-matrix(0,nrow=1,ncol=k)
  for(j in 1:k)
  {
    alpha_local[j]<-exp(alphahat[j])/sum(exp(alphahat))
  }
  grad1 <- 0
  grad2 <- 0
  i=1
  while(i<=k)
  { for(j in 1:k)
    { if(i==j)
      { grad1 <- grad1 + alpha_local[i]*aspectrate[rev,i]*(1-alpha_local[i])
        grad2 <- grad2 + alpha_local[i]*(aspectrate[rev,i]-reviewrate[rev])^2*(1-alpha_local[i])
       }
    if(i!=j)
      {grad1 <- grad1 - alpha_local[i]*aspectrate[rev,j]*alpha_local[j]
      grad2 <- grad2 - alpha_local[i]*(aspectrate[rev,j]-reviewrate[rev])^2*alpha_local[j]
      }
    }
    grad_local[i]<-2*(alpha_local%*%as.matrix(aspectrate[rev,])-reviewrate[rev])/delta*grad1+pi_val*grad2+2*sum((solve(sigma)[i,]*(alphahat-mu)))
    i <- i+1
  }
  return(grad_local)
}



####################################Overall parameters mu, sigma, delta, beta################################################

# mu estimate for EM, alpha is the matrix
muestimate <- function(alpha)
{
  alphasum<-matrix(0,nrow=1, ncol=k)
  for(i in 1:d){alphasum <- alphasum+alpha[i,]}
  mu <-1/d*alphasum
  return(mu)
}

# sigma estimate for EM
sigmaestimate <-function(alpha,mu)
{
  sigmasum <- matrix(0,nrow = k, ncol = k)
  for(i in 1:d){sigmasum<-sigmasum+t(alpha[i,]-mu)%*%(alpha[i,]-mu)}
  sigma<-1/d*sigmasum
  sigma = sigma + diag(k)
  #sigma <- vec2diag(diagonal)
  return(sigma)
}

# delta estimate for EM
deltaestimate <- function(alpha,reviewrate,aspectrate)
{
  deltasum <- 0
  for(i in 1:d){deltasum <- deltasum +(reviewrate[i]-(alpha[i,]%*%aspectrate[i,]))^2}
  delta<-1/d*deltasum
  return(delta)
}

# Cost and Gradient function for beta 

# takes beta vector and returns scaler costbeta
costbeta <- function(beta)
{ 
  cost <- 0
  for(i in 1:d)
  {
    cost <- cost+(alpha_global[i,]%*%aspectrate[i,]-reviewrate[i])^2/delta+
      pi_val*sum((aspectrate[i,]-reviewrate[i])^2*alpha_global[i,])
  }
  #costbeta <- cost-lambda*beta%*%beta
  costbeta <- cost
  
  return(costbeta[1,1])
}

# takes beta vector and returns gradbeta vector 
gradientbeta <- function(beta)
{
  grad_beta <- vector(length=n)
  for(i in 1:d)
  {
    #grad_beta <- grad_beta + 2*alpha_global[i,aspect_no]*((alpha_global[i,]%*%aspectrate[i,])[1,1]-reviewrate[i])/delta+
     # pi_val*(aspectrate[i,aspect_no]-reviewrate[i])*aspectrate[i,aspect_no]*freq[[i]][aspect_no,]-2*lambda*beta}
  grad_beta <- grad_beta + 2*alpha_global[i,aspect_no]*((alpha_global[i,]%*%aspectrate[i,])[1,1]-reviewrate[i])/delta+
    pi_val*(aspectrate[i,aspect_no]-reviewrate[i])*aspectrate[i,aspect_no]*freq[[i]][aspect_no,]}
  return(grad_beta)
}

##################################aspectrate calculation####################################################
aspectrate_calc <- function(freq, beta)
{ 
  aspectrate = matrix(0, nrow = d, ncol= k)
  for(i in 1:d)
  {
    for(j in 1:k)
    { # exponential
      aspectrate[i,j]<-exp(sum(beta[j,]*(freq[[i]][j,])))
      # square
      #aspectrate[i,j]<-(sum(beta[j,]*freq[[i]][j,]))^2
    }
  }
return(aspectrate)
}



################################ Final loop for EM ##############################

error = 10000000000000000000000000
count = 0
Likelihood = 0 

while(error > 0.000001)
  
{ 
Likelihood_prev = Likelihood
count = count + 1
print(count)
    
# 1. aspectrate
aspectrate <- aspectrate_calc(freq,beta)
#aspectrate <- matrix(c(rnorm(d*k, mean = 3.5, sd = 0.5)), ncol = k, nrow = d)

# 2. Alpha optimization
convergence_alpha = c()
alpha_iter = c()
for(rev in 1:d){
  rev = rev
  output_alpha<-optim(alphahat[rev,], costalpha, gr = NULL, method = "BFGS")
  alphahat[rev, ] = output_alpha$par
  convergence_alpha = append(convergence_alpha, output_alpha$convergence)
  alpha_iter = append(alpha_iter, output_alpha$count[1])
}

# alpha calculation (alpha remains global)
alpha_global<-matrix(0,ncol=k, nrow=d)
for(j in 1:d)
{
  for(i in 1:k)
  {
    alpha_global[j,i]<-exp(alphahat[j,i])/sum(exp(alphahat[j,]))
  }
}

#3. Likelihood 

L <- c()  

for (i in  1:d){   
  L_review = - log(delta) - (1/delta)*((alpha_global[i,]%*%aspectrate[i,])-reviewrate[i])^2
              - log(det(sigma)) - (alphahat[i,]-mu)%*%solve(sigma)%*%t(alphahat[i,]-mu) 
              + pi_val*sum(((aspectrate[i,]-reviewrate[i])^2)*alpha_global[i,])
  L <- append(L, L_review)
}

Likelihood = sum(L)

error = abs(Likelihood_prev - Likelihood)

# 4. mu

mu = muestimate(alpha_global)

# 5. sigma

sigma = sigmaestimate(alpha_global, mu)

# 6. delta

delta = deltaestimate(alpha_global, reviewrate, aspectrate)

# 7. beta

beta_iter = c()
convergence_beta = c()
for (aspect_no in 1:k){
  aspect_no = aspect_no  
  beta_output<-optim(beta[aspect_no,], costbeta, gr = NULL, method= "BFGS")
  convergence_beta = append(convergence_beta, beta_output$convergence)
  beta[aspect_no,]<- beta_output$par
  beta_iter = append(beta_iter, beta_output$count[1])
}
  
}

total_aspect_weights = colSums(alpha_global)
total_aspect_weights = total_aspect_weights/d
write.csv(total_aspect_weights, "aspect_weights.csv")  

error = c()
for(i in 1:nrow(reviews)){
  sum(aspectrate[i,]*alpha_global[i,])
  error = append(error,print(sum(aspectrate[i,]*alpha_global[i,])-reviewrate[i]))
  error = sum(error)/d
}

MAD = sum(abs(error))/d
print(MAD)

