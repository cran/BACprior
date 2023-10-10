\name{BACprior.lm}
\alias{BACprior.lm}


\title{
Sensitivity of the Bayesian Adjustment for Confounding Algorithm to the Value of the Hyperparameter Omega When the Outcome and the Exposure Are Continuous Variables.
}
\description{
The BACprior.lm function provides estimates of the posterior mean and posterior standard deviation of the exposure effect for selected values of omega in the covariate inclusion indicators' prior distribution. The output allows the user to evaluate how sensitive the BAC procedure is to the choice of the hyperparameter omega value.}

\usage{BACprior.lm(Y, X, U, 
omega = c(1, 1.1, 1.3, 1.6, 2, 5, 10, 30, 50, 100, Inf),
maxmodels = 150, cutoff = 0.0001, return.best = FALSE)}

\arguments{
    \item{Y}{
A vector of observed values for the continuous outcome.
}
  \item{X}{
A vector of observed values for the continuous exposure.
}
  \item{U}{
A matrix of observed values for the potential confounders, where each column contains observed values for a potential confounder. A recommended implementation is to only consider pre-exposure covariates.
}
  \item{omega}{
A vector of omega values for which the sensitivity analysis is performed. The default is \code{c(1, 1.1, 1.3, 1.6, 2, 5, 10, 30, 50, 100, Inf)}. 
}
  \item{maxmodels}{
The maximum number of outcome and exposure models of each size to be considered. Larger numbers improves the approximation, but can greatly increase the computational burden. The default is \code{150}.
}
  \item{cutoff}{
Minimum posterior probability needed for an outcome model to be considered in the weighted average of the posterior mean and standard deviation of the exposure effect. Smaller values of \code{cutoff} improves the approximation, but add computational complexity. The default is \code{0.0001}.
}
\item{return.best}{If \code{TRUE}, \code{BACprior.lm} returns outcome models meeting the \code{cutoff} criterion and their posterior probabilities. The default if \code{FALSE}.}
}
\details{
Only the best \code{maxmodels} of each size are recorded for both the outcome and exposure models. The marginal likelihoods of these models are a function of the corresponding BIC values. The posterior mean of the exposure effect parameter is the weighted average of the maximum likelihood estimates, where the weights are the posterior probabilities of the outcome models. The standard deviation of the exposure effect parameter accounts for both the within- and between-model variability (Hoeting et al., 1999), where the within-model variability is given by the standard error of the maximum likelihood exposure effect estimate for the model.

\code{BAC.prior.lm} assumes there are no missing values. The objects \code{X}, \code{Y} and \code{U} should be processed beforehand so that every case is complete. The \code{na.omit} function which removes cases with missing data or an imputation package might be helpful.
}
\value{

\item{results}{A matrix in which the first column contains the selected \code{omega} values, the second column contains the posterior mean of the exposure effect for each \code{omega} value and the third column contains the standard deviation of the exposure effect for each \code{omega} value.}

\item{best.models}{Only returned if \code{return.best = TRUE}. A logical matrix whose columns represent the different potential confounders in \code{U} and whose lines indicate which covariates are included in a given outcome model. Only outcome models that have a posterior probability larger than the \code{cutoff} value for at least one \code{omega} value are returned.}

\item{posterior.prob}{Only returned if \code{return.best = TRUE}. A matrix containing the posterior probability of each outcome model in the set of \code{best.models} for the different \code{omega} values. Each line represents a different outcome model and each column corresponds to one of the selected omega value.}
}

\references{
Hoeting, J.A., Madigan D., Raftery, A.E., Volinsky C.T. (1999). \emph{Bayesian model averaging : A tutorial}, Statistical Science, 16, 382-417.

Lefebvre, G., Atherton, J., Talbot, D. (2014). \emph{The effect of the prior distribution in the Bayesian Adjustment for Confounding algorithm}, Computational Statistics & Data Analysis, 70, 227-240.

Wang, C., Parmigiani, G., Dominici, F. (2012). \emph{Bayesian effect estimation accounting for adjustment uncertainty}, Biometrics, 68 (3), 661-671.

%Wang, C., Parmigiani, G., Dominici, F. (2012b). \emph{Rejoinder: Bayesian effect estimation accounting for adjustment uncertainty}, Biometrics DOI: 10.1111/j.15410420.2011.01735.x.
}
\author{
Denis Talbot, Genevieve Lefebvre, Juli Atherton.
}
\note{If either the null outcome model (intercept + exposure only) or the null exposure model (intercept only) have a nonnegligible weight, the approximate inferences given by this function might be poor. This occurs since the \code{regsubset} function used to find the best models of each size does not return null models.
}

\seealso{\code{\link{regsubsets}}, \code{\link{na.omit}}}

\examples{
# Required package to simulate from a multivariate normal distribution.
require(mvtnorm);
	
# Simulate data
# n = 500 observations with 10 covariates.
# (U1, U2, U4, U6, U7, U8, U9, U10) is multivariate normal
# with mean vector 0, variances of 1 and pairwise correlations of 0.25.
# U3 and U5 are causal effects of U2 and U4, respectively.
# X is a causal effect of U1, U2 and U4.
# Y is a causal effect of X, U3, U4 and U5.
# The true exposure effect is 0.1.
	ncov = 10;
	n = 500;
	U = rmvnorm(n = n, mean = rep(0, ncov),
	 sigma = diag(0.75, nrow = ncov) + matrix(0.25, nrow = ncov, ncol = ncov));
	U[,3] = U[,2] + rnorm(n);
	U[,5] = U[,4] + rnorm(n);
	X = 0.5*U[,1] + 0.5*U[,2] + U[,4] + rnorm(n);
	Y = U[,3] + 0.3*U[,4] + U[,5] + 0.1*X + rnorm(n);

	BACprior.lm(Y, X, U, omega = c(1, 1.5, 2, 5, 10, 50, 100, Inf));
	}
