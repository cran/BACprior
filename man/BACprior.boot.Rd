\name{BACprior.boot}
\alias{BACprior.boot}


\title{
A Bootstrap Procedure to Guide the Choice of Omega in the Bayesian Adjustment for Confounding Algorithm.
}
\description{
The \code{BACprior.boot} function proposes a bootstrap procedure to select BAC's omega value in an attempt to minimize the mean squared error (MSE) of the exposure effect estimator. A number B of bootstrap samples are taken from the original sample. Then, the MSE is estimated for each selected omega value, considering the exposure estimate with omega = infinity from the original sample as the true value. The \code{BACprior.boot} function uses the \code{BACprior.lm} function to estimate the exposure effect.}

\usage{BACprior.boot(Y, X, U, 
omega = c(1, 1.1, 1.3, 1.6, 2, 5, 10, 30, 50, 100, Inf),
maxmodels = 150, cutoff = 0.0001, B = 100)}


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
A vector of omega values for which the bootstrap procedure is performed. The default is \code{c(1, 1.1, 1.3, 1.6, 2, 5, 10, 30, 50, 100, Inf)}. 
}
  \item{maxmodels}{
The maximum number of outcome and exposure models of each size to be considered. Larger numbers improves the approximation, but can greatly increase the computational burden. The default is \code{150}.
}
  \item{cutoff}{
Minimum posterior probability needed for an outcome model to be considered in the weighted average of the posterior mean and standard deviation of the exposure effect. Smaller values of \code{cutoff} improves the approximation, but add computational complexity. The default is \code{0.0001}.
}
\item{B}{The number of bootstrap samples to be taken. Larger numbers reduce Monte Carlo error, but require more computation time.}
}



\details{
Since \code{BACprior.boot} uses the \code{BACprior.lm} function to estimate the exposure effect, users should refer to the \code{BACprior.lm} documentation for details of implementation.

\code{BACprior.boot} assumes there are no missing values. The objects \code{X}, \code{Y} and \code{U} should be processed beforehand so that every case is complete. The \code{na.omit} function which removes cases with missing data or an imputation package might be helpful.
}
\value{

\item{Best}{The omega value, among the omega values given in input, which minimizes the estimated MSE.}

\item{MSE}{The estimated MSE for each of the selected omega values.}

\code{BACprior.boot} also returns a plot of the estimated MSEs according to the selected omega values.

}

\references{
Brookhart, M.A., van der Laan, M.J. (2006). \emph{A semiparametric model selection criterion with applications to the marginal structural model}, Computational Statistics & Data Analysis, 50, 475-498.

Lefebvre, G., Atherton, J., Talbot, D. (2014). \emph{The effect of the prior distribution in the Bayesian Adjustment for Confounding algorithm}, Computational Statistics & Data Analysis, 70, 227-240.
}

\author{
Denis Talbot, Genevieve Lefebvre, Juli Atherton.
}


\seealso{\code{\link{BACprior.lm}}, \code{\link{BACprior.CV}},   \code{\link{na.omit}}}

\examples{
# Required package to simulate from a multivariate normal distribution.
require(mvtnorm);


# Simulate data
# n = 500 observations with 5 covariates.
# (U1, U2, U4) is multivariate normal with mean vector 0,
# variances of 1 and 0 pairwise correlations.
# U3 and U5 are causal effects of U2 and U4, respectively.
# X is a causal effect of U1, U2 and U4.
# Y is a causal effect of U3, U4, U5 and X.

set.seed(3417817);
n = 500;
U = rmvnorm(n = n, mean = rep(0, 5), sigma = diag(1, nrow = 5) + matrix(0, nrow = 5, ncol = 5));
U[,3] = U[,2] + rnorm(n);
U[,5] = U[,4] + rnorm(n);
X = U[,1] + U[,2] + U[,4] + rnorm(n);
Y = U[,3] + 0.1*U[,4] + U[,5] + 0.1*X + rnorm(n);

# Remove  ``#'' to run example 
# BACprior.boot(Y, X, U, maxmodels = 150); 
# $best
# [1] 1
# $MSE
#  [1] 0.001467631 0.001480494 0.001505006 0.001539194 0.001580756
# 0.001803000 0.002017034 0.002375198 0.002516998 0.002662188 0.002865611
# Best omega value would be 1
BACprior.lm(Y, X, U);
# $results
#       omega Posterior mean Standard deviation
#  [1,]   1.0      0.1089228         0.02951582
#  [2,]   1.1      0.1087689         0.02971457
#  [3,]   1.3      0.1084802         0.03008991
#  [4,]   1.6      0.1080900         0.03060449
#  [5,]   2.0      0.1076376         0.03121568
#  [6,]   5.0      0.1057020         0.03426854
#  [7,]  10.0      0.1046804         0.03696670
#  [8,]  30.0      0.1044711         0.04124805
#  [9,]  50.0      0.1047315         0.04291842
# [10,] 100.0      0.1051211         0.04462874
# [11,]   Inf      0.1058021         0.04703111

# Posterior mean doesn't change much with omega,
# but posterior standard deviation greatly increases.
# This supports the choice of omega = 1.
}
