# the following creates a sample that has 100 observations of 3 variables, 
# drawn from a population where A has a mean of 0 and SD of 1, while B and C
# have means of 20 and SDs of 5. A correlates with B and C with r = 0.5, and
# B and C correlate with r = 0.25.
# SOURCE: https://cran.r-project.org/web/packages/faux/vignettes/rnorm_multi.html

rnorm_multi <- function(n = 100, vars = NULL, mu = 0, sd = 1, r = 0, varnames = NULL,
                        empirical = FALSE, as.matrix = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
  }
  if (!is.numeric(n) || n %% 1 > 0 || n < 1) {
    stop("n must be an integer > 0")
  }
  if (!(empirical %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  if (is.null(vars)) {
    if (!is.null(varnames)) {
      vars <- length(varnames)
    } else if (length(mu) > 1) {
      vars <- length(mu)
    } else if (length(sd) > 1) {
      vars <- length(sd)
    } else if (is.matrix(r)) {
      vars <- ncol(r)
    }
    if (is.null(vars)) {
      stop("The number of variables (vars) was not explicitly set and can't be guessed from the input.")
    }
  }
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars")
  } else {
  }
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars")
  } else {
  }
  if (n == 1 & empirical == TRUE) {
    warning("When n = 1 and empirical = TRUE, returned data are equal to mu")
    mvn <- mu
    cor_mat <- r
  } else {
    cor_mat <- cormat(r, vars)
    sigma <- (sd %*% t(sd)) * cor_mat
    err <- "The correlated variables could not be generated."
    if (empirical) {
      err <- paste(err, "Try increasing the N or setting empirical = FALSE.")
    }
    p <- length(mu)
    if (!all(dim(sigma) == c(p, p))) {
      stop(err)
    }
    eS <- eigen(sigma, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -1e-06 * abs(ev[1L]))) {
      stop(paste(err))
    }
    X <- matrix(stats::rnorm(p * n), n)
    if (empirical) {
      X <- scale(X, TRUE, FALSE)
      X <- X %*% svd(X, nu = 0)$v
      X <- scale(X, FALSE, TRUE)
    }
    tryCatch(
      {
        X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(
          ev,
          0
        )), p) %*% t(X)
      },
      error = function(e) {
        stop(err)
      }
    )
    mvn <- t(X)
  }
  if (n == 1) {
    mvn <- matrix(mvn, nrow = 1)
  }
  if (length(varnames) == vars) {
    colnames(mvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    colnames(mvn) <- colnames(cor_mat)
  } else if (!is.null(names(mu))) {
    colnames(mvn) <- names(mu)
  } else if (!is.null(names(sd))) {
    colnames(mvn) <- names(sd)
  } else {
    colnames(mvn) <- make_id(ncol(mvn), "X")
  }
  if (as.matrix == TRUE) {
    mvn
  } else {
    data.frame(mvn, check.names = FALSE)
  }
}
