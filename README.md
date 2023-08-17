
# nmfbin: Non-Negative Matrix Factorization for Binary Data

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/nmfbin)](https://CRAN.R-project.org/package=nmfbin)
[![R-CMD-check](https://github.com/michalovadek/nmfbin/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michalovadek/nmfbin/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `nmfbin` R package provides a simple Non-Negative Matrix Factorization (NMF) implementation tailored for binary data matrices. It offers a choice of divergence measures (Kullback-Leibler divergence or binary cross-entropy) to optimize the factorization.

Note the package is in early stages of development.

## Installation

You can install the development version of `nmfbin` from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("michalovadek/nmfbin")
```

## Features
- Designed for binary data matrices (comprising only of 0s and 1s)
- Choice of common loss functions
- Options for initializing matrix factorization
- Includes measures of divergence reduction

## Usage

The input matrix can only contain 0s and 1s.

``` r
library(nmfbin)

# Create a binary matrix for demonstration
V <- matrix(sample(c(0, 1), 100, replace=TRUE), ncol=10)

# Perform NMF
results <- nmfbin(V, k=3, divergence="kl", compute_marginal_divergences=TRUE, init_method="random")
```

## Citation

```
@Manual{,
  title = {nmfbin: Non-negative Matrix Factorization for Binary Data},
  author = {Michal Ovadek},
  year = {2023},
  note = {R package version 0.1.0},
  url = {https://michalovadek.github.io/nmfbin/},
}
```

## Contributions

Contributions to the `nmfbin` package are welcome! Please submit pull requests or open an issue for discussion.
