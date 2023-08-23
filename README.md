
# nmfbin: Non-Negative Matrix Factorization for Binary Data

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/nmfbin)](https://CRAN.R-project.org/package=nmfbin)
[![R-CMD-check](https://github.com/michalovadek/nmfbin/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michalovadek/nmfbin/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `nmfbin` R package provides a simple Non-Negative Matrix Factorization (NMF) implementation tailored for binary data matrices. It offers a choice of initialization methods, loss functions and updating algorithms.

Unlike most other NMF packages, this one is focused on (1) binary (Boolean) data and (2) minimizing dependencies.

Note the package is in early stages of development.

## Installation

You can install the development version of `nmfbin` from [GitHub](https://github.com/michalovadek/nmfbin) with:

``` r
# install.packages("remotes")
remotes::install_github("michalovadek/nmfbin")
```

## Usage

The input matrix can only contain 0s and 1s.

``` r
# load
library(nmfbin)

# Create a binary matrix for demonstration
X <- matrix(sample(c(0, 1), 100, replace=TRUE), ncol=10)

# Perform NMF
results <- nmfbin(X, k=3, optimizer = "mur", init = "nndsvd")
```

## Citation

```
@Manual{,
  title = {nmfbin: Non-negative Matrix Factorization for Binary Data},
  author = {Michal Ovadek},
  year = {2023},
  note = {R package version 0.2.0},
  url = {https://michalovadek.github.io/nmfbin/},
}
```

## Contributions

Contributions to the `nmfbin` package are more than welcome. Please submit pull requests or open an issue for discussion.
