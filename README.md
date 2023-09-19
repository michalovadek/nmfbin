
# nmfbin: Non-negative Matrix Factorization for Binary Data

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/nmfbin)](https://CRAN.R-project.org/package=nmfbin)
[![R-CMD-check](https://github.com/michalovadek/nmfbin/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michalovadek/nmfbin/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `nmfbin` R package provides a simple Non-negative Matrix Factorization (NMF) implementation tailored for binary data matrices. It offers a choice of initialization methods, loss functions and updating algorithms.

NMF is typically used for reducing high-dimensional matrices into lower (k-) rank ones where _k_ is chosen by the user. Given a non-negative matrix _X_ of size $m \times n$, NMF looks for two non-negative matrices _W_ ($m \times k$) and _H_ ($k \times n$), such that:

$$X \approx W \times H$$

In topic modelling, _W_ is interpreted as the document-topic matrix and _H_ as the topic-feature matrix.

Unlike most other NMF packages, `nmfbin` is focused on binary (Boolean) data, while keeping the number of dependencies to a minimum.

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
X <- matrix(sample(c(0, 1), 100, replace = TRUE), ncol = 10)

# Perform Logistic NMF
results <- nmfbin(X, k = 3, optimizer = "mur", init = "nndsvd", max_iter = 1000)
```

## Citation

```
@Manual{,
  title = {nmfbin: Non-negative Matrix Factorization for Binary Data},
  author = {Michal Ovadek},
  year = {2023},
  note = {R package version 0.2.1},
  url = {https://michalovadek.github.io/nmfbin/},
}
```

## Contributions

Contributions to the `nmfbin` package are more than welcome. Please submit pull requests or open an issue for discussion.
