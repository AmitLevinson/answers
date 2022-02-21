Answers
================
Amit Levinson
2022-02-21

# answers

A repository containing answers for questions in forums

<details>
<summary>

### How can you randomly sample 100 values with replacement from a vector and calculate an IQR, 10000 times?

</summary>

``` r
# Fake data
vec <- 1:100

# Option 1: Functional
calc_iqr <- function() {
    inner_vec <- sample(vec, size = 100, replace = T)
    IQR(inner_vec)
}

values_a <- replicate(100, expr = calc_iqr())

# Output ------------------------------------------------------------------
values_a
```

    ##   [1] 52.25 43.75 47.50 50.00 41.25 42.50 46.25 54.25 44.25 50.25 43.25 48.75
    ##  [13] 48.25 42.25 41.25 51.25 47.25 54.25 53.50 44.75 53.25 58.00 57.25 51.75
    ##  [25] 56.25 41.25 54.50 50.25 48.25 55.00 56.25 49.00 51.25 49.25 42.50 52.25
    ##  [37] 50.00 54.00 37.50 57.25 54.00 47.00 52.50 58.00 41.50 47.50 51.25 47.25
    ##  [49] 44.00 57.50 46.25 55.50 55.25 49.50 52.25 56.00 50.50 55.50 55.50 45.00
    ##  [61] 43.25 41.00 48.25 46.25 51.00 44.75 50.00 40.75 55.50 53.50 57.25 41.00
    ##  [73] 57.00 50.25 49.25 48.25 56.25 49.75 47.50 53.25 54.25 51.25 55.00 54.75
    ##  [85] 50.25 60.50 50.25 53.25 52.00 56.50 55.00 50.25 48.50 40.75 45.75 41.25
    ##  [97] 53.00 48.25 42.00 45.50

``` r
# Option 2: For loop
values_b <- vector(mode = "numeric", length = 100)

output <- for (i in 1:100) {
  inner_vec <- sample(vec, size = 100, replace = T)
  values_b[[i]] <- IQR(inner_vec)
}

# Output ------------------------------------------------------------------
values_b
```

    ##   [1] 54.25 48.25 47.25 49.50 45.00 56.00 53.75 50.25 40.50 37.75 39.00 55.75
    ##  [13] 50.50 47.25 56.75 54.25 53.25 43.75 49.50 54.25 46.50 50.50 50.00 61.50
    ##  [25] 46.00 56.50 46.25 53.25 43.25 46.00 47.50 43.00 58.50 50.00 51.50 39.25
    ##  [37] 43.75 51.25 46.25 45.50 60.25 50.50 57.25 55.00 48.00 47.00 48.25 50.50
    ##  [49] 45.25 53.00 47.75 46.25 63.00 49.25 48.50 50.00 51.50 44.50 53.50 52.50
    ##  [61] 46.75 45.00 50.25 48.50 56.50 55.25 51.50 53.75 50.25 47.00 49.50 55.00
    ##  [73] 50.25 47.75 54.75 42.00 49.50 50.50 43.50 46.50 52.75 46.75 46.75 51.25
    ##  [85] 46.75 46.50 55.25 53.00 52.25 54.25 51.25 51.25 38.25 45.00 62.00 58.25
    ##  [97] 46.50 52.50 55.50 56.00

</details>
