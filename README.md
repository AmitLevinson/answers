Answers
================
Amit Levinson
2022-10-24

# answers

A repository containing answers for questions in forums.

You might find discrepancies between the number of times requested in
the question and my response. This is mainly for ease of printing.

<details>
<summary>
How can I iterate over every column in my dataframe and calculate a
t.test between half of the elemnts (e.g. 1:50 vs 51:100) (10/24/2022)
</summary>

``` r
library(purrr)
library(broom)

dat <- data.frame(
  col1 = sample(c(1:100), size = 100, replace = T),
  col2 = sample(c(1:100), size = 100, replace = T),
  col3 = sample(c(1:100), size = 100, replace = T)
)

calculate_ttest <- function(column) {
  t.test(column[50:100], column[1:49], alternative= "less", paired=FALSE, mu=0, conf.level = 0.9, var.equal = TRUE) |>
    broom::tidy() 
}

dat %>% 
  map_dfr(calculate_ttest, .id = 'columnname')
```

    ## # A tibble: 3 × 11
    ##   columnname estimate estimate1 estimate2 statistic p.value parameter conf.low
    ##   <chr>         <dbl>     <dbl>     <dbl>     <dbl>   <dbl>     <dbl>    <dbl>
    ## 1 col1           4.70      54.8      50.1     0.752  0.773         98     -Inf
    ## 2 col2          -9.74      41.7      51.4    -1.58   0.0591        98     -Inf
    ## 3 col3          -7.94      45.6      53.5    -1.38   0.0852        98     -Inf
    ## # … with 3 more variables: conf.high <dbl>, method <chr>, alternative <chr>

</details>
<details>
<summary>
How can we generate \~50 formulas to pass onto a model from a given set
of variables? \*\*
</summary>

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(purrr)
library(broom)

mtcars_vars <- names(mtcars)

# Generate possible formulas
mtcars_formulas <- expand.grid(mtcars_vars,mtcars_vars,mtcars_vars) %>% 
  filter(Var1 != Var2, Var2 != Var3, Var1 != Var3) %>% 
  sample_n(10)

# Function to create formulas
f <- function (Var1, Var2, Var3) paste(Var1, paste(Var2, Var3, sep = " + "), sep = " ~ ")

formulas <- mtcars_formulas %>%
  pmap_chr(f)

head(formulas)
```

    ## [1] "drat ~ hp + cyl"   "qsec ~ am + mpg"   "wt ~ carb + vs"   
    ## [4] "am ~ disp + carb"  "drat ~ gear + mpg" "vs ~ hp + cyl"

``` r
# Iterate across the formulas and tidy the output with formula as name
models <- map(formulas, ~ lm(., data = mtcars)) %>% 
  set_names(formulas) %>% 
  map_dfr(tidy, .id = ".x")

# Output
models
```

    ## # A tibble: 30 × 6
    ##    .x               term        estimate std.error statistic  p.value
    ##    <chr>            <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 drat ~ hp + cyl  (Intercept)  5.07      0.257      19.8   2.30e-18
    ##  2 drat ~ hp + cyl  hp           0.00340   0.00176     1.94  6.26e- 2
    ##  3 drat ~ hp + cyl  cyl         -0.318     0.0674     -4.72  5.53e- 5
    ##  4 qsec ~ am + mpg  (Intercept) 13.8       0.853      16.1   5.10e-16
    ##  5 qsec ~ am + mpg  am          -2.69      0.566      -4.76  5.00e- 5
    ##  6 qsec ~ am + mpg  mpg          0.258     0.0468      5.50  6.27e- 6
    ##  7 wt ~ carb + vs   (Intercept)  3.33      0.450       7.39  3.86e- 8
    ##  8 wt ~ carb + vs   carb         0.100     0.112       0.890 3.81e- 1
    ##  9 wt ~ carb + vs   vs          -0.895     0.360      -2.49  1.90e- 2
    ## 10 am ~ disp + carb (Intercept)  0.782     0.163       4.80  4.47e- 5
    ## # … with 20 more rows

</details>
<details>
<summary>
The categories on the X-axis are too condensed and overflow, can we
expand them to more than one row?
</summary>

``` r
library(ggplot2)

# Fake data
dat <- data.frame(
category = sample(c(stringr::sentences[1:3], stringr::words[1:4]), size = 100, replace = T)
)

# Option 1 - Category to y axis instead of splitting
p1 <- ggplot(dat) +
geom_bar(aes(y = category)) +
labs(title = "Put the category on the Y-axis")

# Option 2: Stringr::str_wrap
p2 <- ggplot(dat) +
geom_bar(aes(x = stringr::str_wrap(category, 15))) +
labs(title = "use stringr:str_wrap(variable, 15)")
```

</details>
<details>
<summary>
I have X classes (groups) and would like to run a t-test for groups
within each class separately
</summary>

``` r
library(ggplot2)
library(dplyr)
library(broom)

mtcars %>%
group_by(gear) %>%
summarize(t.test(mpg , cyl, data = cur_data()) %>% tidy())
```

    ## # A tibble: 3 × 11
    ##    gear estimate estimate1 estimate2 statistic      p.value parameter conf.low
    ##   <dbl>    <dbl>     <dbl>     <dbl>     <dbl>        <dbl>     <dbl>    <dbl>
    ## 1     3     8.64      16.1      7.47      9.36 0.0000000326     17.4      6.70
    ## 2     4    19.9       24.5      4.67     12.8  0.0000000286     11.8     16.5 
    ## 3     5    15.4       21.4      6         4.95 0.00504           4.72     7.24
    ## # … with 3 more variables: conf.high <dbl>, method <chr>, alternative <chr>

``` r
# Using Purrr & unnest
library(purrr)
library(tidyr)

mtcars %>%
group_nest(gear) %>%
summarise(ttest = map(data, ~ (t.test(x = .$mpg , y = .$cyl, data = .x)))) %>%
mutate(ttest = map(ttest, tidy)) %>%
unnest(ttest)
```

    ## # A tibble: 3 × 10
    ##   estimate estimate1 estimate2 statistic    p.value parameter conf.low conf.high
    ##      <dbl>     <dbl>     <dbl>     <dbl>      <dbl>     <dbl>    <dbl>     <dbl>
    ## 1     8.64      16.1      7.47      9.36    3.26e-8     17.4      6.70      10.6
    ## 2    19.9       24.5      4.67     12.8     2.86e-8     11.8     16.5       23.3
    ## 3    15.4       21.4      6         4.95    5.04e-3      4.72     7.24      23.5
    ## # … with 2 more variables: method <chr>, alternative <chr>

</details>
<details>
<summary>
How can you randomly sample 100 values with replacement from a vector
and calculate an IQR, 10000 times?
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

    ##   [1] 50.25 46.00 50.25 43.25 47.50 57.00 48.00 52.25 49.00 48.25 52.00 39.25
    ##  [13] 42.50 62.25 51.50 51.75 56.50 41.75 53.25 50.00 43.00 61.25 51.50 45.00
    ##  [25] 44.25 53.25 53.25 43.00 51.00 50.50 52.25 46.75 47.25 46.50 47.00 50.75
    ##  [37] 52.50 60.00 44.25 49.50 54.25 45.75 51.00 47.50 46.75 52.25 55.75 49.00
    ##  [49] 54.50 49.00 49.75 46.00 48.25 52.50 51.25 51.50 40.50 48.50 42.25 53.50
    ##  [61] 45.50 48.25 47.75 63.75 52.00 37.75 50.25 52.50 44.25 49.75 44.50 48.00
    ##  [73] 46.50 45.50 43.25 40.25 62.00 49.25 46.00 48.25 51.50 46.50 51.25 57.50
    ##  [85] 54.50 50.00 47.25 44.75 44.00 46.25 40.25 46.75 50.00 47.00 42.00 38.25
    ##  [97] 44.00 50.25 40.25 51.50

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

    ##   [1] 44.25 53.25 48.25 45.00 51.50 48.25 54.00 49.50 57.50 44.50 54.50 54.75
    ##  [13] 47.25 45.25 47.25 49.50 46.75 40.25 44.25 52.25 53.25 48.50 41.25 46.00
    ##  [25] 60.00 53.25 48.50 47.50 51.50 50.75 46.25 45.00 58.25 51.25 44.25 46.50
    ##  [37] 40.00 51.00 40.50 46.50 53.00 44.75 49.25 41.00 50.50 49.75 46.50 52.25
    ##  [49] 54.50 49.50 56.00 47.25 41.50 51.00 55.00 49.25 56.25 42.25 50.00 44.50
    ##  [61] 61.25 44.25 48.25 56.25 55.00 50.75 48.50 57.50 50.50 48.25 58.75 50.25
    ##  [73] 50.25 53.50 47.00 45.75 48.25 45.50 44.00 52.25 48.00 44.00 44.50 52.25
    ##  [85] 53.50 51.50 35.25 45.50 47.00 40.25 47.25 54.50 48.25 46.25 58.25 53.25
    ##  [97] 54.00 48.50 57.50 49.00

</details>
