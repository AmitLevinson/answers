Answers
================
Amit Levinson
2022-02-21

# answers

A repository containing answers for questions in forums.

You might find discrepancies between the number of times requested in
the question and my response. This is mainly for ease of printing.

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

    ## [1] "cyl ~ disp + gear"  "disp ~ am + gear"   "carb ~ am + qsec"  
    ## [4] "qsec ~ gear + carb" "wt ~ qsec + cyl"    "drat ~ vs + disp"

``` r
# Iterate across the formulas and tidy the output with formula as name
models <- map(formulas, ~ lm(., data = mtcars)) %>% 
  set_names(formulas) %>% 
  map_dfr(tidy, .id = ".x")

# Output
models
```

    ## # A tibble: 30 x 6
    ##    .x                 term         estimate std.error statistic  p.value
    ##    <chr>              <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 cyl ~ disp + gear  (Intercept)    3.06     1.08        2.83  8.42e- 3
    ##  2 cyl ~ disp + gear  disp           0.0131   0.00139     9.43  2.46e-10
    ##  3 cyl ~ disp + gear  gear           0.0296   0.233       0.127 9.00e- 1
    ##  4 disp ~ am + gear   (Intercept)  416.     133.          3.13  3.95e- 3
    ##  5 disp ~ am + gear   am          -101.      60.2        -1.67  1.05e- 1
    ##  6 disp ~ am + gear   gear         -39.1     40.7        -0.961 3.45e- 1
    ##  7 carb ~ am + qsec   (Intercept)   13.9      2.36        5.88  2.24e- 6
    ##  8 carb ~ am + qsec   am            -0.319    0.462      -0.690 4.96e- 1
    ##  9 carb ~ am + qsec   qsec          -0.614    0.129      -4.75  5.02e- 5
    ## 10 qsec ~ gear + carb (Intercept)   20.2      1.28       15.8   8.86e-16
    ## # ... with 20 more rows

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

    ## # A tibble: 3 x 11
    ##    gear estimate estimate1 estimate2 statistic      p.value parameter conf.low
    ##   <dbl>    <dbl>     <dbl>     <dbl>     <dbl>        <dbl>     <dbl>    <dbl>
    ## 1     3     8.64      16.1      7.47      9.36 0.0000000326     17.4      6.70
    ## 2     4    19.9       24.5      4.67     12.8  0.0000000286     11.8     16.5 
    ## 3     5    15.4       21.4      6         4.95 0.00504           4.72     7.24
    ## # ... with 3 more variables: conf.high <dbl>, method <chr>, alternative <chr>

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

    ## # A tibble: 3 x 10
    ##   estimate estimate1 estimate2 statistic      p.value parameter conf.low conf.high
    ##      <dbl>     <dbl>     <dbl>     <dbl>        <dbl>     <dbl>    <dbl>     <dbl>
    ## 1     8.64      16.1      7.47      9.36 0.0000000326     17.4      6.70      10.6
    ## 2    19.9       24.5      4.67     12.8  0.0000000286     11.8     16.5       23.3
    ## 3    15.4       21.4      6         4.95 0.00504           4.72     7.24      23.5
    ## # ... with 2 more variables: method <chr>, alternative <chr>

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

    ##   [1] 48.00 51.75 49.00 42.50 48.50 51.00 55.25 56.25 50.50 47.75 50.00 47.75
    ##  [13] 54.00 54.25 41.25 54.75 50.50 45.50 49.25 50.25 43.25 46.50 47.00 52.25
    ##  [25] 60.00 46.75 49.50 60.25 51.25 60.00 50.50 55.25 41.50 56.00 45.75 37.00
    ##  [37] 52.75 55.00 40.50 53.25 47.50 49.00 52.25 49.75 55.25 38.25 43.25 55.00
    ##  [49] 45.00 44.25 56.50 52.25 40.75 54.25 42.75 48.25 51.50 47.50 44.25 47.50
    ##  [61] 48.00 57.75 55.25 47.75 47.50 54.75 44.25 51.25 43.50 50.00 49.25 45.75
    ##  [73] 50.00 48.25 42.00 43.25 43.50 51.75 47.25 52.75 53.50 50.00 51.25 47.75
    ##  [85] 51.25 48.25 57.00 51.25 56.25 44.00 46.00 52.75 51.00 54.75 49.50 49.00
    ##  [97] 53.25 53.50 52.50 48.25

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

    ##   [1] 43.25 45.25 40.50 53.50 51.75 49.00 38.75 48.00 54.50 46.25 48.25 57.25
    ##  [13] 57.50 53.25 51.25 54.75 47.00 54.50 52.25 58.50 57.50 46.75 52.00 55.25
    ##  [25] 55.00 47.75 54.25 55.25 52.50 51.50 43.00 44.75 40.75 55.50 42.00 52.50
    ##  [37] 47.50 50.75 44.25 50.25 45.50 44.50 50.00 48.50 52.25 51.00 49.25 50.25
    ##  [49] 47.75 49.50 38.75 42.50 48.75 48.00 52.50 49.00 50.50 49.00 53.75 51.00
    ##  [61] 50.25 47.50 54.00 47.25 53.25 52.25 46.00 45.50 50.50 50.50 44.50 48.75
    ##  [73] 53.25 64.75 43.50 56.25 46.75 48.50 43.00 47.50 55.50 49.00 60.25 49.75
    ##  [85] 43.00 44.25 49.50 47.25 49.75 56.50 51.75 50.50 58.50 57.25 41.25 52.75
    ##  [97] 52.50 43.25 44.25 49.25

</details>
