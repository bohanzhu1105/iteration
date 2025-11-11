Writting_function
================
Bohan Zhu
2025-10-30

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%",
  fig.retina = 2,
  dpi = 320
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Start small!

Everone loves z scores.

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1]  1.09185261  0.23266406 -1.64136610  0.27645366 -0.07964651  2.43301365
    ##  [7] -0.01521327 -1.43295042  1.10083438 -1.27504584 -0.91808864  0.20410632
    ## [13] -0.42352874 -0.08919287  0.57349107  0.18925638  0.05889843  0.39601926
    ## [19] -1.42175322  0.74019581

Write a function to compute z scores.

``` r
z_scores = function(x){
  
  if(!is.numeric(x)){
    stop("The input x should be numeric")
  }
  
  if(length(x) < 5){
    stop("Only compute z scores when the input has 5 or more numbers")
  }
  z = (x - mean(x))/sd(x) 
  
  return(z)
}
```

Let’s try our function

``` r
z_scores(x = x_vec)
```

    ##  [1]  1.09185261  0.23266406 -1.64136610  0.27645366 -0.07964651  2.43301365
    ##  [7] -0.01521327 -1.43295042  1.10083438 -1.27504584 -0.91808864  0.20410632
    ## [13] -0.42352874 -0.08919287  0.57349107  0.18925638  0.05889843  0.39601926
    ## [19] -1.42175322  0.74019581

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  0.700098875 -0.591680033 -0.282859405  0.273173860 -0.475250961
    ##   [6]  1.639674609 -0.413054981  1.503709841  0.995234653 -0.249071492
    ##  [11] -0.389224304 -1.600890923 -0.690988437 -1.810642114 -1.830337605
    ##  [16]  1.227441461 -0.954790883 -0.855407040  2.048548566  0.532477066
    ##  [21] -0.603839985  0.032665424 -0.864105487 -0.889271819  0.614616959
    ##  [26]  0.618891988  0.780875973  0.155378067  1.682993897 -0.180216382
    ##  [31]  1.145051988 -0.291953461  0.759390330 -1.371306798  0.416243555
    ##  [36]  1.691475305 -1.155532743 -0.453300010 -0.242096651  1.389731936
    ##  [41] -0.659731192  1.573239573 -0.251928395  0.757815057  0.114325465
    ##  [46]  0.496666352 -0.781456147 -0.892312516 -0.351126057 -0.958546342
    ##  [51]  0.491755093  0.641531839  1.586081062  0.036397181  1.352614321
    ##  [56] -0.739099824  2.205541262 -3.015015147 -0.249254015 -1.105586404
    ##  [61]  0.413504046 -0.728657216 -0.893981550 -1.362734749 -0.126750850
    ##  [66]  0.554431490 -0.117846184  1.356482264 -1.004210991 -1.147123727
    ##  [71] -0.450046129  1.450243561 -0.949047388  2.343726557  1.590006577
    ##  [76] -0.566160483  0.221957781 -0.098322764 -0.652028203  0.087373175
    ##  [81]  0.988176767 -0.941311842 -1.345472711 -1.250028365  1.437439199
    ##  [86]  0.007210739  0.340679048  1.804837869 -0.835015431  0.702294834
    ##  [91]  0.929758484 -0.790685079 -0.227578966 -1.217525204 -0.548332519
    ##  [96] -0.215373923 -0.310920571 -0.657312005 -0.366642343  0.338072862
    ## [101]  0.480238869  0.142643512  0.585379761  0.121028622 -1.855485626
    ## [106]  0.414930052 -0.454126380  0.488562218 -1.106078774  0.031753894
    ## [111] -0.199084427  0.495236074  1.476708543 -1.174157679  0.224810471
    ## [116]  1.310469173 -1.288491516 -0.063855227  0.532247389  0.354811030
    ## [121] -1.531868925  1.270384605 -0.282905726

Let’s break our function

``` r
z_scores(3)
```

    ## Error in z_scores(3): Only compute z scores when the input has 5 or more numbers

``` r
z_scores("my name is Bohan")
```

    ## Error in z_scores("my name is Bohan"): The input x should be numeric

## Let’s compure some stuff

Let’s compute and return the mean and sd of a numeric vector

``` r
mean_and_sd = function(x){
  
  if(!is.numeric(x)){
    stop("The input x should be numeric")
  }
  
  if(length(x) < 5){
    stop("Only compute mean and sd when the input has 5 or more numbers")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.8  4.02

## Make up data

Let’s *simulate* some data.

``` r
sim_df = 
  tibble(
    x = rnorm(30, mean = 3, sd =2)
  )

sim_df |> 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.73      2.26

Write a function to do simulations.

(we wrote this in a code chunk last time; now it’s being sourced.)

``` r
source("sim_mean_sd.R")
```

Let’s run this function

``` r
sim_mean_sd()
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.95      1.77

Import the LoTR data

``` r
fellowship_ring =
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "Fellowship of the Ring")

two_towers =
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie = "two_towers")

return_of_the_king =
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(movie = "return_of_the_king")

lotr_df = 
  bind_rows(fellowship_ring, two_towers, return_of_the_king)
```

Turn this into a function

``` r
lotr_import = function(cell_range, movie_title){
  
  read_excel("data/LotR_Words.xlsx", range = cell_range) |> 
  mutate(movie = movie_title)
  
}

fellowship = lotr_import(cell_range = "B3:D6", movie_title = "Fellowship")
two_towers = lotr_import(cell_range = "F3:H6", movie_title = "Two Towers")
return = lotr_import(cell_range = "J3:L6", movie_title = "Return")

bind_rows(fellowship, two_towers, return)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf      1229   971 Fellowship
    ## 2 Hobbit     14  3644 Fellowship
    ## 3 Man         0  1995 Fellowship
    ## 4 Elf       331   513 Two Towers
    ## 5 Hobbit      0  2463 Two Towers
    ## 6 Man       401  3589 Two Towers
    ## 7 Elf       183   510 Return    
    ## 8 Hobbit      2  2673 Return    
    ## 9 Man       268  2459 Return

Look at one more example.

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj_year = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))

data_marj_month = 
  nsduh_html |> 
  html_table() |> 
  nth(2) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))

data_marj_first = 
  nsduh_html |> 
  html_table() |> 
  nth(3) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

Write an import function

``` r
nsduh_import = function(html, table_num){
  
  data = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
    
  return(data)
  
}

nsduh_import(nsduh_html, table_num = 1)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, table_num = 2)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, table_num = 3)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    1.42
    ##  2 Alabama 12+   2014-2015    1.49
    ##  3 Alabama 12-17 2013-2014    4.46
    ##  4 Alabama 12-17 2014-2015    4.36
    ##  5 Alabama 18-25 2013-2014    6.04
    ##  6 Alabama 18-25 2014-2015    6.39
    ##  7 Alabama 26+   2013-2014    0.15
    ##  8 Alabama 26+   2014-2015    0.2 
    ##  9 Alabama 18+   2013-2014    0.95
    ## 10 Alabama 18+   2014-2015    1.05
    ## # ℹ 500 more rows
