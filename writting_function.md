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
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
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

    ##  [1]  0.1617703 -0.4190980  1.3862825 -0.2737386 -0.3025789  0.3422143
    ##  [7] -0.5704147 -1.0984467  0.5757657 -2.4567106 -1.0305449  1.3972226
    ## [13]  0.3154328 -0.4465014  1.3060172 -0.1692155  1.5677667  0.7479789
    ## [19] -0.2982300 -0.7349718

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

    ##  [1]  0.1617703 -0.4190980  1.3862825 -0.2737386 -0.3025789  0.3422143
    ##  [7] -0.5704147 -1.0984467  0.5757657 -2.4567106 -1.0305449  1.3972226
    ## [13]  0.3154328 -0.4465014  1.3060172 -0.1692155  1.5677667  0.7479789
    ## [19] -0.2982300 -0.7349718

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  0.1441576082 -0.7232757721 -0.2846758545 -0.3100887401  1.6024787252
    ##   [6]  0.5338047561 -0.8958689775  1.5348348565  0.0122754448 -0.2632039797
    ##  [11] -0.4361371891 -0.8179463485 -0.7066282336 -0.1426634198 -0.3090101841
    ##  [16]  1.3875831605  1.2129725227 -0.8211303960 -0.1182095243  0.3734915275
    ##  [21] -1.4182494945 -0.4021798944  2.3690610890 -0.4140396778 -0.3521455018
    ##  [26] -0.2030266550  0.9774385329 -0.5012892910 -1.2791090429  0.0304701333
    ##  [31] -0.5963796679 -0.5732367729  0.5147297596  1.0771154363 -0.0338442175
    ##  [36]  1.0487589511 -2.0088288115  0.6883134497  0.0934115007  0.4770663234
    ##  [41]  1.0548475818  1.4176705036 -1.2908545409 -0.7050439954 -0.2822855083
    ##  [46] -1.0996637992  1.2052589400  1.8545127566  0.0422279069  3.0670555693
    ##  [51]  1.0713143177 -1.1663612391  0.6627762811 -0.0451645498  2.3514462412
    ##  [56]  0.6268441136 -0.6821801232 -1.4181743706  1.4221132192 -0.7623605152
    ##  [61]  0.1451866421  0.4644558247 -0.4581800229  1.4237729773 -0.9569442169
    ##  [66]  1.8963777025 -1.3885141765 -1.3635420635 -0.0755506139 -0.2587437110
    ##  [71] -0.3880277013 -0.3773757024  0.9070011398  0.4157381774  0.2002502640
    ##  [76]  0.9450682445 -0.2137947681  1.1720040310 -1.7760416585  0.1145141294
    ##  [81] -0.7606422476 -0.3130890579  0.5651776065 -0.5868506933  0.0198063451
    ##  [86] -0.6686973724  0.5869937055 -0.2734826303  0.8120528407  0.3950664513
    ##  [91] -0.1579888880 -1.0206274481  0.7445719540 -1.5404917610 -0.5975730345
    ##  [96] -0.2078425389 -2.5860494103  2.2368699307 -0.4726070436  0.0157814900
    ## [101]  0.0448515671 -0.7478478707  0.6108237939  0.1764697530 -1.6205101198
    ## [106] -0.0782581011 -0.7787187262 -0.0007517501  0.7509885409 -0.5404091236
    ## [111] -1.2194358097 -0.4958633168 -1.8168182200  0.4214171345 -1.4451196270
    ## [116]  1.5351989490  0.8895348767 -1.3998507555  0.8567865794  0.4764037540
    ## [121]  0.2300221491 -0.4819747534  0.2242534605

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
    ## 1  9.53  3.90

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
    ## 1   2.51      1.75

Write a function to do simulations.

``` r
sim_mean_sd = function(n_subj= 30, mu = 3, sigma = 2){
  
  sim_df = 
  tibble(
    x = rnorm(n = n_subj, mean = mu, sd =sigma)
  )

sim_df |> 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
  
}
```

Let’s run this function

``` r
sim_mean_sd()
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.95      2.12

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
