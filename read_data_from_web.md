read_data_from_web
================

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_url =
  read_html(nsduh_url)
```

``` r
marj_use_df =
  nsduh_url |>
  html_table() |>
  first() |>
  slice(-1) #delete first row
```

Import star wars…

``` r
swm_html = 
  read_html("https://www.imdb.com/list/ls070150896/")
```

``` r
swm_title_vec =
  swm_html |>
  html_elements(".lister-item-header a") |>
  html_text()

swm_gross_rev_vec =
  swm_html |>
  html_elements(".text-small:nth-child(7) span:nth-child(5)") |>
  html_text()

swm_runtime_vec = 
  swm_html |>
  html_elements(".runtime") |>
  html_text()

swm_df = 
  tibble(
    title = swm_title_vec,
    rev = swm_gross_rev_vec,
    runtime = swm_runtime_vec)
```

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.csv") |>
  content()
```

    ## Rows: 44 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (4): year, new_york_city_population, nyc_consumption_million_gallons_per...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

BRFSS Data

``` r
brfss_smart2010 = 
  GET("https://chronicdata.cdc.gov/resource/acme-vg9e.csv",
      query = list("$limit" = 5000)) |>
  content()
```

    ## Rows: 5000 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): locationabbr, locationdesc, class, topic, question, response, data...
    ## dbl  (6): year, sample_size, data_value, confidence_limit_low, confidence_li...
    ## lgl  (1): locationid
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Try it now!

``` r
poke_df =
  GET("https://pokeapi.co/api/v2/pokemon/ditto") |>
  content()
```
