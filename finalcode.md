Socioeconomic factors and Rat sightings
================
Nandini M
2023-12-03

## Data Cleaning:

Steps

1.  clean names and select necessary variables

2.  rename the variables

3.  drop any rows with missing observations

4.  filter out the second row (contains comments about the columns)

5.  join all the datasets

6.  drop any rows with missing observations

7.  create new column, id, using the geo id to help with matching to
    zipcodes

8.  filter to only keep counties relevant to NYC

9.  create new column for zipcode by matching to id

Crowding data

- less_than_1 = Occupied housing units with 1.00 or less occupants per
  room
- bet_1to1.5 = Occupied housing units with 1.01 to 1.50 occupants per
  room
- more_than_1.5 = Occupied housing units with 1.51 or more occupants per
  room

``` r
crowding18 =
  read_csv("Data/crowding/2018_crowding.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, name, s2501_c01_006e, s2501_c01_007e, s2501_c01_008e) |> 
  rename(
    less_than_1_y18   = s2501_c01_006e,
    bet_1to1.5_y18    = s2501_c01_007e,
    more_than_1.5_y18 = s2501_c01_008e
  ) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) 
```

    ## Rows: 4919 Columns: 458
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (458): GEO_ID, NAME, S2501_C01_001E, S2501_C01_001M, S2501_C01_002E, S25...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
crowding19 =
  read_csv("Data/crowding/2019_crowding.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s2501_c01_006e, s2501_c01_007e, s2501_c01_008e) |> 
  rename(
    less_than_1_y19   = s2501_c01_006e,
    bet_1to1.5_y19    = s2501_c01_007e,
    more_than_1.5_y19 = s2501_c01_008e
  ) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) 
```

    ## Rows: 4919 Columns: 458
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (458): GEO_ID, NAME, S2501_C01_001E, S2501_C01_001M, S2501_C01_002E, S25...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
crowding20 =
  read_csv("Data/crowding/2020_crowding.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s2501_c01_006e, s2501_c01_007e, s2501_c01_008e) |> 
  rename(
    less_than_1_y20   = s2501_c01_006e,
    bet_1to1.5_y20    = s2501_c01_007e,
    more_than_1.5_y20 = s2501_c01_008e
  ) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) 
```

    ## Rows: 5412 Columns: 458
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (458): GEO_ID, NAME, S2501_C01_001E, S2501_C01_001M, S2501_C01_002E, S25...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
crowding21 =
  read_csv("Data/crowding/2021_crowding.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s2501_c01_006e, s2501_c01_007e, s2501_c01_008e) |> 
  rename(
    less_than_1_y21   = s2501_c01_006e,
    bet_1to1.5_y21    = s2501_c01_007e,
    more_than_1.5_y21 = s2501_c01_008e
  ) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) 
```

    ## Rows: 5412 Columns: 458
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (458): GEO_ID, NAME, S2501_C01_001E, S2501_C01_001M, S2501_C01_002E, S25...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
crowding_10s = 
  full_join(crowding18, crowding19, by = "geo_id")

crowding_20s = 
  full_join(crowding20, crowding21, by = "geo_id")

crowding_all =
  full_join(crowding_10s, crowding_20s, by = "geo_id") |> 
  drop_na() |> 
  mutate(
    id = str_sub(geo_id, 10)
  ) 

   #mutate(
   # county = str_extract(crowding18$name, "(?<=,)[^,]+(?=,)") # how to fix this?
 # )
  
 # filter(name %in% c("Brooklyn County","Bronx County","Queens County","Richmond County","New York County"))
  
# need to figure out how to make county column
# need to filter to keep only nyc counties
# need to convert id to zipcode
```
