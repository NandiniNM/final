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
    zipcodes \# idk if need to do this actually

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
    id = str_sub(geo_id, 10),
    county = str_extract(name, "(?<=,)[^,]+(?=,)")
  ) |>
  filter(county %in% "Albany County ")
  
 #filter(county %in% c("Brooklyn County","Bronx County","Queens County","Richmond County","New York County"))
  #filter(county %in% c("Brooklyn County","Bronx County","Queens County","Richmond County","New York County"))
  
# can I make a map using geo id?
# need to filter to keep only nyc counties, why does count go down to 0
```

Education Data

- less_9 = Population 25 years and over - Less than 9th grade
- no_hs_diploma = Population 25 years and over - 9th to 12th grade, no
  diploma
- hs_grad = Population 25 years and over - High school graduate
  (includes equivalency)
- some_college = Population 25 years and over - Some college, no degree
- associate = Population 25 years and over - Associate’s degree
- bachelor = Population 25 years and over - Bachelor’s degree
- graduate = Population 25 years and over - Graduate or professional
  degree
- hs_or_less = Population 25 years and over with HS equivalent education
  or less
- college = Population 25 years and over with at least some college
  education

``` r
edu18 =
  read_csv("Data/edu/edu_2018.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, name, s1501_c01_007e, s1501_c01_008e, s1501_c01_009e, s1501_c01_010e, s1501_c01_011e, 
         s1501_c01_012e, s1501_c01_013e) |> 
  rename(
    less_9_y18        = s1501_c01_007e,
    no_hs_diploma_y18 = s1501_c01_008e,
    hs_grad_y18       = s1501_c01_009e,
    some_college_y18  = s1501_c01_010e,
    associate_y18     = s1501_c01_011e,
    bachelor_y18      = s1501_c01_012e,
    graduate_y18      = s1501_c01_013e
  ) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) |> 
  mutate_at(c("less_9_y18","no_hs_diploma_y18","hs_grad_y18","some_college_y18","associate_y18",
              "bachelor_y18","graduate_y18"), as.numeric) |> 
  mutate(
    hs_or_less_y18 = rowSums(across(c(less_9_y18,no_hs_diploma_y18,hs_grad_y18))),
    college_y18    = rowSums(across(c(some_college_y18,associate_y18,bachelor_y18,graduate_y18)))
  )
```

    ## Rows: 4919 Columns: 770
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (770): GEO_ID, NAME, S1501_C01_001E, S1501_C01_001M, S1501_C01_002E, S15...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
edu19 =
  read_csv("Data/edu/edu_2019.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s1501_c01_007e, s1501_c01_008e, s1501_c01_009e, s1501_c01_010e, s1501_c01_011e, 
         s1501_c01_012e, s1501_c01_013e) |> 
  rename(
    less_9_y19        = s1501_c01_007e,
    no_hs_diploma_y19 = s1501_c01_008e,
    hs_grad_y19       = s1501_c01_009e,
    some_college_y19  = s1501_c01_010e,
    associate_y19     = s1501_c01_011e,
    bachelor_y19      = s1501_c01_012e,
    graduate_y19      = s1501_c01_013e
  ) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) |> 
  mutate_at(c("less_9_y19","no_hs_diploma_y19","hs_grad_y19","some_college_y19","associate_y19",
              "bachelor_y19","graduate_y19"), as.numeric) |> 
  mutate(
    hs_or_less_y19 = rowSums(across(c(less_9_y19,no_hs_diploma_y19,hs_grad_y19))),
    college_y19    = rowSums(across(c(some_college_y19,associate_y19,bachelor_y19,graduate_y19)))
  )
```

    ## Rows: 4919 Columns: 770
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (770): GEO_ID, NAME, S1501_C01_001E, S1501_C01_001M, S1501_C01_002E, S15...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
edu20 =
  read_csv("Data/edu/edu_2020.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s1501_c01_007e, s1501_c01_008e, s1501_c01_009e, s1501_c01_010e, s1501_c01_011e, 
         s1501_c01_012e, s1501_c01_013e) |> 
  rename(
    less_9_y20        = s1501_c01_007e,
    no_hs_diploma_y20 = s1501_c01_008e,
    hs_grad_y20       = s1501_c01_009e,
    some_college_y20  = s1501_c01_010e,
    associate_y20     = s1501_c01_011e,
    bachelor_y20      = s1501_c01_012e,
    graduate_y20      = s1501_c01_013e
  ) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) |> 
  mutate_at(c("less_9_y20","no_hs_diploma_y20","hs_grad_y20","some_college_y20","associate_y20",
              "bachelor_y20","graduate_y20"), as.numeric) |> 
  mutate(
    hs_or_less_y20 = rowSums(across(c(less_9_y20,no_hs_diploma_y20,hs_grad_y20))),
    college_y20    = rowSums(across(c(some_college_y20,associate_y20,bachelor_y20,graduate_y20)))
  )
```

    ## Rows: 5412 Columns: 770
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (770): GEO_ID, NAME, S1501_C01_001E, S1501_C01_001M, S1501_C01_002E, S15...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
edu21 =
  read_csv("Data/edu/edu_2021.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s1501_c01_007e, s1501_c01_008e, s1501_c01_009e, s1501_c01_010e, s1501_c01_011e, 
         s1501_c01_012e, s1501_c01_013e) |> 
  rename(
    less_9_y21        = s1501_c01_007e,
    no_hs_diploma_y21 = s1501_c01_008e,
    hs_grad_y21       = s1501_c01_009e,
    some_college_y21  = s1501_c01_010e,
    associate_y21     = s1501_c01_011e,
    bachelor_y21      = s1501_c01_012e,
    graduate_y21      = s1501_c01_013e
  ) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) |> 
  mutate_at(c("less_9_y21","no_hs_diploma_y21","hs_grad_y21","some_college_y21","associate_y21",
              "bachelor_y21","graduate_y21"), as.numeric) |> 
  mutate(
    hs_or_less_y21 = rowSums(across(c(less_9_y21,no_hs_diploma_y21,hs_grad_y21))),
    college_y21    = rowSums(across(c(some_college_y21,associate_y21,bachelor_y21,graduate_y21)))
  )
```

    ## Rows: 5412 Columns: 770
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (770): GEO_ID, NAME, S1501_C01_001E, S1501_C01_001M, S1501_C01_002E, S15...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
edu_10s = 
  full_join(edu18, edu19, by = "geo_id")

edu_20s = 
  full_join(edu20, edu21, by = "geo_id")

edu_all =
  full_join(edu_10s, edu_20s, by = "geo_id") |> 
  drop_na() |> 
  mutate(
    id = str_sub(geo_id, 10)
  ) 
```

Poverty Data

- below_poverty = % below poverty level among population for whom
  poverty status is determined

``` r
poverty18 =
  read_csv("Data/poverty/2018_poverty.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, name, s1701_c03_001e) |> 
  rename(
    below_poverty_y18 = s1701_c03_001e) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) 
```

    ## Rows: 4919 Columns: 368
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (368): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
poverty19 =
  read_csv("Data/poverty/2019_poverty.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s1701_c03_001e) |> 
  rename(
    below_poverty_y19 = s1701_c03_001e) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) 
```

    ## Rows: 4919 Columns: 368
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (368): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
poverty20 =
  read_csv("Data/poverty/2020_poverty.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s1701_c03_001e) |> 
  rename(
    below_poverty_y20 = s1701_c03_001e) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) 
```

    ## Rows: 5412 Columns: 368
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (368): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
poverty21 =
  read_csv("Data/poverty/2021_poverty.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, s1701_c03_001e) |> 
  rename(
    below_poverty_y21 = s1701_c03_001e) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1))  
```

    ## Rows: 5412 Columns: 374
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (374): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
poverty_10s = 
  full_join(poverty18, poverty19, by = "geo_id")

poverty_20s = 
  full_join(poverty20, poverty21, by = "geo_id")

poverty_all =
  full_join(poverty_10s, poverty_20s, by = "geo_id") |> 
  drop_na() |> 
  mutate(
    id = str_sub(geo_id, 10)
  ) 
```

Vacancy Data

- total_home = total number of properties for residential purposes
- vacant = total number of vacant properties
- prop_vacant = propertion of vacant properties in a census tract

``` r
vacancy18 =
  read_csv("Data/vacancy/2018_vacancy.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, name, b25004_001e, b25004_008e) |> 
  rename(
    total_home_y18 = b25004_001e,
    vacant_y18     = b25004_008e) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) |> 
  mutate_at(c("total_home_y18","vacant_y18"), as.numeric) |> 
  mutate(prop_vacant_18 = vacant_y18/total_home_y18)
```

    ## Rows: 4919 Columns: 18
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (18): GEO_ID, NAME, B25004_001E, B25004_001M, B25004_002E, B25004_002M, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
vacancy19 =
  read_csv("Data/vacancy/2019_vacancy.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, b25004_001e, b25004_008e) |> 
  rename(
    total_home_y19 = b25004_001e,
    vacant_y19     = b25004_008e) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) |> 
  mutate_at(c("total_home_y19","vacant_y19"), as.numeric) |> 
  mutate(prop_vacant_19 = vacant_y19/total_home_y19)
```

    ## New names:
    ## Rows: 4919 Columns: 19
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (18): GEO_ID, NAME, B25004_001E, B25004_001M, B25004_002E, B25004_002M, ... lgl
    ## (1): ...19
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...19`

``` r
vacancy20 =
  read_csv("Data/vacancy/2020_vacancy.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, b25004_001e, b25004_008e) |> 
  rename(
    total_home_y20 = b25004_001e,
    vacant_y20     = b25004_008e) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) |> 
  mutate_at(c("total_home_y20","vacant_y20"), as.numeric) |> 
  mutate(prop_vacant_20 = vacant_y20/total_home_y20)
```

    ## New names:
    ## Rows: 5412 Columns: 19
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (18): GEO_ID, NAME, B25004_001E, B25004_001M, B25004_002E, B25004_002M, ... lgl
    ## (1): ...19
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...19`

``` r
vacancy21 =
  read_csv("Data/vacancy/2021_vacancy.csv", col_names = TRUE) |> 
  janitor::clean_names() |> 
  select(geo_id, b25004_001e, b25004_008e) |> 
  rename(
    total_home_y21 = b25004_001e,
    vacant_y21     = b25004_008e) |> 
  drop_na() |> 
  filter(!row_number() %in% c(1)) |>  
  mutate_at(c("total_home_y21","vacant_y21"), as.numeric) |> 
  mutate(prop_vacant_21 = vacant_y21/total_home_y21)
```

    ## New names:
    ## Rows: 5412 Columns: 19
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (18): GEO_ID, NAME, B25004_001E, B25004_001M, B25004_002E, B25004_002M, ... lgl
    ## (1): ...19
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...19`

``` r
vacancy_10s = 
  full_join(vacancy18, vacancy19, by = "geo_id")

vacancy_20s = 
  full_join(vacancy20, vacancy21, by = "geo_id")

vacancy_all =
  full_join(vacancy_10s, vacancy_20s, by = "geo_id") |> 
  drop_na() |> 
  mutate( 
    id = str_sub(geo_id, 10))
```

It should be noted that the dataset included information about other
types of properties but were unoccupied such as rental properties, those
listed for sale, properties sold but unoccupied, those designated for
migrant workers, and residences that were used
occasionally/seasonally/recreationally. The only variable considered in
this analysis was vacant properties however it is likely that residences
that go unoccupied for several months can become nests for the rat
population, increasing sightings in those areas. A more thorough and
in-depth analysis should include the other variables provided in the
dataset.

In general, might be losing information with the variables chosen for
analysis. During data collection, the variables were further subsetted
into age groups and race/ethnicity designations. However for this
analysis, the general estimates were taken for the population as a whole
in NYC.

When importing data from 2018 and 2019, there were 4918 census tracts
and when importing data from 2020 and 2021, there were 5411 census
tracts. The datasets for each year were joined together by geography id
to only retain census tracts that were consistent across the years.
There may have been adjustments or redfining areas but that could affect
analysis.

Plots:

``` r
crowding_all |> 
  pivot_longer(less_than_1_y18:more_than_1.5_y21,
               names_to = "occupancy",
               values_to = "count") |> 
  mutate(year     = str_sub(occupancy, -3),
         category = str_sub(occupancy, end=-3)) |> 
  mutate(
    year = case_match(
      year,
      "y18" ~ 2018,
      "y19" ~ 2019,
      "y20" ~ 2020,
      "y21" ~ 2021
    )
  )
```

    ## # A tibble: 0 × 8
    ## # ℹ 8 variables: geo_id <chr>, name <chr>, id <chr>, county <chr>,
    ## #   occupancy <chr>, count <chr>, year <dbl>, category <chr>
