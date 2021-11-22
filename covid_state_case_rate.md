The association between covid and state in US
================
Yujin Zhang

This is an R markdown document about covid vs. state data analysis

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(naniar)
library(usmap)
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
```

## Part 1

In this part I input the CDC covid cases data by state with time period
till Thu Nov 18 2021.

The data generated: Thu Nov 18 2021 22:02:06 GMT-0500 (EST)

``` r
# input data table for Total Cases by State/Territory
covid_total_by_state = 
  read_csv("./data/united_states_covid19_cases_deaths_and_testing_by_state.csv", skip = 2) %>% 
  janitor::clean_names()
```

    ## Rows: 62 Columns: 25

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (25): State/Territory, Level of Community Transmission, Total Cases, Con...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

For the missing data, the original dataset fill it with “N/A”, so I will
replace it with NA and change the variable to numeric.

``` r
# figure out the missing data in data frame
na_strings = "N/A"

# fill in missing data
covid_total_by_state_tidy =
  covid_total_by_state %>% 
  replace_with_na_all(condition = ~.x %in% na_strings) %>% 
  mutate(
    state_territory = as.factor(state_territory),
    case_rate_per_100000 = as.numeric(case_rate_per_100000)
         ) %>% 
  rename(region = "state_territory") %>% 
  select(region, case_rate_per_100000)
```

In the following steps, I will make a choropleth map plot to show the
total covid cases by state in US.

First create a new dataset for plot.

``` r
# load geographic coordinates of each US state
all_state = usmap::us_map() %>% 
  relocate(full)

# combine two dataset
plot_df = left_join(covid_total_by_state_tidy, all_state, by = c("region" = "full"))
```

Add state abbreviations to the map figure.

``` r
us_centroids = 
  plot_df %>% 
  group_by(region) %>% 
  summarise(centroid.x = mean(range(x)), 
            centroid.y = mean(range(y)),
            label = unique(toupper(str_sub(region,1,2))),
            case_rate_per_100000 = unique(case_rate_per_100000))
```

Now, let’s draw the plot.

``` r
plot_df %>% 
  group_by(region) %>% 
  ggplot() +
  geom_polygon(aes(x, y, group = group, fill = case_rate_per_100000)) +
  geom_text(
    data = us_centroids, 
    aes(centroid.x, centroid.y, label = paste(label, "\n", case_rate_per_100000)),
    size = 1
  ) +
  labs(title = "COVID-19 Case Rate by State/Territory (cases per 100,000)", fill = "Cases Rate Per 100,000") +
  # hide ticks on x and y axis
  scale_y_continuous(breaks = c()) + 
  scale_x_continuous(breaks = c()) +
  scale_fill_gradient2(low = "white", mid = "skyblue2", high = "skyblue4", midpoint = 10000) +
  # theme refer from RPubs
  theme(panel.background = element_rect(fill = "grey"),
        plot.background = element_rect(fill = "grey"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border =  element_blank(),
        plot.title = element_text(
          size = 15, hjust = 0.5, family = "Times",colour = "white"),
        legend.title = element_text(
          hjust = 0.4 ,vjust = 0.3, size = 10,family = "Times"),
        legend.text = element_text( hjust = 0.4 ,vjust = 2, size = 8,family = "Times"))
```

    ## Warning: Removed 12 rows containing missing values (geom_text).

<img src="covid_state_case_rate_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

## Part 2

This data frame is the covid case rate per 100000 till Dec.31.2020.

``` r
# read data function
read_data_function = function(file_df) {
  
  state_df = 
    read_csv(file_df, skip = 2)
  
  return(state_df)
}

# create a data frame containing all participants
state_2020 =
  tibble(
    files = list.files("./data/state_covid_separate")
  ) %>% 
  mutate(
    path = map(.x = files, ~paste("./data/state_covid_separate", ., sep = "/"))
  ) %>%                                                             # add path
  mutate(
    observations = map(path, read_data_function))
```

Then I unnest the data frame and filter the cases on Dec.31.2020

``` r
state_2020_tidy =
  state_2020 %>% 
  unnest(cols = "observations") %>% 
  janitor::clean_names() %>% 
  filter(date == "Dec 31 2020") %>%
  select(state, total_cases, total_case_rate_per_100k)
```

Make map plot.

``` r
plot_2020_df = left_join(state_2020_tidy, all_state, by = c("state" = "full"))

centroids_2020 = 
  plot_2020_df %>% 
  group_by(state) %>% 
  summarise(centroid.x = mean(range(x)), 
            centroid.y = mean(range(y)),
            label = unique(toupper(str_sub(state,1,2))),
            total_case_rate_per_100k = unique(total_case_rate_per_100k))
```

``` r
plot_2020_df %>% 
  group_by(state) %>% 
  ggplot() +
  geom_polygon(aes(x, y, group = group, fill = total_case_rate_per_100k)) +
  geom_text(
    data = centroids_2020, 
    aes(centroid.x, centroid.y, label = paste(label, "\n", total_case_rate_per_100k)),
    size = 1
  ) +
  labs(title = "COVID-19 Case Rate by State/Territory (cases per 100,000) 2020", fill = "Cases Rate Per 100,000") +
  # hide ticks on x and y axis
  scale_y_continuous(breaks = c()) + 
  scale_x_continuous(breaks = c()) +
  scale_fill_gradient2(low = "white", mid = "skyblue2", high = "skyblue4", midpoint = 6000) +
  # theme refer from RPubs
  theme(panel.background = element_rect(fill = "grey"),
        plot.background = element_rect(fill = "grey"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border =  element_blank(),
        plot.title = element_text(
          size = 15, hjust = 0.5, family = "Times",colour = "white"),
        legend.title = element_text(
          hjust = 0.4 ,vjust = 0.3, size = 10,family = "Times"),
        legend.text = element_text( hjust = 0.4 ,vjust = 2, size = 8,family = "Times"))
```

    ## Warning: Removed 7 rows containing missing values (geom_text).

<img src="covid_state_case_rate_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />
