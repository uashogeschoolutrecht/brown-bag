#########################################
## Covid demo - Brown Bag Github Demo 
## 2 March 2021
## Marc A.T. Teunis, PhD
########################################

## Packages
library(tidyverse)
library(naniar)

## Data (RIVM)
download.file(
  url = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",
  destfile = here::here(
    "data",
    "COVID-19_aantallen_gemeente_per_dag.csv"
  )
  
)

## Data load
data_covid_per_day <- read_csv2(
  here::here(
    "data",
    "COVID-19_aantallen_gemeente_per_dag.csv"
  )
)

## Inspect
data_covid_per_day

## Provices
unique(data_covid_per_day$Province) %>% length()
unique(data_covid_per_day$date_of_report)


## Clean col names
data_covid_per_day <- data_covid_per_day %>%
  janitor::clean_names()

## Missing
naniar::vis_miss(data_covid_per_day %>% sample_frac(0.1))

## EDA
data_covid_per_day %>%
  na.omit() %>%
  group_by(
    province, 
    date_of_publication) %>%
  summarise(
    total_per_day_hospital = sum(
      hospital_admission)
    ) -> data_per_day_province

data_per_day_province %>% 
  ggplot(aes(x = date_of_publication, y = log10(total_per_day_hospital))) +
  geom_point(aes(colour = province))

## Facets  
data_per_day_province %>% 
  ggplot(aes(
    x = date_of_publication, 
    y = total_per_day_hospital)) +
  geom_point(aes(colour = province)) +
  facet_wrap(~province)

## Floor date to a week
data_per_day_province %>%
  mutate(
    per_week = lubridate::floor_date(
      date_of_publication, 
      unit = "week")) %>%
      group_by(province, per_week) %>%
  summarise(hosp_per_week = sum(total_per_day_hospital)) %>%
  ggplot(aes(
    x = per_week, 
    y = hosp_per_week)) +
  geom_point(aes(colour = province)) +
  facet_wrap(~province)

## With lines
data_per_day_province %>%
  mutate(
    per_week = lubridate::floor_date(
      date_of_publication, 
      unit = "week")) %>%
  group_by(province, per_week) %>%
  summarise(hosp_per_week = sum(total_per_day_hospital)) %>%
  ggplot(aes(
    x = per_week, 
    y = hosp_per_week)) +
  geom_line(aes(colour = province, group = province),
            show.legend = FALSE) +
  facet_wrap(~province) +
  toolboxr::rotate_axis_labels("x", 90)

  # Let's fix some things
  # - rotate axis
  # - hide legend
  # - fix axis labels
  # - apply white background 

## How are we doing on the National scale - deceased
data_covid_per_day %>%
  na.omit() %>%
  mutate(
    per_week = lubridate::floor_date(
      date_of_publication, 
      unit = "week")) %>%
  group_by(per_week) %>%
  summarise(total_deceased = sum(deceased)) %>%
  ggplot(aes(
    x = per_week,
    y = total_deceased
  )) + 
  geom_line()



