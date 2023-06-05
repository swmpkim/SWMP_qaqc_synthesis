library(SWMPr)
library(tidyr)
library(dplyr)
library(ggplot2)
source(here::here("R", "functions.R"))


# hoping to set up in a way to loop through stations and process all files
stn <- "gndblnut"



dat <- import_local(path = here::here("data"), 
                    station_code = stn, 
                    keep_qaqcstatus = TRUE,
                    trace = TRUE)

# keep only qaqc cols for later viz
qaqc_only <- keep_onlyQAQC(dat)

# summary table of flags and codes by parameter
qaqc_summ <- qaqcchk(dat) |>
    tidyr::separate(flag, into = c("flag", "code"),
                    sep = " ",
                    extra = "merge",
                    fill = "right")

# or some table function for Shiny
knitr::kable(qaqc_summ)


# visualize how frequently these flags are used for certain params
qaqc_long <- qaqc_summ %>% 
    pivot_longer(-(1:2),
                 names_to = "param",
                 values_to = "count") %>% 
    mutate(flag = factor(flag)) %>% 
    group_by(flag, param) %>% 
    summarize(count = sum(count, na.rm = TRUE))

ggplot(qaqc_long) +
    geom_col(aes(x = param, y = count, fill = flag)) +
    scale_fill_brewer(type = "qual", palette = "Paired") +
    theme_bw()

test <- dat %>% 
    select(datetimestamp, f_chla_n) %>% 
    separate(f_chla_n, 
             into = c("flag", "code"),
             sep = " ",
             extra = "merge",
             fill = "right") %>% 
    mutate(flag = factor(flag))

ggplot(test) +
    geom_linerange(aes(x = datetimestamp,
                       ymin = 0,
                       ymax = 1,
                       col = flag)) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    theme_bw()


# geom_raster might be what I want
# year on y-axis
# day of year/fraction of day on x-axis
# fill = flag

test2 <- test %>% 
    mutate(Year = lubridate::year(datetimestamp),
           dec.year = lubridate::decimal_date(datetimestamp),
           frac.year = dec.year - Year)

ggplot(test2) +
    geom_point(aes(x = frac.year, y = Year, col = flag)) +
    scale_color_brewer(palette = "Paired")

# nutrient
ggplot(test2) +
    geom_tile(aes(x = frac.year, y = Year, fill = flag),
              col = "gray80",
              width = 1/12) +
    scale_fill_brewer(palette = "Paired") +
    theme_bw()

# wq

