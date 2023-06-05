library(SWMPr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
source(here::here("R", "sourced", "functions.R"))


# hoping to set up in a way to loop through stations and process all files
stn <- "gndblwq"


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

knitr::kable(qaqc_summ)
# or some table function for Shiny

# visualize how frequently these flags are used for certain params
# pivot the summary table
qaqcSumm_long <- qaqc_summ %>% 
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


# pivot original flags
qaqc_long <- qaqc_only %>% 
    mutate(Year = lubridate::year(datetimestamp),
           Month = lubridate::month(datetimestamp),
           Day = lubridate::mday(datetimestamp)) %>% 
    pivot_longer(cols = starts_with("f_"),
                 names_to = "parameter",
                 values_to = "flagCode") %>% 
    mutate(flag = extract_flag(flagCode),
           flag_detailed = case_match(flag,
                                      "<-5>" ~ "Above Detection Limit",
                                      "<-4>" ~ "Below Detection Limit",
                                      "<-3>" ~ "Rejected",
                                      "<-2>" ~ "Missing",
                                      "<-1>" ~ "Optional Param; not collected",
                                      "<0>" ~ "Passed QAQC",
                                      "<2>" ~ "Reserved for future use",
                                      "<1>" ~ "Suspect",
                                      "<3>" ~ "Calculated from other Params",
                                      "<4>" ~ "Pre-auto QAQC",
                                      "<5>" ~ "Corrected Data",
                                      .default = "PROBLEM!"))

qaqc_monthly <- qaqc_long %>% 
    group_by(Year, Month, parameter, flag_detailed) %>% 
    tally()

cols <- c("Passed QAQC" = "gray",
          "Calculated from other Params" = "gray",
          "Pre-auto QAQC" = "gray40",
          "Corrected Data" = "gray20",
          "Missing" = "blue3",
          "Rejected" = "red",
          "Suspect" = "orange",
          "Above Detection Limit" = "green3",
          "Below Detection Limit" = "darkgreen",
          "Reserved for future use" = "purple",
          "Problem!" = "purple"
          )
    
ggplot(filter(qaqc_monthly, parameter == "f_ph")) +
    geom_col(aes(x = Year, y = n,
                 fill = flag_detailed)) +
    facet_wrap(~Month) +
    theme_bw() +
    scale_fill_manual(values = cols) +
    labs(title = "Bangs Lake pH QAQC flags, faceted by month")

ggplot(filter(qaqc_monthly, parameter == "f_do_mgl")) +
    geom_col(aes(x = Year, y = n,
                 fill = flag_detailed)) +
    facet_wrap(~Month) +
    theme_bw() +
    scale_fill_manual(values = cols) +
    labs(title = "Bangs Lake DO(mg/L) QAQC flags, faceted by month")


test2 <- test %>% 
    mutate(Year = lubridate::year(datetimestamp),
           dec.year = lubridate::decimal_date(datetimestamp),
           frac.year = dec.year - Year)

ggplot(test2) +
    geom_point(aes(x = frac.year, y = Year, col = flag)) +
    scale_color_brewer(palette = "Paired")

# wq
# really only width that depends on type of data?
ggplot(test2) +
    geom_tile(aes(x = frac.year, y = Year, fill = flag),
              width = 0.00008) +
    scale_fill_brewer(palette = "Paired") +
    theme_bw()


p <- ggplot(test2) +
    geom_tile(aes(x = frac.year, y = Year, fill = flag),
              width = 0.00008)
    
p + khroma::scale_fill_bright()
p + khroma::scale_fill_okabeito()


test3 <- test2 %>% 
    mutate(flag2 = case_match(flag,
                              "<1>" ~ "SorR",
                              "<-3>" ~ "SorR",
                              "<0>" ~ "okay",
                              .default = "other"))
ggplot(test3) +
    geom_tile(aes(x = frac.year, y = Year, fill = flag2),
              width = 0.00008) +
    khroma::scale_fill_okabeito()

# save(test4, file = here::here("data", "aa_gndbl.RData"))


test4 <- test2 %>% 
    filter(flag != "<0>")

ggplot(test4) +
    geom_tile(aes(x = frac.year, y = Year, fill = flag),
              width = 0.00008) +
    khroma::scale_fill_okabeito()


qaqc_monthly <- qaqc_only %>% 
    mutate(Year = lubridate::year(datetimestamp),
           Month = lubridate::month(datetimestamp),
           Day = lubridate::mday(datetimestamp)) %>% 
    pivot_longer(cols = starts_with("f_"),
                 names_to = "parameter",
                 values_to = "flag-code") %>% 
    separate(`flag-code`, 
             into = c("flag", "code"),
             sep = " ",
             extra = "merge",
             fill = "right")

totest <- qaqc_only %>% 
    mutate(Year = lubridate::year(datetimestamp),
           Month = lubridate::month(datetimestamp),
           Day = lubridate::mday(datetimestamp)) %>% 
    pivot_longer(cols = starts_with("f_"),
                 names_to = "parameter",
                 values_to = "flag-code")    

moretest <- moretest %>% 
    mutate(ph_flag = stringr::str_extract(f_ph, "<-?\\d>"))


tested <- totest %>% 
    mutate(flag = extract_flag(`flag-code`))
