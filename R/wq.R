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

ggplot(qaqcSumm_long) +
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


test2 <- qaqc_long %>% 
    mutate(dec.year = lubridate::decimal_date(datetimestamp),
           frac.year = dec.year - Year)


# more of a heat plot
# really, really slow

p <- ggplot(filter(test2, parameter = "f_ph")) +
    geom_tile(aes(x = frac.year, y = Year, fill = flag_detailed),
              width = 0.00008) +
    scale_fill_manual(values = cols) +
    labs(title = "Bangs Lake pH flags",
         x = "Part of Year (0 = Jan 1, 1 = Dec 31)",
         y = "Year",
         fill = "QA/QC Flag") +
    theme_bw()

p
    
# p + khroma::scale_fill_bright()
# p + khroma::scale_fill_okabeito()



# save(test4, file = here::here("data", "aa_gndbl.RData"))



