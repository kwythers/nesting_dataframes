#############################################################
#                                                           #
#                                                           #
#   Stream clairity trend data processing                   #
#   Kirk R. Wythers                                         #
#   2018.01.01                                              #
#   Purpose: Wrangle the lake clarity data, produce "big    #
#     picture" analysis figures, run Mann-Kendall           #
#     temporal analysis (blocking on months and flow),      #
#     create figures and  write out 'results' table         #
#                                                           #
#                                                           #
#############################################################

# load libraries
library(RODBC)
library(tidyverse)
library(broom)
library(purrr)
library(modelr)
library(tidyquant)
library(lubridate)
library(sqldf)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(ggmap)
library(zoo)
library(rkt)
#library(censReg)
#library(VGAM)
#library(AER)
#library(crch)
#library(mvoutlier)
#library(Trends)
library(EnvStats)
library(survival)
#library(reprex)
#library(datapasta)

######################################################
#####    Connect to WH_TEMPO using ODCconnect    #####
######################################################

# Set up new User DSN on your computer
# Control Panel/Administrative Tools/Data Sources (ODBC)
# Driver = Oracle in OraClient11g_home2
# Data Source Name = DeltaW
# TNS Service Name = deltaw.pca.state.mn.us

# # open ODBC connection to dev
# deltad <- odbcConnect('deltad', uid='tableau', pwd='data_origamiW')
# 
# # view tables inside a given schema
# wh_tempo_tables <- sqlTables(deltad, schema = 'WH_TEMPO')
# odbcGetInfo(deltad)
# 
# # load table into R
# 
# equis_stream_tube_data <- as.tibble(sqlQuery(deltad, '
#                                              SELECT * 
#                                              FROM
#                                              WH_TEMPO.EQUIS_STREAM_SECCHI_TUBE_DATA', stringsAsFactors = FALSE))
# 
# # when you are ready to close the ODBC connection
# odbcClose(deltad)


##### open ODBC connection to prod
deltaw <- odbcConnect('deltaw', uid='tableau', pwd='data_origamiW')

##### view tables inside a given schema
wh_tempo_tables <- sqlTables(deltaw, schema = 'WH_TEMPO')
odbcGetInfo(deltaw)

##### load table into R
equis_stream_tube_data <- as.tibble(sqlQuery(deltaw, '
                                  SELECT *
                                  FROM
                                  WH_TEMPO.EQUIS_STREAM_SECCHI_TUBE_DATA', stringsAsFactors = FALSE))

##### when you are ready to close the ODBC connection
odbcClose(deltaw)

names(equis_stream_tube_data)

######################################################
##### Clean up and assign dates  #####
######################################################

##### convert all column names to lower case and summerze data and examin structure
equis_stream_tube_data <- rename_all(equis_stream_tube_data, tolower) 
names(equis_stream_tube_data)
summary(equis_stream_tube_data) 
str(equis_stream_tube_data)

##### editing data so date is seen as a date by R
equis_stream_tube_data$sample_date <- as.Date(equis_stream_tube_data$sample_date, 
                                                     format = '%m/%d/%Y', origin = '1961-09-18')
##### add columns for year, month, day, and doy
equis_stream_tube_data <- equis_stream_tube_data %>% 
  mutate(year = year(sample_date), 
         month = month(sample_date),
         day = day(sample_date),
         doy = yday(sample_date), 
         ym = format(ymd(sample_date), '%Y-%m'))

##### add columns for huc4
equis_stream_tube_data <- equis_stream_tube_data %>% 
  mutate(huc4 = str_sub(loc_major_basin, 1, 4)) %>% 
  select(huc4, loc_major_basin, everything())

##### quick look
equis_stream_tube_data
##### confirm that the Date variable has a minimum and a maximum
summary(equis_stream_tube_data) 

##### convert instrument exceedences from "NA" and ">" to "T" and "F" (TRUE and FALSE)
equis_stream_tube_data <- equis_stream_tube_data %>% 
  replace_na(list(gt_ttube_60_raw = F, gt_ttube_60_conv = F, 
                  gt_ttube_100_raw = F, gt_ttube_120_raw = F, 
                  gt_ttube_100_conv = F, gt_stube_100_raw = F, 
                  gt_combined_stube_cv100_cv60 = F))

equis_stream_tube_data$gt_ttube_60_raw <- recode_factor(equis_stream_tube_data$gt_ttube_60_raw, '>' = T, .default = F) 
equis_stream_tube_data$gt_ttube_60_conv <- recode_factor(equis_stream_tube_data$gt_ttube_60_conv, '>' = T, .default = F) 
equis_stream_tube_data$gt_ttube_100_raw <- recode_factor(equis_stream_tube_data$gt_ttube_100_raw, '>' = T, .default = F) 
equis_stream_tube_data$gt_ttube_120_raw <- recode_factor(equis_stream_tube_data$gt_ttube_120_raw, '>' = T, .default = F) 
equis_stream_tube_data$gt_ttube_100_conv <- recode_factor(equis_stream_tube_data$gt_ttube_100_conv, '>' = T, .default = F) 
equis_stream_tube_data$gt_stube_100_raw <- recode_factor(equis_stream_tube_data$gt_stube_100_raw, '>' = T, .default = F)
equis_stream_tube_data$gt_combined_stube_cv100_cv60 <- recode_factor(equis_stream_tube_data$gt_combined_stube_cv100_cv60, '>' = T, .default = F)


equis_stream_tube_data %>% 
  group_by(sys_loc_code) %>% 
  tally()

##### looking for problem streams
# trouble <- equis_stream_tube_data %>% 
#   filter(sys_loc_code =='S000-046')

##### Gather converted measures for all tubes
# clarity_conv_long <- equis_stream_tube_data %>%
#   gather(ttube_60_conv, ttube_100_conv, combined_stube_conv100_conv60, key = 'tube_conv', value = 'clarity_conv') %>% 
#   gather(gt_ttube_60_conv, gt_ttube_100_conv, gt_combined_stube_cv100_cv60, 
#          key = 'length_conv', value = 'over_detect_conv') %>%
#   # mutate(over_detect = replace(over_detect, over_detect == ">", "T")) %>%
#   # mutate(over_detect = replace(over_detect, which(is.na(over_detect)), "F")) %>% 
#   mutate(ym = as.yearmon(paste(y, m), "%Y %m")) %>% 
#   select(huc4, basin_name, loc_major_basin, sys_loc_code, sample_date, y, m, d, doy, ym, 
#          stage, over_detect_conv, tube_conv, clarity_conv)

##### Gather raw measures for all tubes
# clarity_raw_long <- equis_stream_tube_data %>%
#   gather(ttube_60_raw, ttube_100_raw, stube_100_raw, ttube_120_raw, key = 'tube_raw', value = 'clarity_raw') %>% 
#   gather(gt_ttube_60_raw, gt_ttube_100_raw, gt_stube_100_raw, gt_ttube_120_raw, 
#          key = 'length_raw', value = 'over_detect_raw') %>% 
#   # mutate(over_detect = replace(over_detect, over_detect == ">", "T")) %>%
#   # mutate(over_detect = replace(over_detect, which(is.na(over_detect)), "F")) %>% 
#   mutate(ym = as.yearmon(paste(y, m), "%Y %m")) %>% 
#   select(huc4, basin_name, loc_major_basin, sys_loc_code, sample_date, y, m, d, doy, ym, 
#          stage, over_detect_raw, tube_raw, clarity_raw)

##### filter for combined_stube_conv100_conv60
# 
# clarity <- clarity_conv_long %>% 
#   filter(tube_conv == 'combined_stube_conv100_conv60') %>% 
#   na.omit()
# 
# clarity_ %>% 
#   group_by(over_detect_conv) %>% 
#   tally()
# 
# clarity_nod <- clarity %>% 
#   filter(over_detect_conv == 'FALSE')
# 
# clarity_nod %>% 
#   filter(clarity_conv < 100)

clarity_converted <- equis_stream_tube_data %>% 
  select(huc4, loc_major_basin, sys_loc_code, basin_name, sample_date, year, month, day, doy, ym, stage, gt_combined_stube_cv100_cv60, combined_stube_conv100_conv60) %>% 
  filter(!is.na(combined_stube_conv100_conv60)) %>% 
  rename(detection_limit = gt_combined_stube_cv100_cv60)

# # looking for problem streams
# trouble <- clarity_converted %>%
#   filter(sys_loc_code =='S000-046')

##### new colum for stage as numbers ("no flow" = Z, "low flow" = L, "normal flow" = N, "high flow" = H)
clarity_converted <- clarity_converted %>% 
  mutate(flow = ifelse(stage == 'Z', 1,
                ifelse(stage == 'L', 2,
                ifelse(stage == 'N', 3,
                ifelse(stage == 'H', 4, NA)))))

##### new colum for stage as numbers ("no flow" = Z, "low flow" = L, "normal flow" = N, "high flow" = H)
clarity_converted$stage <- recode_factor(clarity_converted$stage, 'Z' = 'No Flow')
clarity_converted$stage <- recode_factor(clarity_converted$stage, 'L' = 'Low')
clarity_converted$stage <- recode_factor(clarity_converted$stage, 'N' = 'Normal')
clarity_converted$stage <- recode_factor(clarity_converted$stage, 'H' = 'High')
##### replace factor NA with Unknown
clarity_converted$stage <- fct_explicit_na(clarity_converted$stage, na_level = 'Unknown')

##### add record length for each stream
clarity_converted <- clarity_converted %>% 
  group_by(sys_loc_code) %>% 
  mutate(record_length = max(year) - min(year) + 1) %>% 
  mutate(obs = n_distinct(sample_date))

# # count streams with record_length of eight years or more
# clarity_converted %>% 
#   select(sys_loc_code, record_length) %>% 
#   group_by(sys_loc_code) %>% 
#   filter(record_length >= 8) %>% 
#   summarise(n()) %>% 
#   arrange(`n()`)

# find streams with fewer than 8 years of data and fewer than 50 observations and write file to output
too_little_data <- clarity_converted %>% 
  filter(record_length < 8) %>% 
  filter(obs < 50) %>% 
  group_by(sys_loc_code, record_length, obs) %>% 
  summarise()

write_csv(too_little_data, path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/too_little_data.csv')

##### keep streams with record_length of two years or more data (min number to run test)
clarity_converted <- clarity_converted %>%
  filter(record_length >= 2)

##### keep streams with eight clarity observations or more (min number to run test)
clarity_converted <- clarity_converted %>%
  filter(obs >= 8)

##### temporty fix for a 5040 cm clarity record 
##### clarity_converted <- clarity_converted %>% filter(combined_stube_conv100_conv60 < 200)
  
######################################################
#####             Make some figures              #####
######################################################

##### histogram of water clarity converted measures 
ggplot(clarity_converted, aes(x = combined_stube_conv100_conv60)) + 
  geom_histogram(binwidth = 1, fill = 'lightblue', color = 'black') + 
  labs(title = 'Number of clarity measures for all converted tube types') +
  theme(strip.text.y = element_text(angle = 0))
  # + scale_y_sqrt()  # transform y axis with sqrt funcion to see small values better

ggplot(clarity_converted, aes(x = combined_stube_conv100_conv60)) + 
  geom_histogram(binwidth = 1, fill = 'lightblue', color = 'black') + 
  labs(title = 'Number of clarity measures for all converted tube types') +
  facet_grid(stage ~ ., scales = 'free') + 
  theme(strip.text.y = element_text(angle = 0)) 
    # + scale_y_sqrt()  # transform y axis with sqrt funcion to see small values better

####################################################################
#######  calculate p value, theil senns slope for with tobit #######
#######  regression on years months and flow for censored data #####
#######  on 60 and 100 cm censor limits                      #######
####################################################################

##### by stream #####

##### make a table with all streams data using censored seasonal tobit model
tb <- clarity_converted %>%
  select(huc4, loc_major_basin, sys_loc_code, sample_date, year, month, day, doy, flow, 
         stage, combined_stube_conv100_conv60, detection_limit, record_length, obs) %>%
  filter(combined_stube_conv100_conv60 > 0)

###################################################################################
##### survival analysis - use tobit regression to handle two censored levels  #####
###################################################################################

##### add censoring limits 
tb_cens <- tb %>%
  mutate(
    censored1 = combined_stube_conv100_conv60 == 60,
    censored2 = combined_stube_conv100_conv60 == 100
    )

##### define left and right variables
tb_cens <- tb_cens %>%
  mutate(
    left_clarity = case_when(
      combined_stube_conv100_conv60 <= 0 ~ -Inf,
      combined_stube_conv100_conv60 > 0 ~ combined_stube_conv100_conv60
    ),
    right_clarity = case_when(
      !censored1 & !censored2 ~ combined_stube_conv100_conv60,
      censored1 | censored2 ~ Inf
      )
  )

##### linear model
linear_model <- lm(combined_stube_conv100_conv60 ~ year + month, data = tb_cens)

summary(linear_model)

##### seasonal kendall model
# kendallmodel <- rkt(tb_cens$year, tb_cens$combined_stube_conv100_conv60, tb_cens$month, rep = 'a', correct = TRUE)

##### Survival (censored) tobit model
tobit_model <- survreg(Surv(left_clarity, right_clarity, type = 'interval2') ~ year + month,
             data = tb_cens, dist = 'gaussian')

summary(tobit_model)

# plot lm and tobit fits
# ggplot(tb_cens, aes(x = year, y = combined_stube_conv100_conv60)) +
#   geom_point() + 
#   geom_abline(intercept = -2245, slope = 1.139, color = 'blue') +
#   geom_abline(intercept = -1083, slope = 0.565, color = 'green')

ggplot(tb_cens, aes(x = year, y = combined_stube_conv100_conv60)) + 
  geom_point(alpha = 0.1, size = 0.25) + 
  ylim(0, 130) + 
  ylab('Clarity (cm)') + 
  xlim(1995, 2020) +
  xlab('Year') + 
  geom_abline(intercept = linear_model$coefficients[1], slope = linear_model$coefficients[2], color = 'blue') +
  geom_abline(intercept = tobit_model$coefficients[1], slope = tobit_model$coefficients[2], color = 'green') + 
  geom_smooth(color = 'red') +
  ggtitle('Minnesota stream Secchi tube measures')

##### Show descriptive stats on censoring (easier with tidyverse)
clarity_out <- as.tibble(tobit_model$y[, 1:3])

##### 'time1' in include only y values for observations used
obs_used <- length(clarity_out$time1)

##### 2: left censored, 1: point, 0: right censored
censoring <- clarity_out %>%
  group_by(status) %>%
  summarise(
    count = n()
  )

right <- censoring %>% 
  filter(status == 0) %>% 
  select(count)

##### Example for including in LaTeX file
cat(
  "Of the", obs_used, "observations,", as.numeric(right[1,1]), "are right censored."
)

##### percentage of data that are censored
cat(
  as.numeric(right[1,1]) / obs_used * 100, "percent of the measures are right censored."
)

##### add column and calculate percentage of obs that are censored 
##### filter out steams that have too many censored values relative to measured values
tb_cens <- tb_cens %>% 
  group_by(sys_loc_code) %>% 
  add_tally(right_clarity == 'Inf') %>% 
  mutate(pct_cens = n / obs * 100)

##### save streams with over 50% observations censored
tb_cens_gt50 <- tb_cens %>% 
  filter(pct_cens > 50) %>%  
  group_by(huc4, loc_major_basin, sys_loc_code, pct_cens) %>% 
  summarise(
    count = n(), 
    median = median(combined_stube_conv100_conv60)
  )
##### write_csv(tb_cens_gt50, path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2017/streams_over50%_censored_values.csv')
write_csv(tb_cens_gt50, path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/cens_gt50.csv')

##### drop streams with more than 50 percent censored obserbations
tb_cens_50 <- tb_cens %>%
  filter(pct_cens <= 50)

##### count locations with a percentage of observations
tb_cens %>% 
  group_by(sys_loc_code) %>% 
  tally()

tb_cens_50 %>% 
  group_by(sys_loc_code) %>% 
  tally()

##### create the model (tobit and ols) functions ##### use tb_cens in order to use all data, censor at end
cens_model <- function(tb_cens) {
  survreg(Surv(left_clarity, right_clarity, type = 'interval2') ~ year + month, data = tb_cens, dist = 'gaussian')
}

linear_model <- function(tb_cens) {
  lm(combined_stube_conv100_conv60 ~ year + month, data = tb_cens)
}

##### by huc8 watershed ##### 
by_watershed <- tb_cens %>% 
  group_by(loc_major_basin) %>% 
  nest()
by_watershed

##### apply the tobit data model to each watershed and mutate results to data frame
by_watershed <- by_watershed %>% 
  mutate(censmodel = map(data, cens_model))
by_watershed

##### add tidy and glance results to table lists
by_watershed <- by_watershed %>%
  mutate(tidy = map(censmodel, tidy),
         glance = map(censmodel, glance)
  )
by_watershed

##### unnest glance list
by_watershed_g <- by_watershed %>% 
  unnest(glance) %>% 
  select(loc_major_basin, data, censmodel, p.value)
by_watershed_g

##### unnest and spread the tidy list [4x7] into 28 rows for each watershed
by_watershed_t <- by_watershed %>% 
  unnest(tidy) %>% 
  select(loc_major_basin, term, estimate) %>% 
  spread(key = term, value = estimate) %>%
  select(loc_major_basin, `(Intercept)`, year)

##### join tables to get pvalues and slopes in a single table, rename variables to 
##### make jointing with stream table easier to follow
by_watershed_model <- left_join(by_watershed_g, by_watershed_t) %>% 
  rename(watershed_data = data, watershed_pvalue = p.value, watershed_intercept = `(Intercept)`, watershed_slope = year) 
by_watershed_model

##### add figures to list
by_watershed_figs <- by_watershed_model %>% 
  mutate(tobit_line = pmap(list(watershed_data, watershed_intercept, watershed_slope), 
                      function(a, b, c) ggplot(a, aes(x = year, y = combined_stube_conv100_conv60)) +
                        geom_point(alpha = 0.5, size = 0.5) + 
                        # geom_jitter() +
                        geom_abline(intercept = b, slope = c, color = 'blue') +
                        ylim(0, 130) +
                        ylab('Clarity (cm)') +
                        xlim(1995, 2020) +
                        xlab('Year')
                      )
         )
by_watershed_figs

by_watershed_figs <- by_watershed_figs %>% 
  mutate(
    map(watershed_data, ~ ggplot(., aes(x = year, y = combined_stube_conv100_conv60)) + 
          geom_point(alpha = 0.5, size = 0.5) + 
          # labs(x = 'Year',
          #      y = 'Clarity (cm)',
               # title = 'Minnesota stream Secchi tube measures',
               # subtitle = loc_major_basin) +
          ylim(0, 130) +
          ylab('Clarity (cm)') +
          xlim(1995, 2020) +
          xlab('Year') +
          # geom_jitter() +
          geom_smooth(method = 'loess') 
          # geom_abline(intercept = watershed_intercept, slope = watershed_slope, color = 'orange')
          # geom_abline(intercept = tobit_model$coefficients, slope = tobit_model$coefficients[2], color = 'green') 
          # ggtitle(label = loc_major_basin)
          # ggtitle('Minnesota stream Secchi tube measures')
    )
  ) %>% 
  rename(plots =`map(...)`)

by_watershed_figs
by_watershed_figs$tobit_line[1] # make one figure
by_watershed_figs$plots[1] # make one figure


####################################################################
########            Plot the raw data - by watershed        ########
####################################################################
# map2(paste0(by_watershed_figs$loc_major_basin, ".png"), by_watershed_figs$plots, 
#      path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/figures/huc8s/', 
#      ggsave)

map2(paste0(by_watershed_figs$loc_major_basin, "-watershed", ".png"), by_watershed_figs$plots,
     path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/figures/huc8s',
     width=8, height=4.5, dpi=100,
     ggsave)

##### by stream #####
by_stream <- tb_cens_50 %>% 
  group_by(sys_loc_code, loc_major_basin) %>% 
  nest() %>% 
  rename(stream_data = data)
by_stream

by_huc8 <- tb_cens_50 %>% 
  group_by(loc_major_basin) %>% 
  nest() %>% 
  rename(huc8_data = data)
by_huc8

# join huc8 data to by_stream
by_stream <- left_join(by_stream, by_huc8, by = 'loc_major_basin')
by_stream

##### apply the tobit data model to each watershed and mutate results to data frame
by_stream <- by_stream %>% 
  mutate(stream_censmodel = map(stream_data, cens_model))
by_stream

##### add tidy and glance results to table lists
by_stream <- by_stream %>%
  mutate(tidy = map(stream_censmodel, tidy),
         glance = map(stream_censmodel, glance)
  )
by_stream

##### unnest glance list
by_stream_g <- by_stream %>% 
  unnest(glance) %>% 
  select(sys_loc_code, loc_major_basin, stream_data, huc8_data, stream_censmodel, p.value)
by_stream_g

##### unnest and spread the tidy list [4x7] into 28 rows for each watershed
by_stream_t <- by_stream %>% 
  unnest(tidy) %>% 
  select(sys_loc_code, loc_major_basin, term, estimate) %>% 
  spread(key = term, value = estimate) %>%
  select(sys_loc_code, `(Intercept)`, year)

##### join tables to get pvalues and slopes in a single table, rename variables to 
##### make jointing with stream table easier to follow
by_stream_model <- left_join(by_stream_g, by_stream_t, by = 'sys_loc_code') %>% 
  rename(stream_pvalue = p.value, stream_intercept = `(Intercept)`, stream_slope = year) 
by_stream_model

##### add figures to list #####
# geom_smooth figures for all all stream stages combined
by_stream_figs <- by_stream_model %>% 
  mutate(
    map(stream_data, ~ ggplot(., aes(x = year, y = combined_stube_conv100_conv60)) + 
          geom_point(alpha = 0.5, size = 0.5) + 
          # labs(x = 'Year',
          #      y = 'Clarity (cm)',
          # title = 'Minnesota stream Secchi tube measures',
          # subtitle = loc_major_basin) +
          ylim(0, 130) +
          ylab('Clarity (cm)') +
          xlim(1995, 2020) +
          xlab('Year') +
          # geom_jitter() + 
          geom_smooth(se = TRUE, color = 'blue') 
        # geom_abline(intercept = watershed_intercept, slope = watershed_slope, color = 'orange')
        # geom_abline(intercept = tobit_model$coefficients, slope = tobit_model$coefficients[2], color = 'green') 
        # ggtitle(label = loc_major_basin)
        # ggtitle('Minnesota stream Secchi tube measures')
    )
  ) %>% 
  rename(plot_allstages =`map(...)`)
by_stream_figs

# linear tobit figures for all all stream stages combined
by_stream_figs <- by_stream_figs %>% 
  mutate(tobit_line = pmap(list(stream_data, stream_intercept, stream_slope), 
                           function(a, b, c) ggplot(a, aes(x = year, y = combined_stube_conv100_conv60)) +
                             geom_point(alpha = 0.5, size = 0.5) + 
                             geom_abline(intercept = b, slope = c, color = 'blue')
                           )
         )
by_stream_figs

# add figures for facet grid (by stream stage)
by_stream_figs <- by_stream_figs %>% 
  mutate(
    map(stream_data, ~ ggplot(., aes(x = year, y = combined_stube_conv100_conv60)) + 
          geom_point(alpha = 0.5, size = 0.5) + 
          facet_grid(stage ~ ., scales = 'free') + 
          # labs(x = 'Year',
          #      y = 'Clarity (cm)',
          # title = 'Minnesota stream Secchi tube measures',
          # subtitle = loc_major_basin) +
          ylim(0, 130) +
          ylab('Clarity (cm)') +
          xlim(1995, 2020) +
          xlab('Year') +
          # geom_jitter() + 
          geom_smooth(se = TRUE, color = 'blue')  +
          theme(legend.position = 'none')
        # geom_abline(intercept = watershed_intercept, slope = watershed_slope, color = 'orange')
        # geom_abline(intercept = tobit_model$coefficients, slope = tobit_model$coefficients[2], color = 'green') 
        # ggtitle(label = loc_major_basin)
        # ggtitle('Minnesota stream Secchi tube measures')
    )
  ) %>% 
  rename(plot_bystages =`map(...)`)
by_stream_figs

by_stream_figs <- by_stream_figs %>% 
  mutate(
    plot_allstages_with_huc8 = map2(stream_data, huc8_data,
                                    ~ ggplot(data = .x, aes(x = year, y = combined_stube_conv100_conv60)) + 
                                      geom_smooth(se = TRUE, color = 'blue') +
                                      geom_smooth(data = .y, se = TRUE, color = 'red', method = 'loess') + 
                                      ylim(0, 130) +
                                      ylab('Clarity (cm)') +
                                      xlim(1995, 2020) +
                                      xlab('Year')
                                    )
  )

by_stream_figs

by_stream_figs$plot_allstages[1]
by_stream_figs$tobit_line[1]
by_stream_figs$plot_allstages_with_huc8[1]
by_stream_figs$plot_bystages[1]

####################################################################
########       Plot the raW data - by stream                ########
####################################################################
map2(paste0(by_stream_figs$sys_loc_code, ".png"), by_stream_figs$plot_allstages, 
     path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/new_figures/', 
     width=8, height=4.5, dpi=100,
     ggsave)

# map2(paste0(by_stream_figs$sys_loc_code, ".png"), by_stream_figs$plot_allstages, 
#      path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/figures/combined_stages/', 
#      ggsave)

####################################################################
########  Plot the raW data - by stream and stage facets    ########
####################################################################
map2(paste0(by_stream_figs$sys_loc_code, "-stage", ".png"), by_stream_figs$plot_bystages, 
     path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/new_figures/', 
     width=8, height=4.5, dpi=100,
     ggsave)

# map2(paste0(by_stream_figs$sys_loc_code, ".png"), by_stream_figs$plot_bystages, 
#      path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/figures/by_stage_facets/', 
#      ggsave)

####################################################################
########     Plot the raW data - by stream with huc8s       ########
####################################################################

map2(paste0(by_stream_figs$sys_loc_code, "-huc", ".png"), by_stream_figs$plot_allstages_with_huc8, 
     path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/new_figures/', 
     width=8, height=4.5, dpi=100,
     ggsave)

# map2(paste0(by_stream_figs$sys_loc_code, ".png"), by_stream_figs$plot_allstages_with_huc8, 
#      path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/figures/combined_stages_with_huc8s/', 
#      ggsave)

##### join stream and huc8 tables and drop nested lists columns #####
table <- left_join(by_stream_model, by_watershed_model, by = "loc_major_basin") %>% 
  select(everything(), -stream_data, -huc8_data, -watershed_data, -stream_censmodel, -censmodel)

###################################################################################
#####     calculate first and last year and clarity medians for streams      ######
###################################################################################
stream_year <- clarity_converted %>%
  group_by(sys_loc_code, loc_major_basin, year) %>%
  summarise(
    count = n(), 
    median = median(combined_stube_conv100_conv60)
  ) %>%
  mutate(stream_first_year = min(year)) %>%
  mutate(stream_last_year = max(year)) %>%
  filter(year == max(year))

##### rename columns with 'last_year'
stream_year <- rename(stream_year, stream_last_year_measures = count,
                            stream_last_year_median = median)

##### calculate first and last year and clarity medians for watersheds (huc8)
watershed_year <- clarity_converted %>%
  group_by(loc_major_basin, year) %>%
  summarise(
    count = n(),
    median = median(combined_stube_conv100_conv60)
  ) %>%
  mutate(watershed_first_year = min(year)) %>%
  mutate(watershed_last_year = max(year)) %>%
  filter(year == max(year))

##### rename columns 'last year' and drop 'y'
watershed_year <- rename(watershed_year, watershed_last_year_measures = count, watershed_last_year_median = median) %>% 
  select(-year)

##### join steam and watershed year tables
year_table <- left_join(stream_year, watershed_year, by = "loc_major_basin") %>% 
  select(-year)

##### group tb by sys_loc_code, keep obs and record_length
by_stream_tb <- tb %>% 
  group_by(sys_loc_code, record_length, obs) %>% 
  summarise(
    count = n()
  ) %>% 
  select(-count)

##### join year_table to model data and to final_table, add stream watershed deltas and drop duplicate 
##### columns and nested lists
stream_table <- left_join(year_table, by_stream_tb, by = "sys_loc_code")
stream_table <- stream_table %>% 
  mutate(delta_ly_median = stream_last_year_median - watershed_last_year_median) 
# remove lists from by_stream_model
stream_model <- by_stream_model %>% 
  select(-stream_data, -huc8_data, -stream_censmodel, -loc_major_basin)
# remove lists from by_watershed_model
watershed_model <- by_watershed_model %>% 
  select(loc_major_basin, watershed_pvalue, watershed_intercept, watershed_slope)

stream_table <- left_join(stream_table, stream_model)
table <- left_join(stream_table, watershed_model)

x <- table

###################################################################################
#####            add text strings to explain the stream results              ######
###################################################################################

# add text strings to explain the stream results
table <- table %>% 
  mutate(text1 = 'For years ') %>% 
  mutate(start_date = stream_first_year) %>% 
  mutate(text2 = ' to ') %>% 
  mutate(end_date = stream_last_year) %>% 
  mutate(
    text3 = case_when(
      stream_pvalue <= 0.05 & stream_slope > 0.2 ~ ' there is evidence of improving water clarity at this stream station, of approximately ',
      stream_pvalue <= 0.05 & stream_slope <= 0.2 & stream_slope >= -0.2 ~ ' there is evidence of no detectable change in water clarity at this stream station.',
      stream_pvalue <= 0.05 & stream_slope < -0.2  ~ ' there is evidence of degrading water clarity at this stream station, of approximately ',
      stream_pvalue > 0.05 ~ ' there is no significant water clarity trend at this stream station. ',
      TRUE ~ ' the analysis did not run due to insufficient data. ')
  ) %>%
  mutate(
    slope = case_when(
      stream_pvalue <= 0.05 & stream_slope > 0.2 ~ sprintf('%0.2f', abs(stream_slope) * 10),
      stream_pvalue <= 0.05 & stream_slope < -0.2 ~ sprintf('%0.2f', abs(stream_slope) * 10),
      TRUE ~ ''
    )
  ) %>% 
  mutate(
    text4 = case_when(
      stream_pvalue <= 0.05 & stream_slope > 0.2 ~ ' cm per decade.', 
      stream_pvalue <= 0.05 & stream_slope < -0.2  ~ ' cm per decade.',
      TRUE ~ ''
    )
  ) %>%
  mutate(
    text5 = ' For the most recent year of the analysis, median water clarity was ') %>%
  mutate(
    stream2huc_diff = case_when(
      delta_ly_median > 0 | delta_ly_median < 0 ~ sprintf('%0.2f', abs(delta_ly_median)),
      delta_ly_median == 0 ~ ''
    )
  ) %>%
  mutate(
    text6 = case_when(
      delta_ly_median > 0 ~ ' cm higher than the ',  
      delta_ly_median < 0 ~ ' cm lower than the ', 
      delta_ly_median == 0 ~ ' the same as the '
      )
    ) %>% 
  mutate(text7 = 'watershed median. ')

table <- table %>% 
  mutate(stream_assessment = text3) %>% # added stream_assessment to keep text3 in it's own column
  unite(stream_text, text1, start_date, text2, end_date, text3, slope, text4, text5, stream2huc_diff, text6, text6, text7,
        sep = '', remove = TRUE) %>% 
  select(-stream_text, stream_text) # move text column to last position

##### add text strings to explain the watershed results
final_table <- table %>% 
  mutate(text1 = 'There is') %>% 
  mutate(
    text3 = case_when(
      watershed_pvalue <= 0.05 &  watershed_slope > 0.2 ~ ' evidence of improving water clarity on measured streams in this watershed. ',
      watershed_pvalue <= 0.05 &  watershed_slope <= 0.2 & stream_slope >= -0.2 ~ ' evidence of no change in water clarity on measured streams in this watershed. ',
      watershed_pvalue <= 0.05 &  watershed_slope < -0.2  ~ ' evidence of degrading water clarity on measured streams in this watershed. ',
      watershed_pvalue > 0.05 ~ ' no evidence of a detectable trend in water clarity on measured streams in this watershed. ',
      TRUE ~ ' the analysis did not run due to insufficient data. '
    )
  ) %>%
  mutate(text4 = 'For the last year, median water clarity was ') %>%
  mutate(huc_ly_mean = sprintf('%0.2f', abs(watershed_last_year_median))) %>%
  mutate(text6 = ' cm across all measured streams in this watershed. ') 

##### combine columns for single sentence format
final_table <- final_table %>% 
  unite(huc_text, text1, text3, text4, huc_ly_mean, text6, sep = '', remove = TRUE) %>% 
  select(-huc_text, huc_text) #move text column to last position

##### write model output by streams to csv file
write_csv(final_table, path = 'X:/Agency_Files/Water/Condition Monitoring/Rivers & Streams/CSMP/Trend Analysis/2018/final_table.csv')
# write_csv(final_table, path = 'X:/Agency_Files/Data_Services/DAU/Staff Folders/Kirk Wythers/Projects/CMP/stream_results/final_table.csv')






