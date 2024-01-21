library(here)
library(lubridate)
library(data.table)
library(dplyr)
library(eplusr)
library(ggplot2)
library(broom)
library(readr)

library(ggthemes)
library(psychrolib)

library(rlist)
library(pipeR)

library(frost)

options(datatable.print.class = TRUE)

for (f in fs::dir_ls("R", glob = "*.R")) source(f)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# AIR TIGHTNESS EVALUATION

# Read SF6 data and decay test schedule
sf6_path <- "data-raw/experiments/1-air_tightness_and_ventilation/sf6_all.csv"
sf6_dt <- fread(here::here(sf6_path))

sche_path <- "data-raw/experiments/sche_May10_July15.csv"
sche_dt <- fread(here::here(sche_path))
sche_dt$start <- lubridate::ymd_hm(sche_dt$start)
sche_dt$end <- lubridate::ymd_hm(sche_dt$end)

# Tag the time slots
case_lst <- sche_dt$slot[1:9]

for (i in seq(length(case_lst))){
    sf6_dt[datetime >= sche_dt$start[i] & datetime <= sche_dt$end[i], tag := case_lst[i]]
}

# Data filtering
sf6_dt2 <- sf6_dt[is.na(tag) == FALSE, ]

# Split the data by case and location
sf6_lsts <- split(sf6_dt2, by = c("tag", "location"))

# Calculate infiltration rate & ACH
ach_cal_result_lsts <- lapply(sf6_lsts, cal_ach)

# cal_ach <- function(dt) {
#     dt[, time := as.numeric(datetime - dt[1, datetime])/3600]
#     dt[, concentration_decay := dt[1, SF6] / SF6]
#     m <- lm(log(concentration_decay) ~ time, dt)
#     broom::tidy(m)
# }


# Draw the decay plot (log(C(t1)/C(t)) ~ t-t1 plot)
for (i in seq(length(names(sf6_lsts)))) {
    sf6_item <- as.data.table(sf6_lsts[[i]])
    p_sf6_item <- ggplot(
        sf6_item, aes(time, log(concentration_decay))) +
        geom_line() +
        theme(legend.position = "none") +
        labs(x = "t-t1", y = "log(C(t1)/C(t))")
    p_save_dir <- stringr::str_c("analysis/air_tightness/plots/", names(sf6_lsts)[i], ".jpg", sep = "")
    ggsave(filename = here::here(p_save_dir),
           plot = p_sf6_item, dpi = 150, height = 4, width = 4)
}

# Save linear regression results
ach_result_path <- "analysis/air_tightness/result_lsts.csv"
save_ach_results(ach_cal_result_lsts, ach_result_path)

# save_ach_results <- function(ach_cal_result_lsts, ach_result_path) {
#     ach_result1_dt <- as.data.frame(ach_cal_result_lsts[1])
#     names(ach_result1_dt) <- c("term", "estimate", "error", "statistic", "p_value")
#     ach_result1_dt$category <- names(ach_cal_result_lsts)[1]
#
#     fwrite(ach_result1_dt, file = here::here("analysis/air_tightness/result_lsts.csv"))
#
#     for (i in (2:length(ach_cal_result_lsts))) {
#         # write_csv(as.data.frame(lsts[1]), here::here("analysis/air_tightness/lsts.csv"), col_names = TRUE, append = TRUE)
#         ach_result_dt <- as.data.frame(ach_cal_result_lsts[i])
#         names(ach_result_dt) <- c("term", "estimate", "error", "statistic", "p_value")
#         ach_result_dt$category <- names(ach_cal_result_lsts)[i]
#         fwrite(ach_result_dt, file = here::here(ach_result_path), append = TRUE)
#     }
# }

# ------------------------------------------------------------------------------
# Calculate and save average ACH results

ach_result_dt <- fread(here::here(ach_result_path))
ach_category <- ach_result_dt$category

get_ACHcase <- function(char) {
    ACHcase <- unlist(strsplit(char,split = "[.]"))[1]
    return(ACHcase)
}

get_ACHlocation <- function(char) {
    ACHlocation <- unlist(strsplit(char,split = "[.]"))[2]
    return(ACHlocation)
}

ach_result_dt$case <- data.table::as.data.table(apply(ach_result_dt[, c("category")], 1, get_ACHcase))
ach_result_dt$location <- data.table::as.data.table(apply(ach_result_dt[, c("category")], 1, get_ACHlocation))

ach_result_coeffi_dt <- ach_result_dt[term == "time", ]

# Issue: here the three locations are used to calculate the average without identifying the outlier
ach_result_ave_coeffi <- ach_result_coeffi_dt[, lapply(.SD, mean, na.rm = TRUE), by = case, .SDcols = c("estimate")]
names(ach_result_ave_coeffi) <- c("case", "estimated coefficients")

fwrite(ach_result_ave_coeffi, file = here::here("analysis/air_tightness/ach_coefficients.csv"))
# ------------------------------------------------------------------------------
# Tips
# unlist(strsplit("a.b.c", ".", fixed = TRUE))
# unlist(strsplit("a.b.c", "[.]"))
# ach_cal_result[1] %>% tidy %>% write.csv(file = here::here("analysis/air_tightness/ach_cal_result.csv"))
# sche_dt <- data.table::fread("doc/plan/experiment_schedule_shared.csv", na.strings = c("", "NA"))
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# THERMAL PROPERTIES EVALUATION
pi_path <- "data-raw/experiments/pi_data_May10_July15.csv"
sche_path <- "data-raw/experiments/sche_May10_July15.csv"
location_path <- "data-raw/experiments/3-thermal_performance/location.csv"

# map_dt <- fread(here::here(location_path))

thermal_dt <- extract_thermal_data(pi_path)
thermal_dt <- tag_time_slots(sche_path, thermal_dt)
thermal_tagged_dt <- thermal_dt[is.na(case) == FALSE, ]
thermal_split_dt <- tag_locations_cal(location_path, thermal_tagged_dt)

# plot_conductance(thermal_split_dt)
# plot_surf_temp_e(thermal_split_dt)
# plot_surf_temp_i(thermal_split_dt)
# plot_q(thermal_split_dt)

# reunite the sub table into a long table for plotting
thermal_reunite_dt <- list.rbind(thermal_split_dt)
fwrite(thermal_reunite_dt, here::here("analysis/summary_table/summary_thermal_table.csv"))

# draw thermal conductance plot
save_dir <- "analysis/thermal_performance/plot_conductance_united/conductance_new.jpg"
plot_conductance2(reunited_dt = thermal_reunite_dt, save_dir = save_dir)

# Output average thermal conductance calculation values
# Use aggregate FUNCTION to calculate the average conductance by "case" & "location"
# Use with FUNCTION to directly call variables by the name of each variable

save_dir <- "analysis/thermal_performance/plot_conductance_united/conductance_ave_new.jpg"
cond_ave_df <- cal_ave_conductance(thermal_reunite_dt, save_dir)
fwrite(cond_ave_df, here::here("analysis/summary_table/summary_thermal_ave_table.csv"))
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# EVALUATE THE EFFECTS OF SOLAR RADIATION ON THE COOLING LOAD
# Evaluate the cooling load
cooling_load_dt <- extract_cooling_load(pi_path, sche_path)
load_aggr_dt <- aggregate_load_data(cooling_load_dt)
plot_load_line(cooling_load_dt)
# ------------------------------------------------------------------------------
combined_dt <- merge(thermal_reunite_dt[
    location %in% c("wall_faceCorridor"), c("datetime", "RTDe", "RTDi")],
    cooling_load_dt, by = c("datetime"))

# Significance test
test_sig(combined_dt)
# Correlation test
cor(combined_dt$RTDe, combined_dt$air_side_cooling_load_total, method = "spearman")
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# CREATE WEATHER FILE

# Extract weather data from PI (to develop measured_weather_dt)
pi_path <- "data-raw/experiments/pi_data_May10_July15.csv"
pi_dt <- fread(here::here(pi_path))

exp_weather_dt <- select(pi_dt, "datetime", contains("Weather"))
exp_weather_dt <- select(exp_weather_dt, "datetime", (contains("O3A") | contains("O3B")))

exp_weather_dt$datetime <- ymd_hms(exp_weather_dt$datetime)

exp_weather_dt2 <- apply(exp_weather_dt[, 2:5], 2, as.numeric)
exp_weather_dt2 <- as.data.table(exp_weather_dt2)

names(exp_weather_dt2) <- gsub(".*\\|", "", names(exp_weather_dt2))

exp_weather_dt <- cbind(exp_weather_dt[, "datetime"], exp_weather_dt2)

# measured_weather_dt <- exp_weather_dt[is.na(O3A_TEMP) == FALSE & O3A_TEMP < 40 & O3A_RH > 50, ]
measured_weather_dt <- exp_weather_dt[datetime >= ymd_hms("2021-06-18 15:55:00"), ]

fwrite(measured_weather_dt, file = here::here("analysis/summary_table/exp_weather.csv"))
# check measured experiment weather file
p_epw_RH <- ggplot(exp_weather_dt[datetime >= ymd_hms("2021-06-18 15:55:00"), ], aes(datetime, O3A_RH)) +
    geom_line() +
    geom_point() +
    scale_x_datetime(breaks=scales::date_breaks("12 hours"),
                     labels = function(x) stringr::str_wrap(x, width = 4))
# ------------------------------------------------------------------------------
# Read Template weather file
path_epw <- here::here("data-raw/epw/SGP_Singapore.486980_IWEC.epw")
epw <- read_epw(path_epw)
epw_dt <- epw$data(start_year = 2021)

# select(epw_dt, "datetime", (contains("temperature") | contains("humidity") | contains("enthalpy")))
# ------------------------------------------------------------------------------
exp_wea <- data.table::fread(here::here("analysis/summary_table/exp_weather.csv"))
# ------------------------------------------------------------------------------
# exp_wea_ref <- data.table::fread(here::here("data-raw/epw/exp_weather.csv"))
# exp_wea2 <- merge(exp_wea_ref, exp_wea, by = "datetime", all = TRUE)
# ------------------------------------------------------------------------------
# head(exp_wea)

exp_wea$O3A_DPT <- calcDewPoint(exp_wea$O3A_RH, exp_wea$O3A_TEMP, mode="B")
exp_wea$O3B_DPT <- calcDewPoint(exp_wea$O3B_RH, exp_wea$O3B_TEMP, mode="B")

exp_wea$TEMP <- apply(exp_wea[, c("O3A_TEMP", "O3B_TEMP")], 1, mean)
exp_wea$RH <- apply(exp_wea[, c("O3A_TEMP", "O3B_TEMP")], 1, mean)
exp_wea$DPT <- apply(exp_wea[, c("O3A_TEMP", "O3B_TEMP")], 1, mean)

exp_wea$date <- lubridate::date(exp_wea$datetime)
exp_wea$hour <- lubridate::hour(exp_wea$datetime)

cols_val <- c("TEMP", "RH", "DPT")
exp_hourly_wea <- exp_wea[, lapply(.SD, mean), .SDcols = cols_val, c("date", "hour")]

exp_hourly_wea$datetime <- lubridate::ymd_h(
    stringr::str_c(exp_hourly_wea$date, exp_hourly_wea$hour, sep = " "))

exp_hourly_wea <- dplyr::select(exp_hourly_wea, "datetime", everything())

# length = 642
wea_start <- exp_hourly_wea$datetime[1]
wea_end <- exp_hourly_wea$datetime[length(exp_hourly_wea$datetime)]

# "dry_bulb_temperature", "dew_point_temperature", "relative_humidity"
# error: Supplied 610 items to be assigned to 642 items of column 'dry_bulb_temperature'.
# If you wish to 'recycle' the RHS please use rep() to make this intent clear to readers of your code

# epw_merged_dt <- merge(epw_dt, exp_hourly_wea, by = "datetime", all = TRUE)

epw_dt[datetime >= wea_start & datetime <= wea_end, dry_bulb_temperature := exp_hourly_wea[
    datetime >= wea_start & datetime <= wea_end, ]$TEMP]
epw_dt[datetime >= wea_start & datetime <= wea_end, dew_point_temperature := exp_hourly_wea[
    datetime >= wea_start & datetime <= wea_end, ]$DPT]
epw_dt[datetime >= wea_start & datetime <= wea_end, relative_humidity := exp_hourly_wea[
    datetime >= wea_start & datetime <= wea_end, ]$RH]

# ------------------------------------------------------------------------------
# Use the following codes to correct the error (by creating "merge_dt2") (Useless)
# epw_dt2 <- epw_dt[datetime >= wea_start & datetime <= wea_end, "datetime"]
#
# merge_dt <- merge(epw_dt2, exp_hourly_wea, by = "datetime", all = TRUE)
# nonmerge_dt <- merge_dt[is.na(TEMP) == TRUE, ]
#
# epw_src <- epw_dt[, c("datetime", "dry_bulb_temperature", "dew_point_temperature", "relative_humidity")]
# nonmerge_ref_dt <- merge(nonmerge_dt, epw_src, by = "datetime")
#
# nonmerge_dt$date <- lubridate::date(nonmerge_dt$datetime)
# nonmerge_dt$hour <- lubridate::hour(nonmerge_dt$datetime)
#
# nonmerge_dt$TEMP <- nonmerge_ref_dt$dry_bulb_temperature
# nonmerge_dt$RH <- nonmerge_ref_dt$relative_humidity
# nonmerge_dt$DPT <- nonmerge_ref_dt$dew_point_temperature
#
# exp_hourly_corrected_wea <- rbind(exp_hourly_wea, nonmerge_dt, use.names=TRUE)
#
# merge_dt2 <- merge(epw_dt2, exp_hourly_corrected_wea, by = "datetime")
#
# epw_dt[datetime >= wea_start & datetime <= wea_end, dry_bulb_temperature := merge_dt2$TEMP]
# epw_dt[datetime >= wea_start & datetime <= wea_end, dew_point_temperature := merge_dt2$DPT]
# epw_dt[datetime >= wea_start & datetime <= wea_end, relative_humidity := merge_dt2$RH]
# ------------------------------------------------------------------------------
# Modify solar radiation
# "direct_normal_radiation", "diffuse_horizontal_radiation", "global_horizontal_illumination"
# "extraterrestrial_horizontal_radiation", "extraterrestrial_direct_normal_radiation", "horizontal_infrared_radiation_intensity_from_sky"
# ------------------------------------------------------------------------------
# Explore the relation between globa and direct and diffuse solar radiation (Useless)

# epw_dt_sample <- epw_dt[, c(
#     "direct_normal_radiation", "diffuse_horizontal_radiation", "global_horizontal_radiation")]
#
# epw_dt_sample[, sum := direct_normal_radiation*0.8 + diffuse_horizontal_radiation]
# radiation_model <- lm(epw_dt_sample$global_horizontal_radiation ~
#                           epw_dt_sample$direct_normal_radiation +
#                           epw_dt_sample$diffuse_horizontal_radiation)
#
# summary(radiation_model)
#
# p_solar <- ggplot(epw_dt_sample, aes(global_horizontal_radiation, sum)) +
#     geom_line()
# ------------------------------------------------------------------------------
# Update solar radiation weather data
epw_dt[datetime >= wea_start & datetime <= wea_end, direct_normal_radiation := 0]
epw_dt[datetime >= wea_start & datetime <= wea_end,
       global_horizontal_radiation := diffuse_horizontal_radiation]

# check the modified weather data is correct
p_epw_DBT <- ggplot(epw_dt[datetime >= wea_start &
                               datetime <= wea_end, ], aes(datetime, dry_bulb_temperature)) +
    geom_line() +
    geom_point() +
    scale_x_datetime(breaks=scales::date_breaks("12 hours"),
                     labels = function(x) stringr::str_wrap(x, width = 4))

# ------------------------------------------------------------------------------
# Update wind speed
path_epw <- here::here("data-raw", "epw", "SGP_Singapore_experiment_whole.epw")
epw <- read_epw(path_epw)
epw_dt <- epw$data()

# sche_path <- here::here("data-raw", "experiments", "sche_May10_July15.csv")
# sche_dt <- fread(sche_path)
# wea_start <- sche_dt$start[1]

epw_dt2 <- epw_dt[, wind_speed := 0]
epw2 <- epw$set(epw_dt2, realyear = TRUE)

epw2$num_period()
epw2$abnormal_data(cols = c("dry_bulb_temperature", "dew_point_temperature",
                            "relative_humidity", "direct_normal_radiation",
                            "diffuse_horizontal_radiation", "global_horizontal_radiation", "wind_speed"),
                   type = "out_of_range")

epw2$redundant_data()
epw2$save(path = here::here("data-raw/epw/SGP_Singapore_experiment_whole_wind0.epw"))
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------








