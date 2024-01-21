# tidy_up\update0908\check_INDC_tuned_IDF

# ***************************************
# 1. Validate the HVAC system dynamics
# ***************************************

# Run simulations
# path_epw <- here("data", "epw", "SGP_Singapore_sbb3.epw")
# epw <- read_epw(path_epw)

path_idfs <- fs::dir_ls(here("INDC2"), 
                        recurse = TRUE, regexp = "L[123456]_INDC\\.idf")

path_idfs <- fs::dir_ls(here("INDC2"), 
                        recurse = TRUE, regexp = ".*\\.idf")


path_idfs <- fs::dir_ls(here("INDC3"), 
                        recurse = TRUE, regexp = "L[123456]_INDC\\.idf")

path_epws <- fs::dir_ls(here("data", "idf", "tidy_up", "epw_sbb"), recurse = TRUE, regexp = "_[1-6].*\\.epw")
# path_epw <- here("data", "epw", "SGP_Singapore_sbb3.epw")

group_INDC_AC <- group_job(path_idfs, path_epws)
group_job(lapply(path_idfs, read_idf), lapply(path_epws, read_epw))
group_INDC_AC$run(tempdir(), echo = FALSE)

# run corresponding code in temp.Rmd

ep_dt_INDC_AC <- group_INDC_AC$report_data(
  which = NULL,
  key_value = c("ROOM3", 
                "NODEOUTLET_AIR_VAV3", 
                "NODE_RETURNAIR_ROOM3",
                
                "NODEOUTLET_OUTAIR2",
                "NODEOUTLET_AIR_CLGCOIL_AHU2",
                "NODEOUTLET_AIR_SUPPLY2",
                "NODEOUTLET_AIR_SUPPLY22",
                
                "NODEINLET_WATER_CLGCOIL_AHU2",
                "NODEOUTLET_WATER_CLGCOIL_AHU2",
                "NODELST_OUTAIR2"
  ),
  name = NULL,
  year = NULL,
  tz = "UTC",
  all = FALSE,
  wide = TRUE,
  period = NULL,
  month = NULL,
  day = NULL,
  hour = NULL,
  minute = NULL,
  interval = 60,
  simulation_days = NULL,
  day_type = NULL,
  environment_name = NULL
)

ep_dt_INDC_AC2 <- group_INDC_AC$report_data(
  which = NULL,
  key_value = NULL,
  name = c("Site Outdoor Air Drybulb Temperature",
           "Site Outdoor Air Humidity Ratio",
           "Site Outdoor Air Relative Humidity",
           "Site Outdoor Air Dewpoint Temperature",
           
           "Air System Outdoor Air Flow Fraction",
           "Air System Outdoor Air Mass Flow Rate",
           
           "Fan Electricity Rate",
           "Fan Heat Gain to Air",
           "Fan Air Mass Flow Rate",
           
           "Zone Air Terminal VAV Damper Position",
           
           "Cooling Coil Total Cooling Energy",
           "Cooling Coil Source Side Heat Transfer Energy",
           "Cooling Coil Total Cooling Rate",
           "Cooling Coil Sensible Cooling Rate",
           
           "Plant Supply Side Inlet Mass Flow Rate",
           "Plant Supply Side Inlet Temperature",
           "Plant Supply Side Outlet Temperature",
           "Schedule Value",
           "AFN Zone Infiltration Air Change Rate"
  ),
  year = NULL,
  tz = "UTC",
  all = FALSE,
  wide = TRUE,
  period = NULL,
  month = NULL,
  day = NULL,
  hour = NULL,
  minute = NULL,
  interval = 60,
  simulation_days = NULL,
  day_type = NULL,
  environment_name = NULL
)


ep_dt_AC <- merge(ep_dt_INDC_AC, ep_dt_INDC_AC2, by = c("index", "case", "Date/Time"))
# ep_dt2 <- ep_dt %>% group_by(index) %>% mutate(id = 1:n())
ep_dt_AC$case <- gsub(".*\\/", "", ep_dt_AC$case)
names(ep_dt_AC)[which(names(ep_dt_AC) == "Date/Time")] <- "datetime"
ep_dt_AC$datetime <- ymd_hms(str_c("2021", ep_dt_AC$datetime, sep = "/"))
ep_dt_AC <- select(ep_dt_AC, "datetime", "index", "case", everything())

# Calculate CHW Temperature Difference by idf name
ep_dt_AC <- ep_dt_AC %>% group_by(case) %>% mutate(CHW_dT_sim = `NODEOUTLET_WATER_CLGCOIL_AHU2:System Node Temperature [C](Hourly)` - `NODEINLET_WATER_CLGCOIL_AHU2:System Node Temperature [C](Hourly)`)
ep_dt_AC$datetime <- ep_dt_AC$datetime - dhours(1)

ep_dt_AC$`FAN_AHU2_EX:Fan Electricity Rate [W](Hourly)`[is.na(ep_dt_AC$`FAN_AHU2_EX:Fan Electricity Rate [W](Hourly)`)] <- 0

# ------------------------------------------------------------------------------

pi_path <- here("data", "pi", "pi_data_May10_July15.csv")

begin_lst <- c("2021-07-04 00:00:00")
begin_lst <- c("2021-05-20 00:00:00")

begin_lst <- c("2021-05-21 00:00:00", "2021-05-17 00:00:00", "2021-05-18 00:00:00",
               "2021-05-24 00:00:00", "2021-07-04 00:00:00", "2021-05-19 00:00:00",
               "2021-05-20 00:00:00", "2021-05-26 00:00:00", "2021-05-28 00:00:00")

begin_lst <- c("2021-05-21 00:00:00")
begin_lst <- c("2021-06-28 10:00:00")
# ------------------------------------------------------------------------------
end_lst <- c("2021-07-05 00:00:00")
end_lst <- c("2021-05-21 00:00:00")

end_lst <- c("2021-05-22 00:00:00", "2021-05-18 00:00:00", "2021-05-19 00:00:00",
             "2021-05-25 00:00:00", "2021-07-05 00:00:00", "2021-05-20 00:00:00",
             "2021-05-21 00:00:00", "2021-05-27 00:00:00", "2021-05-29 00:00:00")

end_lst <- c("2021-05-22 00:00:00")
end_lst <- c("2021-06-28 19:00:00")
# ------------------------------------------------------------------------------
pi_lst_AC <- list()

for (i in seq(length(begin_lst))) {
  # Extract refined PI data
  begin <- begin_lst[i]
  end <- end_lst[i]
  # Get refined pi data
  # FUNCTION
  pi_hourly_dt <- get_pi_refined_dt2(pi_path, begin, end)
  pi_hourly_dt$id <- seq_len(nrow(pi_hourly_dt))
  pi_hourly_dt <- select(pi_hourly_dt, "id", "datetime", "date", "hour", everything())
  pi_hourly_dt[, CHW_dT := SS2_RTD1_A-SS2_RTD1_B]
  pi_hourly_dt[, measured_CHW_coolcapacity := SS2_AHU1_THERMALPOWER]
  
  pi_lst_AC <- append(pi_lst_AC, list(pi_hourly_dt))
}

pi_dt_AC <- rbindlist(pi_lst_AC, idcol="file")
names(pi_dt_AC)[which(names(pi_dt_AC) == "file")] <- "index"
# ------------------------------------------------------------------------------

# Combine the IDF simulation results with the measured data

mea_sim_dt_AC <- merge(ep_dt_AC, pi_dt_AC, by = c("datetime"), all = TRUE)

mea_sim_dt_AC <- as.data.table(mea_sim_dt_AC)

names(mea_sim_dt_AC)[which(names(mea_sim_dt_AC) == "index.x")] <- "index"
# names(mea_sim_dt_AC)[which(names(mea_sim_dt_AC) == "id.x")] <- "id"
# ******************************************************************************
mea_sim_dt_AC[, Tzone_setpoint := 24.0]

# determine the supply air flow rate generated from the fan 
# mea_sim_dt_AC$SS4_VAV3_IN_est <- mea_sim_dt_AC$SS4_VAV3_IN
# mea_sim_dt_AC[SS4_VAV3_IN > 100, "SS4_VAV3_IN_est"] <- 100.0
# mea_sim_dt_AC[, Fan_Flow := SS4_SAFMS3_FLOW / (SS4_VAV3_IN_est/100)]

# x <- c(0L, 10L, 20L, 30L, 40L, 50L)

mea_sim_dt_AC[, SS4_SFAN1_REF_est := closest(SS4_SFAN1_REF)]
mea_sim_dt_AC$SS4_SFAN1_REF_est <- as.numeric(mea_sim_dt_AC$SS4_SFAN1_REF_est)
mea_sim_dt_AC[, Fan_Air_Volume_Flow_Rate := `FAN_AHU2:Fan Air Mass Flow Rate [kg/s](Hourly)` / 1.204]

mea_sim_dt_AC <- select(mea_sim_dt_AC, 
                     "datetime", "date", "hour", "index", "case", everything())

# ------------------------------------------------------------------------------
# No need to run if FAN_AHU2_EX's fan power is not required
# Generate FAN_AHU2_EX's simulated fan power
ep_dt_INDC_AC3 <- group_INDC_AC$report_data(
  which = NULL,
  key_value = NULL,
  name = c("Fan Electricity Rate"
  ),
  year = NULL,
  tz = "UTC",
  all = FALSE,
  wide = TRUE,
  period = NULL,
  month = NULL,
  day = NULL,
  hour = NULL,
  minute = NULL,
  interval = 15,
  simulation_days = NULL,
  day_type = NULL,
  environment_name = NULL
)

ep_dt_INDC_AC3$case <- gsub(".*\\/", "", ep_dt_INDC_AC3$case)

ep_dt_INDC_AC3$`FAN_AHU2_EX:Fan Electricity Rate [W](TimeStep)`[is.na(ep_dt_INDC_AC3$`FAN_AHU2_EX:Fan Electricity Rate [W](TimeStep)`)] <- 0
names(ep_dt_INDC_AC3)[which(names(ep_dt_INDC_AC3) == "Date/Time")] <- "datetime"
ep_dt_INDC_AC3$datetime <- ymd_hms(str_c("2021", ep_dt_INDC_AC3$datetime, sep = "/"))

ep_dt_INDC_AC3$date <- as.Date(ep_dt_INDC_AC3$datetime)
ep_dt_INDC_AC3$hour <- hour(ep_dt_INDC_AC3$datetime)

cols_val <- c("FAN_AHU2:Fan Electricity Rate [W](TimeStep)", "FAN_AHU2_EX:Fan Electricity Rate [W](TimeStep)")
ep_dt_INDC_AC3_hourly <- ep_dt_INDC_AC3[, lapply(.SD, mean, na.rm=TRUE), .SDcols = cols_val, c("index","case", "date", "hour")]

ep_dt_INDC_AC3_hourly$datetime <- lubridate::ymd_h(
  stringr::str_c(ep_dt_INDC_AC3_hourly$date, ep_dt_INDC_AC3_hourly$hour, sep = " "))

ep_dt_INDC_AC3_hourly <- dplyr::select(ep_dt_INDC_AC3_hourly, "datetime","date","hour", everything())
ep_dt_INDC_AC3_hourly2 <- ep_dt_INDC_AC3_hourly[date == "2021-07-04", ]

# mea_sim_dt_AC$`FAN_AHU2:Fan Electricity Rate [W](Hourly) cal` <- ep_dt_INDC_AC3_hourly2$`FAN_AHU2:Fan Electricity Rate [W](TimeStep)`
# mea_sim_dt_AC$`FAN_AHU2_EX:Fan Electricity Rate [W](Hourly) cal` <- ep_dt_INDC_AC3_hourly2$`FAN_AHU2_EX:Fan Electricity Rate [W](TimeStep)`

mea_sim_dt_AC <- merge(mea_sim_dt_AC, ep_dt_INDC_AC3_hourly2[, c("datetime", 
                                                                 "case", 
                                                                 "FAN_AHU2:Fan Electricity Rate [W](TimeStep)", 
                                                                 "FAN_AHU2_EX:Fan Electricity Rate [W](TimeStep)")], 
                       by = c("datetime", "case"), all = TRUE)

names(mea_sim_dt_AC)[128] <- "FAN_AHU2:Fan Electricity Rate [W](Hourly) cal"
names(mea_sim_dt_AC)[129] <- "FAN_AHU2_EX:Fan Electricity Rate [W](Hourly) cal"

mea_sim_dt_AC_Fan <- mea_sim_dt_AC[, c("datetime", "case", 
                                       "FAN_AHU2:Fan Electricity Rate [W](Hourly)",
                                       "FAN_AHU2:Fan Electricity Rate [W](Hourly) cal",
                                       "FAN_AHU2_EX:Fan Electricity Rate [W](Hourly) cal")]
# mea_sim_dt_AC <- merge(ep_dt_AC, pi_dt_AC, by = c("datetime"), all = TRUE)



# NO NEED TO RUN THE ABOVE CODE next time cause we have requested output of hourly fan power of FAN_AHU2_EX:Fan 
# ------------------------------------------------------------------------------
# Verify the calibration performance based on measured data

cooling_load_dt <- select(
  mea_sim_dt_AC, "datetime", "date", "hour", "case", 
  "CLGCOIL_AHU2:Cooling Coil Total Cooling Rate [W](Hourly)", "SS2_AHU1_THERMALPOWER")

cooling_load_dt_melt <- melt(cooling_load_dt, id = c("datetime", "date", "hour", "case"))

# fwrite(cooling_load_dt_melt, here("data", "csv", "param", "L1_L5_CoolingLoad_sim.csv"))
# fwrite(cooling_load_dt, here("data", "csv", "param", "L1_L5_CoolingLoad_longtable.csv"))
fwrite(mea_sim_dt_AC, here("data", "csv", "param", "L1_L5_AC_longtable.csv"))
# ------------------------------------------------------------------------------
columns = c("case","rmse","cvrmse")
error_df_cooling = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(error_df_cooling) = columns

# error <- data.frame(case = as.character(), rmse = as.numeric(), cvrmse = as.numeric())

for (i in seq(length(path_idfs))) {
  rmse <- rmse_cal(cooling_load_dt[case == cases[i], ]$`CLGCOIL_AHU2:Cooling Coil Total Cooling Rate [W](Hourly)`, cooling_load_dt[case == cases[i],]$SS2_AHU1_THERMALPOWER)
  cvrmse <- rmse/mean(cooling_load_dt[case == cases[i], ]$SS2_AHU1_THERMALPOWER)
  a <- rbind(error_df_cooling, data.frame(case=cases[i], rmse=rmse, cvrmse=cvrmse))
}

print(error_df_cooling)
# ------------------------------------------------------------------------------
labels <- c(L1_INDC.idf = "level 1", 
            L2_INDC.idf = "level 2", 
            L3_INDC.idf = "level 3", 
            L4_INDC.idf = "level 4", 
            L5_INDC.idf = "level 5", 
            L6_INDC.idf = "level 6")

p_CHW_Clg_rate <- ggplot2::ggplot(cooling_load_dt_melt, aes(hour, value, color = variable)) +
  geom_point(size=2, alpha=0.5) +
  geom_line() +
  facet_wrap(~case, labeller = labeller(case = labels)) +
  # facet_wrap(~as.factor(date), scales = "free_x") +
  labs(x = "Hour", y = "CHW Cooling Rate (W)") +
  theme(legend.position = "top") +
  scale_color_brewer(
    palette = "Set2",
    labels=c("simulation", "measurement")
  ) + 
  labs(color="")

ggsave(filename = here("pic", "param_1015", "overleaf", "CHW_Clg_rate_new.jpg"),
       plot = p_CHW_Clg_rate, dpi = 150, height = 6, width = 9)

# ------------------------------------------------------------------------------
room3_T_dt <- select(
  mea_sim_dt, "datetime", "date", "hour", "case", 
  "ROOM3:Zone Mean Air Temperature [C](Hourly)", "SS5_ROOM3_TEMP")

room3_T_dt_melt <- melt(room3_T_dt, id = c("datetime", "date", "hour", "case"))

p_room3_T <- ggplot2::ggplot(room3_T_dt_melt, aes(hour, value, color = variable)) +
  geom_point(size=2, alpha=0.5) +
  geom_line() +
  facet_wrap(~case) +
  # facet_wrap(~as.factor(date), scales = "free_x") +
  labs(x = "Hour", y = "ROOM3 Mean Air Temperature (℃)") +
  theme(legend.position = "top") +
  scale_color_brewer(
    palette = "Set2",
    labels=c("simulation", "measurement")
  ) + 
  labs(color="")

ggsave(filename = here::here("pic/tidy_up/overleaf/T_room3.jpg"),
       plot = p_room3_T, dpi = 150, height = 6, width = 9)
# ------------------------------------------------------------------------------
# off_coil_VFR_dt <- select(
#   mea_sim_dt, "datetime", "date", "hour", "case", 
#   "ROOM3:Zone Mean Air Temperature [C](Hourly)", "SS5_ROOM3_TEMP")




# ------------------------------------------------------------------------------
# ****************************************************************
# 2. Validate the model performance under free-floating conditions
# ****************************************************************

path_idfs <- fs::dir_ls(here("FF"), 
                        recurse = TRUE, regexp = "L[123456]_INDC.*\\.idf")

# path_epws <- fs::dir_ls(here("data", "idf", "tidy_up", "epw_sbb"), recurse = TRUE, regexp = ".*\\.epw")
path_epws <- fs::dir_ls(here("data", "idf", "tidy_up", "withHVAC", "epw"), recurse = TRUE, regexp = "[1-6]\\.epw")

# path_epw <- here("data", "epw", "SGP_Singapore_sbb3.epw")

group <- group_job(path_idfs, path_epws)
group_job(lapply(path_idfs, read_idf), lapply(path_epws, read_epw))
group$run(tempdir(), echo = FALSE)

# Extract Data 
ep_dt <- group$report_data(
  which = NULL,
  key_value = NULL,
  name = c("Site Outdoor Air Drybulb Temperature",
           "Site Outdoor Air Humidity Ratio",
           "Site Outdoor Air Relative Humidity",
           "Site Outdoor Air Dewpoint Temperature",
           
           "Zone Mean Air Temperature"
  ),
  year = NULL,
  tz = "UTC",
  all = FALSE,
  wide = TRUE,
  period = NULL,
  month = NULL,
  day = NULL,
  hour = NULL,
  minute = NULL,
  interval = NULL,
  simulation_days = NULL,
  day_type = NULL,
  environment_name = NULL
)

ep_dt$case <- gsub(".*\\/", "", ep_dt$case)
names(ep_dt)[which(names(ep_dt) == "Date/Time")] <- "datetime"
ep_dt$datetime <- ymd_hms(str_c("2021", ep_dt$datetime, sep = "/"))
ep_dt2 <- ep_dt[
  datetime >= lubridate::as_datetime("2021-06-19 00:00:00") & datetime <= lubridate::as_datetime("2021-06-20 23:00:00"), ]

# Read PI data
pi_path <- here("data", "pi", "pi_data_May10_July15.csv")
pi_hourly_dt <- get_pi_refined_dt(pi_path)

# Combine the PI data with the eplusout data
combined_dt <- merge(ep_dt2, pi_hourly_dt[, c("datetime", "SS5_ROOM3_TEMP")], by = "datetime")

fwrite(combined_dt, here("pic", "param_1015", "FF_combined_dt.csv"))
fwrite(combined_dt, here("data", "csv", "param", "L1_L5_FF_longtable.csv"))
# ------------------------------------------------------------------------------

# cols <- names(combined_dt)
# cols_val <- c("ROOM3:Zone Mean Air Temperature [C](Hourly)", "SS5_ROOM3_TEMP")
sample_dt <- combined_dt[, c("datetime", "case","ROOM3:Zone Mean Air Temperature [C](Hourly)", "SS5_ROOM3_TEMP")]
# names(sample_dt)[3] <- "predicted"
# names(sample_dt)[4] <- "measured"
# d <- sample_dt[ , lapply(.SD, rmse_cal), .SDcols = c("predicted", "measured"), c("case")]

cases <- c("L1_INDC.idf", "L2_INDC.idf", "L3_INDC.idf", "L4_INDC.idf", "L5_INDC.idf", "L6_INDC.idf")
columns = c("case","rmse","cvrmse")
error_df = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(error_df) = columns

# error <- data.frame(case = as.character(), rmse = as.numeric(), cvrmse = as.numeric())

for (i in seq(length(path_idfs))) {
  rmse <- rmse_cal(sample_dt[case == cases[i], ]$`ROOM3:Zone Mean Air Temperature [C](Hourly)`, sample_dt[case == cases[i],]$SS5_ROOM3_TEMP)
  cvrmse <- rmse/mean(sample_dt[case == cases[i], ][[which(names(sample_dt) == "SS5_ROOM3_TEMP")]])
  error_df <- rbind(error_df, data.frame(case=cases[i], rmse=rmse, cvrmse=cvrmse))
}

print(error_df)

# rmse_specs <- rmse_cal(combined_dt[index == 1, ]$`ROOM3:Zone Mean Air Temperature [C](Hourly)`, combined_dt[index == 1,]$SS5_ROOM3_TEMP)
# cvrmse_specs <- rmse_specs/mean(combined_dt[index == 1, ][[which(names(combined_dt) == "SS5_ROOM3_TEMP")]])
# 
# rmse_FF <- rmse_cal(combined_dt[index == 2, ]$`ROOM3:Zone Mean Air Temperature [C](Hourly)`, combined_dt[index == 2,]$SS5_ROOM3_TEMP)
# cvrmse_FF <- rmse_FF/mean(combined_dt[index == 2, ][[which(names(combined_dt) == "SS5_ROOM3_TEMP")]])

combined_dt_melt <- melt(combined_dt[, c("datetime", "case","ROOM3:Zone Mean Air Temperature [C](Hourly)", "SS5_ROOM3_TEMP")], id.vars = c("datetime", "case"))

# combined_dt_melt$model <- "model_specs"
# combined_dt_melt[index == 1L]$model <- "model_specs"
# combined_dt_melt[index == 2L]$model <- "model_FF"

labels <- c(L1_INDC.idf = "level 1", 
            L2_INDC.idf = "level 2", 
            L3_INDC.idf = "level 3", 
            L4_INDC.idf = "level 4", 
            L5_INDC.idf = "level 5", 
            L6_INDC.idf = "level 6")

p_comp_FF_Tair <- ggplot(combined_dt_melt, aes(datetime, value, color = variable)) +
  geom_point() +
  geom_line() +
  scale_x_datetime(breaks=scales::date_breaks("12 hours"), labels = function(x) stringr::str_wrap(x, width = 6)) +
  labs(x = "Datetime", y = "Air Temperature (℃)", color = "") +
  theme(legend.position = "top") +
  facet_wrap(~case, labeller=labeller(case = labels)) +
  theme(legend.position = "top") +
  scale_color_brewer(
    palette = "Set2",
    labels=c("simulation", "measurement")
  )

ggsave(filename = here::here("pic", "param_1015", "overleaf", "FF_Tair_verify_new.jpg"),
       plot = p_comp_FF_Tair, dpi = 150, height = 6, width = 9)






# ------------------------------------------------------------------------------
# ****************************************************************
# 3. Validate the AHU Fan Power
# ****************************************************************

fan_power_dt <- select(
  mea_sim_dt_AC, "datetime", "date", "hour", "case", 
  "FAN_AHU2:Fan Electricity Rate [W](Hourly)", "FAN_AHU2_EX:Fan Electricity Rate [W](Hourly)", "SS4_SFAN1_POWER")
# fan_power_dt <- select(
#   mea_sim_dt_AC, "datetime", "date", "hour", "case", 
#   "FAN_AHU2:Fan Electricity Rate [W](Hourly)", "FAN_AHU2_EX:Fan Electricity Rate [W](Hourly) cal", "SS4_SFAN1_POWER")

# fan_power_dt <- as.data.table(fan_power_dt)

fan_power_dt[, `Fan Electricity Rate Simulated [W](Hourly)` := `FAN_AHU2:Fan Electricity Rate [W](Hourly)` + 
               `FAN_AHU2_EX:Fan Electricity Rate [W](Hourly)`]
# fan_power_dt[, `Fan Electricity Rate Simulated [W](Hourly)` := `FAN_AHU2:Fan Electricity Rate [W](Hourly)` + 
#                `FAN_AHU2_EX:Fan Electricity Rate [W](Hourly) cal`]

fan_power_dt_melt <- melt(fan_power_dt[, c("datetime", "date", "hour", "case", "SS4_SFAN1_POWER", "Fan Electricity Rate Simulated [W](Hourly)")], 
                          id = c("datetime", "date", "hour", "case"))

cases <- c("L1_INDC.idf", "L2_INDC.idf", "L3_INDC.idf", "L4_INDC.idf", "L5_INDC.idf", "L6_INDC.idf")
columns = c("case","rmse","cvrmse")
error_df_fan = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(error_df_fan) = columns

# error <- data.frame(case = as.character(), rmse = as.numeric(), cvrmse = as.numeric())

for (i in seq(length(path_idfs))) {
  rmse <- rmse_cal(fan_power_dt[case == cases[i], ]$`Fan Electricity Rate Simulated [W](Hourly)`, fan_power_dt[case == cases[i],]$SS4_SFAN1_POWER)
  cvrmse <- rmse/mean(fan_power_dt[case == cases[i], ][[which(names(fan_power_dt) == "SS4_SFAN1_POWER")]])
  a <- rbind(error_df_fan, data.frame(case=cases[i], rmse=rmse, cvrmse=cvrmse))
}

print(error_df_fan)

labels <- c(L1_INDC.idf = "level 1", 
            L2_INDC.idf = "level 2", 
            L3_INDC.idf = "level 3", 
            L4_INDC.idf = "level 4", 
            L5_INDC.idf = "level 5", 
            L6_INDC.idf = "level 6")

p_Fan_power <- ggplot2::ggplot(fan_power_dt_melt, aes(hour, value, color = variable)) +
  geom_point(size=2, alpha=0.5) +
  geom_line() +
  facet_wrap(~case, labeller = labeller(case = labels)) +
  # facet_wrap(~as.factor(date), scales = "free_x") +
  labs(x = "Hour", y = "Fan Power (W)") +
  theme(legend.position = "top") +
  scale_color_brewer(
    palette = "Set2",
    labels=c("measurement", "simulation")
  ) + 
  labs(color="")

ggsave(filename = here("pic", "param_1015", "overleaf", "fan_power_new.jpg"),
       plot = p_Fan_power, dpi = 150, height = 6, width = 9)

# Free floating recheck
# case         rmse     cvrmse
# 1  L1_INDC.idf    2.6307046 0.09729538
# 2  L2_INDC.idf    2.2665953 0.08382896
# 3  L3_INDC.idf    0.3326281 0.01230209
# 4  L4_INDC.idf    0.3151944 0.01165732
# 5  L5_INDC.idf    0.3060774 0.01132013
# 6  L6_INDC.idf    0.2871200 0.01061900


# Fan recheck
# case      rmse    cvrmse
# 1 L1_INDC.idf 434.25895 4.9629594
# 2 L2_INDC.idf  66.62227 0.7613974
# 3 L3_INDC.idf  59.62882 0.6814722
# 4 L4_INDC.idf  60.78876 0.6947286
# 5 L5_INDC.idf  15.17926 0.1734773
# 6 L6_INDC.idf  15.17926 0.1734773

# Cooling load recheck
# case      rmse    cvrmse
# 1 L1_INDC.idf 2789.4231 1.5785514
# 2 L2_INDC.idf 1904.8390 1.0779599
# 3 L3_INDC.idf 2369.3897 1.3408520
# 4 L4_INDC.idf 2314.7446 1.3099280
# 5 L5_INDC.idf 2417.8722 1.3682885
# 6 L6_INDC.idf  293.4204 0.1660484





