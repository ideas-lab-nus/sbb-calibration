library(here)
library(ggplot2)
library(data.table)
library(lubridate)
library(tidyr)
library(dplyr)
library(plyr)
library(hms)
library(scales)
library(splines2)
library(plotly)

options(datatable.print.class = TRUE)


# ##########################
# Measured infiltration rate

# read in measured data
sf6 <- fread("data-raw/experiments/1-air_tightness_and_ventilation/sf6_edited2.csv")

# draw regression plot
formula <- y ~ poly(x, 1, raw = TRUE)
p_sf6_item <- ggplot(
    sf6[case == "noAC_sealed_5s"], aes(datetime, log(`SF6 [ppm]`))) +
    geom_line() +
    facet_wrap(~location, scales = "free_x") +
    # theme(legend.position = "none") +
    labs(x = "", y = expression(`log`[`e`]*"(N)")) +
    scale_x_datetime(labels = date_format("%H:%M")) +
    geom_smooth(method=lm , color="red", se=FALSE, formula = formula) +
    theme(legend.position="none")

# uncertainty analysis: The standard error of regression coefficients (refer to IPMVP Appendix B-2.2.3)
# 1-----------------------------------------------------------------------------
# standard error of coefficient
## Create a linear regression with two variables
lmConcentration_center = lm(log(`SF6 [ppm]`)~datetime, data=sf6[case == "noAC_sealed_5s" & location == "center"])
lmConcentration_neardoor = lm(log(`SF6 [ppm]`)~datetime, data=sf6[case == "noAC_sealed_5s" & location == "near_door"])
lmConcentration_nearwindow = lm(log(`SF6 [ppm]`)~datetime, data=sf6[case == "noAC_sealed_5s" & location == "near_window"])

## Review the results
summary(lmConcentration_center)
# coefficient: -1.216e-05 intercept: 1.971e+04
summary(lmConcentration_neardoor)
# coefficient: -1.138e-05 intercept: 1.845e+04
summary(lmConcentration_nearwindow)
# coefficient: -1.178e-05 intercept: 1.909e+04

## extract the regression coefficient and calculate ACH
## convert air change rate per second into ACH
ACH_center <- lmConcentration_center$coefficients[[2]] * 3600
ACH_near_door <- lmConcentration_neardoor$coefficients[[2]] * 3600
ACH_near_window <- lmConcentration_nearwindow$coefficients[[2]] * 3600

## add regression coefficients and intervals to raw data table to ease later calculation
lr_dt <- data.table("location" = c("center","near_door","near_window"),
                   "coefficient" = c(-1.215743e-05, -1.138016e-05, -1.177584e-05),
                   "residual" = c(19707.77, 18448, 19089.3))

sf6_dt <- sf6[case == "noAC_sealed_5s"]
sf6_dt <- merge(sf6_dt, lr_dt, by="location")

## calculate precision of regression coefficient
## add prediction to raw datatable
sf6_dt <- sf6_dt %>% mutate(loge_sf6 = log(`SF6 [ppm]`)) %>%
    mutate(loge_sf6_cal = as.numeric(sf6_dt$datetime, units="secs")*coefficient+residual) %>%
    mutate(sf6_ppm_cal = exp(1)^loge_sf6_cal) %>%
    mutate(datetime_num=as.numeric(datetime, units="secs"))

fwrite(sf6_dt, here::here("data", "sf6_dt.csv"))
fwrite(sf6_dt, here::here("data", "uncertainty", "sf6_uncertainty_dt.csv"))

## calculate the standard error of the coefficient
datetime_mean_dt <- sf6_dt[ , lapply(.SD, mean), .SDcols = c("datetime_num"), c("location")]
names(datetime_mean_dt) <- c("location", "datetime_num_mean")
sf6_dt <- merge(sf6_dt, datetime_mean_dt, by="location")

sf6_dt <- sf6_dt %>% mutate(Y_Ycal=`SF6 [ppm]`-sf6_ppm_cal) %>%
    mutate(X_Xmean=datetime_num-datetime_num_mean) %>%
    mutate(Y_Ycal_sqr=Y_Ycal^2) %>%
    mutate(X_Xmean_sqr=X_Xmean^2)

cal_SE_b <- function(dt, n) {
SE_b <- (sum(dt$`Y_Ycal_sqr`)/(n-2)/sum(dt$`X_Xmean_sqr`))^0.5
return(SE_b)
}

SE_b_center <- cal_SE_b(sf6_dt[location=="center"], 25)
# 2.282069e-05
SE_b_neardoor <- cal_SE_b(sf6_dt[location=="near_door"], 25)
# 1.200368e-05
SE_b_near_window <- cal_SE_b(sf6_dt[location=="near_window"], 25)
# 1.989051e-05

## t is 1.71 for 25 data points (DF=23 for a regression model) and a 90% confidence level
# therefore the relative precision is
SE_b_percent_center <- 1.71*SE_b_center/ACH_center   # -0.0008916215 0.089%
SE_b_percent_neardoor <- 1.71*SE_b_neardoor/ACH_near_door  # -0.0005010251 0.050%
SE_b_percent_nearwindow <- 1.71*SE_b_near_window/ACH_near_window # -0.0008023203 0.080%

# 2-----------------------------------------------------------------------------
# standard error of the sample mean
# The mean value equals to
ACH_mean <- mean(ACH_center, ACH_near_door, ACH_near_window)
# ACH_mean = -0.04376675
# The standard deviation equals to
n <- 3
s <- (((ACH_center-ACH_mean)^2+(ACH_near_door-ACH_mean)^2+(ACH_near_window-ACH_mean)^2)/(n-1)
      )^0.5
# S=0.002204187
# the standard error euqals to
SE <- s/(n^0.5)
# SE=0.001272588

## t is 2.92 for 3 data points (DF=2) and a 90% confidence level
# therefore the relative precision is
SE_percent <- 2.92*SE/abs(ACH_mean) #SE_percent=0.08490366 8.49%

# 3-----------------------------------------------------------------------------
# standard error of the measured value
ppm_u_abs <- 1/log(10) * (0.001 / 33.3) # 3.720343e-06
ppm_u_rel <- percent(ppm_u_abs/log(33.3)) # 0%
# 4-----------------------------------------------------------------------------
# calculate combined uncertainty
u_ach <- percent((SE_b_percent_center^2+SE_b_percent_neardoor^2+SE_b_percent_nearwindow^2+SE_percent^2
                  +ppm_u_abs^2+ppm_u_abs^2+ppm_u_abs^2)^0.5)

u_ach2 <- (SE_b_center^2+SE_b_neardoor^2+SE_b_near_window^2+SE^2+ppm_u_abs^2+ppm_u_abs^2+ppm_u_abs^2)^0.5
# u_ach2=0.001273205
u_ach_relative <- percent(u_ach2/ACH_mean)
# u_ach_relative=3%


# #############################
# Measured thermal resistance

# read in measured data
heat_flux_dt <- fread(here::here("analysis", "summary_table", "summary_thermal_table.csv"))
# remove the abnormal values
heat_flux_dt <- heat_flux_dt[is.infinite(conductance)==FALSE & conductance>0 & conductance<20]
# calculate the sample mean
conductance_mean_dt <- heat_flux_dt[
    , lapply(.SD, mean, na.rm=TRUE), .SDcols = c("conductance"), c("location")]
names(conductance_mean_dt) <-  c("location", "conductance_mean")

heat_flux_dt <- merge(heat_flux_dt, conductance_mean_dt, by="location")

# 1-----------------------------------------------------------------------------
# The standard error of sample mean
# The variance is
item_count_dt <- as.data.table(table(data.frame(heat_flux_dt$location)))
names(item_count_dt) <- c("location", "sample_size")

conductance_sd_dt <- heat_flux_dt[
    , lapply(.SD, sd, na.rm=TRUE), .SDcols = c("conductance"), c("location")]
names(conductance_sd_dt)[2] <- "conductance_sd"

conductance_sd_dt <- merge(conductance_sd_dt, item_count_dt, by="location")
conductance_sd_dt <- merge(conductance_sd_dt, conductance_mean_dt, by="location")

# t is 1.64 for infinite data points (DF is infinite) and a 90% confidence level
t <- 1.64
conductance_sd_dt <- conductance_sd_dt %>%
    mutate(SE_conductance=conductance_sd/(sample_size^0.5)) %>%
    mutate(SE_conductance_percent = t*SE_conductance/conductance_mean)

# 2-----------------------------------------------------------------------------
# The standard error of the measured value of surface temperature and heat flux
# 2.1 uncertainty of external(internal) surface temperature measurement:0.2 ℃
u_T <- 0.2 # ℃
u_T_combined <- (0.2^2+0.2^2)^0.5

T_mean_dt <- heat_flux_dt[Heatflux>0] %>%
    mutate(T_diff=abs(RTDe-RTDi)) %>%
    select("location", "RTDe", "RTDi", "T_diff") %>%
    filter(T_diff<10)

Tsurf_mean_dt <- T_mean_dt[
    , lapply(.SD, mean, na.rm=TRUE), .SDcols = c("RTDe", "RTDi", "T_diff"), c("location")]
names(Tsurf_mean_dt) <-  c("location", "RTDe_mean", "RTDi_mean", "Tsurf_diff_mean")

# T_mean_dt <- heat_flux_dt[Heatflux>0
#                            , lapply(.SD, mean, na.rm=TRUE), .SDcols = c("RTDe", "RTDi"), c("location")] %>%
#     mutate(T_diff=abs(RTDe-RTDi))

# 2.2 uncertainty of Heat Flux measurement: ±2%
HF_mean_dt <- heat_flux_dt[Heatflux>0
    , lapply(.SD, mean, na.rm=TRUE), .SDcols = c("Heatflux"), c("location")]
names(HF_mean_dt) <- c("location", "heatflux_mean")

# t is 1.64 for infinite data points (DF is infinite) and a 90% confidence level
t <- 1.64
HF_mean_dt <- HF_mean_dt %>% mutate(u_HF=0.02*heatflux_mean/t)

# 3-----------------------------------------------------------------------------
# calculate combined uncertainty

conductance_u_dt <- merge(conductance_sd_dt, HF_mean_dt, by="location")
conductance_u_dt <- merge(conductance_u_dt, Tsurf_mean_dt, by="location")


conductance_u_dt <- conductance_u_dt %>%
    # mutate(u_T_HF_combined=((u_T_combined/Tsurf_diff_mean)^2 + (u_HF/heatflux_mean)^2)^0.5) %>%
    mutate(u_T_HF_combined=((u_T_combined/RTDi_mean)^2 + (u_HF/heatflux_mean)^2)^0.5) %>%
    mutate(u_T_HF_sample_combo=(u_T_HF_combined^2 + SE_conductance^2)^0.5) %>%
    mutate(u_T_HF_sample_percent=percent(u_T_HF_sample_combo/conductance_mean))

fwrite(conductance_u_dt, here::here("data", "uncertainty", "u_conductance_dt.csv"))


# #############################
# Measured fan power



# #############################
# Measured cooling energy




# ------------------------------------------------------------------------------
# CODE NOT IN USE
## calculate the time interval
sf6_dt2 <- ddply(sf6_dt, .(location), summarize, diff(datetime))
names(sf6_dt2) <- c("location", "time_diff")
sf6_dt2 <- as.data.table(sf6_dt2)
## Convert time intervals in minutes to seconds.
sf6_dt2$time_diff_secs <- as.numeric(sf6_dt2$time_diff, units="secs")
## add the row of zero time interval to each position group
sf6_dt2 <- sf6_dt2 %>%
    add_row(location="center", time_diff=as.difftime(0, units = "mins"), time_diff_secs=0, .before = 1)
sf6_dt2 <- sf6_dt2 %>%
    add_row(location="near_door", time_diff=as.difftime(0, units = "mins"),
            time_diff_secs=0, .before = which(sf6_dt2$location=="near_door")[1])
sf6_dt2 <- sf6_dt2 %>%
    add_row(location="near_window", time_diff=as.difftime(0, units = "mins"),
            time_diff_secs=0, .before = which(sf6_dt2$location=="near_window")[1])
## add calculated time intervals to raw data table
sf6_dt <- cbind(sf6_dt, sf6_dt2[,-"location"])

sf6_dt %>%
    mutate(time_spent = cumsum(time_)) %>%
    mutate(sf6_ppm_cal = time_spent*coefficient+residual)

# sf6_dt <- sf6_dt %>% group_by(location) %>%
#     mutate(sf6_ppm_cal = time_diff_secs*coefficient+residual)

# fwrite(sf6_dt2, here::here("data", "sf6_cal.csv"))

# 3-----------------------------------------------------------------------------
# The standard error of the measured value of SF6 concentration
# calculate the mean of the measurement
sf6_mean_dt <- sf6_dt[, lapply(.SD, mean), .SDcols = c("SF6 [ppm]"), c("location")]

# The standard error of sample mean
# sf6_dt <- heat_flux_dt %>% group_by(location) %>%
#     mutate(sample_diff_sqr = (conductance-conductance_mean_dt)^2) %>%
#     with(lapply(sample_diff_sqr, location, var))