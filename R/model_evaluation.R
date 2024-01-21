library(eplusr)
library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)
library(stringr)
library(fs)
library(purrr)
library(data.table)
library(dplyr)
library(reshape2)
library(scales)

library(frost)
library(ggthemes)
library(psychrolib)
library(plotly)


options(datatable.print.class = TRUE)
# for (f in fs::dir_ls(here("fun"), glob = "*.R")) source(f)


###################
#  Read in IDF  #

path_idfs <- fs::dir_ls(here("PAR3_1"), recurse = TRUE, regexp = "level.*\\.idf")
path_epws <- fs::dir_ls(here("data", "idf", "tidy_up", "epw"), recurse = TRUE, regexp = ".*[1-6]\\.epw")

# idf <- read_idf(path_idfs[1])
# idf <- read_idf(path_idfs[2])
# idf <- read_idf(path_idfs[3])
# idf <- read_idf(path_idfs[4])
# idf <- read_idf(path_idfs[5])
# idf <- read_idf(path_idfs[6])


####################
#  Create Measures  #

# set AHU fan for level 1 baseline model
set_AHUFan_lv1 <- function (idf, Fan = NA) {
  # keep the original if applicable
  if (is.na(Fan)) return(idf)
  
  idf$set(
    Branch_AirLoop2 = list(
      ..11 = "Fan:SystemModel",
      ..12 = Fan)
  )
  idf$del("Fan_AHU2_ex")
  
  idf
}


# set AHU fan for level 2 - level 6 baseline models
set_AHUFan <- function (idf, Fan = NA) {
  if (is.na(Fan)) return(idf)
  idf$set(
    Branch_AirLoop2 = list(
      ..15 = "Fan:SystemModel",
      ..16 = Fan)
  )
  idf$del("Fan_AHU2_ex")
  idf$set(
    EMSBasedFanAirMassFlowRateManager = list(
      ..4 = "")
  )
  
  idf
}


set_walls <- function (idf, wall_constr = NA) {
  # keep the original if applicable
  if (is.na(wall_constr)) return(idf)
  idf$set(
    Dagard_Wall_Panel = list(
      ..2 = wall_constr)
  )
  
  idf
}


set_window <- function (idf, win_constr = NA) {
  # keep the original if applicable
  if (is.na(win_constr)) return(idf)
  idf$set(
    Exterior_Window = list(
      ..2 = win_constr)
  )
  idf$set(
    Interior_Window = list(
      ..2 = win_constr)
  )
  
  idf
}


set_lights <- function (idf, lights = NA) {
  # keep the original if applicable
  if (is.na(lights)) return(idf)
  # set 'Watts per Zone Floor Area' in all 'Lights' objects as input LPD
  Lights_Room3 <- idf$to_table(lights)
  Lights_Room3[field == "Schedule Name", value := "Schcmpt_Occupancy1"]
  # update schedule object using the tidy table
  idf$update(Lights_Room3)
  
  idf
}


set_CHWClgSTP <- function (idf, CHWClgSTP = NA) {
  # keep the original if applicable
  if (is.na(CHWClgSTP)) return(idf)
  # Update "Coil:Cooling:Water"
  idf$set(
    ClgCoil_AHU2 = list(
      ..5 = CHWClgSTP)
  )
  # Update "Sizing:Plant"
  idf$set(`Sizing:Plant` := list(
    ..3 = CHWClgSTP
  ))
  # Update "SetpointManager:Scheduled" -> schedule Compact -> "Schcmpt_Temp_chw_Supply"
  idf$set(
    Schcmpt_Temp_chw_Supply = list(
      ..6 = as.character(CHWClgSTP))
  )
  
  idf
}


set_Daylight <- function (idf, DaylightContrl = NA) {
  # keep the original if applicable
  if (is.na(DaylightContrl)) return(idf)
  idf$set(`Daylighting:Controls` := list(
    ..4 = DaylightContrl))
  
  idf
}


# OP: number
set_mixmode <- function (idf, OP = NA) {
  # keep the original if applicable
  if (is.na(OP)) return(idf)
  # prepare settings for mix-mode simulation
  
  # 1
  idf$set(
    SchCmpt_Setpoint_Room3 = list(
      ..8 = "28")
  )
  
  # 2
  id1 <- idf$object_id("AirflowNetwork:MultiZone:Zone")[[1]][1]
  id2 <- idf$object_id("AirflowNetwork:MultiZone:Zone")[[1]][2]
  idf$object(id1)$set(Ventilation_Control_Mode = "Temperature")
  idf$object(id2)$set(Ventilation_Control_Mode = "Temperature")
  
  # 3 Update window opening factor
  WinSutf_id_lst <- idf$object_id("AirflowNetwork:MultiZone:Surface")[[1]][c(1, 2, 3, 22, 23, 24)]
  
  for (i in WinSutf_id_lst) {
    idf$object(i)$set(`Window/Door Opening Factor, or Crack Factor` = OP)
  }
  
  str_setOP_2 <- str_c("SET MyOpenFactor2", OP, sep = " = ")
  str_setOP_3 <- str_c("SET MyOpenFactor3", OP, sep = " = ")
  str_setOP_4 <- str_c("SET MyOpenFactor4", OP, sep = " = ")
  
  
  idf$set(
    OutdoorT_OpeningController_win2 = list(
      ..5 = str_setOP_2)
  )
  
  idf$set(
    OutdoorT_OpeningController_win3 = list(
      ..5 = str_setOP_3)
  )
  
  idf$set(
    OutdoorT_OpeningController_win4 = list(
      ..5 = str_setOP_4)
  )
  
  # 4
  # add new object: EnergyManagementSystem:ProgramCallingManager
  EMS_ProgramCallingManager1 <- list(`EnergyManagementSystem:ProgramCallingManager` = list(
    "AirLoopHVACAvailabilityManager", 
    "AfterPredictorAfterHVACManagers", 
    "AHU_off", 
    .comment = c("Comment for EMS new object 1")))
  
  EMS_ProgramCallingManager2 <- list(`EnergyManagementSystem:ProgramCallingManager` = list(
    "OutdoorT_OpeningController", 
    "BeginTimestepBeforePredictor", 
    "OutdoorT_OpeningController_win2", 
    "OutdoorT_OpeningController_win3", 
    "OutdoorT_OpeningController_win4",
    .comment = c("Comment for EMS new object 2")))
  
  idf$add(EMS_ProgramCallingManager1, EMS_ProgramCallingManager2)
  
  idf
}


####################
#  Apply Measures  #

# for the supplemental simulations, we need to 
# run "set_walls" for mixed-mode scenario
# run "set_lights" for mixed-mode scenario
# run "set_Daylight" for mixed-mode scenario
# run "set_Daylight" for AC-mode scenario

ecm <- function (idf, OP, wall_constr, lights, DaylightContrl) {
  idf %>% set_mixmode(OP) %>%
    set_walls(wall_constr) %>%
    set_lights(lights) %>%
    set_Daylight(DaylightContrl)
}



####################
#  Run Simulations  #

idf_lv1_concept <- read_idf(path_idfs[1])
path_epw <- path_epws[3]
param_lv1_concept <- param_job(idf_lv1_concept, path_epw)


param_lv1_concept$apply_measure(
  ecm,
  OP = c(1,1,1,1,1,1,1,1,1,NA,NA
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",NA,NA,NA,NA,NA,"Wall_0145"
  ),
  lights = c("Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", 
              "T5_more","T5_less","LED_less","Lights_Room3", "Lights_Room3", "Lights_Room3"
  ),
  DaylightContrl = c(NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight", "Schcmpt_Daylight",NA
  ),
  .names = c("lv1_1","lv1_2","lv1_3","lv1_4","lv1_5","lv1_6","lv1_7","lv1_8","lv1_9",
             "lv1_10","lv1_11"
  )
)

lv1_map <- c("L1__1__Wall_0145__MM",
             "L1__2__Wall_2161__MM",
             "L1__3__Wall_3504__MM",
             "L1__4__Wall_0525__MM",
             "L1__5__Wall_1355__MM",
             "L1__6__T5_more__MM",
             "L1__7__T5_less__MM",
             "L1__8__LED_less__MM",
             "L1__9__Schcmpt_Daylight__MM",
             "L1__10__Schcmpt_Daylight__AC",
             "L1__11__Wall_0145__AC")

param_lv1_concept$run()
saveRDS(param_lv1_concept, here("PAR3_1", "rds", "lv1_sim_extra.rds"))

# ------------------------------------------------------------------------------
# lv2
idf_lV2_specs <- read_idf(path_idfs[2])
path_epw <- path_epws[3]
param_lv2_specs <- param_job(idf_lV2_specs, path_epw)


param_lv2_specs$apply_measure(
  ecm,
  OP = c(1,1,1,1,1,1,1,1,1,NA,NA
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",NA,NA,NA,NA,NA,"Wall_0145"
  ),
  lights = c("Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", 
             "T5_more","T5_less","LED_less","Lights_Room3", "Lights_Room3", "Lights_Room3"
  ),
  DaylightContrl = c(NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight", "Schcmpt_Daylight",NA
  ),
  .names = c("lv2_1","lv2_2","lv2_3","lv2_4","lv2_5","lv2_6","lv2_7","lv2_8","lv2_9",
             "lv2_10","lv2_11"
  )
)

lv2_map <- c("L2__1__Wall_0145__MM",
             "L2__2__Wall_2161__MM",
             "L2__3__Wall_3504__MM",
             "L2__4__Wall_0525__MM",
             "L2__5__Wall_1355__MM",
             "L2__6__T5_more__MM",
             "L2__7__T5_less__MM",
             "L2__8__LED_less__MM",
             "L2__9__Schcmpt_Daylight__MM",
             "L2__10__Schcmpt_Daylight__AC",
             "L2__11__Wall_0145__AC")

param_lv2_specs$run()

saveRDS(param_lv2_specs, here("PAR3_1", "rds", "lv2_sim_extra.rds"))

# ------------------------------------------------------------------------------
# lv3
idf_lv3_FF <- read_idf(path_idfs[3])
path_epw <- path_epws[3]
param_lv3_FF <- param_job(idf_lv3_FF, path_epw)


param_lv3_FF$apply_measure(
  ecm,
  OP = c(1,1,1,1,1,1,1,1,1,NA,NA
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",NA,NA,NA,NA,NA,"Wall_0145"
  ),
  lights = c("Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", 
             "T5_more","T5_less","LED_less","Lights_Room3", "Lights_Room3", "Lights_Room3"
  ),
  DaylightContrl = c(NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight", "Schcmpt_Daylight",NA
  ),
  .names = c("lv3_1","lv3_2","lv3_3","lv3_4","lv3_5","lv3_6","lv3_7","lv3_8","lv3_9",
             "lv3_10","lv3_11"
  )
)

lv3_map <- c("L3__1__Wall_0145__MM",
             "L3__2__Wall_2161__MM",
             "L3__3__Wall_3504__MM",
             "L3__4__Wall_0525__MM",
             "L3__5__Wall_1355__MM",
             "L3__6__T5_more__MM",
             "L3__7__T5_less__MM",
             "L3__8__LED_less__MM",
             "L3__9__Schcmpt_Daylight__MM",
             "L3__10__Schcmpt_Daylight__AC",
             "L3__11__Wall_0145__AC")

param_lv3_FF$run()

saveRDS(param_lv3_FF, here("PAR3_1", "rds", "lv3_sim_extra.rds"))

# ------------------------------------------------------------------------------
# lv4
idf_lv4_ACH <- read_idf(path_idfs[4])
path_epw <- path_epws[3]
param_lv4_ACH <- param_job(idf_lv4_ACH, path_epw)

param_lv4_ACH$apply_measure(
  ecm,
  OP = c(1,1,1,1,1,1,1,1,1,NA,NA
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",NA,NA,NA,NA,NA,"Wall_0145"
  ),
  lights = c("Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", 
             "T5_more","T5_less","LED_less","Lights_Room3", "Lights_Room3", "Lights_Room3"
  ),
  DaylightContrl = c(NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight", "Schcmpt_Daylight",NA
  ),
  .names = c("lv4_1","lv4_2","lv4_3","lv4_4","lv4_5","lv4_6","lv4_7","lv4_8","lv4_9",
             "lv4_10","lv4_11"
  )
)

lv4_map <- c("L4__1__Wall_0145__MM",
             "L4__2__Wall_2161__MM",
             "L4__3__Wall_3504__MM",
             "L4__4__Wall_0525__MM",
             "L4__5__Wall_1355__MM",
             "L4__6__T5_more__MM",
             "L4__7__T5_less__MM",
             "L4__8__LED_less__MM",
             "L4__9__Schcmpt_Daylight__MM",
             "L4__10__Schcmpt_Daylight__AC",
             "L4__11__Wall_0145__AC")

param_lv4_ACH$run()

saveRDS(param_lv4_ACH, here("PAR3_1", "rds", "lv4_sim_extra.rds"))

# ------------------------------------------------------------------------------
# lv5
idf_lv5_Fan <- read_idf(path_idfs[5])
path_epw <- path_epws[3]
param_lv5_Fan <- param_job(idf_lv5_Fan, path_epw)

param_lv5_Fan$apply_measure(
  ecm,
  OP = c(1,1,1,1,1,1,1,1,1,NA,NA
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",NA,NA,NA,NA,NA,"Wall_0145"
  ),
  lights = c("Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", 
             "T5_more","T5_less","LED_less","Lights_Room3", "Lights_Room3", "Lights_Room3"
  ),
  DaylightContrl = c(NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight", "Schcmpt_Daylight",NA
  ),
  .names = c("lv5_1","lv5_2","lv5_3","lv5_4","lv5_5","lv5_6","lv5_7","lv5_8","lv5_9",
             "lv5_10","lv5_11"
  )
)

lv5_map <- c("L5__1__Wall_0145__MM",
             "L5__2__Wall_2161__MM",
             "L5__3__Wall_3504__MM",
             "L5__4__Wall_0525__MM",
             "L5__5__Wall_1355__MM",
             "L5__6__T5_more__MM",
             "L5__7__T5_less__MM",
             "L5__8__LED_less__MM",
             "L5__9__Schcmpt_Daylight__MM",
             "L5__10__Schcmpt_Daylight__AC",
             "L5__11__Wall_0145__AC")

param_lv5_Fan$run()

saveRDS(param_lv5_Fan, here("PAR3_1", "rds", "lv5_sim_extra.rds"))

# ------------------------------------------------------------------------------
# lv6

idf_lv6_ACMV <- read_idf(path_idfs[6])
path_epw <- path_epws[3]
param_lv6_ACMV <- param_job(idf_lv6_ACMV, path_epw)


param_lv6_ACMV$apply_measure(
  ecm,
  OP = c(1,1,1,1,1,1,1,1,1,NA,NA
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",NA,NA,NA,NA,NA,"Wall_0145"
  ),
  lights = c("Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", "Lights_Room3", 
             "T5_more","T5_less","LED_less","Lights_Room3", "Lights_Room3", "Lights_Room3"
  ),
  DaylightContrl = c(NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight", "Schcmpt_Daylight",NA
  ),
  .names = c("lv6_1","lv6_2","lv6_3","lv6_4","lv6_5","lv6_6","lv6_7","lv6_8","lv6_9",
             "lv6_10","lv6_11"
  )
)

lv6_map <- c("L6__1__Wall_0145__MM",
             "L6__2__Wall_2161__MM",
             "L6__3__Wall_3504__MM",
             "L6__4__Wall_0525__MM",
             "L6__5__Wall_1355__MM",
             "L6__6__T5_more__MM",
             "L6__7__T5_less__MM",
             "L6__8__LED_less__MM",
             "L6__9__Schcmpt_Daylight__MM",
             "L6__10__Schcmpt_Daylight__AC",
             "L6__11__Wall_0145__AC")

param_lv6_ACMV$run()

saveRDS(param_lv6_ACMV, here("PAR3_1", "rds", "lv6_sim_extra.rds"))

# ------------------------------------------------------------------------------
mapping <- data.table(no = seq(11), case = lv1_map)

mapping <- separate(mapping, case, c("level", "case", "ECM", "mode"), sep = "__", extra = "drop")
mapping$level <- str_replace(mapping$level, "L", "lv")

mapping$lv2 <- str_replace(mapping$level, "1", "2")
mapping$lv3 <- str_replace(mapping$level, "1", "3")
mapping$lv4 <- str_replace(mapping$level, "1", "4")
mapping$lv5 <- str_replace(mapping$level, "1", "5")
mapping$lv6 <- str_replace(mapping$level, "1", "6")

mapping$lv1 <- str_c(mapping$level, mapping$no, sep = "_")
mapping$lv2 <- str_c(mapping$lv2, mapping$no, sep = "_")
mapping$lv3 <- str_c(mapping$lv3, mapping$no, sep = "_")
mapping$lv4 <- str_c(mapping$lv4, mapping$no, sep = "_")
mapping$lv5 <- str_c(mapping$lv5, mapping$no, sep = "_")
mapping$lv6 <- str_c(mapping$lv6, mapping$no, sep = "_")

mapping <- select(mapping, "no", "mode", "ECM", "lv1", "lv2", "lv3", "lv4", "lv5", "lv6")
fwrite(mapping, here("data", "csv", "PAR3_1", "mapping_extra_sim.csv"))

# ------------------------------------------------------------------------------
# extract total end use and end-use breakdown

# Calculate end use breakdown
extract_end_use2 <- function(param, level) {
  param_end_use <- param$tabular_data(table_name = "End Uses", wide = TRUE)[[1L]]
  param_end_use[row_name == "Cooling",]$`Electricity [kWh]` <- param_end_use[
    row_name == "Cooling",]$`District Cooling [kWh]`
  end_use_dt <- param_end_use %>% filter(`Electricity [kWh]`>0, row_name != "Total End Uses") %>% 
    select("case", "row_name", "Electricity [kWh]")
  end_use_dt <- merge(end_use_dt, mapping[, c("no","mode","ECM",level), with=FALSE], by.x = "case", by.y = level)
  end_use_dt <- select(end_use_dt, c(case, no, mode, ECM, everything()))
  end_use_dt$Levels <- level
  end_use_dt$no <- as.numeric(end_use_dt$no)
  end_use_dt <- setorder(end_use_dt, no)
  return(end_use_dt)
}

lv1_end_use_dt <- extract_end_use2(param_lv1_concept, "lv1")
lv2_end_use_dt <- extract_end_use2(param_lv2_specs, "lv2")
lv3_end_use_dt <- extract_end_use2(param_lv3_FF, "lv3")
lv4_end_use_dt <- extract_end_use2(param_lv4_ACH, "lv4")
lv5_end_use_dt <- extract_end_use2(param_lv5_Fan, "lv5")
lv6_end_use_dt <- extract_end_use2(param_lv6_ACMV, "lv6")

l <- list(lv1_end_use_dt, lv2_end_use_dt, lv3_end_use_dt, lv4_end_use_dt, lv5_end_use_dt, lv6_end_use_dt)
names(l) <- c("lv1_concept_model", "lv2_specs_model", "lv3_FF_model", "lv4_ACH_model", "lv5_Fan_model", "lv6_ACMV_model")

bind_end_use <- rbindlist(l, use.names=TRUE, idcol="file")
fwrite(bind_end_use, here("data", "csv", "PAR3_1", "end_use_bind_extra.csv"))

# Calculate total end use
total_end_use <- bind_end_use[, lapply(.SD, sum, na.rm=TRUE), .SDcols = "Electricity [kWh]", 
                              by=c("file", "case", "Levels", "no", "mode", "ECM")]

fwrite(total_end_use, here("data", "csv", "PAR3_1", "end_use_total_extra.csv"))

# calculate energy savings
diff_total_melt <- dcast(select(total_end_use, file, no, mode, ECM, `Electricity [kWh]`), no+mode+ECM~file)

diff_total_melt[mode == "MM", c("lv1_relative_diff", "lv2_relative_diff", 
                              "lv3_relative_diff",
                              "lv4_relative_diff",
                              "lv5_relative_diff",
                              "lv6_relative_diff") := list(
                                lv1_concept_model - 17156.29,
                                lv2_specs_model - 18410.87,
                                lv3_FF_model - 18811.07,
                                lv4_ACH_model - 18811.55,
                                lv5_Fan_model - 20773.09,
                                lv6_ACMV_model - 15782.76
                              )]

diff_total_melt[mode == "AC", c("lv1_relative_diff", 
                                "lv2_relative_diff", 
                                "lv3_relative_diff",
                                "lv4_relative_diff",
                                "lv5_relative_diff",
                                "lv6_relative_diff") := list(
                                  lv1_concept_model - 23108.86,
                                  lv2_specs_model - 20089.65,
                                  lv3_FF_model - 22216.65,
                                  lv4_ACH_model - 22216.65,
                                  lv5_Fan_model - 24656.25,
                                  lv6_ACMV_model - 17745.86
                                )]

# add ECM type column
diff_total_melt$ECM_type <- "WALL"

diff_total_melt[6:8, ]$ECM_type <- "LI"
diff_total_melt[9:10, ]$ECM_type <- "DAYLIGHTING"
diff_total_melt <- diff_total_melt[-11, ]

fwrite(diff_total_melt, here("data", "csv", "PAR3_1", "extra_sim.csv"))

# ------------------------------------------------------------------------------
# Combine the supplemental simulation results with old simulation results

diff_total_melt_1 <- fread(here("data", "csv", "PAR3", "diff_total_melt3.csv"))
diff_total_melt_1[mode=="BASE", ]$mode <- "AC"
diff_total_melt_1$`calibration levels` <- str_replace(diff_total_melt_1$`calibration levels`,"diff2","diff")

diff_total_melt2 <- melt(select(diff_total_melt, 
                                setdiff(names(diff_total_melt), 
                                        c("lv1_concept_model","lv2_specs_model","lv3_FF_model",
                                          "lv4_ACH_model","lv5_Fan_model","lv6_ACMV_model")
                                        )
                                ), 
                         id=c("no","mode","ECM", "ECM_type"))
names(diff_total_melt2) <- c("no","mode","ECM_options","ECM_type","calibration levels","predicted relative difference")
diff_total_melt2$no <- 0

# generate diff_total_combines
diff_total_combines <- rbind(diff_total_melt_1[, -"ECM_and_mode"], diff_total_melt2, use.names=TRUE)

diff_total_combines <- setorder(diff_total_combines, mode, ECM_type, ECM_options, `calibration levels`)
diff_total_combines <- diff_total_combines[
  mode %in% c("AC", "MM") & ECM_type %in% c("WALL", "FAN", "LI", "CHWT", "WIN", "DAYLIGHTING"), ]

fwrite(diff_total_combines, here("data", "csv", "PAR3_1", "diff_total_combines.csv"))

# ------------------------------------------------------------------------------
# ranking the total energy 

diff_total_combines <- diff_total_combines %>% group_by(mode, ECM_type, `calibration levels`) %>% 
  arrange(`predicted relative difference`, .by_group = TRUE) %>% 
  mutate(ranking = row_number())

best_ecm <- diff_total_combines %>% filter(ranking == 1)


best_ecm_type <- best_ecm %>% group_by(mode, `calibration levels`) %>% 
  arrange(`predicted relative difference`, .by_group = TRUE) %>% 
  mutate(ranking_ECM_type = row_number()) %>% 
  select(setdiff(names(best_ecm), c("no", "ranking")), ranking_ECM_type) %>% 
  mutate(`Estimated energy savings [kWh]`= abs(`predicted relative difference`))

best_ecm_type <- as.data.table(best_ecm_type)
best_ecm_type[`predicted relative difference`>0, ]$`Estimated energy savings [kWh]` <- -1*best_ecm_type[
  `predicted relative difference`>0, ]$`predicted relative difference`

names(best_ecm_type)[4] <- "Levels"
names(best_ecm_type)[6] <- "ranking"


best_ecm_type$Levels <- str_remove(best_ecm_type$Levels, "_relative_diff")
best_ecm_type$Levels <- str_replace(best_ecm_type$Levels, "lv", "Level ")

fwrite(best_ecm_type, here("data", "csv", "PAR3_1", "best_ecm_type.csv"))

# ------------------------------------------------------------------------------
# Visualization
best_ecm_type <- fread(here("data", "csv", "PAR3_1", "best_ecm_type.csv")) %>% 
  mutate(Levels=str_replace_all(best_ecm_type$Levels, "Level", "Model"))

# revise Levels 1 to 6 into Models 1 to 6

p_ranking_base <- ggplot(data=best_ecm_type[mode=="AC", ],
                         aes(x=Levels, y=ranking, 
                             color=ECM_type, group=ECM_type)) +
  geom_point(size=3) +
  geom_line(linewidth=1) +
  # facet_wrap(~ECM_type) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "", y = "ECM ranking", color="ECM types") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))

ggsave(filename = here::here("data","revision", "V3", "ECM_ranking_AC.jpg"), 
       plot = p_ranking_base, dpi = 300, height = 4, width = 6)


p_ranking_MM <- ggplot(data=best_ecm_type[mode=="MM", ],
                         aes(x=Levels, y=ranking, 
                             color=ECM_type, group=ECM_type)) +
  geom_point(size=3) +
  geom_line(size=1) +
  # facet_wrap(~ECM_type) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "", y = "ECM ranking", color="ECM types") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))

ggsave(filename = here::here("data","revision", "V3", "ECM_ranking_MM.jpg"), 
       plot = p_ranking_MM, dpi = 300, height = 4, width = 6)

# ------------------------------------------------------------------------------
# Energy savings

# AC mode
## scaleFUN <- function(x) sprintf("%.1f", x)
p_bar_ECM_savings <- ggplot(best_ecm_type[mode=="AC",], aes(x=Levels, y=`Estimated energy savings [kWh]`, fill=ECM_type)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Models", y = "Estimated energy savings [kWh]") +
  facet_wrap(~factor(ECM_type, levels=c("WALL", "FAN", "DAYLIGHTING", "LI", "CHWT", "WIN")), 
             scales = "free_y") +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set2")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5","6")) +
  geom_text(aes(label = sprintf("%.1f", `Estimated energy savings [kWh]`)), vjust = -0.2, size=2.5)

ggsave(filename = here::here("data","revision", "V3", "ECM_savings_AC.jpg"), 
       plot = p_bar_ECM_savings, dpi = 300, height = 5, width = 6)


# mixed mode
p_bar_ECM_savings_all <- ggplot(best_ecm_type, aes(x=Levels, y=`Estimated energy savings [kWh]`, fill=mode)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Models", y = "Estimated energy savings [kWh]") +
  facet_wrap(~factor(ECM_type, levels=c("WALL", "FAN", "DAYLIGHTING", "LI", "CHWT", "WIN")), 
             scales = "free_y") +
  theme(legend.position="top") +
  scale_fill_brewer(palette = "Set2")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5","6")) +
  geom_text(aes(label = sprintf("%.1f", `Estimated energy savings [kWh]`)), vjust = -0.2, size=2.5)

ggsave(filename = here::here("data","revision", "V3", "ECM_savings_all.jpg"), 
       plot = p_bar_ECM_savings_all, dpi = 300, height = 5, width = 6)
# ------------------------------------------------------------------------------
# percentage difference
best_ecm_type_wide <- dcast(best_ecm_type[, c("mode", "ECM_type", "Levels", "Estimated energy savings [kWh]")], 
                         mode+ECM_type~Levels, 
                         value.var="Estimated energy savings [kWh]")

best_ecm_type_wide[, `1 and 6`:=(`Level 1`-`Level 6`) / `Level 6`]
best_ecm_type_wide[, `2 and 6`:=(`Level 2`-`Level 6`) / `Level 6`]
best_ecm_type_wide[, `3 and 6`:=(`Level 3`-`Level 6`) / `Level 6`]
best_ecm_type_wide[, `4 and 6`:=(`Level 4`-`Level 6`) / `Level 6`]
best_ecm_type_wide[, `5 and 6`:=(`Level 5`-`Level 6`) / `Level 6`]
best_ecm_type_wide[, `6 and 6`:=(`Level 6`-`Level 6`) / `Level 6`]


best_ecm_percent_diff <- melt(best_ecm_type_wide[, -c(
  "Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Level 6")], 
  id=c("mode", "ECM_type"), variable.name="Levels", value.name = "Percentage difference")

fwrite(best_ecm_percent_diff, here("data", "csv", "PAR3_1", "best_ecm_percent_diff.csv"))
fwrite(best_ecm_type_wide, here("data", "csv", "PAR3_1", "best_ecm_type_wide.csv"))

# AC mode
p_bar_percent_diff_AC <- ggplot(best_ecm_percent_diff[mode=="AC", ], aes(x=Levels, y=`Percentage difference`, fill=ECM_type)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Models", y = "Difference percentage (%) in the estimated energy savings") +
  facet_wrap(~factor(ECM_type, levels=c("WALL", "FAN", "DAYLIGHTING", "LI", "CHWT", "WIN")),
             scales = "free_y") +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set2")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5","6")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = scales::percent(round(`Percentage difference`, 2))), vjust = -0.2, size=2.5)
  
ggsave(filename = here::here("data","revision", "V3","savings_diff_percentage_AC.jpg"), 
       plot = p_bar_percent_diff_AC, dpi = 300, height = 5, width = 7.5)

# MM mode
p_bar_percent_diff_MM <- ggplot(best_ecm_percent_diff[mode=="MM", ], aes(x=Levels, y=`Percentage difference`, fill=ECM_type)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Levels", y = "Difference percentage (%) in the estimated energy savings") +
  facet_wrap(~factor(ECM_type, levels=c("WALL", "FAN", "DAYLIGHTING", "LI", "CHWT", "WIN")),
             scales = "free_y") +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set2")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5","6")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = scales::percent(round(`Percentage difference`, 2))), vjust = -0.2, size=2.5)

ggsave(filename = here::here("data","revision", "V2","savings_diff_percentage_MM.jpg"), 
       plot = p_bar_percent_diff_MM, dpi = 300, height = 5, width = 7.5)


# AC mode and MM mode comparison
p_bar_percent_diff_combine <- ggplot(best_ecm_percent_diff, aes(x=Levels, y=`Percentage difference`, fill=mode)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Levels", y = "Difference percentage (%) in the estimated energy savings") +
  facet_wrap(~factor(ECM_type, levels=c("WALL", "FAN", "DAYLIGHTING", "LI", "CHWT", "WIN")),
             scales = "free_y") +
  theme(legend.position="top") +
  scale_fill_brewer(palette = "Set2")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5","6")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = scales::percent(round(`Percentage difference`, 2))), vjust = -0.2, size=2.5)

ggsave(filename = here::here("data","revision", "V2","savings_diff_percent_both_3.jpg"), 
       plot = p_bar_percent_diff_combine, dpi = 300, height = 8, width = 7.5)

p_bar_percent_diff_combine <- ggplot(best_ecm_percent_diff, aes(x=Levels, y=`Percentage difference`, fill=mode)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Levels", y = "Difference percentage (%) in the estimated energy savings") +
  facet_wrap(~factor(ECM_type, levels=c("WALL", "FAN", "DAYLIGHTING", "LI", "CHWT", "WIN")),
             scales = "fixed") +
  theme(legend.position="top") +
  scale_fill_brewer(palette = "Set2")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5","6")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = scales::percent(round(`Percentage difference`, 2))), vjust = -0.2, size=2.5)


# ------------------------------------------------------------------------------
# percentage difference of all design options 
# diff_total_combines ★★★★★★

diff_total_combines_wide <- dcast(diff_total_combines[, c("mode", "ECM_type", "ECM_options", "calibration levels", "predicted relative difference")], 
                            mode+ECM_type+ECM_options~`calibration levels`, 
                            value.var="predicted relative difference")
diff_total_combines_wide <- as.data.table(diff_total_combines_wide)

diff_total_combines_wide[, `1 and 6`:=(lv1_relative_diff-lv6_relative_diff) / lv6_relative_diff]
diff_total_combines_wide[, `2 and 6`:=(lv2_relative_diff-lv6_relative_diff) / lv6_relative_diff]
diff_total_combines_wide[, `3 and 6`:=(lv3_relative_diff-lv6_relative_diff) / lv6_relative_diff]
diff_total_combines_wide[, `4 and 6`:=(lv4_relative_diff-lv6_relative_diff) / lv6_relative_diff]
diff_total_combines_wide[, `5 and 6`:=(lv5_relative_diff-lv6_relative_diff) / lv6_relative_diff]
diff_total_combines_wide[, `6 and 6`:=(lv6_relative_diff-lv6_relative_diff) / lv6_relative_diff]

diff_total_combines_percent <- melt(diff_total_combines_wide[, -c(
  "lv1_relative_diff", "lv2_relative_diff", "lv3_relative_diff", "lv4_relative_diff", "lv5_relative_diff", "lv6_relative_diff")], 
  id=c("mode", "ECM_type", "ECM_options"), variable.name="Levels", value.name = "Percentage difference")

diff_total_combines_percent <- merge(diff_total_combines_percent, 
                                     diff_total_combines[,c("mode","ECM_type","ECM_options","Levels", "ranking")],
                                     by=c("mode","ECM_type","ECM_options","Levels"))

fwrite(diff_total_combines_percent, here("data", "csv", "PAR3_1", "diff_total_combines_percent.csv"))


diff_total_combines_savings <- melt(diff_total_combines_wide[, -c(
  "1 and 6","2 and 6","3 and 6","4 and 6","5 and 6","6 and 6")], 
  id=c("mode", "ECM_type", "ECM_options"), variable.name="Levels", value.name = "Estimated energy savings [kWh]")



diff_total_combines_wide2 <- dcast(diff_total_combines[, c("mode", "ECM_type", "ECM_options", "calibration levels", "ranking")], 
      mode+ECM_type+ECM_options~`calibration levels`, 
      value.var="ranking")
names(diff_total_combines_wide2) <- c("mode", "ECM_type", "ECM_options", "lv1_ranking",
                                      "lv2_ranking", "lv3_ranking", "lv4_ranking",
                                      "lv5_ranking","lv6_ranking")
diff_total_combines_wide <- merge(diff_total_combines_wide, diff_total_combines_wide2, 
                                  by=c("mode", "ECM_type", "ECM_options"))

# ------------------------------------------------------------------------------
abs.mean <- function(x){
  abs_mean <- mean(abs(x))
  names(abs_mean) <- "absolute mean difference percentage"
  return(abs_mean)
}

abs.max <- function(x){
  abs_max <- max(abs(x))
  names(abs_max) <- "absolute max difference percentage"
  return(abs_max)
}

# Box plot percentage
p_box_percent_diff_combine <- ggplot(diff_total_combines_percent[Levels != "6 and 6", ], aes(x=Levels, y=`Percentage difference`, fill=mode)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "", y = "Percentage difference as compared to Level 6") +
  facet_wrap(~mode) +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set2") +
  ylim(-5, 5) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5"))+
  stat_summary(fun=abs.mean,geom="point", shape=4, size=4) +
  stat_summary(fun=abs.max,geom="point", shape=15, size=2)

ggsave(filename = here("data","revision", "V2", "energy_saving_percent_diff_box.jpg"), 
       plot = p_box_percent_diff_combine, dpi = 300, height = 4, width = 6)

ggplotly(p_box_percent_diff_combine)

p_box_percent_diff_combine2 <- ggplot(diff_total_combines_percent[Levels != "6 and 6" & ECM_type != "DAYLIGHTING", ], 
                                      aes(x=Levels, y=`Percentage difference`, fill=ECM_type)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "", y = "Percentage difference as compared to Level 6") +
  facet_wrap(~mode) +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set2") +
  ylim(-5, 5) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5"))

p_box_percent_diff_combine3 <- ggplot(diff_total_combines_percent[Levels != "6 and 6" & ECM_type != "DAYLIGHTING", ], 
                                      aes(x=Levels, y=`Percentage difference`, fill=ECM_type)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "", y = "Percentage difference as compared to Level 6") +
  facet_grid(ECM_type~mode, scales = "free_y") +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set2") +
  ylim(-5, 5) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5"))
  # stat_summary(fun=abs.mean,geom="point", shape=4, size=4)
  # stat_summary(fun=abs.max,geom="point", shape=15, size=2)


p_line_percent_diff <- ggplot(diff_total_combines_percent[Levels != "6 and 6", ], 
                               aes(x=Levels, y=`Percentage difference`, color=ECM_options, group=ECM_options)) +
  geom_line() +
  labs(x = "", y = "Percentage difference as compared to Level 6") +
  facet_grid(ECM_type~mode) +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "none") +
  # scale_color_brewer(palette = "Set2") +
  ylim(-5, 5) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5"))

p_line_percent_diff2 <- ggplot(diff_total_combines_percent[Levels != "6 and 6", ], 
                                      aes(x=Levels, y=`Percentage difference`, color=ECM_type, group=ECM_options)) +
  geom_line(alpha=0.5, size=1.5) +
  labs(x = "", y = "Percentage difference as compared to Level 6") +
  facet_wrap(~mode)+
  # facet_grid(ECM_type~mode) +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1") +
  ylim(-5, 5) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5"))+
  geom_hline(aes(yintercept=0.3), linetype="dashed", color="black")+
  geom_hline(aes(yintercept=-0.3), linetype="dashed", color="black")

ggsave(filename = here("data","revision", "V2", "energy_saving_percent_line3c.jpg"), 
       plot = p_line_percent_diff3, dpi = 300, height = 5, width = 6)

p_line_percent_diff3b <- ggplot(diff_total_combines_percent[Levels != "1 and 6" & `Percentage difference`<5, ], 
                               aes(x=Levels, y=`Percentage difference`, color=ECM_type, group=ECM_options)) +
  geom_line(alpha=0.5, size=1.5) +
  labs(x = "", y = "Percentage difference as compared to Level 6") +
  facet_wrap(~mode)+
  # facet_grid(ECM_type~mode) +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  # scale_color_brewer(palette = "Set1") +
  scale_color_jco()+
  # ylim(-5, 5) +
  scale_x_discrete(labels=c("Level2", "Level3", "Level4", "Level5", "Level6"))+
  geom_hline(aes(yintercept=0.3), linetype="dashed", color="black")+
  geom_hline(aes(yintercept=-0.3), linetype="dashed", color="black")


p_line_percent_diff5 <- ggplot(diff_total_combines_percent[Levels != "1 and 6" & `Percentage difference`<5, ], 
                                aes(x=Levels, y=`Percentage difference`, color=ECM_type, group=ECM_options)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "Percentage difference as compared to Level 6") +
  facet_grid(mode~ECM_type)+
  # facet_grid(ECM_type~mode) +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set2") +
  # scale_color_jco()+
  # ylim(-5, 5) +
  scale_x_discrete(labels=c("L2", "L3", "L4", "L5", "L6"))+
  geom_hline(aes(yintercept=0.3), linetype="dashed", color="black")+
  geom_hline(aes(yintercept=-0.3), linetype="dashed", color="black")+
  geom_hline(aes(yintercept=0), linetype="solid", color="black")

ggsave(filename = here("data","revision", "V2", "energy_saving_percent_line5.jpg"), 
       plot = p_line_percent_diff5, dpi = 300, height = 5, width = 7.5)

p_line_percent_diff5b <- ggplot(diff_total_combines_percent[Levels != "1 and 6" & `Percentage difference`<5, ], 
                               aes(x=Levels, y=`Percentage difference`, group=ECM_options)) +
  geom_line() +
  labs(x = "", y = "Percentage difference as compared to Level 6") +
  facet_grid(mode~ECM_type)+
  # facet_grid(ECM_type~mode) +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  # scale_color_brewer(palette = "Set3") +
  # scale_color_jco()+
  # ylim(-5, 5) +
  scale_x_discrete(labels=c("L2", "L3", "L4", "L5", "L6"))+
  geom_hline(aes(yintercept=0.3), linetype="dashed", color="black")+
  geom_hline(aes(yintercept=-0.3), linetype="dashed", color="black")+
  geom_hline(aes(yintercept=0), linetype="solid", color="black")

ggsave(filename = here("data","revision", "V2", "energy_saving_percent_line5.jpg"), 
       plot = p_line_percent_diff5C, dpi = 300, height = 5, width = 7.5)


p_line_percent_diff5C <- ggplot(diff_total_combines_percent[Levels != "1 and 6" & `Percentage difference`<5
                                                            &grepl("0.5&|0.75&", diff_total_combines_percent$ECM_options)==FALSE, ], 
                               aes(x=Levels, y=`Percentage difference`, color=ECM_type, group=ECM_options, shape=as.factor(ranking))) +
  geom_line() +
  geom_point() +
  labs(x = "Models", y = "Percentage difference as compared to Level 6") +
  facet_grid(mode~ECM_type)+
  # facet_grid(ECM_type~mode) +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values=c(17, 19, 19, 19, 19, 19, 19, 19, 19, 19))+
  # scale_linetype_manual(values = c("dotted","solid","solid","solid","solid",
  #                                  "solid","solid","solid","solid","solid")) +
  guides(shape="none") +
  # scale_color_jco()+
  # ylim(-5, 5) +
  scale_x_discrete(labels=c("2", "3", "4", "5", "6"))+
  geom_hline(aes(yintercept=0.3), linetype="dashed", color="black")+
  geom_hline(aes(yintercept=-0.3), linetype="dashed", color="black")+
  geom_hline(aes(yintercept=0), linetype="solid", color="black")

ggsave(filename = here("data","revision", "V3", "energy_saving_percent_line5.jpg"), 
       plot = p_line_percent_diff5C, dpi = 300, height = 5, width = 7.5)

# Box plot energy savings 
p_box_savings_combine <- ggplot(diff_total_combines_savings, aes(x=Levels, y=`Estimated energy savings [kWh]`, fill=mode)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "", y = "Estimated energy savings [kWh]") +
  facet_wrap(~mode, scales = "free_y") +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set2") +
  ylim(-10000, 10000) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5", "Level6"))+
  stat_summary(fun=abs.mean,geom="point", shape=4, size=4) +
  stat_summary(fun=abs.max,geom="point", shape=15, size=2)


# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
# energy saving ratio v.s. percentage difference

diff_total_combines$Levels <- str_replace(diff_total_combines$`calibration levels`, "_relative_diff", "")
diff_total_combines$Levels <- str_replace(diff_total_combines$Levels, "lv", "")
diff_total_combines$Levels <- str_c(diff_total_combines$Levels, "and 6", sep = " ")


diff_total_combines <- merge(diff_total_combines, diff_total_combines_percent, by=c("mode", "ECM_type", "ECM_options", "Levels"))

diff_total_combines <- as.data.table(diff_total_combines)
# diff_total_combines$base_energy_use <- NA

diff_total_combines_refined <- diff_total_combines[grepl("0.5&|0.75&", diff_total_combines$ECM_options)==FALSE, ]
# diff_total_combines[mode=="MM" & `calibration levels`=="lv1_relative_diff"& ECM_options=="Wall_0145", ]$`predicted relative difference`

diff_total_combines_refined$base_energy_use <- 0

diff_total_combines_refined[mode=="AC"& grep("lv1", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 23108.86
diff_total_combines_refined[mode=="AC"& grep("lv2", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 20089.65
diff_total_combines_refined[mode=="AC"& grep("lv3", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 22216.65
diff_total_combines_refined[mode=="AC"& grep("lv4", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 22216.65
diff_total_combines_refined[mode=="AC"& grep("lv5", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 24656.25
diff_total_combines_refined[mode=="AC"& grep("lv6", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 17745.86

diff_total_combines_refined[mode=="MM"& grep("lv1", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 17156.29
diff_total_combines_refined[mode=="MM"& grep("lv2", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 18410.87
diff_total_combines_refined[mode=="MM"& grep("lv3", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 18811.07
diff_total_combines_refined[mode=="MM"& grep("lv4", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 18811.55
diff_total_combines_refined[mode=="MM"& grep("lv5", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 20773.09
diff_total_combines_refined[mode=="MM"& grep("lv6", diff_total_combines_refined$`calibration levels`), ]$base_energy_use <- 15782.76

diff_total_combines_refined$`Estimated energy savings [kWh]` <- diff_total_combines_refined$`predicted relative difference` * (-1)
diff_total_combines_refined[, `energy saving ratio (%)` :=  `Estimated energy savings [kWh]`/base_energy_use *100]
diff_total_combines_refined[, `Percentage difference (%)`:= `Percentage difference`*100]

fwrite(diff_total_combines_refined, here("data", "csv", "PAR3_1", "diff_total_combines_refined.csv"))


p_relation_saving_n_diff_all <- ggplot(diff_total_combines_refined[
  `energy saving ratio (%)`>0 & Levels!="6 and 6", ], aes(
    `energy saving ratio (%)`, `Percentage difference (%)`, color=Levels,fill=Levels,shape=ECM_type)
  ) +
  geom_point(size=2) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Energy saving ratio (%)", y = "Percentage difference of\nenergy saving estimate to Level 6", 
       color="Percentage difference\nbetween Levels", fill="Percentage difference between Levels", shape="ECM types") +
  theme(legend.position = "top") +
  ylim(-500, 500) +
  xlim(0,10)+
  scale_shape_manual(values = c(20, 21, 22, 23, 24, 25))+
  guides(color = guide_legend(nrow = 2))

ggsave(filename = here("data","revision","V2", "relation_saving_n_diff2.jpg"), 
       plot = p_relation_saving_n_diff_all, dpi = 300, height = 6, width = 6)

p_relation_saving_n_diff_all <- ggplot(diff_total_combines_refined[
  `energy saving ratio (%)`>0 & Levels!="6 and 6", ], aes(
    `energy saving ratio (%)`, `Percentage difference (%)`, color=ECM_type)
) +
  geom_point(size=2) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Energy saving ratio (%)", y = "Percentage difference of\nenergy saving estimate to Level 6", 
       color="Percentage difference\nbetween Levels", fill="Percentage difference between Levels", shape="ECM types") +
  theme(legend.position = "top") +
  ylim(-500, 500) +
  xlim(0,10)+
  scale_shape_manual(values = c(20, 21, 22, 23, 24, 25))+
  guides(color = guide_legend(nrow = 2))



# Box plot energy savings 

p_box_saving_ratio_combine2 <- ggplot(diff_total_combines_refined, aes(x=Levels, y=`energy saving ratio (%)`, fill=mode)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "", y = "Estimated energy saving ratios [%]") +
  facet_wrap(~ECM_type, scales = "free_y") +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set2") +
  # ylim(-25, 25) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5", "Level6"))+
  stat_summary(fun=abs.mean,geom="point", shape=4, size=4) +
  stat_summary(fun=abs.max,geom="point", shape=15, size=2)

ggsave(filename = here("data","revision","V2", "saving_ratio_box_1.jpg"), 
       plot = p_box_saving_ratio_combine2, dpi = 300, height = 6, width = 8)


# AC mode estimated energy savings box plot
p_box_saving_ratio_combine3 <- ggplot(diff_total_combines_refined[mode=="AC" & `Estimated energy savings [kWh]`>0 ,], aes(x=Levels, y=`energy saving ratio (%)`, fill=ECM_type)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "", y = "Estimated energy saving ratios [%]") +
  facet_wrap(~ECM_type, scales = "free_y") +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set2") +
  # ylim(-25, 25) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5", "Level6"))+
  stat_summary(fun=abs.mean,geom="point", shape=4, size=4) +
  stat_summary(fun=abs.max,geom="point", shape=15, size=2)

p_box_saving_ratio_combine3 <- ggplot(diff_total_combines_refined[mode=="AC" & ranking==1,], 
                                      aes(x=Levels, y=`energy saving ratio (%)`)) +
  geom_point() +
  labs(x = "", y = "Estimated energy saving ratios [%]") +
  facet_wrap(~ECM_type, scales = "free_y") +
  # facet_grid(cols = vars(mode), scales = "free_x", space = "free_x")+
  theme(legend.position = "top") +
  # scale_fill_brewer(palette = "Set2") +
  # ylim(-25, 25) +
  scale_x_discrete(labels=c("Level1", "Level2", "Level3", "Level4", "Level5", "Level6"))
  # stat_summary(fun=abs.mean,geom="point", shape=4, size=4) +
  # stat_summary(fun=abs.max,geom="point", shape=15, size=2)
# ------------------------------------------------------------------------------
# unquote the two functions later
# ------------------------------------------------------------------------------
# ecm_lv1 <- function (idf, OP, wall_constr, win_constr, lights, CHWClgSTP, DaylightContrl, Fan) {
#   idf %>% set_mixmode(OP) %>% 
#     set_walls(wall_constr) %>% 
#     set_window(win_constr) %>% 
#     set_lights(lights) %>% 
#     set_CHWClgSTP(CHWClgSTP) %>% 
#     set_Daylight(DaylightContrl) %>% 
#     set_AHUFan_lv1(Fan)
# }


# ecm <- function (idf, OP, wall_constr, win_constr, lights, CHWClgSTP, DaylightContrl, Fan) {
#   idf %>% set_mixmode(OP) %>% 
#     set_walls(wall_constr) %>% 
#     set_window(win_constr) %>% 
#     set_lights(lights) %>% 
#     set_CHWClgSTP(CHWClgSTP) %>% 
#     set_Daylight(DaylightContrl) %>% 
#     set_AHUFan(Fan)
# }

# ##############################################################################

# Draw accuracy line plot
accuracy_dt <- fread(here("data", "csv", "PAR3", "model accuracy.csv"))
accuracy_dt_melt <- melt(accuracy_dt, id = c(
  "model version number", "stage of calibration process", "data sources" ))
accuracy_dt_melt$value <- str_replace(accuracy_dt_melt$value, "%", "")
accuracy_dt_melt$value <- as.numeric(accuracy_dt_melt$value)
accuracy_dt_melt <- as.data.table(accuracy_dt_melt)

# subset(accuracy_dt_melt, grepl("Zone Dry Bulb Temperature", variable))

accuracy_dt_melt$variable2 <- accuracy_dt_melt$variable

accuracy_dt_melt[variable2=="Zone Dry Bulb Temperature", ]$variable2 <- "free-floating temperature"
accuracy_dt_melt[variable=="AHU Fan power", ]$variable2 <- "AHU fan power"
accuracy_dt_melt[variable2=="Building Cooling Load", ]$variable2 <- "cooling energy"

p_accuracy <- ggplot(data=accuracy_dt_melt, 
                     aes(x=`model version number`, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  # facet_wrap(~variable) +
  labs(x = "", y = "CV(RMSE) (%)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Set2", name="", 
                     labels=c("free-floating temperature", "AHU Fan power", "cooling coil cooling power"))

ggsave(filename = here("data","revision","calibration_level_accuracy2.jpg"), 
       plot = p_accuracy, dpi = 300, height = 4, width = 6)

p_accuracy_2 <- ggplot(data=accuracy_dt_melt, 
                       aes(x=`model version number`, y=value, group=variable2)) +
  geom_point() +
  geom_line() +
  facet_wrap(~factor(variable2, levels=c("free-floating temperature", "AHU fan power", "cooling energy")),
             scales = "free_y") +
  labs(x = "Models", y = "CV(RMSE) (%)") +
  scale_x_discrete(labels=c("1", "2", "3", "4", "5","6"))+
  geom_hline(aes(yintercept=30), linetype="dashed", color="red")+
  geom_hline(aes(yintercept=-30), linetype="dashed", color="red")
# theme_classic()


ggsave(filename = here("data","revision", "V3", "calibration_level_accuracy.jpg"), 
       plot = p_accuracy_2, dpi = 300, height = 3, width = 6)



# ##############################################################################
# We use the code below for the research compendium on GitHub 

param_lv1_concept$apply_measure(
  ecm_lv1,
  OP = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         1,0.75,0.5,1,1,0.75,0.75,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,
         0.75,0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,
         0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,0.75,
         0.5,0.5,0.5,0.5,0.5,1,1,1,0.75,0.75,0.75,0.5,0.5,0.5,1,1,0.75,
         0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  win_constr = c(NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964",
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,"win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964","win_1267","win_2412","win_1761",
                 "win_2716","win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",
                 "win_1267","win_2412","win_1761","win_2716","win_3964","win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  lights  = c("Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less","T5_more","T5_less",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less",
              "T5_more","T5_less","LED_less","LED_less","LED_less","LED_less","LED_less",
              "T5_more","T5_more","T5_more","T5_more","T5_more","T5_less","T5_less","T5_less",
              "T5_less","T5_less","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3"
  ),
  CHWClgSTP  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,6,8,9,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,6,8,9,6,8,9,6,8,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA
  ),
  DaylightContrl  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  TempMultiplier  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,2,3,NA,NA,NA,NA,NA,NA,NA,NA,
                      2,2,2,2,2,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,3,2,3,2,3,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,2,2,2,2,
                      2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,3,3,3,3,3,3
  ),
  Fan  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           "Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,"Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC"
  ),
  .names = c("lv1_1","lv1_2","lv1_3","lv1_4","lv1_5","lv1_6","lv1_7","lv1_8","lv1_9",
             "lv1_10","lv1_11","lv1_12","lv1_13","lv1_14","lv1_15","lv1_16","lv1_17",
             "lv1_18","lv1_19","lv1_20","lv1_21","lv1_22","lv1_23","lv1_24","lv1_25",
             "lv1_26","lv1_27","lv1_28","lv1_29","lv1_30","lv1_31","lv1_32","lv1_33",
             "lv1_34","lv1_35","lv1_36","lv1_37","lv1_38","lv1_39","lv1_40","lv1_41",
             "lv1_42","lv1_43","lv1_44","lv1_45","lv1_46","lv1_47","lv1_48","lv1_49",
             "lv1_50","lv1_51","lv1_52","lv1_53","lv1_54","lv1_55","lv1_56","lv1_57",
             "lv1_58","lv1_59","lv1_60","lv1_61","lv1_62","lv1_63","lv1_64","lv1_65",
             "lv1_66","lv1_67","lv1_68","lv1_69","lv1_70","lv1_71","lv1_72","lv1_73",
             "lv1_74","lv1_75","lv1_76","lv1_77","lv1_78","lv1_79","lv1_80","lv1_81",
             "lv1_82","lv1_83","lv1_84","lv1_85","lv1_86","lv1_87","lv1_88","lv1_89",
             "lv1_90","lv1_91","lv1_92","lv1_93","lv1_94","lv1_95","lv1_96","lv1_97",
             "lv1_98","lv1_99","lv1_100","lv1_101","lv1_102","lv1_103","lv1_104",
             "lv1_105","lv1_106","lv1_107","lv1_108","lv1_109","lv1_110","lv1_111",
             "lv1_112","lv1_113","lv1_114","lv1_115","lv1_116","lv1_117","lv1_118",
             "lv1_119","lv1_120","lv1_121","lv1_122","lv1_123","lv1_124","lv1_125",
             "lv1_126","lv1_127","lv1_128","lv1_129","lv1_130","lv1_131","lv1_132",
             "lv1_133","lv1_134","lv1_135"
  )
)

lv1_map <- c("L1__BASE-WALL__Wall_0145","L1__BASE-WALL__Wall_2161","L1__BASE-WALL__Wall_3504",
             "L1__BASE-WALL__Wall_0525","L1__BASE-WALL__Wall_1355","L1__BASE-WIN__win_1267",
             "L1__BASE-WIN__win_2412","L1__BASE-WIN__win_1761","L1__BASE-WIN__win_2716",
             "L1__BASE-WIN__win_3964","L1__BASE-MASS__1","L1__BASE-MASS__2","L1__BASE-MASS__3",
             "L1__BASE-LI__LED_less","L1__BASE-LI__T5_more","L1__BASE-LI__T5_less",
             "L1__BASE-CHWT__6","L1__BASE-CHWT__8","L1__BASE-CHWT__9","L1__BASE-FAN__FanNEC",
             "L1__BASE-FAN__FanEC","L1__WIN-MASS__win_1267&2","L1__WIN-MASS__win_2412&2",
             "L1__WIN-MASS__win_1761&2","L1__WIN-MASS__win_2716&2","L1__WIN-MASS__win_3964&2",
             "L1__WIN-MASS__win_1267&3","L1__WIN-MASS__win_2412&3","L1__WIN-MASS__win_1761&3",
             "L1__WIN-MASS__win_2716&3","L1__WIN-MASS__win_3964&3","L1__DL-WIN__Schcmpt_Daylight&win_1267",
             "L1__DL-WIN__Schcmpt_Daylight&win_2412","L1__DL-WIN__Schcmpt_Daylight&win_1761",
             "L1__DL-WIN__Schcmpt_Daylight&win_2716","L1__DL-WIN__Schcmpt_Daylight&win_3964",
             "L1__DL-LI__Schcmpt_Daylight&LED_less","L1__DL-LI__Schcmpt_Daylight&T5_more",
             "L1__DL-LI__Schcmpt_Daylight&T5_less","L1__DL-WIN-LI__Schcmpt_Daylight&win_1267&LED_less",
             "L1__DL-WIN-LI__Schcmpt_Daylight&win_2412&LED_less","L1__DL-WIN-LI__Schcmpt_Daylight&win_1761&LED_less",
             "L1__DL-WIN-LI__Schcmpt_Daylight&win_2716&LED_less","L1__DL-WIN-LI__Schcmpt_Daylight&win_3964&LED_less",
             "L1__DL-WIN-LI__Schcmpt_Daylight&win_1267&T5_more","L1__DL-WIN-LI__Schcmpt_Daylight&win_2412&T5_more",
             "L1__DL-WIN-LI__Schcmpt_Daylight&win_1761&T5_more","L1__DL-WIN-LI__Schcmpt_Daylight&win_2716&T5_more",
             "L1__DL-WIN-LI__Schcmpt_Daylight&win_3964&T5_more","L1__DL-WIN-LI__Schcmpt_Daylight&win_1267&T5_less",
             "L1__DL-WIN-LI__Schcmpt_Daylight&win_2412&T5_less","L1__DL-WIN-LI__Schcmpt_Daylight&win_1761&T5_less",
             "L1__DL-WIN-LI__Schcmpt_Daylight&win_2716&T5_less","L1__DL-WIN-LI__Schcmpt_Daylight&win_3964&T5_less",
             "L1__MM__1","L1__MM__0.75","L1__MM__0.5","L1__MM-MASS__1&2","L1__MM-MASS__1&3","L1__MM-MASS__0.75&2",
             "L1__MM-MASS__0.75&3","L1__MM-MASS__0.5&2","L1__MM-MASS__0.5&3","L1__MM-WIN__1&win_1267",
             "L1__MM-WIN__1&win_2412","L1__MM-WIN__1&win_1761","L1__MM-WIN__1&win_2716","L1__MM-WIN__1&win_3964",
             "L1__MM-WIN__0.75&win_1267","L1__MM-WIN__0.75&win_2412","L1__MM-WIN__0.75&win_1761",
             "L1__MM-WIN__0.75&win_2716","L1__MM-WIN__0.75&win_3964","L1__MM-WIN__0.5&win_1267",
             "L1__MM-WIN__0.5&win_2412","L1__MM-WIN__0.5&win_1761","L1__MM-WIN__0.5&win_2716",
             "L1__MM-WIN__0.5&win_3964","L1__MM-WIN-MASS__1&win_1267&2","L1__MM-WIN-MASS__1&win_2412&2",
             "L1__MM-WIN-MASS__1&win_1761&2","L1__MM-WIN-MASS__1&win_2716&2","L1__MM-WIN-MASS__1&win_3964&2",
             "L1__MM-WIN-MASS__0.75&win_1267&2","L1__MM-WIN-MASS__0.75&win_2412&2","L1__MM-WIN-MASS__0.75&win_1761&2",
             "L1__MM-WIN-MASS__0.75&win_2716&2","L1__MM-WIN-MASS__0.75&win_3964&2","L1__MM-WIN-MASS__0.5&win_1267&2",
             "L1__MM-WIN-MASS__0.5&win_2412&2","L1__MM-WIN-MASS__0.5&win_1761&2","L1__MM-WIN-MASS__0.5&win_2716&2",
             "L1__MM-WIN-MASS__0.5&win_3964&2","L1__MM-WIN-MASS__1&win_1267&3","L1__MM-WIN-MASS__1&win_2412&3",
             "L1__MM-WIN-MASS__1&win_1761&3","L1__MM-WIN-MASS__1&win_2716&3","L1__MM-WIN-MASS__1&win_3964&3",
             "L1__MM-WIN-MASS__0.75&win_1267&3","L1__MM-WIN-MASS__0.75&win_2412&3","L1__MM-WIN-MASS__0.75&win_1761&3",
             "L1__MM-WIN-MASS__0.75&win_2716&3","L1__MM-WIN-MASS__0.75&win_3964&3","L1__MM-WIN-MASS__0.5&win_1267&3",
             "L1__MM-WIN-MASS__0.5&win_2412&3","L1__MM-WIN-MASS__0.5&win_1761&3","L1__MM-WIN-MASS__0.5&win_2716&3",
             "L1__MM-WIN-MASS__0.5&win_3964&3","L1__MM-CHWT__1&6","L1__MM-CHWT__1&8","L1__MM-CHWT__1&9","L1__MM-CHWT__0.75&6",
             "L1__MM-CHWT__0.75&8","L1__MM-CHWT__0.75&9","L1__MM-CHWT__0.5&6","L1__MM-CHWT__0.5&8","L1__MM-CHWT__0.5&9",
             "L1__MM-FAN__1&FanNEC","L1__MM-FAN__1&FanEC","L1__MM-FAN__0.75&FanNEC","L1__MM-FAN__0.75&FanEC","L1__MM-FAN__0.5&FanNEC",
             "L1__MM-FAN__0.5&FanEC","L1__MM-MASS-FAN__1&2&FanNEC","L1__MM-MASS-FAN__1&2&FanEC","L1__MM-MASS-FAN__0.75&2&FanNEC",
             "L1__MM-MASS-FAN__0.75&2&FanEC","L1__MM-MASS-FAN__0.5&2&FanNEC","L1__MM-MASS-FAN__0.5&2&FanEC",
             "L1__MM-MASS-FAN__1&3&FanNEC","L1__MM-MASS-FAN__1&3&FanEC","L1__MM-MASS-FAN__0.75&3&FanNEC",
             "L1__MM-MASS-FAN__0.75&3&FanEC","L1__MM-MASS-FAN__0.5&3&FanNEC","L1__MM-MASS-FAN__0.5&3&FanEC")

param_lv2_specs$apply_measure(
  ecm,
  OP = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         1,0.75,0.5,1,1,0.75,0.75,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,
         0.75,0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,
         0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,0.75,
         0.5,0.5,0.5,0.5,0.5,1,1,1,0.75,0.75,0.75,0.5,0.5,0.5,1,1,0.75,
         0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  win_constr = c(NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964",
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,"win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964","win_1267","win_2412","win_1761",
                 "win_2716","win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",
                 "win_1267","win_2412","win_1761","win_2716","win_3964","win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  lights  = c("Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less","T5_more","T5_less",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less",
              "T5_more","T5_less","LED_less","LED_less","LED_less","LED_less","LED_less",
              "T5_more","T5_more","T5_more","T5_more","T5_more","T5_less","T5_less","T5_less",
              "T5_less","T5_less","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3"
  ),
  CHWClgSTP  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,6,8,9,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,6,8,9,6,8,9,6,8,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA
  ),
  DaylightContrl  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  TempMultiplier  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,2,3,NA,NA,NA,NA,NA,NA,NA,NA,
                      2,2,2,2,2,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,3,2,3,2,3,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,2,2,2,2,
                      2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,3,3,3,3,3,3
  ),
  Fan  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           "Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,"Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC"
  ),
  .names = c("lv2_1","lv2_2","lv2_3","lv2_4","lv2_5","lv2_6","lv2_7","lv2_8","lv2_9",
             "lv2_10","lv2_11","lv2_12","lv2_13","lv2_14","lv2_15","lv2_16","lv2_17",
             "lv2_18","lv2_19","lv2_20","lv2_21","lv2_22","lv2_23","lv2_24","lv2_25",
             "lv2_26","lv2_27","lv2_28","lv2_29","lv2_30","lv2_31","lv2_32","lv2_33",
             "lv2_34","lv2_35","lv2_36","lv2_37","lv2_38","lv2_39","lv2_40","lv2_41",
             "lv2_42","lv2_43","lv2_44","lv2_45","lv2_46","lv2_47","lv2_48","lv2_49",
             "lv2_50","lv2_51","lv2_52","lv2_53","lv2_54","lv2_55","lv2_56","lv2_57",
             "lv2_58","lv2_59","lv2_60","lv2_61","lv2_62","lv2_63","lv2_64","lv2_65",
             "lv2_66","lv2_67","lv2_68","lv2_69","lv2_70","lv2_71","lv2_72","lv2_73",
             "lv2_74","lv2_75","lv2_76","lv2_77","lv2_78","lv2_79","lv2_80","lv2_81",
             "lv2_82","lv2_83","lv2_84","lv2_85","lv2_86","lv2_87","lv2_88","lv2_89",
             "lv2_90","lv2_91","lv2_92","lv2_93","lv2_94","lv2_95","lv2_96","lv2_97",
             "lv2_98","lv2_99","lv2_100","lv2_101","lv2_102","lv2_103","lv2_104",
             "lv2_105","lv2_106","lv2_107","lv2_108","lv2_109","lv2_110","lv2_111",
             "lv2_112","lv2_113","lv2_114","lv2_115","lv2_116","lv2_117","lv2_118",
             "lv2_119","lv2_120","lv2_121","lv2_122","lv2_123","lv2_124","lv2_125",
             "lv2_126","lv2_127","lv2_128","lv2_129","lv2_130","lv2_131","lv2_132",
             "lv2_133","lv2_134","lv2_135"
  )
)


param_lv3_FF$apply_measure(
  ecm,
  OP = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         1,0.75,0.5,1,1,0.75,0.75,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,
         0.75,0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,
         0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,0.75,
         0.5,0.5,0.5,0.5,0.5,1,1,1,0.75,0.75,0.75,0.5,0.5,0.5,1,1,0.75,
         0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  win_constr = c(NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964",
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,"win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964","win_1267","win_2412","win_1761",
                 "win_2716","win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",
                 "win_1267","win_2412","win_1761","win_2716","win_3964","win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  lights  = c("Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less","T5_more","T5_less",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less",
              "T5_more","T5_less","LED_less","LED_less","LED_less","LED_less","LED_less",
              "T5_more","T5_more","T5_more","T5_more","T5_more","T5_less","T5_less","T5_less",
              "T5_less","T5_less","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3"
  ),
  CHWClgSTP  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,6,8,9,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,6,8,9,6,8,9,6,8,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA
  ),
  DaylightContrl  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  TempMultiplier  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,2,3,NA,NA,NA,NA,NA,NA,NA,NA,
                      2,2,2,2,2,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,3,2,3,2,3,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,2,2,2,2,
                      2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,3,3,3,3,3,3
  ),
  Fan  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           "Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,"Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC"
  ),
  .names = c("lv3_1","lv3_2","lv3_3","lv3_4","lv3_5","lv3_6","lv3_7","lv3_8","lv3_9",
             "lv3_10","lv3_11","lv3_12","lv3_13","lv3_14","lv3_15","lv3_16","lv3_17",
             "lv3_18","lv3_19","lv3_20","lv3_21","lv3_22","lv3_23","lv3_24","lv3_25",
             "lv3_26","lv3_27","lv3_28","lv3_29","lv3_30","lv3_31","lv3_32","lv3_33",
             "lv3_34","lv3_35","lv3_36","lv3_37","lv3_38","lv3_39","lv3_40","lv3_41",
             "lv3_42","lv3_43","lv3_44","lv3_45","lv3_46","lv3_47","lv3_48","lv3_49",
             "lv3_50","lv3_51","lv3_52","lv3_53","lv3_54","lv3_55","lv3_56","lv3_57",
             "lv3_58","lv3_59","lv3_60","lv3_61","lv3_62","lv3_63","lv3_64","lv3_65",
             "lv3_66","lv3_67","lv3_68","lv3_69","lv3_70","lv3_71","lv3_72","lv3_73",
             "lv3_74","lv3_75","lv3_76","lv3_77","lv3_78","lv3_79","lv3_80","lv3_81",
             "lv3_82","lv3_83","lv3_84","lv3_85","lv3_86","lv3_87","lv3_88","lv3_89",
             "lv3_90","lv3_91","lv3_92","lv3_93","lv3_94","lv3_95","lv3_96","lv3_97",
             "lv3_98","lv3_99","lv3_100","lv3_101","lv3_102","lv3_103","lv3_104",
             "lv3_105","lv3_106","lv3_107","lv3_108","lv3_109","lv3_110","lv3_111",
             "lv3_112","lv3_113","lv3_114","lv3_115","lv3_116","lv3_117","lv3_118",
             "lv3_119","lv3_120","lv3_121","lv3_122","lv3_123","lv3_124","lv3_125",
             "lv3_126","lv3_127","lv3_128","lv3_129","lv3_130","lv3_131","lv3_132",
             "lv3_133","lv3_134","lv3_135"
  )
)


param_lv4_ACH$apply_measure(
  ecm,
  OP = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         1,0.75,0.5,1,1,0.75,0.75,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,
         0.75,0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,
         0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,0.75,
         0.5,0.5,0.5,0.5,0.5,1,1,1,0.75,0.75,0.75,0.5,0.5,0.5,1,1,0.75,
         0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  win_constr = c(NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964",
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,"win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964","win_1267","win_2412","win_1761",
                 "win_2716","win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",
                 "win_1267","win_2412","win_1761","win_2716","win_3964","win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  lights  = c("Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less","T5_more","T5_less",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less",
              "T5_more","T5_less","LED_less","LED_less","LED_less","LED_less","LED_less",
              "T5_more","T5_more","T5_more","T5_more","T5_more","T5_less","T5_less","T5_less",
              "T5_less","T5_less","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3"
  ),
  
  CHWClgSTP  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,6,8,9,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,6,8,9,6,8,9,6,8,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA
  ),
  DaylightContrl  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  TempMultiplier  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,2,3,NA,NA,NA,NA,NA,NA,NA,NA,
                      2,2,2,2,2,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,3,2,3,2,3,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,2,2,2,2,
                      2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,3,3,3,3,3,3
  ),
  Fan  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           "Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,"Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC"
  ),
  .names = c("lv4_1","lv4_2","lv4_3","lv4_4","lv4_5","lv4_6","lv4_7","lv4_8","lv4_9",
             "lv4_10","lv4_11","lv4_12","lv4_13","lv4_14","lv4_15","lv4_16","lv4_17",
             "lv4_18","lv4_19","lv4_20","lv4_21","lv4_22","lv4_23","lv4_24","lv4_25",
             "lv4_26","lv4_27","lv4_28","lv4_29","lv4_30","lv4_31","lv4_32","lv4_33",
             "lv4_34","lv4_35","lv4_36","lv4_37","lv4_38","lv4_39","lv4_40","lv4_41",
             "lv4_42","lv4_43","lv4_44","lv4_45","lv4_46","lv4_47","lv4_48","lv4_49",
             "lv4_50","lv4_51","lv4_52","lv4_53","lv4_54","lv4_55","lv4_56","lv4_57",
             "lv4_58","lv4_59","lv4_60","lv4_61","lv4_62","lv4_63","lv4_64","lv4_65",
             "lv4_66","lv4_67","lv4_68","lv4_69","lv4_70","lv4_71","lv4_72","lv4_73",
             "lv4_74","lv4_75","lv4_76","lv4_77","lv4_78","lv4_79","lv4_80","lv4_81",
             "lv4_82","lv4_83","lv4_84","lv4_85","lv4_86","lv4_87","lv4_88","lv4_89",
             "lv4_90","lv4_91","lv4_92","lv4_93","lv4_94","lv4_95","lv4_96","lv4_97",
             "lv4_98","lv4_99","lv4_100","lv4_101","lv4_102","lv4_103","lv4_104",
             "lv4_105","lv4_106","lv4_107","lv4_108","lv4_109","lv4_110","lv4_111",
             "lv4_112","lv4_113","lv4_114","lv4_115","lv4_116","lv4_117","lv4_118",
             "lv4_119","lv4_120","lv4_121","lv4_122","lv4_123","lv4_124","lv4_125",
             "lv4_126","lv4_127","lv4_128","lv4_129","lv4_130","lv4_131","lv4_132",
             "lv4_133","lv4_134","lv4_135"
  )
)


param_lv5_Fan$apply_measure(
  ecm,
  OP = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         1,0.75,0.5,1,1,0.75,0.75,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,
         0.75,0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,
         0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,0.75,
         0.5,0.5,0.5,0.5,0.5,1,1,1,0.75,0.75,0.75,0.5,0.5,0.5,1,1,0.75,
         0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  win_constr = c(NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964",
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,"win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964","win_1267","win_2412","win_1761",
                 "win_2716","win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",
                 "win_1267","win_2412","win_1761","win_2716","win_3964","win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  lights  = c("Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less","T5_more","T5_less",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less",
              "T5_more","T5_less","LED_less","LED_less","LED_less","LED_less","LED_less",
              "T5_more","T5_more","T5_more","T5_more","T5_more","T5_less","T5_less","T5_less",
              "T5_less","T5_less","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3"
  ),
  
  CHWClgSTP  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,6,8,9,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,6,8,9,6,8,9,6,8,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA
  ),
  
  DaylightContrl  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  TempMultiplier  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,2,3,NA,NA,NA,NA,NA,NA,NA,NA,
                      2,2,2,2,2,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,3,2,3,2,3,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,2,2,2,2,
                      2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,3,3,3,3,3,3
  ),
  
  Fan  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           "Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,"Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC"
  ),
  
  .names = c("lv5_1","lv5_2","lv5_3","lv5_4","lv5_5","lv5_6","lv5_7","lv5_8","lv5_9",
             "lv5_10","lv5_11","lv5_12","lv5_13","lv5_14","lv5_15","lv5_16","lv5_17",
             "lv5_18","lv5_19","lv5_20","lv5_21","lv5_22","lv5_23","lv5_24","lv5_25",
             "lv5_26","lv5_27","lv5_28","lv5_29","lv5_30","lv5_31","lv5_32","lv5_33",
             "lv5_34","lv5_35","lv5_36","lv5_37","lv5_38","lv5_39","lv5_40","lv5_41",
             "lv5_42","lv5_43","lv5_44","lv5_45","lv5_46","lv5_47","lv5_48","lv5_49",
             "lv5_50","lv5_51","lv5_52","lv5_53","lv5_54","lv5_55","lv5_56","lv5_57",
             "lv5_58","lv5_59","lv5_60","lv5_61","lv5_62","lv5_63","lv5_64","lv5_65",
             "lv5_66","lv5_67","lv5_68","lv5_69","lv5_70","lv5_71","lv5_72","lv5_73",
             "lv5_74","lv5_75","lv5_76","lv5_77","lv5_78","lv5_79","lv5_80","lv5_81",
             "lv5_82","lv5_83","lv5_84","lv5_85","lv5_86","lv5_87","lv5_88","lv5_89",
             "lv5_90","lv5_91","lv5_92","lv5_93","lv5_94","lv5_95","lv5_96","lv5_97",
             "lv5_98","lv5_99","lv5_100","lv5_101","lv5_102","lv5_103","lv5_104",
             "lv5_105","lv5_106","lv5_107","lv5_108","lv5_109","lv5_110","lv5_111",
             "lv5_112","lv5_113","lv5_114","lv5_115","lv5_116","lv5_117","lv5_118",
             "lv5_119","lv5_120","lv5_121","lv5_122","lv5_123","lv5_124","lv5_125",
             "lv5_126","lv5_127","lv5_128","lv5_129","lv5_130","lv5_131","lv5_132",
             "lv5_133","lv5_134","lv5_135"
  )
)


# NA 114 should be replaced as "Lights_Room3" so the interior lighting would not  be 0
param_lv6_ACMV$apply_measure(
  ecm,
  OP = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
         1,0.75,0.5,1,1,0.75,0.75,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,
         0.75,0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,
         0.75,0.5,0.5,0.5,0.5,0.5,1,1,1,1,1,0.75,0.75,0.75,0.75,0.75,
         0.5,0.5,0.5,0.5,0.5,1,1,1,0.75,0.75,0.75,0.5,0.5,0.5,1,1,0.75,
         0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5,1,1,0.75,0.75,0.5,0.5
  ),
  wall_constr = c("Wall_0145","Wall_2161","Wall_3504","Wall_0525","Wall_1355",
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  win_constr = c(NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964",
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,"win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,"win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964","win_1267","win_2412","win_1761",
                 "win_2716","win_3964","win_1267","win_2412","win_1761","win_2716","win_3964",
                 "win_1267","win_2412","win_1761","win_2716","win_3964","win_1267","win_2412",
                 "win_1761","win_2716","win_3964","win_1267","win_2412","win_1761","win_2716",
                 "win_3964","win_1267","win_2412","win_1761","win_2716","win_3964","win_1267",
                 "win_2412","win_1761","win_2716","win_3964",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  lights  = c("Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less","T5_more","T5_less",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","LED_less",
              "T5_more","T5_less","LED_less","LED_less","LED_less","LED_less","LED_less",
              "T5_more","T5_more","T5_more","T5_more","T5_more","T5_less","T5_less","T5_less",
              "T5_less","T5_less","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3",
              "Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3","Lights_Room3"
  ),
  CHWClgSTP  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,6,8,9,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,6,8,9,6,8,9,6,8,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA
  ),
  DaylightContrl  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight","Schcmpt_Daylight","Schcmpt_Daylight",
                      "Schcmpt_Daylight",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),
  TempMultiplier  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,2,3,NA,NA,NA,NA,NA,NA,NA,NA,
                      2,2,2,2,2,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,3,2,3,2,3,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,2,2,2,2,
                      2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,2,2,2,2,2,2,3,3,3,3,3,3
  ),
  Fan  = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           "Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,"Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC",
           "Fan_AHU2_ex_EC","Fan_AHU2_ex_NEC","Fan_AHU2_ex_EC"
  ),
  .names = c("lv6_1","lv6_2","lv6_3","lv6_4","lv6_5","lv6_6","lv6_7","lv6_8","lv6_9",
             "lv6_10","lv6_11","lv6_12","lv6_13","lv6_14","lv6_15","lv6_16","lv6_17",
             "lv6_18","lv6_19","lv6_20","lv6_21","lv6_22","lv6_23","lv6_24","lv6_25",
             "lv6_26","lv6_27","lv6_28","lv6_29","lv6_30","lv6_31","lv6_32","lv6_33",
             "lv6_34","lv6_35","lv6_36","lv6_37","lv6_38","lv6_39","lv6_40","lv6_41",
             "lv6_42","lv6_43","lv6_44","lv6_45","lv6_46","lv6_47","lv6_48","lv6_49",
             "lv6_50","lv6_51","lv6_52","lv6_53","lv6_54","lv6_55","lv6_56","lv6_57",
             "lv6_58","lv6_59","lv6_60","lv6_61","lv6_62","lv6_63","lv6_64","lv6_65",
             "lv6_66","lv6_67","lv6_68","lv6_69","lv6_70","lv6_71","lv6_72","lv6_73",
             "lv6_74","lv6_75","lv6_76","lv6_77","lv6_78","lv6_79","lv6_80","lv6_81",
             "lv6_82","lv6_83","lv6_84","lv6_85","lv6_86","lv6_87","lv6_88","lv6_89",
             "lv6_90","lv6_91","lv6_92","lv6_93","lv6_94","lv6_95","lv6_96","lv6_97",
             "lv6_98","lv6_99","lv6_100","lv6_101","lv6_102","lv6_103","lv6_104",
             "lv6_105","lv6_106","lv6_107","lv6_108","lv6_109","lv6_110","lv6_111",
             "lv6_112","lv6_113","lv6_114","lv6_115","lv6_116","lv6_117","lv6_118",
             "lv6_119","lv6_120","lv6_121","lv6_122","lv6_123","lv6_124","lv6_125",
             "lv6_126","lv6_127","lv6_128","lv6_129","lv6_130","lv6_131","lv6_132",
             "lv6_133","lv6_134","lv6_135"
  )
)











param_lv1_concept$run()
param_lv2_specs$run()
param_lv3_FF$run()
param_lv4_ACH$run()
param_lv5_Fan$run()
param_lv6_ACMV$run()



saveRDS(param_lv1_concept, "param_lv1_concept.rds")
saveRDS(param_lv2_specs, "param_lv2_specs.rds")
saveRDS(param_lv3_FF, "param_lv3_FF.rds")
saveRDS(param_lv4_ACH, "param_lv4_ACH.rds")
saveRDS(param_lv5_Fan, "param_lv5_Fan.rds")
saveRDS(param_lv6_ACMV, "param_lv6_ACMV.rds")


here("param_lv2_specs.rds")
