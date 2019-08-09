
library(data.table)
library(magrittr)
options(scipen=99999999)
source("psychrometrics-imperial.R")
inhg_per_psi <- 2.03602


# 14.696 psi = 29.9 inHg
# 1 psi = 2.03602 inHg

# summer design conditions @ 0ft -> based off http://www.sugartech.co.za/psychro/index.php
# dry bulb = 93F
# wet bulb = 75F
# enthalpy = 38.3376 btu/lb
# humidity ratio = 0.014531 lbs water / lbs dry air
# specific volume = 14.25891 ft3 / lb
# relative humidity = 43.7%
# dew point = 67.7F
# atmospheric pressure = 29.921 / 2.03602
# saturated vapour pressure = 1.5624440099 / 2.03602
# partial vapor pressure = 0.683088747 / 2.03602

t <- 93
tw <- 75
td <- 67.7
rh <- 0.437192
z <- 0

summer_test <- data.table(variable = c("Dry bulb",
                                       "Wet bulb",
                                       "Dew point",
                                       "Relative humidity",
                                       "Enthalpy",
                                       "Humidity ratio",
                                       "Specific volume",
                                       "Atmospheric pressure",
                                       "Saturated vapour pressure",
                                       "Partial vapour pressure")) %>% 
  .[, season := "Summer"] %>% 
  .[, control := c(t, tw, td, rh, 38.33835, 0.0145318588, 14.2528, 29.921 / 2.03602, 1.562444 / 2.03602, 0.683088747 / 2.03602)] %>% 
  .[, wet_bulb := c(t,
                    tw,
                    fun_dew_point_temperature_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_relative_humidity_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_enthalpy_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_humidity_ratio_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_specific_volume_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_barometric_pressure_from_altitude_ip(0),
                    fun_water_vapor_saturation_pressure_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_water_vapor_partial_pressure_from_wet_bulb_temperature_ip(t, tw, 0))] %>% 
  .[, wet_bulb_percent_error := 100 * (wet_bulb / control - 1)] %>% 
  .[, dew_point := c(t,
                     fun_wet_bulb_temperature_from_dew_point_temperature_ip(t, td, 0),
                     td,
                     fun_relative_humidity_from_dew_point_temperature_ip(t, td, 0),
                     fun_enthalpy_from_dew_point_temperature_ip(t, td, 0),
                     fun_humidity_ratio_from_dew_point_temperature_ip(t, td, 0),
                     fun_specific_volume_from_dew_point_temperature_ip(t, td, 0),
                     fun_barometric_pressure_from_altitude_ip(0),
                     fun_water_vapor_saturation_pressure_from_dew_point_temperature_ip(t, td, 0),
                     fun_water_vapor_partial_pressure_from_dew_point_temperature_ip(t, td, 0))] %>% 
  .[, dew_point_percent_error := 100 * (dew_point / control - 1)] %>% 
  .[, rh := c(t,
              fun_wet_bulb_temperature_from_relative_humidity_ip(t, rh, 0),
              fun_dew_point_temperature_from_relative_humidity_ip(t, rh, 0),
              rh,
              fun_enthalpy_from_relative_humidity_ip(t, rh, 0),
              fun_humidity_ratio_from_relative_humidity_ip(t, rh, 0),
              fun_specific_volume_from_relative_humidity_ip(t, rh, 0),
              fun_barometric_pressure_from_altitude_ip(0),
              fun_water_vapor_saturation_pressure_from_relative_humidity_ip(t, rh, 0),
              fun_water_vapor_partial_pressure_from_relative_humidity_ip(t, rh, 0))] %>% 
  .[, rh_percent_error := 100 * (rh / control - 1)]




# winter design conditions @ 0ft -> based of http://www.sugartech.co.za/psychro/index.php
# dry bulb = -10F
# wet bulb = -10.91F
# enthalpy = -2.1578 btu/lb
# humidity ratio = 0.0002292 lbs water / lbs dry air
# specific volume = 11.33698 ft3 / lb
# relative humidity = 50.0%
# dew point = -21.997F
# atmospheric pressure = 29.921 / 2.03602
# saturated vapour pressure = 0.022044 / 2.03602
# partial vapour pressure = 0.0110221362 / 2.03602

t <- -10
tw <- -10.91064557
td <- -21.99719943
rh <- 0.5
z <- 0

winter_test <- data.table(variable = c("Dry bulb",
                                       "Wet bulb",
                                       "Dew point",
                                       "Relative humidity",
                                       "Enthalpy",
                                       "Humidity ratio",
                                       "Specific volume",
                                       "Atmospheric pressure",
                                       "Saturated vapour pressure",
                                       "Partial vapour pressure"),
                          control = c(t,
                                      tw,
                                      td,
                                      rh,
                                      -2.157822253,
                                      0.0002292134,
                                      11.336981563,
                                      29.921 / 2.03602,
                                      0.022044272 / 2.03602,
                                      0.0110221362 / 2.03602)) %>% 
  .[, season := "Winter"] %>% 
  .[, wet_bulb := c(t,
                    tw,
                    fun_dew_point_temperature_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_relative_humidity_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_enthalpy_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_humidity_ratio_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_specific_volume_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_barometric_pressure_from_altitude_ip(0),
                    fun_water_vapor_saturation_pressure_from_wet_bulb_temperature_ip(t, tw, 0),
                    fun_water_vapor_partial_pressure_from_wet_bulb_temperature_ip(t, tw, 0))] %>% 
  .[, wet_bulb_percent_error := 100 * (wet_bulb / control - 1)] %>% 
  .[, dew_point := c(t,
                     fun_wet_bulb_temperature_from_dew_point_temperature_ip(t, td, 0),
                     td,
                     fun_relative_humidity_from_dew_point_temperature_ip(t, td, 0),
                     fun_enthalpy_from_dew_point_temperature_ip(t, td, 0),
                     fun_humidity_ratio_from_dew_point_temperature_ip(t, td, 0),
                     fun_specific_volume_from_dew_point_temperature_ip(t, td, 0),
                     fun_barometric_pressure_from_altitude_ip(0),
                     fun_water_vapor_saturation_pressure_from_dew_point_temperature_ip(t, td, 0),
                     fun_water_vapor_partial_pressure_from_dew_point_temperature_ip(t, td, 0))] %>% 
  .[, dew_point_percent_error := 100 * (dew_point / control - 1)] %>% 
  .[, rh := c(t,
              fun_wet_bulb_temperature_from_relative_humidity_ip(t, rh, 0),
              fun_dew_point_temperature_from_relative_humidity_ip(t, rh, 0),
              rh,
              fun_enthalpy_from_relative_humidity_ip(t, rh, 0),
              fun_humidity_ratio_from_relative_humidity_ip(t, rh, 0),
              fun_specific_volume_from_relative_humidity_ip(t, rh, 0),
              fun_barometric_pressure_from_altitude_ip(0),
              fun_water_vapor_saturation_pressure_from_relative_humidity_ip(t, rh, 0),
              fun_water_vapor_partial_pressure_from_relative_humidity_ip(t, rh, 0))] %>% 
  .[, rh_percent_error := 100 * (rh / control - 1)]


test <- rbindlist(list(summer_test, winter_test),
                  use.names = TRUE,
                  fill = TRUE) %>% 
  .[, units := c("Fahrenheit", "Fahrenheit", "Fahrenheit", "", "Btu/lb", "lbs water/lbs air", "ft3/lb", "psi", "psi", "psi",
                 "Fahrenheit", "Fahrenheit", "Fahrenheit", "", "Btu/lb", "lbs water/lbs air", "ft3/lb", "psi", "psi", "psi")] %>% 
  setcolorder(c("season", "variable", "units", "control", "wet_bulb", "wet_bulb_percent_error", "dew_point", "dew_point_percent_error", "rh", "rh_percent_error"))