
# ASHRAE 2013 - Fundamentals
# Chapter F1 - Psychrometrics
# IP Units

# Rda <- universal gas constant for dry air
# Rda <- 53.350 (ft lbf)/(lbda R) --> lbf represents pound force equation 1
# Rw <- gas constant for water vapor
# Rw <- 85.780 (ft lbf)/(lbw R) -> equation 2



# Units
# t <- dry bulb temperature (fahrenheit)
# R <- dry bulb temperature (Rankine)
# tw <- wet bulb temperature (fahrenheit)
# td <- dew point temperature (fahrenheit)
# RH <- relative humidity
# p <- barometric (atmospheric) pressure (psi)
# pw <- water vapour partial pressure (psi)
# pws <- water vapour saturation pressure (psi)
# W <- humidity ratio
# Ws <- saturation humidity ratio
# hda <- enthalpy of dry air (Btu/lbda)
# hw <- enthalpy of moisture (Btu/lbw)
# h <- total enthalpy (Btu/lb)
# v <- specific volume (ft3/lbda)
# z <- altitude (ft)



# calculate barometric pressure as a function of altitude
fun_barometric_pressure_from_altitude_ip <- function(z) {
  
  # Equation 3
  p <- (14.696 * (1 - 6.8754 * 10 ^ -6 * z) ^ 5.2559)
  return(p)
}



# atmospheric dry bulb temperature as a function of z
fun_dry_bulb_temperature_as_fun_of_altitude_ip <-  function(z) {
  
  # Equation 4
  t <- 59 - 0.0035662 * z
  return(t)
  
}







fun_water_vapor_saturation_pressure_from_temperature_ip <- function(t) {
  # Vapor pressure of water (ps) in saturated moist air differs negligibly from the saturation vapor pressure (pws) of pure water at same temperature.
  # Therefore ps can be used in place of pws with very little error (Page F1.2)
  
  # water vapor saturation constants
  # constants for dry_bulb_fahrenheit <= 32
  c1 <- -1.0214165E04
  c2 <- -4.8932428E00
  c3 <- -5.3765794E-03
  c4 <- 1.9202377E-07
  c5 <- 3.5575832E-10
  c6 <- -9.0344688E-14
  c7 <- 4.1635019E00
  # constants for dry_bulb_fahrenheit > 32
  c8 <- -1.0440397E04
  c9 <- -1.129465E01
  c10 <- -2.7022355E-02
  c11 <- 1.2890360E-05
  c12 <- -2.4780681E-09
  c13 <- 6.5459673E00
  
  # R <- Rankin temperature (can't use T to match ASHRAE -> R doesn't allow T variable)
  R <- t + 459.67
  
  if (t <= 32) {
    #  Equation 5
    pws <- (exp(c1 / R + c2 + c3 * R + c4 * R ^ 2 + c5 * R ^ 3 + c6 * R ^ 4 + c7 * log(R)))
    
  } else {
    # Equation 6
    pws <- (exp(c8 / R + c9 + c10 * R + c11 * R ^ 2 + c12 * R ^ 3 + c13 * log(R)))
  }
  
  return(pws)
}






#####################################  Scenario 1 - t, tw, z ########################

# Scenario 1
# Calculate water vapor saturation pressure from dry bulb temperature and wet bulb temperature
fun_water_vapor_saturation_pressure_star_from_wet_bulb_temperature_ip <- function(t, 
                                                                                  tw, 
                                                                                  z) {
  # Equation 5 or 6 at tw
  pws <- fun_water_vapor_saturation_pressure_from_temperature_ip(tw)
  return(pws)
}



# Scenario 1
# Calculate humidity ratio of saturated moist air* from dry bulb and wet bulb (calculated at wet bulb)
fun_humidity_ratio_of_saturated_moist_air_star_from_wet_bulb_temperature_ip <- function(t, 
                                                                                        tw, 
                                                                                        z) {
  
  # Use water vapor saturation pressure star (calculated at wet bulb temperature)
  pws <- fun_water_vapor_saturation_pressure_star_from_wet_bulb_temperature_ip(t, tw, z)
  p <- fun_barometric_pressure_from_altitude_ip(z)
  
  # Equation 23
  # Ws_star <- saturation humidity ratio (lbw/lbda)
  Ws_star <- 0.621945 * (pws / (p - pws))
  return(Ws_star)
}




# Scenario 1
# Calculate humidity ratio from dry bulb and wet bulb
fun_humidity_ratio_from_wet_bulb_temperature_ip <- function(t,
                                                            tw,
                                                            z) {
  
  Ws_star <- fun_humidity_ratio_of_saturated_moist_air_star_from_wet_bulb_temperature_ip(t, tw, z)
  
  # Ws <- humidity ratio
  if (t > 32) {
    # Equation 35
    W <- ((1093 - 0.556 * tw) * Ws_star - 0.24 * (t - tw)) / (1093 + 0.444 * t - tw)
  } else {
    # Equation 37
    W <- ((1220 - 0.04 * tw) * Ws_star - 0.24 * (t - tw)) / (1220 + 0.444 * t - 0.48 * tw)
  }
  return (W)
}




# Scenario 1
# Calculate water vapor saturation presure from dry bulb and wet bulb
fun_water_vapor_saturation_pressure_from_wet_bulb_temperature_ip <- function(t,
                                                                             tw,
                                                                             z) {
  # Equation 5 or 6 at t
  pws <- fun_water_vapor_saturation_pressure_from_temperature_ip(t)
  return(pws)
}




# Scenario 1
# Calculate humidity ratio of saturated moist air from dry bulb and wet bulb
fun_humidity_ratio_of_saturated_moist_air_from_wet_bulb_temperature_ip <- function(t,
                                                                                   tw,
                                                                                   z) {
  pws <- fun_water_vapor_saturation_pressure_from_temperature_ip(t)
  p <- fun_barometric_pressure_from_altitude_ip(z)
  
  # Equation 23
  Ws <- 0.621945 * (pws / (p - pws))
  return(Ws)
}



# Scenario 1
# Calculate degree of saturation from dry bulb and wet bulb
fun_degree_of_saturation_from_wet_bulb_temperature_ip <- function(t,
                                                                  tw,
                                                                  z) {
  W <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, tw, z)
  Ws <- fun_humidity_ratio_of_saturated_moist_air_from_wet_bulb_temperature_ip(t, tw, z)
  
  # Equation 12 - ASHRAE 2013
  u <- W / Ws
  return(u)
}




# Scenario 1
# Calculate relative humidity from dry bulb and wet bulb
fun_relative_humidity_from_wet_bulb_temperature_ip <- function(t,
                                                               tw,
                                                               z) {
  
  u <- fun_degree_of_saturation_from_wet_bulb_temperature_ip(t, tw, z)
  pws <- fun_water_vapor_saturation_pressure_from_wet_bulb_temperature_ip(t, tw, z)
  p <- fun_barometric_pressure_from_altitude_ip(z)
  
  # Equation 24
  RH <- u / (1 - (1 - u) * (pws / p))
  return(RH)
}




# Scenario 1
# Calculate specific volume from dry bulb and wet bulb
fun_specific_volume_from_wet_bulb_temperature_ip <- function(t,
                                                             tw,
                                                             z) {
  
  W <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, tw, z)
  p <- fun_barometric_pressure_from_altitude_ip(z)
  
  # Equation 28
  v <- 0.370486 * (t + 459.67) * (1 + 1.607858 * W) / p
  return(v)
}



# Scenario 1
# Calculate enthalpy of dry air from dry bulb and wet bulb
fun_enthalpy_of_dry_air_from_wet_bulb_temperature_ip <- function(t,
                                                                 tw,
                                                                 z) {
  # Equation 30
  hda <- 0.24 * t
  return(hda)
}




# Scenario 1
# Calculate enthalpy of moisture from dry bulb and wet bulb
fun_enthalpy_of_moisture_from_wet_bulb_temperature_ip <- function(t,
                                                                  tw,
                                                                  z) {
  W <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, tw, z)
  
  # Equation 31
  hw <- W * (1061 + 0.444 * t)
  return(hw)
}




# scenario 1
# Calculate total enthalpy from dry bulb and wet bulb
fun_enthalpy_from_wet_bulb_temperature_ip <- function(t,
                                                      tw,
                                                      z) {
  hda <- fun_enthalpy_of_dry_air_from_wet_bulb_temperature_ip(t, tw, z)
  hw <- fun_enthalpy_of_moisture_from_wet_bulb_temperature_ip(t, tw, z)
  
  # Equation 32
  h <- hda + hw
  return(h)
}




# Scenario 1
# calculate water vapor partial pressure from dry bulb and wet bulb
fun_water_vapor_partial_pressure_from_wet_bulb_temperature_ip <- function(t,
                                                                          tw,
                                                                          z) {
  p <- fun_barometric_pressure_from_altitude_ip(z)
  W <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, tw, z)
  
  # Equation 22
  pw <- (p * W) / (0.621945 + W)
  return(pw)
}




# Scenario 1
# Calculate dew point from dry bulb and wet bulb
fun_dew_point_temperature_from_wet_bulb_temperature_ip <- function(t,
                                                                   tw,
                                                                   z) {
  # constants for dew point temperature
  c14 <- 100.45
  c15 <- 33.193
  c16 <- 2.319
  c17 <- 0.17074
  c18 <- 1.2063
  
  pw <- fun_water_vapor_partial_pressure_from_wet_bulb_temperature_ip(t, tw, z)
  
  if (t > 32) {
    # Equation 39
    td <- c14 + c15 * log(pw) + c16 * (log(pw)) ^ 2 + c17 * (log(pw))^3 + c18 * pw ^ 0.1984
  } else {
    # Equation 40
    td <- 90.12 + 26.142 * log(pw) + 0.8927 * (log(pw)) ^ 2
  }
  return(td)
}








#### Scenario 2 - t, td, z #####


# Scenario 2
# calculate water vapor partial pressure from dry bulb and dew point
fun_water_vapor_partial_pressure_from_dew_point_temperature_ip <- function(t,
                                                                           td,
                                                                           z) {
  # at dew point temperature water vapor saturation pressure is equal to water vapor partial pressure -> pw <- pws(td)
  # Equation 5 or 6 at td
  pw <- fun_water_vapor_saturation_pressure_from_temperature_ip(td)
  return(pw)
}



# Scenario 2
# Calculate humidity ratio from dry bulb and dew point
fun_humidity_ratio_from_dew_point_temperature_ip <- function(t,
                                                             td,
                                                             z) {
  p <- fun_barometric_pressure_from_altitude_ip(z)
  pw <- fun_water_vapor_partial_pressure_from_dew_point_temperature_ip(t, td, z)
  
  # Equation 22
  W <- 0.621945 * (pw / (p - pw))
  return(W)
}


# Scenario 2
# Calculate water vapor saturation pressure from dry bulb and dew point
fun_water_vapor_saturation_pressure_from_dew_point_temperature_ip <- function(t,
                                                                              td,
                                                                              z) {
  # Equation 5 or 6 at td
  pws <- fun_water_vapor_saturation_pressure_from_temperature_ip(t)
  return(pws)
}




# Scenario 2
# Calculate humidity ratio of saturated moist air from dry bulb and dew point
fun_humidity_ratio_of_saturated_moist_air_from_dew_point_temperature_ip <- function(t,
                                                                                    td,
                                                                                    z) {
  p <- fun_barometric_pressure_from_altitude_ip(z)
  pws <- fun_water_vapor_saturation_pressure_from_dew_point_temperature_ip(t,
                                                                           td,
                                                                           z)
  # Equation 23
  Ws <- 0.621945 * (pws / (p - pws))
  return(Ws)
}




# Scenario 2
# Calculate degree of degree of saturation from dry bulb and dew point
fun_degree_of_saturation_from_dew_point_temperature_ip <- function(t,
                                                                   td,
                                                                   z) {
  W <- fun_humidity_ratio_from_dew_point_temperature_ip(t, td, z)
  Ws <- fun_humidity_ratio_of_saturated_moist_air_from_dew_point_temperature_ip(t, td, z)
  
  # Equation 12
  u <- W / Ws
  return(u)
}


# Scenario 2
# Calculate relative humidity from dry bulb and dew point
fun_relative_humidity_from_dew_point_temperature_ip <- function(t,
                                                                td,
                                                                z) {
  u <- fun_degree_of_saturation_from_dew_point_temperature_ip(t, td, z)
  pws <- fun_water_vapor_saturation_pressure_from_temperature_ip(t)
  p <- fun_barometric_pressure_from_altitude_ip(z)
  
  # Equation 25
  RH <- u/ (1 - (1 - u) * (pws / p))
  return(RH)
}


# Scenario 2
# Calculate specific volume from dry bulb and dew point
fun_specific_volume_from_dew_point_temperature_ip <- function(t,
                                                              td,
                                                              z) {
  W <- fun_humidity_ratio_from_dew_point_temperature_ip(t, td, z)
  p <- fun_barometric_pressure_from_altitude_ip(z)
  
  # Equation 28
  v <- 0.370486 * (t + 459.67) * (1 + 1.607858 * W) / p
  return(v)
}


# scenario 2
# calculate enthalpy of dry air from dry bulb and dew point
fun_enthalpy_of_dry_air_from_dew_point_temperature_ip <- function(t,
                                                                  td,
                                                                  z) {
  # Equation 30
  hda <- 0.24 * t
  return(hda)
}


# Scenario 2
# Calculate enthalpy of moisture in air from dry bulb and dew point
fun_enthalpy_of_moisture_from_dew_point_temperature_ip <- function(t,
                                                                   td,
                                                                   z) {
  W <- fun_humidity_ratio_from_dew_point_temperature_ip(t, td, z)
  
  # Equation 31
  hw <- W * (1061 + 0.444 * t)
  return(hw)
}



# Scenario 2
# Calculate total enthalpy from dry bulb and dew point
fun_enthalpy_from_dew_point_temperature_ip <- function(t,
                                                       td,
                                                       z) {
  hda <- fun_enthalpy_of_dry_air_from_dew_point_temperature_ip(t, td, z)
  hw <- fun_enthalpy_of_moisture_from_dew_point_temperature_ip(t, td, z)
  
  # Equation 32
  h <- hda + hw
  return(h)
}


# Scenario 2
# Calculate wet bulb temperature from dry bulb and dew point using newton raphson iteration
# see -> https://github.com/remcmurry/Psychropy/blob/master/psychropy.py

fun_wet_bulb_temperature_from_dew_point_temperature_ip <- function(t,
                                                                   td,
                                                                   z) {
  humidity_ratio_normal <- fun_humidity_ratio_from_dew_point_temperature_ip(t, td, z)
  result <- t
  
  # pull from scenario 1 equations
  humidity_ratio_new <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, td, z)
  
  while (abs((humidity_ratio_new - humidity_ratio_normal) / humidity_ratio_normal) > 0.001) {
    humidity_ratio_new2 <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, result - 0.001, z)
    dhumidity_ratio_wrt_dew_point_temperature_ip <- (humidity_ratio_new - humidity_ratio_new2) / 0.001
    result <- result - (humidity_ratio_new - humidity_ratio_normal) / dhumidity_ratio_wrt_dew_point_temperature_ip
    humidity_ratio_new <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, result, z)
  }
  tw <- result
  return(tw)
}






#### Scenario 3 -> t, RH, z ####


# Scenario 3
# Calculate water vapor saturation pressure from dry bulb and relative humidity
fun_water_vapor_saturation_pressure_from_relative_humidity_ip <- function(t, 
                                                                          RH, 
                                                                          z) {
  # Equation 5 or 6
  pws <- fun_water_vapor_saturation_pressure_from_temperature_ip(t)
  return(pws)
}


# Scenario 3
# calculate water vapor partial pressure from dry bulb and relative humidity
fun_water_vapor_partial_pressure_from_relative_humidity_ip <- function(t,
                                                                       RH,
                                                                       z) {
  pws <- fun_water_vapor_saturation_pressure_from_relative_humidity_ip(t, RH, z)
  
  # Equation 24
  pw <- RH * pws
  return(pw)
}


# Scenario 3
# Calculate humidity ratio from relative humidity
fun_humidity_ratio_from_relative_humidity_ip <- function(t, 
                                                         RH,
                                                         z) {
  p <- fun_barometric_pressure_from_altitude_ip(z)
  pw <- fun_water_vapor_partial_pressure_from_relative_humidity_ip(t, RH, z)
  
  # Equation 22
  W <- 0.621945 * (pw / (p - pw))
  return(W)
}



# Scenario 3
# Calculate humidity ratio of saturated moist air from dry bulb and relative humidity
fun_humidity_ratio_of_saturated_moist_air_from_relative_humidity_ip <- function(t, 
                                                                                RH,
                                                                                z) {
  p <- fun_barometric_pressure_from_altitude_ip(z)
  pws <- fun_water_vapor_saturation_pressure_from_relative_humidity_ip(t)
  
  # Equation 23
  Ws <- 0.621945 * (pws / (p - pws))
  return(Ws)
}



# Scenario 3
# Calculate degree of saturation from dry bulb and relative humidity
fun_degree_of_saturation_from_relative_humidity_ip <- function(t, 
                                                               RH, 
                                                               z) {
  W <- fun_humidity_ratio_from_relative_humidity_ip(t, RH, z)
  Ws <- fun_humidity_ratio_of_saturated_moist_air_from_relative_humidity_ip(t, RH, z)
  
  # Equation 12
  u <- W / Ws
  return(u)
}


# Scenario 3
# Calculate specific volume from dry bulb and relative humidity
fun_specific_volume_from_relative_humidity_ip <- function(t, 
                                                          RH,
                                                          z) {
  W <- fun_humidity_ratio_from_relative_humidity_ip(t, RH, z)
  p <- fun_barometric_pressure_from_altitude_ip(z)
  
  # Equation 28
  v <- 0.370486 * (t + 459.67) * (1 + 1.607858 * W) / p
  return(v)
}




# Scenario 3
# Calculate enthalpy of dry air from dry bulb and relative humidity
fun_enthalpy_of_dry_air_from_relative_humidity_ip <- function(t,
                                                              RH,
                                                              z) {
  # Page F1.13 - Equation 30
  hda <- 0.24 * t
  return(hda)
}




# Scenario 3
# Calculate enthalpy of moisture from dry bulb and relative humidity
fun_enthalpy_of_moisture_from_relative_humidity_ip <- function(t,
                                                               RH,
                                                               z) {
  W <- fun_humidity_ratio_from_relative_humidity_ip(t, RH, z)
  
  # Equation 31
  hw <- W * (1061 + 0.444 * t)
  return(hw)
}



# Scenario 3
# Calculate total enthalpy from dry bulb and relative humidity
fun_enthalpy_from_relative_humidity_ip <- function(t,
                                                RH,
                                                z) {
  
  hda <- fun_enthalpy_of_dry_air_from_relative_humidity_ip(t, RH, z)
  hw <- fun_enthalpy_of_moisture_from_relative_humidity_ip(t, RH, z)
  # Equation 32
  h <- hda + hw
  return(h)
}




# Scenario 3
# Calculate dew point from dry bulb and relative humidity
fun_dew_point_temperature_from_relative_humidity_ip <- function(t,
                                                                RH,
                                                                z) {
  
  pw <- fun_water_vapor_partial_pressure_from_relative_humidity_ip(t, RH, z)
  
  # constants for dew point temperature
  c14 <- 100.45
  c15 <- 33.193
  c16 <- 2.319
  c17 <- 0.17074
  c18 <- 1.2063
  
  if (t > 32) {
    # Equation 39
    td <- c14 + c15 * log(pw) + c16 * (log(pw)) ^ 2 + c17 * (log(pw))^3 + c18 * pw ^ 0.1984
  } else {
    # Equation 40
    td <- 90.12 + 26.142 * log(pw) + 0.8927 * (log(pw)) ^ 2
  }
  return(td)
}




# Scenario 3
# Calculate wet bulb through iteration from dry bulb and relative humidity using newton raphson iteration
# https://github.com/remcmurry/Psychropy/blob/master/psychropy.py

fun_wet_bulb_temperature_from_relative_humidity_ip <- function(t,
                                                               RH,
                                                               z) {
  
  humidity_ratio_normal <- fun_humidity_ratio_from_relative_humidity_ip(t, RH, z)
  result <- t
  
  # pull from scenario 1 equations
  humidity_ratio_new <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, result, z)
  
  while (abs((humidity_ratio_new - humidity_ratio_normal) / humidity_ratio_normal) > 0.001) {
    humidity_ratio_new2 <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, result - 0.001, z)
    dhumidity_ratio_wrt_wet_bulb_temperature <- (humidity_ratio_new - humidity_ratio_new2) / 0.001
    result <- result - (humidity_ratio_new - humidity_ratio_normal) / dhumidity_ratio_wrt_wet_bulb_temperature
    humidity_ratio_new <- fun_humidity_ratio_from_wet_bulb_temperature_ip(t, result, z)
  }
  tw <- result
  return(tw)
}









#### Get RH from t, h, z ####

# Step 1
# Calculate humidity ratio from enthalpy
fun_humidity_ratio_from_enthalpy_ip <- function(t, 
                                                h, 
                                                z) {
  # Equation 32
  W <- (h - 0.24 * t) / (1061 + 0.444 * t)
  return(W)
}


# Step 2
# Calculate RH from humidity ratio
fun_relative_humidity_from_humidity_ratio_ip <- function(t,
                                                         W, 
                                                         z) {
  pws <- fun_water_vapor_saturation_pressure_from_temperature_ip(t)
  p <- fun_barometric_pressure_from_altitude_ip(z)
  
  # Equation 23
  Ws <- 0.621945 * pws / (p - pws)
  
  # Equation 12
  u <- W / Ws
  
  # Equation 25
  RH <- u / (1 - (1 - u) * (pws / p))
  
  return(RH)
}





