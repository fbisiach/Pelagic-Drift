## This tool estimates the currents-induced drift of the coring device as it gets deployed through the water column 
## and the approximate coordinates of the sediment sample. 
## Make sure to load into the environment the  file which contains the function and database
## This database is not a global comprehensive database. Most of its data concentrates in the Atlantic. 
## To check covered extents see https://www.aoml.noaa.gov/phod/float_traj/files/float_traj.jpg

# Set Work Directory. All output plots will be saved here. 
# This directory should contain the .RData file containing the trimmed NOAA Subsurface Float database and the drift estimation function. 
setwd('C:/Users/...')

load('CoreSamplingEstimationTool.RData')

# Define coring deployment info
CoreLatitude <- 43.010478  # Latitude of Core deployment
CoreLongitude <- -60.211776 # Longitude of Core deployment
Depth <-  2306 # Water depth
CoreLoweringSpeed <- 1 # Average lowering speed of the Core in m/s

# Set below the geographical extent in which currents data will be queried. Units are degrees of distance from the core deployment coordinates. 
# e.g. values of 1  will select data within +-1 degree from core deployment coordinates
LonLowRange <- 1
LonHighRange <- 1 
LatLowRange <- 1
LatHighRange <- 1

#Execute function with user-defined values
CoreDrift(CoreLon = CoreLongitude, CoreLat = CoreLatitude, Depth = Depth, 
          Speed = CoreLoweringSpeed, LatLowRange = LatLowRange, LatHighRange = LatHighRange,
          LonLowRange = LonLowRange, LonHighRange = LonHighRange)

