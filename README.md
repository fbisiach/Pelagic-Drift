# CoreDrift: Estimate Corer Drift Using NOAA Subsurface Float Data

**PelagicDrift** is an R-based tool designed to estimate the drift of a coring device through the water column during deployment, caused by ocean currents. It uses NOAA's subsurface float trajectory data to approximate the displacement between the deployment location at the surface and the final location of the sediment sample on the seafloor.

> ⚠️ **Note:** This tool uses a trimmed NOAA subsurface float database focused primarily on the **Atlantic Ocean**. For data coverage, refer to [this map](https://www.aoml.noaa.gov/phod/float_traj/files/float_traj.jpg).

---

## Features

- Estimates lateral drift during core deployment.
- Visualizes corer drift trajectory based on user input.
- Useful for sediment provenance studies and accurate sample location estimation.

---

## Requirements

- R (version ≥ 4.0)
- Packages: `ggplot2`, `dplyr`, `geosphere`, `maps`, `scales`, `lubridate`

---

## Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/your-username/CoreDrift.git
2. Load the Environment
Open R or RStudio, set your working directory to where the repository is located, and load the pre-compiled database and function:

setwd("path/to/CoreDrift")
load("Database&Function.RData")
Usage
Define the core deployment parameters in R:

# Core deployment info
CoreLatitude <- 43.010478     # Deployment latitude
CoreLongitude <- -60.211776   # Deployment longitude
Depth <- 2306                 # Water depth in meters
CoreLoweringSpeed <- 1        # Average lowering speed in m/s

# Define search window for float data (+/- degrees around deployment)
LonLowRange <- 1
LonHighRange <- 1 
LatLowRange <- 1
LatHighRange <- 1

# Run the function
CoreDrift(
  CoreLon = CoreLongitude,
  CoreLat = CoreLatitude,
  Depth = Depth,
  Speed = CoreLoweringSpeed,
  LatLowRange = LatLowRange,
  LatHighRange = LatHighRange,
  LonLowRange = LonLowRange,
  LonHighRange = LonHighRange
)
Output
Estimated drift distance and direction.

Drift trajectory plotted on a map.

Output files saved to the current working directory.

Notes
The lowering speed is assumed to be constant and vertical (no tether angle).

This is an approximation based on regional float trajectories — results should be interpreted with caution.

Credits
Subsurface float data sourced from NOAA AOML

Developed by Your Name

License
MIT License. See LICENSE file for details.
