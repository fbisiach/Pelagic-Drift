CoreDrift <- function(CoreLon, CoreLat, Depth, 
                      Speed, LatLowRange, LatHighRange,
                      LonLowRange, LonHighRange) {
  
  #Suppress all warnings for the execution of the function, while reinstating them at the end 
  
  list.of.packages <- c("ggplot2", 'R.matlab', 'tidyverse', 'patchwork', 'ggpubr', 'MASS', 'reshape2', 'raster')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  library(R.matlab)
  library(tidyverse)
  library(patchwork)
  library(ggpubr)
  library(MASS)
  library(reshape2)
  library(raster)
  
  theme_set(theme_classic())
  
  #Define constants
  r_earth = 6.378e6
  
  # Remove datapoints where velocity is NA, filter for only datapoints within selected extent
  SubFloat.df <- SubFloat.df %>% filter(!is.na(vn)) %>% filter(!is.na(ve)) %>%
    filter(Longitude <= CoreLon+LonHighRange & Longitude >= CoreLon-LonLowRange) %>%
    filter(Latitude <= CoreLat+LatHighRange & Latitude >= CoreLat-LatLowRange)
  
  #Convert date to a readable format
  formatted_date <- as.POSIXct(SubFloat.df$Date, origin = "1970-01-01", tz = "UTC")
  SubFloat.df$Date <-  formatted_date
  
  #Convert pressure to approximate depth assuming 1 dB = 1 m. Then select only depth equal or shallower than the user inputted depth of site
  SubFloat.df <- add_column(SubFloat.df, 'Approx.Depth' = round(SubFloat.df$Pressure, digits = -2)) %>%
    filter(Approx.Depth <= Depth)
  
  
  #Add a season column
  SubFloat.df <- SubFloat.df %>%
    mutate(
      Season = case_when(
        format(Date, "%m-%d") >= "06-21" & format(Date, "%m-%d") < "09-21" ~ "Summer",
        format(Date, "%m-%d") >= "09-21" & format(Date, "%m-%d") < "12-21" ~ "Fall",
        format(Date, "%m-%d") >= "12-21" | format(Date, "%m-%d") < "03-21" ~ "Winter",
        format(Date, "%m-%d") >= "03-21" & format(Date, "%m-%d") < "06-21" ~ "Spring",
        TRUE ~ NA_character_  # Default case (if needed)
      )
    )
  
  repeat {
    season_input <- readline("Select a season (Fall, Winter, Spring, Summer, or All): ")
    
    # Convert input to title case for consistency
    season_input <- tolower(season_input)
    
    if (!(season_input %in% c("fall", "winter", "spring", "summer", "all"))) {
      message("Invalid input. Please enter one of: Fall, Winter, Spring, Summer, or All.")
    } else {
      break
    }
  }
  
  # Filter dataframe based on input
  if (season_input == "all") {
    message('Selecting all seasons...')
  } else {
    SubFloat.df <- filter(SubFloat.df, Season == tools::toTitleCase(season_input))
  }
  
  # Check how many datapoints are selected, if less than 50 is pauses and asks user to continue or restart
  if (nrow(SubFloat.df) < 50) {
    repeat {
      response <- readline(paste("Only", nrow(SubFloat.df), 'datapoints are selected using this extent and season(s). Do you want to continue? (Y/N):'))
      response <- toupper(trimws(response))  # Normalize input
      
      if (response == "Y") {
        message("Continuing...")
        break  # Exit loop and continue function execution
      } else if (response == "N") {
        options(warn = oldw)
        stop("Process stopped by user. You may increase extent or include all seasons to select more datapoints")
      } else {
        message("Invalid input. Please enter Y or N.")
      }
    }
  }
  
  # Print to user info about the queried data
  print(paste(nrow(SubFloat.df), 'datapoints have been selected'))
  print(paste('from', length(unique(SubFloat.df$Approx.Depth)), 'depths ranging from', min(SubFloat.df$Approx.Depth, na.rm = T), 'meters and', max(SubFloat.df$Approx.Depth,na.rm = T), 'meters'))
  print(paste('and from', length(unique(SubFloat.df$Season)), 'seasons'))
  
  
  #Calculations and their addition to the dataframe
  CoreDisplacementX <- SubFloat.df$ve * (Depth/Speed) 
  CoreDisplacementY <- SubFloat.df$vn * (Depth/Speed)
  
  BotCoreLat <- CoreLat  + (CoreDisplacementY / r_earth) * (180 / pi)
  BotCoreLon <- CoreLon  + (CoreDisplacementX / r_earth) * (180 / pi)/cos(CoreLat*pi/180)
  
  SubFloat.df <- SubFloat.df %>% add_column(CoreDx = CoreDisplacementX, CoreDy = CoreDisplacementY, BotCoreLat = BotCoreLat, BotCoreLon = BotCoreLon)  
  
  
  #Calculate displacement adjusted by currents variation with depth (useful if currents change significantly with depth)
  a <- SubFloat.df %>%
    group_by(Approx.Depth) %>%
    summarise(Avg_ve = mean(ve), Avg_vn = mean(vn))
  
  x <- as.numeric(a[1,'Avg_ve'] * ((a[1,'Approx.Depth']+(a[2,'Approx.Depth'] - a[1,'Approx.Depth'])*0.5)/Speed)) 
  y <- as.numeric(a[1,'Avg_vn'] * ((a[1,'Approx.Depth']+(a[2,'Approx.Depth'] - a[1,'Approx.Depth'])*0.5)/Speed)) 
  
  discTocont <- function(i){
    return((a[i+1,'Approx.Depth'] - a[i,'Approx.Depth'])*0.5 + (a[i,'Approx.Depth'] - a[i-1,'Approx.Depth'])*0.5)
  }  
  
  for (i in c(2:(nrow(a)-1))) {
    x <- x + (a[i,'Avg_ve'] * (discTocont(i)/Speed)) 
    y <- y + (a[i,'Avg_vn'] * (discTocont(i)/Speed))
  }
  
  x <- x + as.numeric((a[nrow(a),'Avg_ve'] * (((a[nrow(a),'Approx.Depth'] - a[nrow(a)-1,'Approx.Depth'])*0.5)/Speed))) 
  y <- y + as.numeric((a[nrow(a),'Avg_vn'] * (((a[nrow(a),'Approx.Depth'] - a[nrow(a)-1,'Approx.Depth'])*0.5)/Speed)))
  
  DepAdjLat <- as.numeric(CoreLat  + (y / r_earth) * (180 / pi))
  DepAdjLon <- as.numeric(CoreLon  + (x / r_earth) * (180 / pi)/cos(CoreLat*pi/180))
  
  
  # Add columns stating where each subsurface float measurement was taken with respect to the core deployment coordinates
  NS <- c(rep('North', nrow(SubFloat.df)))
  
  for (i in c(1:nrow(SubFloat.df))) {
    if (SubFloat.df[i,'Latitude'] < CoreLat) {
      NS[i] = 'South'
    }
  }
  
  WE <- c(rep('East', nrow(SubFloat.df)))
  
  for (i in c(1:nrow(SubFloat.df))) {
    if (SubFloat.df[i,'Longitude'] < CoreLon) {
      WE[i] = 'West'
    }
  }
  
  SubFloat.df <- cbind(SubFloat.df, NS, WE)
  
  ### Plot ve-vn variance
  
  # Define y-axis limits based on the range of ve and vn
  y_limits <- range(c(SubFloat.df$ve, SubFloat.df$vn), na.rm = TRUE)
  
  # Calculate count per Season
  count_data <- SubFloat.df %>%
    group_by(Season) %>%
    summarise(n = n())
  
  ve <- ggplot(SubFloat.df, mapping = aes(Season,ve))+
    geom_boxplot() + 
    geom_text(data = count_data, aes(x = Season, y = 0.9*max(y_limits), label = paste('n=', n)), size = 3,vjust = 0, color = "black") +
    geom_hline(yintercept = mean(SubFloat.df$ve), linetype = 2)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = ".all.", hide.ns = T)+
    labs(title = "ve by Season", y = "Velocity (m/s)", x = "Season") +
    ylim(y_limits)
  
  vn <- ggplot(SubFloat.df, mapping = aes(Season,vn))+
    geom_boxplot() + 
    geom_text(data = count_data, aes(x = Season, y = 0.9*max(y_limits), label = paste('n=', n)),size = 3, vjust = 0, color = "black") +
    geom_hline(yintercept = mean(SubFloat.df$vn), linetype = 2)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = ".all.", hide.ns = T)+ 
    labs(title = "vn by Season", y = "Velocity (m/s)", x = "Season") +
    ylim(y_limits)
  
  v.season <- ve+vn
  
  ggsave('Seasonal Variation Currents Velocity.png', v.season, width = 7, height = 4)
  
  ##
  
  # Convert Approx.Dep to a factor with levels in increasing order
  SubFloat.df <- SubFloat.df %>%
    mutate(Approx.Depth = factor(Approx.Depth, levels = sort(unique(Approx.Depth))))
  
  # Calculate count per Depth
  count_data <- SubFloat.df %>%
    group_by(Approx.Depth) %>%
    summarise(n = n())
  
  ve <- ggplot(SubFloat.df, mapping = aes(Approx.Depth,ve))+
    geom_boxplot() + 
    geom_text(data = count_data, aes(x = Approx.Depth, y = 0.9*max(y_limits), label = paste('n=', n)),size = 3, vjust = 0, color = "black")+
    geom_hline(yintercept = mean(SubFloat.df$ve), linetype = 2)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = ".all.", hide.ns = T)+
    labs(title = "ve by Depth", y = "Velocity (m/s)", x = "Approximate Depth (msbf)") +
    ylim(y_limits)
  
  vn <- ggplot(SubFloat.df, mapping = aes(Approx.Depth,vn))+
    geom_boxplot() + 
    geom_text(data = count_data, aes(x = Approx.Depth, y = 0.9*max(y_limits), label = paste('n=', n)),size = 3, vjust = 0, color = "black")+
    geom_hline(yintercept = mean(SubFloat.df$vn), linetype = 2)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = ".all.", hide.ns = T)+
    labs(title = "vn by Depth", y = "Velocity (m/s)", x = "Approximate Depth (msbf)") +
    ylim(y_limits)
  
  v.depth <- ve+vn
  
  ggsave('Depth Variation Currents Velocity.png', v.depth, width = 8, height = 4)
  
  
  ##
  
  vn1 <- ggplot(SubFloat.df, mapping = aes(WE,ve))+
    geom_boxplot() + facet_wrap(~NS) +
    geom_hline(yintercept = mean(SubFloat.df$ve), linetype = 2)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = ".all.", hide.ns = F)+
    labs(title = "ve by Position Relative to Corer Deployment", y = "Velocity (m/s)", x = "Location Relative to Corer", 
         caption = "Sanity Check: Do currents orientation/magnitude change singnificantly within the chosen geographical extent? If yes then extent should be reduced.") +
    ylim(y_limits) + theme(plot.caption = element_text(hjust = -0.2, colour = 'red'))
  
  vn1
  
  vn2 <- ggplot(SubFloat.df, mapping = aes(WE,vn))+
    geom_boxplot() + facet_wrap(~NS) +
    geom_hline(yintercept = mean(SubFloat.df$vn), linetype = 2)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = ".all.", hide.ns = F)+
    labs(title = "vn by Position Relative to Corer Deployment", y = "Velocity (m/s)", x = "Location Relative to Corer") +
    ylim(y_limits)
  
  v.extent <- vn1 + vn2
  
  ggsave('Currents Variation within Extent.png', v.extent, width = 10, height = 5)
  
  ##
  a <- ggplot(data=SubFloat.df, aes(x=CoreDx)) + stat_density(aes(fill=Season),position="identity",alpha=0.5) + # geom_histogram(aes(fill = Season), line = 'black', bins = 15, position = 'dodge') +
    labs(title = "Latitudinal Core Displacement Across Water Column", x = "Displacement (m)",
         caption = "Sanity Check: Are the distributions unimodal? If not check for variance across season, depth, extent") +
    theme(plot.caption = element_text(hjust = -0.4, colour = 'red'))
  b <- ggplot(data=SubFloat.df, aes(x=CoreDy)) + stat_density(aes(fill=Season),position="identity",alpha=0.5) + #+ geom_histogram(aes(fill = Season), line = 'black', bins = 15, position = 'dodge') +
    labs(title = "Latitudinal Core Displacement Across Water Column", x = "Displacement (m)")
  displ <- a+b
  
  ggsave('Core Displacement Distribution.png', displ, width = 10, height = 5)
  
  # Plot density gradient dotplot
  mean_lon <- mean(SubFloat.df$BotCoreLon)
  mean_lat <- mean(SubFloat.df$BotCoreLat)
  
  print(paste('Mean corer longitudinal displacement is', mean(CoreDisplacementX), 'meters and mean corer latitudinal displacement is', mean(CoreDisplacementY), 'meters'))
  print(paste('Mean corer seafloor landing coordinates are', mean_lon, mean_lat))
  print(paste('When adjusting for currents variation with depth, mean corer longitudinal displacement is', x, 
              'meters, mean corer latitudinal displacement is', y, 'meters'))
  print(paste('and mean corer seafloor landing coordinates are', DepAdjLon, DepAdjLat))
  
  
  xlims <- c(min(BotCoreLon),max(BotCoreLon))
  ylims <- c(min(BotCoreLat),max(BotCoreLat))
  
  g <- ggplot(SubFloat.df, aes(BotCoreLon,BotCoreLat)) + geom_point(color = "darkblue", alpha=0.5)  +     
    geom_density_2d_filled(aes(x = BotCoreLon, y = BotCoreLat), alpha = 0.8) + scale_fill_brewer(palette = "Blues") + 
    geom_segment(
      aes(x = CoreLon, y = CoreLat, xend = mean_lon, yend = mean_lat),  
      arrow = arrow(angle = 35,length = unit(0.12, "inches"), type = "open"),  
      color = "red",
      size = 0.9,
    ) +
    geom_segment(
      aes(x = CoreLon, y = CoreLat, xend = DepAdjLon, yend = DepAdjLat),  
      arrow = arrow(angle = 35,length = unit(0.12, "inches"), type = "open"),  
      color = "green4",
      size = 0.9,
    ) +
    geom_point(aes(x = CoreLon, y = CoreLat), 
               color = "black",  # Border color
               fill = "red",      # Inside color
               size = 3, 
               shape = 24,        # Shape 21 allows fill and color distinction
               stroke = 1) +
    geom_text(aes(x=Inf, y=-Inf, label = paste('Mean Sampling Coords:', mean_lon, ', ', mean_lat)),size = 3, hjust = 1.1, vjust = -1, color = 'red') +
    xlab('Longitude') + ylab('Latitude') + xlim(xlims) + ylim(ylims) +
    theme(panel.background = element_rect(fill = "white", color = 'black'), panel.grid = element_blank(), axis.ticks = element_line(), legend.position = 'none')
  
  ggsave('DensityPlot.png', g, width = 5*(max(SubFloat.df$BotCoreLon)-min(SubFloat.df$BotCoreLon))/(max(SubFloat.df$BotCoreLat)-min(SubFloat.df$BotCoreLat)), height = 5)
  
  ##
  
  # create data:
  set.seed(8675309)
  Sigma <- matrix(c(0.1,0.3,0.3,4),2,2)
  mv <- data.frame(mvrnorm(4000,c(1.5,16),Sigma))
  
  # get the kde2d information: 
  mv.kde <- kde2d(SubFloat.df$BotCoreLon, SubFloat.df$BotCoreLat, n = 100)
  dx <- diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
  dy <- diff(mv.kde$y[1:2])
  sz <- sort(mv.kde$z)
  c1 <- cumsum(sz) * dx * dy
  
  # specify desired contour levels:
  prob <- c(0.95,0.75,0.60,0.45, 0.30, 0.15, 0)
  
  # plot:
  
  dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
  dc <- melt(mv.kde$z)
  dc$prob <- approx(sz,1-c1,dc$value)$y
  p <- ggplot(dc,aes(x=Var1,y=Var2))+
    geom_point(aes(x=BotCoreLon,y=BotCoreLat),data=SubFloat.df,color = "darkblue", alpha=0.5) +
    geom_contour_filled(aes(z=prob),breaks=prob, alpha = 0.7)+ scale_fill_brewer(palette = "Blues")  +
    geom_point(aes(x = CoreLon, y = CoreLat), 
               color = "black",  # Border color
               fill = "red",      # Inside color
               size = 3, 
               shape = 24,        # Shape 24 is a triangle
               stroke = 1) +
    xlab('Longitude') + ylab('Latitude') + xlim(xlims) + ylim(ylims) + guides(fill = guide_legend(title = "Probability")) + 
    theme(panel.background = element_rect(fill = "white", color = 'black'), panel.grid = element_blank(), axis.ticks = element_line())
  
  ggsave('ProbabilityPlot.png',p, width = 5*(max(SubFloat.df$BotCoreLon)-min(SubFloat.df$BotCoreLon))/(max(SubFloat.df$BotCoreLat)-min(SubFloat.df$BotCoreLat)), height = 5)
  
  g+p
  
  f <- ggplot(SubFloat.df, aes(BotCoreLon,BotCoreLat)) +   
    geom_density_2d_filled(aes(x = BotCoreLon, y = BotCoreLat), alpha = 0.5) + scale_fill_brewer(palette = "Oranges") + 
    xlim(xlims) + ylim(ylims) +
    theme(panel.background = element_rect(fill = NA, color = NA), panel.grid = element_blank(), legend.position = 'none', axis.text = element_blank(), axis.title = element_blank(),plot.margin = unit(c(0,0,0,0),"mm"),panel.border = element_blank())
  
  f <- ggsave('ArcGis_Temp.png', f, width = 5*(max(SubFloat.df$BotCoreLon)-min(SubFloat.df$BotCoreLon))/(max(SubFloat.df$BotCoreLat)-min(SubFloat.df$BotCoreLat)), height = 5)
  
  # Load the image as a raster
  r <- raster(f)
  # Manually set the extent based on your known xlim and ylim
  extent(r) <- c(xlims, ylims)
  
  # Assign a CRS (WGS 84, common for lat/lon)
  crs(r) <- "+proj=longlat +datum=WGS84"
  
  # Save as a GeoTIFF with CRS and correct extent
  writeRaster(r, "map_georeferenced.tif", format="GTiff", overwrite=TRUE)
  
  if (oldw == -1) {
    options(warn = 0)
  } else {
    options(warn = oldw)
  }
  return(paste('Check -', getwd(), '- for generated plots'))
}
