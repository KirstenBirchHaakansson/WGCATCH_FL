


#pie_map_per_stock

pie_maps <- function(df, x, y, by, time, type = "rectangle", coulors,  treshold, 
                                geo_data_path, output_path, zoom = "auto", xlim, ylim) {
  
  
  require(mapplots)
  require(shapefiles)
  require(fishPiCodes)
  require(dplyr)
  
  x <- enquo(x)
  y <- enquo(y)
  by <- enquo(by)
  time <- enquo(time)
  
  df <- mutate(df, x = !!x, y = !!y, by = !!by, time = !!time)
  
  name_x <- quo_name(x)
  name_y <- quo_name(y)
  name_by <- quo_name(by)
  #name_time <- quo_name(time)

  
  #Getting geo data and making it ready
  
  country_shape <- read.shapefile(paste(geo_data_path, "/country", sep = ""))
  
  rect_dec <- select(readRDS(paste(geo_data_path, "/ICESsquares_points.RDS", sep = "")), statisticalRectangle, latitudeDecMid, longitudeDecMid)
  rect_dec <- rename(rect_dec, x = statisticalRectangle, lat = latitudeDecMid, lon = longitudeDecMid)
  
  data(UNLOCODE)
  harbour_dec <- select(UNLOCODE, loCode, lat, lon)
  harbour_dec$lat <- as.numeric(harbour_dec$lat)
  harbour_dec$lon <- as.numeric(harbour_dec$lon)
  harbour_dec <- rename(harbour_dec, x = loCode)
  
  
  #Add lat lon to df
  
  if (type == "rectangle") {
    dat <- left_join(df, rect_dec)
  } else if (type == "UNLOCODE") {
    dat <- left_join(df, harbour_dec)
  } else {
    print("Not possible")
  }
  
  #Running a plot per stock
  
  fak <- factor(dat$stock)
  
  for (k in levels(fak)) {
    
    dat_0 <- dat[fak == k,]
    
    #Summarise data 
    dat_sum <- data.frame(summarise(group_by(dat_0, lon, lat, time, by, stock), y = sum(y, na.rm = T)))
    
    dat_sum[,4] <- as.factor(dat_sum[,4])
    
    dat_cumsum <- mutate(arrange(dat_sum,-y), cumsum = cumsum(y), cumpct = (cumsum(y)/sum(y))*100, pct = (y/sum(y))*100)
    
    #For the sake of an overview -  only selecting x's, which account for the treshhold selected
    dat_0 <- filter(dat_cumsum, cumpct <= treshold | pct >= treshold)
    
    if (zoom == "auto") {
    
    #Setting xlim og ylim, so the plot follows the data
    
    zoom_text <- "auto_zoom"
      
    ylim_min <- min(dat_0$lat, na.rm = T)
    ylim_max <- max(dat_0$lat, na.rm = T)
    
    xlim_min <- min(dat_0$lon, na.rm = T)
    xlim_max <- max(dat_0$lon, na.rm = T)
    
    xlim <- c(xlim_min - 2, xlim_max + 2)
    ylim <- c(ylim_min - 1, ylim_max + 1)
    
    if ((ylim_max - ylim_min) < 1 | (xlim_max - xlim_min < 2)) {
      
      xlim <- c(xlim_min - 4, xlim_max + 4)
      ylim <- c(ylim_min - 2, ylim_max + 2)
    }
    } else if (zoom == "manual") {
      xlim <- xlim
      ylim <- ylim
      
      zoom_text <- "manual_zoom"  
    } else {
      print("zoom not selected")
    }
  
    
    #Plot per time
    
    fak_time <- factor(dat_0$time)
    
    for (j in levels(fak_time)) {
      
      dat_1 <- dat_0[fak_time == j,]
  
      #Pct missing position
      
      mis_rect <- round((sum(subset(dat_1, is.na(lon))$y) / sum(sum(dat_1$y)))*100, digits = 1)
      
      mis_rect <- ifelse(length(mis_rect) == 0, 0, mis_rect) 
      
      
      dat_1 <- filter(dat_1, !(is.na(lat)))
      
  #Summarise data 
  dat_sum <- data.frame(summarise(group_by(dat_1, lon, lat, by), y_sum = sum(y, na.rm = T)))
  
  dat_sum[,3] <- as.factor(dat_sum[,3])
  
  dat_cumsum <- mutate(arrange(dat_sum,-y_sum), cumsum = cumsum(y_sum), cumpct = (cumsum(y_sum)/sum(y_sum))*100)

  #Prep for plotting
  
  xyz <- make.xyz(dat_cumsum[,1],dat_cumsum[,2],dat_cumsum[,4],dat_cumsum[,3])

  
  #Setting the tilte of the plot
  title <-  paste(k, ", ", name_y, " per ", name_by, " and ", name_x , ", ", j,  sep = "")
  
  path <- file.path(paste(output_path, k, "_", name_y, "_", name_by, "_", name_x , "_", j, "_", zoom_text, ".jpeg", sep = ""))
  jpeg(path, width = 17, height = 18, res = 1000, units = "cm")
  
  #Plotting
  par(mfrow = c(1,1), oma = c(3,0,0,0), mar = c(1,1,1.5,1))
  basemap(xlim = xlim, ylim = ylim, main = title, bg = "white", xlab = "", ylab = "", axes = FALSE, cex.main = 0.9)
  draw.shape(country_shape, col = "white", xlim = xlim, ylim = ylim)
  draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.4, col = coulors)
  
  par(fig = c(0, 1, 0, 1), oma = c(1, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", legend = levels(dat_sum[,3]), fill = coulors, 
         bty = "n", cex = 0.7, ncol = ceiling(length(levels(dat_sum[,3]))/2),  xpd = TRUE, inset = c(0,0)) 
  mtext(paste("Only including ", name_x, "'s accounting for ", treshold, " pct. of total ", name_y, ", ", 
              mis_rect, "% of total ", name_y, " missing info about position and therefore not shown", sep = ""), side = 1,  outer = T, cex = 0.5,  xpd = TRUE)  
  
  dev.off()
  
    }
    
  }
  }


