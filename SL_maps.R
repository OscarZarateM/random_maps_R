#Packages in Env
library(renv)
renv::init()
#Packages(check that tmap is ver. 4)
library(sf)
library(tmap)
library(tidyverse)
library(maptiles)
library(rnaturalearth)
library(terra)
#Reference Map
##Data
china = ne_countries(country = "china", type = "countries")
## random points (ignore, you will have your point data, hopefully they are GPS
#coordinates, if not you´ll need to transform them)
lat_central = 25.202770
lon_central = 108.017094
radio = 0.01  
set.seed(123)  
random_points = data.frame(
  lat = lat_central + runif(20, -radio, radio),
  lon = lon_central + runif(20, -radio, radio))
# Define a vector of 3 colors
colors = c("red", "green", "blue")

# Add a new column 'color' with random color assignments for each point
random_points$color = sample(colors, 20, replace = TRUE)
##Here read your point data as a data.frame, it should have one column with the
## lat and lon (if you have altitute let me know)
## make for points.df a sf object st_as_sf(yourDF, coords = c("col_with-lon", "col_with_lat"),
#crs = 4326 (if they are GPS coords)) 
#You can make the sf object rpoints and run the code bellow
rpoints = st_as_sf(random_points, coords = c("lon", "lat"), crs = 4326)
rpoints_p = st_transform(rpoints, crs = 32648)
##Bounding box (box containing all points)
bbox = st_bbox(rpoints_p)
##
bbox_polygon =  st_as_sfc(st_bbox(c(xmin = unname(bbox["xmin"]), ymin = unname(bbox["ymin"]),
    xmax = unname(bbox["xmax"]), ymax = unname(bbox["ymax"])), crs = 32648))  
rsquare = st_bbox(st_buffer(bbox_polygon, 480000, endCapStyle = "SQUARE"))

r_square = st_as_sfc(st_bbox(c(xmin = unname(rsquare["xmin"]), ymin = unname(rsquare["ymin"]),
                       xmax = unname(rsquare["xmax"]), ymax = unname(rsquare["ymax"])), crs = 32648))




## Background
tiles = get_tiles(china  , provider = "CartoDB.PositronNoLabels", zoom = 5) #Adjust the zoom if you want it closer or farther
#you can choose if you want the background, also check https://leaflet-extras.github.io/leaflet-providers/preview/
#to see other basemaps providers
#Option 1, with base map, the only problem is that because China is huge and the base map
#uses a mercator projection the Square will look weird. Alternatively you can export the map
#and add the square manually 
##Map with tmap
tm_shape(tiles)+  
  tm_rgb() +
  tm_shape(st_transform(china, crs = 32648))+
  tm_polygons(col = "black")+
  tm_compass(position = c("right","top"), size = 2)+
  tm_scalebar( position = c("right", "bottom"))+
  tm_shape(rpoints)+
  tm_dots(col = "red")+
  tm_shape(r_square)+# modify the
  tm_borders(col = "red")

#Option 2
#without base map. More information could be added but I´m lazy to do it. Just tell me what you would like
tm_shape(st_transform(china, crs = 32648))+
  tm_polygons(col = "black")+
  tm_compass(position = c("right","top"), size = 3)+
  tm_scalebar( position = c("right", "bottom"))+
  tm_shape(rpoints)+
  tm_dots(col = "black")+#col of points
  tm_shape(r_square)+
  tm_borders(col = "red") #change the color if you want

##Export as you wish

###Closeup
#Background for points
tiles = get_tiles(st_bbox(rpoints) , provider = "Esri.WorldImagery", zoom = 17)

  tm_shape(tiles, unit = "m")+
  tm_rgb()+
  tm_shape(rpoints_p)+
  tm_dots(fill = "color")+ # change "color" to a column name if you want different colors, otherwise just write "black"
  tm_compass(position = c("right","top"), size = 3)+
  tm_scalebar(text.size = 0.7)




  