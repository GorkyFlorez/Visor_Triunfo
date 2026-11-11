



library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
archivo_Predio <- 'SHP/Predio.shp'
archivo_lotes <- 'SHP/Lotes_poligono.shp'
archivo_perimetro <- 'SHP/Perimetro.shp'
archivo_vertices <- 'SHP/Vertices_lotes.shp'

archivo_original  <- "ORTO_MOSAICO_Clip21.tif"
archivo_web       <- "ORTO_MOSAICO_Clip21_Clipa_RGB_WEB.tif"

predio <- st_read(archivo_Predio,quiet = TRUE)
lotes <- st_read(archivo_lotes,quiet = TRUE)
perimetro <- st_read(archivo_perimetro,quiet = TRUE)
puntos <- st_read(archivo_vertices, quiet = TRUE)

predio <- st_make_valid(predio)
lotes <- st_make_valid(lotes)
perimetro <- st_make_valid(perimetro)
puntos <- st_make_valid(puntos)

predio_utm <- st_transform(predio,32719)
lotes_utm <- st_transform(lotes,32719)
perimetro_utm <- st_transform(perimetro,32719)
puntos_utm <- st_transform(puntos,32719)

predio_utm <- st_simplify(predio_utm,dTolerance = 0.03,preserveTopology = TRUE)
lotes_utm <- st_simplify(lotes_utm,dTolerance = 0.03,preserveTopology = TRUE)
perimetro_utm <- st_simplify(perimetro_utm,dTolerance = 0.03,preserveTopology = TRUE)
puntos_utm <- st_simplify(puntos_utm,dTolerance = 0.03,preserveTopology = TRUE)

predio_web <- st_transform(predio_utm,4326)
lotes_web <- st_transform(lotes_utm,4326)
perimetro_web <- st_transform(perimetro_utm,4326)
puntos_web <- st_transform(puntos_utm,4326)


lotes_web <- lotes_web[,c("Lote","Area","Perimetro", "Ventas", attr(lotes_web, "sf_column"))]



lotes_web$Lote_texto <-as.character(lotes_web$Lote)


# 13. POPUP DE LOTES
# ============================================================

lotes_web$Lote_texto <-as.character(lotes_web$Lote)

lotes_web$Area_num <-suppressWarnings(as.numeric( lotes_web$Area))
lotes_web$Perimetro_num <-suppressWarnings(as.numeric(lotes_web$Perimetro))
perimetro_web$Distance_num <-suppressWarnings(as.numeric(perimetro_web$Distance))
puntos_web$Vertice_texto <-as.character(puntos_web$Vertices)

lotes_web$Area <- as.numeric(gsub(",", ".", as.character(lotes_web$Area)))
lotes_web$Perimetro <- as.numeric(gsub(",", ".", as.character(lotes_web$Perimetro)))

# 1. CONVERTIR CAMPOS NUMÉRICOS
lotes_web$Area <- as.numeric(gsub(",", ".", as.character(lotes_web$Area)))
lotes_web$Perimetro <- as.numeric(gsub(",", ".", as.character(lotes_web$Perimetro)))
# 2. IDENTIFICAR DISPONIBILIDAD
lotes_web$Ventas <- trimws(as.character(lotes_web$Ventas))
lotes_web$Estado <- ifelse(!is.na(lotes_web$Ventas) &tolower(lotes_web$Ventas) == "vendido","VENDIDO","DISPONIBLE")

# 3. COLORES SEGÚN ESTADO
lotes_web$Color <- ifelse(lotes_web$Estado == "VENDIDO","#D62828","#00A651")
lotes_web$Relleno <- ifelse(lotes_web$Estado == "VENDIDO", "#F5A3A3", "#A8E6BF")

lotes_web$Popup <- paste0(
  "<div style='font-family:Arial;width:220px;text-align:center;'>",
  "<div style='background:#0057B8;color:white;padding:12px;font-size:17px;font-weight:bold;'>",
  htmlEscape(lotes_web$Lote),"</div>","<p><b>Área:</b> ",
  round(lotes_web$Area, 2), " m²</p>", "<p><b>Perímetro:</b> ",
  round(lotes_web$Perimetro, 2), " m</p>","<div style='background:",
  lotes_web$Color,";color:white;padding:9px;border-radius:5px;font-weight:bold;'>",
  lotes_web$Estado,"</div></div>")

# 5. ETIQUETAS DE LOS LOTES
lotes_web$Etiqueta <- paste0(htmlEscape(lotes_web$Lote),"<br><span style='font-size:9px;'>",lotes_web$Estado,"</span>")


centros_lotes <- st_point_on_surface(lotes_web)

coord_lotes <- st_coordinates(centros_lotes)

centros_lotes$lon <-coord_lotes[, "X"]
centros_lotes$lat <-coord_lotes[, "Y"]

centros_lotes$Etiqueta <- paste0("<div class='etiqueta-lote'>","<div class='numero-lote'>","",
  centros_lotes$Lote_texto,"</div>","<div class='datos-lote'>","ÁREA = ",
  format(round(centros_lotes$Area_num, 2), nsmall = 2)," m²","<br>","PERÍMETRO = ",
  format(round(centros_lotes$Perimetro_num,2),nsmall = 2)," m","</div>", "</div>")

centros_lotes$Etiqueta <-lapply(centros_lotes$Etiqueta,HTML)

coord_puntos <- st_coordinates(puntos_web)
puntos_web$lon <- coord_puntos[, "X"]
puntos_web$lat <- coord_puntos[, "Y"]
bbox <- st_bbox(lotes_web)





# ============================================================
# 15. ORTOMOSAICO
# ============================================================

orto <- terra::rast(archivo_original)
rgb <- orto[[1:3]]
names(rgb) <- c("Red","Green","Blue")

# ============================================================
# 16. REDUCIR RESOLUCIÓN
# ============================================================

factor_reduccion <- 3
rgb_web <- terra::aggregate(rgb,fact = factor_reduccion,fun = mean,na.rm = TRUE)

rgb_8bit <- rgb_web

for(i in 1:3) {
  
  valores <- terra::minmax(
    rgb_web[[i]]
  )
  
  minimo <- valores[1, 1]
  
  maximo <- valores[2, 1]
  
  
  if(maximo > minimo) {
    
    rgb_8bit[[i]] <-
      (
        (
          rgb_web[[i]] - minimo
        ) /
          (
            maximo - minimo
          )
      ) * 255
    
  }
  
}


rgb_8bit <- terra::clamp(rgb_8bit, lower = 0,upper = 255)
rgb_8bit <- terra::round(rgb_8bit)

terra::writeRaster(rgb_8bit,archivo_web,overwrite = TRUE,
datatype = "INT1U",gdal = c("COMPRESS=JPEG","JPEG_QUALITY=85","TILED=YES","PHOTOMETRIC=RGB","BIGTIFF=IF_SAFER"))

orto_web <- terra::rast( archivo_web)

if(
  terra::crs(
    orto_web,
    describe = TRUE
  )$code != "4326"
) {
  
  orto_leaflet <- terra::project(
    
    orto_web,
    
    "EPSG:4326",
    
    method = "bilinear"
    
  )
  
} else {
  
  orto_leaflet <- orto_web
  
}


# ============================================================
# ETIQUETAS DE LOTES - ESTILO PLANO CATASTRAL
# ============================================================

# Convertir valores numéricos
centros_lotes$Area_num <- as.numeric(gsub(",", ".", as.character(centros_lotes$Area)))

centros_lotes$Perimetro_num <- as.numeric(gsub(",", ".", as.character(centros_lotes$Perimetro)))

# Crear etiquetas HTML
centros_lotes$Etiqueta <- paste0("<div style='font-family:Arial, sans-serif;text-align:center;line-height:1.05;white-space:nowrap;
    pointer-events:none;'>",
  "<div style='color:#003C8F;font-size:13px;font-weight:900;'>",
  "",centros_lotes$Lote_texto,
  "</div>", "<div style='color:#003C8F;font-size:8px; font-weight:700;'>",
  "ÁREA = ",sprintf("%.2f", centros_lotes$Area_num)," m²",
  "</div>","<div style='color:#003C8F;font-size:8px;font-weight:700;'>",
  "PERÍMETRO = ",sprintf("%.2f", centros_lotes$Perimetro_num)," m","</div>","</div>")

# Convertir a HTML
centros_lotes$Etiqueta <- lapply(
  centros_lotes$Etiqueta,
  htmltools::HTML
)


mapa <-leaflet(
  options = leafletOptions(
    minZoom = 3,maxZoom = 22,zoomControl = TRUE,preferCanvas = TRUE)) %>%
  
addTiles(
  urlTemplate ="https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
  attribution ="Esri, Maxar, Earthstar Geographics",
  group = "Imagen satelital",
  options =
    tileOptions( maxZoom = 22, maxNativeZoom = 19,
      updateWhenIdle = TRUE,updateWhenZooming = FALSE,keepBuffer = 1 ))%>%
  
addTiles(urlTemplate ="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  attribution ="OpenStreetMap contributors",
  group ="Mapa OSM",
  options = tileOptions(maxZoom = 20,updateWhenIdle = TRUE,updateWhenZooming = FALSE,keepBuffer = 1)) %>%
  
addPolygons( data =predio_web, group ="Predio Matriz",
             color ="#D40000", weight =3,opacity =1,fill =FALSE,
  options =pathOptions(interactive =FALSE)) %>%
  
addPolygons( data =
    lotes_web,group ="Lotes",layerId =~paste0("lote_",Lote_texto), 
    color = ~Color, weight =1.5, opacity = 1,fillColor =~Relleno, fillOpacity =0.55, popup = ~Popup,
    labelOptions = labelOptions(direction = "auto",textsize = "11px",
                                style = list("font-weight" = "bold","text-align" = "center")),
    highlightOptions = highlightOptions(weight = 3,color = "#FFD700",bringToFront = TRUE),
    label = ~lapply(Etiqueta, HTML)) %>%
  
addLegend(position = "bottomright",colors = c("#00A651", "#D62828"),
          labels = c("Disponible", "Vendido"),title = "Estado de lotes",opacity = 1)%>%
  
  addLabelOnlyMarkers(
    data = centros_lotes,
    lng = ~lon,
    lat = ~lat,
    label = ~Etiqueta,
    
    group = "Información de lotes",
    
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE,
      opacity = 1,
      className = "etiqueta-catastral",
      
      style = list(
        "background" = "transparent",
        "border" = "none",
        "box-shadow" = "none",
        "padding" = "0",
        "margin" = "0",
        "text-align" = "center"
      )
    )
  )  %>%
  
addPolylines(data =perimetro_web,
  group ="Perímetro y distancias",color ="#00A651",weight = 1,
  opacity = 1,label =~paste0( round(Distance_num,2)," m"),
  labelOptions =
    labelOptions( direction = "auto",textsize ="10px",opacity =0.95)) %>%
  
addCircleMarkers(
  data =puntos_web,lng =~lon,lat =~lat,group ="Vértices",radius =2.5,stroke =FALSE,
  fillColor = "#D40000",
  fillOpacity = 1,
  label =~Vertice_texto,
  labelOptions =labelOptions( direction ="top",textsize ="9px", opacity =1)) %>%

addScaleBar(position = "bottomleft",
  options =scaleBarOptions( metric =TRUE, imperial =FALSE)) %>%
  
addLayersControl(baseGroups =
    c( "Mapa OSM","Imagen satelital"),
  overlayGroups =
    c("Predio Matriz","Lotes","Ortomosaico RGB","Información de lotes","Perímetro y distancias","Vértices"),
  options =layersControlOptions(collapsed =FALSE)) %>%

hideGroup("Mapa") %>%
hideGroup("Información de lotes") %>%
hideGroup("Vértices")  %>%

fitBounds( lng1 =unname(bbox["xmin"]),
           lat1 =unname(bbox["ymin"]),lng2 =unname(bbox["xmax"]),lat2 =unname( bbox["ymax"]))%>%

leaflet.extras::addDrawToolbar(
  targetGroup ="Dibujos",
  polylineOptions =leaflet.extras::drawPolylineOptions
     (shapeOptions = leaflet.extras::drawShapeOptions(color = "#FF6600", weight =3)),
  polygonOptions =
    leaflet.extras::drawPolygonOptions( showArea =TRUE,shapeOptions =
        leaflet.extras::drawShapeOptions( color ="#FF6600", weight =3, fillOpacity = 0.25)),
  rectangleOptions =
    leaflet.extras::drawRectangleOptions( shapeOptions =leaflet.extras::drawShapeOptions(color =
            "#FF6600",weight =3,fillOpacity =0.20)),circleOptions =
    leaflet.extras::drawCircleOptions(shapeOptions =
        leaflet.extras::drawShapeOptions(color ="#FF6600", weight =3, fillOpacity =0.20 )),
  markerOptions =leaflet.extras::drawMarkerOptions(),
  circleMarkerOptions =leaflet.extras::drawCircleMarkerOptions(),
  editOptions = leaflet.extras::editToolbarOptions(edit =TRUE, remove = TRUE))%>%
  
leaflet.extras::addSearchOSM(options =leaflet.extras::searchOptions(autoCollapse = TRUE, minLength =2,zoom =17 ))%>%
  
  # ============================================================
# 38. ORTOMOSAICO
# ============================================================

leafem::addRasterRGB( x =orto_leaflet,r = 1,g = 2,b =3,opacity =1,group =
    "Ortomosaico RGB",project =FALSE,maxBytes = 100 * 1024 * 1024)

archivo_html <- 'Visor.html'
htmlwidgets::saveWidget(mapa,archivo_html,selfcontained=FALSE,title='Visor')
