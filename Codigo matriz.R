



library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
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




library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

archivo_original2  <- "ORTO.tif"
archivo_web2       <- "ORTO_Reducido.tif"

# ============================================================
# 15. ORTOMOSAICO
# ============================================================
orto2 <- terra::rast(archivo_original2)
rgb2 <- orto2[[1:3]]
names(rgb2) <- c("Red","Green","Blue")
# ============================================================
# 16. REDUCIR RESOLUCIÓN
# ============================================================
factor_reduccion2 <- 3
rgb_web2 <- terra::aggregate(rgb2,fact = factor_reduccion2,fun = mean,na.rm = TRUE)
rgb_8bit2 <- rgb_web2
for(i in 1:3) {
  valores <- terra::minmax(
    rgb_web2[[i]])
  minimo <- valores[1, 1]
  maximo <- valores[2, 1]
  if(maximo > minimo) {
    rgb_8bit2[[i]] <-
      (
        (
          rgb_web2[[i]] - minimo
        ) /
          (
            maximo - minimo
          )
      ) * 255
    
  }
  
}


rgb_8bit2 <- terra::clamp(rgb_8bit2, lower = 0,upper = 255)
rgb_8bit2 <- terra::round(rgb_8bit2)

terra::writeRaster(rgb_8bit2,archivo_web2,overwrite = TRUE,
                   datatype = "INT1U",gdal = c("COMPRESS=JPEG","JPEG_QUALITY=85","TILED=YES","PHOTOMETRIC=RGB","BIGTIFF=IF_SAFER"))

orto_web2 <- terra::rast( archivo_web2)

if(
  terra::crs(
    orto_web2,
    describe = TRUE
  )$code != "4326"
) {
  orto_leaflet2 <- terra::project(
    orto_web2,
    "EPSG:4326",
    method = "bilinear"
  )
} else {
  orto_leaflet2 <- orto_web2
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

library(sf)
lados <- list()
k <- 0L
for (i in seq_len(nrow(lotes_utm))) {
  geom <- st_geometry(lotes_utm)[[i]]
  poligonos <- if (inherits(geom, 'MULTIPOLYGON')) geom else list(geom)
  for (pol in poligonos) {
    for (anillo in pol) {
      xy <- as.matrix(anillo)[, 1:2, drop = FALSE]
      if (nrow(xy) < 2) next
      for (j in seq_len(nrow(xy)-1L)) {
        a <- xy[j, ]; b <- xy[j+1L, ]
        d <- sqrt(sum((b-a)^2))
        if (!is.finite(d) || d < 0.15) next
        # Clave independiente de la direccion; tolerancia de 5 cm
        p <- paste(round(a/0.05), collapse = '_')
        q <- paste(round(b/0.05), collapse = '_')
        clave <- paste(sort(c(p,q)), collapse = '|')
        ang <- atan2(b[2]-a[2], b[1]-a[1])*180/pi
        if (ang > 90) ang <- ang-180
        if (ang < -90) ang <- ang+180
        k <- k+1L
        lados[[k]] <- data.frame(clave=clave, x=(a[1]+b[1])/2,
                                 y=(a[2]+b[2])/2, metros=d, angulo=ang)
      }
    }
  }
}
medidas_lados <- do.call(rbind, lados)
medidas_lados <- medidas_lados[!duplicated(medidas_lados$clave), ]
centros_medidas <- st_as_sf(medidas_lados, coords=c('x','y'), crs=32719)
centros_medidas <- st_transform(centros_medidas, 4326)
coord_medidas <- st_coordinates(centros_medidas)
centros_medidas$lon <- coord_medidas[,1]
centros_medidas$lat <- coord_medidas[,2]
centros_medidas$Etiqueta_medida <- lapply(seq_len(nrow(centros_medidas)), function(i) {
  htmltools::HTML(sprintf(
    '<div style="display:inline-block;transform:rotate(%.2fdeg);transform-origin:center;white-space:nowrap;color:#111;background:rgba(255,255,255,.7);font:700 10px Arial;padding:0 2px;pointer-events:none">%.2f m</div>',
    -centros_medidas$angulo[i], centros_medidas$metros[i]))
})





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
    c("Predio Matriz","Lotes","Orto RGB antes","Orto RGB despues",
      "Información de lotes","Perímetro y distancias","Vértices", "Medidas de linderos"),
  options =layersControlOptions(collapsed =FALSE)) %>%

hideGroup("Orto RGB antes") %>%
hideGroup("Medidas de linderos") %>%
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
    "Orto RGB antes",project =FALSE,maxBytes = 100 * 1024 * 1024)%>% 
leafem::addRasterRGB( x =orto_leaflet2,r = 1,g = 2,b =3,opacity =1,group =
                        "Orto RGB despues",project =FALSE,maxBytes = 100 * 1024 * 1024)%>% 
  addControlGPS()%>% 
addLabelOnlyMarkers(
  data = centros_medidas,
  lng = ~lon,
  lat = ~lat,
  label = ~Etiqueta_medida,
  group = "Medidas de linderos",
  labelOptions = labelOptions(
    noHide = TRUE,
    direction = "center",
    textOnly = TRUE,
    opacity = 1,
    className = "etiqueta-medida",
    style = list(
      "background" = "transparent",
      "border" = "none",
      "box-shadow" = "none",
      "padding" = "0",
      "pointer-events" = "none"
    )
  )
)







# AGREGAR DESPUES DE CREAR 'mapa' Y ANTES DE saveWidget()
# Se utilizan los centros de lotes ya calculados en tu script.

library(jsonlite)

lotes_ruta <- data.frame(
  lote = as.character(centros_lotes$Lote_texto),
  lat = as.numeric(centros_lotes$lat),
  lon = as.numeric(centros_lotes$lon),
  stringsAsFactors = FALSE
)
lotes_ruta <- lotes_ruta[!is.na(lotes_ruta$lote) & nzchar(lotes_ruta$lote) &
                           is.finite(lotes_ruta$lat) & is.finite(lotes_ruta$lon), ]
lotes_json <- jsonlite::toJSON(lotes_ruta, dataframe = 'rows', auto_unbox = TRUE)

panel_ruta <- paste0(
  '<div id="buscador-lotes" style="background:white;padding:12px;border-radius:8px;',
  'box-shadow:0 2px 12px #0004;min-width:230px;max-width:280px;font-family:Arial">',
  '<div style="font-weight:bold;color:#003C8F;margin-bottom:7px">BUSCAR Y NAVEGAR AL LOTE</div>',
  '<input id="entrada-lote" list="lista-lotes" placeholder="Escriba el lote" ',
  'style="box-sizing:border-box;width:100%;padding:8px;border:1px solid #bbb;border-radius:5px">',
  '<datalist id="lista-lotes"></datalist>',
  '<button id="boton-ir-lote" type="button" style="width:100%;margin-top:8px;',
  'background:#0057B8;color:white;border:0;padding:9px;border-radius:5px;',
  'cursor:pointer;font-weight:bold">Ir al lote</button>',
  '<button id="boton-limpiar-ruta" type="button" style="width:100%;margin-top:5px;',
  'background:#eee;border:1px solid #ccc;padding:7px;border-radius:5px;cursor:pointer">Limpiar ruta</button>',
  '<div id="mensaje-ruta" role="status" style="margin-top:7px;font-size:11px;color:#333;',
  'white-space:normal;line-height:1.35"></div></div>'
)

mapa <- mapa %>%
  leaflet::addControl(html = htmltools::HTML(panel_ruta), position = 'topleft') %>%
  htmlwidgets::onRender(sprintf('
function(el,x){
  var map=this, lotes=%s;
  var panel=document.getElementById("buscador-lotes");
  var entrada=document.getElementById("entrada-lote");
  var lista=document.getElementById("lista-lotes");
  var mensaje=document.getElementById("mensaje-ruta");
  var ruta=null, origen=null, destino=null;
  L.DomEvent.disableClickPropagation(panel);
  L.DomEvent.disableScrollPropagation(panel);
  lotes.forEach(function(l){
    var o=document.createElement("option");o.value=l.lote;lista.appendChild(o);
  });
  function limpiar(){
    [ruta,origen,destino].forEach(function(c){if(c)map.removeLayer(c)});
    ruta=origen=destino=null;
  }
  document.getElementById("boton-limpiar-ruta").onclick=function(){
    limpiar();mensaje.textContent="Ruta eliminada";
  };
  function buscar(){
    var q=entrada.value.trim().toLocaleLowerCase();
    var lote=lotes.find(function(l){return String(l.lote).trim().toLocaleLowerCase()===q});
    if(!lote){mensaje.textContent="Lote no encontrado. Seleccione un lote de la lista.";return;}
    limpiar();
    var lat=lote.lat, lon=lote.lon;
    destino=L.circleMarker([lat,lon],{radius:10,color:"#FFD700",weight:4,
      fillColor:"#0057B8",fillOpacity:0.9}).addTo(map);
    destino.bindPopup("Destino: "+String(lote.lote).replace(/</g,"&lt;")).openPopup();
    map.setView([lat,lon],19);
    mensaje.textContent="Lote localizado. Obteniendo ubicación para trazar la ruta...";
    if(!navigator.geolocation){mensaje.textContent="Geolocalización no disponible.";return;}
    navigator.geolocation.getCurrentPosition(async function(pos){
      var a=pos.coords.latitude,b=pos.coords.longitude;
      origen=L.circleMarker([a,b],{radius:7,color:"white",weight:2,
        fillColor:"#1976D2",fillOpacity:1}).addTo(map).bindPopup("Mi ubicación");
      var url="https://router.project-osrm.org/route/v1/driving/"+
        b+","+a+";"+lon+","+lat+"?overview=full&geometries=geojson";
      try{
        var respuesta=await fetch(url);
        if(!respuesta.ok)throw new Error("Servicio no disponible");
        var datos=await respuesta.json();
        if(datos.code!=="Ok"||!datos.routes||!datos.routes.length)
          throw new Error("No hay ruta vial disponible");
        var r=datos.routes[0];
        ruta=L.geoJSON(r.geometry,{style:{color:"#147DFF",weight:6,opacity:0.9}}).addTo(map);
        map.fitBounds(L.latLngBounds([[a,b],[lat,lon]]).extend(ruta.getBounds()),{padding:[35,35]});
        mensaje.textContent="Ruta vial estimada: "+(r.distance/1000).toFixed(2)+
          " km · "+Math.round(r.duration/60)+" min en vehículo. Puede finalizar en la vía más cercana.";
      }catch(e){
        map.fitBounds([[a,b],[lat,lon]],{padding:[35,35]});
        mensaje.textContent="Lote y ubicación mostrados, pero no se pudo calcular una ruta vial: "+e.message;
      }
    },function(e){
      mensaje.textContent="Lote localizado. Para dibujar la ruta, permita el acceso a su ubicación (HTTPS o localhost).";
    },{enableHighAccuracy:true,timeout:15000,maximumAge:0});
  }
  document.getElementById("boton-ir-lote").onclick=buscar;
  entrada.addEventListener("keydown",function(e){if(e.key==="Enter"){e.preventDefault();buscar();}});
}
', lotes_json))

archivo_html <- 'Visor.html'
htmlwidgets::saveWidget(mapa, archivo_html, selfcontained = FALSE, title = 'Visor')


