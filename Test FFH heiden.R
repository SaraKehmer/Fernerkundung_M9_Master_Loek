
### kleines Subset#####
##1. Einladen der DOP und zusammefügen als eine gesamtbild####

#🔧 Vorbereitung
#Installiere bei Bedarf diese Pakete:

#install.packages("terra")    # für Rasterdaten
#install.packages("fs")# für einfaches Dateihandling
install.packages("sf")


#📥 Alle .jp2 Dateien laden und mosaikieren

library(terra)
library(fs)

# Ordnerpfad mit deinen JP2-Dateien
#ordner_pfad <- "C:/Pfad/zu/deinem/Ordner"
#DOP_Senne<-("C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Luftbild DOP")
DOP_Senne_Test <-rast("C:/Users/Silas/Desktop/LOEK_Alle_Semester/Master/Fernerkundung/Praktikum/Folien/Github/Github_Einführung/Fernerkundung_M9_Master_Loek/dop10rgbi_32_483_5748_1_nw_2022.jp2")

class(DOP_Senne_Test)

# Extrahiere RGBI-Bänder als neue Raster

rgbi <- DOP_Senne_Test[[1:4]]

NIR<-DOP_Senne_Test$`Band #4`

RED<-DOP_Senne_Test$Red

Green<-DOP_Senne_Test$Green

Blue<-DOP_Senne_Test$Blue

###Berechnung NDVI####
###### 3.1 calculate the NDVI and add as additonal layer to the stack####
#NIR (8 nehmen nicht 8A)- Red/NIR + Red
library(raster)
#NDVI berechnen
NDVI <- (NIR - RED) / (RED + NIR)
plot(NDVI, main = "NDVI")

writeRaster(NDVI, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Luftbild DOP/ndvi.tif", overwrite = TRUE)

# Pfad zu deiner Datei (anpassen!)
NDVI <- rast("C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Luftbild DOP/ndvi.tif")

###########KMEANS Classifikation############
# Pakete installieren (nur beim ersten Mal notwendig)
install.packages(c("terra", "RColorBrewer"))

# Pakete laden
library(terra)
library(RColorBrewer)

# Pfad zu deinem Luftbild
r <- DOP_Senne_Test

# Optional: Bandnamen anzeigen
names(r)

# Raster in Matrix umwandeln
v <- values(r)

# NA-Werte entfernen
v_no_na <- na.omit(v)

# K-Means Clustering (z.B. 5 Klassen)
set.seed(123)
k <- kmeans(v_no_na, centers = 5, iter.max = 100)

# Neues Raster mit Cluster-Labels
clustered <- r[[1]]
values(clustered) <- NA
values(clustered)[!is.na(rowSums(v))] <- k$cluster

# Visualisierung
plot(clustered, col = brewer.pal(5, "Set1"), main = "K-Means Klassifikation")

# Beispiel-Daten generieren
set.seed(123)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)

# K-Means Clustering mit 3 Clustern
kmeans_result <- kmeans(data, centers = 3)

# Cluster-Zugehörigkeit hinzufügen
data$cluster <- as.factor(kmeans_result$cluster)

# Visualisierung mit ggplot2
library(ggplot2)

##
ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering (k=3)",
       x = "X-Achse", y = "Y-Achse") +
  theme_minimal()


#Speichern
writeRaster(clustered, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/K-Means/kmeans_output_1.tif", overwrite = TRUE)


###########KMEANS Classifikation__nur Vegetationshöhe############
# Pakete installieren (nur beim ersten Mal notwendig)
install.packages(c("terra", "RColorBrewer"))

# Pakete laden
library(terra)
library(RColorBrewer)

# Pfad zu deinem Luftbild
r <- DOP_Senne_Test

# Optional: Bandnamen anzeigen
names(r)

# Raster in Matrix umwandeln
v <- values(r)

# NA-Werte entfernen
v_no_na <- na.omit(v)

# K-Means Clustering (z.B. 5 Klassen)
set.seed(123)
k <- kmeans(v_no_na, centers = 5, iter.max = 100)

# Neues Raster mit Cluster-Labels
clustered <- r[[1]]
values(clustered) <- NA
values(clustered)[!is.na(rowSums(v))] <- k$cluster

# Visualisierung
plot(clustered, col = brewer.pal(5, "Set1"), main = "K-Means Klassifikation")

#Speichern
writeRaster(clustered, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/K-Means/kmeans_output_1.tif", overwrite = TRUE)



############################# 📁 Pfad zum Ordner mit deinen .gpkg-Dateien#######################
gpkg_pfad <- "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Shapes Trocken Heiden/Trocken Heiden Senne.gpkg"

library(sf)

# Alle Layer anzeigen (eine GPKG kann mehrere Layer enthalten)
st_layers(gpkg_pfad)

# Konkreten Layer laden (z.B. erster Layer)
daten_shapes <- st_read(gpkg_pfad, layer = "Trocken Heiden Senne")

# Zeige Inhalt
print(daten_shapes)
plot(daten_shapes)

#######################Exceltabellen Einladen######################
#🧭 1. Setup: Pakete und Pfade

library(terra)
library(readxl)
library(tools)   # für file_path_sans_ext

# Pfad Excel-Dateien

#excel_pfad <-"C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/heide_info_adjusted.xlsx"

# Alle Tabellenblätter anzeigen
#excel_sheets(excel_pfad)

# Konkretes Blatt einlesen (z. B. erstes Blatt)
#daten <- read_excel(excel_pfad, sheet = 1)   # oder sheet = "Blattname"

# Daten anzeigen
#head(daten)

####4.b Excel und gpkg zusammenfügen


#🔧 1. Pakete laden

library(sf)
library(readxl)
library(dplyr)

# Inhalt prüfen
#head(gpkg_pfad)
#head(excel_pfad)

#🔗 3. Verknüpfen (per LOCALID)

# Annahme: daten_shapes ist sf, daten ist data.frame
# Kein Duplikat in daten
#stopifnot(!anyDuplicated(daten$LOCALID))

# Join durchführen
#merged <- left_join(daten_shapes, daten, by = "LOCALID")

# Plot prüfen
#plot(st_geometry(merged))  # sicherer als einfach plot(merged)


# Neue GeoPackage-Datei speichern
library(sf)

# Beispiel: merged ist dein sf-Objekt
#st_write(merged, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Shapes Trocken Heiden/Shapes Trocken Heidenheide_klassifikation.gpkg", layer = "heide_2025", driver = "GPKG", append = FALSE)

#Einlesen merged datei
Shape_Trockenheide <- st_read("C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Shapes Trocken Heiden/Shapes Trocken Heidenheide_klassifikation.gpkg")

plot(Shape_Trockenheide)
###########################Vegetationshöhe##########################


################## KMEANS Klassifikation mit Vegetationshöhe & NDVI ############

# Pakete laden
library(terra)
library(RColorBrewer)

# === 1. Raster laden ===
# Luftbild mit RGB/NIR
r <- DOP_Senne_Test           # z. B. 4 Bänder: Red, Green, Blue, NIR

# Vegetationshöhe-Raster laden (z. B. berechnete Z-Differenz)

veg_height <- rast("C:/Users/Asus/OneDrive - Universität Münster/Desktop/FFH Monitoring Fernerkundung Trocken Heiden/Höhenmodell/ndom50_32483_5748_1_nw_2022.tif")

veg_height_clean <- veg_height
veg_height_clean[veg_height_clean < 0] <- NA

plot(veg_height_clean,
     main = "Bereinigtes nDOM (veg_height)",
     col = hcl.colors(20, "YlGnBu", rev = TRUE))

writeRaster(veg_height_clean, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Vegetationshoehe/veg_height_clean.tif", overwrite = TRUE)

library(terra)

###Höhenmodell (z. B. nDOM oder Z-Differenz-Raster) in R ganz einfach in Klassen aufteilen,  in:

#Klasse 1: Höhen < 0.25 m

#Klasse 2: Höhen ≥ 0.25 m

# Raster einlesen (falls noch nicht vorhanden)
#veg_height <- rast("C:/Users/Asus/OneDrive - Universität Münster/Desktop/FFH Monitoring Fernerkundung Trocken Heiden/Höhenmodell/ndom50_32483_5748_1_nw_2022.tif")


# Klassifizierung: < 0.25 m = 1, ≥ 0.25 m = 2
veg_class <- classify(veg_height_clean, rbind(c(-Inf, 0.25, 1), 
                                        c(0.25, Inf, 2)))

# Anzeigen
plot(veg_class, col = c("lightgreen", "darkgreen"), 
     main = "Vegetationshöhenklassen (<25 cm / ≥25 cm)")


###Einzelne Objekte mit den Klassen quantifizieren###
zones <- shape_Trockenheide

zones <- project(shape_Trockenheide, crs(veg_class))

library(terra)

# Bounding Box des Rasters als Polygon
raster_extent_poly <- as.polygons(ext(veg_class), crs = crs(veg_class))

#Nur Zonen extrahieren, die mit dem Raster überlappen
zones_in_raster <- zones[relate(zones, raster_extent_poly, "intersects"), ]

plot(veg_class[[1]], main = "Objekte innerhalb Raster")
plot(zones_in_raster, add = TRUE, border = "red")

class_counts <- terra::extract(veg_class, zones_in_raster, counts = TRUE)

class_raw <- terra::extract(veg_class, zones_in_raster)

library(dplyr)
library(tidyr)

# Umbenennen für Klarheit (optional)
names(class_counts)[2] <- "veg_class"

names(zones_in_raster)

# Zählen der Pixelklassen pro LOCALID
class_counts$LOCALID <- zones_in_raster$LOCALID[class_counts$ID]

library(dplyr)
library(tidyr)

class_percent <- class_counts %>%
  group_by(LOCALID, veg_class) %>%
  summarise(pixel_count = n(), .groups = "drop") %>%
  group_by(LOCALID) %>%
  mutate(total = sum(pixel_count),
         percent = round(100 * pixel_count / total, 2)) %>%
  select(LOCALID, class = veg_class, percent) %>%
  pivot_wider(names_from = class, values_from = percent, names_prefix = "class_")


# Stelle sicher, dass NA-Werte ausgeschlossen sind
zones_ausgewertet <- zones_with_stats[!is.na(zones_with_stats$class_2), ]

# Plot mit Farbskala und automatischer Legende
plot(zones_ausgewertet["class_2"],
     main = "Anteil hoher Vegetation (%)",
     col = hcl.colors(9, "Greens", rev = TRUE),
     legend = TRUE)

# Erstelle Klassen manuell (z.B. für farbliche Einteilung)
zones_ausgewertet$class_klasse <- cut(
  zones_ausgewertet$class_2,
  breaks = c(0, 10, 25, 50, 75, 100),
  labels = c("<10%", "10–25%", "25–50%", "50–75%", ">75%")
)

# Farbpalette pro Klasse
farben <- c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006d2c")

# Grafikfläche mit breiterem rechtem Rand
par(mar = c(5, 4, 4, 6))  # unten, links, oben, rechts

# Plot (keine Legende hier)
plot(zones_ausgewertet, 
     col = farben[zones_ausgewertet$class_klasse],
     main = "Hohe Vegetation – Klassifiziert",
     border = "black")

# Legende innerhalb des Plotfensters, rechts platziert
legend("topright",
       inset = c(0.02, 0),      # kleiner rechter Abstand
       legend = levels(zones_ausgewertet$class_klasse),
       fill = farben,
       title = "Anteil ≥ 25 cm",
       cex = 0.8,
       xpd = TRUE,              # außerhalb Achsenbereich erlaubt
       bty = "n")



writeVector(zones_ausgewertet, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Vegetationshoehe/Trockenheide_Prozent_Hoehenklassen.gpkg", overwrite = TRUE)

# === 3. Alle Raster kombinieren ===
# NDVI muss aus den Bändern von r kommen – also gleiche Dimension → passt ✅

# Aber veg_height (z. B. aus QGIS) muss angepasst werden:
veg_height_resampled <- resample(veg_height, r, method = "near")  # "near" = keine Höhenverzerrung

# Jetzt klappt das Kombinieren:
stacked <- c(r, veg_height_resampled, NDVI)


# === 4. Rasterwerte extrahieren ===
v <- values(stacked)

# NA-Werte entfernen
v_no_na <- na.omit(v)

# === 5. KMeans-Clustering ===
set.seed(123)
k <- kmeans(v_no_na, centers = 5, iter.max = 100)

# === 6. Cluster-Raster erzeugen ===
clustered <- r[[1]]
values(clustered) <- NA
values(clustered)[!is.na(rowSums(v))] <- k$cluster

# === 7. Visualisierung ===
install.packages("RColorBrewer")  # Nur einmal nötig
library(RColorBrewer)

plot(clustered, col = brewer.pal(5, "Set1"), main = "KMeans mit RGBI/NDVI & Vegetationshöhe")
writeRaster(clustered, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/K-Means/kmeans_output_2.tif", overwrite = TRUE)

###ergebniss mit normalisiertem höhenmodell oder ohne ist bei kmeans ähnlich


###############################Strukturelle diversität berechnen##################

#######Strukturelle Diversität pro Plot berechnen############
#Shape_Trockenheide zu schneiden auf den Raster: "stacked"
library(terra)

# Lade die Vektorebene aus dem .gpkg
shape_Trockenheide <- vect("C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Shapes Trocken Heiden/Trocken Heiden Senne.gpkg", layer = "Trocken Heiden Senne")

shape_Trockenheide <- project(shape_Trockenheide, crs(stacked))

# 2. Bestimme die Ausdehnung des Rasters
raster_ext <- ext(stacked)

# 2. Extent als Polygon umwandeln
raster_poly <- as.polygons(raster_ext)

# 3. Prüfe: welche Polygone liegen vollständig innerhalb des Raster-Polygons
shape_in_raster <- shape_Trockenheide[relate(shape_Trockenheide, raster_poly, "within"), ]

stacked_crop <- crop(stacked, shape_in_raster)
stacked_masked <- mask(stacked_crop, shape_in_raster)

plot(stacked_masked)


#######Strukturelle Diversität pro Plot berechnen########

library(terra)

# 10 x 10 m Fenster (bei 0.5 m Raster = 20 Pixel)
w <- 21

r_sd_r <- focal(r[[1]], w = w, fun = sd, na.policy = "omit")
r_sd_g <- focal(r[[2]], w = w, fun = sd, na.policy = "omit")
r_sd_b <- focal(r[[3]], w = w, fun = sd, na.policy = "omit")

# Beispiel: Mittelwert der SDs als Maß für spektrale Diversität
ssd_rgb <- (r_sd_r + r_sd_g + r_sd_b) / 3

plot(ssd_rgb, main = "Stand Structural Diversity aus RGB", col = terrain.colors(100))

writeRaster(ssd_rgb, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/ssd_rgb.tif", overwrite = TRUE)

##############✅ Ziel Berechne Mittelwert der ssd_rgb innerhalb jedes Objekts (Plot)
#Vergleiche mit Schwelle 1.5
#Füge Ergebnis als neues Attribut (ssd_class) zum Vektor hinzu

#🛠️ Schritt-für-Schritt in R mit terra

library(terra)

# 1️⃣ Lade Shape-Datei (als SpatVector)
zones <- shape_Trockenheide # ggf. layer = "Name"

# 2️⃣ Projektionsabgleich (sehr wichtig!)
zones <- project(zones, crs(ssd_rgb))

# 3️⃣ Berechne mittlere SSD pro Objekt (LOCALID)
ssd_zonenwerte <- terra::extract(ssd_rgb, zones, fun = mean, na.rm = TRUE)

# 4️⃣ LOCALID hinzufügen (weil extract nur ID hat)
ssd_zonenwerte$LOCALID <- zones$LOCALID[ssd_zonenwerte$ID]

ssd_zonenwerte$ssd_rgb >= 1.5
names(ssd_zonenwerte)
ssd_zonenwerte$ssd_class <- ifelse(ssd_zonenwerte$focal_sd >= 1.5, "hoch", "niedrig")


#🔍 Ergebnisstruktur:
#  ID	ssd_rgb	LOCALID
#1	1.32	BT-4118-xxx...
#2	1.88	BT-LIP-04646

#✅# Klassifikation anhand der Schwelle 1.5

# Falls noch nicht geschehen: Spalte umbenennen
names(ssd_zonenwerte)[which(names(ssd_zonenwerte) == "focal_sd")] <- "ssd_rgb"

# Dreistufige Klassifikation mit zwei Schwellen
ssd_zonenwerte$ssd_class <- cut(
  ssd_zonenwerte$ssd_rgb,
  breaks = c(-Inf, 1.5, 2.5, Inf),
  labels = c("niedrig", "mittel", "hoch"),
  right = FALSE
)

# 1. Nur Zeilen ohne NA in ssd_class
zones_plot <- zones_result[!is.na(zones_result$ssd_class), ]

zones_plot$ssd_class <- factor(zones_plot$ssd_class,
                               levels = c("niedrig", "mittel", "hoch"))

farben <- c("gold", "orange", "darkgreen")

plot(zones_plot,
     col = farben[as.numeric(zones_plot$ssd_class)],
     border = "black",
     main = "Strukturelle Diversität (nur 'hoch')")

legend("bottomleft", legend = c("niedrig", "mittel", "hoch"),
       fill = farben, title = "SSD-Klasse", cex = 0.8, bty = "n")

table(zones_plot$ssd_class, useNA = "always")

###niedrig  mittel    hoch    <NA> 
#0       0     341       0 
####Nur Hohe Sturkturvielfalt... kann nicht richtig sein


#nur hohe Strukturvielfalt wurde bestimmt

#🔗 Ergebnis zurück an Vektorlayer anhängen

zones_result <- merge(zones, ssd_zonenwerte[, c("LOCALID", "ssd_rgb", "ssd_class")], by = "LOCALID", all.x = TRUE)


##################🛠️ 🔽 Skript: Calluna-Erkennung mit NDVI + Höhe


###🔍 1. Spektrale Signatur
#Calluna-Heide hat:
  # niedrigen bis mittleren NDVI (meist 0.2–0.5)
  #oft rötlich-bräunliche Färbung im Spätsommer/Herbst (Blütezeit!) ->fällt leider raus...
  #relativ niedrigen Wassergehalt → schwächer im NIR

  #➡️ Erkennbar mit:
  
  #Multispektralen Luftbildern (RGB + NIR)

  #NDVI und Red Edge Indices (bei Hyperspektraldaten)

#📐 2. Strukturelle Merkmale (LiDAR oder nDOM)
#Calluna-Heide ist typischerweise:
  
 # sehr niedrig: 0.2m – 0.5m

#gleichmäßig-flächig wachsend
#oft auf offenen, vegetationsarmen Böden

#➡️ Erkennbar mit:
  
 # nDOM: Filter auf 0.2–0.5m Höhe

#Höhen-Standardabweichung: gering (homogen)

# 📦 Pakete laden
library(terra)

# 1️⃣ Raster laden
# r = RGBI Raster: R=1, G=2, B=3, IR=4
r <- DOP_Senne_Test
veg_height <- veg_height_clean  # gereinigtes nDOM

# 2️⃣ NDVI berechnen
ndvi <- NDVI

# 3️⃣ Höhen- und NDVI-Maske erstellen
# Calluna typischerweise: Höhe 0.2–0.5 m, NDVI 0.2–0.5
ndvi_mask <- ndvi >= 0.2 & ndvi <= 0.5
height_mask <- veg_height >= 0.2 & veg_height <= 0.5

# 4️⃣ Kombinierte Calluna-Maske
calluna_mask <- ndvi_mask & height_mask

# NDVI und Höhenraster vorausgesetzt:
# ndvi <- ...
# veg_height <- ...

ndvi
veg_height
compareGeom(ndvi, veg_height, stopOnError = FALSE)

###Anpassen der beiden raster
ndvi_resampled <- resample(ndvi, veg_height)

ndvi_mask <- ndvi_resampled >= 0.2 & ndvi_resampled <= 0.5
height_mask <- veg_height >= 0.2 & veg_height <= 0.5

calluna_mask <- ndvi_mask & height_mask

# 1. NDVI-Maske: Calluna typischerweise NDVI zwischen 0.2 und 0.5
ndvi_mask <- ndvi >= 0.2 & ndvi <= 0.5

ndvi_resampled <- resample(ndvi, veg_height)

ndvi_mask   <- ndvi_resampled >= 0.2 & ndvi_resampled <= 0.5
height_mask <- veg_height >= 0.2 & veg_height <= 0.5


# 2. Höhenmaske: Calluna meist 0.2–0.5 m
height_mask <- veg_height >= 0.2 & veg_height <= 0.5

# 3. Kombinierte Maske
calluna_mask <- ndvi_mask & height_mask

plot(calluna_mask,
     main = "Potenzielle Calluna-Heide-Flächen",
     col = c("white", "purple"))


# 6️⃣ Optional: als Raster speichern
writeRaster(calluna_mask, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/calluna_heide_maske.tif", overwrite = TRUE)




######library(terra)
library(dplyr)
library(tidyr)

# 1️⃣ Raster und Vektor laden
raster_kmeans <- rast("C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/K-Means/K-Meanskmeans_output.tif")           # Klassifiziertes Raster
polygon <- vect("C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/test shape.qlr.gpkg")                   # Polygon-Shape

# 2️⃣ Pro Polygon: Pixelanzahl je Klasse extrahieren
counts_df <- terra::extract(raster_kmeans, polygon, counts = TRUE)


# Berechne pro ID die Anzahl pro Klasse (hier: Red)
class_percent <- counts_df %>%
  group_by(ID, Red) %>%
  summarise(pixel_count = n(), .groups = "drop") %>%
  group_by(ID) %>%
  mutate(total = sum(pixel_count),
         percent = round(100 * pixel_count / total, 2)) %>%
  select(ID, class = Red, percent) %>%
  pivot_wider(names_from = class, values_from = percent, names_prefix = "class_")

# Umbenennen der Spalten nach Bedeutung
class_percent <- class_percent %>%
  rename(
    offenboden_ohne_Vegetation = class_1,
    heide                      = class_2,
    junge_heide                = class_3,
    baeume_grosse_objekte      = class_4,
    pioniergras                = class_5
  )


##########Vegetationshöhe klassifizieren, um den Anteil der Calluna altersphasen zu erhalten########
library(terra)

library(dplyr)
library(tidyr)

# Höhengeraster in 3 Klassen (bereits vorhanden)
# veg_class <- classify(veg_height, rcl = mat)

# Polygon (GPKG)
shp <- shape_Trockenheide  # z. B. shape_Trockenheide

# Ausschneiden: nur Polygone, die vollständig im Raster liegen
raster_extent_poly <- as.polygons(ext(veg_class))
shp_in_raster <- shp[relate(shp, raster_extent_poly, "within"), ]

# Extrahiere Werte pro Polygonpixel
ex <- terra::extract(veg_class, shp_in_raster)

# Achtung: Standardmäßig heißt veg_class-Spalte gleich dem Layernamen
# wir benennen sie um (falls nötig)
names(ex)[2] <- "class"

# Prozent je Klasse pro Polygon
class_percent <- ex %>%
  group_by(ID, class) %>%
  summarise(pixel_count = n(), .groups = "drop") %>%
  group_by(ID) %>%
  mutate(total = sum(pixel_count),
         percent = round(100 * pixel_count / total, 1)) %>%
  select(ID, class, percent) %>%
  pivot_wider(names_from = class, values_from = percent, names_prefix = "height_")

# Spalten sinnvoll umbenennen
class_percent <- class_percent %>%
  rename(
    height_le_15cm = height_1,
    height_15_40cm = height_2,
    height_gt_40cm = height_3
  )

write.csv(class_percent, "C:/Users/Asus/R/Forschungsprojekt Trocken Heiden/Vegetationshoehe/class_percent_veg_hoehen.csv", row.names = FALSE)




########Offenbodenstellen###
# NDVI-Maske für sehr niedrige Vegetationsbedeckung
bare_soil_mask <- ndvi < 0.1

plot(bare_soil_mask, col = c("white", "brown"),
     main = "Potenzielle Offenboden-Flächen")


########Stat species Index######
##### objektbasierte Texturanalyse mit GLCM (Grey-Level Co-occurrence Matrix) auf einem RGB-Bild in R durchführen kannst, um z. B. Vegetationsstruktur oder Offenboden-Muster zu erkennen

# Installation (nur 1x nötig)
# install.packages("glcm")

library(terra)
install.packages("glcm")
library(glcm)

# 1️⃣ Lade das RGB-Raster (z. B. nur Red-Kanal für Textur)
rgb_stack <- DOP_Senne_Test

# Beispiel: Red-Kanal (oder auch NDVI, falls verfügbar)
red_band <- rgb_stack[[1]]  # oder z. B. NDVI

# 2️⃣ Konvertiere zu RasterLayer, falls nötig
red_raster <- raster::raster(red_band)

# 3️⃣ GLCM-Textur berechnen (z. B. „variance“ im 5x5 Fenster)
textur_var <- glcm(red_raster, window = c(5, 5), statistics = c("variance"))

# 4️⃣ Plot
plot(textur_var, main = "Textur (Varianz) im Red-Band")

##weiter Texturmaße📊 Weitere interessante Texturmaße:
#"contrast" – Unterschiede in Nachbarschaft (strukturreich)

#"homogeneity" – Maß für Gleichförmigkeit (offener Boden oft sehr homogen!)

#"entropy" – Zufälligkeit (höher bei Mischvegetation)

texturen <- glcm(red_raster, window = c(5,5),
                 statistics = c("contrast", "homogeneity", "entropy"))
#Optional: Auf Polygone aggregieren
#Wenn du bereits shape_Trockenheide o. ä. hast, kannst du z. B. pro Fläche den Mittelwert der Textur berechnen:
textur_mean <- terra::extract(rast(textur_var), vect(shape_Trockenheide), fun = mean, na.rm = TRUE)


