# Graph 3 - 4 countries
# central african republic, morocco, niger, south africa 

# -- DATASET --

dta.g3 <- data.frame(africa_data[,c("Inequality.in.education...",
                         "Inequality.in.income....", 
                         "Inequality.in.life.expectancy.....2010.2015", 
                         "Gender.Development.Index.value")])[,-5]

rownames(dta.g3) <- africa_data$sovereignt

max.min = data.frame(rbind (c(100,100,100, 1), c(0,0,0,0)))
colnames(max.min) <- colnames(dta.g3)


dta.g3.CAR <- rbind(max.min, dta.g3["Central African Republic",])
dta.g3.MOR <- rbind(max.min, dta.g3["Morocco",])
dta.g3.NIG <- rbind(max.min, dta.g3["Niger",])
dta.g3.RSA <- rbind(max.min, dta.g3["South Africa",])

# Changement des noms de colonnes
colnames(dta.g3.CAR) <- c("Education", 
                          "Revenus", 
                          "Espérance de vie", 
                          "GDI")
colnames(dta.g3.MOR) <- c("Education", 
                          "Revenus", 
                          "Espérance de vie",
                          "GDI")
colnames(dta.g3.NIG) <- c("Education", 
                          "Revenus", 
                          "Espérance de vie",
                          "GDI")
colnames(dta.g3.RSA) <- c("Education", 
                          "Revenus", 
                          "Espérance de vie",
                          "GDI")

library(fmsb)

par(mfrow = c(2,2), omi=c(0,0.25,0,0.25),oma = c(0,0,2,0),mar = c(0,0,2,0))

# =========================
# Central African Republic 

colCAR = c("#E64814")
areaCAR = rgb(232,120,84, alpha = 120, maxColorValue = 255 )

radarchart(dta.g3.CAR[-4], 
           # -- GRID --
           cglty = 2,       # Grid line type
           cglcol = "grey", # Grid line color
           
           # -- LINE -- 
           pcol = colCAR,   # Color for each line
           plwd = 2,        # Width for each line
           plty = 1 ,       # Line type for each line
           
           # -- AREA --
           pfcol = areaCAR,
           
           # -- TEXTS --
           title = "République Centre Africaine",
           maxmin = TRUE)

text(0,1.07,paste(dta.g3.CAR[3,1]), col = colCAR)
text(-1.03,-0.75,paste(dta.g3.CAR[3,2]), col = colCAR)
text(1.03,-0.75,paste(dta.g3.CAR[3,3]), col = colCAR)
text(1.4,0.4, paste("GDI :", dta.g3.CAR[3,4]), cex = 1, family = "sans", font = 4)

plotimage("D:/Documents/3A/visualisation/femme.png", 
          size = 1, pos = 1)
# par(omi=c(0,0.25,0,0.25),oma = c(0,0,2,0),mar = c(0,0,2,0),
#     family="Arial", las=1)

# =======
# Morocco

colMOR = c("#E64814")
areaMOR = rgb(232,120,84, alpha = 120, maxColorValue = 255 )

radarchart(dta.g3.MOR[-4], 
           # -- GRID --
           cglty = 2,       # Grid line type
           cglcol = "grey", # Grid line color
           
           # -- LINE -- 
           pcol = colMOR,   # Color for each line
           plwd = 2,        # Width for each line
           plty = 1 ,       # Line type for each line
           
           # -- AREA --
           pfcol = areaMOR,
           
           # -- TEXTS --
           title = "Maroc", 
           maxmin = TRUE,
           
           # -- AXES --
           
           axistype = 2,
           axislabcol = colMOR)

# par(omi=c(0,0.25,0,0.25),oma = c(0,0,2,0),mar = c(0,0,2,0),
#     family="Arial", las=1)


# =======
# Niger

colNIG = c("#E64814")
areaNIG = rgb(232,120,84, alpha = 120, maxColorValue = 255 )

radarchart(dta.g3.NIG[-4], 
           # -- GRID --
           cglty = 2,       # Grid line type
           cglcol = "grey", # Grid line color
           
           # -- LINE -- 
           pcol = colNIG,   # Color for each line
           plwd = 2,        # Width for each line
           plty = 1 ,       # Line type for each line
           
           # -- AREA --
           pfcol = areaNIG,
           
           # -- TEXTS --
           title = "Niger", 
           maxmin = TRUE,
           
           # -- AXES --
           
           axistype = 2,
           axislabcol = colNIG)

# par(omi=c(0,0.25,0,0.25),oma = c(0,0,2,0),mar = c(0,0,2,0),
#     family="Arial", las=1)

# =======
# South Africa

colRSA = c("#E64814")
areaRSA = rgb(232,120,84, alpha = 120, maxColorValue = 255 )

radarchart(dta.g3.RSA[-4], 
           # -- GRID --
           cglty = 2,       # Grid line type
           cglcol = "grey", # Grid line color
           
           # -- LINE -- 
           pcol = colRSA,   # Color for each line
           plwd = 2,        # Width for each line
           plty = 1 ,       # Line type for each line
           
           # -- AREA --
           pfcol = areaRSA,
           
           # -- TEXTS --
           title = "Afrique du Sud", 
           maxmin = TRUE,
           
           # -- AXES --
           
           axistype = 2,
           axislabcol = colRSA)

# par(omi=c(0,0.25,0,0.25),oma = c(0,0,2,0),mar = c(0,0,2,0),
#     family="Arial", las=1)


# GDI 
# c(CAR = dta.g3.CAR$GDI[3], MOR = dta.g3.MOR$GDI[3], NIG = dta.g3.NIG$GDI[3], RSA = dta.g3.RSA$GDI[3])


# ===============
# Images pour GDI
# ===============



plotimage <- function(file, x = NULL, y = NULL, size = 1, add = FALSE,
                      angle = 0, pos = 0, bg = "lightgray", ...) {
  if (length(grep(".png", file)) > 0) {
    require("png")
    img <- readPNG(file, native = TRUE)
  }
  if (length(grep(".tif", file)) > 0) {
    require("tiff")
    img <- readTIFF(file, native = TRUE)
  }
  if (length(grep(".jp", file)) > 0) {
    require("jpeg")
    img <- readJPEG(file, native = TRUE)
  }
  
  res <- dim(img)[2:1]
  
  if (add) {
    xres <- par()$usr[2] - par()$usr[1]
    yres <- par()$usr[4] - par()$usr[3]
    res <- c(xres, yres)
  } else {
    par(mar = c(1, 1, 1, 1), bg = bg, xaxs = "i", yaxs = "i")
    dims <- c(0, max(res))
    plot(0, type = "n", axes = F, xlim = dims, ann = F, ylim = dims,
         ...)
  }
  if (is.null(x) && is.null(y)) {
    if (pos == "center" || pos == 0) {
      x <- par()$usr[1] + (par()$usr[2] - par()$usr[1])/2
      y <- par()$usr[3] + (par()$usr[4] - par()$usr[3])/2
    }
    if (pos == "bottom" || pos == 1) {
      x <- par()$usr[1] + (par()$usr[2] - par()$usr[1])/2
      y <- par()$usr[3] + res[2] * size/2
    }
    if (pos == "left" || pos == 2) {
      x <- par()$usr[1] + res[1] * size/2
      y <- par()$usr[3] + (par()$usr[4] - par()$usr[3])/2
    }
    if (pos == "top" || pos == 3) {
      x <- par()$usr[1] + (par()$usr[2] - par()$usr[1])/2
      y <- par()$usr[4] - res[2] * size/2
    }
    if (pos == "right" || pos == 4) {
      x <- par()$usr[2] - res[1] * size/2
      y <- par()$usr[3] + (par()$usr[4] - par()$usr[3])/2
    }
    if (pos == "bottomleft" || pos == 5) {
      x <- par()$usr[1] + res[1] * size/2
      y <- par()$usr[3] + res[2] * size/2
    }
    if (pos == "topleft" || pos == 6) {
      x <- par()$usr[1] + res[1] * size/2
      y <- par()$usr[4] - res[2] * size/2
    }
    if (pos == "topright" || pos == 7) {
      x <- par()$usr[2] - res[1] * size/2
      y <- par()$usr[4] - res[2] * size/2
    }
    if (pos == "bottomright" || pos == 8) {
      x <- par()$usr[2] - res[1] * size/2
      y <- par()$usr[3] + res[2] * size/2
    }
  }
  xx <- res[1] * size/2
  yy <- res[2] * size/2
  rasterImage(img, x - xx, y - yy, x + xx, y + yy, angle = angle)
}


plotimage("D:/Documents/3A/visualisation/femme.png", 
          size = 1, pos = 1)