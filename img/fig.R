
library(spatstat)
library(maptools)
library(raster)
library(cartography)
library(sf)

load("data/paname.RData")

wh <- getFigDim(x = paris, width = 1000, mar = c(0,0,1.2,0), res = 100)
bb <- as(bars, "Spatial")
bbowin <- as.owin(as(paris, "Spatial"))
pts <- coordinates(bb)
p <- ppp(pts[,1], pts[,2], window=bbowin)
ds <- density.ppp(p, sigma = 200, eps = c(20,20))
rasdens <- raster(ds) * 1000 * 1000
rasdens <- rasdens+1
bks <- getBreaks(values(rasdens), nclass = 12, method = "equal")
cols <- colorRampPalette(c("black","#940000", "white"))(length(bks)-1)
png("img/barsOSM.png", width = wh[1], height = wh[2], res = 100, bg = NA)
par(mar = c(0,0,1.2,0))
plot(paris, col = NA, border = NA, main="", bg = "#FBEDDA")
plot(rasdens, breaks= bks, col=cols, add = T,legend=F)
legendChoro(pos = "topright",cex = 0.7, title.cex = 0.7,
            title.txt = "Bar density\nKDE, sigma=200m\n(bars per km2)",
            breaks = bks-1, nodata = FALSE,values.rnd = 0, 
            col = cols)
plot(seine, col = "#AAD3DF", add=T, lwd = 4)
plot(parc, col = "#CDEBB235", border = NA, add=T)
plot(quartier$geometry, add=T, border = "white",lty = 2, lwd = 0.1)
plot(bars, add=T, col = "#0000ff90", pch = 20, cex = 0.1)
plot(paris, col = NA, add=T)
north(pos = c(661171.8,6858500))
layoutLayer(title = "Bar Density in Paris", scale = 1,
            tabtitle = TRUE, frame = FALSE,
            author = "Map data © OpenStreetMap contributors, under CC BY SA.", 
            sources = "cartography 2.0.2") 
dev.off()
