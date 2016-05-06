

# COUNTIES IN THE MSA

Syracuse, NY (Metropolitan Statistical Area) (45060)

Madison, NY [36053]
Onondaga, NY [36067]
Oswego, NY [36075]




# CENSUS TABLES

Median monthly housing costs B25105, median costs HD01_VD01

Selected Housing Characteristics: DP04

Means of Transit to Work: B08301

Travel Time to Work: B08303, total travel time HD01_VD01

Educational Attainment: S1501, % with bachelor degree HC01_EST_VC05

Age: S0101

Employment census data by tract only available for 2000.

https://ask.census.gov/faq.php?id=5000&faqId=1185




# LOAD PACKAGES

library( maptools )
library( maps )
library( sp )


# LOAD SHAPEFILES

setwd( "C:/Users/jdlecy/Dropbox/02 - CLASSES/02 - MASTERS/05 - Urban Policy/04 - Assignments/03 - Syracuse Demographics" )

ny <- readShapePoly( fn="tl_2014_36_tract",
                       proj4string=CRS("+proj=longlat +datum=WGS84") )

these <- c("053", "067", "075" )

syr <- ny[ ny$COUNTYFP %in% these, ]

shp.id <- as.character(syr$GEOID)

roads <- readShapeLines( fn="tl_2014_36_prisecroads",
                       proj4string=CRS("+proj=longlat +datum=WGS84") )

lakes <- readShapePoly( fn="tl_2014_36067_areawater",
                       proj4string=CRS("+proj=longlat +datum=WGS84") )





# CREATE MAP LABELS

map.labels <- rbind( 
c("Onondaga Lake", 43.092381, -76.210240),
c("Syracuse", 43.048533, -76.147262 ),
c("Fayetteville", 43.029950, -76.004988 ),
c("Manlius", 43.001783, -75.976221 ),
c("Airport", 43.113727, -76.110653 ),
c("Liverpool", 43.106458, -76.217083 ),
c("Solvay", 43.058040, -76.207452 ),
c("Southside", 43.020050, -76.161028 ),
c("Camillus", 43.039814, -76.306125 ),
c("North Syracuse", 43.134152, -76.130268 )
)

map.labels <- as.data.frame( map.labels, stringsAsFactors=F )
names( map.labels ) <- c("Name","y","x")
map.labels$x <- as.numeric(map.labels$x)
map.labels$y <- as.numeric(map.labels$y)







# CREATE COLOR SCALE

color.function <- colorRampPalette( c(rgb(207,211,224,max=255),rgb(5,55,105,max=255) ) )

col.vals <- color.function(5)





# UPDATED COMPASS ROSE FUNCTION - add color option
#
# function to add a compass rose to the map

compassRose<-function(x,y,rot=0,cex=1,my.col="dark gray") { 
  oldcex<-par(cex=cex) 
  mheight<-strheight("M") 
  xylim<-par("usr") 
  plotdim<-par("pin") 
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1] 
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180 
  crspans<-rep(c(mheight*3,mheight/2),4) 
  xpoints<-cos(point.angles)*crspans*xmult+x 
  ypoints<-sin(point.angles)*crspans+y 
  polygon(xpoints,ypoints,border=my.col) 
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x 
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y 
  text(txtxpoints,txtypoints,c("E","N","W","S"),col=my.col) 
  par(oldcex) 
} 






# UPDATED MAP SCALE FUNCTION - add color option

# use fix( map.scale ) to get code

map.scale <- function (x, y, relwidth = 0.15, metric = TRUE, ratio = TRUE, 
                       my.color="dark gray", ...) 
{
    format.pretty <- function(x, digits = 2) {
        x = signif(x, 2)
        prettyNum(formatC(x, format = "fg", digits = digits), 
            big.mark = ",")
    }
    usr <- par("usr")
    if (missing(y)) 
        y <- (9 * usr[3] + usr[4])/10
    if (abs(y) >= 90) 
        warning("location of scale out of this world!")
    if (missing(x)) 
        x <- (9 * usr[1] + usr[2])/10
    cosy <- cos((2 * pi * y)/360)
    perdeg <- (2 * pi * (6356.78 + 21.38 * cosy) * cosy)/360
    scale <- (perdeg * 1e+05)/(2.54 * (par("pin")/diff(par("usr"))[-2])[1])
    if (metric) 
        unit <- "km"
    else {
        perdeg <- perdeg * 0.6213712
        unit <- "mi"
    }
    len <- perdeg * relwidth * (usr[2] - usr[1])
    ats <- pretty(c(0, len), n = 2)
    nats <- length(ats)
    labs <- as.character(ats)
    labs[nats] <- paste(labs[nats], unit)
    linexy <- matrix(NA, ncol = 2, nrow = 3 * nats)
    colnames(linexy) <- c("x", "y")
    cxy <- par("cxy")
    dy <- cxy[2] * par("tcl")
    dx <- ats[nats]/perdeg/(nats - 1)
    linexy[1, ] <- c(x, y)
    linexy[2, ] <- c(x, y + dy)
    for (i in 1:(nats - 1)) {
        linexy[3 * i, ] <- c(x + (i - 1) * dx, y)
        linexy[3 * i + 1, ] <- c(x + i * dx, y)
        linexy[3 * i + 2, ] <- c(x + i * dx, y + dy)
    }
    lines(linexy, col=my.color )
    text(x + ats/perdeg, y + dy - 0.5 * cxy[2], labs, adj = c(0.4, 
        0.5), col=my.color, ...)
    if (ratio) 
        text(x, y + 0.5 * cxy[2], paste("scale approx 1:", format.pretty(scale), 
            sep = ""), adj = 0, ...)
    invisible(scale)
}



  


# POP OVER 60

census.dat <- read.csv( "ACS_13_5YR_S0101_with_ann.csv", colClasses="character" )

census.dat <- census.dat[ -1 , ] # get rid of annotations

fips <- substr( census.dat$GEO.id2, 3, 5 )

these <- c("053", "067", "075" )

census.dat <- census.dat[ fips %in% these , ]

geo.id <- census.dat$GEO.id2

over.60 <- as.numeric(census.dat$HC01_EST_VC29)

my.breaks <- quantile( over.60, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1 ), na.rm=T )

my.breaks

my.breaks[1] <- my.breaks[1] - 0.01

col.vector <- as.character(cut( over.60, breaks=my.breaks, labels=col.vals ) )

order <- match( shp.id, geo.id  )

geo.id <- geo.id[ order ]

col.vector <- col.vector[ order ]


plot( syr, border="gray", col=col.vector, 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )


 
 
pdf( "Syracuse Over 60 Population.pdf" )


plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.7), 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )
                   
plot( roads, add=T, col=adjustcolor("dark gray", alpha.f=0.3), lwd=1.5 )                      

plot( lakes, add=T, col=adjustcolor("gray80", alpha.f=0.4), 
      border=adjustcolor("gray80", alpha.f=0.4))

title( "Population Over the Age of 60 in Syracuse, NY" )

legend.text=c(" 2-12 %"," 12-21 %"," 21-29%","29-42 %", "42-78%")

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.7), 
        title="Pop Over Age 60")

compassRose(x=-75.94184,y=43.18622, rot=0, cex=0.3)

map.scale( metric=F, ratio=F, relwidth = 0.10, cex=0.5, my.color="gray60" )

text( map.labels$x, map.labels$y, labels=toupper(map.labels$Name), col="white", cex=0.5 )

dev.off()





# CHILD DEPENDENCY RATIO

census.dat <- read.csv( "ACS_13_5YR_S0101_with_ann.csv", colClasses="character" )

census.dat <- census.dat[ -1 , ] # get rid of annotations

fips <- substr( census.dat$GEO.id2, 3, 5 )

these <- c("053", "067", "075" )

census.dat <- census.dat[ fips %in% these , ]

geo.id <- census.dat$GEO.id2

child.ratio <- as.numeric(census.dat$HC01_EST_VC39)

my.breaks <- quantile( child.ratio, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1 ), na.rm=T )

my.breaks

my.breaks[1] <- my.breaks[1] - 0.01

col.vector <- as.character(cut( child.ratio, breaks=my.breaks, labels=col.vals ) )

order <- match( shp.id, geo.id  )

geo.id <- geo.id[ order ]

col.vector <- col.vector[ order ]


plot( syr, border="gray", col=col.vector, 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )


 
 
 
# START OF PDF 
 
pdf( "Syracuse Child Dependency Ratio2.pdf" )

plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.8), 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )
                   
plot( roads, add=T, col=adjustcolor("dark gray", alpha.f=0.3), lwd=1.5 )                      

plot( lakes, add=T, col=adjustcolor("gray80", alpha.f=0.4), 
      border=adjustcolor("gray80", alpha.f=0.4))
                     

title( "Child Dependency Ratio in Syracuse, NY" )

legend.text=c(" < 1 %"," 0.1-2 %"," 2-4%","4-9 %", "9-65%")

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.8), 
        title="Child Dep. Ratio")

compassRose(x=-75.94184,y=43.18622, rot=0, cex=0.3)

map.scale( metric=F, ratio=F, relwidth = 0.10, cex=0.5, my.color="gray60" )

text( map.labels$x, map.labels$y, labels=toupper(map.labels$Name), col="white", cex=0.5 )

dev.off()








# BACHELORs DEGREE OR HIGHER


census.dat <- read.csv( "ACS_13_5YR_S1501_with_ann.csv", colClasses="character" )

census.dat <- census.dat[ -1 , ] # get rid of annotations

fips <- substr( census.dat$GEO.id2, 3, 5 )

these <- c("053", "067", "075" )

census.dat <- census.dat[ fips %in% these , ]

geo.id <- census.dat$GEO.id2

college <- as.numeric(census.dat$HC01_EST_VC05)


my.breaks <- quantile( college, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1 ), na.rm=T )

my.breaks

my.breaks[1] <- my.breaks[1] - 0.01

col.vector <- as.character(cut( college, breaks=my.breaks, labels=col.vals ) )

order <- match( shp.id, geo.id  )

geo.id <- geo.id[ order ]

col.vector <- col.vector[ order ]


plot( syr, border="gray", col=col.vector, 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )


 
 
pdf( "Syracuse Education Level College.pdf" )

plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.7), 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )
                   
plot( roads, add=T, col=adjustcolor("dark gray", alpha.f=0.3), lwd=1.5 )                      

plot( lakes, add=T, col=adjustcolor("gray80", alpha.f=0.4), 
      border=adjustcolor("gray80", alpha.f=0.4))

title( "Population with a College Degree in Syracuse, NY" )

legend.text=c(" 0-3 %"," 3-7 %"," 7-13%","13-22 %", "22-66%")

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.7), 
        title="Pop with College Degree")

compassRose(x=-75.94184,y=43.18622, rot=0, cex=0.3)

map.scale( metric=F, ratio=F, relwidth = 0.10, cex=0.5, my.color="gray60" )

text( map.labels$x, map.labels$y, labels=toupper(map.labels$Name), col="white", cex=0.5 )



plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.7) )
               
plot( roads, add=T, col=adjustcolor("dark gray", alpha.f=0.3), lwd=1.5 )                      

plot( lakes, add=T, col=adjustcolor("gray80", alpha.f=0.4), 
      border=adjustcolor("gray80", alpha.f=0.4))
                     

title( "Population with a College Degree" )

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.7), 
        title="Pop with College Degree")


dev.off()




  

# MEDIAN HOUSING COSTS

census.dat <- read.csv( "ACS_13_5YR_B25105_with_ann.csv", colClasses="character" )

census.dat <- census.dat[ -1 , ] # get rid of annotations

fips <- substr( census.dat$GEO.id2, 3, 5 )

these <- c("053", "067", "075" )

census.dat <- census.dat[ fips %in% these , ]

geo.id <- census.dat$GEO.id2

med.house.cost <- as.numeric(census.dat$HD01_VD01)


my.breaks <- quantile( med.house.cost, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1 ), na.rm=T )

my.breaks

my.breaks[1] <- my.breaks[1] - 0.01

col.vector <- as.character(cut( med.house.cost, breaks=my.breaks, labels=col.vals ) )

order <- match( shp.id, geo.id  )

geo.id <- geo.id[ order ]

col.vector <- col.vector[ order ]


plot( syr, border="gray", col=col.vector, 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )



pdf( "Syracuse Median Monthly Housing Cost.pdf" )

plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.7), 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )
                   
plot( roads, add=T, col=adjustcolor("dark gray", alpha.f=0.3), lwd=1.5 )                      

plot( lakes, add=T, col=adjustcolor("gray80", alpha.f=0.4), 
      border=adjustcolor("gray80", alpha.f=0.4))
                     

title( "Median Monthly Housing Cost in Syracuse, NY" )

legend.text=c("$278-$730"," $730-$826","$826-$950","$950-$1088", "$1088-1858%")

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.7), 
        title="Monthly Housing Cost")

compassRose(x=-75.94184,y=43.18622, rot=0, cex=0.3)

map.scale( metric=F, ratio=F, relwidth = 0.10, cex=0.5, my.color="gray60" )

text( map.labels$x, map.labels$y, labels=toupper(map.labels$Name), col="white", cex=0.5 )

dev.off()









# TRAVEL MODE - public transit

census.dat <- read.csv( "ACS_13_5YR_B08301_with_ann.csv", colClasses="character" )

census.dat <- census.dat[ -1 , ] # get rid of annotations

fips <- substr( census.dat$GEO.id2, 3, 5 )

these <- c("053", "067", "075" )

census.dat <- census.dat[ fips %in% these , ]

geo.id <- census.dat$GEO.id2

summary( as.numeric(census.dat$HD01_VD10)/as.numeric(census.dat$HD01_VD01) )

pub.transit <- as.numeric(census.dat$HD01_VD10) / as.numeric(census.dat$HD01_VD01)


my.breaks <- quantile( pub.transit, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1 ), na.rm=T )

my.breaks

my.breaks[1] <- my.breaks[1] - 0.01

col.vector <- as.character(cut( pub.transit, breaks=my.breaks, labels=col.vals ) )

order <- match( shp.id, geo.id  )

geo.id <- geo.id[ order ]

col.vector <- col.vector[ order ]


plot( syr, border="gray", col=col.vector, 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )



# START PDF

pdf( "Syracuse Public Transit Reliance.pdf" )

plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.7), 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )
                   
plot( roads, add=T, col=adjustcolor("dark gray", alpha.f=0.3), lwd=1.5 )                      

plot( lakes, add=T, col=adjustcolor("gray80", alpha.f=0.4), 
      border=adjustcolor("gray80", alpha.f=0.4))
                    

title( "Percentage of Population Reliant on Public Transit for Commuting" )

legend.text=c(" 0.0-0.5 %"," 0.5-1.0 %"," 1.0-2.5 %"," 2.5-4.0 %", "4.0-16.0 %")

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.7), 
        title="Population Reliant on Public Transit")

compassRose(x=-75.94184,y=43.18622, rot=0, cex=0.3)

map.scale( metric=F, ratio=F, relwidth = 0.10, cex=0.5, my.color="gray60" )

text( map.labels$x, map.labels$y, labels=toupper(map.labels$Name), col="white", cex=0.5 )



plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.7) )

title( "Percentage of Population Reliant on Public Transit for Commuting" )

plot( roads, add=T, col="dark gray", lwd=1.5 )                      

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.7), 
        title="Population Reliant on Public Transit")
        
dev.off()







# TRAVEL TIME - median HD01_VD01

census.dat <- read.csv( "ACS_13_5YR_B08303_with_ann.csv", colClasses="character" )

# t( census.dat[ 1, 1:20 ] )

census.dat <- census.dat[ -1 , ] # get rid of annotations

fips <- substr( census.dat$GEO.id2, 3, 5 )

these <- c("053", "067", "075" )

census.dat <- census.dat[ fips %in% these , ]

geo.id <- census.dat$GEO.id2

travel.time <- ( as.numeric( census.dat$HD01_VD08 ) + 
                 as.numeric( census.dat$HD01_VD09 ) + 
                 as.numeric( census.dat$HD01_VD10 ) + 
                 as.numeric( census.dat$HD01_VD11 ) + 
                 as.numeric( census.dat$HD01_VD12 ) + 
                 as.numeric( census.dat$HD01_VD13 ) ) / 
                 as.numeric( census.dat$HD01_VD01 )

summary( travel.time )

my.breaks <- quantile( travel.time, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1 ), na.rm=T )

my.breaks[1] <- my.breaks[1] - 1



col.vector <- as.character(cut( travel.time, breaks=my.breaks, labels=col.vals ) )

order <- match( shp.id, geo.id  )

geo.id <- geo.id[ order ]

travel.time <- travel.time[ order ]

col.vector <- col.vector[ order ]


plot( syr, border="gray", col=col.vector, 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )






pdf( "Syracuse Median Commute Time.pdf" )

plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.7), 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )
                   
plot( roads, add=T, col=adjustcolor("dark gray", alpha.f=0.3), lwd=1.5 )                      

plot( lakes, add=T, col=adjustcolor("gray80", alpha.f=0.4), 
      border=adjustcolor("gray80", alpha.f=0.4))

title( "Commutes Over 30 Minutes in Syracuse" )

legend.text=c(" < 11 %"," 11-16 %"," 16-22 %"," 22-33 %", "33-100 %")

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.7), 
        title="Commute Times Over 30 Min")

compassRose(x=-75.94184,y=43.18622, rot=0, cex=0.3)

map.scale( metric=F, ratio=F, relwidth = 0.10, cex=0.5, my.color="gray60" )

text( map.labels$x, map.labels$y, labels=toupper(map.labels$Name), col="white", cex=0.5 )

dev.off()








# EXAMPLE OF POOR FIGURE AND GROUND - CHILD DEPENDENCY RATIO

census.dat <- read.csv( "ACS_13_5YR_S0101_with_ann.csv", colClasses="character" )

census.dat <- census.dat[ -1 , ] # get rid of annotations

fips <- substr( census.dat$GEO.id2, 3, 5 )

these <- c("053", "067", "075" )

census.dat <- census.dat[ fips %in% these , ]

geo.id <- census.dat$GEO.id2

child.ratio <- as.numeric(census.dat$HC01_EST_VC39)

my.breaks <- quantile( child.ratio, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1 ), na.rm=T )

my.breaks

my.breaks[1] <- my.breaks[1] - 0.01

col.vector <- as.character(cut( child.ratio, breaks=my.breaks, labels=col.vals ) )

order <- match( shp.id, geo.id  )

geo.id <- geo.id[ order ]

col.vector <- col.vector[ order ]


plot( syr, border="gray", col=col.vector, 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )


 
 
 
# START OF PDF 
 
pdf( "Poor Ground and Figure.pdf" )

plot( syr, border="gray", col=col.vector, 
      xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )
      
# plot( syr, border="NA", col=adjustcolor(col.vector, alpha.f = 0.7), 
#       xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )
                   
plot( roads, add=T, col="dark gray", lwd=1.5 )                      

plot( lakes, add=T, col="gray80", border="gray80" )
                     

title( "Child Dependency Ratio in Syracuse, NY" )

legend.text=c(" < 1 %"," 0.1-2 %"," 2-4%","4-9 %", "9-65%")

legend( "bottomright",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=legend.text, bg="white", box.col="gray",
        col=adjustcolor(col.vals, alpha.f = 0.7), 
        title="Child Dep. Ratio")

compassRose(x=-75.94184,y=43.18622, rot=0, cex=0.3, my.col="black")

map.scale( metric=F, ratio=F, relwidth = 0.10, cex=0.5, my.color="black" )

text( map.labels$x, map.labels$y, labels=toupper(map.labels$Name), col="black", cex=0.5 )

dev.off()






