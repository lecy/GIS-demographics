# GIS-demographics

Build a basic choropleth map in R.

## Step 01

Download data from the Census for Syracuse NY: http://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t

* Median monthly housing costs B25105, median costs HD01_VD01
* Selected Housing Characteristics: DP04
* Means of Transit to Work: B08301
* Travel Time to Work: B08303, total travel time HD01_VD01
* Educational Attainment: S1501, % with bachelor degree HC01_EST_VC05
* Age: S0101
* Employment census data by tract only available for 2000.


## Step 02

Download Tiger shapefiles for Syracuse NY:  https://www.census.gov/geo/maps-data/data/tiger-line.html

* Census tracts
* Water bodies
* Primary roads

## Step 03

Load the data:

```R
census.dat <- read.csv( "ACS_13_5YR_S0101_with_ann.csv", colClasses="character" )

census.dat <- census.dat[ -1 , ] # get rid of annotations

fips <- substr( census.dat$GEO.id2, 3, 5 )

these <- c("053", "067", "075" )

census.dat <- census.dat[ fips %in% these , ]

geo.id <- census.dat$GEO.id2

over.60 <- as.numeric(census.dat$HC01_EST_VC29)

```


Create a color scheme:

```R
color.function <- colorRampPalette( c(rgb(207,211,224,max=255),rgb(5,55,105,max=255) ) )

col.vals <- color.function(5)
```

Cut your data into quantiles:

```R
# identify break points for the 'population over 60' variable

my.breaks <- quantile( over.60, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1 ), na.rm=T )

# add a buffer 

my.breaks[1] <- my.breaks[1] - 0.01

# create a color vector based upon the breaks

col.vector <- as.character(cut( over.60, breaks=my.breaks, labels=col.vals ) )

```

Plot your data:

```R
order <- match( shp.id, geo.id  )

col.vector <- col.vector[ order ]

plot( syr, border="gray", col=col.vector,  xlim=c(-76.31331,-75.89993), ylim=c(42.97662, 43.14608) )

```

![alt text](https://github.com/lecy/GIS-demographics/blob/master/example.png)

