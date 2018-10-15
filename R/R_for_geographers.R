# R for geographers
## An exposition 
library(tidyverse); library(lubridate); 


# Basic plumbing with tidyverse style methods -----------------------------
# This is a 'pipe' symbol: %>% 
# The keyboard shortcut in Rstudio is ctrl+shift+m
# pipes allow chaining operations to an object

# non pipe way
hist(log(rnorm(1000, mean = 100, sd=3)))

# pipe way 
#  vector object            -> function -> function
rnorm(1000, mean=100, sd=3) %>% log() %>% hist()

# So pipes allow us to arbitrarily long things without nesting or creating
# copyies of dataframes


# Life before and after dplyr ---------------------------------------------
# excellent tutorial: [https://suzan.rbind.io/categories/tutorial/]

# - Tibbles vs dataframes - 
# tibbles are a more clever version of data frames that offer some features (?tibble)
# but also, are compatible for tidyverse methods
junk_df <- data.frame(x=rnorm(1000, mean = 0, sd=10), 
                   y=rnorm(1000, mean=10, sd=1))
print(junk_df) # prints the whole df!

junk_tb <- tibble(x=rnorm(1000, mean = 0, sd=10), 
                      y=rnorm(1000, mean=10, sd=1))
print(junk_tb) # just prints the top 10 rows

# - Filtering - 
# filtering data before dplyr: 
iris[iris[,'Species']=="setosa" & iris[,"Sepal.Length"] > 5.0,]
# filtering with dplyr
iris %>% filter(Species=="setosa" & Sepal.Length > 5.0)

# - Creating or transforming variables - 
iris <- iris %>% 
  mutate(p_wl_ratio = Petal.Width/Petal.Length) %>% 
  mutate(narrow = ifelse(p_wl_ratio < 0.25, TRUE, FALSE))

# - Renaming variables - 
iris %>% names()
iris %>% 
  rename_all(tolower) %>% # rename cols with lowercase
  head()

# - sample random rows - 
iris %>% 
  sample_n(10)

# - Summarize groups with a statistic - 
iris %>% # count number of observations per species
  group_by(Species) %>% 
  summarize(nobs = n()) %>% 
  ungroup()

iris %>% # calc mean of traits per species
  group_by(Species) %>% 
  summarise_all(mean)
  
iris %>% # calc mean of traits across three species
  group_by(Species) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  select(-Species) %>% 
  summarize_all(mean)


# readr & tidyr for getting data into a workable shape ----------------------------

read.table("data/mei_1950_2018.data",skip = 1, nrows = 68) # base R way

mei_wide <- readr::read_table("data/mei_1950_2018.data", skip = 1, col_names = F, n_max = 68) # slightly easier tidyverse way

names(mei_wide) <- c("year",1:12) # add names to columns

mei_long <- mei_wide %>% # reshape the data frame from 'wide' to 'long' 
  gather(key="month",value="index",-year) %>% # use gather() to assemble key-value pairs
  mutate(date=parse_date_time(paste(year,month,1),"ymd")) # mutate the date

mei_long %>% 
  ggplot(data=., aes(date, index))+geom_line()


# Joining data frames by an identifier ------------------------------------
USArrests # no column name for state!
USArrests %>% class
rownames(USArrests)
dat1 <- USArrests %>% 
  mutate(state=rownames(USArrests))
dat2 <- MASS::road
dat2 <- MASS::road %>% 
  mutate(state=rownames(dat2))

pmatch(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)
pmatch(dat2$state, dat1$state, nomatch = NA_integer_, duplicates.ok = FALSE)

dat2[pmatch(dat2$state, dat1$state, duplicates.ok = FALSE),]


# ggPlotting El Niño --------------------------------------------------------
# "always plot your data" 
tmp <- read_csv("data/nino34_1870_2017.csv")

# plot the whole record
tmp %>% 
  #__________________x_____y_______thing to add to plot
  ggplot(data=., aes(date, index))+geom_line()

#plot record by month
tmp %>%
  ggplot(data=., aes(date, index))+
  geom_line()+
  geom_smooth(method='lm',se=F)+
  facet_wrap(~month)

# plot recent ENSO record
tmp %>%
  filter(year>=1990) %>% 
  ggplot(data=., aes(date, index))+
  geom_line()

# LETS 'smooth' the record with a moving average
library(RcppRoll)
tmp %>% 
  arrange(date) %>% 
  mutate(index_12mo = roll_meanr(index, n=12, fill=NA)) %>%  #
  filter(year>=1990) %>% 
  ggplot(data=., aes(date, index))+
  geom_line()+
  geom_line(aes(date, index_12mo),col='red',lwd=1.5)

# LET's 'deseasonlize' the record by subtracting the monthly mean
df_norms <- tmp %>%
  group_by(month) %>% 
  summarize(index_u = mean(index, na.rm=T))
tmp2 <- left_join(tmp, df_norms, by=c("month")) # now we join it back together

tmp2 %>% 
  mutate(index_ds = index-index_u) %>% 
  filter(year>=1990) %>% 
  ggplot(data=., aes(date, index))+
  geom_line()+
  geom_line(aes(date, index_ds), col='red') # so that actually didn't make much of a difference


# La Selva CARBONO plots data ---------------------------------------------
carb <- read_csv("data/claros1999_2012fulldataset.csv", skip = 5)
carb %>% glimpse()
carb <- carb %>% 
  rename(year=`*Year`) %>% 
  mutate(date = parse_date_time(Date, '%d-%m-%y')) %>% 
  select(-Date)

carb %>% # bad way
  filter(year==1999) %>% 
  ggplot(data=., aes(X_m, Y_m, color=height_cm))+
  geom_point()

carb %>% # better way
  filter(year==1999 & plot=="A1") %>% 
  ggplot(data=., aes(X_m, Y_m, fill=log10(height_cm)))+
  geom_raster()+
  scale_fill_viridis_c()

carb %>% # visualize through time
  filter(plot=='A1') %>%
  ggplot(data=., aes(X_m, Y_m, fill=log10(height_cm)))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_wrap(~year)

#REALLY VISUALIZE it 
library(gganimate)
carb %>% # visualize through time
  filter(year==1999) %>%
  ggplot(data=., aes(X_m, Y_m, fill=log10(height_cm)))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_wrap(~plot)

p <- carb %>% # visualize through time
  ggplot(data=., aes(X_m, Y_m, fill=log10(height_cm), frame=year))+
  geom_raster()+
  coord_equal()+
  scale_fill_viridis_c("Canopy Height [log cm]", option = 'B')+
  facet_wrap(~plot)+
  labs(title='Year: {frame_time}')
gganimate(p, "outputs/carbono_plot_heights.gif")


# Plot distributions ------------------------------------------------------
hist(carb$height_cm) # old base-R way to plot histogram
plot(density(carb$height_cm)) # base-R way to plot kernel density
carb %>% glimpse
carb %>% ggplot(data=., aes(x=height_cm))+geom_histogram()
carb %>% 
  filter(near(year,2000,tol = 0.1)) %>% # filtering for numbers can be tricky, use near to specify a filter with a tolerance
  ggplot(data=., aes(x=height_cm))+geom_histogram()+facet_wrap(~plot)
carb %>% 
  filter(near(year,2000,tol = 0.1)) %>% 
  ggplot(data=., aes(x= log1p(height_cm)))+
  geom_histogram(bins = 10)+
  scale_y_continuous(trans="log1p")+
  facet_wrap(~plot)


# Spatiotemporal example --------------------------------------------------
nasa

nasa %>% glimpse()

nasa %>% 
  as_tibble() %>% 
  ggplot(data=., aes(long,lat))+
  geom_raster(aes(fill=ozone))+
  coord_equal()+
  scale_fill_viridis_c() + 
  facet_wrap(~month)

nasa %>% 
  as_tibble() %>% 
  group_by(lat,long, month) %>% 
  summarize(u=mean(temperature,na.rm=T)) %>% 
  ggplot(data=., aes(long,lat))+
  geom_raster(aes(fill=u))+
  coord_equal()+
  scale_fill_viridis_c() + 
  facet_wrap(~month)

nasa %>% 
  as_tibble() %>% 
  group_by(lat,long, month) %>% 
  summarize(u=mean(temperature,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(tempC = u - 273.15) %>% 
  ggplot(data=., aes(long,lat))+
  geom_raster(aes(fill=tempC))+
  coord_equal()+
  scale_fill_viridis_c() + 
  facet_wrap(~month)

nasa %>% 
  as_tibble() %>% 
  group_by(lat,long, year,month) %>% 
  # summarize(u=mean(temperature,na.rm=T)) %>% 
  # ungroup() %>% 
  mutate(tempC = temperature - 273.15) %>% 
  mutate(hemi = cut(lat,breaks = c(-Inf,0,Inf),labels = c("SH","NH"))) %>% 
  group_by(hemi,year,month) %>% 
  summarize(u=mean(tempC,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(date=parse_date_time(paste(year,month,1),'ymd')) %>% 
  ggplot(data=., aes(date,u,color=hemi))+
  geom_line()+
  scale_fill_viridis_c() 


# Seals -------------------------------------------------------------------
data("seals")
seals %>% glimpse

seals %>% 
  mutate(distance=sqrt(delta_long**2 + delta_lat**2)) %>% 
  ggplot(., aes(long, lat, color=distance)) +
  geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat),
               arrow = arrow(length = unit(0.1,"cm")),lwd=2) +
  coord_equal()+
  borders("usa")+
  scale_x_continuous(limits = c(-150,-120))+
  scale_color_viridis_c()+
  theme_dark()

map(database = 'usa', regions = "california")


# storms ------------------------------------------------------------------
names(storms)

# bad!
storms %>% 
  ggplot(data=., aes(long,lat,size=category))+
  geom_point()+
  borders("world")+
  scale_x_continuous(limits=c(-120,0))+
  scale_y_continuous(limits = c(-10,55))+
  borders('world', fill = "brown")

# bad!
storms %>% 
  arrange(wind) %>% 
  ggplot(data=., aes(long,lat,size=category,color=wind))+
  borders("coast")+
  geom_point()+
  scale_color_viridis_c()+
  scale_x_continuous(limits=c(-130,0))+
  scale_y_continuous(limits = c(-10,55))

storms %>% group_by(year) %>% summarize(nobs=n()) %>% ggplot(data=., aes(year,nobs))+geom_line()


storms %>% group_by(month) %>% summarize(wind_25=quantile(wind,0.025), 
                                         wind_50=median(wind), 
                                         wind_75=quantile(wind, 0.95)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(month, wind_50))+
  geom_ribbon(aes(x=month, ymax=wind_75, ymin=wind_25),lty=0,alpha=0.25)+
  geom_line()+
  labs(x="Month",y="Wind speed [knots]",title = "95% quantile rate of hurricane wind speed")

# swap 'month' for 'year'
storms %>% group_by(year) %>% summarize(wind_25=quantile(wind,0.025), 
                                         wind_50=median(wind), 
                                         wind_75=quantile(wind, 0.95)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, wind_50))+
  geom_ribbon(aes(x=year, ymax=wind_75, ymin=wind_25),lty=0,alpha=0.25)+
  geom_line()+
  labs(x="Month",y="Wind speed [knots]",title = "95% quantile rate of hurricane wind speed")

# advanced!
p = c(0.025, 0.25,0.5,0.75,0.975)
storms %>% 
  group_by(month) %>% 
  summarise(quantiles = list(sprintf("%1.0f%%", p*100)),
            wind = list(quantile(wind, p))) %>% 
  unnest %>% 
  ggplot(data=., aes(month, wind, color=quantiles))+
  geom_line()+
  scale_color_viridis_d(end=0.8)


# PCA example with columns scaling -------------------------------------------------------------
iris %>% 
  prcomp(~.-Species, data=.) %>% # BAD!, vars need to be scaled
  biplot()
iris %>% 
  group_by(Species) %>% 
  mutate_all(scale) %>% 
  prcomp(~.-Species, data=.) %>% 
  biplot()

# Reshaping data example --------------------------------------------------
population %>% 
  filter(country=="Italy") %>% 
  ggplot(data=., aes(year, population))+geom_line()+geom_point()


# Super advanced dplyr ----------------------------------------------------
# inspired by: https://twitter.com/SuzanBaert and modified from: https://github.com/suzanbaert/RLadies_RoCur/blob/master/Dplyr_tricks.pdf

# using !! "bang"
vars <- c("lat","long","wind")
storms %>% select(!!vars)

# select columns by regex
who %>% names # lots of column names
who %>% select(country, year, matches("*2534")) # select country, year, and columns with '2534' in the name

# rename columns with regex
library(stringr);
iris %>% 
  as_tibble() %>% 
  rename_all(tolower) %>% 
  rename_all(~str_replace_all(., "\\.","_"))

# mutate *observation* names
storms %>% 
  select(name,year,status) %>% 
  mutate_all(tolower) %>% # Amy -> amy
  mutate_all(~str_replace_all(., " ","_")) # 'tropical depression' -> 'tropical_depression'

# find highest values
storms %>% 
  top_n(5, wind) # storms with 5 highest windspeeds

# making new vars from conditions
starwars %>%
  select(name, species, homeworld, birth_year, hair_color) %>%
  mutate(new_group = case_when(
    species == "Droid" ~ "Robot",
    homeworld == "Tatooine" & hair_color == "blond" ~ "Blond Tatooinian",
    homeworld == "Tatooine" ~ "Other Tatooinian",
    hair_color == "blond" ~ "Blond non-Tatooinian",
    TRUE ~ "Other Human"))


# Time Series Example with Sunspots ---------------------------------------------
sunspot.month %>% plot
sunspots %>% class # it's a timeseries object
sunspots %>% as.matrix()

library(maps)
maps::us.cities %>% glimpse()
us.cities %>% 
  filter(!country.etc %in% c("AK","HI")) %>% # filter to only lower 48 states
  ggplot(data=., aes(long,lat,color=as.factor(capital),size=pop))+
  geom_point(alpha=0.5)+
  coord_equal()+
  map('usa', boundary = T)+
  scale_color_viridis_d()

library(ggmap)
hdf <- get_map("houston, texas")
ggmap(hdf, extent = "normal")
ggmap(hdf) # extent = "panel", note qmap defaults to extent = "device"
ggmap(hdf, extent = "device")


# SPATIAL METHODS + TIDYVERSE ---------------------------------------------
install.packages("rgdal","")

# Required libraries: 
library(sf); 
library(tidyverse); 
library(lubridate); 

dir.create("data/SouthAmerica")
unzip(zipfile = "data/SouthAmerica.zip", exdir = "data/SouthAmerica")
list.files('data/SouthAmerica/')

SA <- sf::st_read("data/SouthAmerica/SouthAmerica.shp")

plot(SA) # blah, not ideal
plot(SA["SQKM"]) # base R method - a little better, but not so easy to control

ggplot() + geom_sf(data=SA, aes(fill=SQKM))+
  scale_fill_viridis_c()

ggplot() + geom_sf(data=SA, aes(fill=log(SQKM, base = 10)))+
  scale_fill_viridis_c()

SA %>% mutate(population=ifelse(POP2007>0, POP2007,1)) %>% 
  select(population) %>% pull(population)
  ggplot()+
  geom_sf(data=SA, aes(fill=population))+
  scale_fill_viridis_c()



SA %>% 
  ggplot(data=., aes())+geom_sf(fill="SQKM")+
  geom_point(data=data.frame(lat=0,lon=-80),aes(lat,lon),col='red')

ggplot(data=SA, aes())+geom_sf()+
  geom_point(data=data.frame(lat=0,lon=-80),aes(lat,lon),col='red')



# Calculate NDVI from Landsat 7 -------------------------------------------
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(r = raster::stack(tif))
raster::plot(r) # Not ideal.

l7 <- raster::as.data.frame(r, xy=T) %>% as_tibble()
l7 <- l7 %>%
  mutate(ndvi=(L7_ETMs.4-L7_ETMs.3)/(L7_ETMs.4+L7_ETMs.3))
l7 %>% 
  ggplot(data=., aes(x,y,fill=ndvi))+
  geom_raster()+
  coord_equal()+
  theme_bw()+
  scale_fill_viridis_c("NDVI")+
  labs(x="UTM X [m]",y="UTM Y [m]")+
  theme(legend.position = c(0.9,0.15),
        legend.title = element_text(size=15, face = 'bold'),
        legend.text = element_text(size=10),
        axis.title.x = element_text(size=25), 
        axis.text.x = element_text(size=15), 
        axis.title.y = element_text(size=25), 
        axis.text.y = element_text(size=15))



# A peak into stars! ------------------------------------------------------------------
# stars is developing package for dealing with spatial raster and vector data
# It's tidyverse compliant, and is/will be much better suited for processing large spatial data in R
#[https://www.r-spatial.org/r/2018/03/22/stars2.html]

#! CAUTIONARY NOTE ! if you are processing Gbs worth of raster or other spatiotemporal data, consider doing it in Python

library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(r = raster::stack(tif))
raster::plot(r) # Not ideal.

(x = read_stars(tif))
plot(x) # much improved (see ?plot.stars)
x[,,,1] %>% plot # plot band 1
x[,,,2] %>% plot # plot band 2
x[,1:100,1:100,1] %>% plot # plot spatial subset
x[,1:10,1:10,c(1,2,3)] %>% plot





