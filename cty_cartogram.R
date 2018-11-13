library(tidyverse)
library(sf)
library(cartogram)
library(tidycensus)
library(purrr)

v15 <- load_variables(2016, "acs5", cache = TRUE)

county_tidy<-get_acs(state="GA",geography="county",var="B01001_001",geometry=TRUE) %>%
  st_transform(2780)

county_nc<-cartogram_ncont(county_tidy,weight="estimate",k=0.5,inplace=FALSE)
#plot(county_nc)
ggplot(county_nc)+geom_sf()+theme_minimal()

st_write(county_nc,"gacty_ncont.gpkg")
#Have to do some tweaking in QGIS here around the metro to create a little more space.

#Create grid based on cartogram
county_nc_edit<-st_read("gacty_ncont.gpkg")
grid<-st_sf(st_make_grid(county_nc_edit, cellsize = 3000))
#plot(grid)
grid1<-st_join(grid,county_nc_edit,join=st_intersects) %>%
  filter(estimate>-1) %>%
  group_by(GEOID) %>% 
  mutate(id = row_number())
ggplot(grid1)+geom_sf()+theme_minimal()
st_write(grid1,"gacty_grid.gpkg",delete_layer = TRUE)

#Create dissolved counties
county_boundaries<-grid1 %>%
  group_by(GEOID,NAME) %>%
  summarise() %>%
  separate(NAME,c("cty","v1"),sep=" County") %>% select(-v1)
plot(county_boundaries)

#Read in vote data and clean
#Data from https://results.enr.clarityelections.com/GA/91639/Web02-state.220747/#/cid/20000/c/Emanuel
gov_votes<-read_csv("gov_votes_2018_11_13.csv",skip=2) %>%
  filter(County!="Total:")
  
kemp_votes<-gov_votes[,c(1,3:7)] %>%
  mutate(cand="kemp")
abrams_votes<-gov_votes[,c(1,8:12)] %>%
  mutate(cand="abrams")
names(abrams_votes)<-names(kemp_votes)
metz_votes<-gov_votes[,c(1,13:17)] %>%
  mutate(cand="metz")
names(metz_votes)<-names(kemp_votes)
all_votes<-bind_rows(kemp_votes,abrams_votes,metz_votes) %>%
  group_by(County) %>%
  mutate(vote_total=sum(`Total Votes`),
         vote_pct=`Total Votes`/vote_total,
         NAME=paste(County," County, Georgia",sep="")) %>%
  left_join(county_tidy[,1:2])

#Assign votes to grid cells. Create a function to do this by county
dem="blue"
gop="red"

color_assign<-function(county_fips){
  votes_abrams<-all_votes %>%
    filter(cand=="abrams") %>%
    filter(GEOID==county_fips)

  grid_cty<-grid1 %>%
    filter(GEOID==county_fips) %>%
    st_set_geometry(NULL)
  grid_cnt<-nrow(grid_cty)
  grid_cnt_blue<-round(grid_cnt*votes_abrams$vote_pct,0)
  grid_cty_dem<-sample_n(grid_cty,grid_cnt_blue) %>%
   mutate(color=dem)
  grid_cty_id<-grid_cty_dem 
  grid_cnt_gop<-grid_cty %>%
   anti_join(grid_cty_id,by="id") %>%
   mutate(color=gop)
  grid_all<-rbind(grid_cty_dem,grid_cnt_gop)
}

cty_fid<-all_votes %>% 
  select(GEOID) %>%
  distinct
cty_colors<-bind_rows(map(cty_fid$GEOID,color_assign)) 
cty_colors_sf<-grid1 %>%
  right_join(cty_colors)

st_write(cty_colors_sf,"cty_colors_sf.gpkg")
st_write(county_boundaries,"cty_boundaries.gpkg")

library(tmap)
tm_shape(cty_colors_sf)+
  tm_polygons("color",border.alpha=0.1,lwd=0) +
tm_shape(county_boundaries)+
  tm_polygons(alpha=0,border.alpha=0.5,lwd=1)+
  tm_text("cty",size=0.5,just="top",ymod=1)
