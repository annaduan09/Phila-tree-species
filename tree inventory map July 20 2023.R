#### prep ####
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mapview)
library(tigris)
library(pander)
library(pals)
library(stringr)
library(waffle)
library(cartogram)
library(broom)
library(tmap)

pal_1 <- c("#da68f8", "#bdf767", "#70f7ee", "#d34ea5", "#0df38f", "#f85b57", "#21a645", "#fa1bfc", "#34f50e", "#c78bb9", "#809b31", "#7a2edc", "#f79302", "#4d57a8", "#dad892", "#53a4e5", "#fbd127", "#256676", "#fcd6ff", "#8b500e", "#1eafae", "#7d525f", "#fcb790")

mapTheme <- function(base_size = 15) {
  theme(
    text = element_text(color = "black", family="Helvetica"),
    plot.title = element_text(size = 20,colour = "black", hjust=0, face="bold"),
    plot.subtitle=element_text(face="italic", hjust = 0),
    plot.caption=element_text(size = 7, hjust = 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "white", fill=NA, size=0),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.position="right"
  )
}

#define function to calculate mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#### read data ####
sf_use_s2(FALSE)
neigh <- st_read("Data/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.shp") %>%
  st_transform("ESRI:102729") %>%
  dplyr::select(geometry, MAPNAME)
streets <- st_read("Data/Street_Centerline.geojson")%>%
  st_transform("ESRI:102729") %>%
  group_by(ST_NAME) %>%
  summarize(geometry = st_union(geometry))

tree <- st_read("Data/PPR_Tree_Inventory_2022.geojson")%>%
  st_transform("ESRI:102729") %>%
  mutate(TREE_NAME = sub(".*- ", "", TREE_NAME),
         TREE_NAME = str_to_title(TREE_NAME),
         TREE_NAME = ifelse(grepl("Cherry", TREE_NAME) == TRUE, "Cherry",
                            ifelse(grepl("Maple", TREE_NAME) == TRUE, "Maple",
                                   ifelse(grepl("Oak", TREE_NAME) == TRUE, "Oak",
                                          ifelse(grepl("Spruce", TREE_NAME) == TRUE, "Spruce", 
                                                 ifelse(grepl("Elm", TREE_NAME) == TRUE, "Elm",
                                                        ifelse(grepl("Crabapple", TREE_NAME) == TRUE, "Crabapple", 
                                                               ifelse(grepl("Linden", TREE_NAME) == TRUE, "Linden",
                                                                      ifelse(grepl("Hawthorn", TREE_NAME) == TRUE, "Hawthorn", TREE_NAME))))))))) %>%
  filter(is.na(TREE_NAME) == FALSE & TREE_NAME != "Unknown" & TREE_NAME != "" & TREE_NAME != " ")

nj <- zctas(state = "NJ", year = 2010) %>%
  dplyr::select(geometry, STATEFP10)%>%
  st_transform("ESRI:102729")

pa <- zctas(state = "PA", year = 2010) %>%
  dplyr::select(geometry, STATEFP10)%>%
  st_transform("ESRI:102729")

pa_nj <- rbind(nj, pa)
#phl_bound <- st_union(neigh) %>%
#  st_buffer(10000)

#, county = c("Delaware", "Philadelphia", "Montgomery", "Bucks")
pa_st <- primary_secondary_roads(year = "2020", state = "PA") %>%
  dplyr::select(geometry)%>%
  st_transform("ESRI:102729")

#, county = c("Camden", "Gloucester", "Burlington")
nj_st <- primary_secondary_roads(year = "2020", state = "NJ") %>%
  dplyr::select(geometry)%>%
  st_transform("ESRI:102729")

base_st <- rbind(pa_st, nj_st) %>%
  st_make_valid() %>%
  st_union() %>%
  st_crop(xmin= 2650588, ymin= 194650.8, xmax= 2760108, ymax= 314963.8)

base_zcta <- pa_nj %>%
  st_make_valid() %>%
  st_crop(xmin= 2650588, ymin= 194650.8, xmax= 2760108, ymax= 314963.8) %>%
  erase_water()

base_state <- base_zcta %>%
  group_by(STATEFP10) %>%
  summarize(geometry = st_union(geometry))

base_st <- base_st %>%
  st_intersection(st_union(base_zcta)) %>%
  st_make_valid()

#### prep data ####
streets_buffer <- streets %>%
  st_buffer(dist = 30)

tree_species <- tree %>%
  mutate(count = 1) %>%
  dplyr::group_by(TREE_NAME) %>%
  dplyr::summarize(count = sum(count)) %>%
  arrange(desc(count)) %>%
  st_drop_geometry()

tree %>%
  st_intersection(streets_buffer) %>%
  mutate(count = 1) %>%
  dplyr::group_by(TREE_NAME) %>%
  st_drop_geometry() %>%
  dplyr::summarize(count = sum(count)) %>%
  arrange(desc(count)) %>%
  head(23) %>%
  pander(caption = "Tree species")

tree_st <- streets_buffer %>%
  st_intersection(tree) %>%
  mutate(count = 1) %>%
  group_by(ST_NAME, TREE_NAME) %>%
  summarize(count_species = sum(count)) %>%
  mutate(total_trees = sum(count_species),
         pct_species = count_species/total_trees) %>%
  dplyr::group_by(ST_NAME) %>%
  arrange(desc(pct_species)) %>%
  dplyr::summarize(TREE_NAME = first(TREE_NAME)) %>%
  st_drop_geometry() %>%
  left_join(streets) %>%
  st_as_sf()

tree_neigh <- neigh %>%
  st_intersection(tree) %>%
  mutate(count = 1) %>%
  group_by(MAPNAME, TREE_NAME) %>%
  summarize(count_species = sum(count)) %>%
  mutate(total_trees = sum(count_species),
         pct_species = count_species/total_trees) %>%
  dplyr::group_by(MAPNAME) %>%
  arrange(desc(pct_species)) %>%
  dplyr::summarize(TREE_NAME = first(TREE_NAME),
                   count_species = first(count_species)) %>%
  st_drop_geometry() %>%
  left_join(neigh) %>%
  st_as_sf()
  
rm(tree, nj, pa, nj_st, pa_st, streets_buffer)

#### Maps ####

tree_st$TREE_NAME <- factor(tree_st$TREE_NAME, levels = c("Maple", "London Planetree", "Oak", "Honeylocust", "Cherry", "Callery Pear", "Japanese Zelkova", "Gingko", "Linden", "Elm", "Japanese Tree Lilac", "Japanese Pagoda Tree", "Crabapple", "Hawthorn", "Sweetgum", "Other Ash", "Eastern Redbud", "Hackberry", "Yellowwood", "Other Serviceberry", "Amur Maackia", "Blackgum", "Goldenrain Tree"))
tree_neigh$TREE_NAME <- factor(tree_neigh$TREE_NAME, levels = c("Maple", "London Planetree", "Oak", "Honeylocust", "Cherry", "Callery Pear", "Japanese Zelkova", "Gingko", "Linden", "Elm", "Japanese Tree Lilac", "Japanese Pagoda Tree", "Crabapple", "Hawthorn", "Sweetgum", "Other Ash", "Eastern Redbud", "Hackberry", "Yellowwood", "Other Serviceberry", "Amur Maackia", "Blackgum", "Goldenrain Tree"))

##### streets #####
st_map <- ggplot() +
  geom_sf(data = base_zcta, fill = "gray20", color = "gray25", size = 10) +
  geom_sf(data = base_st, color = "gray35", size = 0.1) +
  geom_sf(data = base_state, color = "gray40", fill = NA) +
  geom_sf(data = tree_st %>% filter(is.na(TREE_NAME) == FALSE), aes(color = TREE_NAME, fill = TREE_NAME)) +
   scale_color_manual(values=pal_1) +
   scale_fill_manual(values=pal_1) +
  labs(subtitle = "Most common tree by street", color = "Species", fill = "Species") +
  mapTheme() + theme(legend.position = c(0.82, 0.27),
                     panel.background = element_rect(fill = "gray10", size = 0),
                     legend.text = element_text(color = "white", face = "italic"),
                     legend.title = element_text(color = "white", face = "bold"))

# I used this to figure out the bounding box dimensions I used below
# t <- neigh %>%
#   filter(MAPNAME %in% c("University City", "Old City"))

st_map +
  coord_sf(
    xlim = c(2681071, 2699984),
    ylim = c(232174.7, 239972.5),
    expand = FALSE
  ) + 
  theme(legend.position = "none")

##### streets table #####
tree_st %>%
  mutate(ST_NAME = str_to_title(ST_NAME)) %>%
  filter(ST_NAME %in% c("Race", "Cherry", "Arch", "Chestnut", "Walnut", "Locust", "Spruce",
                        "Pine", "South")) %>%
  mutate(ST_NAME = ifelse(ST_NAME == "Race", "Race (originally Sassafras)",
                          ifelse(ST_NAME == "Arch", "Arch (originally Mulberry)",
                                 ifelse(ST_NAME == "South", "South (originally Cedar)",
                                        ST_NAME)))) %>%
  rename(Street = ST_NAME,
         Most_Common_Tree = TREE_NAME) %>%
  st_drop_geometry() %>%
  pander(caption = "Tree Street Names")
##### neighborhood #####
ggplot() +
  geom_sf(data = base_zcta, fill = "gray20", color = "gray25", size = 10) +
  geom_sf(data = base_st, color = "gray35", size = 0.1) +
  geom_sf(data = base_state, color = "gray40", fill = NA) +
  geom_sf(data = tree_neigh %>% filter(is.na(TREE_NAME) == FALSE), aes(fill = TREE_NAME), color = "gray99") +
  scale_fill_manual(values=pal_1) +
  labs(subtitle = "Most common tree by neighborhood", fill = "Species") +
  mapTheme() + theme(legend.position = c(0.82, 0.22),
                     panel.background = element_rect(fill = "gray10", size = 0),
                     legend.text = element_text(color = "white", face = "italic"),
                     legend.title = element_text(color = "white", face = "bold"))


##### Proportional symbols #####
# ggplot() +
#   geom_sf(data = base_zcta, fill = "gray35", color = NA, size = 10) +
#   geom_sf(data = base_st, color = "gray40", size = 0.5) +
#   geom_sf(data = neigh, fill = NA, color = "gray45", alpha = 0.2) +
#   geom_sf(data = tree_neigh %>% st_centroid() %>% filter(is.na(TREE_NAME) == FALSE), 
#           aes(fill = TREE_NAME, size = count_species), color = "white", shape = 21, 
#           alpha = 0.8) +
#   scale_size(range=c(2,20)) +
#   scale_fill_manual(values=pal_1) +
#   labs(subtitle = "Most common tree by neighborhood", fill = "Species") +
#   mapTheme() + theme(legend.position = "bottom",
#                      panel.background = element_rect(fill = "gray30"))

##### Cartogram #####
# phl_cont <- cartogram_ncont(tree_neigh, "count_species")
# # plot it
# tm_shape(tree_neigh) + tm_borders() +
#   tm_shape(phl_cont) + tm_polygons("count_species", style = "jenks") +
#   tm_layout(frame = FALSE, legend.position = c("left", "bottom"))

##### Waffle chart #####
t <- tree_species %>%
  arrange(desc(count)) %>%
  mutate(count = count/500) %>%
  head(10) 

waffle(t, rows = 10, colors = pal_1, keep = TRUE, xlab = "Species", 
       size = 2, legend_pos = "bottom")

