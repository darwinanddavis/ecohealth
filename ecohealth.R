# --------------------------------------------------------------------
# 2019-nCov distribution by country     
# web scraped data from the european centre for disease control
# author: Matt Malishev
# @darwinanddavis  

# output
# https://darwinanddavis.github.io/worldmaps/coronavirus.html

# packages ----------------------------------------------------------------
pacman::p_load(maps,dplyr,leaflet,xml2,rvest,ggmap,geosphere,htmltools,mapview,purrr,rworldmap,rgeos,stringr,here,htmlwidgets,readxl,httr,readr,stringi)

# scrape data from web \xml2 ----------------------------------------------------------------------------
url <- "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases" # get daily data
url2 <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv" # get historical data 

# get geocode \ rgeos rworldmaps ------------------------------------------
lonlat <- getMap(resolution="low") %>% 
  gCentroid(byid=TRUE) %>% 
  as.data.frame 
lonlat$Country <- rownames(lonlat) # add country col
colnames(lonlat) <- c("Lon", "Lat","Country") # rename cols

# function for getting lonlat from rgeos database 
find_lonlat <- function(country_string){
  country_string_return <- lonlat %>% filter(Country %in% str_subset(lonlat$Country,country_string))
  country_string_return_name <- country_string_return %>% dplyr::select("Country") # get country string
  print(country_string_return)
}

# function for getting current country name in cv
set_country_name <-  function(country_name){
  cv[str_which(cv$Country,c(country_name)),"Country"] 
}

# convert cv webtable to tibble \rvest   
tb <- url %>% read_html %>% html_table(trim = T)  
cv <- tb[[1]] # get df
cv[is.na(cv)] <- 0 # rm nas
# cv_hist <- url2 %>% read_csv() # save historical data to dir 

# mod data
cv <- setNames(cv,c("Continent","Country","Cases","Deaths","Cases_last_15_days")) # set names 
cv$Deaths <- cv$Deaths %>% stri_replace_all_charclass("\\p{WHITE_SPACE}","") # remove middle white space
cv$Cases <- cv$Cases %>% stri_replace_all_charclass("\\p{WHITE_SPACE}","")
cv$Cases_last_15_days <- cv$Cases_last_15_days %>% stri_replace_all_charclass("\\p{WHITE SPACE}","")
cv$Deaths <- cv$Deaths %>% as.integer() # set as int
cv$Cases <- cv$Cases %>% as.integer() # set as int
cv$Cases_last_15_days <- cv$Cases_last_15_days %>% as.integer()

# get totals
cv_total <- cv %>% summarise(Total_cases = max(Cases,na.rm = T),
                             Total_deaths = max(Deaths,na.rm = T),
                             Total_recent_cases = max(Cases_last_15_days,na.rm = T))
cv <- cv[!cv$Continent=="Total",] # rm total from country df
cv <- cv[!cv$Country %in% c("Other","Asia"),] # remove 'other' country

# clean strings
cv$Cases <- cv$Cases %>% str_replace(" ","") %>% as.numeric()
cv$Deaths <- cv$Deaths %>% str_replace(" ","") %>%  as.numeric()
cv$Country <- cv$Country %>% str_replace_all("_"," ") %>% as.character()

# rename erroneous/repeated countries for getting centroid  /rgeos
cv <- cv[!cv$Country==str_subset(cv$Country,"conveyance Japan"),] # remove japan duplicate
cv[str_which(cv$Country,"Maced"),"Country"] <- find_lonlat("Maced")$Country 
cv[str_which(cv$Country,"Ser"),"Country"] <- find_lonlat("Serb")$Country 
cv[str_which(cv$Country,"Holy"),"Country"] <- find_lonlat("Vatic")$Country  
cv[str_which(cv$Country,"Brun"),"Country"] <- find_lonlat("Brun")$Country 
cv[str_which(cv$Country,"Eswa"),"Country"] <- find_lonlat("Swazi")$Country
cv[str_which(cv$Country,"Ivo"),"Country"] <- find_lonlat("Ivo")$Country 
cv[str_which(cv$Country,"Baha"),"Country"] <- find_lonlat("Baha")$Country
cv[str_which(cv$Country,"Timor"),"Country"] <- find_lonlat("Timor")$Country
cv[str_which(cv$Country,"Turks"),"Country"] <- find_lonlat("Turks")$Country
cv[str_which(cv$Country,"Cura"),"Country"] <- find_lonlat("Curac")$Country
cv[str_which(cv$Country,"Falkland"),"Country"] <- find_lonlat("Falk")$Country
cv[str_which(cv$Country,"Czech"),"Country"] <- find_lonlat("Czech")$Country
# repeated geocodes
cv[str_which(cv$Country,"Pales"),"Country"] <- "Palestine* (as neither recognition nor prejudice towards the State)"
cv[str_which(cv$Country,"Congo"),"Country"][1] <- find_lonlat("Congo")$Country[2]
cv[str_which(cv$Country,"Democratic"),"Country"] <- find_lonlat("Congo")$Country[1]

# rank data
cv <- cv %>% arrange(desc(Cases)) # rank data in descending order to layer map points 
# get global case and death rankings 
cv <- cv %>% mutate(Cases_ranked = (Cases %>% dense_rank %>% max + 1) - (Cases %>% dense_rank),
              Deaths_ranked = (Deaths %>% dense_rank %>% max + 1) - (Deaths %>% dense_rank),
              Cases_15days_ranked = (Cases_last_15_days %>%  dense_rank %>% max + 1) - (Cases_last_15_days %>% dense_rank)
              ) 

# create vars
cv_country <- cv$Country
cv_cases <- cv$Cases %>% as.numeric()
cv_deaths <- cv$Deaths %>% as.numeric()
cv_total_cases <- cv_total$Total_cases
cv_total_deaths <- cv_total$Total_deaths
cv_total_recent_cases <- cv_total$Total_recent_cases
cv_recent_cases <- cv$Cases_last_15_days %>% as.numeric()
cv_cases_ranked <- cv$Cases_ranked %>% as.numeric()
cv_deaths_ranked <- cv$Deaths_ranked %>% as.numeric()
cv_cases_15days_ranked <- cv$Cases_15days_ranked %>% as.numeric()

# match cv country lonlat to lonlat rgeos database
lonlat_final <- lonlat[cv_country,] 
lonlat_final  %>%   # write to dir
  readr::write_csv(here::here("data","cv_lonlat.csv"))
cv[,c("Lon","Lat")] <- lonlat_final[,c("Lon","Lat")] # add lonlat to df

# check country name with latlon
if(any(lonlat_final$Country == cv$Country)!=TRUE){
  cat("\n\n\nCheck country lonlat before plotting\n\n\n",rep("*",10))}

# fix misc latlon
nudge <- 0.5 # nudge overlapping latlon
cv[cv$Country=="Malaysia",c("Lon","Lat")] <- c(101.975769,4.210484) # malaysia
cv[cv$Country==cv[str_which(cv$Country,"Pales"),"Country"],c("Lon","Lat")] <- cv %>% filter(Country=="Israel") %>% dplyr::select(c("Lon","Lat")) + nudge # displace Palestine latlon from israel
cv[cv$Country==cv[str_which(cv$Country,"Gibral"),"Country"],c("Lon","Lat")] <- cv %>% filter(Country=="Spain") %>% dplyr::select(c("Lon","Lat")) + nudge # displace gibraltar latlon from spain
cv[cv$Country==cv[str_which(cv$Country,"Bonai"),"Country"],c("Lon","Lat")] <- cv %>% filter(Country=="Curacao") %>% dplyr::select(c("Lon","Lat")) + nudge - 0.3  # displace dutch caribbean2 latlon from curacao
cv <- cv[complete.cases(cv),] # rm final empty row



# get final latlon matrix  
lon <- cv$Lon ; lat <- cv$Lat 
lonlat_matrix <- matrix(c(lon,lat), ncol = 2) # get matrix for arc lines  

# check NAs
if(any(is.na(cv$Lat))==TRUE){
  cat("\n\n\nLatlon in cv dataset contains NAs\n",rep("*",10),"\n")
  cv[which(is.na(cv$Lat)),"Country"]
}

# find which countries show NAs/anomalies from latlon database  
find_lonlat("<insert country string>")
# get current country name in cv for replacement   
set_country_name("<insert country string>") 

# style -------------------------------------------------------------------
custom_tile <- names(providers)[113] # choose tiles
colv <- "#F90F40" # cases
colv2 <- "#FA0303" # deaths
colv3 <- "#DA740D" # recent cases 
opac <- 0.7
colvec_cases <- ifelse(cv_cases > 0, colv,NaN) # get colvec w/o nafta cases
colvec_deaths <- ifelse(cv_deaths > 0 ,colv2,NaN) # remove 0 points
colvec_recent_cases <- ifelse(cv_recent_cases > 0, colv3,NaN) # remove 0 points

# text --------------------------------------------------------------------
# title 
ttl <- paste0("<div style=\"color:#F90F40;\"> 
              COVID19 
              </div>","global distribution")

# tr
heading_tr <- paste(
                    "<strong> Total cases <div style=\"color:#F90F40; font-size:150%\">",format(cv_total_cases,big.mark=",",scientific = F,trim = T),"</div> </strong>", "<br/>",
                    "<strong> Total deaths <div style=\"color:#FA0303; font-size:150%\">",format(cv_total_deaths,big.mark = ",",scientific = F,trim = T),"</div> </strong>","<br/>",
                    "<strong> Total cases in last 15 days <div style=\"color:#DA740D; font-size:150%\">",format(cv_total_recent_cases,big.mark = ",",scientific = F,trim = T),"</div> </strong>"
                    )

# bl
heading_bl <- paste0(
                    "Data source: <a style=color:",colv,"; href=https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases>ECDC</a><br>
                    Last data scrape: ", Sys.time(),"<br>
                    Designer: <a style=color:",colv,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a><br>
                    Twitter/Github: <a style=color:",colv,"; href=https://github.com/darwinanddavis/worldmaps/tree/gh-pages> @darwinanddavis </a><br>
                    Spot an error? <a style=color:",colv,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a>"
                    )

# labels ## not run 
label_cases <- paste(
  "<strong> Continent: </strong>", cv$Continent, "<br/> "
) %>% map(htmltools::HTML)

# popups
popup_cases <- paste0(
  "<strong>",cv_country,"</strong><br/><br/>",
  "<strong> Cases </strong><br/><span style=color:",colv,";>", cv_cases %>% format(big.mark=",",scientific = F,trim = T),"</span><br/>",
  "<strong> Global cases ranking </strong>","<br/>", cv_cases_ranked %>% format(big.mark=",",scientific = F,trim = T),"/",cv_cases_ranked %>% max,"<br/>"
) %>% map(htmltools::HTML)

popup_deaths <- paste0(
  "<strong>",cv_country,"</strong><br/><br/>",
  "<strong> Deaths </strong><br/><span style=color:",colv2,";>", cv_deaths %>% format(big.mark=",",scientific = F,trim = T),"</span><br/>",
  "<strong> Global death ranking </strong>","<br/>", cv_deaths_ranked %>% format(big.mark=",",scientific = F,trim = T),"/",cv_deaths_ranked %>% max
) %>% map(htmltools::HTML)

popup_recent_cases <- paste0(
  "<strong>",cv_country,"</strong><br/><br/>",
  "<strong> Cases in last 15 days </strong><br/><span style=color:",colv3,";>", cv_recent_cases %>% format(big.mark=",",scientific = F,trim = T),"</span><br/>",
  "<strong> Global recent cases ranking </strong>","<br/>", cv_cases_15days_ranked, "/",cv_cases_15days_ranked %>% max
) %>% map(htmltools::HTML)

# style options -----------------------------------------------------------
# css
# title 
map_title <- tags$style( 
  HTML(".leaflet-control.map-title { 
       transform: translate(-50%,-20%);
       position: fixed !important;
       left: 50%;
       text-align: center;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 0.5;
       font-size: 40px;
       font-family: Optima;
       }"
       ))

title <- tags$div(
  map_title, HTML(ttl)
)  

# control box
map_control_box <- tags$style( 
  HTML(".leaflet-control.layers-base { 
       text-align: left;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 1;
       font-size: 15px;
       }"
       ))

control_box <- tags$div(
  map_control_box, HTML("")
)  

# text labels 
style <- list(
  "color" = "black",
  "font-weight" = "normal",
  "font-family" = "Optima",
  "padding" = "8px"
)

style_nafta <- list(
  "color" = "#F90F40",
  "font-weight" = "normal",
  "font-family" = "Optima",
  "padding" = "8px"
)

# text label options 
text_label_opt <- labelOptions(noHide = F, direction = "top", textsize = "15px",
                               textOnly = F, opacity = 0.7, offset = c(0,0),
                               style = style, permanent = T
)

text_label_opt_nafta <- labelOptions(noHide = T, direction = "top", textsize = "15px",
                               textOnly = F, opacity = 0.7, offset = c(0,0),
                               style = style_nafta, permanent = T
)

# layer options 
layer_options <- layersControlOptions(collapsed = F)

# tile options
min_zoom <- 3
max_zoom <- 10

# set max map bounds
latlon_origin <- cv %>% filter(Country=="China") %>% dplyr::select(c("Lon","Lat")) %>% as.numeric() # china lonlat
max_bound1 <- c(-150,90)
max_bound2 <- c(180,-90)

# set map projections 
proj_options <- leafletOptions(worldCopyJump = T)

# layers ------------------------------------------------------------------

# titles
layer1 <- "Cases"
layer2 <- "Deaths"
layer3 <- "Cases in last 15 days"  

# point size
cex <- 70
radius_cases <- (sqrt(cv_cases) / cex) 
radius_deaths <- (sqrt(cv_deaths) / cex)
radius_recent_cases <- (sqrt(cv_recent_cases) / cex)

# easy buttons 
locate_me <- easyButton( # locate user
  icon="fa-crosshairs", title="Zoom to my position",
  onClick=JS("function(btn, map){ map.locate({setView: true}); }"))

reset_zoom <- easyButton( # reset zoom 
  icon="fa-globe", title="Reset zoom",
  onClick=JS("function(btn, map){ map.setZoom(3);}"))

# map ---------------------------------------------------------------------

# set arc matrix
m <- gcIntermediate(latlon_origin,
               lonlat_matrix,
               n=100,
               addStartEnd=T,
               breakAtDateLine = T,
               sp=T) %>% 
  leaflet(options=proj_options) %>% # add world jump 
  setView(latlon_origin[1],latlon_origin[2],zoom=min_zoom) %>% 
  addTiles(custom_tile,
           options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom) # set zoom bounds
           ) %>% 
  addProviderTiles(custom_tile, 
                   group = c(layer1,layer2,layer3),
                   options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom) # set zoom bounds
                   ) %>% 
  addPolylines(color=colvec_deaths, # deaths
               opacity = opac,
               weight = 0.3,
               group = layer2) %>%
  addCircleMarkers(lon,lat, # cases
             weight=1,
             radius= radius_cases,
             color=colv,
             fillColor=colv,
             label = popup_cases,
             popup = popup_cases,
             labelOptions = text_label_opt,
             popupOptions = text_label_opt,
             group = layer1) %>% 
  addCircleMarkers(lon,lat, # deaths 
             weight=1,
             radius=radius_deaths,
             color=colvec_deaths,
             fillColor=colvec_deaths,
             label = popup_deaths,  
             popup = popup_deaths,
             labelOptions = text_label_opt,
             popupOptions = text_label_opt,
             group = layer2) %>%
  addCircleMarkers(lon,lat, # recent cases 
             weight=1,
             radius=radius_recent_cases,
             color=colvec_recent_cases,
             fillColor=colvec_recent_cases,
             label = popup_recent_cases,
             popup = popup_recent_cases,
             labelOptions = text_label_opt, 
             popupOptions = text_label_opt,
             group = layer3) %>%
  addLayersControl(
    baseGroups = c(layer1,layer2,layer3),
    options = layer_options) %>% 
  hideGroup(c(layer2,layer3)) %>% 
  addControl(title, "bottomleft", className = "map-title") %>% 
  addControl(heading_bl,"bottomleft") %>%
  addControl(heading_tr, "topright") %>% 
  addControl(control_box, "topright", className = "layers-base") %>% 
  addEasyButton(reset_zoom) %>% 
  addEasyButton(locate_me) 
m

# save outputs ---------------------------------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
m %>% saveWidget(here::here("coronavirus.html"))  

