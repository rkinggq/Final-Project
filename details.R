---
  title: "Final Project"
author: "Group 6"
date: "2024-12-06"
output: html_document
---
  
  ```{r, libraries, include=FALSE}
library(rnaturalearth)
library(sf)
library(tidyverse)
library(dplyr)
library(stars)
library(randomForest)
library(mapview)
library(ggplot2)
```

# Introduction 

<center>
  **Figure 1:** Map of Towns of Interest in NH 
</center>
  ```{r, plot of NH, fig.cap = "Map of NH with towns that amphibian data was collected from."}
ggplot() +
  geom_sf(data = nh, fill = "gray80", color = "black", lwd = 0.1) +  # Background towns
  geom_sf(data = nh_filtered, fill = "red", color = "black", lwd = 0.5) +  # Highlight towns of interest
  geom_sf_text(data = nh_filtered, aes(label = TOWN), size = 3, color = "black") +  # Label towns of interest
  theme_void()
```

<center>
  **Figure 2:** Eastern Newt
</center>
  
  <center>
  [![Eastern Newt](https://upload.wikimedia.org/wikipedia/commons/4/49/Notophthalmus_viridescensPCCA20040816-3983A.jpg?20071210151624)](https://upload.wikimedia.org/wikipedia/commons/4/49/Notophthalmus_viridescensPCCA20040816-3983A.jpg?20071210151624)
</center>
  
  <center>
  **Figure 3:** Wood Frog 
</center>
  
  <center>
  **Figure 4:** Spring Peeper
</center>
  
  <center>
  **Figure 5:** Spotted Salamander
</center>
  
  # Methods 
  
  
  
  ```{r, load the data, warning=F, echo = F, message = F}
#amphibian data: 
amphibian <- read_csv("amphibiandata.csv")
amphibian <- amphibian %>%
  rename(Dead = `# Dead`) %>% 
  rename(Crossed = `# Crossed`) %>% 
  rename(TOWN = `Town`)

#towns: 
nh <- read_sf("NH/New_Hampshire_Political_Boundaries.shp") %>%
  rename(TOWN=pbpNAME) 

towns_of_interest <- c("Hancock", "Nelson", "Peterborough", "Keene")

nh_filtered <- nh %>% filter(TOWN %in% towns_of_interest)
```


```{r, "raster data", message = F, warning=F, echo = F}
#raster data:
f <- list.files("NLCD",pattern = ".tif", full.names=TRUE)
print(f)

lndchg <- f[grepl("LndChg", f)]
new_lndchg <- gsub(".*(LndChg)_([0-9]{4}).*", "\\1_\\2", basename(lndchg))

lndcov <- f[grepl("LndCov", f)]
new_lndcov <- gsub(".*(LndCov)_([0-9]{4}).*", "\\1_\\2", basename(lndcov))

imprev <- f[grepl("ImpDsc", f)]
new_imprev <- gsub(".*(ImpDsc)_([0-9]{4}).*", "\\1_\\2", basename(imprev))


#HAVING PROBLEMS HERE!
#land use
land_use <- read_stars(f[grepl("LndCov", f)]) %>%
  setNames("land_use") %>%
  st_transform(st_crs(nh_filtered)) %>%
  st_crop(nh_filtered)

#imprevious
imprevious <- read_stars(f[grepl("ImpDsc", f)]) %>%
  setNames("imprevious") %>%
  st_transform(st_crs(nh_filtered)) %>%
  st_crop(nh_filtered) 

#land change
land_change <- read_stars(f[grepl("LndChg", f)]) %>%
  setNames("land_change") %>%
  st_transform(st_crs(nh_filtered)) %>%
  st_crop(nh_filtered)
```

```{r, make nh_amphibian, message = F, warning=F, echo = F}
nh_amphibian <- nh_filtered %>% 
  left_join(amphibian, by ="TOWN") %>% 
  dplyr::select(TOWN, Dead, geometry)
```

```{r, analysis}
#HELP! This part giving issues
nh_lu <- land_use %>% 
  st_as_sf %>% 
  st_join(nh_filtered) %>% 
  select(land_use, TOWN, geometry)

land_use <- st_as_sf(land_use)
```

# Results 

```{r, spatial autocorrelation}
# Is amphibian abudnance spaitally autocorrelated, are there pockets of higher or lower dead?? 
nh_amphibian %>%
  ggplot() + 
  geom_sf(aes(fill = Dead)) +
  labs(title = "Amphibian Abundance (Dead) Across New Hampshire") +
  scale_fill_gradient(
    name = "Dead Count", 
    limits = c(0, 150), 
    low = "lightblue", 
    high = "darkblue", 
    na.value = "grey50" # Handles NA values
  ) +
  theme_minimal()

mapview(nh_amphibian, zcol="Dead")
``` 
