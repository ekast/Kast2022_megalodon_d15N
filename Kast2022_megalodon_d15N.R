# This script includes all data processing, plotting, and analysis 
# involved in the manuscript Kast et al., "Cenozoic megatooth sharks occupied 
# extremely high trophic positions", Science Advances (2022) \
# doi: 

# author: Emma R. Kast, emma.r.kast@gmail.com

# written w/ R version 4.0.1
# required packages: tidyverse, readxl, cowplot, BSDA, deming, ggforce

# Other files
# Data are in the subdirectory "Data" and include:
#     Data Table S1 (d15N measurements)
#     Data Table S2 (shark d15N literature compilation)
#     Data Table S3 (marine mammal d15N literature compilation)
#     GTS2012 (epoch boundaries)
#     comparison_tests (file describing all possible comparisons to analyse, see Analysis section)
# Figures will be saved to the subdirectory "Plots"
# Tables will be saved to the subdirectory "Data/Calculated data tables"

#Sections:
#1.Load data (data files are stored in subfolder "Data")
#2.Process data
  # standards, shark teeth data, shark literature, marine mammal literature
#3.Plots
  # Site map, Fig. 1, Fig. 2, Fig. 3, Fig. S1, Fig. S2, Fig. S3, Fig. S4, Fig. S5
#4.Other Analysis
  # Comparison / t tests, Table S1, d15N vs N content correlations

# Note: Load data & Process data need to be run before any of the 
# Plots or Analysis. The Plots and Analysis sub-sections can be run (and re-run)
# in any order once you have loaded and processed the data

# load packages:
library(tidyverse)
library(readxl)
library(cowplot)
library(BSDA)
library(deming)
library(ggforce)

# clear data and values:
# rm(list = ls())

# 1. Load data  ------------------------------------------------------------

# load required data:
N_data             <- read_excel("Data/Data S1.xlsx", sheet = "Specimens") # this file contains all meta data, d15N, N content, and total length estimates
N_data_standards   <- read_excel("Data/Data S1.xlsx", sheet = "Standards") # this file contains d15N and N content of standards that were measured alongside specimens in Data Table 1

shark_lit          <- read_excel("Data/Data S2 shark_d15N_literature.xlsx", sheet = "data") # compilation of modern shark d15N values, Data Table 2
shark_lit_class    <- read_excel("Data/Data S2 shark_d15N_literature.xlsx", sheet = "classification") # classification scheme for sharks (genus, family, etc)

marmamm_lit        <- read_excel("Data/Data S3 marine_mammal_d15N_literature.xlsx", sheet = "data") # compilation of modern marine mammal d15N values, Data Table 3
marmamm_lit_class  <- read_excel("Data/Data S3 marine_mammal_d15N_literature.xlsx", sheet = "classification") # classification scheme for marine mammals (genus, family, etc)

GTS2012            <- read_excel("Data/GTS2012.xlsx", sheet = "series ages") # information on epoch boundaries

# a few variables for consistent plots
figures.mrkrsize = 3;
figures.textsize = 10;

# Create values for ordering lists (epochs, shark families, shark genera, marine mammal families)
epoch_order <- c("Late Cretaceous", 
                 "Paleocene", 
                 "Eocene",
                 "Oligocene", 
                 "Miocene", 
                 "Pliocene", 
                 "Modern")

location_group_order <- c("USA (West Coast)", 
                          "USA (East Coast)",
                          "NW Europe",
                          "South Africa",
                          "Japan")

shark_family_order <- c("Hexanchidae",
                        "Squalidae",
                        "Centrophoridae",
                        "Dalatiidae",
                        "Etmopteridae",
                        "Somniosidae",
                        "Stegostomatidae",
                        "Ginglymostomatidae",
                        "Rhincodontidae",
                        "Odontaspididae",
                        "Pseudocarchariidae",
                        "Megachasmidae",
                        "Alopiidae",
                        "Carchariidae",
                        "Cetorhinidae",
                        "Lamnidae",
                        "Scyliorhinidae",
                        "Triakidae",
                        "Hemigaleidae",
                        "Carcharhinidae",
                        "Sphyrnidae")

shark_species_order <- c("Carcharodon carcharias",
                         "Carcharodon hastalis",
                         
                         "Otodus chubutensis",
                         "Otodus megalodon",
                         "Otodus angustidens",
                         "Otodus auriculatus",
                         "Otodus obliquus",
                         "Cretalamna sp.",
                         
                         "Carcharias taurus", 
                         "Carcharias sp.", 
                         "Striatolamia macrota",
                         "Striatolamia striata", 
                         "Striatolamia sp.",
                         "Scapanorhynchus elegans",
                         "Scapanorhynchus rhaphiodon",
                         "Scapanorhynchus sp.", 
                         "Palaeohypotodus rutoti")

shark_genera_order <- c("Carcharias", 
                        "Striatolamia",
                        "Palaeohypotodus",
                        "Scapanorhynchus", 
                        "Carcharodon", 
                        "Otodus", 
                        "Cretalamna")

pisc_list_genus_species <- c("Carcharias taurus", 
                             "Carcharias sp.", 
                             "Scapanorhynchus elegans",
                             "Scapanorhynchus rhaphiodon",
                             "Scapanorhynchus sp.", 
                             "Striatolamia striata", 
                             "Striatolamia macrota",
                             "Striatolamia sp.",
                             "Palaeohypotodus sp.", 
                             "Palaeohypotodus rutoti")

pisc_list_genus <- c("Carcharias", 
                     "Scapanorhynchus", 
                     "Striatolamia", 
                     "Palaeohypotodus")

# Cetacean order from McGowen 2020
marmamm_family_order <- c("Ursidae",
                          "Mustelidae",
                          "Odobenidae",
                          "Otariidae",
                          "Phocidae",
                          "Balaenidae",
                          #"Neobalaenidae,
                          "Balaenopteridae",
                          "Eschrichtiidae",
                          "Physeteridae",
                          "Kogiidae",
                          #"Platanistidae",
                          "Ziphiidae",
                          #"Lipotidae",
                          #"Iniidae",
                          "Pontoporiidae",
                          "Monodontidae",
                          "Phocoenidae",
                          "Delphinidae",
                          "Trichechidae")

# 2. Process data ---------------------------------------------------------


## Process standards -------------------------------------------------------
# ________________________ 

# renaming variables to be easier to work with
N_data_standards <- N_data_standards %>%
  rename(standard = Standard,
         rundate = `Run date (yyyy-mm-dd)`,
        d15n = `d15N (‰ vs. air)`,
        ncont = `N content (umol N / g)`)
       
# get fossil enameloid standard data      
standard_enameloid <- N_data_standards %>%
  filter(standard == "fossil enameloid")

# summarise fossil enameloid standard data by run 
standard_enameloid_summary <- standard_enameloid %>%
  group_by(rundate) %>%
  summarise(d15n_sd = sd(d15n),
            d15n_mean = mean(d15n),
            ncont_sd = sd(ncont),
            ncont_mean = mean(ncont))

# get coral carbonate standard data      
standard_coral <- N_data_standards %>%
  filter(standard == "coral carbonate")

# summarise coral carbonate standard data by run
standard_coral_summary <- standard_coral %>%
  group_by(rundate) %>%
  summarise(d15n_sd = sd(d15n),
            d15n_mean = mean(d15n),
            ncont_sd = sd(ncont),
            ncont_mean = mean(ncont))

## Process shark teeth N data ------------------------------------------------
# ________________________

# renaming variables from Data Table 1 to be easier to work with:
N_data <-N_data %>%
  rename(Location_group = `Location group`,
         d15n = `d15N enameloid-bound (‰ vs. air)`,
         d15n_sd = `d15N enameloid-bound 1sd`,
         ncont = `N content enameloid-bound (umol N / g)`,
         ncont_sd = `N content enameloid-bound 1sd`,
         length_cm = `Total length, calculated (cm)`,
         N_replicate = `N replicate type enameloid-bound`,
         d15n_coll = `d15N collagen (‰ vs. air)`,
         lat_approx = `Latitude (approximate)`,
         long_approx = `Longitude (approximate)`) %>%
  mutate(Species = ifelse(Species == "\"appendiculata\"", "sp.", Species)) %>%
  mutate(Species = ifelse(Species == "augustidens", "angustidens", Species)) %>% # type in specimen list
  mutate(Species = ifelse(Species == "chubetensis", "chubutensis", Species)) # type in Data 1
# Add some descriptive variables:
N_data <- N_data %>%
  mutate(Epoch = ifelse(is.na(Epoch), "Modern", Epoch)) %>% # Assigning epoch name to be "Modern" for modern samples
  mutate(shark_type = ifelse(Genus %in% pisc_list_genus, "baseline","megatooth")) %>% # Classifying each species as piscivorous ("baseline") or "megatooth" 
  mutate(genus_species = paste(Genus,Species,sep = " ")) # Making a new variable, Genus species

# Assign levels for easy sorting
N_data$Epoch <- factor(N_data$Epoch, levels = epoch_order)              # epoch names
N_data$genus_species <- factor(N_data$genus_species, levels = shark_species_order) # genus species names
N_data$Genus <- factor(N_data$Genus, levels = shark_genera_order)       # genera names
N_data$Location_group <- factor(N_data$Location_group, levels = location_group_order)       # genera names


# Extract piscovore ("baseline") data 
N_data_piscivores <- N_data %>%
  filter(shark_type == "baseline")

# Summarize piscivores by epoch
N_data_piscivores_summ_epoch <- N_data_piscivores %>%
  group_by(Epoch) %>%
  summarise(
    d15n_temp = mean(d15n, na.rm=TRUE), d15nsd_temp = sd(d15n, na.rm = TRUE),
    ncont_temp = mean(ncont, na.rm=TRUE), ncontsd_temp = sd(ncont, na.rm = TRUE),
    n_ind_pisc = n_distinct(d15n, na.rm = TRUE),
    d15n_min_pisc = min(d15n, na.rm = TRUE), d15n_max_pisc = max(d15n, na.rm = TRUE)) 

N_data_piscivores_summ_epoch <- N_data_piscivores_summ_epoch %>%
  rename(d15n_pisc = d15n_temp, d15n_sd_pisc = d15nsd_temp, 
         ncont_pisc = ncont_temp, ncont_sd_pisc = ncontsd_temp) %>%
  mutate(d15n_range_pisc = d15n_max_pisc - d15n_min_pisc)

# Summarize piscivores by epoch and species
N_data_piscivores_summ_epoch_species <- N_data_piscivores %>%
  group_by(Epoch) %>%
  summarise(
    d15n_temp = mean(d15n, na.rm=TRUE), d15nsd_temp = sd(d15n, na.rm = TRUE),
    ncont_temp = mean(ncont, na.rm=TRUE), ncontsd_temp = sd(ncont, na.rm = TRUE),
    n_ind_pisc = n_distinct(d15n, na.rm = TRUE),
    d15n_min_pisc = min(d15n, na.rm = TRUE), d15n_max_pisc = max(d15n, na.rm = TRUE)) 

N_data_piscivores_summ_epoch_species <- N_data_piscivores_summ_epoch_species %>%
  rename(d15n_pisc = d15n_temp, d15n_sd_pisc = d15nsd_temp, 
         ncont_pisc = ncont_temp, ncont_sd_pisc = ncontsd_temp) %>%
  mutate(d15n_range_pisc = d15n_max_pisc - d15n_min_pisc)

# Add piscivore epoch summary data back to N_data
# Note that these variables (d15n_pisc, etc) in the N_data table are averages for the entire epoch!
N_data <- N_data %>%
  left_join(N_data_piscivores_summ_epoch) 

# Calculate d15N offset from piscivores
N_data <- N_data %>% 
  mutate(d15n_offset = d15n - d15n_pisc, # calculating difference between each shark d15n and the average piscivore d15n for that epoch
         d15n_offset_sd = ifelse(is.na(d15n_sd), d15n_sd_pisc, (d15n_sd^2 + d15n_sd_pisc^2)^(1/2))) # error for d15n_offset, either equal to sd of average piscivore value for that epoch, or calculated by propogating d15n measurement error (d15n_sd) and the piscivore d15n error (d15n_pisc_sd)

# Separate megatooth species
N_data_megatooth <- N_data %>%
  filter(shark_type == "megatooth")

# Summarize N data by species and epoch
N_data_summ_epoch_species <- N_data %>%
  group_by(genus_species, Epoch) %>%
  summarise(d15n_temp = mean(d15n, na.rm=TRUE),d15nsd_temp = sd(d15n, na.rm = TRUE),
            ncont_temp = mean(ncont, na.rm=TRUE),ncontsd_temp = sd(ncont, na.rm = TRUE),
            n_ind = n_distinct(d15n, na.rm = TRUE),
            d15n_min = min(d15n, na.rm = TRUE), d15n_max = max(d15n, na.rm = TRUE),
            Genus = Genus[[1]],
            Family = Family[[1]]) %>%
  rename(d15n = d15n_temp, d15n_sd = d15nsd_temp, 
         ncont = ncont_temp, ncont_sd = ncontsd_temp) %>%
  mutate(d15n_range = d15n_max - d15n_min) %>%
  mutate(shark_type = ifelse(Genus %in% pisc_list_genus, "baseline","megatooth")) %>%
  left_join(N_data_piscivores_summ_epoch) %>% # adding piscivore epoch summary back to table
  mutate(d15n_offset = d15n - d15n_pisc, # calculating difference between each shark d15n and the average piscivore d15n for that epoch
         d15n_offset_sd = (d15n_sd^2 + d15n_sd_pisc^2)^(1/2)) # error for d15n_offset calculated by propogating epoch-species average d15n error(d15n_sd) and the piscivore d15n error (d15n_pisc_sd)

# reorder variables
N_data_summ_epoch_species$Genus         <- factor(N_data_summ_epoch_species$Genus,levels = shark_genera_order)
N_data_summ_epoch_species$genus_species <- factor(N_data_summ_epoch_species$genus_species,levels = shark_species_order)
N_data_summ_epoch_species$Epoch         <- factor(N_data_summ_epoch_species$Epoch,levels = epoch_order)


# Summarize N data by genera and epoch
N_data_summ_epoch_genus <- N_data %>%
  group_by(Genus, Epoch) %>%
  summarise(d15n_temp = mean(d15n, na.rm=TRUE),d15nsd_temp = sd(d15n, na.rm = TRUE),
            ncont_temp = mean(ncont, na.rm=TRUE),ncontsd_temp = sd(ncont, na.rm = TRUE),
            n_ind = n_distinct(d15n, na.rm = TRUE),
            d15n_min = min(d15n, na.rm = TRUE), d15n_max = max(d15n, na.rm = TRUE),
            Family = Family[[1]]) %>%
  rename(d15n = d15n_temp, d15n_sd = d15nsd_temp, 
         ncont = ncont_temp, ncont_sd = ncontsd_temp) %>%
  mutate(d15n_range = d15n_max - d15n_min) %>%
  mutate(shark_type = ifelse(Genus %in% pisc_list_genus, "baseline","megatooth")) %>%
  left_join(N_data_piscivores_summ_epoch) # adding piscivore epoch summary back to table

# reorder variables
N_data_summ_epoch_genus$Genus         <- factor(N_data_summ_epoch_genus$Genus,levels = shark_genera_order)
N_data_summ_epoch_genus$Epoch         <- factor(N_data_summ_epoch_genus$Epoch,levels = epoch_order)

# Summarize N data by genera, epoch, and location
N_data_summ_epoch_genus_loc <- N_data %>%
  group_by(Genus, Epoch, Location_group) %>%
  summarise(d15n_temp = mean(d15n, na.rm=TRUE),d15nsd_temp = sd(d15n, na.rm = TRUE),
            ncont_temp = mean(ncont, na.rm=TRUE),ncontsd_temp = sd(ncont, na.rm = TRUE),
            n_ind = n_distinct(d15n, na.rm = TRUE),
            d15n_min = min(d15n, na.rm = TRUE), d15n_max = max(d15n, na.rm = TRUE),
            Family = Family[[1]]) %>%
  rename(d15n = d15n_temp, d15n_sd = d15nsd_temp, 
         ncont = ncont_temp, ncont_sd = ncontsd_temp) %>%
  mutate(d15n_range = d15n_max - d15n_min) %>%
  mutate(shark_type = ifelse(Genus %in% pisc_list_genus, "baseline","megatooth")) 

# reorder variables
N_data_summ_epoch_genus_loc$Genus         <- factor(N_data_summ_epoch_genus_loc$Genus,levels = shark_genera_order)
N_data_summ_epoch_genus_loc$Epoch         <- factor(N_data_summ_epoch_genus_loc$Epoch,levels = epoch_order)
N_data_summ_epoch_genus_loc$Location_group<- factor(N_data_summ_epoch_genus_loc$Location_group, levels = location_group_order)


# ______________________________ 
## Process shark literature data ------------------------------------------
# ______________________________

shark_lit <- shark_lit %>%
  
  mutate(d15N_sd = ifelse(d15N_error_type == "sd", 
                          d15N_error, d15N_error * n_ind^(1/2)))#, #calculating d15N sd if error was given as a standard error
         # lat = rowMeans(cbind(latitude_min, latitude_max), na.rm = TRUE), # get mean latitude
         # long = rowMeans(cbind(longitude_min, longitude_max), na.rm = TRUE), # get mean longitude
         # 
         # # separating lipid extraction and urea extraction into easier variables:
         # lipids_extracted_N = ifelse(lipids_extracted %in% c("E", "E for C:N > 3.5"),
         #                             "E","N"),
         # lipids_extracted_C = ifelse(lipids_extracted %in% c("E", "E for C:N > 3.5", "E for C, N for N","C for C, N for N","E and C for C"),
         #                             "E","N"),
         # urea_extracted_N = ifelse(urea_extracted %in% c("E"), 
         #                           "E", "N"),
         # urea_extracted_C = ifelse(urea_extracted %in% c("E", "E for C, N for N"), 
                                   # "E", "N"))
shark_lit <- shark_lit %>%
  #get genus from genus_species:
  mutate(genus = sapply(strsplit(shark_lit$genus_species, " "), head, 1)) %>% 
  #join classification (family, order, etc) by genus name:
  left_join(shark_lit_class)

shark_family_summ <- shark_lit %>%
  group_by(family) %>%
  summarize(n_ind_family = sum(n_ind))

# Assign levels to family names:
shark_lit$family <- factor(shark_lit$family,levels = shark_family_order)
shark_family_summ$family <- factor(shark_family_summ$family,levels = shark_family_order)



## Process marine mammal literature data -------------------------------------------------
# _____________________________________


marmamm_lit <- marmamm_lit %>%
  #get genus from genus_species:
  mutate(genus = sapply(strsplit(marmamm_lit$genus_species, " "), head, 1)) %>%
  #join classification (family, order, etc) by genus name:
  left_join(marmamm_lit_class)

marmamm_family_summ <- marmamm_lit %>%
  group_by(family) %>%
  summarize(n_ind_family = sum(n_ind))

# Assign levels to family names:
marmamm_lit$family = factor(marmamm_lit$family, levels = marmamm_family_order)
marmamm_family_summ$family = factor(marmamm_family_summ$family, levels = marmamm_family_order)



# 3. Plots --------------------------------------------------------------------------

## Site map, inset on Fig. 2 and Fig. S_  -----------------------------------------
#________________________________________________

# Summarize location data:
broad_locations <- N_data %>%
  group_by(Location_group) %>%
  summarise(lat = mean(lat_approx, na.rm = TRUE),
            long = mean(long_approx, na.rm = TRUE),
            n_meas = n())
specific_locations <- N_data %>%
  group_by(lat_approx, long_approx, Location_group, shark_type) %>%
  summarise(n_meas = n())

# get world map data
world_map <- map_data("world")

# plot:
shark_site_map <- ggplot() + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "gray80", colour = "white", size = 0.01) +
  
  geom_point(data = broad_locations, 
             aes(x = long, y = lat, size = n_meas),
             color = "black", alpha = 0.3) +

  geom_point(data = specific_locations, 
             aes(x = long_approx, y = lat_approx),
             shape = 21, size = 2, fill = "white", color = "black", stroke = 0.2) +

  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60)) +
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) +
  scale_size_area(max_size = 15) + 
  
  coord_map(projection = "mollweide",
            xlim = c(-180,180), ylim = c(-90,90)) + 
  
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

shark_site_map

#uncomment next line to save plot to pdf:
#ggsave("site_map.pdf", path = "Plots", height = 3, width = 4, units = "in")



## Fig. 1 ------------------------------------------------------------------
# Collagen
#____________________________________

#### analysis --------------------------------------------------------
# get collagen data
shark_coll <- N_data %>%
  filter(!is.na(d15n_coll)) %>%
  filter(!is.na(d15n))

# correlations between d15NEB and collagen d15N
pearson_r_enam_coll = cor.test(x = shark_coll$d15n_coll, y = shark_coll$d15n)
demingregress_enam_coll <- deming(d15n ~ d15n_coll, data = shark_coll,
                                  xstd = rep(0.2,length(shark_coll$d15n_coll)),
                                  ystd = rep(0.5,length(shark_coll$d15n_coll)))

# bootstrapped deming regression between d15NEB and collagen d15N
iterations = 1000
slopelist = vector()
interceptlist = vector()
xtemp_predict <- seq(min(shark_coll$d15n_coll, na.rm = TRUE),max(shark_coll$d15n_coll, na.rm = TRUE), by = 0.1)
ytemp_predict <- vector()

for (n in 1:iterations) {
  data_temp <- tibble(x = shark_coll$d15n_coll, xsd = rep(0.2,length(shark_coll$d15n_coll)),
                      y = shark_coll$d15n,      ysd = rep(0.7,length(shark_coll$d15n_coll))) %>%
    mutate(y_ran = rnorm(n(), mean = y, sd = ysd)) %>%
    mutate(x_ran = rnorm(n(), mean = x, sd = xsd))   # for each n, want to fit linear model and save slope, intercept, etc
  
  tempfit <- deming(y_ran ~ x_ran, data = data_temp, xstd = xsd, ystd = ysd) # DEMING REGRESSION
  slopelist[[n]] <- tempfit$coefficients[2]
  interceptlist[[n]] <- tempfit$coefficients[1]
  
  for (k in 1:length(xtemp_predict)) {
    ytemp_predict[[(n-1)*length(xtemp_predict)+k]] <- xtemp_predict[[k]]*slopelist[[n]] + interceptlist[[n]] 
  }
}

ypred <- matrix(data = ytemp_predict, nrow = iterations, ncol = length(xtemp_predict), byrow = TRUE)
ypred_mean <- apply(ypred, 2, mean)
ypred_sd <- apply(ypred, 2, sd)

# setting up confidence intervals for plotting
cix_boot <- c(xtemp_predict, rev(xtemp_predict))
ciy_boot <- c(ypred_mean - 2*ypred_sd, rev(ypred_mean + 2*ypred_sd))
ci_boot <- tibble(cix_boot,ciy_boot)

#### plot -------------------------------------------------------------------
en_vs_coll <- ggplot() +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # deming regression
  geom_line(aes(x = xtemp_predict, 
                y =  demingregress_enam_coll$coefficients[2]*xtemp_predict +  
                     demingregress_enam_coll$coefficients[1])) +
  # bootstrapped confidence intervals for deming regression
  geom_polygon(data = ci_boot, aes(x = cix_boot, y = ciy_boot),
               alpha = 0.2, fill = "black") +
  # plot measurements
  geom_point(data = shark_coll, aes(x = d15n_coll, y = d15n), 
             shape = 21, size = 2.5, color = "black", fill = "white") + 
  # appearance and labels
  scale_x_continuous(limits = c(13, 19), expand = c(0,0)) + 
  scale_y_continuous(limits = c(13, 19), expand = c(0,0)) +
  ylab("d15N enameloid") + 
  xlab("d15N collagen") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 12))

# plot difference between d15NEB and collagen d15N measurements
en_coll_offset <- shark_coll %>%
  ggplot(aes(x = 1, y = d15n - d15n_coll)) +
  # plot boxplot of d15NEB - collagen d15N
  geom_boxplot(fill = 'grey82', width = 0.5, outlier.colour = NA) +
  # plot measurements of d15NEB - collagen d15N
  geom_sina(maxwidth = 0.7, seed = 0.8,
            shape = 21, size = 2.5, color = "black", fill = "white") +
  # appearance and labels
  scale_y_continuous(limits = c(0, 3), expand = c(0,0)) +
  ylab("d15N enameloid - d15N collagen") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12))

# plot combined
combined <- plot_grid(en_vs_coll, en_coll_offset, align = 'hv', rel_widths = c(2.5, 1))
combined

#uncomment next line to save plot to pdf:
#ggsave("Fig_01.pdf", path = "Plots", height = 3, width = 5, units = "in")

## Fig. 2  -----------------------------------------------------------------
# (A) d15N vs. genus, by epoch and (B) d15N offset vs. genus, by epoch
#____________________________________

d15n_vs_genus_by_epoch <- N_data %>%
  ggplot(aes(x = Genus,y = d15n)) + 
  
  # individual data points:
  geom_point(data =  N_data_piscivores,
             aes(x = Genus, y = d15n), fill = "grey40", color = "black", shape = 21, 
             size = 3, alpha = 0.5, stroke = 0.2) +
  geom_point(data =  N_data_megatooth,
             aes(x = Genus, y = d15n, fill = genus_species, shape = Genus), 
             size = 3, alpha = 0.7, stroke = 0.2) +
  
  # genus averages for each epoch, with error bars:
  geom_errorbar(data =  N_data_summ_epoch_genus,
                aes(x = Genus, ymin = d15n - d15n_sd, ymax = d15n + d15n_sd), color = "black", 
                width = 0.3, size = 0.5) +
  geom_point(data =  filter(N_data_summ_epoch_genus, shark_type == "baseline"),
             aes(x = Genus, y = d15n), shape = 21, fill = "black",size = 3) +
  geom_point(data =  filter(N_data_summ_epoch_genus, shark_type == "megatooth"),
             aes(x = Genus, y = d15n, shape = Genus), fill = "black",size = 3) +
  
  # text label for number of individuals measured
  geom_text(data = N_data_summ_epoch_genus,
            aes(x = Genus, y = 7.5, label = paste("(",n_ind,")", sep = "")), size = 2) +
  
  scale_shape_manual(values = c(22,23,24)) +
  
  facet_grid(cols = vars(Epoch)) + 
  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, face = "italic", hjust = 1, vjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0,0,0,0)),
        panel.spacing = unit(0.1, "lines"),
        panel.border = element_rect(size = 0.3),
        plot.background = element_rect(size = 0.3),
        strip.background = element_rect(size = 0.3)) +
  
  scale_x_discrete(expand = c(0.2,0.2),
                   limits = shark_genera_order) +
  scale_y_continuous(limits = c(5, 35), expand = c(0,0)) +
  ylab("d15NEB (permil vs. air)") + 
  scale_fill_brewer(palette = "Set3")


d15n_offset_vs_genus_by_epoch <-   N_data_summ_epoch_species %>%
  filter(shark_type == "megatooth") %>%
  ggplot() + 
  geom_errorbar(aes(x = Genus, ymin = d15n_offset - d15n_offset_sd, ymax = d15n_offset + d15n_offset_sd), color = "black", 
                width = 0.3, size = 0.5) +
  geom_point(aes(x = Genus, y = d15n_offset, shape = Genus, fill = genus_species),
             size = 3, stroke = 0.2) +
  
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  
  scale_shape_manual(values = c(22,23,24)) +
  
  facet_grid(cols = vars(Epoch)) + 
  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -45, face = "italic"),
        axis.title.x = element_blank()) + 
  
  scale_x_discrete(expand = c(0.2,0.2),
                   limits = shark_genera_order) +
  
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(0,0,0,0))) +
  
  theme(panel.spacing = unit(0.1, "lines"),
        panel.border = element_rect(size = 0.3),
        plot.background = element_rect(size = 0.3),
        strip.background = element_rect(size = 0.3)) +
  
  scale_y_continuous(name = "offset d15N", 
                     sec.axis = sec_axis(~ . / 2.5, name = "offset TL")) +
  
  ylab("offset d15NEB") + 
  scale_fill_brewer(palette = "Set3")

plot_grid(d15n_vs_genus_by_epoch  + theme(legend.position = "none"), 
          d15n_offset_vs_genus_by_epoch,
          align = "hv",
          nrow = 2,
          ncol = 1,
          rel_heights = c(6,4))

# uncomment next line to save
#ggsave("Fig_02.pdf", path = "Plots", height = 5.5, width = 4.75, units = "in")


## Fig. 3 ------------------------------------------------------------------
# estimated O. megalodon diet d15N and modern marine mammal and shark literature d15N
#____________________________________


#### (A) estimated diet distribution ---------------------------------------------

tdf = 2.5 #setting the TDF equal to 2.5
meg_min <-  min(filter(N_data, genus_species == "Otodus megalodon")$d15n)
meg_max <-  max(filter(N_data, genus_species == "Otodus megalodon")$d15n)
meg_avg <- mean(filter(N_data, genus_species == "Otodus megalodon")$d15n)
meg_sd  <-   sd(filter(N_data, genus_species == "Otodus megalodon")$d15n)

meg_dist_plot <- N_data %>%
  filter(genus_species == "Otodus megalodon") %>%
  mutate(d15n_prey = d15n - tdf) %>%
  
  ggplot(aes(x = d15n_prey)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) + 
  
  geom_density(data = marmamm_lit, aes(x = d15N_mean, weight = n_ind),
               fill = "darkgoldenrod3", color = "darkgoldenrod3", alpha = 0.2) +
  geom_density(data = shark_lit, aes(x = d15N_mean, weight = n_ind), 
               fill = "dodgerblue4", color = "dodgerblue4", alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size = figures.textsize)) + 
  scale_x_continuous(limits = c(0,30),
                     breaks = c(0, 5, 10, 15, 20, 25), 
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  xlab("d15N - TDF")

#### (B) sharks ------------------------------------------------------------------
shark_filtered <- shark_lit %>%
  
  filter(family %in% filter(shark_family_summ, n_ind_family > 10)$family) %>%
  filter(!is.na(family))

diet_plot_shark <-  shark_filtered %>%
  filter(genus_species != "Carcharodon carcharias") %>%
  ggplot() +
  
  geom_vline(aes(xintercept = meg_min - tdf), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = meg_max - tdf), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = meg_avg - tdf), size = 1) +
  
  geom_point(aes(y = family, x = d15N_mean, size = n_ind, group = common_name),
             alpha = 0.5, color = "dodgerblue4") +
  geom_point(data = filter(shark_filtered,(genus_species == "Carcharodon carcharias")),
             aes(y = family, x = d15N_mean, group = common_name, size = n_ind), 
             alpha = 1, shape = 21, fill = "white", color = "dodgerblue4") +
  
  geom_point(data = filter(N_data, Epoch == "Modern"), aes(y = Family, x = d15n),
             shape = 4, color = "black", size = 2, stroke = .8) +
  
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0),
        text = element_text(size = figures.textsize)) +
  
  scale_x_continuous(limits = c(0,30), 
                     breaks = c(0, 5, 10, 15, 20, 25), 
                     expand = c(0,0)) + 
  
  scale_y_discrete(expand = c(.1,.1), 
                   limits = shark_family_order[shark_family_order %in% 
                                                 filter(shark_family_summ, 
                                                        n_ind_family > 10)$family]) +
  scale_size_continuous(limits = c(1,150))

#### (C) marine mammals ----------------------------------------------------------

# filtering to remove families that have less than 10 individuals measured
# note: this removes one family, Kogiidae which only had one measurement
marmamm_filtered <- marmamm_lit %>%
  filter(family %in% filter(marmamm_family_summ, n_ind_family > 10)$family) %>%
  filter(!is.na(family)) 

diet_plot_mamm <-  marmamm_filtered%>%
  filter((genus_species != "Ursus maritimus" | notes != "transient, mammalian prey")) %>%
  ggplot() +
  
  geom_vline(aes(xintercept = meg_min - tdf), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = meg_max - tdf), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = meg_avg - tdf), size = 1) +
  
  geom_point(aes(y = family, x = d15N_mean, group = common_name, size = n_ind), 
             alpha = 0.5, color = "darkgoldenrod3") +
  geom_point(data = filter(marmamm_filtered,(genus_species == "Ursus maritimus" | notes == "transient, mammalian prey")),
             aes(y = family, x = d15N_mean, group = common_name, size = n_ind), 
             alpha = 1, shape = 21, fill = "white", color = "darkgoldenrod3") +
  
  theme_bw() +
  xlab("d15N (permil vs. air)") +
  theme(axis.text.y = element_text(angle = 0),
        text = element_text(size = figures.textsize)) +
  
  scale_x_continuous(limits = c(0,30), 
                     breaks = c(0, 5, 10, 15, 20, 25), 
                     expand = c(0,0)) + 
  
  scale_y_discrete(expand = c(.1,.1),
                   limits = marmamm_family_order[marmamm_family_order %in% 
                                                   filter(marmamm_family_summ, n_ind_family > 10)$family]) +
  scale_size_continuous(limits = c(1,150))


#### plot --------------------------------------------------------------------
plot_grid(meg_dist_plot + theme(axis.title.y = element_blank()),
          diet_plot_shark + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()),
          diet_plot_mamm + theme(legend.position = "none", axis.title.y = element_blank()),
          align = "hv",
          nrow = 3,
          ncol = 1,
          rel_heights = c(8,length(filter(shark_family_summ, n_ind_family > 10)$family), 
                          length(filter(marmamm_family_summ, n_ind_family > 10)$family)))

#uncomment next line to save
#ggsave("Fig_03.pdf", path = "Plots", height = 6, width = 4.75, units = "in")


## Fig.  S1 ----------------------------------------------------------------
# Standards over time 
standard_enameloid_d15n <- standard_enameloid_summary %>%
  ggplot() + 
  # plot 1sd error bar for run averages
  geom_errorbar(aes(x = as.Date(rundate), ymin = d15n_mean - d15n_sd, 
                    ymax = d15n_mean + d15n_sd)) +
  # plot run averages
  geom_point(aes(x = as.Date(rundate), y = d15n_mean),
             shape = 16, fill = "black", size = 1.5) +
  # plot individual measurements
  geom_point(data = standard_enameloid, aes(x = as.Date(rundate), y = d15n),
             shape = 21, fill = "white", color = "black", size = 3) +
  # appearance and labels
  scale_y_continuous(limits = c(16, 20.5), expand = c(0,0)) +
  scale_x_date(date_labels = "%m-%Y") +
  theme_bw()  +
  ggtitle("Fossil enameloid standard") +
  ylab("d15N (permil vs. air)") +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 10))

standard_enameloid_ncont <- standard_enameloid_summary %>%
  ggplot() + 
  # plot 1sd error bar for run averages
  geom_errorbar(aes(x = as.Date(rundate), ymin = ncont_mean - ncont_sd, 
                    ymax = ncont_mean + ncont_sd)) +
  # plot run averages
  geom_point(aes(x = as.Date(rundate), y = ncont_mean),
             shape = 16, fill = "black", size = 1.5) +
  # plot individual measurements
  geom_point(data = standard_enameloid, aes(x = as.Date(rundate), y = ncont),
             shape = 21, fill = "white", color = "black", size = 3) +
  
  # appearance and labels
  scale_y_continuous(limits = c(5, 10), expand = c(0,0)) +
  scale_x_date(date_labels = "%m-%Y") +
  theme_bw()  +
  ylab("N content (umol / g)") +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank())


standard_coral_d15n <- standard_coral_summary %>%
  ggplot() + 
  # plot 1sd error bar for run averages
  geom_errorbar(aes(x = as.Date(rundate), ymin = d15n_mean - d15n_sd, 
                    ymax = d15n_mean + d15n_sd)) +
  # plot run averages
  geom_point(aes(x = as.Date(rundate), y = d15n_mean),
             shape = 16, fill = "black", size = 1.5) +
  # plot individual measurements
  geom_point(data = standard_coral, aes(x = as.Date(rundate), y = d15n),
             shape = 21, fill = "white", color = "black", size = 3) +
  
  # appearance and labels
  scale_y_continuous(limits = c(3, 7.5), expand = c(0,0)) +
  scale_x_date(limits = c(min(as.Date(standard_enameloid$rundate)), 
                          max(as.Date(standard_enameloid$rundate))), 
               date_labels = "%m-%Y") +
  theme_bw()  +
  ggtitle("Coral carbonate standard") +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 10))

standard_coral_ncont <- standard_coral_summary %>%
  ggplot() + 
  # plot 1sd error bar for run averages
  geom_errorbar(aes(x = as.Date(rundate), ymin = ncont_mean - ncont_sd, 
                    ymax = ncont_mean + ncont_sd)) +
  # plot run averages
  geom_point(aes(x = as.Date(rundate), y = ncont_mean),
             shape = 16, fill = "black", size = 1.5) +
  # plot individual measurements
  geom_point(data = standard_coral, aes(x = as.Date(rundate), y = ncont),
             shape = 21, fill = "white", color = "black", size = 3) +
  
  # appearance and labels
  scale_y_continuous(limits = c(0, 5), expand = c(0,0))  +
  scale_x_date(limits = c(min(as.Date(standard_enameloid$rundate)), 
                          max(as.Date(standard_enameloid$rundate))), 
               date_labels = "%m-%Y") +
  theme_bw()  +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# make combined plot
plot_grid(standard_enameloid_d15n, standard_coral_d15n, 
          standard_enameloid_ncont, standard_coral_ncont,
          nrow = 2, align = "hv")

# uncomment following line to save figure as pdf
# ggsave("Fig_S01.pdf", path = "Plots", height = 6, width = 7)



## Fig.  S2 ----------------------------------------------------------------
# d15N vs. N content
#____________________________________
d15N_vs_ncont <- ggplot() + 
  # linear regression of all fossil data
  geom_smooth(data =  filter(N_data, Epoch != "Modern"),
              aes(x = ncont, y = d15n),
              method = "lm", color = "black",
              size = 0.5) +
  # plot fossil piscivore data
  geom_point(data =  filter(N_data_piscivores, Epoch != "Modern"),
             aes(x = ncont, y = d15n), 
             fill = "grey70", color = "black", 
             shape = 21, size = figures.mrkrsize, stroke = 0.2) +
  # plot modern piscivore data
  geom_point(data =  filter(N_data_piscivores, Epoch == "Modern"),
             aes(x = ncont, y = d15n), 
             fill = "white", color = "black", 
             shape = 21, size = figures.mrkrsize, stroke = 0.5) +
  # plot fossil large shark data
  geom_point(data =  filter(N_data_megatooth, Epoch != "Modern"),
             aes(x = ncont, y = d15n, fill = genus_species, shape = Genus), 
             size = figures.mrkrsize, stroke = 0.2) +
  # plot modern large shark data
  geom_point(data =  filter(N_data_megatooth, Epoch == "Modern"),
             aes(x = ncont, y = d15n, color = genus_species, shape = Genus), 
             fill = "white", size = figures.mrkrsize, stroke = 0.5) +
  # appearance and labels
  scale_shape_manual(values = c(22,23,24)) +
  scale_fill_brewer(palette = "Set3") + 
  scale_color_brewer(palette = "Set3") + 
  scale_x_continuous(limits = c(0, 15), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(5, 30), expand = c(0, 0)) +
  ylab("d15NEB (permil vs. air)") + 
  xlab("N content (umol N / g enameloid)") +
  theme_bw()  +
  theme(text = element_text(size = figures.textsize))

d15N_vs_ncont + theme(legend.position = "none")

#uncomment next line to save plot to pdf
#ggsave("Fig_S02.pdf", path = "Plots/Megalodon", height = 4, width = 6, units = "in")


## Fig. S3 -----------------------------------------------------------------
# N content vs. genus by epoch
#____________________________________
ncont_vs_genus_by_epoch <-   N_data %>%
  ggplot(aes(x = Genus,y = ncont)) + 

  # plotting piscivore sharks
  geom_point(data =  N_data_piscivores,
             aes(x = Genus, y = ncont), fill = "grey40", color = "black", shape = 21, 
             size = 4, alpha = 0.5, stroke = 0.2) +
  # plotting large sharks
  geom_point(data =  N_data_megatooth,
             aes(x = Genus, y = ncont, fill = genus_species, shape = Genus), 
             size = 4, alpha = 0.7, stroke = 0.2) +
  # plotting 1sd error bars for genus averages
  geom_errorbar(data =  N_data_summ_epoch_genus,
                aes(x = Genus, ymin = ncont - ncont_sd, ymax = ncont + ncont_sd), 
                color = "black", width = 0.3, size = 0.5) +
  # plotting genus averages for piscivores
  geom_point(data =  filter(N_data_summ_epoch_genus, shark_type == "baseline"),
             aes(x = Genus, y = ncont), shape = 21, fill = "black",size = 4) +
  # plotting genus averages for large sharks
  geom_point(data =  filter(N_data_summ_epoch_genus, shark_type == "megatooth"),
             aes(x = Genus, y = ncont, shape = Genus), fill = "black",size = 4) +
  # adding sample size labels for each genus
  geom_text(data = N_data_summ_epoch_genus,
            aes(x = Genus, y = 1, label = paste("(",n_ind,")", sep = "")), size = 2) +
  # shapes for each large shark genus
  scale_shape_manual(values = c(22,23,24)) +
  
  # separating by epoch
  facet_grid(cols = vars(Epoch)) + 
  
  # appearance and labels
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, face = "italic", hjust = 1, vjust = 0.5),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        panel.border = element_rect(size = 0.3),
        plot.background = element_rect(size = 0.3),
        strip.background = element_rect(size = 0.3)) +
  
  scale_x_discrete(expand = c(0.2,0.2),
                   limits = shark_genera_order) +
  
  theme(legend.position = "none") +
  ylab("N content (umol/g)") + 
  scale_fill_brewer(palette = "Set3")

ncont_vs_genus_by_epoch

#uncomment next line to save plot to pdf
#ggsave("Fig_S03.pdf", path = "Plots", height = 4, width = 6, units = "in")







## Fig.  S4 ----------------------------------------------------------------
# baseline d15N changes: piscivore d15n vs. species by epoch
#____________________________________
d15n_baseline <- N_data %>%
  ggplot() + 
  
  # plot piscivore shark data
  geom_point(data =  N_data_piscivores,
             aes(x = genus_species, y = d15n), fill = "grey40", color = "black", shape = 21, 
             size = 3, alpha = 0.5, stroke = 0.2) +
  
  # plot error bars for species averages
  geom_errorbar(data =  filter(N_data_summ_epoch_species, shark_type == "baseline"),
                aes(x = genus_species, ymin = d15n - d15n_sd, ymax = d15n + d15n_sd), color = "black", width = 0.3) +
  # plot species averages
  geom_point(data =  filter(N_data_summ_epoch_species, shark_type == "baseline"),
             aes(x = genus_species, y = d15n), shape = 21, fill = "black",size = 3) +
  
  facet_grid(cols = vars(Epoch)) + 
  
  # appearance and labels
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, face = "italic", hjust = 1, vjust = 0.5),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        panel.border = element_rect(size = 0.3),
        plot.background = element_rect(size = 0.3),
        strip.background = element_rect(size = 0.3)) +
  
  scale_x_discrete(expand = c(0.2,0.2)) +
  scale_y_continuous(limits = c(0, 25), expand = c(0,0)) +
  ylab("d15NEB (permil vs. air)")
  
d15n_baseline + theme(legend.position = "none")

#uncomment next line to save plot as pdf
#ggsave("Fig_S04.pdf", path = "Plots", height = 4, width = 6, units = "in")




## Fig.  S5 ----------------------------------------------------------------
# d15N vs. genus by epoch and location
#____________________________________
# By location
d15n_vs_genus_by_epoch_and_loc <-   N_data %>%
  ggplot(aes(x = Genus,y = d15n)) + 
  
  # plot piscivore sharks
  geom_point(data =  filter(N_data, shark_type == "baseline"),
             aes(x = Genus, y = d15n, group = Location_group), fill = "grey40", color = "black", shape = 21, 
             size = 2.5, alpha = 0.8, stroke = 0.2) +
  # plot large sharks
  geom_point(data =  filter(N_data, shark_type == "megatooth"),
             aes(x = Genus, y = d15n, fill = genus_species, shape = Genus, group = Location_group), 
             size = 2.5, alpha = 0.8, stroke = 0.2) +
  # plot 1sd error bar for genus averages
  geom_errorbar(data =  N_data_summ_epoch_genus_loc,
                aes(x = Genus, ymin = d15n - d15n_sd, ymax = d15n + d15n_sd), 
                color = "black", width = 0.2) +
  # plot piscivore genus averages 
  geom_point(data =  filter(N_data_summ_epoch_genus_loc, shark_type == "baseline"),
             aes(x = Genus, y = d15n), shape = 21, fill = "black",size = 1.5) +
  # plot large shark genus averages 
  geom_point(data =  filter(N_data_summ_epoch_genus_loc, shark_type == "megatooth"),
             aes(x = Genus, y = d15n, shape = Genus), fill = "black",size = 1.5) +
  # add sample size labels for each genus
  geom_text(data = N_data_summ_epoch_genus_loc, 
            aes(x = Genus, y = 7.5, label = paste("(",n_ind,")", sep = "")), size = 2) +
 
  # shapes for each large shark genus
  scale_shape_manual(values = c(22,23,24)) +
  
  # separate by location and epoch
  facet_grid(cols = vars(Location_group), rows = vars(Epoch)) + 
  
  #appearance
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, face = "italic", hjust = 1, vjust = 0.5),
        axis.title.x = element_blank(),panel.spacing = unit(0.1, "lines"),
        panel.border = element_rect(size = 0.3),
        plot.background = element_rect(size = 0.3),
        strip.background = element_rect(size = 0.3)) +
  
  scale_x_discrete(expand = c(0.2,0.2),
                   limits = shark_genera_order) +
  scale_y_continuous(limits = c(5, 30), expand = c(0,0)) +
  theme(legend.position = "none") +
  ylab("d15NEB (permil vs. air)") + 
  scale_fill_brewer(palette = "Set3")

d15n_vs_genus_by_epoch_and_loc

#uncomment next line to save plot as pdf
#ggsave("Fig_S05.pdf", path = "Plots", height = 8, width = 6, units = "in")




## Fig.  S6 ----------------------------------------------------------------
# d15N vs. calculated length for O. megalodon
#____________________________________

d15n_vs_length <-  ggplot() + 
  # plot data with calculated length:
  geom_point(data = filter(N_data, !is.na(length_cm) & genus_species == "Otodus megalodon"),
             aes(x = length_cm, y = d15n, fill = Location_group),
             shape = 23, color = "black", size = 4) + 
  # plot data without calculated length at x=0:
  geom_point(data = filter(N_data, is.na(length_cm) & genus_species == "Otodus megalodon"),
             aes(x = 0, y = d15n, color = Location_group),
             shape = 5, size = 3, stroke = 1) +
  # appearance:
  theme_bw()  +
  scale_color_manual(values = c("goldenrod", "red","dodgerblue")) +
  scale_fill_manual(values = c("goldenrod", "red","dodgerblue")) +
  scale_x_continuous(limits = c(0,1200),
                     expand = c(0,0),
                     name = "Total length (cm)") + 
  scale_y_continuous(limits = c(5, 35),
                     expand = c(0,0),
                     breaks = c(5, 10, 15, 20, 25, 30),
                     name = "d15NEB (permil vs air)") 

d15n_vs_length + theme(legend.position = "none")

#uncomment next line to save plot to pdf
#ggsave("Fig_S06.pdf", path = "Plots", height = 3, width = 4, units = "in")


# 4. Analysis -------------------------------------------------------------


## Comparisons / t tests -------------------------------------------------------------

# getting table of comparisons to make, each line is one comparison for which we will do a t-test
comparison_tests <- read_excel("Data/comparison_tests.xlsx", sheet = "Sheet1")

# Initialize columns for ttest results
#..._d15n refers to comparing direct d15n measurements
#..._offset refers to comparing offset from contemporaneous piscivores
# sig_diff is YES if there is a significant difference
# sig_label is letter codes matching all items with insignificantly different results.
comparison_tests <- comparison_tests %>%
  mutate(t_d15n = NA,
         df_d15n = NA,
         p_d15n = NA,
         t_offset = NA,
         df_offset = NA,
         p_offset = NA
  )

# Separate comparison to contemporaneous piscivores: 
comparison_tests_vspisc <- comparison_tests %>%
  filter(testtype == "vs_pisc")

# Separate comparisons between megatooth genera or megatooth genera between epochs
comparison_tests_intspec <- comparison_tests %>%
  filter(testtype == "inter_species")


### d15N compared to other large sharks ---------------------------------------------------------
# ________T-TEST analysis for d15N values of large sharks________________________________________
for (k in 1:length(comparison_tests_intspec$testtype)) {
  tempdata <- ungroup(N_data) %>%
    filter((genus_species == comparison_tests_intspec$Group1_species[k] & 
              Epoch         == comparison_tests_intspec$Group1_epoch[k]) |
             (genus_species == comparison_tests_intspec$Group2_species[k] & 
                Epoch         == comparison_tests_intspec$Group2_epoch[k])) %>%
    select(d15n, Epoch, genus_species)
  
  ifelse(comparison_tests_intspec$Group1_species[k] == comparison_tests_intspec$Group2_species[k],
         ttest_result <- t.test(d15n ~ Epoch, data = tempdata),
         ttest_result <- t.test(d15n ~ genus_species, data = tempdata))
  
  comparison_tests_intspec$t_d15n[k]  <- ttest_result$statistic
  comparison_tests_intspec$df_d15n[k] <- ttest_result$parameter
  comparison_tests_intspec$p_d15n[k]  <- ttest_result$p.value
  
}


### d15N offset compared to other large sharks -------------------------------------------------
# ______T-TEST analysis for d15N offset of large sharks______________________


for (k in 1:length(comparison_tests_intspec$testtype)) {
  d15n_offset_1 <- filter(N_data_summ_epoch_species,
                          (genus_species == comparison_tests_intspec$Group1_species[k] & 
                             Epoch         == comparison_tests_intspec$Group1_epoch[k]))$d15n_offset
  d15nsd_offset_1 <- filter(N_data_summ_epoch_species,
                            (genus_species == comparison_tests_intspec$Group1_species[k] & 
                               Epoch         == comparison_tests_intspec$Group1_epoch[k]))$d15n_offset_sd
  n_ind_1 <- filter(N_data_summ_epoch_species,
                    (genus_species == comparison_tests_intspec$Group1_species[k] & 
                       Epoch         == comparison_tests_intspec$Group1_epoch[k]))$n_ind
  
  d15n_offset_2 <- filter(N_data_summ_epoch_species,
                          (genus_species == comparison_tests_intspec$Group2_species[k] & 
                             Epoch         == comparison_tests_intspec$Group2_epoch[k]))$d15n_offset
  d15nsd_offset_2 <- filter(N_data_summ_epoch_species,
                            (genus_species == comparison_tests_intspec$Group2_species[k] & 
                               Epoch         == comparison_tests_intspec$Group2_epoch[k]))$d15n_offset_sd
  n_ind_2 <- filter(N_data_summ_epoch_species,
                    (genus_species == comparison_tests_intspec$Group2_species[k] & 
                       Epoch         == comparison_tests_intspec$Group2_epoch[k]))$n_ind
  
  ttest_result <- tsum.test(d15n_offset_1, d15nsd_offset_1, n_ind_1, 
                            d15n_offset_2, d15nsd_offset_2, n_ind_2)
  
  comparison_tests_intspec$t_offset[k]  <- ttest_result$statistic
  comparison_tests_intspec$df_offset[k] <- ttest_result$parameter
  comparison_tests_intspec$p_offset[k]  <- ttest_result$p.value
  
}


### d15N compared to piscivores ---------------------------------------------
#________T-TEST for difference from piscivores (Fig. 2b) _____________________________________
for (k in 1:length(comparison_tests_vspisc$testtype)) {
  tempdata <- ungroup(N_data) %>%
    filter((genus_species == comparison_tests_vspisc$Group1_species[k] & 
              Epoch         == comparison_tests_vspisc$Group1_epoch[k]) |
             (shark_type == "baseline" & 
                Epoch         == comparison_tests_vspisc$Group2_epoch[k])) %>%
    select(d15n,shark_type)
  
  ttest_result <- t.test(d15n ~ shark_type, data = tempdata)
  
  comparison_tests_vspisc$t_d15n[k]  <- ttest_result$statistic
  comparison_tests_vspisc$df_d15n[k] <- ttest_result$parameter
  comparison_tests_vspisc$p_d15n[k]  <- ttest_result$p.value
  
}


### tables ------------------------------------------------------------------

comparison_tests_result <- comparison_tests_vspisc %>%
  full_join(comparison_tests_intspec)

#uncomment following five lines to save as csv file:
# write_csv(comparison_tests_result, 
#           paste("Data/Megalodon/Calculated data tables/",as.character(as.POSIXct(Sys.Date())),
#                 "_comparison_tests_result",
#                 ".csv", 
#                 sep = ""))

TS1_table_pisc <- comparison_tests_vspisc %>%
  select(c("Group1_epoch", "Group1_species","t_d15n","df_d15n","p_d15n")) %>%
  rename(epoch = Group1_epoch,
         species = Group1_species,
         t = t_d15n,
         df = df_d15n,
         p = p_d15n)

#uncomment following five lines to save as csv file:
# write_csv(TS1_table_pisc,
#           paste("Data/Calculated data tables/", as.character((Sys.Date())),
#                 "_TS1",
#                 ".csv",
#                 sep = ""))



## d15N vs N content correlations ------------------------------------------

# linear regression of all fossil data
fit_all = lm(d15n ~ ncont, filter(N_data, Epoch != "Modern"))
# linear regression of all fossil megatooth data
fit_megatooth = lm(d15n ~ ncont, filter(N_data_megatooth, Epoch != "Modern" & Genus == "Otodus"))
# linear regression of all fossil piscivore data
fit_piscivore = lm(d15n ~ ncont, filter(N_data_piscivores, Epoch != "Modern"))

# Pearson's correlation of all fossil data
pearson_r_all = cor.test(x = filter(N_data, Epoch != "Modern")$ncont,
                         y = filter(N_data, Epoch != "Modern")$d15n)
# Pearson's correlation of all fossil megatooth data
pearson_r_megatooth = cor.test(x = filter(N_data_megatooth, Epoch != "Modern" & Genus == "Otodus")$ncont,
                               y = filter(N_data_megatooth, Epoch != "Modern" & Genus == "Otodus")$d15n)
# Pearson's correlation of all fossil piscivore data
pearson_r_piscovore = cor.test(x = filter(N_data_piscivores, Epoch != "Modern")$ncont,
                               y = filter(N_data_piscivores, Epoch != "Modern")$d15n)


# Session Info ------------------------------------------------------------

# R version 4.1.0 (2021-05-18)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS 12.3.1
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] ggforce_0.3.3   deming_1.4      BSDA_1.2.1      lattice_0.20-44 cowplot_1.1.1   readxl_1.3.1    forcats_0.5.1  
# [8] stringr_1.4.0   dplyr_1.0.7     purrr_0.3.4     readr_2.1.2     tidyr_1.2.0     tibble_3.1.6    ggplot2_3.3.5  
# [15] tidyverse_1.3.1
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.8         lubridate_1.8.0    class_7.3-19       assertthat_0.2.1   digest_0.6.29      utf8_1.2.2        
# [7] R6_2.5.1           cellranger_1.1.0   backports_1.4.1    reprex_2.0.1       e1071_1.7-9        httr_1.4.2        
# [13] pillar_1.7.0       rlang_1.0.1        rstudioapi_0.13    Matrix_1.3-3       labeling_0.4.2     splines_4.1.0     
# [19] polyclip_1.10-0    munsell_0.5.0      proxy_0.4-26       broom_0.7.9        compiler_4.1.0     modelr_0.1.8      
# [25] pkgconfig_2.0.3    mgcv_1.8-35        tidyselect_1.1.1   fansi_1.0.2        crayon_1.4.2       tzdb_0.2.0        
# [31] dbplyr_2.1.1       withr_2.4.3        MASS_7.3-54        grid_4.1.0         nlme_3.1-152       jsonlite_1.7.3    
# [37] gtable_0.3.0       lifecycle_1.0.1    DBI_1.1.1          magrittr_2.0.2     scales_1.1.1       cli_3.1.1         
# [43] stringi_1.7.6      farver_2.1.0       mapproj_1.2.8      fs_1.5.2           xml2_1.3.3         ellipsis_0.3.2    
# [49] generics_0.1.2     vctrs_0.3.8        boot_1.3-28        RColorBrewer_1.1-2 tools_4.1.0        glue_1.6.1        
# [55] tweenr_1.0.2       maps_3.4.0         hms_1.1.1          colorspace_2.0-2   rvest_1.0.1        haven_2.4.3     
