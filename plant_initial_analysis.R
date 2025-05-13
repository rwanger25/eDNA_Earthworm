library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)



#dev.off()

#reading in the data

soil_chem_df <- read.csv("Site_Data - Soil Chemistry Data.csv") #soil data
site_data_df <- read.csv("Site_Data - Master Data (2).csv") #Master data
trees_df <- read.csv("Site_Data - Tree Composition Data.csv") #Tree data
saplings_df <- read.csv("Site_Data - Sapling Composition Data.csv") #Sapling data
quadrat_df <- read.csv("Site_Data - Quadrat Composition Data.csv") #Quadrat data
species_codes <- read.csv("Site_Data - codes.csv") #species codes and full latin and common names


### cleaning the dataframes

# removing unnecessary rows
soil_chem_df <- soil_chem_df[-(57:68),] 
site_data_df <- site_data_df[-(57:64),] 
species_codes <- species_codes[-(109:112),] 

# adding columns with full latin names

# for all columns
site_data_df_full_names <- site_data_df %>% #site data
  mutate(across(everything(), ~ case_when(as.character(.) %in% species_codes$Code ~ species_codes$Latin_Name[match(as.character(.), species_codes$Code)],
                                          TRUE ~ as.character(.)))) #!(. %in% species_codes$Code)

trees_df_full_names <- trees_df %>% #tree species data
  mutate(across(everything(), ~ case_when(as.character(.) %in% species_codes$Code ~ species_codes$Latin_Name[match(as.character(.), species_codes$Code)],
                                          TRUE ~ as.character(.)))) #!(. %in% species_codes$Code)

saplings_df_full_names <- saplings_df %>% #seedling species data
  mutate(across(everything(), ~ case_when(as.character(.) %in% species_codes$Code ~ species_codes$Latin_Name[match(as.character(.), species_codes$Code)],
                                          TRUE ~ as.character(.)))) #!(. %in% species_codes$Code)

quadrat_df_full_names <- quadrat_df %>% #quadrat species data
  mutate(across(everything(), ~ case_when(as.character(.) %in% species_codes$Code ~ species_codes$Latin_Name[match(as.character(.), species_codes$Code)],
                                          TRUE ~ as.character(.)))) #!(. %in% species_codes$Code)

#making a theme
interactive_theme <- theme_bw() +# White background, black and white theme
  theme(plot.title = element_text(family = "Georgia", face = "bold", size = 20),
        axis.title = element_text(family = "DIN Alternate", size = 18),
        axis.text = element_text(size = 14),
        legend.title = element_text(family = "DIN Alternate", face = "bold", size = 14),
        legend.text = element_text(family = "DIN Alternate", size = 14),
        legend.box.margin = margin(6, 6, 6, 6))

#recoding variables
site_data_df_full_names <- site_data_df_full_names %>% #turning the columns numeric
  mutate(Elevation..m. = as.numeric(Elevation..m.)) %>%
  mutate(Slope = as.numeric(Slope)) %>%
  mutate(Aspect = as.numeric(Aspect)) %>%
  mutate(Temp = as.numeric(Temp)) %>%
  mutate(Temp.C = ((Temp - 32) * 5/9)) %>%
  mutate(Rain.in.last.24..in.hr. = as.numeric(Rain.in.last.24..in.hr.)) %>%
  mutate(Rain.in.last.24..cm.hr. = (Rain.in.last.24..in.hr. * 2.54)) %>%
  mutate(Crown.Density = as.numeric(Crown.Density)) %>%
  mutate(Overstory...cover = as.numeric(Overstory...cover)) %>%
  mutate(Understory...Cover = as.numeric(Understory...Cover)) %>%
  mutate(Herbaceous.Layer...Cover = as.numeric(Herbaceous.Layer...Cover)) %>%
  mutate(Dom.Moss...Cover = as.numeric(Dom.Moss...Cover)) %>%
  mutate(litter.cover..1 = as.numeric(litter.cover..1)) %>%
  mutate(litter.cover..2 = as.numeric(litter.cover..2)) %>%
  mutate(litter.cover..3 = as.numeric(litter.cover..3)) %>%
  mutate(litter.cover..4 = as.numeric(litter.cover..4)) 

#recoding the soil variables
soil_chem_df <- soil_chem_df %>% #turning the columns numeric
  mutate(soil_pH = as.numeric(soil_pH)) %>%
  mutate(buffer_pH = as.numeric(buffer_pH)) %>%
  mutate(NO3.N..mg.kg. = as.numeric(NO3.N..mg.kg.)) %>%
  mutate(P..mg.kg. = P..lb.A./2) %>% #adding the mg.kg instead of lb/A
  mutate(K..mg.kg.. = K..lb.A./2) %>%
  mutate(Mg..mg.kg.. = Mg..lb.A/2) %>%
  mutate(Ca..mg.kg.. = Ca..lb.A/2)

#recoding the tree variables
trees_df_full_names <- trees_df_full_names %>% #turning the columns numeric
  mutate(DBH = as.numeric(DBH)) 

#recoding the understory variables
saplings_df_full_names <- saplings_df_full_names %>% #turning the columns numeric
  mutate(DBH = as.numeric(DBH)) 

#recoding the understory variables
quadrat_df_full_names <- quadrat_df_full_names %>% #turning the columns numeric
  mutate(Count = as.numeric(Count)) 


#trees
tree_counts <- summarize(group_by(trees_df_full_names, Species), count = n(), prop = count/nrow(trees_df_full_names))

ggplot(data = tree_counts, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Tree Species",
       y = "Frequency",
       title = "Tree Species Counts Across Plots") +
  interactive_theme +
  coord_flip()
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))

#understory plants
sapling_counts <- summarize(group_by(saplings_df_full_names, Species), count = n(), prop = count/nrow(saplings_df_full_names))

ggplot(data = sapling_counts, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Sapling Species",
       y = "Frequency",
       title = "Sapling Species Counts Across Plots") +
  interactive_theme +
  coord_flip()
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

##herbaceous plants
herb_counts <- summarize(group_by(quadrat_df_full_names, Species), count = sum(Count), prop = count/nrow(quadrat_df_full_names))
herb_counts <- top_n(herb_counts, 10, count) #reording to get top 10
ggplot(data = herb_counts, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Herb Species",
       y = "Frequency",
       title = "Herb Species Counts Across Plots") +
  interactive_theme +
  coord_flip()
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

##invasive plants

quadrat_df_full_names_invasives <- filter(quadrat_df_full_names, Invasive. == "Y")

invasives_count <- summarize(group_by(quadrat_df_full_names_invasives, Species), count = sum(Count))
view(invasives_count)
ggplot(data = invasives_count, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Non-Native Species",
       y = "Frequency",
       title = "Non-Native Species Counts Across Plots") +
  interactive_theme +
  coord_flip()


