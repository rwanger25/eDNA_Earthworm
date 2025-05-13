library(tidyverse)
library(ggplot2)
library(dplyr)

#dev.off()

#reading in the data

soil_chem_df <- read.csv("Site_Data - Soil Chemistry Data.csv") #soil data
site_data_df <- read.csv("Site_Data - Master Data - 18.csv") #Master data
trees_df <- read.csv("Site_Data - Tree Composition Data.csv") #Tree data
saplings_df <- read.csv("Site_Data - Sapling Composition Data.csv") #Sapling data
quadrat_df <- read.csv("Site_Data - Quadrat Composition Data.csv") #Quadrat data
species_codes <- read.csv("Site_Data - codes.csv") #species codes and full latin and common names

View(site_data_df)

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

# averages for elevation, cover, leaf litter, etc. and histograms acoss plots

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

summary(site_data_df_full_names)

#elevation
mean(site_data_df_full_names$Elevation..m.)
IQR(site_data_df_full_names$Elevation..m.)
sd(site_data_df_full_names$Elevation..m.)
typeof(site_data_df_full_names$Elevation..m.)

ggplot(data = site_data_df_full_names, aes(x = Elevation..m.)) +
  geom_histogram(fill = "#0492C2", color = "#281E5D", 
                 binwidth = 50) +
  labs(x = "Elevation (m)",
       y = "Frequency",
       title = "Distribution of Elevation Across All Plots") +
  interactive_theme
  
#slope
mean(site_data_df_full_names$Slope)
IQR(site_data_df_full_names$Slope)
sd(site_data_df_full_names$Slope)

ggplot(data = site_data_df_full_names, aes(x = Slope)) +
  geom_histogram(fill = "#0492C2", color = "#281E5D", 
                 binwidth = 1) +
  labs(x = "Slope (º)",
       y = "Frequency",
       title = "Distribution of Slope across All Plots") +
  interactive_theme

#aspect
mean(site_data_df_full_names$Aspect, na.rm = T)
IQR(site_data_df_full_names$Aspect, na.rm = T)
sd(site_data_df_full_names$Aspect, na.rm = T)

ggplot(data = site_data_df_full_names, aes(x = Aspect)) +
  geom_histogram(fill = "#0492C2", color = "#281E5D", 
                 binwidth = 20) +
  labs(x = "Aspect (º)",
       y = "Frequency",
       title = "Distribution of Aspect Across All Plots") +
  interactive_theme

#temp
mean(site_data_df_full_names$Temp.C)
IQR(site_data_df_full_names$Temp.C)
sd(site_data_df_full_names$Temp.C)

ggplot(data = site_data_df_full_names, aes(x = Temp.C)) +
  geom_histogram(fill = "#0492C2", color = "#281E5D", 
                 binwidth = 2) +
  labs(x = "Temperature (ºC)",
       y = "Frequency",
       title = "Distribution of Temperature Across All Plots") +
  interactive_theme

ggplot(data = site_data_df_full_names, aes(x = Temp)) +
  geom_histogram(fill = "#0492C2", color = "#281E5D", 
                 binwidth = 2) +
  labs(x = "Temperature (ºF)",
       y = "Frequency",
       title = "Distribution of Temperature Across All Plots") +
  interactive_theme

#rain
mean(site_data_df_full_names$Rain.in.last.24..cm.hr., na.rm = T)
IQR(site_data_df_full_names$Rain.in.last.24..cm.hr., na.rm = T)
sd(site_data_df_full_names$Rain.in.last.24..cm.hr., na.rm = T)

ggplot(data = site_data_df_full_names, aes(x = Rain.in.last.24..in.hr.)) +
  geom_histogram(fill = "#0492C2", color = "#281E5D", 
                 binwidth = 0.01) +
  labs(x = "Rain in the Last 24 Hours (in)",
       y = "Frequency",
       title = "Distribution of Rain Across All Plots") +
  interactive_theme

ggplot(data = site_data_df_full_names, aes(x = Rain.in.last.24..cm.hr.)) +
  geom_histogram(fill = "#0492C2", color = "#281E5D", 
                 binwidth = 0.1) +
  labs(x = "Rain in the Last 24 Hours (cm)",
       y = "Frequency",
       title = "Distribution of Rain Across All Plots") +
  interactive_theme

#crown density
mean(site_data_df_full_names$Crown.Density)
IQR(site_data_df_full_names$Crown.Density)
sd(site_data_df_full_names$Crown.Density)

site_data_df_full_names %>%
  mutate(Fall = fct_relevel(Fall, c("Y", "N"))) %>%
  ggplot(aes(x = Crown.Density)) +
    geom_histogram(aes(fill = Fall), color = "#281E5D",   #if_else(Fall == "N",  "#0492C2", "red")),
                 binwidth = 5) +
    scale_fill_manual(values = c("#c20433", "#0492C2")) +
    labs(x = "Crown Density (%)",
       y = "Frequency",
       title = "Distribution of Crown Density Across All Plots") +
    interactive_theme

#overstory cover
mean(site_data_df_full_names$Overstory...cover, na.rm = T)
IQR(site_data_df_full_names$Overstory...cover, na.rm = T)
sd(site_data_df_full_names$Overstory...cover, na.rm = T)

site_data_df_full_names %>%
  mutate(Fall = fct_relevel(Fall, c("Y", "N"))) %>%
  ggplot(aes(x = Overstory...cover)) +
  geom_histogram(aes(fill = Fall), color = "#281E5D",
                 binwidth = 5) +
  scale_fill_manual(values = c("#c20433", "#0492C2")) +
  labs(x = "Estimated Overstory Coverage (%)",
       y = "Frequency",
       title = "Distribution of Estimated Overstory Coverage Across All Plots") +
  interactive_theme

#understory cover
mean(site_data_df_full_names$Understory...Cover, na.rm = T)
IQR(site_data_df_full_names$Understory...Cover, na.rm = T)
sd(site_data_df_full_names$Understory...Cover, na.rm = T)

site_data_df_full_names %>%
  mutate(Fall = fct_relevel(Fall, c("Y", "N"))) %>%
  ggplot(aes(x = Understory...Cover)) +
  geom_histogram(aes(fill = Fall), color = "#281E5D",
                 binwidth = 5, position = "dodge") +
  scale_fill_manual(values = c("#c20433", "#0492C2")) +
  labs(x = "Estimated Understory Coverage (%)",
       y = "Frequency",
       title = "Distribution of Estimated Understory Coverage Across All Plots") +
  interactive_theme

#herb cover
mean(site_data_df_full_names$Herbaceous.Layer...Cover, na.rm = T)
IQR(site_data_df_full_names$Herbaceous.Layer...Cover, na.rm = T)
sd(site_data_df_full_names$Herbaceous.Layer...Cover, na.rm = T)

site_data_df_full_names %>%
  mutate(Fall = fct_relevel(Fall, c("Y", "N"))) %>%
  ggplot(aes(x = Herbaceous.Layer...Cover)) +
  geom_histogram(aes(fill = Fall), color = "#281E5D",
                 binwidth = 5, position = "dodge") +
  scale_fill_manual(values = c("#c20433", "#0492C2")) +
  labs(x = "Estimated Herbaceous Coverage (%)",
       y = "Frequency",
       title = "Distribution of Estimated Herbaceous Coverage Across All Plots") +
  interactive_theme

#moss cover
mean(site_data_df_full_names$Dom.Moss...Cover,na.rm = T)
IQR(site_data_df_full_names$Dom.Moss...Cover,na.rm = T)
sd(site_data_df_full_names$Dom.Moss...Cover,na.rm = T)

site_data_df_full_names %>%
  mutate(Fall = fct_relevel(Fall, c("Y", "N"))) %>%
  ggplot(aes(x = Dom.Moss...Cover)) +
  geom_histogram(aes(fill = Fall), color = "#281E5D",
                 binwidth = 5, position = "dodge") +
  scale_fill_manual(values = c("#c20433", "#0492C2")) +
  labs(x = "Estimated Moss Coverage (%)",
       y = "Frequency",
       title = "Distribution of Estimated Moss Coverage Across All Plots") +
  interactive_theme


# average litter cover
site_data_df_leaf_litter <- site_data_df_full_names %>%
  select(litter.cover..1, litter.cover..2, litter.cover..3, litter.cover..4) %>%
  mutate(avg.litter.cover = rowMeans(site_data_df_leaf_litter))
mean(site_data_df_leaf_litter$avg.litter.cover)
IQR(site_data_df_leaf_litter$avg.litter.cover)
sd(site_data_df_leaf_litter$avg.litter.cover)
summary(site_data_df_leaf_litter$avg.litter.cover)

#adding the litter cover column to the original dataframe
site_data_df_full_names <- cbind(site_data_df_full_names, site_data_df_leaf_litter$avg.litter.cover) 
site_data_df_full_names <- site_data_df_full_names %>% #renaming column
  rename(avg.litter.cover = 'site_data_df_leaf_litter$avg.litter.cover')


site_data_df_full_names %>%
  mutate(Fall = fct_relevel(Fall, c("Y", "N"))) %>%
  ggplot(aes(x = avg.litter.cover)) +
  geom_histogram(aes(fill = Fall), color = "#281E5D",
                 binwidth = 5, position = "dodge") +
  scale_fill_manual(values = c("#c20433", "#0492C2")) +
  labs(x = "Estimated Litter Coverage (%)",
       y = "Frequency",
       title = "Distribution of Estimated Average Litter Coverage Across All Plots") +
  interactive_theme

## Counts

#soil moisture

soil.moisture.table <- summarize(group_by(site_data_df_full_names, soil.moisture), count = n(), prop = count/nrow(site_data_df_full_names)) #summarizing the total 
levels(site_data_df_full_names$soil.moisture)

ggplot(data = site_data_df_full_names, aes(x = soil.moisture)) +
    geom_bar(aes(fill = soil.moisture), color = "#281E5D") +
    scale_fill_manual(values = c("#0492C2", "#ADD8E6")) +
    labs(x = "Soil Moisture",
       y = "Frequency",
       title = "Frequency of Soil Moisture Types",
       fill = "Soil Moisture") +
    interactive_theme

ggplot(data = soil.moisture.table, aes(x = soil.moisture)) +
    geom_bar(aes(y = prop, fill = soil.moisture), stat = "identity", color = "#281E5D") +
    scale_fill_manual(values = c("#0492C2", "#ADD8E6")) +
    labs(x = "Soil Moisture",
       y = "Proportion",
       title = "Proportion of Soil Moisture Types",
       fill = "Soil Moisture") +
    interactive_theme

ggplot(data = site_data_df_full_names, aes(x = "", fill = soil.moisture)) +
  geom_bar(position = "fill", 
           color = "#281E5D") +
  scale_fill_manual(values = c("#0492C2", "#ADD8E6")) +
  labs(x = "Soil Moisture",
       y = "Frequency",
       title = "Frequency of Soil Moisture Types",
       fill = "Soil Moisture") +
  interactive_theme

#drainage
soil.drainage.table <- summarize(group_by(site_data_df_full_names, drainage), count = n(), prop = count/nrow(site_data_df_full_names)) #summarizing the total 


site_data_df_full_names %>%
  mutate(drainage = fct_relevel(drainage, c("Poor", "Moderate", "Well"))) %>%
  ggplot(aes(x = drainage)) +
    geom_bar(aes(fill = drainage), color = "#281E5D") +
    scale_fill_manual(values = c("#43270f", "#7C4700", "#997950")) +
    labs(x = "Soil Drainage",
       y = "Frequency",
       title = "Frequency of Soil Drainage Types",
       fill = "Soil Drainage") +
    interactive_theme

soil.drainage.table %>%
  mutate(drainage = fct_relevel(drainage, c("Poor", "Moderate", "Well"))) %>%
  ggplot(aes(x = drainage)) +
  geom_bar(aes(y = prop, fill = drainage), stat = "identity", color = "#281E5D") +
  scale_fill_manual(values = c("#43270f", "#7C4700", "#997950")) +
  labs(x = "Soil Drainage",
       y = "Proportion",
       title = "Proportion of Soil Drainage Types",
       fill = "Soil Drainage") +
  interactive_theme

site_data_df_full_names %>%
  mutate(drainage = fct_relevel(drainage, c("Poor", "Moderate", "Well"))) %>%
  ggplot(aes(x = "", fill = drainage)) +
  geom_bar(position = "fill", 
           color = "#281E5D") +
  scale_fill_manual(values = c("#43270f", "#7C4700", "#997950")) +
  labs(x = "Soil Drainage",
       y = "Frequency",
       title = "Frequency of Soil Drainage Types",
       fill = "Soil Drainage") +
  interactive_theme

  





  



