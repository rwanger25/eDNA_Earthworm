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

summary(soil_chem_df)

#soil ph
sd(soil_chem_df$soil_pH)
IQR(soil_chem_df$soil_pH)

#buffer pH
sd(soil_chem_df$buffer_pH)
IQR(soil_chem_df$buffer_pH)

# % organic matter
sd(soil_chem_df$OM....)
IQR(soil_chem_df$OM....)

# nitrate
sd(soil_chem_df$NO3.N..mg.kg., na.rm = T)
IQR(soil_chem_df$NO3.N..mg.kg., na.rm = T)

#ammonium
sd(soil_chem_df$NH4.N..mg.kg.)
IQR(soil_chem_df$NH4.N..mg.kg.)

# phosphorous
sd(soil_chem_df$P..mg.kg.)
IQR(soil_chem_df$P..mg.kg.)

#potassium
sd(soil_chem_df$K..mg.kg..)
IQR(soil_chem_df$K..mg.kg..)

#magnesium
sd(soil_chem_df$Mg..mg.kg..)
IQR(soil_chem_df$Mg..mg.kg..)

#calcium
sd(soil_chem_df$Ca..mg.kg..)
IQR(soil_chem_df$Ca..mg.kg..)

#aluminium
sd(soil_chem_df$Al..mg.kg.)
IQR(soil_chem_df$Al..mg.kg.)

#boron
sd(soil_chem_df$B..mg.kg.)
IQR(soil_chem_df$B..mg.kg.)

#copper
sd(soil_chem_df$Cu..mg.kg.)
IQR(soil_chem_df$Cu..mg.kg.)

#iron
sd(soil_chem_df$Fe..mg.kg.)
IQR(soil_chem_df$Fe..mg.kg.)

#manganese
sd(soil_chem_df$Mn..mg.kg.)
IQR(soil_chem_df$Mn..mg.kg.)

#sodium
sd(soil_chem_df$Na..mg.kg.)
IQR(soil_chem_df$Na..mg.kg.)

#sulfur
sd(soil_chem_df$S..mg.kg.)
IQR(soil_chem_df$S..mg.kg.)

#zinc
sd(soil_chem_df$Zn..mg.kg.)
IQR(soil_chem_df$Zn..mg.kg.)

#ECEC
sd(soil_chem_df$ECEC..me.100g.)
IQR(soil_chem_df$ECEC..me.100g.)

# % K
sd(soil_chem_df$K....)
IQR(soil_chem_df$K....)

# % Mg
sd(soil_chem_df$Mg....)
IQR(soil_chem_df$Mg....)

# % Ca
sd(soil_chem_df$Ca....)
IQR(soil_chem_df$Ca....)

# % Acidity
sd(soil_chem_df$Acidity....)
IQR(soil_chem_df$Acidity....)


# Boxplot
soil_chem_df$buffer_pH

soil_pH_boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = soil_pH), fill = "#0492C2") +
  labs(x = "",
       y = "Soil pH",
       title = "Soil pH Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

buffer_pH_boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = buffer_pH), fill = "#0492C2") +
  labs(x = "",
       y = "Buffer pH",
       title = "Buffer pH Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

OM...._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = OM....), fill = "#0492C2") +
  labs(x = "",
       y = "Organic Matter (%)",
       title = "Organic Matter Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

NO3.N..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = NO3.N..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Nitrate-Nitrogen (mg/kg)",
       title = "Nitrate-Nitrogen Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

NH4.N..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = NH4.N..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Ammonium-Nitrogen (mg/kg)",
       title = "Ammonium-Nitrogen Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

P..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = P..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Phosphorous (mg/kg)",
       title = "Phosphorous Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

K..mg.kg.._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = K..mg.kg..), fill = "#0492C2") +
  labs(x = "",
       y = "Potassium (mg/kg)",
       title = "Potassium Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Mg..mg.kg.._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Mg..mg.kg..), fill = "#0492C2") +
  labs(x = "",
       y = "Magnesium (mg/kg)",
       title = "Magnesium Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Ca..mg.kg.._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Ca..mg.kg..), fill = "#0492C2") +
  labs(x = "",
       y = "Calcium (mg/kg)",
       title = "Calcium Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Al..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Al..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Aluminum (mg/kg)",
       title = "Aluminum Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

B..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = B..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Boron (mg/kg)",
       title = "Boron Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Cu..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Cu..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Copper (mg/kg)",
       title = "Copper Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Fe..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Fe..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Iron (mg/kg)",
       title = "Iron Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Mn..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Mn..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Manganese (mg/kg)",
       title = "Manganese Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Na..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Na..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Sodium (mg/kg)",
       title = "Sodium Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

S..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = S..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Sulfur (mg/kg)",
       title = "Sulfur Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Zn..mg.kg._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Zn..mg.kg.), fill = "#0492C2") +
  labs(x = "",
       y = "Zinc (mg/kg)",
       title = "Zinc Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

ECEC..me.100g._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = ECEC..me.100g.), fill = "#0492C2") +
  labs(x = "",
       y = "Effective Cation Exchange Capacity (me/100g)",
       title = "ECEC Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

K...._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = K....), fill = "#0492C2") +
  labs(x = "",
       y = "Potassium (%)",
       title = "Potassium Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Mg...._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Mg....), fill = "#0492C2") +
  labs(x = "",
       y = "Magnesium (%)",
       title = "Magnesium Across All Plots")+
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Ca...._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Ca....), fill = "#0492C2") +
  labs(x = "",
       y = "Calcium (%)",
       title = "Calcium Across All Plots") +
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

Acidity...._boxplot <- ggplot(data = soil_chem_df) +
  geom_boxplot(aes(y = Acidity....), fill = "#0492C2") +
  labs(x = "",
       y = "Acidity (%)",
       title = "Acidity Across All Plots")+
  interactive_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

grid.arrange(soil_pH_boxplot, buffer_pH_boxplot, OM...._boxplot, 
             NO3.N..mg.kg._boxplot, NH4.N..mg.kg._boxplot, P..mg.kg._boxplot,
             K..mg.kg.._boxplot, Mg..mg.kg.._boxplot, Ca..mg.kg.._boxplot,
             Al..mg.kg._boxplot, B..mg.kg._boxplot, Cu..mg.kg._boxplot, Fe..mg.kg._boxplot,
             Mn..mg.kg._boxplot, Na..mg.kg._boxplot, S..mg.kg._boxplot, Zn..mg.kg._boxplot,
             ECEC..me.100g._boxplot, K...._boxplot, Mg...._boxplot, Ca...._boxplot, Acidity...._boxplot)













