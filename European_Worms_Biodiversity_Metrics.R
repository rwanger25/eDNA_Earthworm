library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)

#dev.off()

#reading in the data

soil_chem_df <- read.csv("Site_Data - Soil Chemistry Data.csv") #soil data
site_data_df <- read.csv("Site_Data - Master Data - genetic.csv") #Master data
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
  mutate(litter.depth..cm. = as.numeric(litter.depth..cm.)) %>%
  mutate(Dom.Moss...Cover = as.numeric(Dom.Moss...Cover)) %>%
  mutate(litter.cover..1 = as.numeric(litter.cover..1)) %>%
  mutate(litter.cover..2 = as.numeric(litter.cover..2)) %>%
  mutate(litter.cover..3 = as.numeric(litter.cover..3)) %>%
  mutate(litter.cover..4 = as.numeric(litter.cover..4)) %>%
  rowwise () %>%
  mutate(avg.litter.cover = mean(c_across(c(litter.cover..1, litter.cover..2, litter.cover..3, litter.cover..4)), na.rm = TRUE)) %>%
  ungroup()



#creating a dataframe for the rows where we know whether there are european worms or not
site_data_df_full_names_nanopore <- subset(site_data_df_full_names, is.na(Detected.European) == F)
site_data_df_full_names_euro <- subset(site_data_df_full_names, Detected.European == 'Y')
site_data_df_full_names_failed.detect <- subset(site_data_df_full_names, Detected.European == 'N')

soil_chem_df_euro <- subset(soil_chem_df, is.na(soil_chem_df$European.Worms) == F)

# Mann-Whitney-Wilcoxon Test - Non-parametric

site_data_df_full_names_euro.chem <- soil_chem_df_euro %>%
  mutate(European.Worms = as.factor(European.Worms)) %>%
  mutate(soil_pH = as.numeric(soil_pH)) %>%
  mutate(buffer_pH = as.numeric(buffer_pH)) %>%
  mutate(OM.... = as.numeric(OM....)) %>%
  mutate(NO3.N..mg.kg. = as.numeric(NO3.N..mg.kg.)) %>%
  mutate(NH4.N..mg.kg. = as.numeric(NH4.N..mg.kg.)) %>%
  mutate(P..lb.A. = as.numeric(P..lb.A.)) %>%
  mutate(K..lb.A. = as.numeric(K..lb.A.)) %>%
  mutate(Mg..lb.A = as.numeric(Mg..lb.A)) %>%
  mutate(Ca..lb.A = as.numeric(Ca..lb.A)) %>%
  mutate(Al..mg.kg. = as.numeric(Al..mg.kg.)) %>%
  mutate(B..mg.kg. = as.numeric(B..mg.kg.)) %>%
  mutate(Cu..mg.kg. = as.numeric(Cu..mg.kg.)) %>%
  mutate(Fe..mg.kg. = as.numeric(Fe..mg.kg.)) %>%
  mutate(Mn..mg.kg. = as.numeric(Mn..mg.kg.)) %>%
  mutate(Fe..mg.kg. = as.numeric(Fe..mg.kg.)) %>%
  mutate(Na..mg.kg. = as.numeric(Na..mg.kg.)) %>%
  mutate(S..mg.kg. = as.numeric(S..mg.kg.)) %>%
  mutate(Zn..mg.kg. = as.numeric(Zn..mg.kg.)) %>%
  mutate(ECEC..me.100g. = as.numeric(ECEC..me.100g.)) %>% #*****
  mutate(K.... = as.numeric(K....)) %>%
  mutate(Mg.... = as.numeric(Mg....)) %>%
  mutate(Ca.... = as.numeric(Ca....)) %>%
  mutate(Acidity.... = as.numeric(Acidity....))


# Calculate species richness


## TREES

trees_df_full_names_w_invasives <- trees_df_full_names %>% #making a data frame with the tree counts and the invasive worm information
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Detected.European), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Lumbricus.rubellus.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Lumbricus.terrestris.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Aporrectodea.caliginosa.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Contamination), by = c("Site", "Plot"))


#1. creating abolute abundance tables 
trees_df_full_names_w_invasives.abundances <- trees_df_full_names_w_invasives %>%
  group_by(Plot, Site, Species) %>%
  summarise(count = n(), .groups = "drop")
# group_by(Plot, Site) %>%
# mutate(prop = count / sum(count)) %>%
# mutate(percent = prop*100)

#pivoting the table so rows are plots and columns are each tree species
tree_abundance_table <- trees_df_full_names_w_invasives.abundances %>%
  pivot_wider(names_from = Species,   # Make tree species into columns
              values_from = count,     # Fill cells with Abundance values
              values_fill = 0)              # Fill missing values with 0

#fixing the plot names
tree_abundance_table <- tree_abundance_table %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  


trees_df_full_names_w_invasives <- trees_df_full_names_w_invasives %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  

#adding whether the worms were detected to the abundance table

tree_abundance_table_joined <- tree_abundance_table %>%
  mutate(Detected.European = trees_df_full_names_w_invasives$Detected.European[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = trees_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = trees_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = trees_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)])

#remove unnecessary columns
tree_abundance_table.fixed <- tree_abundance_table_joined %>%
  select(c(!Site & !Plot & !Plot.name & !Detected.European & !Lumbricus.rubellus. & !Lumbricus.terrestris. & !Aporrectodea.caliginosa.))

#turning it into a matrix
tree_matrix <- as.matrix(tree_abundance_table.fixed)
Plot.name <- as.vector(tree_abundance_table_joined$Plot.name)
rownames(tree_matrix) <- Plot.name


# Example: calculate richness for each sample
richness <- specnumber(tree_matrix)
richness.tree <- data.frame(richness, tree_abundance_table_joined$Plot.name)

#adding the worm detections
richness.tree <- richness.tree %>%
  mutate(Detected.European = trees_df_full_names_w_invasives$Detected.European[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = trees_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = trees_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = trees_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
richness.tree <- richness.tree %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))


ggplot(aes(x = fct_reorder(tree_abundance_table_joined.Plot.name, richness, .desc = FALSE), y = richness), data = richness.tree) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Tree Richness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(tree_abundance_table_joined.Plot.name, richness, .desc = FALSE), y = richness), data = richness.tree) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Tree Richness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

tree.richness <- ggplot(aes(x = Detected.European, y = richness), data = richness.tree) +
  geom_boxplot() +
  labs(x = "", #Worms Detected?
       y = "Tree Richness") +
  interactive_theme 


#mann-whitney U test
wilcox.test(richness ~ Detected.European, data=richness.tree, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
richness.tree.no.na <- na.omit(richness.tree)
obs_diff <- with(richness.tree.no.na, mean(richness[Detected.European == "Y"]) - mean(richness[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(richness.tree.no.na$Detected.European)
  mean(richness.tree.no.na$richness[shuffled == "Y"]) - mean(richness.tree.no.na$richness[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")


## SAPLINGS

saplings_df_full_names_w_invasives <- saplings_df_full_names %>% 
  left_join(
    site_data_df_full_names_nanopore %>%
      dplyr::select(Site, Plot, Detected.European, 
                    Lumbricus.rubellus., 
                    Lumbricus.terrestris., 
                    Aporrectodea.caliginosa., 
                    Contamination),
    by = c("Site", "Plot")
  )



#1. creating abolute abundance tables 
saplings_df_full_names_w_invasives.abundances <- saplings_df_full_names_w_invasives %>%
  group_by(Plot, Site, Species) %>%
  summarise(count = n(), .groups = "drop")
# group_by(Plot, Site) %>%
# mutate(prop = count / sum(count)) %>%
# mutate(percent = prop*100)

#pivoting the table so rows are plots and columns are each saplings species
saplings_abundance_table <- saplings_df_full_names_w_invasives.abundances %>%
  pivot_wider(names_from = Species,   # Make saplings species into columns
              values_from = count,     # Fill cells with Abundance values
              values_fill = 0)              # Fill missing values with 0

#fixing the plot names
saplings_abundance_table <- saplings_abundance_table %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  


saplings_df_full_names_w_invasives <- saplings_df_full_names_w_invasives %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  

#adding whether the worms were detected to the abundance table

saplings_abundance_table_joined <- saplings_abundance_table %>%
  mutate(Detected.European = saplings_df_full_names_w_invasives$Detected.European[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = saplings_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = saplings_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = saplings_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)])

#remove unnecessary columns
saplings_abundance_table.fixed <- saplings_abundance_table_joined %>%
  select(c(!Site & !Plot & !Plot.name & !Detected.European & !Lumbricus.rubellus. & !Lumbricus.terrestris. & !Aporrectodea.caliginosa.))

#turning it into a matrix
saplings_matrix <- as.matrix(saplings_abundance_table.fixed)
Plot.name <- as.vector(saplings_abundance_table_joined$Plot.name)
rownames(saplings_matrix) <- Plot.name


# Example: calculate richness for each sample
richness.saplings <- specnumber(saplings_matrix)
richness.saplings.df <- data.frame(richness.saplings, saplings_abundance_table_joined$Plot.name)

#adding the worm detections
richness.saplings.df <- richness.saplings.df %>%
  mutate(Detected.European = saplings_df_full_names_w_invasives$Detected.European[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = saplings_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = saplings_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = saplings_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
richness.saplings.df <- richness.saplings.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(saplings_abundance_table_joined.Plot.name, richness.saplings, .desc = FALSE), y = richness.saplings), data = richness.saplings.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Richness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(saplings_abundance_table_joined.Plot.name, richness.saplings, .desc = FALSE), y = richness.saplings), data = richness.saplings.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Richness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

sapling.richness <- ggplot(aes(x = Detected.European, y = richness.saplings), data = richness.saplings.df) +
  geom_boxplot() +
  labs(x = "",
       y = "Sapling Richness") + #Worms Detected?
  interactive_theme 

#mann-whitney U test
wilcox.test(richness.saplings ~ Detected.European, data=richness.saplings.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
richness.saplings.no.na <- na.omit(richness.saplings.df)
obs_diff <- with(richness.saplings.no.na, mean(richness.saplings[Detected.European == "Y"]) - mean(richness.saplings[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(richness.saplings.no.na$Detected.European)
  mean(richness.saplings.no.na$richness.saplings[shuffled == "Y"]) - mean(richness.saplings.no.na$richness.saplings[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")


## QUADRAT

quadrat_df_full_names_w_invasives <- quadrat_df_full_names %>% 
  left_join(
    site_data_df_full_names_nanopore %>%
      dplyr::select(Site, Plot, Detected.European, 
                    Lumbricus.rubellus., 
                    Lumbricus.terrestris., 
                    Aporrectodea.caliginosa., 
                    Contamination),
    by = c("Site", "Plot")
  )

#removing plots where herb data was collected in the fall
quadrat_df_full_names_w_invasives <- quadrat_df_full_names_w_invasives %>%
  filter(!(quadrat_df_full_names_w_invasives$Date == "10/26/24"),
         !(quadrat_df_full_names_w_invasives$Date == "11/22/24"))


#1. creating abolute abundance tables 
quadrat_df_full_names_w_invasives.abundances <- quadrat_df_full_names_w_invasives %>%
  group_by(Plot, Site, Species) %>%
  summarise(count = n(), .groups = "drop")
# group_by(Plot, Site) %>%
# mutate(prop = count / sum(count)) %>%
# mutate(percent = prop*100)

#pivoting the table so rows are plots and columns are each quadrat species
quadrat_abundance_table <- quadrat_df_full_names_w_invasives.abundances %>%
  pivot_wider(names_from = Species,   # Make quadrat species into columns
              values_from = count,     # Fill cells with Abundance values
              values_fill = 0)              # Fill missing values with 0

#fixing the plot names
quadrat_abundance_table <- quadrat_abundance_table %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  


quadrat_df_full_names_w_invasives <- quadrat_df_full_names_w_invasives %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  

#adding whether the worms were detected to the abundance table

quadrat_abundance_table_joined <- quadrat_abundance_table %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

#remove unnecessary columns
quadrat_abundance_table.fixed <- quadrat_abundance_table_joined %>%
  select(c(!Site & !Plot & !Plot.name & !Detected.European & !Lumbricus.rubellus. & !Lumbricus.terrestris. & !Aporrectodea.caliginosa.))

#turning it into a matrix
quadrat_matrix <- as.matrix(quadrat_abundance_table.fixed)
Plot.name <- as.vector(quadrat_abundance_table_joined$Plot.name)
rownames(quadrat_matrix) <- Plot.name
View(quadrat_matrix)

# Example: calculate richness for each sample
richness.quadrat <- specnumber(quadrat_matrix)
richness.quadrat.df <- data.frame(richness.quadrat, quadrat_abundance_table_joined$Plot.name)

#adding the worm detections
richness.quadrat.df <- richness.quadrat.df %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
richness.quadrat.df <- richness.quadrat.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, richness.quadrat, .desc = FALSE), y = richness.quadrat), data = richness.quadrat.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Richness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, richness.quadrat, .desc = FALSE), y = richness.quadrat), data = richness.quadrat.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Richness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

herb.richness <- ggplot(aes(x = Detected.European, y = richness.quadrat), data = richness.quadrat.df) +
  geom_boxplot() +
  labs(x = "",
       y = "Herbaceous Layer Plant Richness") + #Worms Detected?
  interactive_theme 

#mann-whitney U test
wilcox.test(richness.quadrat ~ Detected.European, data=richness.quadrat.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
richness.quadrat.no.na <- na.omit(richness.quadrat.df)
obs_diff <- with(richness.quadrat.no.na, mean(richness.quadrat[Detected.European == "Y"]) - mean(richness.quadrat[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(richness.quadrat.no.na$Detected.European)
  mean(richness.quadrat.no.na$richness.quadrat[shuffled == "Y"]) - mean(richness.quadrat.no.na$richness.quadrat[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")




#arranging the boxplots
library(grid)
# Create custom text grobs for top and bottom
top_text <- textGrob("Distribution of Tree, Sapling, and Herb Richness by Earthworm Detection", gp = gpar(fontsize = 14, fontfamily = "Georgia", fontface = "bold"))
bottom_text <- textGrob("Invasive Earthworms?", gp = gpar(fontsize = 13, fontfamily = "DIN Alternate"))

grid.arrange(tree.richness, sapling.richness, herb.richness,
             ncol = 3, top = top_text, bottom = bottom_text)




# Calculate Shannon Index

## TREE

#calculating shannon's H
shannon_diversity.tree <- diversity(tree_matrix, index = "shannon")
shannon.tree.df <- data.frame(shannon_diversity.tree, tree_abundance_table_joined$Plot.name)

#adding the worm detections
shannon.tree.df <- shannon.tree.df %>%
  mutate(Detected.European = trees_df_full_names_w_invasives$Detected.European[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = trees_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = trees_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = trees_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
shannon.tree.df <- shannon.tree.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(tree_abundance_table_joined.Plot.name, shannon_diversity.tree, .desc = FALSE), y = shannon_diversity.tree), data = shannon.tree.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(tree_abundance_table_joined.Plot.name, shannon_diversity.tree, .desc = FALSE), y = shannon_diversity.tree), data = shannon.tree.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

tree.shannon <- ggplot(aes(x = Detected.European, y = shannon_diversity.tree), data = shannon.tree.df) +
  geom_boxplot() +
  labs(x = "", #Worms Detected?
       y = "Tree Shannon Diversity Index (H')") +
  interactive_theme 

#mann-whitney U test
wilcox.test(shannon_diversity.tree ~ Detected.European, data=shannon.tree.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
shannon_diversity.tree.no.na <- na.omit(shannon.tree.df)
obs_diff <- with(shannon_diversity.tree.no.na, mean(shannon_diversity.tree[Detected.European == "Y"]) - mean(shannon_diversity.tree[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(shannon_diversity.tree.no.na$Detected.European)
  mean(shannon_diversity.tree.no.na$shannon_diversity.tree[shuffled == "Y"]) - mean(shannon_diversity.tree.no.na$shannon_diversity.tree[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")



## SAPLINGS

#calculating shannon's H
shannon_diversity.saplings <- diversity(saplings_matrix, index = "shannon")
shannon.saplings.df <- data.frame(shannon_diversity.saplings, saplings_abundance_table_joined$Plot.name)

#adding the worm detections
shannon.saplings.df <- shannon.saplings.df %>%
  mutate(Detected.European = saplings_df_full_names_w_invasives$Detected.European[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = saplings_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = saplings_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = saplings_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
shannon.saplings.df <- shannon.saplings.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(saplings_abundance_table_joined.Plot.name, shannon_diversity.saplings, .desc = FALSE), y = shannon_diversity.saplings), data = shannon.saplings.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(saplings_abundance_table_joined.Plot.name, shannon_diversity.saplings, .desc = FALSE), y = shannon_diversity.saplings), data = shannon.saplings.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

sapling.shannon <- ggplot(aes(x = Detected.European, y = shannon_diversity.saplings), data = shannon.saplings.df) +
  geom_boxplot() +
  labs(x = "",
       y = "Sapling Shannon Diversity Index (H')") +
  interactive_theme 

#mann-whitney U test
wilcox.test(shannon_diversity.saplings ~ Detected.European, data=shannon.saplings.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
shannon_diversity.saplings.no.na <- na.omit(shannon.saplings.df)
obs_diff <- with(shannon_diversity.saplings.no.na, mean(shannon_diversity.saplings[Detected.European == "Y"]) - mean(shannon_diversity.saplings[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(shannon_diversity.saplings.no.na$Detected.European)
  mean(shannon_diversity.saplings.no.na$shannon_diversity.saplings[shuffled == "Y"]) - mean(shannon_diversity.saplings.no.na$shannon_diversity.saplings[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")



## QUADRAT

#calculating shannon's H
shannon_diversity.quadrat <- diversity(quadrat_matrix, index = "shannon")
shannon.quadrat.df <- data.frame(shannon_diversity.quadrat, quadrat_abundance_table_joined$Plot.name)

#adding the worm detections
shannon.quadrat.df <- shannon.quadrat.df %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
shannon.quadrat.df <- shannon.quadrat.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, shannon_diversity.quadrat, .desc = FALSE), y = shannon_diversity.quadrat), data = shannon.quadrat.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, shannon_diversity.quadrat, .desc = FALSE), y = shannon_diversity.quadrat), data = shannon.quadrat.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

herb.shannon <- ggplot(aes(x = Detected.European, y = shannon_diversity.quadrat), data = shannon.quadrat.df) +
  geom_boxplot() +
  labs(x = "", #Worms Detected?
       y = "Herbaceous Layer Shannon Diversity Index (H')") +
  interactive_theme 

#mann-whitney U test
wilcox.test(shannon_diversity.quadrat ~ Detected.European, data=shannon.quadrat.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
shannon_diversity.quadrat.no.na <- na.omit(shannon.quadrat.df)
obs_diff <- with(shannon_diversity.quadrat.no.na, mean(shannon_diversity.quadrat[Detected.European == "Y"]) - mean(shannon_diversity.quadrat[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(shannon_diversity.quadrat.no.na$Detected.European)
  mean(shannon_diversity.quadrat.no.na$shannon_diversity.quadrat[shuffled == "Y"]) - mean(shannon_diversity.quadrat.no.na$shannon_diversity.quadrat[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")



#arranging the boxplots
library(grid)
# Create custom text grobs for top and bottom
top_text <- textGrob("Distribution of Tree, Sapling, and Herb Shannon Diversity Index by Earthworm Detection", gp = gpar(fontsize = 14, fontfamily = "Georgia", fontface = "bold"))
bottom_text <- textGrob("Invasive Earthworms?", gp = gpar(fontsize = 13, fontfamily = "DIN Alternate"))

grid.arrange(tree.shannon, sapling.shannon, herb.shannon,
             ncol = 3, top = top_text, bottom = bottom_text)




###species evenness

#TREE
evenness.tree <- shannon_diversity.tree / log(richness.tree$richness)
evenness.tree.df <- data.frame(evenness.tree, tree_abundance_table_joined$Plot.name)

#replacing the infinites where the richness is 1 with 0 for evenness
evenness.tree.df$evenness.tree[evenness.tree.df$evenness.tree == Inf] <- 0

#adding the worm detections
evenness.tree.df <- evenness.tree.df %>%
  mutate(Detected.European = trees_df_full_names_w_invasives$Detected.European[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = trees_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = trees_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = trees_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
evenness.tree.df <- evenness.tree.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(tree_abundance_table_joined.Plot.name, evenness.tree.df, .desc = FALSE), y = evenness.tree), data = evenness.tree.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(tree_abundance_table_joined.Plot.name, evenness.tree.df, .desc = FALSE), y = evenness.tree), data = evenness.tree.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Species Evenness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

tree.evenness <- ggplot(aes(x = Detected.European, y = evenness.tree), data = evenness.tree.df) +
  geom_boxplot() +
  labs(x = "", #Worms Detected?
       y = "Tree Species Evenness (J)") +
  interactive_theme 

#mann-whitney U test
wilcox.test(evenness.tree ~ Detected.European, data=evenness.tree.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
evenness.tree.no.na <- na.omit(evenness.tree.df)
obs_diff <- with(evenness.tree.no.na, mean(evenness.tree[Detected.European == "Y"]) - mean(evenness.tree[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(evenness.tree.no.na$Detected.European)
  mean(evenness.tree.no.na$evenness.tree[shuffled == "Y"]) - mean(evenness.tree.no.na$evenness.tree[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")



#SAPLING


evenness.saplings <- shannon_diversity.saplings / log(richness.saplings.df$richness)
evenness.saplings.df <- data.frame(evenness.saplings, saplings_abundance_table_joined$Plot.name)

#replacing the infinites where the richness is 1 with 0 for evenness
evenness.saplings.df$evenness.saplings[is.nan(evenness.saplings.df$evenness.saplings) == T] <- 0

#adding the worm detections
evenness.saplings.df <- evenness.saplings.df %>%
  mutate(Detected.European = saplings_df_full_names_w_invasives$Detected.European[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = saplings_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = saplings_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = saplings_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
evenness.saplings.df <- evenness.saplings.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(saplings_abundance_table_joined.Plot.name, evenness.saplings, .desc = FALSE), y = evenness.saplings), data = evenness.saplings.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(saplings_abundance_table_joined.Plot.name, evenness.saplings, .desc = FALSE), y = evenness.saplings), data = evenness.saplings.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Species Evenness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

sapling.eveness <- ggplot(aes(x = Detected.European, y = evenness.saplings), data = evenness.saplings.df) +
  geom_boxplot() +
  labs(x = "", #Worms Detected?
       y = "Sapling Species Evenness (J)") +
  interactive_theme 

#mann-whitney U test
wilcox.test(evenness.saplings ~ Detected.European, data=evenness.saplings.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
evenness.saplings.no.na <- na.omit(evenness.saplings.df)
obs_diff <- with(evenness.saplings.no.na, mean(evenness.saplings[Detected.European == "Y"]) - mean(evenness.saplings[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(evenness.saplings.no.na$Detected.European)
  mean(evenness.saplings.no.na$evenness.saplings[shuffled == "Y"]) - mean(evenness.saplings.no.na$evenness.saplings[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")


#QUADRAT

evenness.quadrat <- shannon_diversity.quadrat / log(richness.quadrat.df$richness.quadrat)
evenness.quadrat.df <- data.frame(evenness.quadrat, quadrat_abundance_table_joined$Plot.name)

#replacing the infinites where the richness is 1 with 0 for evenness
evenness.quadrat.df$evenness.quadrat[is.nan(evenness.quadrat.df$evenness.quadrat) == T] <- 0

#adding the worm detections
evenness.quadrat.df <- evenness.quadrat.df %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
evenness.quadrat.df <- evenness.quadrat.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, evenness.quadrat, .desc = FALSE), y = evenness.quadrat), data = evenness.quadrat.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Species Evenness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, evenness.quadrat, .desc = FALSE), y = evenness.quadrat), data = evenness.quadrat.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Species Evenness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

herb.evenness <- ggplot(aes(x = Detected.European, y = evenness.quadrat), data = evenness.quadrat.df) +
  geom_boxplot() +
  labs(x = "",
       y = "Herbaceous Layer Species Evenness (J)") +
  interactive_theme 


#mann-whitney U test
wilcox.test(evenness.quadrat ~ Detected.European, data=evenness.quadrat.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
evenness.quadrat.no.na <- na.omit(evenness.quadrat.df)
obs_diff <- with(evenness.quadrat.no.na, mean(evenness.quadrat[Detected.European == "Y"]) - mean(evenness.quadrat[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(evenness.quadrat.no.na$Detected.European)
  mean(evenness.quadrat.no.na$evenness.quadrat[shuffled == "Y"]) - mean(evenness.quadrat.no.na$evenness.quadrat[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")



#arranging the boxplots
library(grid)
# Create custom text grobs for top and bottom
top_text <- textGrob("Distribution of Tree, Sapling, and Herb Pielou's Species Evenness Index by Earthworm Detection", gp = gpar(fontsize = 14, fontfamily = "Georgia", fontface = "bold"))
bottom_text <- textGrob("Invasive Earthworms?", gp = gpar(fontsize = 13, fontfamily = "DIN Alternate"))

grid.arrange(tree.evenness, sapling.eveness, herb.evenness,
             ncol = 3, top = top_text, bottom = bottom_text)








### NON-NATIVE PLANTS 

quadrat_df_full_names_w_invasives <- quadrat_df_full_names %>% #making a data frame with the tree counts and the invasive worm information
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Detected.European), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Lumbricus.rubellus.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Lumbricus.terrestris.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Aporrectodea.caliginosa.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Contamination), by = c("Site", "Plot"))


quadrat_df_full_names_invasives <- filter(quadrat_df_full_names_w_invasives, Invasive. == "Y")

invasives.Species.table <- quadrat_df_full_names_invasives %>%
  group_by(Species, Detected.European) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Detected.European) %>%
  mutate(prop = count / sum(count))

quadrat_df_full_names_w_invasives <- quadrat_df_full_names %>% 
  left_join(
    site_data_df_full_names_nanopore %>%
      dplyr::select(Site, Plot, Detected.European, 
                    Lumbricus.rubellus., 
                    Lumbricus.terrestris., 
                    Aporrectodea.caliginosa., 
                    Contamination),
    by = c("Site", "Plot")
  )
view(quadrat_df_full_names)


#1. creating abolute abundance tables 
invasive_df_full_names_w_invasives.abundances <- quadrat_df_full_names_invasives %>%
  group_by(Plot, Site, Species) %>%
  summarise(count = n(), .groups = "drop")
# group_by(Plot, Site) %>%
# mutate(prop = count / sum(count)) %>%
# mutate(percent = prop*100)

#pivoting the table so rows are plots and columns are each quadrat species
invasive_abundance_table <- invasive_df_full_names_w_invasives.abundances %>%
  pivot_wider(names_from = Species,   # Make quadrat species into columns
              values_from = count,     # Fill cells with Abundance values
              values_fill = 0)              # Fill missing values with 0

#fixing the plot names
invasive_abundance_table <- invasive_abundance_table %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  


quadrat_df_full_names_w_invasives <- quadrat_df_full_names_w_invasives %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  

#adding whether the worms were detected to the abundance table

invasive_abundance_table_joined <- invasive_abundance_table %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

#remove unnecessary columns
invasive_abundance_table_joined.fixed <- invasive_abundance_table_joined %>%
  select(c(!Site & !Plot & !Plot.name & !Detected.European & !Lumbricus.rubellus. & !Lumbricus.terrestris. & !Aporrectodea.caliginosa.))

#turning it into a matrix
invasive_matrix <- as.matrix(invasive_abundance_table_joined.fixed)
Plot.name <- as.vector(invasive_abundance_table_joined$Plot.name)
rownames(invasive_matrix) <- Plot.name


# Example: calculate richness for each sample
richness.invasive <- specnumber(invasive_matrix)
richness.invasive.df <- data.frame(richness.invasive, invasive_abundance_table_joined$Plot.name)


#adding the worm detections
richness.invasive.df <- richness.invasive.df %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(invasive_abundance_table_joined.Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(invasive_abundance_table_joined.Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(invasive_abundance_table_joined.Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(invasive_abundance_table_joined.Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
richness.invasive.df <- richness.invasive.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(invasive_abundance_table_joined.Plot.name, richness.invasive, .desc = FALSE), y = richness.invasive), data = richness.invasive.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Richness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))


ggplot(aes(x = fct_reorder(invasive_abundance_table_joined.Plot.name, richness.invasive, .desc = FALSE), y = richness.invasive), data = richness.invasive.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Richness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

invasive.richness <- ggplot(aes(x = Detected.European, y = richness.invasive), data = richness.invasive.df) +
  geom_boxplot() +
  labs(x = "",
       y = "Richness") + #Worms Detected?
  interactive_theme 

#mann-whitney U test
wilcox.test(richness.invasive ~ Detected.European, data=richness.invasive.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
richness.invasive.no.na <- na.omit(richness.invasive.df)
obs_diff <- with(richness.invasive.no.na, mean(richness.invasive[Detected.European == "Y"]) - mean(richness.invasive[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(richness.invasive.no.na$Detected.European)
  mean(richness.invasive.no.na$richness.invasive[shuffled == "Y"]) - mean(richness.invasive.no.na$richness.invasive[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")



#calculating shannon's H
shannon_diversity.invasive <- diversity(invasive_matrix, index = "shannon")
shannon.invasive.df <- data.frame(shannon_diversity.invasive, invasive_abundance_table_joined$Plot.name)

#adding the worm detections
shannon.invasive.df <- shannon.invasive.df %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
shannon.invasive.df <- shannon.invasive.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, shannon_diversity.invasive, .desc = FALSE), y = shannon_diversity.quadrat), data = shannon.invasive.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, shannon_diversity.invasive, .desc = FALSE), y = shannon_diversity.quadrat), data = shannon.invasive.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "shannon") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

invasive.shannon <- ggplot(aes(x = Detected.European, y = shannon_diversity.invasive), data = shannon.invasive.df) +
  geom_boxplot() +
  labs(x = "", #Worms Detected?
       y = "Shannon Diversity Index (H')") +
  interactive_theme 

#mann-whitney U test
wilcox.test(shannon_diversity.invasive ~ Detected.European, data=shannon.invasive.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
shannon_diversity.invasive.no.na <- na.omit(shannon.invasive.df)
obs_diff <- with(shannon_diversity.invasive.no.na, mean(shannon_diversity.invasive[Detected.European == "Y"]) - mean(shannon_diversity.invasive[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(shannon_diversity.invasive.no.na$Detected.European)
  mean(shannon_diversity.invasive.no.na$shannon_diversity.invasive[shuffled == "Y"]) - mean(shannon_diversity.invasive.no.na$shannon_diversity.invasive[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")


##evenness

evenness.invasive <- shannon_diversity.invasive / log(richness.invasive.df$richness.invasive)
evenness.invasive.df <- data.frame(evenness.invasive, invasive_abundance_table_joined$Plot.name)

#replacing the infinites where the richness is 1 with 0 for evenness
evenness.invasive.df$evenness.invasive[is.nan(evenness.invasive.df$evenness.invasive) == T] <- 0

#adding the worm detections
evenness.invasive.df <- evenness.invasive.df %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

#adding an extra column for when both worms are present
evenness.invasive.df <- evenness.invasive.df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, evenness.invasive, .desc = FALSE), y = evenness.invasive), data = evenness.invasive.df) +
  geom_bar(stat = "identity", aes(fill = Detected.European)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Species Evenness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

ggplot(aes(x = fct_reorder(quadrat_abundance_table_joined.Plot.name, evenness.invasive, .desc = FALSE), y = evenness.invasive), data = evenness.invasive.df) +
  geom_bar(stat = "identity", aes(fill = worm_species)) +
  coord_flip() +
  labs(x = "Plots",
       y = "Species Evenness") +
  interactive_theme +
  theme(axis.text = element_text(size = 10))

invasive.evenness <- ggplot(aes(x = Detected.European, y = evenness.invasive), data = evenness.invasive.df) +
  geom_boxplot() +
  labs(x = "",
       y = "Pielou's Species Evenness Index (J)") +
  interactive_theme 


#mann-whitney U test
wilcox.test(evenness.invasive ~ Detected.European, data=evenness.invasive.df, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
evenness.invasive.no.na <- na.omit(evenness.invasive.df)
obs_diff <- with(evenness.invasive.no.na, mean(evenness.invasive[Detected.European == "Y"]) - mean(evenness.invasive[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(evenness.invasive.no.na$Detected.European)
  mean(evenness.invasive.no.na$evenness.invasive[shuffled == "Y"]) - mean(evenness.invasive.no.na$evenness.invasive[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")


#arranging the boxplots
library(grid)
# Create custom text grobs for top and bottom
top_text <- textGrob("Distribution of Non-Native Plant Species Diversity Metrics by Earthworm Detection", gp = gpar(fontsize = 14, fontfamily = "Georgia", fontface = "bold"))
bottom_text <- textGrob("Invasive Earthworms?", gp = gpar(fontsize = 13, fontfamily = "DIN Alternate"))

grid.arrange(invasive.richness, invasive.shannon, invasive.evenness,
             ncol = 3, top = top_text, bottom = bottom_text)




























### GLMMS


#adding richness, diveristy to main data set 

richness.quadrat.df

#getting full data with plot.name columns to allow for easier dataframe joining
site_data_df_full_names.plot.name <- site_data_df_full_names %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  

richness.tree <- as.data.frame(richness.tree)
richness.tree$richness <- as.numeric(richness.tree$richness)

#adding in evenness, shannon diversity, and richness columns
site_data_df_full_names.plot.name <- site_data_df_full_names.plot.name %>%
  mutate(richness.quadrat = richness.quadrat.df$richness.quadrat[match(Plot.name, 
                                                                       richness.quadrat.df$quadrat_abundance_table_joined.Plot.name)]) %>%
  mutate(richness.saplings = richness.saplings.df$richness.saplings[match(Plot.name, 
                                                                        richness.saplings.df$saplings_abundance_table_joined.Plot.name)]) %>%
#  mutate(richness.tree = richness.tree$richness[match(Plot.name, 
                                                                 #richness.tree$tree_abundance_table_joined.Plot.name)]) %>%
  mutate(shannon_diversity.quadrat = shannon.quadrat.df$shannon_diversity.quadrat[match(Plot.name, 
                                                                       shannon.quadrat.df$quadrat_abundance_table_joined.Plot.name)]) %>%
  mutate(shannon_diversity.saplings = shannon.saplings.df$shannon_diversity.saplings[match(Plot.name, 
                                                            shannon.saplings.df$saplings_abundance_table_joined.Plot.name)]) %>%
  mutate(shannon_diversity.tree = shannon.tree.df$shannon_diversity.tree[match(Plot.name, 
                                                        shannon.tree.df$tree_abundance_table_joined.Plot.name)]) %>%
  mutate(evenness.quadrat = evenness.quadrat.df$evenness.quadrat[match(Plot.name, 
                                                                     evenness.quadrat.df$quadrat_abundance_table_joined.Plot.name)]) %>%
  mutate(evenness.saplings = evenness.saplings.df$evenness.saplings[match(Plot.name, 
                                                                      evenness.saplings.df$saplings_abundance_table_joined.Plot.name)]) %>%
  mutate(evenness.tree = evenness.tree.df$evenness.tree[match(Plot.name, 
                                                                  evenness.tree.df$tree_abundance_table_joined.Plot.name)]) 



#creating a worm species column
site_data_df_full_names.plot.name <- site_data_df_full_names.plot.name %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))





# Example with lme4
library(lme4)
site_data_df_full_names.plot.name$Detected.European <- as.factor(site_data_df_full_names.plot.name$Detected.European)
site_data_df_full_names.plot.name$worm_species <- as.factor(site_data_df_full_names.plot.name$worm_species)


#scaling

site_data_df_full_names.plot.name <- site_data_df_full_names.plot.name %>%
  mutate(across(
    c(litter.depth..cm., avg.litter.cover, shannon_diversity.quadrat,
      shannon_diversity.saplings, shannon_diversity.tree),
    scale
  ))

#looking for multicollinearity 
library(car)
vif(lm(worm_species ~ litter.depth..cm. + avg.litter.cover +
         shannon_diversity.quadrat + shannon_diversity.saplings + shannon_diversity.tree,
       data = site_data_df_full_names.plot.name))

#removing NAs for dredge
site_data_df_full_names.plot.name.no.na <- site_data_df_full_names.plot.name %>%
  filter(!is.na(Detected.European),
         !is.na(litter.depth..cm.),
         !is.na(avg.litter.cover),
         !is.na(shannon_diversity.quadrat),
         !is.na(shannon_diversity.saplings),
         !is.na(shannon_diversity.tree),
         !is.na(Site))

site_data_df_full_names.plot.name.no.na$Herbaceous.Layer...Cover

site_data_df_full_names.plot.name.no.na <- site_data_df_full_names.plot.name.no.na %>%
  filter(!(Site == "Scopan Lake")) %>%
  mutate(Site = as.factor(Site)) %>%
  mutate(Region = case_when(Site %in% c("Androscoggin Riverlands", "Bradbury Mountain", "Bradley", 
                              "Coastal Botanical Gardens", "Colby Arboretum", "Colby Hume", "Kennebec Highlands",
                              "Kennebec Public Land Trust", "Viles Arboretum") ~ "Central Interior & Midcoast",
                            Site %in% c("Bigelow", "Little Moose", "Mahoosuc") ~ "Central & Western Mountains",
                            Site %in% c("Falmouth Land Trust", "Seboeis Lake") ~ "Central & Eastern Lowlands",
                            Site == "Sebago Lake" ~ "Southern Maine")) #Site == "Scopan Lake" ~ "Aroostook Hills & Lowlands",
  
table(site_data_df_full_names.plot.name.no.na$Region)


model <- glmer(Detected.European ~  drainage + Crown.Density + shannon_diversity.quadrat +
                 shannon_diversity.tree + (1|Region),
             data = site_data_df_full_names.plot.name.no.na, family = binomial)# control = glmerControl(optimizer = "nlminb", optCtrl = list(maxeval = 10000))#
summary(model)

#attempting

library(brms)

# Fit Bayesian GLMM
bayes_model <- brm(
  Detected.European ~ drainage + Crown.Density + 
    shannon_diversity.quadrat + shannon_diversity.tree + (1 | Plot.name),
  data = site_data_df_full_names.plot.name.no.na,
  family = bernoulli(link = "logit"),
  chains = 4,
  iter = 4000,
  control = list(adapt_delta = 0.95, max_treedepth = 15)  # helps with convergence
)

### GLM, should not use because not great if data collected is non-random
model <- glm(Detected.European ~ Elevation..m. + Slope + litter.depth..cm. + Herbaceous.Layer...Cover +
               soil.moisture + drainage + avg.litter.cover + Crown.Density + shannon_diversity.quadrat +
                 shannon_diversity.saplings + shannon_diversity.tree + Site,
               data = site_data_df_full_names.plot.name.no.na, family = binomial(link = "logit"))# control = glmerControl(optimizer = "nlminb", optCtrl = list(maxeval = 10000))#
summary(model)

table(site_data_df_full_names.plot.name$worm_species, 
          site_data_df_full_names.plot.name$Site)

table(site_data_df_full_names.plot.name$worm_species)

View(site_data_df_full_names.plot.name)
write.csv(site_data_df_full_names.plot.name, file = "site_data_df_full_names.plot.name.csv")

#dredging the model
library(MuMIn)
options(na.action = "na.fail")
model.dredge <- dredge(model)

plot(y = model.dredge[1:30,]$AICc, x = model.dredge[1:30,]$df)
points(y = model.dredge[1,]$AICc, x = model.dredge[1,]$df, col = "red")
points(y = model.dredge[28,]$AICc, x = model.dredge[28,]$df, col = "red")

which(model.dredge[model.dredge$AICc < 50.23907 + 2])
which(model.dredge[], model.dredge$AICc < 50.23907 + 2)

model.dredge[model.dredge$AICc < (50.23907 + 2) & model.dredge$df == 6]

model.dredge.choice <- glm(Detected.European ~ drainage + Crown.Density + shannon_diversity.quadrat + 
                                      shannon_diversity.tree,
                                    data = site_data_df_full_names.plot.name.no.na, 
                                    family = binomial(link = "logit"))# control = glmerControl(optimizer = "nlminb", optCtrl = list(maxeval = 10000))#


summary(model.dredge.choice)


model.dredge.choice.2 <- glm(Detected.European ~ shannon_diversity.quadrat,
                           data = site_data_df_full_names.plot.name.no.na, 
                           family = binomial(link = "logit"))# control = glmerControl(optimizer = "nlminb", optCtrl = list(maxeval = 10000))#

summary(model.dredge.choice.2)





