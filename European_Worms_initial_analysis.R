library(tidyverse)
library(ggplot2)
library(dplyr)

#dev.off()

#reading in the data

soil_chem_df <- read.csv("Site_Data - Soil Chemistry Data.csv") #soil data
site_data_df <- read.csv("Site_Data - Master Data - genetic.csv") #Master data
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
        axis.title = element_text(family = "DIN Alternate", size = 12),
        axis.text = element_text(size = 12),
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





#Mann-Whitney U Tests



#soil ph
soil.ph <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = soil_pH)) +
  geom_boxplot() +
  labs(x = "",y = "Soil pH") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(soil_pH ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(soil_pH[European.Worms == "Y"]) - mean(soil_pH[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$soil_pH[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$soil_pH[shuffled == "N"])
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



#buffer pH
buffer.ph <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = buffer_pH)) +
  geom_boxplot() +
  labs(x = "",y = "Buffer pH") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(buffer_pH ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(buffer_pH[European.Worms == "Y"]) - mean(buffer_pH[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$buffer_pH[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$buffer_pH[shuffled == "N"])
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



#organic matter

om.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = OM....)) +
  geom_boxplot() +
  labs(x = "", y = "OM (%)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(OM.... ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(OM....[European.Worms == "Y"]) - mean(OM....[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$OM....[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$OM....[shuffled == "N"])
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



#NO3.N..mg.kg.

no3.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = NO3.N..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Nitrate (mg/kg)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(NO3.N..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(NO3.N..mg.kg.[European.Worms == "Y"], na.rm = T) - mean(NO3.N..mg.kg.[European.Worms == "N"], na.rm = T))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$NO3.N..mg.kg.[shuffled == "Y"], na.rm = T) - mean(site_data_df_full_names_euro.chem$NO3.N..mg.kg.[shuffled == "N"], na.rm = T)
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


#NH4.N..mg.kg.

nh4.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = NH4.N..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Ammonium (mg/kg)") + #x = "Invasive Earthworms?") 
  interactive_theme

#test
wilcox.test(NH4.N..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(NH4.N..mg.kg.[European.Worms == "Y"]) - mean(NH4.N..mg.kg.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$NH4.N..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$NH4.N..mg.kg.[shuffled == "N"])
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



#P..lb.A.

phos.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = P..lb.A.)) +
  geom_boxplot() +
  labs(x = "", y = "Phosphorus (lb/A)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(P..lb.A. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(P..lb.A.[European.Worms == "Y"]) - mean(P..lb.A.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$P..lb.A.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$P..lb.A.[shuffled == "N"])
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



#K..lb.A.

pot.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = K..lb.A.)) +
  geom_boxplot() +
  labs(x = "", y = "Potassium (lb/A)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(K..lb.A. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(K..lb.A.[European.Worms == "Y"]) - mean(K..lb.A.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$K..lb.A.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$K..lb.A.[shuffled == "N"])
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



#Mg..lb.A

mag.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Mg..lb.A)) +
  geom_boxplot() +
  labs(x = "", y = "Magnessium (lb/A)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Mg..lb.A ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Mg..lb.A[European.Worms == "Y"]) - mean(Mg..lb.A[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Mg..lb.A[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Mg..lb.A[shuffled == "N"])
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



#Ca..lb.A

calc.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Ca..lb.A)) +
  geom_boxplot() +
  labs(x = "", y = "Calcium (lb/A)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Ca..lb.A ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Ca..lb.A[European.Worms == "Y"]) - mean(Ca..lb.A[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Ca..lb.A[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Ca..lb.A[shuffled == "N"])
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



#Al..mg.kg.

all.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Al..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Aluminum (mg/kg)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Al..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Al..mg.kg.[European.Worms == "Y"]) - mean(Al..mg.kg.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Al..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Al..mg.kg.[shuffled == "N"])
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



#B..mg.kg.

boron.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = B..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Boron (mg/kg)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(B..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(B..mg.kg.[European.Worms == "Y"]) - mean(B..mg.kg.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$B..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$B..mg.kg.[shuffled == "N"])
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



#Cu..mg.kg.

copper.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Cu..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Copper (mg/kg)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Cu..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Cu..mg.kg.[European.Worms == "Y"]) - mean(Cu..mg.kg.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Cu..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Cu..mg.kg.[shuffled == "N"])
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



#Fe..mg.kg.

iron.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Fe..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Iron (mg/kg)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Fe..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Fe..mg.kg.[European.Worms == "Y"]) - mean(Fe..mg.kg.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Fe..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Fe..mg.kg.[shuffled == "N"])
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



#Mn..mg.kg.

magn.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Mn..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Manganese (mg/kg)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Mn..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Mn..mg.kg.[European.Worms == "Y"]) - mean(Mn..mg.kg.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Mn..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Mn..mg.kg.[shuffled == "N"])
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



#Na..mg.kg.

sodium.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Na..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Sodium (mg/kg)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Na..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Na..mg.kg.[European.Worms == "Y"]) - mean(Na..mg.kg.[European.Worms == "N"]))

# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Na..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Na..mg.kg.[shuffled == "N"])
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))

# Output
hist(perm_diffs, main="Permutation Null Distribution", xlab="Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)
cat("Observed difference:", obs_diff, "\n")
cat("Permutation p-value:", p_val, "\n")



#S..mg.kg.

sulfur.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = S..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Sulfur (mg/kg)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(S..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(S..mg.kg.[European.Worms == "Y"]) - mean(S..mg.kg.[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$S..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$S..mg.kg.[shuffled == "N"])
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



#Zn..mg.kg.

zinc.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Zn..mg.kg.)) +
  geom_boxplot() +
  labs(x = "", y = "Zinc (mg/kg)") +#x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Zn..mg.kg. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Zn..mg.kg.[European.Worms == "Y"]) - mean(Zn..mg.kg.[European.Worms == "N"]))

# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Zn..mg.kg.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Zn..mg.kg.[shuffled == "N"])
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



#ECEC..me.100g.

ecec.boxplot <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = ECEC..me.100g.)) +
  geom_boxplot() +
  labs(x = "", y = "ECEC (me/100g)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(ECEC..me.100g. ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(ECEC..me.100g.[European.Worms == "Y"]) - mean(ECEC..me.100g.[European.Worms == "N"]))
obs_diff

# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$ECEC..me.100g.[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$ECEC..me.100g.[shuffled == "N"])
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



#K....

potas.percent <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = K....)) +
  geom_boxplot() +
  labs(x = "", y = "Potassium (%)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(K.... ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(K....[European.Worms == "Y"]) - mean(K....[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$K....[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$K....[shuffled == "N"])
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



#Mg....

mag.perc <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Mg....)) +
  geom_boxplot() +
  labs(x = "", y = "Magnesium (%)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Mg.... ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Mg....[European.Worms == "Y"]) - mean(Mg....[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Mg....[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Mg....[shuffled == "N"])
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




#Ca....

calc.perc <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Ca....)) +
  geom_boxplot() +
  labs(x = "", y = "Calcium (%)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Ca.... ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Ca....[European.Worms == "Y"]) - mean(Ca....[European.Worms == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Ca....[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Ca....[shuffled == "N"])
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


#Acidity....

acidity.perc <- ggplot(data = site_data_df_full_names_euro.chem, aes(x = European.Worms, y = Acidity....)) +
  geom_boxplot() +
  labs(x = "", y = "Acidity (%)") + #x = "Invasive Earthworms?"
  interactive_theme

#test
wilcox.test(Acidity.... ~ European.Worms, data=site_data_df_full_names_euro.chem, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties

# Observed difference in means
obs_diff <- with(site_data_df_full_names_euro.chem, mean(Acidity....[European.Worms == "Y"]) - mean(Acidity....[European.Worms == "N"]))

# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_euro.chem$European.Worms)
  mean(site_data_df_full_names_euro.chem$Acidity....[shuffled == "Y"]) - mean(site_data_df_full_names_euro.chem$Acidity....[shuffled == "N"])
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


library(grid)
# Create custom text grobs for top and bottom
top_text <- textGrob("Distribution of Soil Chemistry Values by Earthworm Detection", gp = gpar(fontsize = 18, fontfamily = "Georgia", fontface = "bold"))
bottom_text <- textGrob("Invasive Earthworms?", gp = gpar(fontsize = 15, fontfamily = "DIN Alternate"))


grid.arrange(soil.ph,buffer.ph, om.boxplot, zinc.boxplot, 
             iron.boxplot, phos.boxplot, 
             mag.boxplot, calc.boxplot, all.boxplot, boron.boxplot, 
             copper.boxplot, magn.boxplot, pot.boxplot, no3.boxplot, 
             nh4.boxplot, sodium.boxplot, 
             sulfur.boxplot, ecec.boxplot, potas.percent, 
             mag.perc, calc.perc, acidity.perc, 
             bottom = bottom_text, 
             top = top_text)


# mean soil chemistry values by area
site_data_df_full_names_euro.chem.table <- site_data_df_full_names_euro.chem %>%
  group_by(European.Worms) %>%
  summarise(n = n(),
    
            mean.soil.ph = mean(soil_pH),
            sd.soil_pH= sd(soil_pH, na.rm = TRUE),
            ci95.low.soil.ph = mean.soil.ph - 1.96 * sd.soil_pH / sqrt(n),
            ci95.high.soil.ph = mean.soil.ph + 1.96 * sd.soil_pH / sqrt(n),
            
            mean.buffer_pH = mean(buffer_pH),
            sd.buffer_pH = sd(buffer_pH, na.rm = TRUE),
            ci95.low.buffer_pH= mean.buffer_pH - 1.96 * sd.buffer_pH / sqrt(n),
            ci95.high.buffer_pH = mean.buffer_pH + 1.96 * sd.buffer_pH / sqrt(n),
            
            mean.OM.... = mean(OM....),
            sd.OM.... = sd(OM...., na.rm = TRUE),
            ci95.low.OM.... = mean.OM.... - 1.96 * sd.OM.... / sqrt(n),
            ci95.high.OM.... = mean.OM.... + 1.96 * sd.OM.... / sqrt(n),
            
            mean.Zn..mg.kg. = mean(Zn..mg.kg.),
            sd.Zn..mg.kg. = sd(Zn..mg.kg., na.rm = TRUE),
            ci95.low.Zn..mg.kg. = mean.Zn..mg.kg. - 1.96 * sd.Zn..mg.kg. / sqrt(n),
            ci95.high.Zn..mg.kg. = mean.Zn..mg.kg. + 1.96 * sd.Zn..mg.kg. / sqrt(n),
            
            mean.Fe..mg.kg. = mean(Fe..mg.kg.),
            sd.Fe..mg.kg. = sd(Fe..mg.kg., na.rm = TRUE),
            ci95.Fe..mg.kg. = mean.Fe..mg.kg. - 1.96 * sd.Fe..mg.kg. / sqrt(n),
            ci95.high.Fe..mg.kg. = mean.Fe..mg.kg. + 1.96 * sd.Fe..mg.kg. / sqrt(n),
            
            mean.P..lb.A. = mean(P..lb.A.),
            sd.P..lb.A. = sd(P..lb.A., na.rm = TRUE),
            ci95.low.P..lb.A. = mean.P..lb.A. - 1.96 * sd.P..lb.A. / sqrt(n),
            ci95.high.P..lb.A. = mean.P..lb.A. + 1.96 * sd.P..lb.A. / sqrt(n),
            
            mean.Mg..lb.A = mean(Mg..lb.A),
            sd.Mg..lb.A = sd(Mg..lb.A, na.rm = TRUE),
            ci95.low.Mg..lb.A = mean.Mg..lb.A - 1.96 * sd.Mg..lb.A / sqrt(n),
            ci95.high.Mg..lb.A = mean.Mg..lb.A + 1.96 * sd.Mg..lb.A / sqrt(n),
            
            mean.Ca..lb.A = mean(Ca..lb.A),
            sd.Ca..lb.A = sd(Ca..lb.A, na.rm = TRUE),
            ci95.low.Ca..lb.A = mean.Ca..lb.A - 1.96 * sd.Ca..lb.A / sqrt(n),
            ci95.high.Ca..lb.A = mean.Ca..lb.A + 1.96 * sd.Ca..lb.A / sqrt(n),
            
            mean.Al..mg.kg.= mean(Al..mg.kg.),
            sd.Al..mg.kg. = sd(Al..mg.kg., na.rm = TRUE),
            ci95.low.Al..mg.kg. = mean.Al..mg.kg. - 1.96 * sd.Al..mg.kg. / sqrt(n),
            ci95.high.Al..mg.kg. = mean.Al..mg.kg. + 1.96 * sd.Al..mg.kg. / sqrt(n),
            
            mean.B..mg.kg. = mean(B..mg.kg.),
            sd.B..mg.kg. = sd(B..mg.kg., na.rm = TRUE),
            ci95.low.B..mg.kg. = mean.B..mg.kg. - 1.96 * sd.B..mg.kg. / sqrt(n),
            ci95.high.B..mg.kg. = mean.B..mg.kg. + 1.96 * sd.B..mg.kg. / sqrt(n),
            
            mean.Cu..mg.kg. = mean(Cu..mg.kg.),
            sd.Cu..mg.kg. = sd(Cu..mg.kg., na.rm = TRUE),
            ci95.low.Cu..mg.kg. = mean.Cu..mg.kg. - 1.96 * sd.Cu..mg.kg. / sqrt(n),
            ci95.high.Cu..mg.kg. = mean.Cu..mg.kg. + 1.96 * sd.Cu..mg.kg. / sqrt(n),
            
            mean.Mn..mg.kg. = mean(Mn..mg.kg.),
            sd.Mn..mg.kg. = sd(Mn..mg.kg., na.rm = TRUE),
            ci95.low.Mn..mg.kg. = mean.Mn..mg.kg. - 1.96 * sd.Mn..mg.kg. / sqrt(n),
            ci95.high.Mn..mg.kg. = mean.Mn..mg.kg. + 1.96 * sd.Mn..mg.kg. / sqrt(n),
            
            mean.K..lb.A. = mean(K..lb.A.),
            sd.K..lb.A. = sd(K..lb.A., na.rm = TRUE),
            ci95.low.K..lb.A. = mean.K..lb.A. - 1.96 * sd.K..lb.A. / sqrt(n),
            ci95.high.K..lb.A. = mean.K..lb.A. + 1.96 * sd.K..lb.A. / sqrt(n),
            
            mean.NO3.N..mg.kg. = mean(NO3.N..mg.kg., na.rm = TRUE),
            sd.NO3.N..mg.kg. = sd(NO3.N..mg.kg., na.rm = TRUE),
            ci95.low.NO3.N..mg.kg. = mean.NO3.N..mg.kg. - 1.96 * sd.NO3.N..mg.kg. / sqrt(n),
            ci95.high.NO3.N..mg.kg. = mean.NO3.N..mg.kg. + 1.96 * sd.NO3.N..mg.kg. / sqrt(n),
            
            mean.NH4.N..mg.kg. = mean(NH4.N..mg.kg.),
            sd.NH4.N..mg.kg. = sd(NH4.N..mg.kg., na.rm = TRUE),
            ci95.low.NH4.N..mg.kg. = mean.NH4.N..mg.kg. - 1.96 * sd.NH4.N..mg.kg. / sqrt(n),
            ci95.high.NH4.N..mg.kg. = mean.NH4.N..mg.kg. + 1.96 * sd.NH4.N..mg.kg. / sqrt(n),
            
            mean.Na..mg.kg. = mean(Na..mg.kg.),  
            sd.Na..mg.kg. = sd(Na..mg.kg., na.rm = TRUE),
            ci95.low.Na..mg.kg. = mean.Na..mg.kg. - 1.96 * sd.Na..mg.kg. / sqrt(n),
            ci95.high.Na..mg.kg. = mean.Na..mg.kg. + 1.96 * sd.Na..mg.kg. / sqrt(n),
            
            mean.S..mg.kg. = mean(S..mg.kg.),
            sd.S..mg.kg. = sd(S..mg.kg., na.rm = TRUE),
            ci95.low.S..mg.kg. = mean.S..mg.kg. - 1.96 * sd.S..mg.kg. / sqrt(n),
            ci95.high.S..mg.kg. = mean.S..mg.kg. + 1.96 * sd.S..mg.kg. / sqrt(n),
            
            mean.ECEC..me.100g. = mean(ECEC..me.100g.), 
            sd.ECEC..me.100g. = sd(ECEC..me.100g., na.rm = TRUE),
            ci95.low.ECEC..me.100g. = mean.ECEC..me.100g. - 1.96 * sd.ECEC..me.100g. / sqrt(n),
            ci95.high.ECEC..me.100g. = mean.ECEC..me.100g. + 1.96 * sd.ECEC..me.100g. / sqrt(n),
            
            mean.K.... = mean(K....),
            sd.K.... = sd(K...., na.rm = TRUE),
            ci95.low.K.... = mean.K.... - 1.96 * sd.K.... / sqrt(n),
            ci95.high.K.... = mean.K.... + 1.96 * sd.K.... / sqrt(n),
            
            mean.Mg.... = mean(Mg....),  
            sd.Mg.... = sd(Mg...., na.rm = TRUE),
            ci95.low.Mg.... = mean.Mg.... - 1.96 * sd.Mg.... / sqrt(n),
            ci95.high.Mg.... = mean.Mg.... + 1.96 * sd.Mg.... / sqrt(n),
            
            mean.Ca.... = mean(Ca....), 
            sd.Ca.... = sd(Ca...., na.rm = TRUE),
            ci95.low.Ca.... = mean.Ca.... - 1.96 * sd.Ca.... / sqrt(n),
            ci95.high.Ca.... = mean.Ca.... + 1.96 * sd.Ca.... / sqrt(n),
            
            mean.Acidity.... = mean(Acidity....),
            sd.Acidity.... = sd(Acidity...., na.rm = TRUE),
            ci95.low.Acidity.... = mean.Acidity.... - 1.96 * sd.Acidity.... / sqrt(n),
            ci95.high.Acidity.... = mean.Acidity.... + 1.96 * sd.Acidity.... / sqrt(n),
  )











## other soil characteristics


### soil drainage
soil.drainage.table <- site_data_df_full_names_nanopore %>%
  group_by(drainage, Detected.European) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Detected.European) %>%
  mutate(prop = count / sum(count),
         total = sum(count),
         se = sqrt(prop * (1 - prop) / total),
         ci95.low = prop - 1.96 * se,
         ci95.high = prop + 1.96 * se) 

site_data_df_full_names_nanopore %>%
  mutate(drainage = fct_relevel(drainage, c("Poor", "Moderate", "Well"))) %>%
  ggplot(aes(x = drainage)) +
  geom_bar(aes(fill = drainage), color = "#281E5D") +
  scale_fill_manual(values = c("#43270f", "#7C4700", "#997950")) +
  labs(x = "Soil Drainage",
       y = "Frequency",
       title = "Frequency of Soil Drainage Types",
       fill = "Soil Drainage") +
  interactive_theme +
  facet_grid(. ~ Detected.European)


#FIX
soil.drainage.table$ci95.high
soil.drainage.table %>%
  mutate(drainage = fct_relevel(drainage, c("Poor", "Moderate", "Well"))) %>%
  ggplot(aes(x = drainage)) +
  geom_bar(aes(y = prop, fill = drainage), stat = "identity", color = "#281E5D") +
  scale_fill_manual(values = c("#43270f", "#7C4700", "#997950")) +
  labs(x = "Soil Drainage",
       y = "Proportion",
       title = "Proportion of Soil Drainage Types by Earthworm Detection",
       fill = "Soil Drainage", 
       subtitle = "Error Bars Represent 95% Confidence Intervals") +
  interactive_theme + 
  theme(plot.title = element_text(size = 15)) +
  geom_errorbar(aes(ymin = ci95.low, ymax = ci95.high)) +
  facet_grid(. ~ Detected.European, labeller = labeller(Detected.European = c(
    "Y" = "European Worms: Detected",
    "N" = "European Worms: Not Detected"
  )))



mantelhaen.test(table(site_data_df_full_names_nanopore$drainage,
                      site_data_df_full_names_nanopore$Detected.European,
                      site_data_df_full_names_nanopore$Site))



# Observed proportions
tab <- table(site_data_df_full_names_nanopore$drainage, site_data_df_full_names_nanopore$Detected.European)

# Proportion of worm presence in each soil type
p_Moderate <- tab["Moderate", "Y"] / sum(tab["Moderate", ])
p_Poor <- tab["Poor", "Y"] / sum(tab["Poor", ])
p_Well <- tab["Well", "Y"] / sum(tab["Well", ])
obs_diff <- c(p_Moderate - p_Poor, p_Moderate - p_Well, p_Poor - p_Well)
obs_diff

# Permutation: shuffle soil labels, recalculate difference
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled_soil <- sample(site_data_df_full_names_nanopore$drainage)
  tab_perm <- table(shuffled_soil, site_data_df_full_names_nanopore$Detected.European)
  p_perm_moderate <- tab_perm["Moderate", "Y"] / sum(tab_perm["Moderate", ])
  p_perm_poor <- tab_perm["Poor", "Y"] / sum(tab_perm["Poor", ])
  p_perm_well <- tab_perm["Well", "Y"] / sum(tab_perm["Well", ])
  c(p_perm_moderate - p_perm_poor, p_perm_moderate - p_perm_well, p_perm_poor - p_perm_well)
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Plot results
hist(perm_diffs, main = "Permutation Test: Worm Detection by Soil Type",
     xlab = "Difference in Proportion (Hydric - Mesic)", breaks = 30)
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)

# Output
cat("Observed difference in worm detection:", round(obs_diff, 3), "\n")
cat("Permutation p-value:", round(p_val, 4), "\n")


#soil moisture

soil.moisture.table <- site_data_df_full_names_nanopore %>%
  group_by(soil.moisture, Detected.European) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Detected.European) %>%
  mutate(prop = count / sum(count),
         total = sum(count),
         se = sqrt(prop * (1 - prop) / total),
         ci95.low = prop - 1.96 * se,
         ci95.high = prop + 1.96 * se) 

ggplot(data = site_data_df_full_names_nanopore, aes(x = soil.moisture)) +
  geom_bar(aes(fill = soil.moisture), color = "#281E5D") +
  scale_fill_manual(values = c("#0492C2", "#ADD8E6")) +
  labs(x = "Soil Moisture",
       y = "Frequency",
       title = "Frequency of Soil Moisture Types",
       fill = "Soil Moisture") +
  interactive_theme + 
  facet_grid(. ~ Detected.European)

ggplot(data = soil.moisture.table, aes(x = soil.moisture)) +
  geom_bar(aes(y = prop, fill = soil.moisture), stat = "identity", color = "#281E5D") +
  scale_fill_manual(values = c("#0492C2", "#ADD8E6")) +
  labs(x = "Soil Moisture",
       y = "Proportion",
       title = "Proportion of Soil Moisture Types by Earthworm Detection",
       fill = "Soil Moisture",
       subtitle = "Error Bars Represent 95% Confidence Intervals") +
  interactive_theme + 
  geom_errorbar(aes(ymin = ci95.low, ymax = ci95.high)) +
  theme(plot.title = element_text(size = 15)) +
  facet_grid(. ~ Detected.European, labeller = labeller(Detected.European = c(
    "Y" = "European Worms: Detected",
    "N" = "European Worms: Not Detected"
  )))





# Observed proportions
tab <- table(site_data_df_full_names_nanopore$soil.moisture, site_data_df_full_names_nanopore$Detected.European)

# Proportion of worm presence in each soil type
p_hydric <- tab["Hydric", "Y"] / sum(tab["Hydric", ])
p_mesic <- tab["Mesic", "Y"] / sum(tab["Mesic", ])
obs_diff <- p_hydric - p_mesic
obs_diff

# Permutation: shuffle soil labels, recalculate difference
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled_soil <- sample(site_data_df_full_names_nanopore$soil.moisture)
  tab_perm <- table(shuffled_soil, site_data_df_full_names_nanopore$Detected.European)
  p_perm_hydric <- tab_perm["Hydric", "Y"] / sum(tab_perm["Hydric", ])
  p_perm_mesic <- tab_perm["Mesic", "Y"] / sum(tab_perm["Mesic", ])
  p_perm_hydric - p_perm_mesic
})

# Two-sided p-value
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
p_val

# Plot results
hist(perm_diffs, main = "Permutation Test: Worm Detection by Soil Type",
     xlab = "Difference in Proportion (Hydric - Mesic)", breaks = 30)
abline(v = obs_diff, col = "red", lwd = 2)
abline(v = -obs_diff, col = "red", lwd = 2)

# Output
cat("Observed difference in worm detection:", round(obs_diff, 3), "\n")
cat("Permutation p-value:", round(p_val, 4), "\n")


#comparing
mantelhaen.test(table(site_data_df_full_names_nanopore$soil.moisture,
                      site_data_df_full_names_nanopore$Detected.European,
                      site_data_df_full_names_nanopore$Site))


# leaf litter

leaf.litter <- ggplot(data = site_data_df_full_names_nanopore, aes(x = Detected.European, y = avg.litter.cover)) +
  geom_boxplot() +
  labs(x = "", y = "Average Litter Cover (%)") + #Invasive Earthworms?
  interactive_theme

#test
wilcox.test(avg.litter.cover ~ Detected.European, data=site_data_df_full_names_nanopore, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties



# Observed difference in means
obs_diff <- with(site_data_df_full_names_nanopore, mean(avg.litter.cover[Detected.European == "Y"]) - mean(avg.litter.cover[Detected.European == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_nanopore$Detected.European)
  mean(site_data_df_full_names_nanopore$avg.litter.cover[shuffled == "Y"]) - mean(site_data_df_full_names_nanopore$avg.litter.cover[shuffled == "N"])
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


#litter depth

litter.depth <- ggplot(data = site_data_df_full_names_nanopore, aes(x = Detected.European, y = litter.depth..cm.)) +
  geom_boxplot() +
  labs(x = "", y = "Litter Depth (cm)") + #Invasive Earthworms?
  interactive_theme

#test
wilcox.test(litter.depth..cm. ~ Detected.European, data=site_data_df_full_names_nanopore, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties


# Observed difference in means
obs_diff <- with(site_data_df_full_names_nanopore, mean(litter.depth..cm.[Detected.European == "Y"]) - mean(litter.depth..cm.[Detected.European == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_nanopore$Detected.European)
  mean(site_data_df_full_names_nanopore$litter.depth..cm.[shuffled == "Y"]) - mean(site_data_df_full_names_nanopore$litter.depth..cm.[shuffled == "N"])
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



#herb cover

herb.cover <- ggplot(data = site_data_df_full_names_nanopore, aes(x = Detected.European, y = Herbaceous.Layer...Cover)) +
  geom_boxplot() +
  labs(x = "", y = "Herbaceous Layer Cover (%)") + #Invasive Earthworms?
  interactive_theme

#test
wilcox.test(Herbaceous.Layer...Cover ~ Detected.European, data=site_data_df_full_names_nanopore, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties



# Observed difference in means
obs_diff <- with(site_data_df_full_names_nanopore, mean(Herbaceous.Layer...Cover[Detected.European == "Y"]) - mean(Herbaceous.Layer...Cover[Detected.European == "N"]))


# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_nanopore$Detected.European)
  mean(site_data_df_full_names_nanopore$Herbaceous.Layer...Cover[shuffled == "Y"]) - mean(site_data_df_full_names_nanopore$Herbaceous.Layer...Cover[shuffled == "N"])
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


# temp
ggplot(data = site_data_df_full_names_nanopore, aes(x = Detected.European, y = Temp.C)) +
  geom_boxplot() +
  labs(x = "", y = "Temperature (ºC)") + #
  interactive_theme

#test
wilcox.test(Temp.C ~ Detected.European, data=site_data_df_full_names_nanopore, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties



#crown density
crown.density <- ggplot(data = site_data_df_full_names_nanopore, aes(x = Detected.European, y = Crown.Density)) +
  geom_boxplot() +
  labs(x = "", y = "Crown Density (%)") + #Invasive Earthworms?
  interactive_theme

#test
wilcox.test(Crown.Density ~ Detected.European, data=site_data_df_full_names_nanopore, correct=FALSE, exact = F)  #have to use the correct = 5 bc data was below 50 and there are ties



# Observed difference in means
obs_diff <- with(site_data_df_full_names_nanopore, mean(Crown.Density[Detected.European == "Y"]) - mean(Crown.Density[Detected.European == "N"]))
# Permutation test
set.seed(123)
n_perm <- 10000
perm_diffs <- replicate(n_perm, {
  shuffled <- sample(site_data_df_full_names_nanopore$Detected.European)
  mean(site_data_df_full_names_nanopore$Crown.Density[shuffled == "Y"]) - mean(site_data_df_full_names_nanopore$Crown.Density[shuffled == "N"])
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
top_text <- textGrob("Distribution of Litter and Vegetation Coverage by Earthworm Detection", gp = gpar(fontsize = 14, fontfamily = "Georgia", fontface = "bold"))
bottom_text <- textGrob("Invasive Earthworms?", gp = gpar(fontsize = 13, fontfamily = "DIN Alternate"))

grid.arrange(leaf.litter, litter.depth, herb.cover, 
             crown.density, bottom = bottom_text, 
             top = top_text)



## Looking at plants


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

trees_df_full_names_w_invasives <- trees_df_full_names %>% #making a data frame with the tree counts and the invasive worm information
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Detected.European), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Lumbricus.rubellus.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Lumbricus.terrestris.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Aporrectodea.caliginosa.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Contamination), by = c("Site", "Plot"))

tree_counts <- summarize(group_by(trees_df_full_names_w_invasives, Species), count = n(), prop = count/nrow(trees_df_full_names_w_invasives))

tree.Species.table <- trees_df_full_names_w_invasives %>%
  group_by(Species, Detected.European) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Detected.European) %>%
  mutate(prop = count / sum(count))

ggplot(data = tree_counts, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Tree Species",
       y = "Frequency",
       title = "Tree Species Counts Across Plots") +
  interactive_theme +
  coord_flip()


tree.Species.table.top.5.count <- top_n(tree.Species.table, 5, count)
ggplot(data = tree.Species.table.top.5.count, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Tree Species",
       y = "Frequency",
       title = "Tree Species Counts Across Plots by Invasive Species Detection") +
  interactive_theme +
  coord_flip() +
  facet_grid(Detected.European ~ .)

tree.Species.table.top.5.prop <- top_n(tree.Species.table, 5, prop)
ggplot(data = tree.Species.table.top.5.prop, aes(x = reorder(Species, +prop), y = prop)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Tree Species",
       y = "Proportion",
       title = "Tree Species Proportions Across Plots by Earthworm Detection") +
  interactive_theme +
  theme(plot.title = element_text(size = 15)) +
  coord_flip() +
  facet_grid(Detected.European ~ ., labeller = labeller(Detected.European = c(
    "Y" = "European Worms: Detected",
    "N" = "European Worms: Not Detected",
    "NA" = "DNA Not Sequenced"
  ))) 
  
#theme(axis.text.x = element_text(angle = 45, hjust = 1))


#making a data frame with the tree counts and the invasive worm information
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


sapling.Species.table <- saplings_df_full_names_w_invasives %>%
  group_by(Species, Detected.European) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Detected.European) %>%
  mutate(prop = count / sum(count))


ggplot(data = sapling_counts, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Sapling Species",
       y = "Frequency",
       title = "Sapling Species Counts Across Plots") +
  interactive_theme +
  coord_flip()
#theme(axis.text.x = element_text(angle = 45, hjust = 1))


sapling.Species.table.top.5.prop <- top_n(sapling.Species.table, 5, prop)
ggplot(data = sapling.Species.table.top.5.prop, aes(x = reorder(Species, +prop), y = prop)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Sapling Species",
       y = "Proportion",
       title = "Sapling Species Proportions Across Plots by Earthworm Detection") +
  interactive_theme +
  theme(plot.title = element_text(size = 15)) +
  coord_flip() +
  facet_grid(Detected.European ~ ., labeller = labeller(Detected.European = c(
    "Y" = "European Worms: Detected",
    "N" = "European Worms: Not Detected",
    "NA" = "DNA Not Sequenced"
  ))) 


##herbaceous plants
herb_counts <- summarize(group_by(quadrat_df_full_names, Species), count = sum(Count), prop = count/nrow(quadrat_df_full_names))



quadrat_df_full_names_w_invasives <- quadrat_df_full_names %>% #making a data frame with the tree counts and the invasive worm information
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Detected.European), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Lumbricus.rubellus.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Lumbricus.terrestris.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Aporrectodea.caliginosa.), by = c("Site", "Plot")) %>%
  left_join(site_data_df_full_names_nanopore %>% dplyr::select(Site, Plot, Contamination), by = c("Site", "Plot"))

#removing plots where herb data was collected in the fall
quadrat_df_full_names_w_invasives <- quadrat_df_full_names_w_invasives %>%
  filter(!(quadrat_df_full_names_w_invasives$Date == "10/26/24"),
         !(quadrat_df_full_names_w_invasives$Date == "11/22/24"))

quadrat.Species.table <- quadrat_df_full_names_w_invasives %>%
  group_by(Species, Detected.European) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Detected.European) %>%
  mutate(prop = count / sum(count))

herb_counts <- top_n(herb_counts, 10, count) #reording to get top 10
ggplot(data = herb_counts, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Herb Species",
       y = "Frequency",
       title = "Herb Species Counts Across Plots") +
  interactive_theme +
  coord_flip()

#theme(axis.text.x = element_text(angle = 45, hjust = 1))

quadrat.Species.table.top.5.prop <- top_n(quadrat.Species.table, 5, prop)
ggplot(data = quadrat.Species.table.top.5.prop, aes(x = reorder(Species, +prop), y = prop)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Herb Species",
       y = "Proportion",
       title = "Herb Species Proportions Across Plots by Earthworm Detection") +
  interactive_theme +
  theme(plot.title = element_text(size = 15)) +
  coord_flip() +
  facet_grid(Detected.European ~ ., labeller = labeller(Detected.European = c(
    "Y" = "European Worms: Detected",
    "N" = "European Worms: Not Detected",
    "NA" = "DNA Not Sequenced"
  ))) 


##invasive plants

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


ggplot(data = invasives.Species.table, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Non-Native Species",
       y = "Frequency",
       title = "Non-Native Species Counts Across Plots") +
  interactive_theme +
  coord_flip()

ggplot(data = invasives.Species.table, aes(x = reorder(Species, +count), y = count)) +
  geom_col(fill = "#7C4700") +
  labs(x = "Non-Native Species",
       y = "Frequency",
       title = "Non-Native Species Counts Across Plots") +
  interactive_theme +
  coord_flip()+
  facet_grid(Detected.European ~ ., labeller = labeller(Detected.European = c(
    "Y" = "European Worms: Detected",
    "N" = "European Worms: Not Detected",
    "NA" = "DNA Not Sequenced"
  ))) 

ggplot(data = invasives.Species.table, aes(x = reorder(Species, +prop), y = prop)) +
  geom_col(fill = "#7C4700") +
  geom_col(fill = "#7C4700") +
  labs(x = "Non-Native Species",
       y = "Frequency",
       title = "Non-Native Species Proportion Across Plots by Earthworm Detection") +
  interactive_theme +
  theme(plot.title = element_text(size = 15)) +
  coord_flip() +
  facet_grid(Detected.European ~ ., labeller = labeller(Detected.European = c(
    "Y" = "European Worms: Detected",
    "N" = "European Worms: Not Detected",
    "NA" = "DNA Not Sequenced"
  ))) 


# NMDS

library(vegan)

#full data: site_data_df_full_names
#nanopore data: site_data_df_full_names_nanopore

site_data_df_full_names.nmds <- site_data_df_full_names[-c(1, 2, 6,7,8, 9, 10, 11, 12, 17, 19, 28, 29, 30, 31, 32, 34, 35, 36, 37,
                                                           38, 39, 40, 41, 42, 43, 44, 45, 51, 60)]

nmds_input <- site_data_df_full_names.nmds %>%
  dplyr::select(where(is.numeric)) %>%                                 # Keep only numeric columns
  mutate(across(everything(), ~ ifelse(is.na(.) | . < 0, 0, .)))       # Replace NA and negatives with 0
  


data.matrix <- as.matrix(nmds_input)
rownames(data.matrix) <- c(site_data_df_full_names$Plot_Full_Name)
rownames(data.matrix) <- c("Colby 1", "Little Moose 4", "Little Moose 1", "Little Moose 2",
                           "Little Moose 3", "Mahoosuc 2", "Mahoosuc 1", "Mahoosuc 3", "Mahoosuc 4",
                           "Bigelow 4", "Bigelow 2", "Bigelow 3", "Bigelow 1", "Colby 2",
                           "Bradbury 1", "Bradbury 2", "Bradbury 3", "Bradbury 4", "Kennebec Highlands 1",
                           "Kennebec Highlands 3", "Kennebec Highlands 2", "Kennebec Highlands 4",
                           "Androscoggin Riverlands 1", "Androscoggin Riverlands 3", "Androscoggin Riverlands 4",
                           "Androscoggin Rverlands 2", "Bradley 1", "Bradley 2", "Sebago Lake 3", 
                           "Sebago Lake 1", "Sebago Lake 2", "Colby Hume 1", "Colby 3", "Falmouth Land Trust 1", 
                           "Falmouth Land Trust 3", "Falmouth Land Trust 2", "Colby 5", "Colby 4", "Colby 6", "Colby 7",
                           "Viles Aboretum 3", "Viles Aboretum 2", "Viles Aboretum 1", "Colby 9", "Colby 8",
                           "Seboeis Lake 3", "Seboeis Lake 1", "Seboeis Lake 2", "Kennebec Pubic Land Trust 1",
                           "Kennebec Pubic Land Trust 2", "Kennebec Pubic Land Trust 3", "Scopan Lake 1", "Scopan Lake 2",
                           "Coastal Maine Botanical Gardens 2", "Coastal Maine Botanical Gardens 1", "Coastal Maine Botanical Gardens 3")
data.matrix <- scale(data.matrix)
                      
set.seed(123)
#i use euclidean because the data are very different temp, moss cover, etc. 
nmds = metaMDS(data.matrix, distance = "euclidean", autotransform = FALSE, na.rm = T)
nmds

# Shepards test/goodness of fit
goodness(nmds) # Produces a results of test statistics for goodness of fit for each point
stressplot(nmds)

plot(nmds, type = 't')
with(site_data_df_full_names.nmds, points(nmds, display = 'Site'))



# Assuming you already have your NMDS result stored in `nmds`

# Step 1: Extract site scores (coordinates for each site)
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Add your plot/site names back in
nmds_scores$Site <- rownames(nmds_scores)

# Optional: if you have groupings (like habitat type, region, etc.), you can add them here
# For now, just the Site names.

# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, label = Site)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, hjust = 0.5, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Site Environmental Characteristics",
       x = "NMDS1",
       y = "NMDS2") +
  theme(plot.title = element_text(hjust = 0.5))

# Assuming you have your NMDS object `nmds`
# And your original environmental matrix `nmds_input`

envfit_results <- envfit(nmds, nmds_input, permutations = 999)

# See the results:
print(envfit_results)

# Plot arrows for environmental variables:
plot(nmds, display = "sites")
plot(envfit_results, p.max = 0.05)  # only show variables with p < 0.05






## NMDS: Soil Chemistry Variables
soil_chem_df_euro

nmds_input <- site_data_df_full_names_euro.chem %>%
  dplyr::select(where(is.numeric)) %>%                                 # Keep only numeric columns
  mutate(across(everything(), ~ ifelse(is.na(.) | . < 0, 0, .)))       # Replace NA and negatives with 0


data.matrix <- as.matrix(nmds_input)
rownames(data.matrix) <- c(site_data_df_full_names$Plot_Full_Name)
rownames(data.matrix) <- c("Colby 1", "Colby 2", "Colby 3", "Colby 4", "Colby 5", "Colby 6", "Colby 7", "Colby 8",
                           "Colby 9", "Seboeis Lake 3", "Androscoggin Rverlands 2","Kennebec Highlands 1", 
                           "Kennebec Highlands 2", "Kennebec Highlands 4", "Kennebec Pubic Land Trust 1",
                           "Kennebec Pubic Land Trust 3","Viles Aboretum 1", "Viles Aboretum 2", "Viles Aboretum 3",
                           "Bigelow 1", "Bigelow 4",  "Bradbury 1", "Bradbury 2", "Bradley 2", "Falmouth Land Trust 1", 
                           "Falmouth Land Trust 3", "Colby Hume 1", "Little Moose 3", "Little Moose 4", "Mahoosuc 2",
                           "Sebago Lake 1", "Sebago Lake 2", "Scopan Lake 2","Coastal Maine Botanical Gardens 1", 
                           "Coastal Maine Botanical Gardens 2", "Coastal Maine Botanical Gardens 3")
data.matrix <- scale(data.matrix)

set.seed(123)
#i use euclidean because the data are very different temp, moss cover, etc. 
nmds = metaMDS(data.matrix, distance = "euclidean", autotransform = FALSE, na.rm = T)
nmds

# Shepards test/goodness of fit
goodness(nmds) # Produces a results of test statistics for goodness of fit for each point
stressplot(nmds)

plot(nmds, type = 't')
with(site_data_df_full_names.nmds, points(nmds, display = 'Site'))


# Assuming you already have your NMDS result stored in `nmds`

# Step 1: Extract site scores (coordinates for each site)
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Add your plot/site names back in
nmds_scores$Site <- rownames(nmds_scores)


# Optional: if you have groupings (like habitat type, region, etc.), you can add them here
# For now, just the Site names.
library(ggrepel)

# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, label = Site)) +
  geom_point(size = 3, aes(col = soil_chem_df$European.Worms)) +
  geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1.3, hjust = 0, size = 1.3, position = position_dodge(5)) +
  theme_minimal() +
  labs(title = "NMDS of All Plot Soil Chemistry Characteristics",
       x = "NMDS1",
       y = "NMDS2",
       label = "Plot", 
       color = "Whether Worms Detected") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values  = c("#000000", "#f03b20"))

site_data_df_full_names_euro.chem.worm <- site_data_df_full_names_euro.chem %>%
  mutate(worm_species = case_when(site_data_df_full_names_euro.chem$Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  site_data_df_full_names_euro.chem$Lumbricus.terrestris. == "Y" ~ "Lumbricus terrestris",
                                  site_data_df_full_names_euro.chem$Aporrectodea.caliginosa. == "Y" ~ "Aporrectodea caliginosa", 
                                  site_data_df_full_names_euro.chem$Aporrectodea.caliginosa. == "N" ~ "No Worms Detetected"))

# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(size = 3, aes(col = site_data_df_full_names_euro.chem.worm$worm_species)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text(vjust = -1.3, hjust = 0, size = 1.3, position = position_dodge(5)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Soil Chemistry Characteristics",
       x = "NMDS1",
       y = "NMDS2",
       label = "Plot", 
       color = "Worms Detected?") + #ffeda0","feb24c", "f03b20", "000000
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values  = c("#ffeda5","#feb24c", "#f03b20", "#000000"))


# Assuming you have your NMDS object `nmds`
# And your original environmental matrix `nmds_input`

envfit_results <- envfit(nmds, nmds_input, permutations = 999)

# See the results:
print(envfit_results)

# Plot arrows for environmental variables:
plot(nmds, display = "sites")
plot(envfit_results, p.max = 0.05)  # only show variables with p < 0.05



###NMDS Chemistry all sites

nmds_input <- soil_chem_df %>%
  dplyr::select(where(is.numeric)) %>%                                 # Keep only numeric columns
  mutate(across(everything(), ~ ifelse(is.na(.) | . < 0, 0, .)))       # Replace NA and negatives with 0
View(data.matrix)
data.matrix <- as.matrix(nmds_input)
rownames(data.matrix) <- c("Colby Arboretum 1", "Colby Arboretum 2", "Colby Arboretum 3", 
                           "Colby Arboretum 4", "Colby Arboretum 5", "Colby Arboretum 6", 
                           "Colby Arboretum 7", "Colby Arboretum 8",
                           "Colby Arboretum 9", "Seboeis Lake 1", "Seboeis Lake 2",
                           "Seboeis Lake 3", "Androscoggin Riverlands 1", "Androscoggin Riverlands 2",
                           "Androscoggin Riverlands 3", "Androscoggin Riverlands 4", "Kennebec Highlands 1", 
                           "Kennebec Highlands 2", "Kennebec Highlands 3", "Kennebec Highlands 4", "Kennebec Public Land Trust 1",
                           "Kennebec Public Land Trust 2", 
                           "Kennebec Public Land Trust 3","Viles Arboretum 1", "Viles Arboretum 2", "Viles Arboretum 3",
                           "Bigelow 1", "Bigelow 2", "Bigelow 3","Bigelow 4",  "Bradbury Mountain 1", "Bradbury Mountain 2", 
                           "Bradbury Mountain 3", "Bradbury Mountain 4",
                           "Bradley 1", "Bradley 2", "Falmouth Land Trust 1",  "Falmouth Land Trust 2", 
                           "Falmouth Land Trust 3", "Colby Hume 1", "Little Moose 1", "Little Moose 2", "Little Moose 3", 
                           "Little Moose 4", "Mahoosuc 1", "Mahoosuc 2", "Mahoosuc 3", "Mahoosuc 4",
                           "Sebago Lake 1", "Sebago Lake 2", "Sebago Lake 3", "Scopan Lake 1", "Scopan Lake 2",
                           "Coastal Botanical Gardens 1", 
                           "Coastal Botanical Gardens 2", "Coastal Botanical Gardens 3")
data.matrix <- scale(data.matrix)

set.seed(123)
#i use euclidean because the data are very different temp, moss cover, etc. 
nmds = metaMDS(data.matrix, distance = "euclidean", autotransform = FALSE, na.rm = T)
nmds

# Shepards test/goodness of fit
goodness(nmds) # Produces a results of test statistics for goodness of fit for each point
stressplot(nmds)

plot(nmds, type = 't')
with(site_data_df_full_names.nmds, points(nmds, display = 'Site'))



# Assuming you already have your NMDS result stored in `nmds`

# Step 1: Extract site scores (coordinates for each site)
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Add your plot/site names back in
nmds_scores$Site <- rownames(nmds_scores)

# Optional: if you have groupings (like habitat type, region, etc.), you can add them here
# For now, just the Site names.

soil_chem_df.worm <- soil_chem_df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))


# get worm presence   
detected_type <- soil_chem_df.worm %>% 
  distinct(Site, Plot, European.Worms) %>%
  mutate(Site = paste0(Site, " ", Plot))

# Extract NMDS scores for sites 
nmds_SiteScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)) %>% #nmds_scores$Site
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>%
  # join our habitat type (grouping variable) to each site 
  left_join(detected_type, by = "Site")

# Extract NMDS scores for species  
nmds_SpeciesScores <- as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
chemistry_Centroid <- 
  nmds_SiteScores %>% 
  group_by(European.Worms) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
chemistry.hull <- 
  nmds_SiteScores %>% 
  group_by(European.Worms) %>%
  slice(chull(NMDS1, NMDS2))



# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(data = chemistry_Centroid, 
             aes(x = axis1, y = axis2, color = European.Worms), 
             size = 5, shape = 17) +
  geom_label(aes(label = Site)) +
  # add convex hull
  geom_polygon(data = chemistry.hull, 
               aes(x = NMDS1, y = NMDS2, fill = European.Worms, group = European.Worms), 
               alpha = 0.30) +
  geom_point(size = 2, aes(col = soil_chem_df.worm$European.Worms)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1, hjust = -1, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Chemistry Characteristics by Earthworm Detection",
       x = "NMDS1",
       y = "NMDS2", 
       color = "Worms Detected?", 
       fill = "Worms Detected?") +
  annotate("text", x = Inf, y = -Inf, label = paste0("Stress = ", round(nmds$stress, 3)), 
           hjust = 1.1, vjust = -1.1, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  # add centroid 
  scale_color_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000")) +
  scale_fill_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000"))


# get worm presence   
worm_type <- soil_chem_df.worm %>% 
  distinct(Site, Plot, worm_species) %>%
  mutate(Site = paste0(Site, " ", Plot))

# Extract NMDS scores for sites 
nmds_SiteScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>%
  # join our habitat type (grouping variable) to each site 
  left_join(worm_type, by = "Site")

# Extract NMDS scores for species  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
worm_Centroid <- 
  nmds_SiteScores %>% 
  group_by(worm_species) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
worn.hull <- 
  nmds_SiteScores %>% 
  group_by(worm_species) %>%
  slice(chull(NMDS1, NMDS2))


# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(data = worm_Centroid, 
             aes(x = axis1, y = axis2, color = worm_species), 
             size = 5, shape = 17) +
  # add convex hull
  geom_polygon(data = worn.hull, 
               aes(x = NMDS1, y = NMDS2, fill = worm_species, group = worm_species), 
               alpha = 0.30) +
  geom_point(size = 2, aes(col = soil_chem_df.worm$worm_species)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1, hjust = -1, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Chemistry Characteristics by Earthworm Species",
       x = "NMDS1",
       y = "NMDS2", 
       color = "Worms Detected?",
       fill = "Worms Detected?") +
  annotate("text", x = Inf, y = -Inf, label = paste0("Stress = ", round(nmds$stress, 3)), 
           hjust = 1.1, vjust = -1.1, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000")) +
  scale_fill_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000"))




# Assuming you have your NMDS object `nmds`
# And your original environmental matrix `nmds_input`

envfit_results <- envfit(nmds, nmds_input, permutations = 999)

# See the results:
print(envfit_results)

# Plot arrows for environmental variables:
plot(nmds, display = "sites")
plot(envfit_results, p.max = 0.05)  # only show variables with p < 0.05


#PERMANOVA

nmds_input <- site_data_df_full_names_euro.chem %>%
  dplyr::select(where(is.numeric)) %>%                                 # Keep only numeric columns
  mutate(across(everything(), ~ ifelse(is.na(.) | . < 0, 0, .)))       # Replace NA and negatives with 0


data.matrix <- as.matrix(nmds_input)
rownames(data.matrix) <- c(site_data_df_full_names$Plot_Full_Name)
rownames(data.matrix) <- c("Colby 1", "Colby 2", "Colby 3", "Colby 4", "Colby 5", "Colby 6", "Colby 7", "Colby 8",
                           "Colby 9", "Seboeis Lake 3", "Androscoggin Rverlands 2","Kennebec Highlands 1", 
                           "Kennebec Highlands 2", "Kennebec Highlands 4", "Kennebec Pubic Land Trust 1",
                           "Kennebec Pubic Land Trust 3","Viles Aboretum 1", "Viles Aboretum 2", "Viles Aboretum 3",
                           "Bigelow 1", "Bigelow 4",  "Bradbury 1", "Bradbury 2", "Bradley 2", "Falmouth Land Trust 1", 
                           "Falmouth Land Trust 3", "Colby Hume 1", "Little Moose 3", "Little Moose 4", "Mahoosuc 2",
                           "Sebago Lake 1", "Sebago Lake 2", "Scopan Lake 2","Coastal Maine Botanical Gardens 1", 
                           "Coastal Maine Botanical Gardens 2", "Coastal Maine Botanical Gardens 3")


soil_chem_df.worm <- soil_chem_df %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))


soil_chem_df.worm.tested <- soil_chem_df.worm[is.na(soil_chem_df.worm$European.Worms) == F, ]


#remove unnecessary columns
soil_chem_df.worm.fixed <- soil_chem_df.worm.tested %>%
  select(c(!ME_Lab_. & !Sample_ID & !Contamination & !Site & !Plot & !European.Worms & !worm_species & !Lumbricus.rubellus. & !Lumbricus.terrestris. & !Aporrectodea.caliginosa.))



site_data_df_full_names_euro.chem <- site_data_df_full_names_euro.chem %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

View(data.matrix)

data.matrix[-c(23, 33),] #removing bradbury 2 and scopan lake 2

site_data_df_full_names_euro.chem$worm_species[-c(23, 33)]  #removing bradbury 2 and scopan lake 2

#comparison between locations with different worm species
adonis_result <- adonis2(data.matrix~ site_data_df_full_names_euro.chem$worm_species, na.rm = T, 
                         method = "euclidean")
print(adonis_result)

#with outliers removed
adonis_result <- adonis2(data.matrix[-c(23, 33),] ~ site_data_df_full_names_euro.chem$worm_species[-c(23, 33)], na.rm = T, 
                         method = "euclidean")
print(adonis_result)

#comparison between locations with and without worms
adonis_result <- adonis2(tree_matrix.rel ~ site_data_df_full_names_euro.chem$European.Worms, na.rm = T,
                         method = "euclidean")
print(adonis_result)











### NMDS with plant composition 

## TREES

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

#turning absolute abundance into relative abundance to avoid skewness
# Calculating relative abundance and creating new dataframe with relative abundance data
tree_matrix.rel <-         
  decostand(tree_matrix, method = "total")


#making the NMDS
set.seed(123)
#i use euclidean because the data are very different temp, moss cover, etc. 
nmds = metaMDS(tree_matrix.rel, distance = "bray", 
               autotransform = T, na.rm = T, k = 6)
nmds


# Shepards test/goodness of fit
goodness(nmds) # Produces a results of test statistics for goodness of fit for each point
stressplot(nmds)

#stress vs. dimensionalty plot
library(goeveg)
dimcheck_out <- 
  dimcheckMDS(tree_matrix.rel, distance = "bray", 
              autotransform = T, k = 6) #com_matrix[,2:9], distance = "bray", k = 6
 
plot(nmds, type = 't')
with(site_data_df_full_names.nmds, points(nmds, display = 'Site'))



# Assuming you already have your NMDS result stored in `nmds`

# Step 1: Extract site scores (coordinates for each site)
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Add your plot/site names back in
nmds_scores$Site <- rownames(nmds_scores)

#add worm species column
tree_abundance_table_joined.worm <- tree_abundance_table_joined %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

# get worm presence   
detected_type <- tree_abundance_table_joined.worm%>% 
  distinct(Site, Plot, Detected.European) %>%
  mutate(Site = paste0(Site, " ", Plot))

# Extract NMDS scores for sites 
nmds_SiteScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)$sites) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>%
  # join our habitat type (grouping variable) to each site 
  left_join(detected_type, by = "Site")

# Extract NMDS scores for species  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
detected_Centroid <- 
  nmds_SiteScores %>% 
  group_by(Detected.European) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
detected.hull <- 
  nmds_SiteScores %>% 
  group_by(Detected.European) %>%
  slice(chull(NMDS1, NMDS2))



# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(data = detected_Centroid, 
             aes(x = axis1, y = axis2, color = Detected.European), 
             size = 5, shape = 17) +
  # add convex hull
  geom_polygon(data = detected.hull, 
               aes(x = NMDS1, y = NMDS2, fill = Detected.European, group = Detected.European), 
               alpha = 0.30) +
  geom_point(size = 2, aes(col = tree_abundance_table_joined$Detected.European)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1, hjust = -1, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Tree Relative Abundances",
       x = "NMDS1",
       y = "NMDS2", 
       color = "Worms Detected?", 
       fill = "Worms Detected?") +
  annotate("text", x = Inf, y = -Inf, label = paste0("Stress = ", round(nmds$stress, 3)), 
           hjust = 1.1, vjust = -1.1, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  # add centroid 
  scale_color_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000")) +
  scale_fill_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000"))


# get worm presence   
worm_type <- tree_abundance_table_joined.worm %>% 
  distinct(Site, Plot, worm_species) %>%
  mutate(Site = paste0(Site, " ", Plot))

# Extract NMDS scores for sites 
nmds_SiteScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)$sites) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>%
  # join our habitat type (grouping variable) to each site 
  left_join(worm_type, by = "Site")

# Extract NMDS scores for species  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
worm_Centroid <- 
  nmds_SiteScores %>% 
  group_by(worm_species) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
worn.hull <- 
  nmds_SiteScores %>% 
  group_by(worm_species) %>%
  slice(chull(NMDS1, NMDS2))


# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(data = worm_Centroid, 
             aes(x = axis1, y = axis2, color = worm_species), 
             size = 5, shape = 17) +
  # add convex hull
  geom_polygon(data = worn.hull, 
               aes(x = NMDS1, y = NMDS2, fill = worm_species, group = worm_species), 
               alpha = 0.30) +
  geom_point(size = 2, aes(col = tree_abundance_table_joined.worm$worm_species)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1, hjust = -1, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Tree Relative Abundances",
       x = "NMDS1",
       y = "NMDS2", 
       color = "Worms Detected?",
       fill = "Worms Detected?") +
  annotate("text", x = Inf, y = -Inf, label = paste0("Stress = ", round(nmds$stress, 3)), 
           hjust = 1.1, vjust = -1.1, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000")) +
  scale_fill_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000"))



# Assuming you have your NMDS object `nmds`
# And your original environmental matrix `nmds_input`

envfit_results <- envfit(nmds, tree_matrix.rel, permutations = 999)

# See the results:
print(envfit_results)

# Plot arrows for environmental variables:
plot(nmds, display = "sites")
plot(envfit_results, p.max = 0.05)  # only show variables with p < 0.05

#PERMANOVA

tree_abundance_table_joined <- tree_abundance_table %>%
  mutate(Detected.European = trees_df_full_names_w_invasives$Detected.European[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = trees_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = trees_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = trees_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, trees_df_full_names_w_invasives$Plot.name)])

tree_abundance_table_joined.tested <- tree_abundance_table_joined[is.na(tree_abundance_table_joined$Detected.European) == F, ]

#remove unnecessary columns
tree_abundance_table.fixed <- tree_abundance_table_joined.tested %>%
  select(c(!Site & !Plot & !Plot.name & !Detected.European & !Lumbricus.rubellus. & !Lumbricus.terrestris. & !Aporrectodea.caliginosa.))

#turning it into a matrix
tree_matrix <- as.matrix(tree_abundance_table.fixed)
Plot.name <- as.vector(tree_abundance_table_joined.tested$Plot.name)
rownames(tree_matrix) <- Plot.name

#turning absolute abundance into relative abundance to avoid skewness
# Calculating relative abundance and creating new dataframe with relative abundance data
tree_matrix.rel <-         
  decostand(tree_matrix, method = "total")

tree_abundance_table_joined.worm <- tree_abundance_table_joined.tested %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

#comparison between locations with different worm species
adonis_result <- adonis2(tree_matrix.rel ~ tree_abundance_table_joined.worm$worm_species, na.rm = T)
print(adonis_result)

#comparison between locations with and without worms
adonis_result <- adonis2(tree_matrix.rel ~ tree_abundance_table_joined.worm$Detected.European, na.rm = T)
print(adonis_result)



## SAPLINGS

saplings_df_full_names_w_invasives
#1. creating abolute abundance tables 
saplings_df_full_names_w_invasives.abundances <- saplings_df_full_names_w_invasives %>%
  group_by(Plot, Site, Species) %>%
  summarise(count = n(), .groups = "drop")
# group_by(Plot, Site) %>%
# mutate(prop = count / sum(count)) %>%
# mutate(percent = prop*100)

#pivoting the table so rows are plots and columns are each tree species
saplings_abundance_table <- saplings_df_full_names_w_invasives.abundances %>%
  pivot_wider(names_from = Species,   # Make tree species into columns
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
sapling_matrix <- as.matrix(saplings_abundance_table.fixed)
Plot.name <- as.vector(saplings_abundance_table_joined$Plot.name)
rownames(sapling_matrix) <- Plot.name

#turning absolute abundance into relative abundance to avoid skewness
# Calculating relative abundance and creating new dataframe with relative abundance data
sapling_matrix.rel <-         
  decostand(sapling_matrix, method = "total")


#making the NMDS
set.seed(123)
#i use euclidean because the data are very different temp, moss cover, etc. 
nmds = metaMDS(sapling_matrix.rel, distance = "bray", 
               autotransform = T, na.rm = T, k = 6)
nmds


# Shepards test/goodness of fit
goodness(nmds) # Produces a results of test statistics for goodness of fit for each point
stressplot(nmds)

#stress vs. dimensionalty plot
library(goeveg)
dimcheck_out <- 
  dimcheckMDS(sapling_matrix.rel, distance = "bray", 
              autotransform = T, k = 6) #com_matrix[,2:9], distance = "bray", k = 6

plot(nmds, type = 't')
with(site_data_df_full_names.nmds, points(nmds, display = 'Site'))



# Assuming you already have your NMDS result stored in `nmds`

# Step 1: Extract site scores (coordinates for each site)
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Add your plot/site names back in
nmds_scores$Site <- rownames(nmds_scores)

#add worm species column
saplings_abundance_table_joined.worm <- saplings_abundance_table_joined %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

# get worm presence   
detected_type <- saplings_abundance_table_joined.worm%>% 
  distinct(Site, Plot, Detected.European) %>%
  mutate(Site = paste0(Site, " ", Plot))

# Extract NMDS scores for sites 
nmds_SiteScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)$sites) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>%
  # join our habitat type (grouping variable) to each site 
  left_join(detected_type, by = "Site")

# Extract NMDS scores for species  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
detected_Centroid <- 
  nmds_SiteScores %>% 
  group_by(Detected.European) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
detected.hull <- 
  nmds_SiteScores %>% 
  group_by(Detected.European) %>%
  slice(chull(NMDS1, NMDS2))



# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(data = detected_Centroid, 
             aes(x = axis1, y = axis2, color = Detected.European), 
             size = 5, shape = 17) +
  # add convex hull
  geom_polygon(data = detected.hull, 
               aes(x = NMDS1, y = NMDS2, fill = Detected.European, group = Detected.European), 
               alpha = 0.30) + 
  geom_point(size = 2, aes(col = saplings_abundance_table_joined$Detected.European)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1, hjust = -1, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Sapling Relative Abundances",
       x = "NMDS1",
       y = "NMDS2", 
       color = "Worms Detected?",
       fill = "Worms Detected?") +
  annotate("text", x = Inf, y = -Inf, label = paste0("Stress = ", round(nmds$stress, 3)), 
           hjust = 1.1, vjust = -1.1, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  # add centroid 
  scale_color_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000")) +
  scale_fill_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000"))


# get worm presence   
worm_type <- saplings_abundance_table_joined.worm %>% 
  distinct(Site, Plot, worm_species) %>%
  mutate(Site = paste0(Site, " ", Plot))

# Extract NMDS scores for sites 
nmds_SiteScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)$sites) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>%
  # join our habitat type (grouping variable) to each site 
  left_join(worm_type, by = "Site")

# Extract NMDS scores for species  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
worm_Centroid <- 
  nmds_SiteScores %>% 
  group_by(worm_species) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
worn.hull <- 
  nmds_SiteScores %>% 
  group_by(worm_species) %>%
  slice(chull(NMDS1, NMDS2))


# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(data = worm_Centroid, 
             aes(x = axis1, y = axis2, color = worm_species), 
             size = 5, shape = 17) +
  # add convex hull
  geom_polygon(data = worn.hull, 
               aes(x = NMDS1, y = NMDS2, fill = worm_species, group = worm_species), 
               alpha = 0.30) +
  geom_point(size = 2, aes(col = saplings_abundance_table_joined.worm$worm_species)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1, hjust = -1, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Sapling Relative Abundances",
       x = "NMDS1",
       y = "NMDS2", 
       color = "Worms Detected", 
       fill = "Worms Detected") +
  annotate("text", x = Inf, y = -Inf, label = paste0("Stress = ", round(nmds$stress, 3)), 
           hjust = 1.1, vjust = -1.1, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000")) +
  scale_fill_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000"))



# Assuming you have your NMDS object `nmds`
# And your original environmental matrix `nmds_input`

envfit_results <- envfit(nmds, sapling_matrix.rel, permutations = 999)

# See the results:
print(envfit_results)

# Plot arrows for environmental variables:
plot(nmds, display = "sites")
plot(envfit_results, p.max = 0.05)  # only show variables with p < 0.05

#PERMANOVA

sapling_matrix.rel_joined <- saplings_abundance_table %>%
  mutate(Detected.European = saplings_df_full_names_w_invasives$Detected.European[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = saplings_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = saplings_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = saplings_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, saplings_df_full_names_w_invasives$Plot.name)])

saplings_abundance_table_joined.tested <- saplings_abundance_table_joined[is.na(saplings_abundance_table_joined$Detected.European) == F, ]

#remove unnecessary columns
saplings_abundance_table.fixed <- saplings_abundance_table_joined.tested %>%
  select(c(!Site & !Plot & !Plot.name & !Detected.European & !Lumbricus.rubellus. & !Lumbricus.terrestris. & !Aporrectodea.caliginosa.))

#turning it into a matrix
sapling_matrix <- as.matrix(saplings_abundance_table.fixed)
Plot.name <- as.vector(saplings_abundance_table_joined.tested$Plot.name)

rownames(sapling_matrix) <- Plot.name

#turning absolute abundance into relative abundance to avoid skewness
# Calculating relative abundance and creating new dataframe with relative abundance data
sapling_matrix.rel <-         
  decostand(sapling_matrix, method = "total")

saplings_abundance_table_joined.worm <- saplings_abundance_table_joined.tested %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

#comparison between locations with different worm species
adonis_result <- adonis2(sapling_matrix.rel ~ saplings_abundance_table_joined.worm$worm_species, na.rm = T)
print(adonis_result)

#comparison between locations with and without worms
adonis_result <- adonis2(sapling_matrix.rel ~ saplings_abundance_table_joined.worm$Detected.European, na.rm = T)
print(adonis_result)






## HERBS / QUADRAT

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

#pivoting the table so rows are plots and columns are each tree species
quadrat_abundance_table <- quadrat_df_full_names_w_invasives.abundances %>%
  pivot_wider(names_from = Species,   # Make tree species into columns
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

#turning absolute abundance into relative abundance to avoid skewness
# Calculating relative abundance and creating new dataframe with relative abundance data
quadrat_matrix.rel <-         
  decostand(quadrat_matrix, method = "total")


#making the NMDS
set.seed(123)
#i use euclidean because the data are very different temp, moss cover, etc. 
nmds = metaMDS(quadrat_matrix.rel, distance = "bray", 
               autotransform = T, na.rm = T, k = 6)
nmds


# Shepards test/goodness of fit
goodness(nmds) # Produces a results of test statistics for goodness of fit for each point
stressplot(nmds)

#stress vs. dimensionalty plot
library(goeveg)
dimcheck_out <- 
  dimcheckMDS(quadrat_matrix.rel, distance = "bray", 
              autotransform = T, k = 6) #com_matrix[,2:9], distance = "bray", k = 6

plot(nmds, type = 't')
with(site_data_df_full_names.nmds, points(nmds, display = 'Site'))



# Assuming you already have your NMDS result stored in `nmds`

# Step 1: Extract site scores (coordinates for each site)
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Add your plot/site names back in
nmds_scores$Site <- rownames(nmds_scores)

#add worm species column
quadrat_abundance_table_joined.worm <- quadrat_abundance_table_joined %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

# get worm presence   
detected_type <- quadrat_abundance_table_joined.worm%>% 
  distinct(Site, Plot, Detected.European) %>%
  mutate(Site = paste0(Site, " ", Plot))

# Extract NMDS scores for sites 
nmds_SiteScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)$sites) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>%
  # join our habitat type (grouping variable) to each site 
  left_join(detected_type, by = "Site")

# Extract NMDS scores for species  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
detected_Centroid <- 
  nmds_SiteScores %>% 
  group_by(Detected.European) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
detected.hull <- 
  nmds_SiteScores %>% 
  group_by(Detected.European) %>%
  slice(chull(NMDS1, NMDS2))



# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(data = detected_Centroid, 
             aes(x = axis1, y = axis2, color = Detected.European), 
             size = 5, shape = 17) +
  # add convex hull
  geom_polygon(data = detected.hull, 
               aes(x = NMDS1, y = NMDS2, fill = Detected.European, group = Detected.European), 
               alpha = 0.30) + 
  geom_point(size = 2, aes(col = quadrat_abundance_table_joined$Detected.European)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1, hjust = -1, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Herb Relative Abundances",
       x = "NMDS1",
       y = "NMDS2", 
       color = "Worms Detected?",
       fill = "Worms Detected?") +
  annotate("text", x = Inf, y = -Inf, label = paste0("Stress = ", round(nmds$stress, 3)), 
           hjust = 1.1, vjust = -1.1, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  # add centroid 
  scale_color_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000")) +
  scale_fill_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000"))


# get worm presence   
worm_type <- quadrat_abundance_table_joined.worm %>% 
  distinct(Site, Plot, worm_species) %>%
  mutate(Site = paste0(Site, " ", Plot))

# Extract NMDS scores for sites 
nmds_SiteScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)$sites) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>%
  # join our habitat type (grouping variable) to each site 
  left_join(worm_type, by = "Site")

# Extract NMDS scores for species  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# get centroid 
worm_Centroid <- 
  nmds_SiteScores %>% 
  group_by(worm_species) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
worn.hull <- 
  nmds_SiteScores %>% 
  group_by(worm_species) %>%
  slice(chull(NMDS1, NMDS2))


# Step 2: Make the ggplot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) + #label = Site
  geom_point(data = worm_Centroid, 
             aes(x = axis1, y = axis2, color = worm_species), 
             size = 5, shape = 17) +
  # add convex hull
  geom_polygon(data = worn.hull, 
               aes(x = NMDS1, y = NMDS2, fill = worm_species, group = worm_species), 
               alpha = 0.30) +
  geom_point(size = 2, aes(col = quadrat_abundance_table_joined.worm$worm_species)) + #shape = site_data_df_full_names_euro.chem.worm$Site
  #geom_text_repel(size = 2.5) +
  #geom_text(vjust = -1, hjust = -1, size = 2, position = position_dodge(1.2)) +
  theme_minimal() +
  labs(title = "NMDS of Plot Herb Relative Abundances",
       x = "NMDS1",
       y = "NMDS2", 
       color = "Worms Detected", 
       fill = "Worms Detected") +
  annotate("text", x = Inf, y = -Inf, label = paste0("Stress = ", round(nmds$stress, 3)), 
           hjust = 1.1, vjust = -1.1, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000")) +
  scale_fill_manual(values  = c("#fee5d9","#fcae91", "#fb6a4a", "#cb181d", "#000000"))



# Assuming you have your NMDS object `nmds`
# And your original environmental matrix `nmds_input`

envfit_results <- envfit(nmds, quadrat_matrix.rel, permutations = 999)

# See the results:
print(envfit_results)

# Plot arrows for environmental variables:
plot(nmds, display = "sites")
plot(envfit_results, p.max = 0.05)  # only show variables with p < 0.05

#PERMANOVA

quadrat_matrix.rel_joined <- quadrat_abundance_table %>%
  mutate(Detected.European = quadrat_df_full_names_w_invasives$Detected.European[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.rubellus. = quadrat_df_full_names_w_invasives$Lumbricus.rubellus.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Lumbricus.terrestris. = quadrat_df_full_names_w_invasives$Lumbricus.terrestris.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)]) %>%
  mutate(Aporrectodea.caliginosa. = quadrat_df_full_names_w_invasives$Aporrectodea.caliginosa.[match(Plot.name, quadrat_df_full_names_w_invasives$Plot.name)])

quadrat_abundance_table_joined.tested <- quadrat_abundance_table_joined[is.na(quadrat_abundance_table_joined$Detected.European) == F, ]

#remove unnecessary columns
quadrat_abundance_table.fixed <- quadrat_abundance_table_joined.tested %>%
  select(c(!Site & !Plot & !Plot.name & !Detected.European & !Lumbricus.rubellus. & !Lumbricus.terrestris. & !Aporrectodea.caliginosa.))

#turning it into a matrix
quadrat_matrix <- as.matrix(quadrat_abundance_table.fixed)
Plot.name <- as.vector(quadrat_abundance_table_joined.tested$Plot.name)
rownames(quadrat_matrix) <- Plot.name

#turning absolute abundance into relative abundance to avoid skewness
# Calculating relative abundance and creating new dataframe with relative abundance data
quadrat_matrix.rel <-         
  decostand(quadrat_matrix, method = "total")

quadrat_abundance_table_joined.worm <- quadrat_abundance_table_joined.tested %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))

#comparison between locations with different worm species
adonis_result <- adonis2(quadrat_matrix.rel ~ quadrat_abundance_table_joined.worm$worm_species, na.rm = T)
print(adonis_result)

#comparison between locations with and without worms
adonis_result <- adonis2(quadrat_matrix.rel ~ quadrat_abundance_table_joined.worm$Detected.European, na.rm = T)
print(adonis_result)



#table in results



#mean average leaf litter
View(site_data_df_full_names_euro.chem.worm)
View(site_data_df_full_names)
mean(site_data_df_full_names)

site_data_df_full_names.table <- site_data_df_full_names %>%
  group_by(Detected.European) %>%
  summarise(
    n = n(),
    
    mean.leaf.cover = mean(avg.litter.cover, na.rm = TRUE),
    sd.leaf.cover = sd(avg.litter.cover, na.rm = TRUE),
    ci95.low.leaf.cover = mean.leaf.cover - 1.96 * sd.leaf.cover / sqrt(n),
    ci95.high.leaf.cover = mean.leaf.cover + 1.96 * sd.leaf.cover / sqrt(n),
    
    mean.litter.depth = mean(litter.depth..cm., na.rm = TRUE),
    sd.litter.depth = sd(litter.depth..cm., na.rm = TRUE),
    ci95.low.litter.depth = mean.litter.depth - 1.96 * sd.litter.depth / sqrt(n),
    ci95.high.litter.depth = mean.litter.depth + 1.96 * sd.litter.depth / sqrt(n),
    
    mean.crown.density = mean(Crown.Density, na.rm = TRUE),
    sd.crown.density = sd(Crown.Density, na.rm = TRUE),
    ci95.low.crown.density = mean.crown.density - 1.96 * sd.crown.density / sqrt(n),
    ci95.high.crown.density = mean.crown.density + 1.96 * sd.crown.density / sqrt(n),
    
    mean.herb.cover = mean(Herbaceous.Layer...Cover, na.rm = TRUE),
    sd.herb.cover = sd(Herbaceous.Layer...Cover, na.rm = TRUE),
    ci95.low.herb.cover = mean.herb.cover - 1.96 * sd.herb.cover / sqrt(n),
    ci95.high.herb.cover = mean.herb.cover + 1.96 * sd.herb.cover / sqrt(n)
  )


#drainage classes
site_data_df_full_names.table.prop <- site_data_df_full_names %>%
  group_by(Detected.European, drainage) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Detected.European) %>%
  mutate(
    total = sum(count),
    prop = count / total,
    se = sqrt(prop * (1 - prop) / total),
    ci95.low = prop - 1.96 * se,
    ci95.high = prop + 1.96 * se
  )

#moisture  
site_data_df_full_names.table.prop <- site_data_df_full_names %>%
  group_by(Detected.European, soil.moisture) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Detected.European) %>%
  mutate(
    total = sum(count),
    prop = count / total,
    se = sqrt(prop * (1 - prop) / total),
    ci95.low = prop - 1.96 * se,
    ci95.high = prop + 1.96 * se
  )



# by worm species 

site_data_df_full_names <- site_data_df_full_names %>%
  mutate(worm_species = case_when(Lumbricus.rubellus. == "Y" ~ "Lumbricus rubellus",
                                  (Aporrectodea.caliginosa. == "N" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris",
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "N") ~ "Aporrectodea caliginosa", 
                                  (Aporrectodea.caliginosa. == "Y" & Lumbricus.terrestris. == "Y") ~ "Lumbricus terrestris and Aporrectodea caliginosa",
                                  Lumbricus.rubellus. == "N" & Lumbricus.terrestris. == "N" & Aporrectodea.caliginosa. == "N" ~ "No Worms Detected",
                                  TRUE ~ NA_character_))



site_data_df_full_names.table.species <- site_data_df_full_names %>%
  group_by(worm_species) %>%
  summarise(
    n = n(),
    
    mean.leaf.cover = mean(avg.litter.cover, na.rm = TRUE),
    sd.leaf.cover = sd(avg.litter.cover, na.rm = TRUE),
    ci95.low.leaf.cover = mean.leaf.cover - 1.96 * sd.leaf.cover / sqrt(n),
    ci95.high.leaf.cover = mean.leaf.cover + 1.96 * sd.leaf.cover / sqrt(n),
    
    mean.litter.depth = mean(litter.depth..cm., na.rm = TRUE),
    sd.litter.depth = sd(litter.depth..cm., na.rm = TRUE),
    ci95.low.litter.depth = mean.litter.depth - 1.96 * sd.litter.depth / sqrt(n),
    ci95.high.litter.depth = mean.litter.depth + 1.96 * sd.litter.depth / sqrt(n),
    
    mean.crown.density = mean(Crown.Density, na.rm = TRUE),
    sd.crown.density = sd(Crown.Density, na.rm = TRUE),
    ci95.low.crown.density = mean.crown.density - 1.96 * sd.crown.density / sqrt(n),
    ci95.high.crown.density = mean.crown.density + 1.96 * sd.crown.density / sqrt(n),
    
    mean.herb.cover = mean(Herbaceous.Layer...Cover, na.rm = TRUE),
    sd.herb.cover = sd(Herbaceous.Layer...Cover, na.rm = TRUE),
    ci95.low.herb.cover = mean.herb.cover - 1.96 * sd.herb.cover / sqrt(n),
    ci95.high.herb.cover = mean.herb.cover + 1.96 * sd.herb.cover / sqrt(n)
  )


#drainage classes
site_data_df_full_names.table.prop <- site_data_df_full_names %>%
  group_by(worm_species, drainage) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(worm_species) %>%
  mutate(
    total = sum(count),
    prop = count / total,
    se = sqrt(prop * (1 - prop) / total),
    ci95.low = prop - 1.96 * se,
    ci95.high = prop + 1.96 * se
  )

#moisture  
site_data_df_full_names.table.prop <- site_data_df_full_names %>%
  group_by(worm_species, soil.moisture) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(worm_species) %>%
  mutate(
    total = sum(count),
    prop = count / total,
    se = sqrt(prop * (1 - prop) / total),
    ci95.low = prop - 1.96 * se,
    ci95.high = prop + 1.96 * se
  )


#sites by detection
site_data_df_full_names <- site_data_df_full_names %>%
  mutate(Plot.name = paste0(Site, " ", Plot))  

table(site_data_df_full_names$Plot.name, 
          site_data_df_full_names$worm_species)
















                                                              
