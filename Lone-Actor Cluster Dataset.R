###############################################################
# Lone-Actor imputed dataset for cluster analysis
###############################################################

library(haven)

# Link to download PIRUS dataset https://www.start.umd.edu/data-tools/profiles-individual-radicalization-united-states-pirus

#Import PIRUS Data
pirus_classify <- read_excel("INSERT PATH TO DOWNLOAD FILE")

# Remove dates prior to 1995
pirus_clust$Date_Exposure <- as.Date(pirus_clust$Date_Exposure)
pirus <- pirus %>% filter(Date_Exposure >= as.Date("1995-01-01"))

# Remove missing group membership variables
pirus_clust <- pirus_clust %>%
  filter(Group_Membership != -99) %>% 
  filter(Age != -99)

# Select vars
vars <- c('Standing','Radical_Beliefs','Work_History','Angry_US','Platonic_Troubles','Group_Membership','Gender', 'Age', 'Previous_Criminal_Activity', 'Psychological', 'Internet_Radicalization', 'Attack_Preparation', 'Rad_duration', 'Plot_Target1', 'Anticp_Fatals_Targ', 'Op_Security', 'Military', 'Marital_Status')
pirus_clust <- pirus_clust %>% select(all_of(vars))

# Remove -88 values
pirus_clust[pirus_clust == -88] <- NA
pirus_clust <- na.omit(pirus_clust)

# Summarise Missing Data
pirus_clust_na <- pirus_clust %>% summarise_all(~ sum(is.na(.)))
total_na <- sum(pirus_clust_na)/(256*17)

# Set dependent variables
pirus_clust$Group_Membership <- ifelse(pirus_clust$Group_Membership == 0, 1, 0)
pirus_clust$Group_Membership <- factor(pirus_clust$Group_Membership, levels = 0:1, labels = c('group', 'loner'))

pirus_clust <- pirus_clust %>% filter(Group_Membership == 'loner') %>%
  select(-Group_Membership)

# Convert all -99 to NA
pirus_clust[pirus_clust < 0] <- NA

# Impute Missing Data
imputed_pirus_clust <- mice(pirus_clust, m=25, maxit = 50, method = 'cart', seed = 500)
pirus_clust <- complete(imputed_pirus_clust)

# Convert to SPSS Data
write_sav(pirus_clust, 'cluster_data.sav')