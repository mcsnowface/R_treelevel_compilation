#### Required packages ###

library(tidyverse)

#### Required data sets ####

## read measurement data ##
measurements <- read_csv("G:\Shared drives\Growth & Yield Lab\Data Sets\PGYI\2023-09-08 PGYI Export\trees_measurement.csv")
dim(measurements)

# check for duplicates
measurements %>%
  group_by_all() %>%
  filter(n() > 1) # NO DUPLICATES FOUND


## read measurement information ##
measurement_info <- read_csv("G:\Shared drives\Growth & Yield Lab\Data Sets\PGYI\2023-09-08 PGYI Export\plot_measurement.csv")
dim(measurement_info)

# check for duplicates
measurement_info %>%
  group_by_all() %>%
  filter(n() > 1) # NO DUPLICATES FOUND


## read species list ##
species_list <- read_csv("G:\Shared drives\Growth & Yield Lab\Data Sets\PGYI\2023-09-08 PGYI Export\trees.csv")
dim(species_list)

# check for duplicates
species_list %>%
  group_by_all() %>%
  filter(n() > 1) # NO DUPLICATES FOUND


## read plot information ##
plot_info <- read_csv("G:\Shared drives\Growth & Yield Lab\Data Sets\PGYI\2023-09-08 PGYI Export\plot.csv")
dim(plot_info)

# check for duplicates
plot_info %>%
  group_by_all() %>%
  filter(n() > 1) %>%
  print(n = Inf) -> plot_info_dups


## write out duplicates for further checking and for documentation ##
write_csv(plot_info_dups, "duplicates/plot_duplicates.csv")


## remove duplicate rows from plot_info and save as plot_info2 ##
plot_info %>%
  distinct() -> plot_info2


#### Joining datasets ####

# start with the largest file first than join the smaller ones
# keep track that the number of rows won't get larger

measurements %>%
  left_join(measurement_info) -> info1

info1 %>%
  left_join(species_list) -> info2

info2 %>%
  left_join(plot_info2) -> pgyi_tree_level_master

dim(pgyi_tree_level_master)

# again checking whether there are any duplicates with respect to the measurements of interest (the group_by() statement)
pgyi_tree_level_master %>%
  group_by(company,
           company_plot_number,
           tree_number,
           measurement_year,
           dbh) %>%
  filter(n() > 1)  %>%
  print(n = Inf) -> master_dups

# refer to column "plot_measurement_comment" and column "condition_code1"
# all these duplicates are labeled as "missing" trees and will be removed when filtering for alive and dead trees
# so we won't exclude them here but simply write them out to have a record of this
write_csv(master_dups, "duplicates/pgyi_tree_level_master_duplicates.csv")

# write master pgyi file
write_csv(pgyi_tree_level_master, "pgyi_tree_level_master.csv")


#### Here is an example of how species grouping can be created as well as how to calculate basal area and BALT (competition index) ####
# The final result of this I call "augmented pgyi tree level master file"
pgyi_tree_level_master %>%
  # remove any dbh measurements with NAs
  # at present I don't have a comparable way of imputing dbh measurements they way the SAS code does!
  filter(!is.na(dbh)) %>%
  # create species groups
  mutate(
    species_groups = case_when(
      species %in% c("Aw", "Pb", "Bw", "Ax") ~ "Deciduous",
      species %in% c("Pl", "Pw", "Pf", "Pj", "Px") ~ "Pine",
      species %in% c("Sw", "Se", "Sb", "Sx", "Lt", "Lw", "La", "Ls") ~ "Spruce",
      species %in% c("Fb", "Fa", "Fd") ~ "Fir",
      species %in% c("Lt", "Lw", "La", "Ls") ~ "Larch",
      .default = NA # if none of the above conditions are met, the species_groups will be NA
      # this is just a check. Currently there is no NAs in species_groups
    ),
    # calculate basal area
    basal_area = ((pi * ((
      dbh / 2
    ) ^ 2)) / 10000),
    # calculate expansion factor
    expansion_factor = case_when(
      tree_type %in% c("T", "ET") ~ 10000 / tree_plot_area,
      tree_type %in% c("S1", "ES1") ~ 10000 / sapling_plot_area,
      tree_type %in% c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10") ~ 10000 / (regen_plot_area * number_regen_plots),
      .default = NA
    ),
    # scale basal area to one hectare
    basal_area2 = basal_area * expansion_factor
  ) %>%
  # calculate basal area sum of trees larger than the subject tree within the PSP and YEAR (BALT)
  group_by(measurement_year, company_plot_number) %>%
  arrange(desc(basal_area2), .by_group = TRUE) %>%
  mutate(balt = lag(cumsum(basal_area2), default = 0)) %>%
  ungroup() -> pgyi_tree_level_master_augmented

# write out augmented pgyi tree level master file
write_csv(pgyi_tree_level_master_augmented, "pgyi_tree_level_master_augmented.csv")
