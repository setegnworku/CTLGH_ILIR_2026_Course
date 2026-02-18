########################################################
# Day 3: Pedigree BLUP using BLUPf90
# 05th February, 2025
# Prepared by: Gabriela and adapted by Isidore
#######################################################
# Preparing data for BLUPF90
# and reading results

# this is a package for data manipulation 
# it's syntax is easier to understand 
# and certain data modifications are easier
library(tidyverse)

# set the working directory to the folder where your data is
setwd("/Users/ihouaga2/Desktop/CTLGH_TRAINING_ILRI_2025/Day_3_Wed_5th_February_2025/")
getwd()
# the data you worked with was already cleaned
# here are the steps I took to generate it

# reading the raw datafile
pheno_file = read.csv("/Users/ihouaga2/Desktop/CTLGH_TRAINING_ILRI_2025/data_training/CT_traits_724_pc_res.csv")
head(pheno_file)

# BLUPF90 codes 0 as missing data
# to avoid issues I will recode sex 0 as 2
# to change the values I use the function mutate()
# I tell mutate() that in the column sex, if the value is 0, change to 2,
# else keep the data as it is:
pheno_file = pheno_file %>% 
  mutate(sex = ifelse(sex == 0, 2, sex))

# we can use table() to check the change:
table(pheno_file$sex)

# I'll remove all missing observations in LW
# I use the function filter()
# and it keepes only the rows where LW is not NA 
# (the ! means NOT and the function is.na() identifies all rows with NA for LW)
pheno_file = pheno_file %>% filter(!is.na(LW))

# level 1 has too little records
# create a new variable where we combine 1-2
# I use the function mutate() again but now I tell it that
# in the column DamAge, if values are 1 or 2, change to 12, else,
# keep the original data
pheno_file = pheno_file %>% 
  mutate(DamAge = ifelse(DamAge %in% c(1,2), 12,
                         DamAge))

# we can check the changes with table()
table(pheno_file$DamAge)

# saving the output file in BLUPF90 format
write.table(pheno_file, "sheep.dat",
            quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)


# READING RESULTS ----
# if you don't have the object pheno_file, run the following line
#pheno_file = read_table("sheep.dat", col_names = FALSE)

# import BLUPF90 solutions to R 
solutions = read_table("solutions.orig", col_names = TRUE)

# filter the table by:
# trait we want the results for
# the effect we are interested in (animal random effect)
# select the columns with the ids and solutions
# order the data set by the ids
sol = solutions %>% 
  filter(trait == 1, 
         effect == 9) %>%  
  select(original_id, solution) %>%
  arrange(original_id)

#pheno_file = read_table("sheep.dat", col_names = FALSE)
# select only the id and LW columns
# order the phenotype file by id
pheno_file = pheno_file %>% 
  select(id, LW) %>%
  arrange(id)

# create a new column for estimated breeding values
# (the results from BLUPF90)
# assign to that new column the values in the sol table
# making sure that ids match between both table
pheno_file["ebv"] = sol$solution[match(pheno_file$id, 
                                       sol$original_id)]

# we can check how the results correlate with the phenotype
# to have an indication of how well our model is fitting
# the function summarise() applies a function to all rows
# and return a single result
pheno_file %>%
   summarise(acc = cor(LW, ebv))
# = 0.441
# that's how much of the phenotype the ebvs explain
