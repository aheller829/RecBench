# Reading in table from afmss database to get list of BLM field offices with O&G development
# Load packages
library(dplyr)

# Read in csv
fedapds <- read.csv("fedAPDs.csv")
# Filter and clean
fedapds <- dplyr::filter(fedapds, Well.Name != "" | Well.Name != " ")
# Remove total lines
fedapds <- fedapds[!grepl("Total", fedapds$Field.Office), ]
# Make list of unique field offices
apd_fos <- as.data.frame(unique(fedapds$Field.Office))
# Remove blank row
apd_fos <- apd_fos[-c(1), ]
# As dataframe
apd_fos <- as.data.frame(apd_fos)


# Read in BLM field office shapefile
blm <- read.csv("OG_FOs.csv")
# Subset to fedapd fo's
# First make names all upper
apd_fos$apd_fos <- toupper(apd_fos$apd_fos)
blm_ss <- subset(blm, blm$Administrative.Unit.Code %in% apd_fos$apd_fos)
# Which are missing?
fos_missing <- subset(apd_fos, !(apd_fos %in% blm_ss$Administrative.Unit.Code))

# Write to csv
write.csv(blm_ss, "fos_edited.csv")
