

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/cpfv_logbooks/processed/"
outdir <- "data"

# Read data
data_orig <- readRDS(file=file.path(datadir, "CDWF_2000_2020_cpfv_logbook_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Calculate angler hours
  mutate(angler_hours=n_fishers * hrs_fished) %>% 
  # Summarize
  group_by(port_complex, port, block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nlogbooks=n_distinct(logbook_id),
            nfishers=sum(n_fishers, na.rm=T),
            nfisherhours=sum(angler_hours, na.rm=T),
            nretained=sum(n_kept, na.rm = T),
            ndiscarded=sum(n_released, na.rm=T),
            ncaught=nretained+ndiscarded) %>% 
  ungroup() %>% 
  # Rule of three
  filter(nvessels>=3)

# Export data
saveRDS(data, file=file.path(outdir, "2000_2020_CPFV_effort_by_port_and_block.Rds"))


