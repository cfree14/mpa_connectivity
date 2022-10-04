

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/cpfv_logbooks/processed/"
outdir <- "data"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "CDFW_2000_2020_cpfv_logbook_data.Rds"))


# Build data
################################################################################

# Target species
target_species <- c("Lingcod",
                    "Other",
                    "Rockfish",
                    "Salmon",
                    "Sharks",
                    "Striped bass",
                    "Sturgeon",
                    "Tuna",
                    "Potluck",
                    "Misc. coastal",
                    "Misc. bay",
                    "Misc. offshore")

# Loop through and plot
i <- 1
for(i in 1:length(target_species)){
  
  # Target species
  targ_spp <- target_species[i]
  
  # Build data
  data <- data_orig %>%  
    # Filter to trips targeting a species
    filter(grepl(targ_spp, target_species)) %>% 
    # Count
    group_by(comm_name, sci_name) %>% 
    summarize(nfish=sum(n_kept, na.rm=T)) %>% 
    ungroup() %>% 
    # Arrange
    arrange(desc(nfish)) %>% 
    slice(1:40) %>% 
    mutate(comm_name=factor(comm_name, levels=comm_name))
  
  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     plot.title=element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot
  g <- ggplot(data, aes(x=nfish/1e6, y=comm_name)) +
    geom_bar(stat="identity") +
    # Labels
    labs(x="Millions of fish retained (2000-2020)", y="", title=paste("On CPFV trips targetting:", targ_spp)) +
    # Theme
    theme_bw() +  my_theme
  g
  
  # Export
  ggsave(g, filename=file.path(plotdir, paste("FigX_species_caught_when_targeting_", targ_spp, ".png")), 
         width=6.5, height=4.5, units="in", dpi=600)

  
}









