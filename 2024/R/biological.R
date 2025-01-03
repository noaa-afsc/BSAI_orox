# Biological data (length comps) for SST only (and only fishery and LLS)
# Contact: jane.sullivan@noaa.gov
# Last updated: Oct 2024

# devtools::session_info()
# R version 4.3.2 (2023-10-31 ucrt)

# 2024 was an update assessment and wasn't supposed to include length comps but
# Ivonne O requested them due to the large drop in LLS and warm temps on EBS shelf


# Set up ----

# Assessment year (most recent year with complete data set)
yr <- 2024

pkgs <- c("tidyverse", "ggridges", "here", "cowplot", "ggthemes")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# Data ----

lengths <- read_csv(here(yr, "data", "length_frequencies_slope_sst.csv"))
fshlen <- read_csv(here(yr, "data", "fshlen.csv"))
fshlen %>% 
  filter()
# Get length comps ----

comps <- lengths %>% 
  group_by(source, fmp_subarea, species_code, year, length) %>% 
  summarize(n = sum(frequency)) %>% 
  mutate(prop = n / sum(n),
         common_name = 'Shortspine thornyhead') %>% 
  # left_join(spp %>% select(species_code, common_name)) %>% 
  group_by(source, fmp_subarea, species_code, year) %>% 
  # filter(sum(n) > 50) %>% 
  mutate(N = paste0("N = ", sum(n))) %>% 
  ungroup()

# Comp test: should all be 1!
comps %>% group_by(source, fmp_subarea, species_code, year) %>% summarize(tst = sum(prop)) %>% pull(tst) 
comps %>% group_by(source, fmp_subarea, species_code, year) %>% summarize(tst = sum(prop)) %>% print(n=Inf)
unique(comps$source)
comps %>% filter(source == 'EBS slope BTS')


# scale_fill_manual(values = c("grey30", "#00BFC4", "#daa520", "#da2055")) +
  
# Plot SST EBS ----
sstyrs <- expand.grid(source = factor(c('EBS LLS')),#, 'EBS fishery'
                      year = 1996:yr)
p2 <- ggplot(data = sstyrs %>% 
               left_join(comps %>% 
                           filter(year >= 1997 & species_code == 30020 & between(length, 10, 70)) %>% #View() 
                           filter(source %in% c('EBS LLS') & #'EBS fishery', 'AI BTS', , 'EBS slope BTS') & 
                                    fmp_subarea %in% c('EBS')) %>% #, 'SBS'
                           # mutate(source = ifelse(source == 'AI BTS', 'AI BTS in SBS', source)) %>% 
                           mutate(source = factor(source, levels = c('EBS LLS')))), #'AI BTS in SBS', ,'EBS slope BTS')))),, 'EBS fishery'
             aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "white", alpha = 0.4, #alpha = 0.8
                      panel_scaling = TRUE, size = 0.5) +
  scale_fill_manual(values = c( "#00BFC4", "#daa520")) + #, "#da2055")) +"grey30",
  # geom_hline(yintercept = factor(2002:YEAR), col = "lightgrey") +
  labs(x = "Length (cm)", y = NULL, fill = NULL, title = "EBS shortspine thornyhead") + #) +
  theme_light() +
  # facet_wrap(~ fmp_subarea) +
  theme(legend.position = "top",
        strip.background = element_blank())
p2
ggsave(here(yr, "results", "lencomps_sst_lls.png"), 
       dpi=300, height=7, width=5, units="in")



# Data ----

lengths <- read_csv(here(yr, "data", "length_frequencies.csv"))  

# Get length comps ----

comps <- lengths %>% 
  group_by(source, fmp_subarea, species_code, year, length) %>% 
  summarize(n = sum(frequency)) %>% 
  mutate(prop = n / sum(n),
         common_name = 'Shortspine thornyhead') %>% 
  # left_join(spp %>% select(species_code, common_name)) %>% 
  group_by(source, fmp_subarea, species_code, year) %>% 
  # filter(sum(n) > 50) %>% 
  mutate(N = paste0("N = ", sum(n))) %>% 
  ungroup()

# Comp test: should all be 1!
comps %>% group_by(source, fmp_subarea, species_code, year) %>% summarize(tst = sum(prop)) %>% pull(tst) 
comps %>% group_by(source, fmp_subarea, species_code, year) %>% summarize(tst = sum(prop)) %>% print(n=Inf)
unique(comps$source)
comps %>% distinct(source, species_code)
# scale_fill_manual(values = c("grey30", "#00BFC4", "#daa520", "#da2055")) +
  
# Plot SST EBS ----
sstyrs <- expand.grid(source = factor(c('EBS LLS','EBS fishery','AI BTS')),#, ,
                      year = 1996:yr)
p2 <- ggplot(data = sstyrs %>% 
               left_join(comps %>% 
                           filter(year >= 1997 & species_code == 30020 & between(length, 10, 70)) %>% #View() 
                           filter(source %in% c('EBS LLS', 'EBS fishery','AI BTS') & 
                                    fmp_subarea %in% c('EBS'))) %>% 
                           mutate(source = ifelse(source == 'AI BTS', 'AI BTS in SBS', source)),# %>%
                           # mutate(source = factor(source, levels = c('EBS LLS')))), #'AI BTS in SBS', ,'EBS slope BTS')))),, 'EBS fishery'
             aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "white", alpha = 0.4, #alpha = 0.8
                      panel_scaling = TRUE, size = 0.5) +
  scale_fill_manual(values = c( "#daa520","#00BFC4","#da2055")) + #, )) +"grey30", 
  # geom_hline(yintercept = factor(2002:YEAR), col = "lightgrey") +
  labs(x = "Length (cm)", y = NULL, fill = NULL) + #) +
  theme_light() +
  # facet_wrap(~ fmp_subarea) +
  theme(legend.position = "top",
        strip.background = element_blank())
p2
ggsave(here(yr, "results", "lencomps_sst_lls_fishery.png"), 
       dpi=300, height=7, width=5, units="in")

# Plot SST EBS ----
sstyrs <- expand.grid(source = factor(c('EBS LLS','EBS fishery')),#, ,,'AI BTS'
                      year = 1996:yr)
p2 <- ggplot(data = sstyrs %>% 
               left_join(comps %>% 
                           filter(year >= 1997 & species_code == 30020 & between(length, 10, 70)) %>% #View() 
                           filter(source %in% c('EBS LLS', 'EBS fishery') & #,'AI BTS'
                                    fmp_subarea %in% c('EBS'))) %>% 
                           mutate(source = ifelse(source == 'AI BTS', 'AI BTS in SBS', source)),# %>%
             aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "white", alpha = 0.4, #alpha = 0.8
                      panel_scaling = TRUE, size = 0.5) +
  scale_fill_manual(values = c( "#daa520","#00BFC4","#da2055")) + #, )) +"grey30", 
  labs(x = "Length (cm)", y = NULL, fill = NULL) + #) +
  theme_light() +
  theme(legend.position = "top",
        strip.background = element_blank())
p2
ggsave(here(yr, "results", "lencomps_sst_lls_fishery.png"), 
       dpi=300, height=7, width=5, units="in")

