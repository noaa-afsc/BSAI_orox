# BSAI Orox 2024 stock assessment
# Run REMA Model 22 update assessment
# Contact: jane.sullivan@noaa.gov
# Last updated: Oct 2024

# Remove black from colorblind theme FILL
scale_fill_colorblind7 = function(.ColorList = 2L:8L, ...){
  scale_fill_discrete(..., type = colorblind_pal()(8)[.ColorList])
}

# Remove black from colorblind theme Color
scale_color_colorblind7 = function(.ColorList = 2L:8L, ...){
  scale_color_discrete(..., type = colorblind_pal()(8)[.ColorList])
}

# set up ----

# assessment year
yr <- 2024

pkgs <- c("tidyverse", "here", "cowplot", "ggthemes")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema")
library(rema)

# folder set up
dir.create(here(yr, "results"))

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 15) +
                     cowplot::background_grid() +
                     cowplot::panel_border())
# new data ----

biomass_dat <- read_csv(here(yr, "data", "sppgroup_biomass_orox.csv")) %>% 
  rename(strata = survey)

sst_biomass_dat <- biomass_dat %>% 
  filter(group == 'SST') 

nonsst_biomass_dat <- biomass_dat %>% 
  filter(group == 'non-SST') 

nonsst_biomass_dat %>% print(n = Inf)
nonsst_biomass_dat %>% filter(biomass == 0)
nonsst_biomass_dat %>% filter(strata == 'EBS Shelf') %>% nrow()

cpue_dat <- read_csv(here(yr, "data", "lls_rpw_sst.csv")) %>% 
  filter(strata == 'EBS Slope')

# create alt data set that makes 2025 RPW = 2021 RPW: sensitivity run, what
# would the biomass estimate be if the 2025 RPW returns to 2021 values (i.e.,
# what if the 2023 survey was a fluke?)
cpue_alt_dat <- cpue_dat %>% 
  bind_rows(cpue_dat %>% 
              filter(year == 2021) %>% 
              mutate(year = 2025))

# M22 add LLS in EBS slope for SST ----
input <- prepare_rema_input(model_name = 'M22_2024',
                            biomass_dat = sst_biomass_dat,
                            sum_cpue_index = TRUE,
                            cpue_dat = cpue_dat,
                            end_year = yr+1,
                            multi_survey = TRUE,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)))

m22_sst <- fit_rema(input)
saveRDS(m22_sst, here::here(yr, "results", "m22_sst.rds"))
m22_sst_out <- tidy_rema(m22_sst)
# create empty placeholder so that strata plot panels are aligned for the
# BTS/LLS
m22_sst_out$cpue_by_strata <- m22_sst_out$cpue_by_strata %>% 
  bind_rows(expand_grid(strata = c("AI", "SBS"),
                       year = unique(m22_sst_out$cpue_by_strata$year)))
m22_sst_plots <- plot_rema(tidy_rema = m22_sst_out, biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')

p1 <- m22_sst_plots$biomass_by_strata + 
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
  labs(x = NULL, y = NULL, 
       title = 'Shortspine thornyhead (SST)',
       subtitle = 'Model fits to bottom trawl survey biomass (t) by region', y = NULL) 
p2 <- m22_sst_plots$cpue_by_strata +
  labs(subtitle = 'Model fit to the NMFS longline survey RPWs', y = NULL)
p3 <- m22_sst_plots$total_predicted_biomass +
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
  labs(subtitle = 'Total predicted biomass (t)', y = NULL)     

cowplot::plot_grid(p1, p2, p3, ncol = 1, 
                   rel_heights = c(0.4, 0.3, 0.3))

ggsave(here(yr, "results", "sst_m22.png"), units = 'in', bg = 'white',
       height = 8.5, width = 8, dpi = 300)

# M22 nonSST----
input <- prepare_rema_input(model_name = 'M22_2024',
                            biomass_dat = nonsst_biomass_dat,
                            end_year = yr+1,
                            zeros = list(assumptions = 'NA'),
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1)))
m22_nonsst <- fit_rema(input)
saveRDS(m22_nonsst, here::here(yr, "results", "m22_nonsst.rds"))
m22_nonsst_out <- tidy_rema(m22_nonsst)
m22_nonsst_plots <- plot_rema(m22_nonsst_out, biomass_ylab = "Biomass (t)")

p1 <- m22_nonsst_plots$biomass_by_strata +
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
  labs(x = NULL, y = NULL, 
       title = 'Other non-SST rockfish',
       subtitle = 'Model fits to bottom trawl survey biomass (t) by region') 
p2 <- m22_nonsst_plots$total_predicted_biomass +   
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
  labs(subtitle = 'Total predicted biomass (t)', y = NULL)

cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(0.7, 0.3))

ggsave(here(yr, "results", "nonsst_m22.png"), units = 'in', bg = 'white',
       height = 8, width = 7, dpi = 300)

# write output ----

m22_tot <- tidy_rema(m22_sst)$total_predicted_biomass %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(tidy_rema(m22_nonsst)$total_predicted_biomass %>% 
              mutate(species_group = 'non-SST')) %>% 
  select(species_group, model_name, year, pred, pred_lci, pred_uci) 

m22_tot %>%
  pivot_wider(id_cols = c(species_group, year), names_from = model_name, values_from = pred) %>% 
  bind_rows(m22_tot %>% 
              group_by(model_name, year) %>% 
              summarise(biomass = sum(pred)) %>% 
              pivot_wider(id_cols = year, names_from = model_name, values_from = biomass) %>% 
              arrange(year) %>% 
              mutate(species_group = 'Total')) %>% 
  write_csv(here(yr, "results", "tot_biomass.csv"))

m22_strata <- tidy_rema(m22_sst)$biomass_by_strata %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(tidy_rema(m22_nonsst)$biomass_by_strata %>% 
              mutate(species_group = 'non-SST')) %>% 
  select(species_group, model_name, strata, year, pred, pred_lci, pred_uci) 

unique(m22_strata$strata)
m22_strata %>% write_csv(here(yr, "results", "m22_pred_biomass_by_strata.csv"))
  
# total abc/ofl ---

natmat <- data.frame(species_group = c('SST', 'non-SST'),
                     M = c(0.03, 0.09))
spp_abc <- m22_tot %>% 
  filter(year == yr+1) %>% 
  dplyr::select(species_group, model_name, year, biomass = pred) %>% 
  left_join(natmat) %>% 
  mutate(OFL = biomass * M,
         maxABC = biomass * (0.75 * M)) 

spp_abc %>% write_csv(here(yr, "results", "abc_ofl_by_sppgroup.csv"))

sumtable <- spp_abc %>% 
  group_by(model_name, year) %>% 
  dplyr::summarise(biomass = sum(biomass),
            OFL = sum(OFL),
            maxABC = sum(maxABC)) 
sumtable %>% write_csv(here(yr, "results", "abc_ofl_summary.csv"))

# apportionment ----

m22_appo <- tidy_rema(m22_sst)$biomass_by_strata %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(tidy_rema(m22_nonsst)$biomass_by_strata %>% 
              mutate(species_group = 'non-SST')) %>% 
  select(species_group, model_name, strata, year, biomass = pred) 

appo <- m22_appo %>% 
  filter(year == yr) %>% 
  mutate(fmp = ifelse(strata == 'AI', 'AI', 'EBS')) 

apposum <- appo  %>% 
  group_by(species_group, model_name, fmp) %>% 
  summarise(biomass = sum(biomass)) %>% 
  left_join(natmat) %>% 
  mutate(species_fmp_ABC = biomass * (0.75 * M),
         species_fmp_OFL = biomass * M) %>% 
  group_by(model_name, species_group) %>% 
  mutate(total_biomass = sum(biomass),
         prop_biomass = biomass / total_biomass) 

apposum %>% write_csv(here(yr, "results", "abc_apportionment.csv"))  

# parameter estimates ----

m22_par <- tidy_rema(m22_sst)$parameter_estimates %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(tidy_rema(m22_nonsst)$parameter_estimates %>% 
              mutate(species_group = 'non-SST')) %>% 
  select(species_group, model_name, parameter, estimate, std_err, lci, uci) 

m22_par %>% write_csv(here(yr, "results", "sst_nonsst_parameters.csv"))

# LLS data summary tbl ----

read_csv(here(yr, "data", "lls_rpw_sst.csv")) %>% 
  mutate(cpue2 = ifelse(is.na(cpue), NA,
                        paste0(prettyNum(cpue, big.mark = ',', digits = 1, trim = TRUE),
                               " (", prettyNum(round(cv, 2), digits = 2), ")"))) %>%
  pivot_wider(id_cols = year, names_from = strata, values_from = cpue2) %>% 
  arrange(year) %>% 
  write_csv(here(yr, "results", "rpws.csv"))

# BTS data summary tbl ----

data.frame(year = 1982:yr) %>% 
  left_join(nonsst_biomass_dat %>% 
              mutate(biomass2 = ifelse(is.na(biomass), NA,
                                       paste0(prettyNum(biomass, big.mark = ',', digits = 1, trim = TRUE),
                                              " (", prettyNum(round(cv, 2), digits = 2), ")"))) %>%
              pivot_wider(id_cols = year, names_from = strata, values_from = biomass2) %>% 
              arrange(year)) %>%
  write_csv(here(yr, "results", "nonsst_bts_biomass.csv"))

data.frame(year = 1982:yr) %>% 
  left_join(sst_biomass_dat %>% 
              mutate(biomass2 = ifelse(is.na(biomass), NA,
                                       paste0(prettyNum(biomass, big.mark = ',', digits = 1, trim = TRUE),
                                              " (", prettyNum(round(cv, 2), digits = 2), ")"))) %>%
              pivot_wider(id_cols = year, names_from = strata, values_from = biomass2) %>% 
              arrange(year)) %>% 
  write_csv(here(yr, "results", "sst_bts_biomass.csv"))

# Presentation total biomass ----
ggplot(data = m22_tot %>% 
         filter(year >= 2002 & model_name == 'M22_2024') %>% 
         mutate(species_group = factor(species_group, levels = c("SST", "non-SST"), ordered = TRUE))) +
  geom_line(aes(x = year, y = pred, col = species_group),
            size = 1) +
  geom_ribbon(aes(x = year, ymin = pred_lci, ymax = pred_uci, fill = species_group),
              alpha = 0.3, col = "white") +
  scale_color_manual(values = c("#6ba292", "#f0b74a")) +
  scale_fill_manual(values = c("#6ba292", "#f0b74a")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL, title = "Exploitable biomass (t)",
       col = NULL, fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(here(yr, "results", "pres_biom.png"), 
       dpi=300, height=3, width=4.5, units="in")

# Presentation catch ----

catch <- read_csv(here(yr, "data", "bsai_orox_catch_confidential.csv"))

ggplot(data = catch %>%
         mutate(species = ifelse(species_name == "SST", "SST", "non-SST")) %>%
         group_by(year, species) %>%
         summarize(Catch = sum(tons, na.rm = TRUE)) %>% 
         mutate(species = factor(species, levels = c("SST", "non-SST"), ordered = TRUE))) +
  geom_area(aes(x = year, y = Catch, fill = species),
            alpha = 0.5 , size = 0.5, colour = "white") +
  scale_fill_manual(values = c("#6ba292", "#f0b74a")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL, title = "Catch (t)", fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

ggsave(here(yr, "results", "pres_catch.png"), 
       dpi=300, height=3, width=4.5, units="in")

# catch to biomass table ----
m22_strata %>% 
  mutate(fmp = ifelse(strata == "AI", "AI", "EBS"), .before = strata) %>% 
  group_by(model_name, species_group, fmp, year) %>% 
  summarise(pred = sum(pred)) %>% 
  left_join(catch %>%
              mutate(species_group = ifelse(species_name == "SST", "SST", "non-SST")) %>%
              group_by(year, fmp = fmp_subarea, species_group) %>%
              summarize(catch = sum(tons))) %>% 
  filter(year >= 2003) %>% 
  mutate(catch_to_biomass = catch / pred) %>% 
  write_csv(here(yr, "results", "catch_to_predicted_biomass.csv"))

# Plot Catch/ABC/OFL  ----

# by FMP and species
apposum %>% 
  ungroup() %>%
  filter(model_name == 'M22_2024') %>% 
  dplyr::select(species = species_group, FMP = fmp, `ABC*` = species_fmp_ABC,
         `OFL*` = species_fmp_OFL) %>%
  left_join(catch %>%
              mutate(species = ifelse(species_name == "SST", "SST", "non-SST")) %>%
              group_by(year, FMP = fmp_subarea, species) %>%
              summarize(Catch = sum(tons))) %>% 
  pivot_longer(cols = c("ABC*", "OFL*", "Catch")) %>% 
  mutate(name = factor(name, levels = c("Catch", "ABC*", "OFL*"), ordered = TRUE),
         species = factor(species, levels = c("SST", "non-SST"), ordered = TRUE)) %>% #View()
  ggplot(aes(x = year, y = value, col = name, lty = name, size = name)) + #
  geom_line() +
  facet_grid(species ~ FMP, scales = "free") +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_grey() +
  scale_size_manual(values = c(.5, 1, 1)) +
  labs(x = NULL, y = "Catch (t)", col = NULL, size = NULL, lty = NULL) +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))

ggsave(here(yr, "results", "catch_abc_ofl_fmp.png"), 
       dpi=300, height=5, width=7, units="in")

# Catch/ABC/OFL by species
spp_abc %>%
  filter(model_name == 'M22_2024') %>% 
  dplyr::select(species = species_group, ABC = maxABC, OFL) %>%
  left_join(catch %>%
              mutate(species = ifelse(species_name == "SST", "SST", "non-SST")) %>%
              group_by(year, species) %>%
              summarize(Catch = sum(tons))) %>% 
  pivot_longer(cols = c("ABC", "OFL", "Catch")) %>% 
  mutate(name = factor(name, levels = c("Catch", "ABC", "OFL"), ordered = TRUE)) %>% 
  ggplot(aes(x = year, y = value, col = name, lty = name, size = name)) + #
  geom_line() +
  facet_wrap(~ species, scales = "free", ncol = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_grey() +
  scale_size_manual(values = c(.5, 1, 1)) +
  labs(x = NULL, y = "Catch (t)", col = NULL, size = NULL, lty = NULL) +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))

ggsave(here(yr, "results", "catch_abc_ofl_complex.png"), 
       dpi=300, height=5, width=6, units="in")

# Percent SST catch over time:
catch %>%
  mutate(species = ifelse(species_name == "SST", "SST", "non-SST")) %>%
  group_by(year, species) %>%
  summarize(Catch = sum(tons)) %>%
  group_by(year) %>% 
  mutate(Total_Catch = sum(Catch),
         percent = 100 * Catch / Total_Catch) %>% 
  filter(species == "SST" & year < yr) %>% 
  ungroup() %>% 
  mutate(mean_percent = mean(percent),
         min_percent = min(percent),
         max_percent = max(percent)) %>% 
  print(n=Inf)
# SST makes up 44% of the catch on avg since 2003 (mod catch timeseries), 53% in
# 2023 (highest since 2019)

m22_tot %>% 
  group_by(year) %>% 
  mutate(total_biom = sum(pred),
         percent = 100 * pred / total_biom) %>% 
  filter(species_group == "SST" & year < yr & year >= 2003) %>% 
  ungroup() %>% 
  mutate(mean_percent = mean(percent),
         min_percent = min(percent),
         max_percent = max(percent)) %>% print(n=Inf)
# SST is 96% of the total biomass on avg since 2003 (mod catch timeseries), estimated
# to be 94% of total biomass in 2023

m22_strata %>% 
  group_by(year) %>% 
  mutate(total_biom = sum(pred),
         percent = 100 * pred / total_biom) %>% 
  filter(species_group == "SST" & year < yr & year >= 2003) %>% 
  group_by(strata) %>% 
  mutate(mean_percent = mean(percent)) %>% 
  ungroup() %>% 
  mutate(min_percent = min(percent),
         max_percent = max(percent)) %>% print(n=Inf)

# Total Catch/ABC/OFL
catchlimit_sum <- spp_abc %>%
  filter(model_name == 'M22_2024') %>% 
  dplyr::select(species = species_group, ABC = maxABC, OFL) %>%
  ungroup() %>% 
  distinct() %>% 
  summarize(species = "All Other Rockfish",
            ABC = sum(ABC),
            OFL = sum(OFL)) %>% 
  left_join(catch %>%
              mutate(species = "All Other Rockfish") %>% 
              group_by(year, species) %>%
              summarize(Catch = sum(tons))) %>% 
  pivot_longer(cols = c("ABC", "OFL", "Catch")) %>% 
  mutate(name = factor(name, 
                       levels = c("Catch", "ABC", "OFL"), 
                       labels = c("Catch", "2025/26 ABC", "2025/26 OFL"),
                       ordered = TRUE)) 

catchlimit_sum %>% 
  ggplot(aes(x = year, y = value, col = name, lty = name, size = name)) + #
  geom_line() +
  geom_point(data = catchlimit_sum %>% filter(name == "Catch"),
             aes(x = year, y = value), size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_grey() +
  scale_size_manual(values = c(.5, 1, 1)) +
  labs(x = NULL, y = NULL, title = "BSAI Other Rockfish",
       subtitle = "Comparison of catch to ABC/OFL (t)", 
       col = NULL, size = NULL, lty = NULL) +
  expand_limits(y = 0) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        legend.position = c(0.8, 0.2))

ggsave(here(yr, "results", "total_catch_abc_ofl.png"), 
       dpi=300, height=4, width=6, units="in")

# Catch to biomass ratio ----

refs <- data.frame(species = c("non-SST", "SST"),
                   yint = c(1, NA)) %>% 
  mutate(species = factor(species, 
                          levels = c("SST", "non-SST"), 
                          ordered = TRUE))
ratio <- apposum %>% 
  ungroup() %>%
  filter(model_name == 'M22_2024') %>% 
  dplyr::select(species = species_group, FMP = fmp, F_OFL = M, ) %>% 
  left_join(catch %>%
              mutate(species = ifelse(species_name == "SST", "SST", "non-SST")) %>%
              group_by(year, FMP = fmp_subarea, species) %>%
              summarize(Catch = sum(tons))) %>% 
  left_join(m22_appo %>% 
              mutate(FMP = ifelse(strata == 'AI', 'AI', 'EBS')) %>% 
              dplyr::select(species = species_group, year, FMP, biomass) %>%
              group_by(species, year, FMP) %>% 
              dplyr::summarise(biomass = sum(biomass))) %>% 
  mutate(ratio = Catch / biomass,
         species = factor(species, 
                          levels = c("SST", "non-SST"), 
                          ordered = TRUE)) 
ratio %>% 
  ggplot(aes(x = year, y = ratio, col = FMP, lty = FMP)) + #
  geom_line() +
  facet_wrap(~ species, scales = "free_y") +
  scale_colour_manual(values = c("grey", "black")) +
  scale_linetype_manual(values = c(1, 5)) +
  geom_hline(data = refs, aes(yintercept = yint),
             col = "red", lty = 3) +
  scale_y_continuous(expand = c(0.01, 0), limits = c(0, NA)) +
  labs(x = NULL, y = "Catch/biomass", col = NULL, size = NULL, lty = NULL) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))

ggsave(here(yr, "results", "ratio_catch_biomass.png"), 
       dpi=300, height=3.75, width=6, units="in")

ratio %>% 
  group_by(FMP, species) %>% 
  summarize(mean_ratio = mean(ratio))

ratio %>% filter(year == yr & species == "non-SST" & FMP == "AI")

ratio %>% 
  group_by(species, year) %>% 
  summarise(ratio = (sum(Catch) / sum(biomass))*100) %>% 
  # filter(year >= yr-3) 
  print(n=Inf)

# Presentation catch/biomass----

ratio %>% 
  mutate(FMP = factor(FMP, 
                      labels = c("Aleutian Islands", "Eastern Bering Sea"),
                      levels = c("AI", "EBS"), 
                      ordered = TRUE),
         species = factor(species, 
                          levels = c("SST", "non-SST"), 
                          ordered = TRUE)) %>% 
  ggplot(aes(x = year, y = ratio, col = species, lty = FMP)) +
  geom_line(size = 1) +
  labs(x = NULL, y = NULL, title = "Exploitation rate (catch/biomass)",
       # subtitle = "Note differences in y-axis scale",
       col = NULL, fill = NULL, lty = NULL) +
  geom_hline(data = refs, aes(yintercept = yint),
             col = "red", lty = 3) +
  scale_color_manual(values = c("#6ba292", "#f0b74a")) + #, guide = 'none') +
  theme_minimal(base_size = 13) +
  theme(#legend.position = "top",
        legend.position = c(.8, .8),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

ggsave(here(yr, "results", "pres_ratio.png"), 
       dpi=300, height=4, width=7, units="in")

# detailed non-SST ----

CUSTOMpalette <- c("magenta", "#1a2fff", "#0d177d", "skyblue", "#fa751a", 
                   "#00BFC4", "limegreen", "#fae51a", "#c3b104", 
                   "darkred",  "purple", "darkgreen", "black")

nonsst_eda <- read_csv(here(yr, "data", "raw", "biomass_orox.csv")) %>% #distinct(area_name)
  mutate(survey = ifelse(area_name %in% c("Southern Bering Sea"), 'SBS', survey)) %>% 
  filter(group != "SST") %>% 
  # combine dusky/dark and dusky
  mutate(common_name = ifelse(common_name == "dusky and dark rockfishes unid.",
                          "dusky and dark rockfish", common_name)) %>% 
  mutate(common_name = ifelse(common_name == "dusky rockfish",
                          "dusky and dark rockfish", common_name)) %>% 
  # combine broadfin and longspine thornyhead
  mutate(common_name = ifelse(common_name %in% c("broadfin thornyhead", "longspine thornyhead", "thornyhead unid."),
                          "other thornyhead", common_name)) %>% 
  group_by(common_name, survey, year) %>% 
  summarize(biomass = sum(biomass),
            var = sum(var)) %>% 
  ungroup() %>% 
  # complete(common_name, survey, year, fill = list(biomass = 0, var = 0)) %>% 
  mutate(survey = factor(survey, levels = c("AI", "EBS Shelf", "SBS", "EBS Slope"), 
                         ordered = TRUE))
         
ggplot(data = nonsst_eda, aes(x = year, y = biomass, 
                              fill = common_name)) +
  geom_bar(stat = "identity", alpha = 0.7, 
           position = position_stack(reverse = TRUE),
           size = 0.3,
           width = 1,
           colour = "black") +
  # ggthemes::scale_fill_colorblind() +
  scale_fill_manual(values = CUSTOMpalette) +
  # scale_fill_grey(start = 1, end = 0) +
  facet_wrap(~survey, scales = "free_y", ncol = 1) +
  # theme_minimal() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Biomass (t)", fill = "Non-SST species") +
  guides(fill = guide_legend(nrow = 3))

ggsave(here(yr, "results", "srvbiom_nonSST_spp.png"), 
       dpi=400, height=8, width=8, units="in")

# compare rema ----

# compare rema models for this year and lasyr

m22_sst_past <- readRDS(here::here(yr-2, "results", "m22_sst.RDS"))
m22_sst <- readRDS(here::here(yr, "results", "m22_sst.RDS"))

compare <- compare_rema_models(rema_models = list(m22_sst, m22_sst_past),
                               biomass_ylab = "Biomass (t)", cpue_ylab = "RPW")
p1 <- compare$plots$biomass_by_strata +
  facet_wrap(~strata, nrow = 1, scales = 'free_y') +
  scale_y_continuous(labels = scales::comma, expand = c(0.0, 0), limits = c(0, NA)) +
  labs(x = NULL, y = NULL, 
       title = 'Shortspine thornyhead (SST)',
       subtitle = 'Model fits to bottom trawl survey biomass (t) by region', y = NULL) +
  scale_color_colorblind7() +
  scale_fill_colorblind7() 
p2 <- compare$plots$cpue_by_strata +
  labs(subtitle = 'Model fit to the NMFS longline survey RPWs', y = NULL) +
  scale_color_colorblind7() +
  scale_fill_colorblind7() 
p3 <- compare$plots$total_predicted_biomass +
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
  labs(subtitle = 'Total predicted biomass (t)', y = NULL) +
  scale_color_colorblind7() +
  scale_fill_colorblind7() 

cowplot::plot_grid(p1, p2, p3, ncol = 1, rel_heights = c(0.4, 0.3, 0.3))
ggsave(here(yr, "results", "compare_last_assess_m22_sst.png"), units = 'in', bg = 'white',
       height = 9, width = 8.5, dpi = 300)

m22_nonsst_past <- readRDS(here::here(yr-2, "results", "m22_nonsst.RDS"))
m22_nonsst <- readRDS(here::here(yr, "results", "m22_nonsst.RDS"))

compare2 <- compare_rema_models(rema_models = list(m22_nonsst, m22_nonsst_past),
                               biomass_ylab = "Biomass (t)")
p1 <- compare2$plots$biomass_by_strata +
  facet_wrap(~strata, scales = 'free_y') +
  scale_y_continuous(labels = scales::comma, expand = c(0.0, 0), limits = c(0, NA)) +
  labs(x = NULL, y = NULL, 
       title = 'Other non-SST rockfish',
       subtitle = 'Model fits to bottom trawl survey biomass (t) by region', y = NULL) +
  scale_color_colorblind7() +
  scale_fill_colorblind7() 
p3 <- compare2$plots$total_predicted_biomass +
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
  labs(subtitle = 'Total predicted biomass (t)', y = NULL) +
  scale_color_colorblind7() +
  scale_fill_colorblind7() 

cowplot::plot_grid(p1, p3, ncol = 1, rel_heights = c(0.4, 0.3, 0.3))
ggsave(here(yr, "results", "compare_last_assess_m22_nonsst.png"), units = 'in', bg = 'white',
       height = 9, width = 8.5, dpi = 300)

compare$output$total_predicted_biomass %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(compare2$output$total_predicted_biomass %>% 
              mutate(species_group = 'non-SST')) %>% 
  group_by(model_name, year) %>% 
  summarise(pred = sum(pred)) %>%
  pivot_wider(id_cols = year, values_from = pred, names_from = model_name) %>% 
  filter(year >= 1991) %>% 
  write_csv(here(yr, "results", "compare_last_assess_total_biomass.csv"))

compare$output$total_predicted_biomass %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(compare2$output$total_predicted_biomass %>% 
              mutate(species_group = 'non-SST')) %>% 
  group_by(model_name, year, species_group) %>%
  summarise(pred = sum(pred)) %>%
  ungroup() %>% 
  pivot_wider(id_cols = c(year, species_group), values_from = pred, names_from = model_name) %>% 
  filter(year >= 1991) %>% 
  arrange(species_group) %>% 
  write_csv(here(yr, "results", "compare_last_assess_spp_group_total_biomass.csv"))

# group_by(model_name, year, species_group, .drop = TRUE) %>% 
# mutate(tot = sum(pred)) %>% 
#   pivot_longer(cols = tot, names_to = species_group,)

# Sensitivity -----

# What if the LLS RPW goes back up to the 2021 value in 2025 for the 2026 assessment?

cpue_dat_sens <- cpue_dat %>% 
  bind_rows(cpue_dat %>% 
              filter(year == 2021) %>% 
              mutate(year = 2025)) %>% 
  arrange(year)
input <- prepare_rema_input(model_name = 'M22_2026_sens',
                            biomass_dat = sst_biomass_dat,
                            sum_cpue_index = TRUE,
                            cpue_dat = cpue_dat_sens,
                            end_year = 2026,
                            multi_survey = TRUE,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)))

m22_sst_s <- fit_rema(input)
saveRDS(m22_sst_s, here::here(yr, "results", "m22_sst_sensitivity.rds"))
m22_sst_sout <- tidy_rema(m22_sst_s)

compare <- compare_rema_models(rema_models = list(m22_sst, m22_sst_past, m22_sst_s),
                               biomass_ylab = "Biomass (t)", cpue_ylab = "RPW")
p1 <- compare$plots$biomass_by_strata +
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
  labs(x = NULL, y = NULL, 
       title = 'Shortspine thornyhead (SST)',
       subtitle = 'Model fits to bottom trawl survey biomass (t) by region', y = NULL) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() 
p2 <- compare$plots$cpue_by_strata +
  labs(subtitle = 'Model fit to the NMFS longline survey RPWs', y = NULL) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() 
p3 <- compare$plots$total_predicted_biomass +
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
  labs(subtitle = 'Total predicted biomass (t)', y = NULL) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() 

cowplot::plot_grid(p1, p2, p3, ncol = 1, rel_heights = c(0.4, 0.3, 0.3))
ggsave(here(yr, "results", "compare_last_assess_with_sensitivity_m22_sst.png"), units = 'in', bg = 'white',
       height = 8.5, width = 8, dpi = 300)

# write output

tot <- compare$output$total_predicted_biomass %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(tidy_rema(m22_nonsst)$total_predicted_biomass %>% 
              mutate(species_group = 'non-SST')) %>% 
  select(species_group, model_name, year, pred, pred_lci, pred_uci) 

tot %>%
  pivot_wider(id_cols = c(species_group, year), names_from = model_name, values_from = pred) %>% 
  bind_rows(tot %>% 
              group_by(model_name, year) %>% 
              summarise(biomass = sum(pred)) %>% 
              pivot_wider(id_cols = year, names_from = model_name, values_from = biomass) %>% 
              arrange(year) %>% 
              mutate(species_group = 'Total')) %>% 
  write_csv(here(yr, "results", "tot_biomass_sensitivity.csv"))

# total abc/ofl ---

natmat <- data.frame(species_group = c('SST', 'non-SST'),
                     M = c(0.03, 0.09))
spp_abc_s <- tot %>% 
  filter(year == yr+1) %>%
  dplyr::select(species_group, model_name, year, biomass = pred) %>% 
  left_join(natmat) %>% 
  mutate(OFL = biomass * M,
         maxABC = biomass * (0.75 * M)) 

spp_abc_s %>% write_csv(here(yr, "results", "abc_ofl_by_sppgroup_sensitivity.csv"))

sumtable_s <- spp_abc_s %>% 
  group_by(model_name, year) %>% 
  dplyr::summarise(biomass = sum(biomass),
                   OFL = sum(OFL),
                   maxABC = sum(maxABC)) 
sumtable_s %>% write_csv(here(yr, "results", "abc_ofl_summary_sensitivity.csv"))

# apportionment

m22_appo_s <- compare$output$biomass_by_strata %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(tidy_rema(m22_nonsst)$biomass_by_strata %>% 
              mutate(species_group = 'non-SST')) %>% 
  select(species_group, model_name, strata, year, biomass = pred) 

appo_s <- m22_appo_s %>% 
  filter(year == yr) %>% 
  mutate(fmp = ifelse(strata == 'AI', 'AI', 'EBS')) 

apposum_s <- appo_s %>% 
  group_by(species_group, model_name, fmp) %>% 
  summarise(biomass = sum(biomass)) %>% 
  left_join(natmat) %>% 
  mutate(species_fmp_ABC = biomass * (0.75 * M),
         species_fmp_OFL = biomass * M) %>% 
  group_by(model_name, species_group) %>% 
  mutate(total_biomass = sum(biomass),
         prop_biomass = biomass / total_biomass) 

apposum_s %>% write_csv(here(yr, "results", "abc_apportionment_sensitivity.csv"))  

# parameter estimates ----

m22_par_s <- compare$out$parameter_estimates %>% 
  mutate(species_group = 'SST') %>% 
  bind_rows(tidy_rema(m22_nonsst)$parameter_estimates %>% 
              mutate(species_group = 'non-SST')) %>% 
  select(species_group, model_name, parameter, estimate, std_err, lci, uci) 

m22_par_s %>% write_csv(here(yr, "results", "sst_nonsst_parameters_sensitivity.csv"))

# SARA ----

# These are the 2022 current values:
#CATCH_INPUT_DATA options: 0 - None, 1 - Major gaps preclude use, 2 - Major gaps
#in some sectors(s), 3 - Minor gaps across sectors, 4 - Minor gaps in some
#sector(s), 5 - Near complete knowledge)
# 2 # I updated this to a 3
#ABUNDANCE_INPUT_DATA 0 - None, 1 - Uncertain or expert opinion, 2 -
#Standardized fishery-dependent, 3 - Limited fishery-independent, 4 -
#Comprehensive fishery-independent, 5 - Absolute abundance
# 3 # I think this is correct
#BIOLOGICAL_INPUT_DATA 0 - None, 1 - Proxy-based, 2 - Empirical and proxy-based,
#3 - Mostly empirical estimates, 4 - Track changes over time, 5 - Comprehensive
#over time and space
# 3 # I updated this to a 2

# base weighted M on ratio of biomass estimates between SST/non-SST in terminal
# year (same as projection year)
sara_pred <- m22_tot %>% 
  filter(year >= 1991) %>% 
  pivot_wider(id_cols = year, names_from = species_group, values_from = pred) %>% 
  mutate(total = SST + `non-SST`,
         p_SST = SST / total,
         SST_M = 0.03,
         nonSST_M = 0.09,
         weighted_M = (SST_M * p_SST) + (nonSST_M * (1-p_SST))) 

write_csv(sara_pred, here(yr, "results", "sara_biomass_pred.csv"))

weighted_M <- sara_pred %>% 
  # FLAG, check yr
  filter(year == yr+1) %>% 
  pull(weighted_M)
  
weighted_M

# previously the SARA biomass estimates combine biomass, re-run model. this is
# how it was done previously, sticking with status quo

# based on combined ... TO DO...
sara_biomass_dat <- biomass_dat %>% 
  mutate(area = ifelse(area == 'SBS', 'AI', area)) %>% 
  group_by(year, area) %>% 
  dplyr::summarise(biomass = sum(biomass),
                   var = sum(var)) %>% 
  mutate(cv = sqrt(var) / biomass) %>% 
  select(-var)

# sara_biomass_dat <- sara_biomass_dat %>% 
#   filter(year >= 1991) %>% 
#   pivot_wider(id_cols = year, names_from = area, values_from = biomass,
#               values_fill = NA)
# 
# cpue_dat %>% 
#   mutate(area = 'LLS') %>% 
#   select(-strata, -cv, biomass = cpue)

sara_biomass_dat <- sara_biomass_dat %>% 
  filter(year >= 1991) %>% 
  pivot_wider(id_cols = area, names_from = year, values_from = biomass,
              values_fill = NA)

write_csv(sara_biomass_dat, here(yr, "results", "sara_survey_biomass_data.csv"))

sara_exploitation <- catch %>% 
  group_by(year) %>% 
  dplyr::summarise(catch = sum(tons)) %>% 
  left_join(sara_pred %>% 
              select(year, biomass = total)) %>% 
  mutate(exploitation = catch / biomass)
  
write_csv(sara_exploitation, paste0(out_path, '/sara_exploitation_rates.csv'))


