# low SST biomass in the EBS slope exploratory data analysis

# set up -----

# assessment year
yr <- 2024

pkgs <- c("tidyverse", "here", "cowplot", "ggthemes")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# folder set up
dir.create(here(yr, "results"))

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 10) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# data ----

llscatch <- read_csv(here(yr, "data", "sst_catch_lls.csv")) %>% 
  mutate(fishing_event_id = paste0(cruise_number, '_', station_number, '_', hachi)) 
sable <- read_csv(here(yr, "data", "sable_lls_ebsslope.csv"))
stations <- read_csv(here(yr, "data", "lls_ebs_stations.csv"))
sst <- read_csv(here(yr, "data", "raw", "lls_rpw_sst.csv")) %>% 
  filter(grepl("Bering", geographic_area_name) & country == "United States")
totalllscatch <- read_csv(here(yr, "data", "total_lls_catch.csv"))
totalllscatch %>% 
  group_by(fishing_event_id)
sst %>% 
  ggplot(aes(year, rpw)) +
  geom_point() +
  geom_line() +
  facet_wrap(~geographic_area_name) +
  labs(x = NULL, y = "SST RPW") 

ggplot(data = sable, aes(year, rpn)) +
  geom_point() +
  geom_line() +
  facet_wrap(~geographic_area_name) +
  labs(x = NULL, y = "sable RPW")

# Sets depredated by KW ----

# by geographic_area_name
depred <- llscatch %>% 
  group_by(year, geographic_area_name) %>% 
  dplyr::summarize(nsets = length(unique(fishing_event_id))) %>% 
  left_join(llscatch %>% 
              filter(rpn_filter %in% c('k', 'K')) %>% 
              group_by(year, geographic_area_name) %>% 
              dplyr::summarize(nsets_depred = length(unique(fishing_event_id)))) %>% 
  mutate(nsets_depred = replace_na(nsets_depred, 0),
         propn_sets_depred = nsets_depred / nsets)

depred %>% 
  ggplot(aes(x = year, y = propn_sets_depred)) +
  geom_line() +
  geom_point() +
  facet_wrap(~geographic_area_name ) +
  labs(x = NULL, y = "Proportion sets depredated")

ggsave(here(yr, "results", "kwdepred_sets.png"), units = "in",
       width = 7, height = 5, bg = 'white')


# empty or baited hooks (45 hooks on each hachi)
empty <- totalllscatch %>% 
  group_by(year, geographic_area_name, fishing_event_id) %>% 
  summarise(empty = 45 - sum(catch_freq, na.rm = TRUE) - unique(ineffective) -unique(baited)) %>% # 
  group_by(year, geographic_area_name) %>% 
  summarise(empty = sum(empty))

std <- sst %>% 
  group_by(geographic_area_name) %>% 
  mutate(std = (rpw-mean(rpw, na.rm=TRUE))/sd(rpw, na.rm=TRUE),
         value = rpw,
         var = 'SST RPW') %>% 
  ungroup() %>% 
  select(var, year, geographic_area_name, value, std) %>% 
  bind_rows(sable %>% 
              group_by(geographic_area_name) %>% 
              mutate(std = (rpn-mean(rpn))/sd(rpn),
                     value = rpn,
                     var = 'Sable RPN') %>% 
              ungroup() %>% 
              select(var, geographic_area_name, year, value, std)) %>% 
  bind_rows(depred %>% 
              group_by(geographic_area_name) %>% 
              mutate(std = (propn_sets_depred-mean(propn_sets_depred))/sd(propn_sets_depred),
                     value = propn_sets_depred,
                     var = 'KW depred') %>% 
              ungroup() %>% 
              select(var, geographic_area_name, year, value, std)) %>% 
  # bind_rows(empty %>% 
  #             group_by(geographic_area_name) %>% 
  #             mutate(std = (empty-mean(empty))/sd(empty),
  #                    value = empty,
  #                    var = 'Empty hooks') %>% 
  #             ungroup() %>% 
  #             select(var, geographic_area_name, year, value, std)) %>% 
  mutate(var = factor(var, levels = c("SST RPW", "Sable RPN", #"Empty hooks", 
                                      "KW depred"),
                      ordered = TRUE))

std %>% 
  filter(!is.na(std)) %>% 
  pivot_wider(id_cols = c(year, geographic_area_name),
              names_from = var, values_from = std) %>% 
  group_by(geographic_area_name) %>% 
  filter(!is.na(`SST RPW`)) %>% 
  summarise(cor_sable = cor(`SST RPW`, `Sable RPN`, method = 'pearson'),
            cor_kw = cor(`SST RPW`, `KW depred`, method = 'pearson'))

std %>% 
  ggplot(aes(x = year, y = std, col = var, shape = var)) + #lty = var, 
  geom_line() +
  geom_point() +
  facet_wrap(~geographic_area_name) +
  labs(x = NULL, y = NULL,
       col = NULL, shape = NULL, lty = NULL) +
  ggthemes::scale_color_colorblind() +
  theme_bw(base_size = 13) 


std %>% 
  ggplot(aes(x = year, y = value, col = var, lty = var, shape = var)) +
  geom_vline(xintercept = 2015, col = 'grey') +
  geom_line() +
  geom_point() +
  facet_grid(var~geographic_area_name, scales = 'free') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL,
       col = NULL, shape = NULL, lty = NULL) +
  ggthemes::scale_color_colorblind() +
  # theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))


ggsave(here(yr, "results", "sst_sable_kwdepred.png"), units = "in",
       width = 8, height = 7, bg = 'white')

