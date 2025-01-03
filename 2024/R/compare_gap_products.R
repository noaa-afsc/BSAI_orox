# compare GAP products

pkgs <- c("tidyverse", "here", "cowplot", "ggthemes")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# folder set up
dir.create(here(yr, "results", "compare_gap_products"))

yr <- 2024

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 10) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# 2022 assessment data
df22 <- read_csv(here("2022", "data", "bsai_orox_biomass_2022.csv")) %>% 
  mutate(group = ifelse(common_name == "shortspine thornyhead", "SST", "non-SST"),
         survey = case_when(survey == "EBS_SLOPE" ~ "EBS Slope",
                            survey == "EBS_SHELF" ~ "EBS Shelf",
                            survey == "AI" ~ "AI"))
# 2024 assessment data
df24 <- read_csv(here("2024", "data", "raw", "biomass_orox.csv")) %>% 
  mutate(area = case_when(area_name == "Western Aleutians" ~ "Western AI",
                          area_name == "Eastern Aleutians" ~ "Eastern AI",
                          area_name == "Central Aleutians" ~ "Central AI",
                          area_name == "Southern Bering Sea" ~ "SBS",
                          area_name == "Standard" ~ "EBS Shelf",
                          area_name == "EBS slope: All Strata" ~ "EBS Slope")) %>% 
  select(-area_name, -area_id, -survey_definition_id)

df <- bind_rows(df22 %>% mutate(Database = "Old"), 
                df24 %>% mutate(Database = "New")) %>% 
  filter(year %in% 1982:2022) %>% 
  group_by(Database, survey, common_name, year) %>% 
  summarise(biomass = sum(biomass, na.rm = TRUE),
            var = sum(var, na.rm = TRUE)) %>% 
  filter(biomass != 0)

df %>% 
  filter(survey == "AI") %>% 
  ggplot(aes(x = year, y = biomass, fill = Database)) +
  geom_bar(stat = 'identity', width = 2, position = position_dodge2(width = 0.5,preserve = "single", padding = 0)) +  
  # geom_errorbar(aes(ymin = biomass, ymax = biomass + sqrt(var)),
  #                position = position_dodge2(width = 0, preserve = "single", padding = 0)) +
    facet_wrap(~common_name, scale = 'free_y', ncol = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL, y = "Biomass (t)", title = "AI trawl survey") +
  # ggthemes::scale_fill_colorblind() +
 scale_fill_manual(values = c("limegreen", "magenta")) +
  scale_y_continuous(labels = scales::comma)

ggsave(here(yr, "results", "compare_gap_products", "compare_ai.png"), units = 'in', bg = 'white',
       height = 10, width = 8.5, dpi = 300)

df %>% 
  filter(survey == "EBS Slope") %>% 
  ggplot(aes(x = year, y = biomass, fill = Database)) +
  # geom_bar(stat = 'identity', position = position_dodge2(preserve = "single", padding = 0)) +
  # geom_errorbar(aes(ymin = biomass, ymax = biomass + sqrt(var)),
  #               position = position_dodge2(width = 0.5, preserve = "single", padding = 0)) +
  geom_bar(stat = 'identity', width = 2, position = position_dodge2(width = 0.5,preserve = "single", padding = 0)) +  
  # geom_errorbar(aes(ymin = biomass, ymax = biomass + sqrt(var)),
  #                position = position_dodge2(width = 0, preserve = "single", padding = 0)) +
  facet_wrap(~common_name, scale = 'free_y') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL, y = "Biomass (t)", title = "EBS Slope trawl survey") +
  # ggthemes::scale_fill_colorblind() +
  scale_fill_manual(values = c("limegreen", "magenta")) +
  scale_y_continuous(labels = scales::comma)

ggsave(here(yr, "results", "compare_gap_products", "compare_ebsslope.png"), units = 'in', bg = 'white',
       height = 8, width = 8.5, dpi = 300)

df %>% 
  filter(survey == "EBS Shelf") %>% 
  ggplot(aes(x = year, y = biomass, fill = Database)) +
  # geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single", padding = 0)) +
  # geom_errorbar(aes(ymin = biomass, ymax = biomass + sqrt(var)),
  #               position = position_dodge2(width = 0.5, preserve = "single", padding = 0)) +
  geom_bar(stat = 'identity', width = 2, position = position_dodge2(width = 0.5,preserve = "single", padding = 0)) +  
  # geom_errorbar(aes(ymin = biomass, ymax = biomass + sqrt(var)),
  #                position = position_dodge2(width = 0, preserve = "single", padding = 0)) +
  facet_wrap(~common_name, scale = 'free_y', ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL, y = "Biomass (t)", title = "EBS Shelf trawl survey") +
  scale_fill_manual(values = c("limegreen", "magenta")) +
  scale_y_continuous(labels = scales::comma)

ggsave(here(yr, "results", "compare_gap_products", "compare_ebsshelf.png"), units = 'in', bg = 'white',
       height = 8, width = 8.5, dpi = 300)
