# Queries for BSAI OROX update assessment
# Contact: jane.sullivan@noaa.gov
# Last updated: Sep 2024

# devtools::session_info()
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32

# Note: also includes analysis using observer data to partition thornyhead catch
# to shortspine (SST) and other thornyhead

# Set up ----

pkgs <- c("tidyverse", "odbc", "keyring", "dbplyr", "here")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

yr <- 2024
dir.create(here(yr))
dir.create(here(yr, "data"))
dir.create(here(yr, "data", "raw"))

# database ----
akfin <- DBI::dbConnect(odbc::odbc(), "akfin", uid = keyring::key_list("akfin")$username,
                        pwd = keyring::key_get("akfin", keyring::key_list("akfin")$username))
# afsc <- DBI::dbConnect(odbc::odbc(), "afsc", uid = keyring::key_list("afsc")$username,
#                         pwd = keyring::key_get("afsc", keyring::key_list("afsc")$username))

# species codes ----
spp_lkup <- read_csv("general/bsai_orox_spp_lookup.csv")
my_spp_codes <- spp_lkup$species_code

# survey area id ----
lkup <- tbl(akfin, sql("gap_products.akfin_area")) %>%
# lkup <- tbl(afsc, sql("gap_products.area")) %>%
  rename_all(tolower) %>%
  # 98 = Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey
  # 52 = AI
  # 78 = EBS slope
  filter(c(survey_definition_id == 98 & area_type == "REGION" & area_name == "Standard") |
           c(survey_definition_id == 52 & area_type == "INPFC") |
           c(survey_definition_id == 78 & area_type == "REGION" & design_year == 2023)) %>% 
  collect()

my_ids <- lkup %>%  pull(area_id)

# BTS biomass ----

biom <- tbl(akfin, sql("gap_products.akfin_biomass")) %>%
# biom <- tbl(afsc, sql("gap_products.biomass")) %>%
  rename_all(tolower) %>%
  select(survey_definition_id, area_id, species_code, year, biomass_mt, biomass_var) %>%
  filter(species_code %in% my_spp_codes & area_id %in% my_ids ) %>% #& & year >= 1987 &
           # biomass_mt != 0 & biomass_var != 0) %>%
  collect() %>%
  left_join(spp_lkup) %>% 
  left_join(lkup %>% select(area_id, area_name)) %>% 
  mutate(survey = ifelse(survey_definition_id == 98, "EBS Shelf",
                         ifelse(survey_definition_id == 52, "AI",
                                "EBS Slope")),
         group = ifelse(common_name %in% c("shortspine thornyhead"), "SST", "non-SST")) %>%
  arrange(survey, group, common_name, year) %>%
  select(survey, area_name, common_name, group, year, biomass = biomass_mt, var = biomass_var, 
         survey_definition_id, area_id, species_code)

write_csv(biom, here(yr, "data", "raw", "biomass_orox.csv"))

# prep for rema:

# species group file - by SST and non-SST
biomsum <- biom %>%
  mutate(survey = ifelse(area_name == "Southern Bering Sea", "SBS", survey)) %>% 
  group_by(survey, group, year) %>%
  summarize(biomass = sum(biomass),
            cv = ifelse(biomass > 0, sqrt(sum(var))/biomass, NA),
            .groups = "drop") 
biomsum %>% filter(year == yr)
biomsum %>% filter(year == min(biom$year))
biomsum %>%  write_csv(here(yr, "data", "sppgroup_biomass_orox.csv"))

# LLS RPWs SST ----

query = "select    *
         from      afsc.lls_area_rpn_all_strata
         where     species_code = '30020' and country = 'United States' and fmp_management_area = 'BSAI'
        order by   year asc"
lls <- DBI::dbGetQuery(akfin, query) %>% 
  rename_all(tolower) 
unique(lls$geographic_area_name)

lls %>% write_csv(here(yr, "data", "raw", 'lls_rpw_sst.csv'))

llssum <- lls %>% 
  filter(country == 'United States' & 
           !geographic_area_name %in% c('NW Aleutians slope', 'SW Aleutians slope')) %>% 
  mutate(strata = ifelse(geographic_area_name %in% c('NE Aleutians slope', 'SE Aleutians slope'),
                         'Eastern AI', 'EBS Slope')) %>% 
  group_by(strata, year) %>% 
  dplyr::summarise(cpue = sum(rpw, na.rm = TRUE),
                   cv = sqrt(sum(rpw_var, na.rm = TRUE)) / cpue)

llssum %>% 
  ggplot(aes(x = year, y = cpue)) +
  geom_line() +
  geom_point() +
  facet_wrap(~strata)

llssum %>% write_csv(here(yr, "data", 'lls_rpw_sst.csv'))

lls %>% 
  filter(!is.na(rpn)) %>% 
  ggplot(aes(x = year, y = rpn, col = country)) +
  geom_point() +
  geom_line() +
  facet_wrap(~geographic_area_name, scales = 'free_y')

# LLS eda for 2024 ----
stations <- tbl(akfin, sql("afsc.lls_stations_view")) %>%
  rename_all(tolower) %>%
  # select(survey_definition_id, area_id, species_code, year, biomass_mt, biomass_var) %>%
  filter(council_management_area %in% c("Bering Sea") & 
           station_number < 100) %>%
  collect()
stations %>% distinct(station_number, geographic_area_name) %>% 
  print(n=Inf)
stations %>% write_csv(here(yr, "data", "lls_ebs_stations.csv"))

totalllscatch <- tbl(akfin, sql("afsc.lls_catch_summary_with_nulls_mv")) %>%
  rename_all(tolower) %>%
  filter(council_sablefish_management_area == 'Bering Sea' &
           country == 'United States' &
           year >= 1997 ) %>%
  select(cruise_number, station_number, hachi, 
         year, common_name, catch_freq, baited, 
         rpn_filter, intrpdep, stratum, ineffective,
         geographic_area_name) %>% 
  collect() %>% 
  mutate(fishing_event_id = paste0(cruise_number, '_', station_number, '_', hachi)) 

totalllscatch %>% write_csv(here(yr, "data", "total_lls_catch.csv"))

sable <- tbl(akfin, sql("afsc.lls_area_rpn_all_strata")) %>%
  rename_all(tolower) %>%
  select(geographic_area_name, country, year, species_code, year, rpn) %>%
  filter(species_code == 20510 &
           country == 'United States') %>%
  collect() %>% 
  filter(grepl("Bering", geographic_area_name)) %>% 
  arrange(year)

sable %>% write_csv(here(yr, "data", "sable_lls_ebsslope.csv"))

sable %>% 
  ggplot(aes(year, rpw)) +
  geom_point() +
  # geom_smooth(method = "loess", se = F) +
  geom_line() +
  facet_wrap(~geographic_area_name) +
  labs(x = "Year", y = "RPW")

llscatch <- tbl(akfin, sql("afsc.lls_catch_summary_with_nulls_mv")) %>%
  rename_all(tolower) %>%
  # select(geographic_area_name, country, year, species_code, year, rpw) %>%
  filter(species_code == 30020 &
           council_sablefish_management_area == 'Bering Sea' &
           country == 'United States' &
           year >= 1997 ) %>%
  collect() %>% 
  left_join(stations %>% select(station_number, geographic_area_name))

llscatch %>% write_csv(here(yr, "data", "sst_catch_lls.csv"))

# Species lookup ----

query <- paste0("select   species_code, species_name, common_name
                 from     afsc.race_racespeciescodes")

spp <- DBI::dbGetQuery(akfin, query) %>%
  rename_all(tolower)

spp %>% filter(grepl("dusky", common_name))
spp %>% filter(grepl("thornyhead|rockfish", common_name))
spp %>% filter(grepl("Atka", common_name))

# Seven most common BSAI Orox
orox <- c("dusky rockfish", "shortspine thornyhead", "redstripe rockfish",
          "redbanded rockfish", "yelloweye rockfish", "harlequin rockfish",
          "sharpchin rockfish")

# Other Orox species that sometimes show up? catch tables
other_orox <- c("black rockfish", "darkblotched rockfish", "rosethorn rockfish",
                "silvergray rockfish", "rockfish unid.", "dusky and dark rockfishes unid.",
                "yellowmouth rockfish", "broadfin thornyhead", "longspine thornyhead")

codes <- spp %>%
  filter(common_name %in% c(orox, other_orox)| grepl("thornyhead", common_name)) %>%
  pull(species_code)

codes_string <- toString(sprintf("'%s'", codes)) # allows you to pass vector into sql query

# spp %>%
#   filter(species_code %in% codes) %>%
#   write_csv(here("general", "bsai_orox_spp_lookup.csv"))

# Shortspine Thornyhead = SST = 30020 = 350
# Dusky = 30152 = 330

# Observer program codes uses different species codes than RACE (5 digits). You
# need the 3 digit codes to pull observer data

query <- "select   distinct species_code, common_name, scientific_name,
                   race_species_num as species_code_race
          from     norpac.atl_lov_species_code"

akr_spp <- DBI::dbGetQuery(afsc, query) %>%
  rename_all(tolower) %>%
  filter(species_code_race %in% codes)

# Catch ----

# 3 digit species codes - can't find a look up table for this one.
# northern rockfish (136) currently in species_group_name = "Other Rockfish"
# group but shouldn't be. removed from bsai_orox3 list

# query <- "select   distinct agency_species_code, species_name, species_group_name
#           from     council.comprehensive_blend_ca
#           where    species_group_name = 'Other Rockfish'"
# catch_spp <- DBI::dbGetQuery(akfin, query)

# catch_spp %>% 
#   rename_all(tolower) %>% 
#   filter(!is.na(species_name)) %>% 
#   filter(!grepl(c('dusky|northern|thornyhead'), species_name)) %>% 
#   filter(agency_species_code != 141) %>%  # POP
#   filter(agency_species_code != 144) %>%  # unspecified slope
#   arrange(agency_species_code) %>% 
#   write_csv('goaorox_catch_sppcodes.csv')

# dusky = (154, 172)
bsai_orox3 <- c(153, 154, 172, 148, 147, 157, 139, 158, 145, 176, 143, 142, 
                150, 156, 155, 175, 149, 159, 166, 146, 184, 137,
                138, 178, 182, 179)

# these are species classified in catch as "Other Rockfish" that occur primarily
# in GOA. Keep these in just in case any of these spp start to show up in the
# BSAI catch
goa_orox3 <- c(179, 182, 178, 138, 137, 184, 146, 149, 155, 156, 147, 148)

sort(unique(c(bsai_orox3, goa_orox3)))
codes_string2 <- toString(sprintf("'%s'", c(bsai_orox3, goa_orox3))) # allows you to pass vector into sql query

query <- "select   species_group_name, species_group_code, species_name, agency_species_code,
                   year, week_end_date, catch_activity_date,
                   fmp_area, fmp_subarea, reporting_area_code, 
                   weight_posted, retained_or_discarded, fmp_gear, agency_gear_code,
                   harvest_sector, trip_target_name
          from     council.comprehensive_blend_ca
          where    agency_species_code in (%s) and
                   fmp_area = 'BSAI' and
                   year >= 2003"

catch <- DBI::dbGetQuery(akfin, sprintf(query, codes_string2)) %>% 
  rename_all(tolower) 

catch %>% write_csv(here(yr, "data", "raw", "bsai_orox_catch_confidential.csv"))
# catch %>% write_csv(here(yr, "data", "raw", "bsai_orox_catch_confidential_novgpt.csv"))

# Proportion SST ----

# Fishery catch is only reported as "thornyhead", so use observer data to
# estimate proportion SST from other thornyheads

thorny <- akr_spp %>% filter(grepl("THORNY", common_name))

thorny_string <- toString(sprintf("'%s'", thorny$species_code)) 

query <- "select    a.year, a.species, a.species_name, a.sample_number, a.sample_size,
                    a.sample_weight, a.extrapolated_weight, a.extrapolated_number, 
                    a.percent_retained, b.gear_type, b.latdd_start, b.londd_start,
                    b.nmfs_area, a.haul_join
                    
          from      obsint.debriefed_spcomp a
          
          join      obsint.debriefed_haul b on a.haul_join = b.haul_join
          
          where     a.species in (%s) and 
                    a.year >= 2003 and
                    b.nmfs_area between 500 and 543"

observed <- DBI::dbGetQuery(afsc, sprintf(query, thorny_string)) %>% 
  rename_all(tolower) 

observed %>% write_csv(here(yr, "data", "raw", "obs_thornyheads_confidential.csv"))

# observed %>% filter(species == 350) %>% count(year, haul_join) %>% group_by(year) %>% summarize(tst = length(which(n >= 3)))
# observed %>% filter(species == 350) %>% count(haul_join) %>% filter(n > 10)
# observed %>% filter(species == 350) %>% count(year, gear_type, haul_join) %>% group_by(year, gear_type) %>% summarize(n_hauljoins_3plusrows = length(which(n >= 3))) #%>% View()
# observed %>% filter(haul_join == 24213002718000001024)

# Get catch by SST ----

catch_clean <- catch %>% 
  mutate(year = as.numeric(year),
         species = str_replace(species_name, "rockfish, ", ""),
         species = str_replace(species, fixed(" (red snapper)"), ""),
         species = str_replace(species, fixed(" (idiots)"), ""),
         # Re-level Bering Sea so it's consistent with SAFE terminology
         fmp_subarea = ifelse(fmp_subarea == "BS", "EBS", fmp_subarea)) %>% 
  select(year, date = catch_activity_date, fmp = fmp_area, fmp_subarea,
         nmfs_area = reporting_area_code, gear = agency_gear_code,
         retained_or_discarded, target = trip_target_name, 
         species_code = agency_species_code, species_group = species_group_name,
         species_name = species, tons = weight_posted)

# proportion of SSTs in thornyhead catch using observer data
prop_sst <- observed %>% 
  mutate(thorny = ifelse(species == 350, "shortspine thornyhead", "other thornyhead"),
         fmp_subarea = ifelse(between(nmfs_area, 500, 540), "EBS", "AI")) %>% 
  group_by(year, fmp_subarea, thorny) %>% 
  summarize(w = sum(extrapolated_weight)) %>% 
  group_by(year, fmp_subarea) %>% 
  mutate(W = sum(w)) %>% 
  ungroup() %>% 
  mutate(prop_sst = w / W) %>% 
  filter(thorny == "shortspine thornyhead") %>% 
  select(year, fmp_subarea, prop_sst)

# create new species_names for SST and other thornyheads
catch_clean <- catch_clean %>% 
  filter(species_name == "thornyhead") %>%
  left_join(prop_sst) %>% 
  mutate(tons = tons * prop_sst,
         species_name = "SST") %>% 
  bind_rows(catch_clean %>% 
              filter(species_name == "thornyhead") %>%
              left_join(prop_sst) %>% 
              mutate(tons = tons * (1 - prop_sst),
                     species_name = "other thornyheads")) %>% 
  select(- prop_sst) %>% 
  bind_rows(catch_clean %>% 
              filter(species_name != "thornyhead")) 

catch_clean %>% write_csv(here(yr, "data", "bsai_orox_catch_confidential.csv"))

observed %>% 
  mutate(thorny = ifelse(species == 350, "SST", "other thornyhead"),
         fmp_subarea = ifelse(between(nmfs_area, 500, 540), "BS", "AI")) %>% 
  write_csv(here(yr, "data", "obs_thornyheads_confidential.csv"))

prop_sst %>% write_csv(here(yr, "data", "prop_sst_catch.csv"))

# Specs ----

spec <- tbl(akfin, sql('akr.v_cas_tac')) %>% 
  rename_all(tolower) %>% 
  filter(year >= 2003 & 
           species_group_code %in% c('ROCK') & 
           fmp_area_code %in% c("BSAI")) %>%
  collect()

spec <- spec %>% 
  arrange(year) %>% 
  select(year, area = area_label, complex = species_group_label, 
         abc = acceptable_biological_catch, ofl = overfishing_level,
         tac = total_allowable_catch) %>%
  pivot_longer(cols = c(ofl, abc, tac)) 

spec %>% write_csv(here(yr, "data", "specs.csv"))

spec %>% filter(area == "BS" & name == 'abc')  

# Non-commercial catch ----

noncom <- tbl(akfin, sql('akr.v_noncommercial_fishery_catch')) %>%
  rename_all(tolower) %>% # colnames() -> cn
  filter(species_group_name %in% c("Other Rockfish") &
           fmp_area_code %in% c("BS", "AI")) %>%
  collect()

unique(noncom$collection_agency)

write_csv(noncom, here(yr, "data", "noncommercial_catch.csv"))

# Fishery lengths ----

fshlen <- tbl(akfin, sql('norpac.debriefed_length_mv')) %>%
  rename_all(tolower) %>% # colnames() -> cn
  select(year, nmfs_area, gear, species, 
         length, sex, frequency, sample_system) %>% 
  filter(species %in% c('350', '330') &
           nmfs_area < 600 &
           between(year, 2002, yr)) %>%
  collect() 

write_csv(fshlen %>% rename(species_code = species), here(yr, "data", "fshlen.csv"))


# BTS Size frequency ----

lkup <- tbl(akfin, sql("gap_products.akfin_area")) %>%
  # lkup <- tbl(afsc, sql("gap_products.area")) %>%
  rename_all(tolower) %>%
  # 98 = Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey
  # 52 = AI
  # 78 = EBS slope
  filter(c(survey_definition_id == 52 & area_type == "INPFC")) %>% 
  collect()

distinct(lkup, area_name, area_id)
ailen_area_ids <- unique(lkup$area_id)
btslen <- dplyr::tbl(akfin, dplyr::sql('gap_products.akfin_sizecomp')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(survey_definition_id, area_id, species_code, year, length = length_mm, sex, population_count) %>% 
  dplyr::filter(survey_definition_id == 52 & 
                  species_code %in% c(30152, 30020) & 
                  sex %in% c(1,2) &
                  area_id %in% ailen_area_ids & 
                  year >= 1991) %>% 
  dplyr::collect() %>% #nrow
  dplyr::left_join(lkup %>% 
                     # filter(survey == 'AI' & area_id %in% ailen_area_ids & type == 'SUBAREA BY DEPTH') %>% #
                     distinct(survey_definition_id, area_id, area_name), 
                   by = join_by(survey_definition_id, area_id)) %>% 
  dplyr::mutate(length = length / 10)

write_csv(btslen, here(yr, "data", "raw", "bts_sizefreq.csv"))

# LLS lengths ----

llslen <- tbl(akfin, sql('afsc.lls_length_rpn_by_area_all_strata')) %>%
  rename_all(tolower) %>% # colnames() -> cn
  filter(species_code %in% c('30020')) %>%
  collect() %>% 
  arrange(year)
write_csv(llslen, here(yr, "data", "llslen.csv"))

lls_len_sum <- llslen %>% 
  filter(grepl(c('Bering|Aleutians'), geographic_area_name)) %>% 
  group_by(year, council_sablefish_management_area, length) %>% 
  dplyr::summarise(rpw = sum(rpw, na.rm = TRUE)) %>% 
  write_csv(here(yr, "data", "lls_length_sst.csv"))

# Write lengths ----

comps <- fshlen %>% 
  mutate(species_code = ifelse(species == 330, 30152, 30020),
         fmp_subarea = ifelse(nmfs_area %in% c(541:543), "AI", "EBS")) %>% 
  # filter(species_code == 350) %>% 
  select(species_code, year, length, frequency, fmp_subarea) %>% 
  mutate(source = paste0(fmp_subarea, " fishery")) %>% 
  bind_rows(btslen %>%
              mutate(fmp_subarea = ifelse(grepl("Aleutians", area_name), "AI", "EBS"),
                     source = "AI BTS") %>%
              select(species_code, year, fmp_subarea, length, frequency=population_count, source)) %>%
  # bind_rows(ebs_slope_len %>% 
  #             select(species_code, year, length, frequency) %>% 
  #             mutate(fmp_subarea = "EBS",
  #                    source = "EBS slope BTS")) %>% 
  bind_rows(lls_len_sum %>% 
              ungroup() %>% 
              mutate(species_code = 30020,
                     fmp_subarea = ifelse(council_sablefish_management_area == 'Aleutians', 'AI', 'EBS'),
                     source = paste0(fmp_subarea, ' LLS')) %>% 
              select(species_code, year, fmp_subarea, length, frequency = rpw, source)) %>% 
  write_csv(here(yr, "data", "length_frequencies.csv"))
