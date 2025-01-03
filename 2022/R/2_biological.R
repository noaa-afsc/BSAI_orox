# Biological data (length comps) for SST and Duskies
# Contact: jane.sullivan@noaa.gov
# Last updated: Oct 2020

# devtools::session_info()
# version  R version 4.0.2 (2020-06-22)
# os       Windows 10 x64              
# system   x86_64, mingw32             
# ui       RStudio 

# Set up ----

# Assessment year (most recent year with complete data set)
YEAR <- 2022
dat_path <- paste0(YEAR, "/data"); dir.create(dat_path)
out_path <- paste0(YEAR, "/results/"); dir.create(out_path)

libs <- c("tidyverse", "ggridges", "cowplot")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# Data ----

lengths <- read_csv(paste0(dat_path, "/lengths_sstdusky_", YEAR, ".csv"))
spp <- read_csv(paste0("general/bsai_orox_spp_lookup.csv"))

# Get length comps ----

comps <- lengths %>% 
  group_by(source, fmp_subarea, species_code, year, length) %>% 
  summarize(n = sum(frequency)) %>% 
  mutate(prop = n / sum(n)) %>% 
  left_join(spp %>% select(species_code, common_name)) %>% 
  group_by(source, fmp_subarea, species_code, year) %>% 
  filter(sum(n) > 50) %>% 
  mutate(N = paste0("N = ", sum(n))) %>% 
  ungroup()

# Comp test: should all be 1!
comps %>% group_by(source, fmp_subarea, species_code, year) %>% summarize(tst = sum(prop)) %>% pull(tst) 
comps %>% group_by(source, fmp_subarea, species_code, year) %>% summarize(tst = sum(prop)) %>% print(n=Inf)
unique(comps$source)
comps %>% filter(source == 'EBS slope BTS')
# comps <- comps %>% 
#   mutate(source = paste0(fmp_subarea, " ", source))

# Plot AI dusky ----

duskyyrs <- expand.grid(source = factor(c("AI fishery", "AI BTS")),
                        year = 1997:YEAR)

p1 <- ggplot(data = duskyyrs %>% 
               left_join(comps %>% 
                           filter(species_code == 30152 & between(length, 20, 50) & 
                                    source %in% c("AI fishery", "AI BTS") & fmp_subarea == 'AI') %>% 
                           mutate(source = factor(source, levels = c("AI BTS", "AI fishery")))), 
             aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "lightgrey", alpha = 0.5, 
                      panel_scaling = TRUE, size = 0.5) +
  # geom_hline(yintercept = factor(2002:YEAR), col = "lightgrey") +
  scale_fill_manual(values = c("grey30", "#00BFC4")) +
  labs(x = "Length (cm)", y = NULL, fill = NULL, title = "AI dusky rockfish") + 
  theme_light() +
  theme(legend.position = "top")
p1
ggsave(paste0(out_path, "/lencomps_dusky_AI_", YEAR, ".png"), 
       dpi=300, height=7, width=5, units="in")

# Plot EBS dusky ----

duskyyrs <- expand.grid(source = factor(c("EBS fishery", "AI BTS in SBS")),
                        year = 1997:YEAR)
p2 <- ggplot(data = duskyyrs %>% 
               left_join(comps %>% 
                           filter(species_code == 30152 & between(length, 20, 50) & 
                                    source %in% c("EBS fishery", "AI BTS") & 
                                    fmp_subarea %in% c('EBS', 'SBS')) %>% 
                           mutate(source = ifelse(source == 'AI BTS', 'AI BTS in SBS', source)) %>% 
                           mutate(source = factor(source, levels = c("AI BTS in SBS", "EBS fishery")))), 
             aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "lightgrey", alpha = 0.5, 
                      panel_scaling = TRUE, size = 0.5) +
  # geom_hline(yintercept = factor(2002:YEAR), col = "lightgrey") +
  scale_fill_manual(values = c("grey30", "#00BFC4")) +
  labs(x = "Length (cm)", y = NULL, fill = NULL, title = "EBS dusky rockfish") + 
  theme_light() +
  theme(legend.position = "top")

p2
ggsave(paste0(out_path, "/lencomps_dusky_EBS_", YEAR, ".png"), 
       dpi=300, height=7, width=5, units="in")

plot_grid(p1, p2, ncol = 2)

ggsave(paste0(out_path, "/dusky_lengths_", YEAR, ".png"), 
       dpi=300, height=7, width=10, units="in")

# Plot SST AI ----

sstyrs <- expand.grid(source = factor(c("AI BTS", "AI fishery", 'AI LLS')),
                        year = 1996:YEAR)
p1 <- ggplot(data = sstyrs %>% 
         left_join(comps %>% 
                     filter(year >= 1996 & species_code == 30020 & between(length, 10, 70)) %>% #View() 
                     filter(source %in% c("AI BTS", "AI fishery", 'AI LLS')) %>% 
                     filter(fmp_subarea == 'AI')),
       # filter(source %in% c("AI fishery", "EBS fishery", "AI survey")), #, "EBS slope survey"
       aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "white", alpha = 0.4, #alpha = 0.8
                      panel_scaling = TRUE, size = 0.5) +
  scale_fill_manual(values = c("grey30", "#00BFC4", "#daa520", "#da2055")) +
  # geom_hline(yintercept = factor(2002:YEAR), col = "lightgrey") +
  labs(x = "Length (cm)", y = NULL, fill = NULL, title = "AI shortspine thornyhead") +
  theme_light() +
  # facet_wrap(~ fmp_subarea) +
  theme(legend.position = "top",
        strip.background = element_blank())
p1 
ggsave(paste0(out_path, "/lencomps_sst_AI_", YEAR, ".png"), 
       dpi=300, height=7, width=5, units="in")

# Plot SST EBS ----
sstyrs <- expand.grid(source = factor(c('AI BTS in SBS', 'EBS LLS', 'EBS fishery')), #,'EBS slope BTS')),
                      year = 1996:YEAR)
p2 <- ggplot(data = sstyrs %>% 
               left_join(comps %>% 
                           filter(year >= 1997 & species_code == 30020 & between(length, 10, 70)) %>% #View() 
                           filter(source %in% c('AI BTS', 'EBS fishery', 'EBS LLS') & #, 'EBS slope BTS') & 
                                    fmp_subarea %in% c('EBS', 'SBS')) %>% 
                           mutate(source = ifelse(source == 'AI BTS', 'AI BTS in SBS', source)) %>% 
                           mutate(source = factor(source, levels = c('AI BTS in SBS', 'EBS LLS', 'EBS fishery')))), #,'EBS slope BTS')))),
             aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "white", alpha = 0.4, #alpha = 0.8
                      panel_scaling = TRUE, size = 0.5) +
  scale_fill_manual(values = c("grey30", "#00BFC4", "#daa520")) + #, "#da2055")) +
  # geom_hline(yintercept = factor(2002:YEAR), col = "lightgrey") +
  labs(x = "Length (cm)", y = NULL, fill = NULL, title = "EBS shortspine thornyhead") + #) +
  theme_light() +
  # facet_wrap(~ fmp_subarea) +
  theme(legend.position = "top",
        strip.background = element_blank())
p2
ggsave(paste0(out_path, "/lencomps_sst_EBS_", YEAR, ".png"), 
       dpi=300, height=7, width=5, units="in")

plot_grid(p1, p2, ncol = 2)

ggsave(paste0(out_path, "/sst_lengths_", YEAR, ".png"), 
       dpi=300, height=7, width=10, units="in")

# bubble plots ----
sply <- comps %>% 
  filter(year >= 1996 & species_code == 30020 & between(length, 10, 70)) %>% #View() 
  filter(source %in% c("EBS fishery")) %>% #, "AI BTS", "AI fishery", 'AI LLS')) %>% 
  filter(fmp_subarea == 'EBS') %>% 
  group_by(year) %>% 
  mutate(std_prop = scale(prop, scale = TRUE)) %>% 
  ungroup() %>% 
  mutate(col = ifelse(std_prop <0, "-Ve", "+Ve"))
  
ggplot(sply, aes(year, length)) +
  geom_point(aes(size = sqrt(abs(std_prop)) * 0.2, color = col)) + #, show.legend = FALSE) +
  scale_color_manual(values=c("black","grey"))+
  # geom_text(aes(label=text1),size=3,vjust=-1,angle=45,color="black")+
  labs(x="Years",y="Length", title = paste0(unique(sply$source), " in ", unique(sply$fmp_subarea))) +
  theme_bw() 

  sdat%>%group_by(Year)%>%mutate(prop.index=index/sum(index,na.rm=T))%>%
  group_by(Age)%>%mutate(std.prop.index=scale(prop.index,scale=T))%>%ungroup()%>%
  mutate(col=ifelse(std.prop.index<0,"-Ve","+Ve"),
         text=ifelse(Age==max(Age)|Year==max(Year),YC,NA),
         text1=substr(text,3,4))

ggplot(sdat.spay,aes(x=Year,y=Age))+
  geom_point(aes(size=sqrt(abs(std.prop.index))*0.2,color=col),show.legend=F)+
  scale_color_manual(values=c("black","grey"))+
  geom_text(aes(label=text1),size=3,vjust=-1,angle=45,color="black")+
  labs(x="Years",y="Ages")+
  theme_bw()