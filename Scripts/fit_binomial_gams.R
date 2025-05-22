# PURPOSE: to fit binomial GAM to mature (0,1) weighted by CPUE or unweighted

# Author: Emily Ryznar

# NOTES:
# Make sure you're using updated chela database compiled by shannon, though can't use it for everything bc
# it doesn't have sampling factor...

# LOAD LIBS/PARAMS/DATA ----
source("./Scripts/load_libs_params.R")
source("./Scripts/get_survey_data.R")


cpue <- rbind(readRDS("./Data/snow_survey_CPUE_male1mm.rda") %>% mutate(SPECIES = "SNOW"),
              readRDS("./Data/tanner_survey_CPUE_male1mm.rda") %>% mutate(SPECIES = "TANNER"))

cutline.params <- read.csv("./Output/cutline_parameters.csv")


# Join cutline parameters and 1mm cpue to chela crab 
mod.dat <- chela_db %>% # loaded from get_survey_data.R
              right_join(., cutline.params) %>%
              mutate(CUTOFF= BETA0 + BETA1*LN_CW, # apply cutline model
                     MATURE = case_when((LN_CH > CUTOFF) ~ 1,
                                        TRUE ~ 0),
                     MAT_TEXT = case_when((MATURE == 1) ~ "Mature",
                                          TRUE ~ "Immature")) %>% # 
              right_join(., cpue) %>% # adding in haul-level CPUE for corresponding 1mm size bin
              mutate(YEAR = as.factor(YEAR)) %>%
              mutate(MATURE = case_when((SPECIES == "SNOW" & SIZE <=35) ~ 0,
                                        (SPECIES == "SNOW" & SIZE >= 135) ~ 1,
                                        (SPECIES == "TANNER" & SIZE <= 55) ~ 0, # All EBS only
                                        (SPECIES == "TANNER" & SIZE >= 145) ~1, # All EBS only
                                        TRUE ~ MATURE),
                     CPUE = as.integer(round(CPUE))) %>%
          na.omit()

# FIT SNOW BINOMIAL GAMS ----
mod.dat2 <- mod.dat %>% filter(SPECIES == "SNOW")

# Unweighted model
unwt.mod <- bam(MATURE ~ YEAR + s(SIZE), family = binomial(link = "logit"), data = mod.dat2)

unwt.prd <- predict(unwt.mod, data.frame(YEAR = mod.dat2$YEAR, SIZE = mod.dat2$SIZE),
               type = "response", se.fit = TRUE)$fit

unwt.dat <- data.frame(Species = mod.dat2$SPECIES, 
                       Year = mod.dat2$YEAR, 
                       Mature = mod.dat2$MATURE, 
                       Size = mod.dat2$SIZE,
                       Type = "Unweighted",
                       P.molt = unwt.prd)


# Cpue-weighted model
cpue.mod <- bam(MATURE ~ YEAR + s(SIZE), family = binomial(link = "logit"), data = mod.dat2, weights = CPUE)

cpue.prd <- predict(cpue.mod , data.frame(YEAR = mod.dat2$YEAR, SIZE = mod.dat2$SIZE),
               type = "response", se.fit = TRUE)$fit

cpue.dat <- data.frame(Species = mod.dat2$SPECIES, 
                       Year = mod.dat2$YEAR, 
                       Mature = mod.dat2$MATURE, 
                       Size = mod.dat2$SIZE,
                       Type = "CPUE-weighted",
                       P.molt = cpue.prd)


# Combine predictions from both models
all.dat <- rbind(unwt.dat, cpue.dat)
  
 
ggplot()+
  geom_line(all.dat %>% filter(Type == "Unweighted"), mapping = aes(Size, P.molt, color = as.factor(1), group = Year), linewidth = 1)+
  geom_line(all.dat %>% filter(Type == "CPUE-weighted"), mapping = aes(Size, P.molt, color = as.factor(2), group = Year), linewidth = 1)+
  scale_color_manual(values = c("darkgoldenrod", "cadetblue"), labels = c("Unweighted", "CPUE-weighted"), name = "")+
  theme_bw()+
  facet_wrap(~Year)+
  ylab("p(terminal molt)")+
  xlab("Carapace width (mm)")+
  ggtitle("Snow")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave("./Figures/snow_binomialGAMs_pmolt_predictions.png", width = 11, height = 8.5)

# FIT TANNER BINOMIAL GAMS ----
mod.dat2 <- mod.dat %>% filter(SPECIES == "TANNER")

# Unweighted model
unwt.mod <- bam(MATURE ~ YEAR + s(SIZE), family = binomial(link = "logit"), data = mod.dat2)

unwt.prd <- predict(unwt.mod, data.frame(YEAR = mod.dat2$YEAR, SIZE = mod.dat2$SIZE),
                    type = "response", se.fit = TRUE)$fit

unwt.dat <- data.frame(Species = mod.dat2$SPECIES, 
                       Year = mod.dat2$YEAR, 
                       Mature = mod.dat2$MATURE, 
                       Size = mod.dat2$SIZE,
                       Type = "Unweighted",
                       P.molt = unwt.prd)


# Cpue-weighted model
cpue.mod <- bam(MATURE ~ YEAR + s(SIZE), family = binomial(link = "logit"), data = mod.dat2, weights = CPUE)

cpue.prd <- predict(cpue.mod , data.frame(YEAR = mod.dat2$YEAR, SIZE = mod.dat2$SIZE),
                    type = "response", se.fit = TRUE)$fit

cpue.dat <- data.frame(Species = mod.dat2$SPECIES, 
                       Year = mod.dat2$YEAR, 
                       Mature = mod.dat2$MATURE, 
                       Size = mod.dat2$SIZE,
                       Type = "CPUE-weighted",
                       P.molt = cpue.prd)


# Combine predictions from both models
all.dat <- rbind(unwt.dat, cpue.dat)


ggplot()+
  geom_line(all.dat %>% filter(Type == "Unweighted"), mapping = aes(Size, P.molt, color = as.factor(1), group = Year), linewidth = 1)+
  geom_line(all.dat %>% filter(Type == "CPUE-weighted"), mapping = aes(Size, P.molt, color = as.factor(2), group = Year), linewidth = 1)+
  scale_color_manual(values = c("darkgoldenrod", "cadetblue"), labels = c("Unweighted", "CPUE-weighted"), name = "")+
  theme_bw()+
  facet_wrap(~Year)+
  ylab("p(terminal molt)")+
  xlab("Carapace width (mm)")+
  ggtitle("Tanner")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave("./Figures/tanner_binomialGAMs_pmolt_predictions.png", width = 11, height = 8.5)


