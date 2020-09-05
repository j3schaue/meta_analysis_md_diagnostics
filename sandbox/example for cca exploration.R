library(tidyverse)
library(naniar)
library(visdat)
adt <- read_rds("./data/adt_data.RDS")

skimr::skim(adt)
adt %>% 
  select_if(is.numeric) %>% 
  select(-pk_es, -studyid, -groupid1, -groupid2, -varid) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot::corrplot()

nms <- read_csv("./code/keys_names.csv")
names(adt)

adt %>%
  group_by(groupid1, groupid2, varid) %>%
  arrange(desc(estimingpost)) %>%
  slice(1) %>%
  ungroup()

vars = adt %>% 
  group_by(groupid1, groupid2, varid) %>% 
  distinct() %>%
  tally() %>%
  ungroup()

adt_example <- adt %>%
  group_by(studyid, groupid1, groupid2) %>%
  arrange(desc(estimingpost)) %>%
  slice(1) %>%
  ungroup()

gg_summary_covariate_miss(adt_example)

cor(adt_example$g1hrsperweek, adt_example$g2hrsperweek, 
    use = "pairwise.complete.obs")

dd <- adt_example %>%
  mutate(g1out = (g1loc == "2. Outpatient")) %>%
  select(studyid, es_g, se_g, g1hrs = g1hrsperweek, g2hrs = g2hrsperweek, g1out) %>%
  mutate(v_g = se_g^2)

skimr::skim(dd)

tmp <- dd %>% 
  distinct(studyid, g1hrs, g2hrs) %>%
  group_by(studyid) %>%
  tally() %>%
  filter(n > 1) %>%
  left_join(dd) %>%
  distinct(studyid, g1hrs, g2hrs)

tmp %>%
  distinct(studyid)

# RVE
library(robumeta)

# Mod 1: Outpatient
rv_mod_outpatient <- robu(formula = es_g ~ g1out, 
                          data = dd, 
                          studynum = studyid, 
                          var.eff.size = v_g, 
                          small = TRUE)

rv_mod_hrs <-  robu(formula = es_g ~ g1hrs + g2hrs, 
                    data = dd,
                    studynum = studyid, 
                    var.eff.size = v_g,
                    # rho = 0.5,
                    small = TRUE)

rv_mod_hiint <- robu(formula = es_g ~ g1hi + g2hi, 
                     data = dd %>% mutate(g1hi = g1hrs > 1.5, 
                                          g2hi = g2hrs > 1.5), 
                     studynum = studyid, 
                     var.eff.size = v_g,
                     # rho = 0.5,
                     small = TRUE)

rv_mod_hiint
rv_mod_hrs

rvmod_hiint_g1 <- robu(formula = es_g ~ g1hi, 
                       data = dd %>% mutate(g1hi = g1hrs > 1.5, 
                                            g2hi = g2hrs > 1.5), 
                       studynum = studyid, 
                       var.eff.size = v_g,
                       # rho = 0.5,
                       small = TRUE)

rvmod_hiint_g2 <- robu(formula = es_g ~ g2hi, 
                       data = dd %>% mutate(g1hi = g1hrs > 1.5, 
                                            g2hi = g2hrs > 1.5), 
                       studynum = studyid, 
                       var.eff.size = v_g,
                       # rho = 0.5,
                       small = TRUE)




## MV
library(metafor)
mv_mod <- rma.mv(yi = es_g, V = v_g, 
                 mods = ~ 1 +  g1hrs + g2hrs,
                 random = ~ 1 | studyid,
                 data = dd)
mv_mod
rv_mod


# Repeat with study-level aggregates

dd_sl <- dd %>%
  group_by(studyid) %>% 
  summarize(sm_g1hrs = mean(g1hrs, na.rm = TRUE), 
            sm_g2hrs = mean(g2hrs, na.rm = TRUE)) %>%
  left_join(dd)


rv_mod_sm <- robu(formula = es_g ~ g1hrs + g2hrs + sm_g1hrs + sm_g2hrs, 
                  var.eff.size = v_g,
                  data = dd_sl,
                  studynum = studyid, 
                  # rho = 0.5, 
                  small = TRUE)

mv_mod_sm <- rma.mv(yi = es_g, V = v_g, 
                 mods = ~ 1 +  g1hrs + g2hrs + sm_g1hrs + sm_g2hrs,
                 random = ~ 1 | studyid,
                 data = dd_sl)

rv_mod_sm
mv_mod_sm


dd_days <- adt_example %>%
  select(studyid, es_g, se_g, g1days = g1txdays, g2days = g2txdays) %>%
  mutate(v_g = se_g^2)

skimr::skim(dd_days)

# RVE
library(robumeta)
rv_mod_days <-  robu(formula = es_g ~ g1days + g2days, data = dd_days,
                     studynum = studyid, var.eff.size = v_g,
                     # rho = 0.5, 
                     small = TRUE)
rv_mod_days


## MV
library(metafor)
mv_mod_days <- rma.mv(yi = es_g, V = v_g, 
                      mods = ~ 1 +  g1days + g2days,
                      random = ~ 1 | studyid,
                      data = dd_days)
mv_mod_days
rv_mod_days


# Repeat with study-level aggregates

dd_days_sl <- dd_days %>%
  group_by(studyid) %>% 
  summarize(sm_g1days = mean(g1days, na.rm = TRUE), 
            sm_g2days = mean(g2days, na.rm = TRUE)) %>%
  left_join(dd_days)


rv_mod_days_sm <- robu(formula = es_g ~ g1days + g2days + sm_g1days + sm_g2days, 
                       var.eff.size = v_g,
                       data = dd_days_sl,
                       studynum = studyid, 
                       # rho = 0.5, 
                       small = TRUE)

mv_mod_days_sm <- rma.mv(yi = es_g, V = v_g, 
                         mods = ~ 1 +  g1days + g2days + sm_g1days + sm_g2days,
                         random = ~ 1 | studyid,
                         data = dd_days_sl)

rv_mod_days_sm
mv_mod_days_sm
