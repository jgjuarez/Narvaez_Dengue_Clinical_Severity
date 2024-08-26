#R code for the main analysis of the manuscript by Narvaez et al. 

library(tidyverse)
library(readxl)
library(broom)
library(ggeffects)
library(grid)
library(formattable)
library(forestploter)
library(broom.helpers)
library(forestploter)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(kableExtra)
library(tinytex)
library(gtsummary)
library(pandoc)
library(gt)
library(ggpubr)
library(janitor)

dfComp <- read_excel("Informe_Clasificacion_EH_CH.xlsx", 
                     col_types = c("numeric", "text", "date", 
                                   "numeric", "text", "text", "date", 
                                   "date", "date", "date", "date", "numeric", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "numeric", "text", 
                                   "numeric", "date", "date", "numeric", 
                                   "numeric", "text", "text", "text", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "numeric", "text", "text", "text", 
                                   "text", "text", "numeric", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "date", "date", 
                                   "numeric", "numeric", "text", "text", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "text", "numeric", "numeric", "text", 
                                   "text", "text", "text", "text", "numeric", 
                                   "text", "text", "text", "numeric", 
                                   "text", "text", "text", "text", "numeric", 
                                   "text", "text", "text", "text", "numeric", 
                                   "text", "text", "text", "numeric", 
                                   "text", "text", "text", "text", "numeric", 
                                   "text", "text", "text"))

only_sn_columns <- function(df) {
  sn_cols <- names(df)[sapply(df, function(col) {
    all(is.na(col) | col %in% c("S", "N"))
  })]
  sn_cols
}

dfComp$FIS <- format(as.Date(dfComp$FIS, format = "%Y/%m/%d"),"%Y")

# Use mutate and across to apply the transformation and filter for the time period
setd2 <- dfComp |>
  rename(Age = Edad,
         IR = RI) |>
  mutate(across(only_sn_columns(dfComp), ~ case_when(
    . == "S" ~ 1,
    . == "N" ~ 0,
    is.na(.) ~ NA_real_)), as.numeric(Age)) 


setd3 <- setd2 |>
  filter(serotipo < 5,
         IR != "I") |>
  mutate(Serotype = case_when(serotipo == "1" ~ "DENV1",
                              serotipo == "2" ~ "DENV2",
                              serotipo == "3" ~ "DENV3",
                              serotipo == "4" ~ "DENV4"),
         severe1997 = if_else(WHO1997 %in% c("DHF","DSS"),1,0),
         severe2009 = if_else(WHO2009 %in% c("SS"),1,0),
         study = case_when(grupo == "COHORTE" ~ "Cohort", 
                           grupo == "CLINICO" ~ "Hospital",
                           grupo == "TRAZA" ~ "Cohort")) #table 1 chance to Trace

### Figures are shown first with the analysis used to generate them###

Fig1 <- setd3 |> 
  select("Serotype", "IR", "FIS", "study") |>
  mutate(SerInf= paste(Serotype, IR))

cbbPalette <- c("#1A3263", "#3668D2", "#F15055", "#FF9191", "#FAB95B", "#F8FA74", "#989390", "#E8E2DB")


MFig1 <- ggplot(Fig1) +
  aes(x = FIS, fill = SerInf) +
  geom_bar(color = "#000000") +
  labs(x = "Years", 
       y= "Dengue positive cases", 
       fill = "Serotype") +
  #scale_fill_brewer(palette = "Set1", labels = c("DENV1-P", "DENV1-S", "DENV2-P", "DENV2-S", "DENV3-P", "DENV3-S", "DENV4-P", "DENV4-S")) +
  scale_fill_manual(values = cbbPalette, labels = c("DENV1-P", "DENV1-S", "DENV2-P", "DENV2-S", "DENV3-P", "DENV3-S", "DENV4-P", "DENV4-S")) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.box = "horizontal", 
        text = element_text(size = 12)) +
  guides(fill = guide_legend(label.position = "bottom", nrow = 1))

plot(MFig1)

############ severity models - overall Figure 2###################

setd4 = setd3 %>%
  mutate(IR = factor(IR, levels = c("P","S"),labels = c("Primary","Secondary")),
         `DENV Serotype` = Serotype,
         `DENV Serotype` = factor(`DENV Serotype` ,levels = c("DENV1","DENV2","DENV3","DENV4")))


mod1997.a = glm(severe1997 ~ `DENV Serotype`:IR + IR + `DENV Serotype` + Age + sexo, 
                data = setd4 %>% filter(`DENV Serotype` != "DENV4"), 
                family = binomial(link = "logit"))

mod2009.a = glm(severe2009 ~ `DENV Serotype`:IR + IR + `DENV Serotype`+ Age + sexo, 
                data = setd4 %>% filter(`DENV Serotype` != "DENV4"), 
                family = binomial(link = "logit"))


models = bind_rows(
  ggpredict(mod1997.a, terms = c("DENV Serotype","IR")) %>% as.data.frame() %>% mutate(study = "Overall", Clas = "WHO-1997"),
  ggpredict(mod2009.a, terms = c("DENV Serotype","IR")) %>% mutate(study = "Overall", Clas = "WHO-2009"),
  ) %>%
  mutate(study = factor(study, levels = c("Overall", "Cohort", "Hospital"))) 

#This removes the NA's from the data frame
models <- filter(models, !is.na(study))

Amodels <- filter(models, study == "Overall")

sev.mod.graph1 <- ggplot(Amodels, aes(x= x, y= predicted, color= group)) +
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high),
                  position = position_dodge(width = 0.5), 
                  size = 1, fatten = 3) +
  facet_grid(study~Clas + group, scales = "free", space = "free_x") + #remove study
  labs(x = " ", y = "Proportion of severity", title = "", color = "IR") +
  theme_bw(base_size = 10) +
  scale_color_manual(values = c('#FFCC00','#CC0033')) +
  geom_hline(aes(yintercept = intercept), data = Amodels  %>%
               mutate(DENV1 = if_else(x == "DENV1", predicted, NA_real_)) %>% 
               group_by(study, Clas, group) %>%
               mutate(intercept = first(na.omit(DENV1))),
             linetype = "dashed", color = "blue") 

plot(sev.mod.graph1)
ggsave(sev.mod.graph1, device = "png", filename = "Figure3.png", width = 7, height = 3, dpi = 300)

summary(mod1997.a)
summary(mod2009.a)

summary(models)

####Clinical signs and symptoms Figure 3####

setd5 <- setd3 |>
  select("choque_total","choque", "choquecompensado", "derrame", "ascitis", "malllenadocapilar","hemoconcentracion", "min_plaquetas_menores_100","min_plaquetas_menores_50", "hemomucosas","uci", "drogas_inotropicas", "vomitos", "dabdominal", "mialgia", "cefalea", "rash", "study", "IR", "Serotype", "Age", "sexo")

var.interes = c("choque_total","choque", "choquecompensado", "derrame", "ascitis", "malllenadocapilar","hemoconcentracion", "min_plaquetas_menores_100","min_plaquetas_menores_50", "hemomucosas","uci", "drogas_inotropicas", "vomitos", "dabdominal", "mialgia", "cefalea", "rash")

var.serotipos = c("1","2","3","4")

## univariate models ######
models <- list()
tidied_models <- list()

models.c <- list()
tidied_models.c <- list()

models.h <- list()
tidied_models.h <- list()

# Nested loop for logistic regression
for (x in var.interes) {
  
  
  # Construct the formula
  formula <- as.formula(paste(x, "~", "Serotype + IR + Age + sexo"))
  
  ############ All
  
  # Use tryCatch for error handling
  result <- tryCatch({
    # Fit the logistic regression model
    model <- glm(formula, data = setd5, family = binomial(link = "logit"))
    
    tidied_model <- tidy_add_header_rows(tidy_add_estimate_to_reference_rows(tidy_add_n(tidy_add_reference_rows(tidy_and_attach(model, conf.int = T, exponentiate = F)) ))) 
    tidied_model$model <- paste(x)
    tidied_models[[paste(x)]] <- tidied_model
    
    # Store the model in the list
    models[[paste(x)]] <- model
  }, error = function(e) {
    # Print the error message
    message("Failed to fit model for ", x, " ~ ", ": ", e$message)
  })
  
  
  ############## cohorte
  
  # Use tryCatch for error handling
  result.c <- tryCatch({
    # Fit the logistic regression model
    model.c <- glm(formula, data = setd5[setd5$study == "Cohort",], family = binomial(link = "logit"))
    
    tidied_model.c <- tidy_add_header_rows(tidy_add_estimate_to_reference_rows(tidy_add_n(tidy_add_reference_rows(tidy_and_attach(model.c, conf.int = T, exponentiate = F)) ))) 
    tidied_model.c$model <- paste(x)
    tidied_models.c[[paste(x)]] <- tidied_model.c
    
    # Store the model in the list
    models.c[[paste(x)]] <- model.c
  }, error = function(e) {
    # Print the error message
    message("Failed to fit model for ", x, " ~ ", ": ", e$message)
  })
  
  
  ############## Hospital
  
  # Use tryCatch for error handling
  result.h <- tryCatch({
    # Fit the logistic regression model
    model.h <- glm(formula, data = setd5[setd5$study == "Hospital",], family = binomial(link = "logit"))
    
    tidied_model.h <- tidy_add_header_rows(tidy_add_estimate_to_reference_rows(tidy_add_n(tidy_add_reference_rows(tidy_and_attach(model.h, conf.int = T, exponentiate = F)) ))) 
    tidied_model.h$model <- paste(x)
    tidied_models.h[[paste(x)]] <- tidied_model.h
    
    # Store the model in the list
    models.h[[paste(x)]] <- model.h
  }, error = function(e) {
    # Print the error message
    message("Failed to fit model for ", x, " ~ ", ": ", e$message)
  })
  
  
}

combined_models.a <- bind_rows(tidied_models, .id = "model") %>%
  filter(term != "(Intercept)") %>%
  mutate(var = str_split(model,simplify = T,pattern = "[[:space:]]")[,1]) %>%
  filter((std.error < 10 & estimate < 7& conf.high < 10) | is.na(std.error)) %>%
  mutate(conf.low = case_when(reference_row == T ~ 0, T ~ conf.low),
         conf.high = case_when(reference_row == T ~ 0, T ~ conf.high),
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high),
         panel = "All")

combined_models.c <- bind_rows(tidied_models.c, .id = "model") %>%
  filter(term != "(Intercept)") %>%
  mutate(var = str_split(model,simplify = T,pattern = "[[:space:]]")[,1]) %>%
  filter((std.error < 10 & estimate < 7& conf.high < 10) | is.na(std.error)) %>%
  mutate(conf.low = case_when(reference_row == T ~ 0, T ~ conf.low),
         conf.high = case_when(reference_row == T ~ 0, T ~ conf.high),
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high),
         panel = "Cohort") 

combined_models.h <- bind_rows(tidied_models.h, .id = "model") %>%
  filter(term != "(Intercept)") %>%
  mutate(var = str_split(model,simplify = T,pattern = "[[:space:]]")[,1]) %>%
  filter((std.error < 10 & estimate < 7& conf.high < 10) | is.na(std.error)) %>%
  mutate(conf.low = case_when(reference_row == T ~ 0, T ~ conf.low),
         conf.high = case_when(reference_row == T ~ 0, T ~ conf.high),
         estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high),
         panel = "Hospital") 

combined_models = bind_rows(combined_models.a, combined_models.h, combined_models.c)

# forestplot(
#   df = combined_models,
#   se = std.error,
#   estimate = estimate,
#   pvalue = p.value,
#   logodds = T,
#   colour = term,
#   shape = var2,
#   name = var,
#   title = "",
#   xlab = "OR"
# )

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
combined_models$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
combined_models$`OR (95% CI)` <- ifelse(is.na(combined_models$std.error), "",
                                        sprintf("%.2f (%.2f to %.2f)",
                                                combined_models$estimate, combined_models$conf.low, combined_models$conf.high))

combined_models = combined_models %>%
  mutate(term = case_when(grepl("DENV1",x = term)~ "DENV1",
                          grepl("DENV2",x = term)~ "DENV2",
                          grepl("DENV3",x = term)~ "DENV3",
                          grepl("DENV4",x = term)~ "DENV4"),
         interaction = interaction(term,var),
         p.star = gtools::stars.pval(p.value),
         label2 = case_when(reference_row == T ~ var,
                            T ~ " "),
         `Percentage (%)` =  round((n_event/n_obs)*100, digits = 2)) %>%
  rename(`Serotype` = label,
         `Clinical variable` = label2,
         `Observations`= n_obs,
         `Events`= n_event,
         `p value`= p.star) %>%
  filter(!Serotype %in% c("P","S","Age","F","M"))

combined_models$se <- 1/combined_models$p.value

#combined_modelsP = combined_models %>% filter(var2 == "P")
#combined_modelsS = combined_models %>% filter(var2 == "S")


tm <- forest_theme(base_size = 10,
                   refline_col = "red",
                   arrow_type = "closed",
                   footnote_col = "blue")


renamefunc <- function(x){
  x %>%
    mutate(`Clinical variable` = case_when(
      `Clinical variable` == "uci" ~ "ICU",
      `Clinical variable` == "drogas_inotropicas" ~ "Inotropic drugs",
      `Clinical variable` == "ascitis" ~ "Ascitis",
      `Clinical variable` == "rash" ~ "Rash",
      `Clinical variable` == "derrame" ~ "Pleural effusion",
      `Clinical variable` == "plasmaleak" ~ "Plasma leakage",
      `Clinical variable` == "dabdominal" ~ "Abdominal pain",
      `Clinical variable` == "hemomucosas" ~ "Mucosal and internal bleeding",
      `Clinical variable` == "hemoconcentracion" ~ "Hemoconcentration",
      `Clinical variable` == "choque_total" ~ "Shock (Hypo or Comp)",
      `Clinical variable` == "choque" ~ "Hypotensive shock",
      `Clinical variable` == "choquecompensado" ~ "Compensated shock",
      `Clinical variable` == "malllenadocapilar" ~ "Poor capillary refill", 
      `Clinical variable` == "vomitos" ~ "Vomiting", 
      `Clinical variable` == "cefalea" ~ "Headache",
      `Clinical variable` == "mialgia" ~ "Myalgia",
      `Clinical variable` == "artralgia" ~ "Artralgia",
      `Clinical variable` == "severe1997" ~ "Severe - clasification 1997",
      `Clinical variable` == "severe2009" ~ "Severe - clasification 2009",
      `Clinical variable` == "min_plaquetas_menores_100" ~ "Platelets < 100,000",
      `Clinical variable` == "min_plaquetas_menores_50" ~ "Platelets < 50,000",
      TRUE ~ `Clinical variable`
    ))
}

##All foresplot

combined_models.1.All = combined_models %>% 
  filter(panel == "All") %>%
  filter(var %in% 
           c("choque_total","choque", "choquecompensado", "derrame", "ascitis", "malllenadocapilar","hemoconcentracion", "min_plaquetas_menores_100","min_plaquetas_menores_50", "hemomucosas","uci", "drogas_inotropicas")) %>%
  renamefunc()

combined_models.2.All = combined_models %>% 
  filter(panel == "All") %>%
  filter(var %in% 
           c("dabdominal", "vomitos", "rash", "cefalea", "mialgia", "altralgia")) %>%
  renamefunc()

combined_models.1.All = combined_models %>% 
  filter(panel == "All") %>%
  filter(var %in% 
           c("choque_total","choque", "choquecompensado", "derrame", "ascitis", "malllenadocapilar","hemoconcentracion", "min_plaquetas_menores_100","min_plaquetas_menores_50", "hemomucosas","uci", "drogas_inotropicas")) %>%
  renamefunc()


combined_models.2.All = combined_models %>% 
  filter(panel == "All") %>%
  filter(var %in% 
           c("dabdominal", "vomitos", "rash", "cefalea", "mialgia", "altralgia")) %>%
  renamefunc() 

APS1 = forest(combined_models.1.All%>%
                select("Clinical variable", "Serotype","Events","Observations",
                       "Percentage (%)",  " ", "OR (95% CI)", "p value" ),
              est = combined_models.1.All$estimate,
              lower = combined_models.1.All$conf.low, 
              upper = combined_models.1.All$conf.high,
              sizes = 0.5,
              ci_column = 6,
              ref_line = 1,
              arrow_lab = c("Less likely than DENV1", "More likely than DENV1"),
              x_trans = "log",
              xlim = c(0.1, 10),
              # ticks_at = c(0, 0.5, 1, 2, 3),
              theme = tm)
BPS1 = forest(combined_models.2.All%>%
                select("Clinical variable", "Serotype","Events","Observations",
                       "Percentage (%)",  " ", "OR (95% CI)", "p value" ),
              est = combined_models.2.All$estimate,
              lower = combined_models.2.All$conf.low, 
              upper = combined_models.2.All$conf.high,
              sizes = 0.5,
              ci_column = 6,
              ref_line = 1,
              arrow_lab = c("Less likely than DENV1", "More likely than DENV1"),
              x_trans = "log",
              xlim = c(0.1, 10),
              # ticks_at = c(0, 0.5, 1, 2, 3),
              theme = tm)

plot(APS1)
plot(BPS1)



setd3 %>%
  select(Serotype, IR, severe1997, severe2009) %>%
  tbl_strata(
    strata = c(Serotype, IR),
    .tbl_fun = ~ .x %>%
      tbl_summary()
  )


