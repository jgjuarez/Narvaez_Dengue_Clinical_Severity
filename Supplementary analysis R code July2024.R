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
  filter(RFDENGUE == "Positivo",
         serotipo >= 1 & serotipo <= 4,
         IR != "I") |>
  mutate(Serotype = case_when(serotipo == "1" ~ "DENV1",
                              serotipo == "2" ~ "DENV2",
                              serotipo == "3" ~ "DENV3",
                              serotipo == "4" ~ "DENV4"),
         severe1997 = if_else(WHO1997 %in% c("DHF","DSS"),1,0),
         severe2009 = if_else(WHO2009 %in% c("SS"),1,0),
         study = case_when(grupo == "COHORTE" ~ "Cohort", 
                           grupo == "CLINICO" ~ "Hospital",
                           grupo == "TRAZA" ~ "Cohort")) #table 1 change to Trace

setd3 |> select(Serotype, IR, severe1997, severe2009) |> 
  gtsummary::tbl_summary(by = Serotype)

print(missing_values)
### Figures are shown first with the analysis used to generate them###

Fig1 <- setd3 |> 
  select("Serotype", "IR", "FIS", "study") |>
  mutate(SerInf= paste(Serotype, IR))

cbbPalette <- c("#3668D2", "#1A3263", "#FF9191", "#F15055", "#F8FA74", "#FAB95B", "#E8E2DB", "#989390")


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

SFig1 <- ggplot(Fig1) +
  aes(x = FIS, fill = SerInf) +
  geom_bar(color = "#000000") +
  labs(x = "Years", 
       y= "Dengue positive cases", 
       fill = "Serotype") +
  # scale_fill_brewer(palette = "BuGn", labels = c("DENV1-P",  "DENV1-S",  "DENV2-P",  "DENV2-S", "DENV3-P", "DENV3-S", "DENV4-P", "DENV4-S")) +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(facets = "study", nrow = 3) + #Remove this line for main Figure 1
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.box = "horizontal", 
        text = element_text(size = 12)) +
  guides(fill = guide_legend(label.position = "bottom", nrow = 1))

plot(MFig1)
plot(SFig1)

ggsave(MFig1, device = "png", filename = "MFig1.png", width = 8, height = 5, dpi = 300)
ggsave(SFig1, device = "png", filename = "SFig1.png", width = 8, height = 5, dpi = 300)

Fig2.1 <- setd3 |>
  select("severe2009", 
         "Serotype", 
         "IR",
         "study") |>
  mutate(severe2009 = case_when(severe2009 == "0" ~ "not severe", 
                                severe2009 == "1" ~ "severe"), 
         Classification = "WHO-2009")|>
  rename(Severity = severe2009) |>
  filter(Severity == "severe")

Fig2.2 <- setd3 |>
  select("severe1997", 
         "Serotype", 
         "IR",
         "study") |>
  mutate(severe1997 = case_when(severe1997 == "0" ~ "not severe", 
                                severe1997 == "1" ~ "severe"), 
         Classification = "WHO-1997")|>
  rename(Severity = severe1997) |>
  filter(Severity == "severe") 

FigTest <- bind_rows(Fig2.1, Fig2.2) 

Fig2 <- FigTest |>
  count(Serotype, IR, Classification) 

Fig2Per <- Fig2 |>
  group_by(Serotype, Classification) |>
  mutate(Percentage = formattable::percent(n / sum(n)))

MFig2 <- ggplot(Fig2Per) + 
  aes(x= Serotype, y= Percentage, fill= IR) +
  geom_bar(stat = "identity", width= 0.8) +
  facet_wrap(vars(Classification)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        text = element_text(size = 20)) +
  scale_fill_manual(values = c('#FFCC00','#CC0033')) +
  labs(x= "Serotype", 
       y= "Distribution by IR among severe cases") +
  geom_text(aes(label= n, colour = IR), fontface = "bold",position =
              position_stack(vjust = 0.5), show.legend = FALSE) +
  scale_colour_manual(values = c(P= "black", S= "white")) +
  theme_classic() 

plot(MFig2)

ggsave(MFig2, device = "png", filename = "Fig2Per.png", width = 5, height = 3, dpi = 300)

SFig2 <- FigTest |>
  count(Serotype, IR, study, Classification) 

SFig2Per <- SFig2 |>
  group_by(Serotype, study, Classification) |>
  mutate(Percentage = formattable::percent(n / sum(n)))

SupFig2 <- ggplot(SFig2Per) + 
  aes(x= Serotype, y= Percentage, fill= IR) +
  geom_bar(stat = "identity", width= 0.8) +
  facet_grid(study~Classification, scales = "free", space = "free_x") + #remove study
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c('#FFCC00','#CC0033')) +
  labs(x= "Serotype", 
       y= "Distribution by IR among severe cases") +
  geom_text(aes(label= n, colour = IR), 
            fontface = "bold",
            position = position_stack(vjust = 0.5), 
            show.legend = FALSE) +
  scale_colour_manual(values = c(P= "black", S= "white")) +
  theme_classic() 

plot(SupFig2)

ggsave(SupFig2, device = "png", filename = "SFig2Per.png", width = 7, height = 7 , dpi = 300)


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


setd4 = setd3 %>%
  mutate(IR = factor(IR, levels = c("P","S"),labels = c("Primary","Secondary")),
         `DENV Serotype` = Serotype,
         `DENV Serotype` = factor(`DENV Serotype`, levels = c("DENV1","DENV2","DENV3","DENV4")))


mod1997.a = glm(severe1997 ~ `DENV Serotype`:IR + IR + `DENV Serotype` + Age + sexo, 
                data = setd4 %>% filter(`DENV Serotype` != "DENV4"), 
                family = binomial(link = "logit"))

mod2009.a = glm(severe2009 ~ `DENV Serotype`:IR + IR + `DENV Serotype`+ Age + sexo, 
                data = setd4 %>% filter(`DENV Serotype` != "DENV4"), 
                family = binomial(link = "logit"))

mod1997.h = glm(severe1997 ~ `DENV Serotype`:IR + IR + `DENV Serotype`+ Age + sexo, 
                data = setd4 %>% filter(`DENV Serotype` != "DENV4", study == "Hospital"), 
                family = binomial(link = "logit"))

mod2009.h = glm(severe2009 ~ `DENV Serotype`:IR + IR + `DENV Serotype`+ Age + sexo, 
                data = setd4 %>% filter(`DENV Serotype` != "DENV4", study == "Hospital"), 
                family = binomial(link = "logit"))
mod1997.c = glm(severe1997 ~ `DENV Serotype`:IR + IR + `DENV Serotype`+ Age + sexo, 
                data = setd4 %>% filter(`DENV Serotype` != "DENV4", study == "Cohort"), 
                family = binomial(link = "logit"))

mod2009.c = glm(severe2009 ~ `DENV Serotype`:IR + IR + `DENV Serotype`+ Age + sexo, 
                data = setd4 %>% filter(`DENV Serotype` != "DENV4", study == "Cohort"), 
                family = binomial(link = "logit"))

models = bind_rows(
  ggpredict(mod1997.a, terms = c("DENV Serotype","IR")) %>% as.data.frame() %>% mutate(study = "Overall", Clas = "WHO-1997"),
  ggpredict(mod2009.a, terms = c("DENV Serotype","IR")) %>% mutate(study = "Overall", Clas = "WHO-2009"),
  ggpredict(mod1997.h, terms = c("DENV Serotype","IR")) %>% mutate(study = "Hospital", Clas = "WHO-1997"),
  ggpredict(mod2009.h, terms = c("DENV Serotype","IR")) %>% mutate(study = "Hospital", Clas = "WHO-2009"),
  ggpredict(mod1997.c, terms = c("DENV Serotype","IR")) %>% mutate(study = "Cohort", Clas = "WHO-1997"),
  ggpredict(mod2009.c, terms = c("DENV Serotype","IR")) %>% mutate(study = "Cohort", Clas = "WHO-2009"),
  
) %>%
  mutate(study = factor(study, levels = c("Overall", "Cohort", "Hospital"))) 

#This removes the NA's from the data frame
models <- filter(models, !is.na(study))

Amodels <- filter(models, study == "Overall")
CHmodels <- filter(models, study != "Overall")

sev.mod.graph2 = ggplot(CHmodels, aes(y = predicted, x = x, color = group)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5), size = 1, fatten = 3) + 
  facet_grid(study~Clas + group, scales = "free", space = "free_x") +
  labs(x = " ", y = "Proportion of severity",
       title = "", color = "IR") + theme_bw(base_size = 10) +
  scale_color_manual(values = c('#FFCC00','#CC0033')) +
  geom_hline(aes(yintercept = intercept), data = CHmodels %>% 
               mutate(DENV1 = if_else(x == "DENV1", predicted, NA_real_)) %>% 
               group_by(group, study, Clas) %>%
               mutate(intercept = first(na.omit(DENV1))),
             linetype = "dashed", color = "blue")

plot(sev.mod.graph2)
ggsave(sev.mod.graph2, device = "png", filename = "SFigure3.png", width = 7, height = 5, dpi = 300)



####Clinical signs and symptoms Figure 4####

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
           c("choque", "choquecompensado", "derrame", "ascitis", "malllenadocapilar","hemoconcentracion", "min_plaquetas_menores_100","min_plaquetas_menores_50", "hemomucosas","uci", "drogas_inotropicas")) %>%
  renamefunc()

combined_models.2.All = combined_models %>% 
  filter(panel == "All") %>%
  filter(var %in% 
           c("dabdominal", "vomitos", "rash", "cefalea", "mialgia", "altralgia")) %>%
  renamefunc()

combined_models.1.All = combined_models %>% 
  filter(panel == "All") %>%
  filter(var %in% 
           c("choque", "choquecompensado", "derrame", "ascitis", "malllenadocapilar","hemoconcentracion", "min_plaquetas_menores_100","min_plaquetas_menores_50", "hemomucosas","uci", "drogas_inotropicas")) %>%
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

ggsave(APS1, filename = "A - P+S cases - forestplot - All.png", device = "png",width = 11, height = 12)
ggsave(BPS1, filename = "B - P+S cases - forestplot - All.png", device = "png",width = 10, height = 6)


##Tables
Table1 <- setd3 |>
  select(Age, Edaden, Serotype, IR, sexo, study) |>
  mutate(Edaden = case_when(Edaden == "M" ~ 0,
                            Edaden == "A" ~ 1),
         Age = Age * Edaden, 
         ranges = cut(Age,
                      c(0,
                        5,
                        10,
                        14,
                        17)),
         ranges = case_when(is.na(ranges) == T ~ "< 1",
                            ranges == "(0,5]" ~ "1 to 5",
                            ranges == "(5,10]" ~ "6 to 10",
                            ranges == "(10,14]" ~ "11 to 14",
                            ranges == "(14,17]" ~ "15 to 17"),  
         SerInf = paste(Serotype, IR)) |>
  arrange(desc(ranges))


serotipos <- c("DENV1", "DENV2", "DENV3", "DENV4")

Table2 <- Table1 |>
  select(ranges,Serotype, sexo, study) |>
  gtsummary::tbl_summary(by = study) 

print(Table2)

CTable1 <- filter(Table1, study == "Cohort")
HTable1 <- filter(Table1, study == "Hospital")
Trace <- filter(Table1, study == "Trace")

mean(CTable1$Age)
sd(CTable1$Age)

mean(HTable1$Age)
sd(HTable1$Age)

mean(Trace$Age)
sd(Trace$Age)

Table1 <- setd3 |>
  select(Age, Edaden, Serotype, IR, sexo, study) |>
  mutate(Edaden = case_when(Edaden == "M" ~ 0,
                            Edaden == "A" ~ 1),
         Age = Age * Edaden, 
         ranges = cut(Age,
                      c(0,
                        5,
                        10,
                        14,
                        17)),
         ranges = case_when(is.na(ranges) == T ~ "< 1",
                            ranges == "(0,5]" ~ "1 to 5",
                            ranges == "(5,10]" ~ "6 to 10",
                            ranges == "(10,14]" ~ "11 to 14",
                            ranges == "(14,17]" ~ "15 to 17"),   
         SerInf = paste(Serotype, IR)) |>
  arrange(desc(ranges))

new_order <- c("< 1","1 to 5","6 to 10","11 to 14", "15 to 17")

Table1$ranges <- factor(Table1$ranges, levels = new_order)

# Define the Serotype values
serotipos <- c("DENV1", "DENV2", "DENV3", "DENV4")

#Balma´s style
# Initialize an empty list to store the summary tables
summary_tables <- list()

# Loop through each serotipo value
for (Serotype in serotipos) {
  filtered_table <- Table1 |>
    filter(Serotype == !!Serotype) |>
    select(SerInf, ranges, sexo) |>
    rename(Ages = ranges, Sex = sexo) |>
    tbl_summary(by = SerInf, percent = "row")
  
  summary_tables[[Serotype]] <- filtered_table
}

# Merge the summary tables into a single table with spanner labels
Acombined_tbl1 <- tbl_merge(tbls = summary_tables, tab_spanner = serotipos) |>
  as_kable_extra(format = "html", 
                 booktabs = T, 
                 caption = "Overall DENV by immune status",
                 col.names = c(" ", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary")) |>
  kable_styling(position = "left",
                latex_options = c("striped", 
                                  "hold_position"),
                full_width = T, 
                font_size = 12) 

CTable1 <- filter(Table1, study == "Cohort")
HTable1 <- filter(Table1, study == "Hospital")


# Now the Cohort
for (Serotype in serotipos) {
  filtered_table <- CTable1 |>
    filter(Serotype == !!Serotype) |>
    select(SerInf, ranges, sexo) |>
    rename(Ages = ranges, Sex = sexo) |>
    tbl_summary(by = SerInf, percent = "row")
  
  summary_tables[[Serotype]] <- filtered_table
}

Ccombined_tbl1 <- tbl_merge(tbls = summary_tables, tab_spanner = serotipos) |>
  as_kable_extra(format = "html", 
                 booktabs = T, 
                 caption = "Cohort DENV by immune status",
                 col.names = c(" ", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary")) |>
  kable_styling(position = "left",
                latex_options = c("striped", 
                                  "hold_position"),
                full_width = T, 
                font_size = 12) 

#Now the Hospital

for (Serotype in serotipos) {
  filtered_table <- HTable1 |>
    filter(Serotype == !!Serotype) |>
    select(SerInf, ranges, sexo) |>
    rename(Ages = ranges, Sex = sexo) |>
    tbl_summary(by = SerInf, percent = "row")
  
  summary_tables[[Serotype]] <- filtered_table
}

Hcombined_tbl1 <- tbl_merge(tbls = summary_tables, tab_spanner = serotipos) |>
  as_kable_extra(format = "html", 
                 booktabs = T, 
                 caption = "Hospital DENV by immune status",
                 col.names = c(" ", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary", 
                               "Primary", 
                               "Secondary")) |>
  kable_styling(position = "left",
                latex_options = c("striped", 
                                  "hold_position"),
                full_width = T, 
                font_size = 12) 

Acombined_tbl1
Ccombined_tbl1
Hcombined_tbl1

#Other version with percentages by col
Table1 |> select(SerInf, ranges, sexo) |> rename(Ages = ranges, Sex = sexo) |> gtsummary::tbl_summary(by = SerInf)  


Table2 <- setd3 |>
  mutate(SerInf = paste(Serotype, 
                        IR)) |>
  select("WHO1997", 
         "WHO2009",
         "SerInf", 
         "study") |>
  filter(WHO2009 != "No cumple criterio de sospecha de dengue") |>
  mutate(WHO2009 = case_when(WHO2009 == "SA" ~ "DWW",
                             WHO2009 == "SS" ~  "SD",
                             WHO2009 == "SSA" ~ "DWoWS"),
         WHO2009 = factor(WHO2009, levels = c("DWoWS", "DWW", "SD"))) 


Table2 |>
  select(WHO1997,WHO2009,SerInf) |>
  gtsummary::tbl_summary(by = SerInf) |> 
  gtsummary::as_kable_extra(format = "html", 
                            booktabs = T, 
                            caption = "Complete Study dengue cases by WHO 1997 and 2009 classification", 
                            col.names = c(" ", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary")
  ) |>
  kableExtra::kable_styling(position = "left",
                            latex_options = c("striped", 
                                              "hold_position"),
                            full_width = T, 
                            font_size = 12) |>
  add_header_above(header = c(" " = 1, 
                              "Dengue 1" = 2, 
                              "Dengue 2" = 2, 
                              "Dengue 3" = 2, 
                              "Dengue 4"= 2)) 


Table2 |>
  filter(study == "Cohort") |>
  select(WHO1997,WHO2009, SerInf) |>
  gtsummary::tbl_summary(by = SerInf) |> 
  gtsummary::as_kable_extra(format = "html", 
                            booktabs = T, 
                            caption = "Cohort Study dengue cases by WHO 1997 and 2009 classification", 
                            col.names = c(" ", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary")
  ) |>
  kableExtra::kable_styling(position = "left",
                            latex_options = c("striped", 
                                              "hold_position"),
                            full_width = T, 
                            font_size = 12) |>
  add_header_above(header = c(" " = 1, 
                              "Dengue 1" = 2, 
                              "Dengue 2" = 2, 
                              "Dengue 3" = 2, 
                              "Dengue 4"= 2)) 

Table2 |>
  filter(study == "Hospital") |>
  select(WHO1997,WHO2009, SerInf) |>
  gtsummary::tbl_summary(by = SerInf) |> 
  gtsummary::as_kable_extra(format = "html", 
                            booktabs = T, 
                            caption = "Hospital Study dengue cases by WHO 1997 and 2009 classification", 
                            col.names = c(" ", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary", 
                                          "Primary", 
                                          "Secondary")
  ) |>
  kableExtra::kable_styling(position = "left",
                            latex_options = c("striped", 
                                              "hold_position"),
                            full_width = T, 
                            font_size = 12) |>
  add_header_above(header = c(" " = 1, 
                              "Dengue 1" = 2, 
                              "Dengue 2" = 2, 
                              "Dengue 3" = 2, 
                              "Dengue 4"= 2)) 














#Testing just setting up DENV4 into the model, does not work since we have a very small sample size for primary
setd4 = setd3 %>%
  mutate(IR = factor(IR, levels = c("P","S"),labels = c("Primary","Secondary")),
         `DENV Serotype` = Serotype,
         `DENV Serotype` = factor(`DENV Serotype` ,levels = c("DENV1","DENV2","DENV3","DENV4")))


mod1997.a = glm(severe1997 ~ `DENV Serotype`:IR + IR + `DENV Serotype` + Age + sexo, 
                data = setd4, 
                family = binomial(link = "logit"))

mod2009.a = glm(severe2009 ~ `DENV Serotype`:IR + IR + `DENV Serotype`+ Age + sexo, 
                data = setd4, 
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
