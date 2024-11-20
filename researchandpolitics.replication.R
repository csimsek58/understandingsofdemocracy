library(tidyverse)
library(readr)
library(broom)
library(stargazer)
library(janitor)
library(interplot)
library(sjPlot)
library(kableExtra)
library(dplyr)
library(lavaan)
library(semTools)
library(semPlot)
library(lme4)
library(ggplot2)
library(semptools)
library(tidySEM)

ess10 <- read_csv("C:/Users/csimsek2/OneDrive - Universität Münster/akademik/munster/data/ess10_bernd.csv")

#removing missingness
ess10 <- ess10 [ which(ess10$stfdem!=77 & ess10$stfdem !=88 & ess10$stfdem !=99), ]
ess10 <- ess10 [ which(ess10$agea!=999), ]
ess10 <- ess10 [ which(ess10$hinctnta!=77 & ess10$hinctnta!=88 & ess10$hinctnta!=99), ]
ess10 <- ess10 [ which(ess10$eduyrs!=77 & ess10$eduyrs!=88 & ess10$eduyrs!=99), ]
ess10$eduyrs <- ifelse(ess10$eduyrs <= 25, ess10$eduyrs, 25) #dealing with exreme values
ess10 <- ess10 [ which(ess10$gndr!=9), ]

#creating the understanding of democracy dummies
ess10$socdem_core<-0
ess10$socdem_core[ess10$impdema==3 | ess10$impdemb==4 | ess10$impdemc==5 | ess10$impdemd==1 | ess10$impdeme==2]<-1
ess10$libdem_core<-0
ess10$libdem_core[ess10$impdema==2 | ess10$impdemb==3 | ess10$impdemc==4 | ess10$impdemd==5 |ess10$impdeme==1]<-1
ess10$elecdem_core<-0
ess10$elecdem_core[ess10$impdema==1 | ess10$impdemb==2 | ess10$impdemc==3 | ess10$impdemd==4 |ess10$impdeme==5]<-1

ess10$uod_choice=NA
ess10$uod_choice[ess10$socdem_core==1]="Social"
ess10$uod_choice[ess10$libdem_core==1]="Liberal"
ess10$uod_choice[ess10$elecdem_core==1]="Electoral"
ess10$uod_choice[ess10$popdem_core==1]="Populist"
ess10$uod_choice[ess10$directdem_core==1]="Direct"

ess10 <- ess10[!is.na(ess10$uod_choice), ]#removing missingness

ess10$uod_choice <- relevel(ess10$uod_choice, ref = "Electoral") #setting the baseline category

#figure 1

b<-as.data.frame(table(ess10$uod_choice))

ggplot(b, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "gray", alpha = 0.7) +
  theme_minimal() +
  labs(#title = "Distribution of conceptions of democracy",
    x = "Conception of Democracy", y = "Frequency",
    caption = "Source: ESS Data Round 10, N=43763" 
  )+
  geom_text(aes(label = Freq), vjust = -0.5, size = 3)+
  ylim(0, 20000)+
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"))+
  theme(axis.title.y = element_text(face = "bold"))+
  theme(axis.title.x = element_text(face = "bold"))+
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.caption = element_text(
      size = 11,    
      face = "italic", 
      hjust = 0.5,    
      vjust = 1,      
      margin = margin(t = 10) 
    ),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20) 
  )

#min-max transforming variables for the lmer model

ess10 <- ess10 %>%
  mutate(
    agea_std = (agea - min(agea)) / (max(agea) - min(agea)),
    hinctnta_std = (hinctnta - min(hinctnta)) / (max(hinctnta) - min(hinctnta)),
    eduyrs_std = (eduyrs - min(eduyrs)) / (max(eduyrs) - min(eduyrs)),
  )

uod_stfdem3 <- lmer(stfdem ~ libdem_core + socdem_core + directdem_core + popdem_core +
                      agea_std + hinctnta_std + eduyrs_std + gndr + (1 | cntry),
                    data = ess10)

summary(uod_stfdem3)
stargazer(uod_stfdem3, type="latex")

#creating figure 2 

plot_model(uod_stfdem3, type = "est", show.ci = TRUE, show.values = FALSE,
           colors = "black", axis.labels = c("Gender", "Education", "Income", "Age",
                                             "Populist Democracy", "Direct Democracy", 
                                             "Social Democracy", "Liberal Democracy")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  labs(
    title = "",
    caption = "Min-max normalized coefficients with 95 % confidence intervals,
    Linear mixed-effects model with random country intercepts,
    ESS Data Round 10, N=43763, Marginal R-squared:0.037, Conditional R-squared:0.20" 
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.caption = element_text(
      size = 11,    
      face = "italic", 
      hjust = 0.5,     
      vjust = 1,      
      margin = margin(t = 10) 
    ),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20)
  )

#sem conventional

model_trad <- '
  # regressions
  stfdem ~ a1*socdem_core + a2*libdem_core + a3*directdem_core + a4*popdem_core
  part_tradition ~ b1*stfdem + b2*socdem_core + b3*libdem_core + b4*directdem_core + b5*popdem_core + agea + hinctnta + eduyrs + gndr

  # indirect effects
  indirect_socdem_core := a1 * b1
  indirect_libdem_core := a2 * b1
  indirect_directdem_core := a3 * b1
  indirect_popdem_core := a4 * b1

  # total effects (optional)
  total_socdem_core := b2 + indirect_socdem_core
  total_libdem_core := b3 + indirect_libdem_core
  total_directdem_core := b4 + indirect_directdem_core
  total_popdem_core := b5 + indirect_popdem_core
'

fit_trad<-sem(model=model_trad, data=ess10, cluster ="cntry")
summary(fit_trad,standardized=T, fit.measures=T)

fit_trad_boot<-sem(model=fit_trad, data=ess10, cluster ="cntry",se = "bootstrap",
                   bootstrap = 1000,
                   parallel = "snow",
                   ncpus = 4,
                   iseed = 1234)

summary(fit_trad_boot, standardized=T, fit.measures=T)

#creating the sem plot (we have not used R forour SEM plots but this will produce similar looking plot).

lay <- get_layout("agea", "", "",
                  "hinctnta", "", "",
                  "eduyrs","","",
                  "gndr","","",
                  "socdem_core","","part_tradition",
                  "directdem_core", "","",
                  "libdem_core","","",
                  "popdem_core","stfdem","", rows = 8)



plot <- semPaths(fit_trad, what="path",
                 whatLabels = "est",
                 layout = lay,
                 sizeMan = 10,
                 edge.label.cex = 1,
                 style = "ram",
                 nCharNodes = 0, nCharEdges = 0,
                 intercepts ='FALSE',
                 label.cex=1,
                 label.font=1,
                 label.prop=0.9,
                 equalizeManifests=F,
                 reorder=F,
                 allvars=F,
                 residuals =F)
#label.scale=F)
#layout = m,)


plot2 <- mark_sig(plot, fit_trad)
plot(plot2)


plot3 <- change_node_label(plot2,
                           c(stfdem = "Satisfaction",
                             part_tradition = "Conventional",
                             socdem_core = "Social",
                             libdem_core = "Liberal",
                             directdem_core="Direct",
                             popdem_core="Populist",
                             agea="Age",
                             hinctnta="Income",
                             eduyrs="Education",
                             gndr="Gender")
)
plot(plot3)

#unconventional model
model_protest <- '
  # regressions
  stfdem ~ a1*socdem_core + a2*libdem_core + a3*directdem_core + a4*popdem_core
  part_protest ~ b1*stfdem + b2*socdem_core + b3*libdem_core + b4*directdem_core + b5*popdem_core + agea + hinctnta + eduyrs + gndr

  # indirect effects
  indirect_socdem_core := a1 * b1
  indirect_libdem_core := a2 * b1
  indirect_directdem_core := a3 * b1
  indirect_popdem_core := a4 * b1

  # total effects 
  total_socdem_core := b2 + indirect_socdem_core
  total_libdem_core := b3 + indirect_libdem_core
  total_directdem_core := b4 + indirect_directdem_core
  total_popdem_core := b5 + indirect_popdem_core
'

fit_protest <- sem(model_protest, cluster="cntry",data = ess10)
summary(fit_protest, standardized=TRUE,fit.measures=TRUE)

fit_protest_boot<-sem(model=model_protest, data=ess10, cluster ="cntry",se = "bootstrap",
                      bootstrap = 1000,
                      parallel = "snow",
                      ncpus = 4,
                      iseed = 1234)

summary(fit_protest_boot,standardized=TRUE,fit.measures=TRUE)

#creating the sem plot (we have not used R forour SEM plots but this will produce similar looking plot).

lay <- get_layout("agea", "", "",
                  "hinctnta", "", "",
                  "eduyrs","","",
                  "gndr","","",
                  "socdem_core","","part_protest",
                  "directdem_core", "","",
                  "libdem_core","","",
                  "popdem_core","stfdem","", rows = 8)



plot <- semPaths(fit_protest, what="path",
                 whatLabels = "std",
                 layout = lay,
                 sizeMan = 10,
                 edge.label.cex = 1,
                 style = "ram",
                 nCharNodes = 0, nCharEdges = 0,
                 intercepts ='FALSE',
                 label.cex=1,
                 label.font=1,
                 label.prop=0.9,
                 equalizeManifests=F,
                 reorder=F,
                 allvars=F,
                 residuals =F)
#label.scale=F)
#layout = m,)


plot2 <- mark_sig(plot, fit_protest)
plot(plot2)


plot3 <- change_node_label(plot2,
                           c(stfdem = "Satisfaction",
                             part_protest = "Unconventional",
                             socdem_core = "Social",
                             libdem_core = "Liberal",
                             directdem_core="Direct",
                             popdem_core="Populist",
                             agea="Age",
                             hinctnta="Income",
                             eduyrs="Education",
                             gndr="Gender")
)
plot(plot3)

