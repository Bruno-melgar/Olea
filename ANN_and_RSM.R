rm(list = ls())     # clear objects  
graphics.off() 
########################################
###### Olea #########
########################################



# Packages ----------------------------------------------------------------

inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "rgl", "fpc","cluster", "readxl", "magrittr","multipanelfigure", 
              "neuralnet", "MASS", "sigmoid", "Metrics")
inst(packages)



# Dataframes ------------------------------------------------

(ccd <- read_excel("~/Desktop/Meta_and_df.xlsx", 
                           sheet = "DF2", col_types = c("skip", 
                                                        "skip", "numeric", "skip", "skip", 
                                                        "skip", "numeric", "numeric", "numeric", 
                                                        "skip", "skip", "skip", "numeric", 
                                                        "numeric", "numeric")))
(bmc <- read_excel("~/Desktop/Meta_and_df.xlsx", 
                   sheet = "DF3", col_types = c("skip", 
                                                "skip", "skip", "skip", "numeric", 
                                                "numeric", "skip", "numeric", "numeric", 
                                                "skip", "numeric", "numeric", "skip")))

(fit <- read_excel("~/Desktop/Meta_and_df.xlsx", 
                   sheet = "DF3", col_types = c("numeric", 
                                                "skip", "skip", "skip", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric")))


# Manipulation ----------------------------------------------------------------

(df <- ccd %>% 
  group_by(Run) %>% 
  summarize_all(mean) %>% 
  column_to_rownames("Run") %>% 
  rename(x1 = `Temperature (Coded)`, 
         x2 = `Extraction time (Coded)`, 
         x3 = `Ramp time (Coded)`, 
         Tyosol = `Tyrosol (mg/g)`, 
         Verbascoside = `Verbacoside (mg/g)`, 
         Oleuropein = `Oleuropein (mg/g)`) %>% 
  as_tibble())

(bmc <- bmc %>% 
    rename(olea = `Oleuropein Experimental (mg/g extract)`, 
           polea = `Oleuropein RSM (mg/g extract)`, 
           tyr = `Tyrosol Experimental (mg/g extract)`, 
           ptyr = `Tyrosol RSM (mg/g extract)`, 
           verb = `Verbascoside Experimental (mg/g extract)`, 
           pverb = `Verbascoside RSM (mg/g extract)`) %>% 
    as_tibble())

(fit <- fit %>% 
    rename(run = `Run`, 
           olea = `Oleuropein Experimental (mg/g extract)`, 
           rsmolea = `Oleuropein RSM (mg/g extract)`, 
           annolea = `Oleuropein ANN (mg/g extract)`,
           tyr = `Tyrosol Experimental (mg/g extract)`, 
           rsmtyr = `Tyrosol RSM (mg/g extract)`, 
           anntyr = `Tyrosol ANN (mg/g extract)`,
           verb = `Verbascoside Experimental (mg/g extract)`, 
           rsmverb = `Verbascoside RSM (mg/g extract)`, 
           annverb = `Verbascoside ANN (mg/g extract)`) %>% 
    as_tibble())


# Modeling construction  ---------------------------------------------------
## Regresion model Tyrosol --------------------------------------------------

nn_tyr= neuralnet(Tyosol ~ x1 + x2 + x3, 
                  data=df,
                  hidden = 3, 
                  err.fct = "sse", 
                  threshold = 0.05,
                  linear.output = TRUE)
summary(nn_tyr)
nn_tyr
nn_tyr$result.matrix
nn_tyr$net.result
plot(nn_tyr)

capture.output(nn_tyr, file = "Enero_tyr_ann.txt")

## Regresion model Verbascoside -------------------------------------------------

nn_verb= neuralnet(Verbascoside ~ x1+x2+x3, 
                   data=df,
                   hidden = 3, 
                   err.fct = "sse", 
                   threshold = 0.05,
                   linear.output = TRUE)
summary(nn_verb)
nn_verb
nn_verb$result.matrix
nn_verb$net.result
plot(nn_verb)

capture.output(nn_verb, file = "Enero_verb_ann.txt")

## Regresion model Oleuropein -------------------------------------------------

custom <- function(x) {log(1+exp(x))} #Softplus
nn_olea= neuralnet(Oleuropein ~ x1+x2+x3, 
                   data=df,
                   hidden = 3,
                   act.fct = custom,
                   err.fct = "sse", 
                   threshold = 0.05,
                   linear.output = TRUE)
summary(nn_olea)
nn_olea
nn_olea$result.matrix
nn_olea$net.result
plot(nn_olea)

capture.output(nn_olea, file = "Enero_olea_ann.txt")



# Confirmation points  --------------------------------------------------

c1=c(1,1,-1,-1)
c2=c(1,1,0.787,1)
c3=c(-0.756,-1,1,-1)
test=data.frame(c1,c2,c3)

Predict1<-compute(nn_tyr,test)
Predict1$net.result

Predict2<-compute(nn_verb,test)
Predict2$net.result

Predict3<-compute(nn_olea,test)
Predict3$net.result

Pred<-cbind(Predict1$net.result,Predict2$net.result,Predict3$net.result)
write.table(Pred, "pred.csv", sep = ",")



# ANN Loss calculators ---------------------------------------------------------

#1. ROOT MEAN SQUARE  ERROR (RMSE)
tyrrsme<-rmse(df$Tyosol,nn_tyr$net.result[[1]])
verbrsme<-rmse(df$Verbascoside,nn_verb$net.result[[1]])
olearsme<-rmse(df$Oleuropein,nn_olea$net.result[[1]])

#2. MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
tyrmape<-mean(abs((df$Tyosol-nn_tyr$net.result[[1]])/df$Tyosol)) * 100
verbmape<-mean(abs((df$Verbascoside-nn_verb$net.result[[1]])/df$Verbascoside)) * 100
oleamape<-mean(abs((df$Oleuropein-nn_olea$net.result[[1]])/df$Oleuropein)) * 100

#3. R SQUARED error metric -- Coefficient of Determination
tyrr2<-cor(df$Tyosol,nn_tyr$net.result[[1]])^2
verbr2<-cor(df$Verbascoside,nn_verb$net.result[[1]])^2
olear2<-cor(df$Oleuropein,nn_olea$net.result[[1]])^2

loss<-cbind(tyrrsme,tyrmape,tyrr2,verbrsme,verbmape,verbr2,olearsme,oleamape,olear2)
write.table(loss, "loss.csv", sep = ",")



# RSM Loss calculators -------------------------

#1. ROOT MEAN SQUARE  ERROR (RMSE)
rsmtyrrsme<-rmse(bmc$tyr,bmc$ptyr)
rsmverbrsme<-rmse(bmc$verb,bmc$pverb)
rsmolearsme<-rmse(bmc$olea,bmc$polea)

#2. MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
rsmtyrmape<-mean(abs((bmc$tyr-bmc$ptyr)/bmc$tyr)) * 100
rsmverbmape<-mean(abs((bmc$verb-bmc$pverb)/bmc$verb)) * 100
rsmoleamape<-mean(abs((bmc$olea-bmc$polea)/bmc$olea)) * 100

#3. R SQUARED error metric -- Coefficient of Determination
rsmtyrr2<-cor(bmc$tyr,bmc$ptyr)^2
rsmverbr2<-cor(bmc$verb,bmc$pverb)^2
rsmolear2<-cor(bmc$verb,bmc$pverb)^2

lossrsm<-cbind(rsmtyrrsme,rsmtyrmape,rsmtyrr2,rsmverbrsme,rsmverbmape,rsmverbr2,rsmolearsme,rsmoleamape,rsmolear2)
write.table(lossrsm, "lossrsm.csv", sep = ",")



# Actual vs Predicted plot (RSM vs ANN)------------------------------------

ggplot() +
  geom_point(data = df, aes(nn_tyr$net.result[[1]], Tyosol), size = 3, alpha=0.6, shape= 22, fill="#0D2C84") +
  geom_abline(linetype = "dotted", color="#0D2C84", size = 1) +
  geom_point(data = bmc, aes(ptyr, tyr), size = 3, alpha=0.6, shape= 21, fill="#41B6C4") +
  geom_abline(color="#41B6C4") +
  labs(x='Predicted Values', y='Actual Values', title='Tyrosol Predicted vs. Actual Values') +
  theme_minimal() 

ggplot() +
  geom_point(data = df, aes(nn_verb$net.result[[1]], Verbascoside), size = 3, alpha=0.6, shape= 22, fill="#0D2C84") +
  geom_abline(linetype = "dotted", color="#0D2C84", size = 1) +
  geom_point(data = bmc, aes(pverb, verb), size = 3, alpha=0.6, shape= 21, fill="#41B6C4") +
  geom_abline(color="#41B6C4") +
  labs(x='Predicted Values', y='Actual Values', title='Verbascoside Predicted vs. Actual Values') +
  theme_minimal() 

ggplot() +
  geom_point(data = df, aes(nn_olea$net.result[[1]], Oleuropein), size = 3, alpha=0.6, shape= 22, fill="#0D2C84") +
  geom_abline(linetype = "dotted", color="#0D2C84", size = 1) +
  geom_point(data = bmc, aes(polea, olea), size = 3, alpha=0.6, shape= 21, fill="#41B6C4") +
  geom_abline(color="#41B6C4") +
  labs(x='Predicted Values', y='Actual Values', title='Oleuropein Predicted vs. Actual Values') +
  theme_minimal() 



# Experimental run vs experimental and predicted values plots (RSM vs ANN vs Exp) --------------------------------------------------------------

ggplot(data= fit) + 
  geom_line(aes(x = run, y = tyr, group = 1), linetype = "F1", color= "black",size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = tyr), size = 3, alpha=0.6, shape= 24, fill= "black") +
  geom_line(aes(x = run, y = rsmtyr, group = 2), linetype = "F1", color="#41B6C4", size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = rsmtyr), size = 3, alpha=0.6, shape= 21, fill="#41B6C4") +
  geom_line(aes(x = run, y = anntyr, group = 3), linetype = "F1", color="#0D2C84", size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = anntyr), size = 3, alpha=0.6, shape= 22, fill="#0D2C84") +
  labs(x='Experimental run number', y='Tyrosol (mg/g extract)', title='Tyrosol experimental runs vs RSM and ANN predictions') +
  theme_minimal() 

ggplot(data= fit) + 
  geom_line(aes(x = run, y = verb, group = 1), linetype = "F1", color= "black",size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = verb), size = 3, alpha=0.6, shape= 24, fill= "black") +
  geom_line(aes(x = run, y = rsmverb, group = 2), linetype = "F1", color="#41B6C4", size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = rsmverb), size = 3, alpha=0.6, shape= 21, fill="#41B6C4") +
  geom_line(aes(x = run, y = annverb, group = 3), linetype = "F1", color="#0D2C84", size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = annverb), size = 3, alpha=0.6, shape= 22, fill="#0D2C84") +
  labs(x='Experimental run number', y='Verbascoside (mg/g extract)', title='Verbascoside experimental runs vs RSM and ANN predictions') +
  theme_minimal() 

ggplot(data= fit) + 
  geom_line(aes(x = run, y = olea, group = 1), linetype = "F1", color= "black",size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = olea), size = 3, alpha=0.6, shape= 24, fill= "black") +
  geom_line(aes(x = run, y = rsmolea, group = 2), linetype = "F1", color="#41B6C4", size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = rsmolea), size = 3, alpha=0.6, shape= 21, fill="#41B6C4") +
  geom_line(aes(x = run, y = annolea, group = 3), linetype = "F1", color="#0D2C84", size = 1, alpha=0.6) +
  geom_point(aes(x = run, y = annolea), size = 3, alpha=0.6, shape= 22, fill="#0D2C84") +
  labs(x='Experimental run number', y='Oleuropein (mg/g extract)', title='Oleuropein experimental runs vs RSM and ANN predictions') +
  theme_minimal() 