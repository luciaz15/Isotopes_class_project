#Playing with data part 1
library(tidyverse)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(flextable)
library(officer)
library(ggpmisc)

#Import Dataset
filename <- file.choose ()
isotope <- read_xlsx(filename)
isotope

# Rename a column in R
colnames (Isotopes_final) [colnames (Isotopes_final) == "Altitude (m)"] <- "altitude"
colnames (Isotopes_final) [colnames (Isotopes_final) == "δ13C (‰)"] <- "d13_C"
colnames (Isotopes_final) [colnames (Isotopes_final) == "δ15N (‰)"] <- "d15_N"
colnames (Isotopes_final) [colnames (Isotopes_final) == "Mean_annual_precipitation (mm)"] <- "prep"
colnames (Isotopes_final) [colnames (Isotopes_final) == "N2-fixing"] <- "N2"
colnames (Isotopes_final) [colnames (Isotopes_final) == "Vegetation_type"] <- "Veg"
colnames (Isotopes_final) [colnames (Isotopes_final) == "Collected species"] <- "species"
colnames (Isotopes_final) [colnames (Isotopes_final) == "Mycorrhizal types*"] <- "myco"

#Diagram proof for "δ13C (‰)" and "δ15N (‰)", bins
Isotopes_final %>% 
  mutate (Type = ifelse(d13_C< -19, 'C3', 'C4'))%>%
  ggplot(aes(x=d15_N,y = d13_C, color=Type, palette = "jco" ))+
    geom_point()+
    (labs (x="δ15N (‰)", y= "δ13C (‰)", title = "Correlation between δ13C (‰) and δ15N (‰)")) +
    geom_smooth(method = 'lm' , se=FALSE) + 
  scale_color_brewer(palette="Dark2")


lm(formula=d13_C~d15_N, data=Isotopes_final) 

# regression line plot
#rl <- lm(d13_C~d15_N, data=Isotopes_final)

#Another form to group variables 
#Otra forma de agrupar que puede servir par todo
summarise(grouped, mean=mean(value), sd=sd(value))

library(ggpmisc)
Isotopes_final %>% 
  mutate (Type = ifelse(d13_C< -20, 'C3', 'C4'))%>%
  ggplot(aes(x=d15_N,y = d13_C, color=Type))+
  geom_point()+
  (labs (x="δ15N (‰)", y= "δ13C (‰)", title = "Correlation between δ13C (‰) and δ15N (‰)")) +
  geom_smooth(method = 'lm' , se=FALSE) +
  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, label.y = "center")+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = y ~ x), 
                  aes(label = paste("p-value=", signif(..p.value.., digits = 2))), label.x= "right", label.y = "center")+
  scale_color_brewer(palette="Dark2")

#Add regression line to plot
#ggplot(aes(y=d13_C, x=d15_N), data = Isotopes_final)+
  #geom_point()+ (labs (x="N 15", y="13 C", title = "Correlation between d13 C and d15 N")) + 
  #geom_smooth (method='lm', se=FALSE)

# Precipitation and isotopes
Isotopes_final %>% 
  mutate (Type = ifelse(d13_C< -19, 'C3', 'C4'))%>%
  ggplot(aes(x=prep,y = d13_C, color=Type))+
  geom_point()+
  (labs (x="Mean Annual Precipitation (mm)", y= " δ13C(‰)", title = " Mean Annual Precipitation (mm) vs  δ13C (‰)")) +
  geom_smooth(method = 'lm' , se=FALSE) +
  stat_poly_eq( formula = y~x, aes(label = paste( ..eq.label..,..rr.label.., sep = "~~~")), parse = TRUE,
                label.y = "center" ) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = y ~ x), 
                  aes(label = paste("p-value=", signif(..p.value.., digits = 2))), label.x = "right", label.y = "middle") +
  scale_color_brewer(palette="Dark2")
                                   # "   R-squared=", signif(..r.squared.., digits = 2), sep = "")), )
# Species Nitraria sibirica (circle, solid line), Reaumuria oongorica (square, dash line) and Hedysarum mongolicum (diamond, dotted line).
# d13C value of three dominant C3 species

# group species and d?13C and d15N
#sp_a <- subset(Isotopes_final, species== c("Reaumuria soongorica (Pall.) Maxim.", "Hedysarum mongolicum Turcz.","Nitraria sibirica Pall."),
#select=c(Sample_site, d13_C, d15_N, Veg, prep, species, altitude))

names (Isotopes_final)
# species dominant
x <-count(Isotopes_final, species)

sub_1 <- Isotopes_final[Isotopes_final$species %in% c("Reaumuria soongorica (Pall.) Maxim.", "Hedysarum mongolicum Turcz.","Nitraria sibirica Pall."), ]
sub_1

sub_2 <- Isotopes_final[Isotopes_final$species %in% c("Reaumuria soongorica (Pall.) Maxim.", "Hedysarum mongolicum Turcz.","Nitraria sibirica Pall.", "Ephedra przewalskii Stapf."), ]
sub_2

qplot(data=sub_1, x = prep, y = d15_N, color= species)


qplot(data=sub_1, x = prep, y = d13_C, color= species)
# d13C and species
sp <- ggscatter(sub_1, x = "prep", y = "d13_C",
                color = "species", palette = "jco",
                add = "reg.line", conf.int = FALSE)
sp + stat_cor(aes(color = species,label = paste( ..p.label.., ..rr.label..,sep = "~`,`~")) , label.x = 3) +
  (labs (x="Mean Annual Precipitation (mm)", y= " δ13C(‰)", title = " Leaf δ13C(‰) as function of MAP"))

#d15N and species
sp <- ggscatter(sub_1, x = "prep", y = "d15_N",
                color = "species", palette = "jco",
                add = "reg.line", conf.int = FALSE)
sp + stat_cor(aes(color = species,label = paste( ..p.label.., ..rr.label..,sep = "~`,`~")) , label.x = 3) +
  (labs (x="Mean Annual Precipitation (mm)", y= " δ15N(‰)", title = " Leaf δ15N(‰) as function of MAP"))

#N15 with species and MAP

p<- ggplot(Isotopes_final, aes(x=prep,y =d15_N))+
  geom_point(aes(shape=N2, color=myco, size=N2))+
  scale_shape_manual(values=c(13, 6))+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  scale_size_manual(values=c(1,2.5))+
  (labs (x="Mean Annual Precipitation (mm)", y= " δ15N(‰)", title = " Plant  δ15N (‰) vs Mean Annual Precipitation (mm)")) +
  geom_smooth(method = 'lm' , se=FALSE, size=0.5, color="darkgrey") +
  stat_poly_eq( formula = y~x, aes(label = paste( ..eq.label..,..rr.label.., sep = "~~~")), parse = TRUE,
                label.y = "bottom" ) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = y ~ x), 
                  aes(label = paste("p-value=", signif(..p.value.., digits = 2))), label.x = "right", label.y = "middle")
p

ggplot(df, aes(x=wt, y=mpg, group=cyl)) +
  geom_point(aes(shape=cyl, color=cyl), size=2)+
  scale_shape_manual(values=c(3, 16, 17))+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))

sp <- ggscatter(Isotopes_final, x = "prep", y = "d15_N",
                color = ("myco"), palette = "jco",
                add = "reg.line", conf.int = FALSE)
sp + stat_cor(aes(color = myco, label = paste(..p.label.., ..rr.label..,sep = "~`,`~")) , label.x = 3) +
  stat_poly_eq( formula = y~x, aes(label = paste( ..eq.label.., sep = "~~~")), parse = TRUE,label.x = "right")+ 
  (labs (x="Mean Annual Precipitation (mm)", y= " δ15N(‰)", title = " Leaf δ15N(‰) as function of MAP"))

prep_N <- qplot(x=prep, y=d15_N, data= Isotopes_final, 
                xlab='MAP (mm)', ylab='15 N', color= myco)
prep_N

 #---------//------

alt_C <- qplot(x=altitude, y=d13_C, data= Isotopes_final, 
               xlab='Altitude (m)', ylab='13 C')
alt_C

alt_N <- qplot(x=altitude, y=d15_N, data= Isotopes_final, 
               xlab='Altitude (m)', ylab='15 N')
alt_N

# Precipitation and isotopes
prep_C <- qplot(x=prep, y=d13_C, 
                data= Isotopes_final, xlab='MAP (mm)', ylab='13 C')
prep_C

prep_N <- qplot(x=prep, y=d15_N, data= Isotopes_final, 
                xlab='MAP (mm)', ylab='15 N')
prep_N


#------------------
  

# Vegetation type and d13 and d15
Sub_desert <-Isotopes_final[Isotopes_final$Veg %in% c("Desert"), ]
Sub_desert

#interesante por el ciclo del N
qplot(data=Sub_desert, x = d15_N, y = d13_C, color= Veg)

qplot(data=Sub_desert, x = prep, y = d13_C, color= Veg)

#Plant δ15N values (N2-fixing, open circles; AM, open triangles; Ecto, open squares; Non, filled circles) as a function of mean annual precipitation.


# Aprox SOM by grouping by site vs mean 13C and 15N
df_1 <- Isotopes_final %>%filter(d13_C%in% (x>=-35 & x<=-20))
df_1

a<-subset(Isotopes_final,d13_C< -21 )
a
lm1<-lm(d13_C~prep, data =a)
summary(lm1)

lm2<-lm(d15_N~prep, data=Isotopes_final)
summary(lm2)


  
df_n <- Isotopes_final
  group_by(Veg, prep)%>%
  summarise(mean_c = mean(d13_C),
            mean_n = mean (d15_N),
            sd=sd(d13_C),
            sd=sd(d15_N)
            )
df_n

p<- ggplot(Isotopes_final, aes(x=prep,y =altitude))+
  geom_point(aes(color=Veg))+
  geom_smooth(method = 'lm' , se=FALSE, size=0.5, color="darkgrey") +
  stat_poly_eq( formula = y~x, aes(label = paste( ..eq.label..,..rr.label.., sep = "~~~")), parse = TRUE,
                label.y = "bottom" ) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = y ~ x), 
                  aes(label = paste("p-value=", signif(..p.value.., digits = 2))), label.x = "right", label.y = "middle")+
  #scale_shape_manual(values=c(13, 6))+
  #scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  #scale_size_manual(values=c(1,2.5))+
  (labs (x="Mean Annual Precipitation (mm)", y= " Altitude", title = " Relationship between Altitude and Precipitation"))

p
#Show all the d13 C for all types of ecosystem
df_1 <- Isotopes_final %>%
  group_by(Veg) %>%
  summarise(mean_c = mean(d13_C),
            mean_n = mean (d15_N),
            prep= mean(prep),
            sd=sd(d13_C),
            sd=sd(d15_N))
df_1

# error bars 
ebars <- ggplot(data=df_1, aes (x=Veg, y=mean_c))+ 
  geom_point(size=2) + theme_classic (base_size = 14)+
  geom_errorbar(data= df_1, width= 0.1, col='darkred',aes(ymin=mean_c-sd, ymax=mean_c+sd, y=NULL))+
  labs(x='Vegetation', y= 'Mean C')

ebars

qplot(data=df_1, x=Veg, y=mean_c)

# Show relationship between mean n and c per site
qplot( data=df_n, x=mean_c, y=mean_n)
 
# The average by ecosystem c by prep:
qplot(data=df_1, x = prep, y = mean_c, color= Veg)

 
ggplot(df_n,aes(x=prep,y = d13_C, color=Veg))+
  geom_point()+
  (labs (x="Mean Annual Precipitation (mm)", y= " δ13C(‰)", title = " Mean Annual Precipitation (mm) vs  δ13C (‰)")) +
  geom_smooth(method = 'lm' , se=FALSE) +
  stat_poly_eq( formula = y~x, aes(label = paste( ..eq.label..,..rr.label.., sep = "~~~")), parse = TRUE,
                label.y = "bottom" ) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = y ~ x), 
                  aes(label = paste("p-value=", signif(..p.value.., digits = 2))), label.x = "right", label.y = "middle")

Vegetation <- ggplot(data=df_n, aes (x=Veg, y=prep), color=Veg, group=Veg)+ 
  geom_point(size=0.5, fill='lightblue') + # theme_classic (base_size = 14)+
  #geom_errorbar(data= df_n, width= 0.25, col='blue', aes(ymin=mean_c-sd, ymax=mean_c+sd, y=NULL))+
  labs(x='Vegetation Type', y= 'δ13C(‰)')
Vegetation

grouped <- group_by(melted, sex, treatment)
summarise(grouped, mean=mean(value), sd=sd(value))
