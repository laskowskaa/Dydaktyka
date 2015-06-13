library(ggplot2)
library(dplyr)
library(sjPlot)
library(scales)
library(rgeos)
library(rgdal)
load('matury_kraj.RData')
matury$wyniki_matur[matury$matura_nazwa == 'geografia rozszerzona'] <- matury$wyniki_matur[matury$matura_nazwa == 'geografia rozszerzona'] / 1.2
matury$matura_nazwa[matury$matura_nazwa == 'geografia podstawowa'] <- 'Geografia pdst'
matury$matura_nazwa[matury$matura_nazwa == 'geografia rozszerzona'] <- 'Geografia roz'
matury$matura_nazwa[matury$matura_nazwa == 'informatyka podstawowa'] <- 'Inormatyka pdst'
matury$matura_nazwa[matury$matura_nazwa == 'informatyka rozszerzona'] <- 'Informatyka roz'
matury$matura_nazwa[matury$matura_nazwa == 'matematyka podstawowa'] <- 'Matematyka pdst'
matury$matura_nazwa[matury$matura_nazwa == 'matematyka rozszerzona'] <- 'Matematyka roz'

#mazowieckie w regionie B
matury$polska<-ifelse(matury$wojewodztwo %in% c("mazowieckie","³ódzkie","podlaskie","œwiêtokrzyskie",
                                                "lubelskie", "ma³opolskie","podkarpackie"),"B",
                      ifelse(matury$wojewodztwo %in% c("kujawsko-pomorskie","pomorskie","œl¹skie",
                                                       "warmiñsko-mazurskie","lubuskie","wielkopolskie",
                                                       "zachodniopomorskie","dolnoœl¹skie","opolskie"),"A",NA))
# Barplot matury A i B œrednie wyniki                                               
matury <- matury %>%
  mutate(wyniki_proc = wyniki_matur / 50)
matury%>%
filter(polska %in% c("A", "B")) %>%
ggplot(., aes(x = rok, y = wyniki_proc, fill = as.character(polska))) + 
  stat_summary(fun.y = 'mean', geom = 'bar', position = position_dodge(), aes(width = 0.8)) +
  scale_y_continuous(labels = percent) +
  labs(x = 'Rok', y = 'Œredni wynik matury', title = 'Œrednie wyniki matur w Polsce A i B') + 
  theme(plot.title = element_text(size = 20, face = "bold"), 
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 13)
  )

# Wykres liniowy wyniki matur w kolejnych latach
df <- matury %>%
  select(wyniki_matur)%>%
  group_by(Region=matury$polska, rok=matury$rok)%>%
  summarise(procent=mean(wyniki_matur*2))
df%>%
  filter(Region %in% c("A", "B")) %>%
  ggplot(., aes(x=rok, y=procent, colour=Region, group=Region))+
  geom_point(size=3)+
  geom_line(size=1)+
  geom_line(aes(x=rok,y=0), linetype = 0)+
  labs(x='Rok',y='Procentowe wyniki matur', title='Œrednie procentowe wyniki matur dla regionów Polski')+
  theme(plot.title=(element_text(face='bold')))

# boxplot Polska A i B
matury%>%
  filter(polska %in% c("A", "B")) %>%
  ggplot(., 
         aes(x=as.character(polska), y=wyniki_matur,fill = factor(polska))) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3, show_guide = FALSE) +
  facet_grid(. ~ matura_nazwa) +
  scale_colour_brewer(palette="Set2") +
  labs(x="Region Polski", y="Wyniki matur [pkt]", title="Porównanie rozk³adów w wynikach matur w Polsce A i B") +
  theme(plot.title = element_text(size=15, face="bold"), 
        axis.text.x=element_text(size=10, face="bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 7, face="bold")
  )

#mazowieckie w regionie A
matury$polska2<-ifelse(matury$wojewodztwo %in% c("³ódzkie","podlaskie","œwiêtokrzyskie",
                                                "lubelskie", "ma³opolskie","podkarpackie"),"B",
                      ifelse(matury$wojewodztwo %in% c("mazowieckie","kujawsko-pomorskie","pomorskie","œl¹skie",
                                                       "warmiñsko-mazurskie","lubuskie","wielkopolskie",
                                                       "zachodniopomorskie","dolnoœl¹skie","opolskie"),"A",NA))

# Barplot matury A i B œrednie wyniki                                               
matury <- matury %>%
  mutate(wyniki_proc = wyniki_matur / 50)
matury%>%
  filter(polska %in% c("A", "B")) %>%
  ggplot(., aes(x = rok, y = wyniki_proc, fill = as.character(polska2))) + 
  stat_summary(fun.y = 'mean', geom = 'bar', position = position_dodge(), aes(width = 0.8)) +
  scale_y_continuous(labels = percent) +
  labs(x = 'Rok', y = 'Œredni wynik matury', title = 'Œrednie wyniki matur w Polsce A i B') +
  theme(plot.title = element_text(size = 20, face = "bold"), 
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 13)
  )

# Wykres liniowy wyniki matur w kolejnych latach
df <- matury %>%
  select(wyniki_matur)%>%
  group_by(Region=matury$polska2, rok=matury$rok)%>%
  summarise(procent=mean(wyniki_matur*2))
df%>%
  filter(Region %in% c("A", "B")) %>%
  ggplot(., aes(x=rok, y=procent, colour=Region, group=Region))+
  geom_point(size=3)+
  geom_line(size=1)+
  geom_line(aes(x=rok,y=0), linetype = 0)+
  labs(x='Rok',y='Procentowe wyniki matur', title='Œrednie procentowe wyniki matur dla regionów Polski')+
  theme(plot.title=(element_text(face='bold')))

# boxplot Polska A i B
matury%>%
  filter(polska2 %in% c("A", "B")) %>%
  ggplot(., 
         aes(x=as.character(polska2), y=wyniki_matur,fill = factor(polska2))) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3, show_guide = FALSE) +
  facet_grid(. ~ matura_nazwa) +
  scale_colour_brewer(palette="Set2") +
  labs(x="Region Polski", y="Wyniki matur [pkt]", title="Porównanie rozk³adów w wynikach matur w Polsce A i B") +
  theme(plot.title = element_text(size=15, face="bold"), 
        axis.text.x=element_text(size=10, face="bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 7, face="bold")
  )

#barplot udzia³ procentowy w iloœci zdawanych matur w regionach A i B
matury %>%
  filter(polska %in% c("A", "B")) %>%
  count(polska, rok) %>%
  group_by(rok) %>%
  mutate(procent = n / sum(n),
         csum = cumsum(procent) - procent/4) %>%
  ggplot(., aes(x = rok, y = procent, fill = polska, group = polska, label = round(procent, 3)*100)) +
  geom_bar(stat = "identity", position = "fill") +
    labs(x='Rok',y='Procentowy udzia³ regionów', title='Procentowy udzia³ regionów w iloœci uczniów przystêpuj¹cych do matury')+
  geom_text(aes(y=csum), vjust = 1, col = "black", size = 5, stat = "identity") +
  scale_fill_discrete(name = "Region") +
  scale_y_continuous(labels = c("0", "25", "50", "75", "100")) +
  theme(plot.title = element_text(face = "bold")) 

#Tabela matur zdanych i niezdanych w A i B
matury$zdal<-ifelse(matury$wyniki_matur>=15,1,2)

matury <- matury %>%
  filter(is.na(polska) == F)
matury <- matury %>%
  filter(is.na(polska2) == F)
matury <- matury %>%
  filter(is.na(wojewodztwo) == F)
matury$zdal<-set_val_labels(matury$zdal,
                            lab=c("Zdana","Niezdana"))
#Tabela matur zdanych i niezdanych w A i B (mazowieckie w B)
sjt.xtab(matury$polska,matury$zdal,
         var.grp=matury$rok,
         encoding='cp1250',
         stringTotal='Ogó³em',
         variableLabels=c('Region Polski','Zdawalnoœæ matur','Rok'),
         showLegend=FALSE,
         highlightTotal=TRUE,
         showRowPerc=TRUE
)

#Tabela matur zdanych i niezdanych w A i B (mazowieckie w A)
sjt.xtab(matury$polska2,matury$zdal,
         var.grp=matury$rok,
         encoding='cp1250',
         stringTotal='Ogó³em',
         variableLabels=c('Region Polski','Zdawalnoœæ matur',"Rok"),
         showLegend=FALSE,
         showNA = FALSE,
         highlightTotal=TRUE,
         showRowPerc=TRUE
)

#Tabela zdawalnoœci matur w poszczególnych województwoach
sjt.xtab(matury$wojewodztwo,matury$zdal,
         encoding='cp1250',
         stringTotal='Ogó³em',
         variableLabels=c('Region Polski','Zdawalnoœæ matur'),
         showLegend=FALSE,
         showNA = FALSE,
         highlightTotal=TRUE,
         showRowPerc=TRUE
)

### MAPY ###

# przygotowanie danych
maturyWoj <- group_by(matury[,1:29], wojewodztwo)
maturyWoj <- summarise(maturyWoj, mean(wyniki_matur))
colnames(maturyWoj) <- c("woj", "œredni_wynik")
for(i in 1:16){
  if(maturyWoj$œredni_wynik[i] >= mean(maturyWoj$œredni_wynik)){
    maturyWoj$Wynik[i] <- "Powy¿ej œredniej"
  }
  else { 
    maturyWoj$Wynik[i] <- "Poni¿ej œredniej"
  }
}

# wczytanie mapy
devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")

map = readOGR("pl-all.geo.json", "OGRGeoJSON", encoding = 'utf-8')
map_df <- fortify(map)
dat <- data.frame(id=0:(length(map@data$alt.name)-1), woj=c("zachodniopomorskie", "lubuskie", "wielkopolskie", "kujawsko-pomorskie", "œl¹skie", "³ódzkie", "mazowieckie", "œwiêtokrzyskie", "podlaskie", "lubelskie", "podkarpackie", "opolskie", "ma³opolskie", "warmiñsko-mazurskie", "pomorskie", "dolnoœl¹skie"))
map_df <- merge(map_df, dat, by="id")
map_df <- merge(map_df, maturyWoj, by="woj")

centers <- data.frame(gCentroid(map, byid=TRUE))
centers$woj <- dat$woj

cbbPalette <- c("#D55E00", "#009E73")

# wykres - mapa
ggmap <- ggplot() +
  geom_map(data=map_df, map=map_df, 
           aes(map_id=id, x=long, y=lat, group=group, fill = Wynik),
           color="#ffffff", size=0.25) +
  scale_fill_manual(values=cbbPalette) +
  geom_text(data=centers, aes(label=woj, x=x, y=y), size=4) +
  labs(x="", y="") +
  ggtitle("Œrednie wyniki matur w latach 2010-14") +
  theme_map() +
  theme(plot.title = element_text(size=20, face = "bold"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=12))

ggmap