library(tidyverse)
library(readxl)
library(ggthemepark)
library(swissparl)
library(fuzzyjoin)
library(AER)
library(glm.predict)
library(texreg)
library(MASS)
library(pscl)
library(extrafont)

######## Data abrevation and cleaning -----

###### Parl-Data -----

#Retrieve data tables via swissparl package
get_tables()

#Get sessions and retrieve start and end dates of legislative periods
legislatives <- get_data("Session", Language= "DE") %>%
  group_by(LegislativePeriodNumber) %>%
  summarise(start = min(StartDate),
            end = max(EndDate)) %>%
  mutate(LegislativePeriodNumber = factor(LegislativePeriodNumber))

#Get data about who is responsible for a bill
responsabilities <- get_data("BusinessResponsibility", Language= "DE") 

responsabilities <- responsabilities %>%
  filter(DepartmentAbbreviation != "Parl",
         IsLeading) %>%
  select(BusinessNumber, DepartmentAbbreviation, DepartmentName) %>%
  distinct(BusinessNumber, .keep_all = T) #remove duplicates

#get main tables (bill and resolution)
bills <- get_data("Bill", Language = "DE")
resolution <- get_data("Resolution", Language ="DE")

bills <- bills %>%
  rename("IdBill"= ID)

full <- left_join(resolution, bills, by= "IdBill")

#Data exploration

#Create vector that contains keywords to filter out
corpus <- c("Annahme der Dringlichkeitsklausel", "Kenntnisnahme", "Fortsetzung", 
            "Frist für die Behandlung", "Behandlung der Vorlage bis", "Behandlungsfrist der Volks",
            "Eintretensdebatte", "erübrigt", "fortgesetzt", NA, "Tagesordnung", "wurde bis Art.", 
            "unterbrochen", "Fristenverlängerung um ein Jahr", "Diskussion", "Redaktionskommission",
            "Die Dringlichkeitsklausel wird angenommen", "Beginn der Debatte", 
            "Fristverlängerung bis 20.07.2015","Sessionsprogramm", "Beratungen werden ausgesetzt",
            "Vorlage ist an den Bundesrat zurückzuweisen mit dem Auftrag", "Rückweisung an den Bundesrat",
            "Frist zur Behandlung der Volksinitiative", "Fristverlängerung bis 3. Januar 2010.",
            "Fristverlängerung bis 28. Januar 2008", "Zustimmung zum Rückweisungsantrag.",
            "Fristverlängerung bis 14. März 2009.", "Bleibt hängig", "Zustimmung zur Rückweisung.",
            "Kenntnis vom Bericht.", "verschoben", "vereidigt", "nimmt Kenntnis",
            "Vorlage wird an die Finanzkommisison zurückgewiesen.", "Zustimmung zur Fristverlängerung.",
            "Art. 6, Abs. 2, wird gemäss Antrag der Kommission genehmigt.", 
            "Behandlungsfrist bis 31. August 2009 verlängert.",
            "Beginn der Beratung","Die Dringlichkeitsklausel wird nicht angenommen (absolutes Mehr nicht erreicht).",
            "Vom Mittelfristplan wird Kenntnis genommen.","Fristverlängerung", "Vom Bericht wird",
            "Die Beratung der Vorlage wird sistiert","Behandlungsfrist für die Volksinitiative",
            "Art. 2, Abs. 2 wird an die Kommission zurückgewiesen.", "Behandlungsfrist wird gemäss",
            "nicht weiter behandelt.","Frist wird bis zum 27. Mai 2004 verlängert.",
            "Sistierung der Beratungen","Beratung wird gemäss","Sistierung der Beratungen",
            "Amtlichen Sammlung des Bundesrechts veröffentlicht, sobald der Bundesrat das Inkrafttreten",
            "Frist zur Ansetzung der","Behandlungsfrist für die Volksinitiative",
            "Fristverlängerung um ein Jahr, d.h. bis", "provisorischen Büros des Nationalrates genehmigt.",
            "Festhalten (=Die Behandlung dieses Berichts wird ausgesetzt).",
            "Die Behandlung dieses Berichts wird ausgesetzt","Die Behandlung von Entwurf1 wird ausgesetzt",
            "Die Beratung wird ausgesetzt.","Fristverlängerung bis zum","Bundesrat wird ermächtigt",
            "Schlussabstimmung", "Rückweisung", "Eintreten"
            )

#Filter non-applicable businesses out
full_br <- full %>%
  filter(BusinessTypeName== "Geschäft des Bundesrates",
         BusinessStatusText== "Erledigt",
         CategoryName == "Normal",
         !Council==3,
         !grepl("Bericht", Title),
         !grepl("Wahlprüfung", Title),
         !grepl("Legislaturplanung", Title),
         !grepl("Konstituierung und Vereidigung", Title),
         !grepl(paste(corpus, collapse="|"), ResolutionText))

abgeschrieben <- full_br %>%
  filter(grepl("abgeschrieben", ResolutionText))

abschreibung <- full_br %>%
  filter(grepl("Abschreibung", ResolutionText))

nichteintreten <- full_br %>%
  filter(grepl("Nichteintreten", ResolutionText))

n1 <- full_br %>%
  group_by(BusinessShortNumber, IdBill)%>%
  tally()%>%
  filter(n==1)

full_br2 <- full_br %>%
  filter(!IdBill %in% abgeschrieben$IdBill,
         !IdBill %in% abschreibung$IdBill,
         !IdBill %in% nichteintreten$IdBill,
         !IdBill %in% n1$IdBill)

#Corpus for non-necessary resolutions regarding Einigungskonferenz
corpus2 <- c("Beschluss gemäss Antrag der Einigungskonferenz", "Gemäss Einigungskonferenz",
             "Zustimmung zu den Anträgen der Einigungskonferenz","Zustimmung zum Antrag der Einigungskonferenz",
             "Gemäss Anträgen der Einigungskonferenz","Durch die Annahme des Antrages der Einigungskonferent")

#Create df with number of rounds and Einigungskonferenz
rounds <- full_br2 |>
  group_by(BusinessShortNumber, BusinessNumber, IdBill) |>
  mutate(einigungskonferenz= ifelse(sum(grepl("Einigungskonferenz", ResolutionText))>0,1,0),
         
         num_ang = sum(grepl(paste(corpus2, collapse="|"), ResolutionText)),
         
         einigungskonferenz_ang = case_when(
           num_ang == 2 ~ 1,
           num_ang == 1 ~ 0,
           einigungskonferenz == 1 & num_ang == 0 ~ 0,
           T ~ NA_real_
         ),
           
         referencedate = max(ResolutionDate)) |>
  ungroup() |>
  fuzzy_left_join(legislatives,
                  by= c("referencedate" = "start",
                        "referencedate" = "end"),
                  match_fun = list(`>=`, `<=`)) |>
  group_by(BusinessShortNumber, BusinessNumber, IdBill, einigungskonferenz, 
           einigungskonferenz_ang, LegislativePeriodNumber) |>
  tally(name="rounds") |>
  ungroup() |>
  mutate(rounds= ifelse(rounds>8, 8, rounds),
         diff= case_when(
           einigungskonferenz == 0 ~ rounds-2,
           einigungskonferenz_ang == 1 ~ 5,
           einigungskonferenz_ang == 0 ~ 6
         )) 

rounds <- left_join(rounds, responsabilities)

###### Polarization data ----


#### Mandates

middle <- c("FDP", "CVP", "LdU", "EVP", "GLP", "BDP", "Die Mitte") #non-plar parties

#Import and reshape nr and sr data
nr_hist <- read_excel("data/nr_hist.xlsx", na="*") %>%
  pivot_longer(cols= `1919`:`2019`, names_to = "year", values_to = "mandates")%>%
  mutate(polar_party= ifelse(Partei %in% middle,0,1),
         year= as.numeric(year),
         council= "NR")%>%
  filter(! Partei == "Übrige",
         year >= 1987)

sr_hist <- read_excel("data/sr_hist.xlsx", na="*") %>%
  pivot_longer(cols= `1919`:`2019`, names_to = "year", values_to = "mandates")%>%
  mutate(polar_party= ifelse(Partei %in% middle,0,1),
         year= as.numeric(year),
         council= "SR")%>%
  filter(! Partei == "Übrige",
         year >= 1987)

mandates <- full_join(nr_hist, sr_hist)

#Calculate difference of polar-parties
pol_diff <- mandates %>%
  group_by(council, year, polar_party) %>%
  summarise(mandates= sum(mandates, na.rm = T)) %>%
  group_by(council, year) %>%
  mutate(perc= mandates/ sum(mandates)) %>%
  filter(polar_party==1) %>%
  group_by(year) %>%
  summarise(pol_diff= abs(diff(perc)))%>%
  mutate(LegislativePeriodNumber= factor(43:51))

  
### LSQ

lsq <- mandates %>%
  group_by(council, year) %>%
  mutate(mandates= ifelse(is.na(mandates), 0, mandates),
                          perc= mandates/ sum(mandates, na.rm = T))%>%
  ungroup()%>%
  pivot_wider(names_from = council, values_from = c(mandates, perc)) %>%
  mutate(diff= perc_NR - perc_SR) %>%
  group_by(year) %>%
  summarise(lsq= sqrt(0.5*sum(diff^2, na.rm = T))) %>%
  mutate(LegislativePeriodNumber = factor(43:51))


### Polarization index
library(vdemdata)

vdem <- vparty %>%
  filter(country_name == "Switzerland",
         year %in% mandates$year)%>%
  select(v2pashname, year, v2pavote, v2paminor_ord, v2paimmig_ord, v2palgbt_ord, v2parelig_ord,
         v2pawomlab_ord, v2pariglef_ord) %>%
  mutate(share= v2pavote/100,
         v2pariglef_ord= 6-v2pariglef_ord,
         position= rowSums(across(v2paminor_ord:v2pariglef_ord)),
         position2= position/6) %>%
  filter(v2pashname %in% c("SVP", "SP")) %>%
  group_by(year) %>%
  summarise(party_pol = abs(diff(position2)))%>%
  ungroup()%>%
  mutate(LegislativePeriodNumber= factor(43:51))


###### Full data for analysis ----

analysis_data <- rounds %>%
  left_join(., pol_diff) %>%
  left_join(., lsq) %>%
  left_join(., vdem)

write_csv(analysis_data, "analysis_data.csv")

######### Model choice -----

poisson1 <- glm(diff ~ DepartmentAbbreviation + as.numeric(LegislativePeriodNumber)+ lsq + 
                  pol_diff + party_pol,
               data = analysis_data, family = poisson
               )
screenreg(poisson1)
summary(poisson1)

nb1 <- glm.nb(diff ~ DepartmentAbbreviation + as.numeric(LegislativePeriodNumber)+ lsq + 
         pol_diff + party_pol,
       data = analysis_data)
summary(nb1)
odTest(nb1)

zero <- zeroinfl(diff ~ DepartmentAbbreviation + as.numeric(LegislativePeriodNumber)+ lsq + 
                   pol_diff + party_pol | party_pol + pol_diff,
                 data = analysis_data, dist = "negbin")
summary(zero)

vuong(zero, nb1)

##### Descriptive Statistics -----

rounds %>%
  ggplot(aes(x=factor(diff)))+
  geom_bar()+
  labs(x= "Anzahl Differenzen", y= "Anzahl Geschäfte",
       #title= "Differenzen zwischen NR und SR",
       #subtitle= "Erledigte Bundesratsgeschäfte mit ord. Verfahren 1991-2023"
       )+
  theme_simple()+
  theme(text = element_text(family = "Times New Roman"))
ggsave("plots/diff_plot.jpeg")

diff_table <- rounds %>%
  filter(diff>0) %>%
  group_by(diff) %>%
  tally() %>%
  ungroup() %>%
  mutate(perc= n/sum(n))
xtable::xtable(diff_table)
sum(diff_table$n)

mandates %>%
  group_by(council, year, polar_party) %>%
  summarise(mandates= sum(mandates, na.rm = T)) %>%
  group_by(council, year) %>%
  mutate(perc= mandates/ sum(mandates)) %>%
  filter(polar_party==1) %>%
  ungroup()%>%
  mutate(leg= rep(43:51, 2))%>%
  ggplot(aes(x=factor(leg), y=perc, 
             linetype= council, group= council))+
  geom_line()+
  annotate(geom= "segment", x="43", xend = "43",
           y=0.261, yend = 0.477, linetype= "dotted", color= "darkgrey")+
  annotate(geom= "segment", x="51", xend = "51",
           y=0.444, yend = 0.62, linetype= "dotted", color= "darkgrey")+
  annotate(geom= "text", label= "21.7 Prozentpunkte",
           y=0.369, x="43", hjust= -0.02, size=3, family= "Times New Roman")+
  annotate(geom= "text", label= "17.6 Prozentpunkte",
           y=0.532, x="51", hjust= 1.02, size=3, family= "Times New Roman")+
  labs(x= "Legislaturperiode", y= "Anteil Polparteien in Prozent",
       linetype= "")+
  scale_linetype_manual(labels = c("Nationalrat", "Ständerat"),
                        values = c("solid", "longdash"))+
  scale_y_continuous(labels= scales::percent)+
  theme_simple()+
  theme(text = element_text(family = "Times New Roman"))
ggsave("plots/pol_entwicklung.jpeg")

vdem %>%
  ggplot(aes(x= LegislativePeriodNumber, y= party_pol, group=1))+
  geom_point()+
  geom_line()+
  labs(x= "Legislaturperiode", y= "Polarisierung des Parteiensystemes",
       caption = "\nHohe Werte = Hohe Polarisierung des Parteiensystems; Tiefe Werte = Tiefe Polarisierung")+
  scale_y_continuous(breaks = seq(1.9, 3.1, 0.2))+
  theme_simple()+
  theme(text = element_text(family = "Times New Roman"))
ggsave("plots/party_pol.jpeg")

mandates %>%
  filter(year== 2019) %>%
  mutate(party=  ifelse(Partei %in% c("SP", "GPS", "GLP", "BDP", "CVP", "FDP", "SVP"), 
                        Partei, "Übrige")) %>%
  group_by(party, council) %>%
  summarise(mandates2= sum(mandates, na.rm = T)) %>%
  mutate(perc= case_when(
    council == "NR" ~ mandates2 / 200,
    T ~ mandates2 / 46),
    party2 = factor(party, 
                    levels= c("SP", "GPS", "GLP", "BDP", "CVP", "FDP", "SVP", "Übrige")),
    
    council2= case_when(council== "NR" ~ "Nationalrat",
                         council== "SR" ~ "Ständerat")) %>%
  ggplot(aes(x= party2, y= perc))+
  geom_col()+
  geom_text(aes(label= ifelse(perc==0, 
                              "", paste0(round(perc*100,1), "%"))), vjust=-0.4,
            size= 3, family= "Times New Roman")+
  facet_wrap(~council2)+
  scale_y_continuous(labels = scales::percent)+
  labs(x="", y= "Sitzanteil (%)", caption = "Daten: Bundesamt für Statistik")+
  theme_bw()+
  theme(text = element_text(family = "Times New Roman"),
        plot.caption = element_text(hjust = 0),
        panel.grid.major.x = element_blank())
ggsave("plots/mandate.jpeg")

######## Graph Seats -----
sr <- read_excel("data/ständerat.xlsx", na="*") %>%
  drop_na(Mandate) %>%
  mutate(Rat= "Ständerat",
         Partei=
           case_when(
             Partei %in% c("SVP", "SP", "FDP", "CVP", "GLP", "GPS", "BDP") ~ Partei,
             T ~ "Übrige"
           )) %>%
  group_by(Partei, Rat) %>%
  summarise(Mandate= sum(Mandate))


nr <- read_excel("data/nationalrat.xlsx", na="*") %>%
  drop_na(Mandate) %>%
  mutate(
    Rat= "Nationalrat",
    
    Partei=
      case_when(
        Partei %in% c("SVP", "SP", "FDP", "CVP", "GLP", "GPS", "BDP") ~ Partei,
        T ~ "Übrige"
      )) %>%
  group_by(Partei, Rat) %>%
  summarise(Mandate= sum(Mandate)) 

full_join(nr,sr) %>%
  group_by(Rat) %>%
  mutate(Partei=
           factor(Partei, levels = c("SVP", "FDP", "CVP", "BDP",
                                     "GLP", "GPS", "SP", "Übrige")),
         perc= Mandate/sum(Mandate)) %>%
  ggplot(aes(x=Partei, y=perc))+
  geom_col()+
  facet_wrap(~Rat)+
  labs(x="", y="Anteil Mandate",
       caption= "Daten: Bundesamt für Statistik")+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  theme(panel.grid.major.x = element_blank())
ggsave("plots/mandatverteilung.jpeg", scale = 2)

#table descriptive statistics
desc_data <- analysis_data %>%
  dplyr::select(diff, einigungskonferenz, pol_diff, lsq, party_pol)%>% 
  as.data.frame()
stargazer::stargazer(desc_data)



##### Statistical Analysis -----


### Zero inflated Model 1
zero_mod0 <- zeroinfl(diff ~ DepartmentAbbreviation + as.numeric(LegislativePeriodNumber)
                      | party_pol, data = analysis_data, dist = "negbin")
summary(zero_mod0)

zero_mod1 <- zeroinfl(diff ~ DepartmentAbbreviation + as.numeric(LegislativePeriodNumber)+ lsq
                    | party_pol, data = analysis_data, dist = "negbin")
summary(zero_mod1)

zero_mod2 <- zeroinfl(diff ~ DepartmentAbbreviation + as.numeric(LegislativePeriodNumber)+ lsq+
                        pol_diff
                      | party_pol, data = analysis_data, dist = "negbin")
summary(zero_mod2)

zero_mod3 <- zeroinfl(diff ~ DepartmentAbbreviation + as.numeric(LegislativePeriodNumber)+ lsq+
                        pol_diff+ party_pol
                      | party_pol, data = analysis_data, dist = "negbin")
summary(zero_mod3)

coef_names <- c("Count model: Intercept", "Count model: Dep. Bundeskanzlei", 
                "Count model: Dep. BR", "Count model: Dep. EDA", "Count model: Dep. EDI",
                "Count model: Dep. EFD", "Count model: Dep. EJPD", "Count model: Dep. EMD",
                "Count model: Dep. EVD", "Count model: Dep. EVED", "Count model: Dep. UVEK",
                "Count model: Dep. VBS", "Count model: Dep. WBF", "Count model: Legislatur",
                "Count model: Log(theta)", "Zero model: Intercept", 
                "Zero model: Polarisierung Parteiensystem", "Count model: LSQ",
                "Count model: Anteil Polparteien", "Count model: Polarisierung Parteiensystem")
                
              

texreg(list(zero_mod0, zero_mod1, zero_mod2, zero_mod3),
          custom.model.names = c("Modell 1a", "Modell 1b", "Modell 1c", "Modell 1d"),
          reorder.coef = c(1, 14, 18:20, 2:13, 15:17),
          custom.coef.names = coef_names,
       caption = "Ergebnisse Modelle 1a bis 1c",
       label = "model1")

zero_mod4 <- zeroinfl(diff ~ DepartmentAbbreviation + as.numeric(LegislativePeriodNumber)*party_pol
                      | party_pol, data = analysis_data, dist = "negbin")
summary(zero_mod4)

#### Logit model 2
logit_mod0 <- glm(einigungskonferenz ~ as.numeric(LegislativePeriodNumber) + 
                    DepartmentAbbreviation, data= analysis_data, family = binomial)


logit_mod1 <- glm(einigungskonferenz ~ lsq + as.numeric(LegislativePeriodNumber) + 
                    DepartmentAbbreviation, data= analysis_data, family = binomial)
summary(logit_mod1)

logit_mod2 <- glm(einigungskonferenz ~ as.numeric(LegislativePeriodNumber) + 
                    DepartmentAbbreviation + lsq + pol_diff, 
                  data= analysis_data, family = binomial)
summary(logit_mod2)

logit_mod3 <- glm(einigungskonferenz ~ as.numeric(LegislativePeriodNumber) + 
                    DepartmentAbbreviation + lsq + party_pol + pol_diff, 
                  data= analysis_data, family = binomial(link = logit))
summary(logit_mod3)

coef_names2 <- c("Intercept", "Legislative", "Dep. Bundeskanzlei", "Dep. BR",
                 "Dep. EDA", "Dep. EDI", "Dep. EFD", "Dep. EJPD", "Dep. EMD", "Dep. EVD",
                 "Dep. EVED", "Dep. UVEK", "Dep. VBS", "Dep. WBF", "LSQ", "Anteil Polparteien",
                 "Polarisierung Parteiensystem")

texreg(list(logit_mod0, logit_mod1,logit_mod2,logit_mod3),
          custom.model.names = c("Modell 2a", "Modell 2b", "Modell 2c", "Modell 2d"),
          reorder.coef = c(1:2, 15:17, 3:14),
          custom.coef.names = coef_names2,
       caption = "Ergebnisse Modelle 2a bis 2c",
       label = "model2")




