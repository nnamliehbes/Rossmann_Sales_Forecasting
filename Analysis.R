#### Aufgabenbeschreibung ####
# You are provided with historical sales data for 1,115 Rossmann stores.
# The task is to forecast the "Sales" column for the test set.
# Note that some stores in the dataset were temporarily closed for refurbishment.

#### Lade Bibliotheken und Datum-Daten ####
library(tidyverse)
library(data.table)
library(reshape2)
library(hrbrthemes)
library(VIM)
library(forecast)
library(timetk)
library(sweep)
library(lubridate)
library(tidymodels)
library(feasts)
library(tsibble)
library(cowplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

train <- read_csv("train.csv")

#### Exploratorische Datenanalyse ####

## Beschreibung der Daten laut website:
# Store: id of store
# Sales: the turnover for any given day (target variable).
# Customers: the number of customers on a given day.
# Open: an indicator for whether the store was open: 0 = closed, 1 = open.
# Promo: indicates whether a store is running a promo on that day.
# StateHoliday: indicates a state holiday. Normally all stores, with few exceptions, are closed on state holidays.
# SchoolHoliday: indicates if the (Store, Date) was affected by the closure of public schools.

## Spalten zu Faktoren konvertieren
spalten <- c("Store", "Open", "Promo", "StateHoliday", "SchoolHoliday")
train[, spalten] <- sapply(train[, spalten], as.factor)

train %>%
  filter(Sales != 0 | Open != 0) %>%
  mutate(logSales = log(Sales)) %>%
  select(Sales, logSales) %>%
  reshape2::melt() %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram() +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  facet_wrap(~ variable, scales = "free", ncol = 1)

train <- train %>%
  mutate(logSales = log(Sales))

## Wie viel Geld gab ein Kunde am Tag aus?
## Wie viele Einnahmen machte ein Store am Tag?
## Wie viele Kunden besuchten den Store?
train <- train %>% mutate(Per_Day = Sales / Customers)
train %>% filter(Per_Day != "NaN") %>% summarise_at(c("Sales", "Customers", "Per_Day"), list(min = min, max = max, mean = mean))

train %>% filter(Per_Day != "NaN") %>% select(c("Sales", "Customers", "Per_Day")) %>% reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, color = variable, fill = variable)) +
    geom_boxplot() +
    facet_wrap(~ variable, scales = "free") +
    theme_ipsum_rc() +
    scale_color_ipsum() +
    scale_fill_ipsum()
    
# Durchschnittlich gaben Kunden am Tag 9.49 € aus.
# Durchschnittlich machte ein Store Einnahmen von rund 6956 €.
# Durchschnittlich besuchten 763 Kunden einen Store am Tag.

## Wie viele Läden haben geschlossen? An wie vielen Tagen wurden keine Einnahmen gemacht?
train %>% filter(Sales == 0 | Open == 0)
# An 172861 Tagen hatten die Stores geschlossen oder keine Einnahmen gemacht.


## Stores ohne Verkäufe filtern
train_clean <- train %>% filter(Sales != 0 | Open != 0)
## StateHoliday bereinigen
train_clean <- train_clean %>% filter_all(all_vars(!is.infinite(.)))
train_clean[which(is.na(train_clean$StateHoliday)),"StateHoliday"] <- "0"

#### Lade und exploriere Store-Daten ####
store <- read.csv("store.csv")

## Beschreibung der Daten laut website:
# Store: a unique Id for each store
# StoreType: differentiates between 4 different store models: a, b, c, d
# Assortment: describes an assortment level: a = basic, b = extra, c = extended
# CompetitionDistance: distance in meters to the nearest competitor store
# CompetitionOpenSince[Month/Year]: gives the approximate year and month of the time the nearest competitor was opened
# Promo2: Promo2 is a continuing a promotion for some stores: 0 = store is not participating, 1 = store is participating
# Promo2Since[Year/Week]: describes the year and calendar week when the store started participating in Promo2
# PromoInterval: describes the consecutive intervals Promo2 is started, naming the months the promotion is started. E.g. "Feb,May,Aug,Nov" means each round starts in February, May, August, November of any given year for that store
spalten <- c("Store", "StoreType", "Assortment", "Promo2")
store[,spalten] <- sapply(store[,spalten], as.factor)

## Wie viele fehlende Werte gibt es?
store %>% matrixplot(interactive = F, cex.axis = .3)
# Die meisten fehlenden Werte beziehen sich auf die Eröffnung konkurrierender Stores und zu den Angaben, wann ein Store an der Promo teilnahm.

# Wie wir sehen, fehlen ca. 50 % der Daten in dieser Variable 
store %>%
  group_by(StoreType) %>%
  count(PromoInterval)

store[store$StoreType == "a" & store$PromoInterval == "","PromoInterval"] <- "Jan,Apr,Jul,Oct"
store[store$StoreType == "b" & store$PromoInterval == "","PromoInterval"] <- "Jan,Apr,Jul,Oct"
store[store$StoreType == "c" & store$PromoInterval == "","PromoInterval"] <- "Jan,Apr,Jul,Oct"
store[store$StoreType == "d" & store$PromoInterval == "","PromoInterval"] <- "Jan,Apr,Jul,Oct"

# entfernen von Spalten mit zu vielen fehlenden Werten
store <- store %>% select(!c(CompetitionOpenSinceMonth, CompetitionOpenSinceYear, Promo2SinceWeek, Promo2SinceYear))

## Zusammenfassung der Entfernung
store %>% filter(CompetitionDistance != "NA") %>% group_by(StoreType) %>% summarise_at("CompetitionDistance", list(min = min, max = max, median = median, mean = mean))
# Store B hat am auffälligsten nur geringe Entfernungen zu einem anderen Store.

## Fehlende Werte in CompetitionDistance ersetzen mit dem gruppenspezifischen Kennwert
store %>%
  ggplot(aes(x = CompetitionDistance, color = StoreType, fill = StoreType)) +
  geom_histogram() +
  facet_wrap(~StoreType) +
  theme_ipsum() +
  scale_color_ipsum() +
  scale_fill_ipsum()
# Es sieht aus, als wäre der Median am aussagekräftigsten
store[which(is.na(store$CompetitionDistance)),"CompetitionDistance"]  <- c(5040, 1790, 5040)
sum(is.na(store))

#### Datensätze zusammenführen und explorieren ####
join_data <- left_join(train_clean, store, by = "Store")

## Wie viel Einnahmen macht ein bestimmter Storetyp am Tag?
join_data %>%
  ggplot(aes(x = StoreType, y = Sales, color = StoreType, fill = StoreType)) +
  geom_boxplot() +
  theme_ipsum_rc() +
  scale_color_ipsum() +
  scale_fill_ipsum()
# Storetyp "B" scheint die meisten Erträge einzubringen

## Wie viel Geld geben Kunden durchschnittlich täglich in einem Store aus und variiert das mit der Entfernung?
join_data %>%
  ggplot(aes(x = StoreType, y = Per_Day, color = StoreType, fill = StoreType)) +
  geom_boxplot() +
  theme_ipsum_rc() +
  scale_color_ipsum() +
  scale_fill_ipsum()
# Im Storetyp "D" gibt ein Kunde scheinbar täglich mehr aus, im Vergleich zu anderen Geschäften
# Obwohl Storetyp "B" die meisten Erträge bringt, gehen diese scheinbar auf kleinere Beträge vieler Kunden zurück.
# Bei Typ "B" könnte es sich um Stores in Bahnhöfen oder dergleichen handeln.

## Hat die Art des Sortiments einen Einfluss auf die Erträge?
join_data %>%
  ggplot(aes(x = Assortment, y = Per_Day, color = StoreType, fill = StoreType)) +
  geom_boxplot() +
  theme_ipsum_rc()
# Das Sortimentangebot spielt bei den Erträgen scheinbar keine Rolle. 

## Haben Schulferien einen EInfluss auf die Erträge?
join_data %>%
  mutate(SchoolHoliday = as.factor(SchoolHoliday)) %>%
  ggplot(aes(y = Sales, fill = SchoolHoliday, color = SchoolHoliday)) +
  geom_boxplot() +
  theme_ipsum_rc() +
  scale_fill_ipsum() +
  scale_color_ipsum()
# Schulferien scheinen keinen allzu starken auf die Erträge zu haben.

## Welcher Store ist in der Summe frequentiertesten?
join_data %>%
  group_by(StoreType) %>%
  summarise_at("Customers", list(sum = sum, min = min, max = max, median = median, mean = mean))
# Storetyp A > D > C > B

## Wie sehen die Verkaustrends im Laufe eines Monats aus?
join_data %>%
  filter(Per_Day != "NaN") %>%
  mutate(Year_Month = format(Date, "%d")) %>%
  group_by(StoreType, Year_Month) %>%
  summarise(MeanSales = mean(Per_Day)) %>%
  ggplot(aes(x = Year_Month, y = MeanSales, color = StoreType, group = StoreType)) +
  geom_point() +
  geom_line() +
  theme_ipsum_rc() +
  scale_color_ipsum()
# Wir sehen, dass es in einem Monat drei Hochpunkte gibt.
# Möglicherweise liegt das daran, dass der Lohn zu Beginn, Mitte oder Anfang eines Monat überwiesen wird.

## Ändern initale sowie konsekutive Promos die Erträge?
options(scipen = 999)

join_data %>%
  filter(Per_Day != "NaN") %>%
  filter(!(Promo == 0 & Promo2 == 1)) %>%
  mutate(Year_Month = format(Date, "%m")) %>%
  group_by(Promo, Promo2, StoreType, Year_Month) %>%
  summarise(MeanSales = mean(Per_Day)) %>%
  ggplot(aes(x = Year_Month, y = MeanSales, color = StoreType, group = StoreType)) +
  geom_point() +
  geom_line() +
  theme_ipsum_rc() +
  scale_color_ipsum() +
  facet_wrap(Promo ~ Promo2, ncol = 3)

# Zu Dezember steigen die Einnahmen durch das "Wintergeschäft".
# Promos haben scheinbar nur einen geringfügigen Einfluss auf die Einnahmen im StoreTyp "B".
# Das könnte am Clientel dieses Stores liegen, nämlich dass diese dort eher nur für schnelle Erledigungen einkaufen gehen.
# Konsekutive Promos sind generell besser für das Geschäft, doch nicht für StoreTyp "B" und am stärksten für StoreTyp "D".

## Zu welchen Zeitpunkt nach den Feiertagen sind Promos am sinnvollsten?
join_data %>%
  group_by(Store) %>%
  mutate(OCC = sequence(rle(as.character(StateHoliday))$lengths)) %>%
  filter(OCC < 10) %>%
  mutate(OCC = as.factor(OCC)) %>%
  group_by(Promo, OCC) %>%
  summarise(MeanSales = mean(logSales, na.rm = TRUE)) %>%
  mutate(Promo = as.factor(Promo)) %>%
  ggplot(aes(x = OCC, y = MeanSales, color = Promo, group = Promo)) +
  geom_point() +
  geom_line() +
  theme_ipsum_rc() +
  scale_color_ipsum()
# Interessanterweise zeigen sich nach sieben Tagen eines Feiertages nahezu keine Unterschiede.

join_data %>%
  filter(Promo == 0 & Promo2 == 0) %>%
  mutate(Week = format(Date, "%Y-%m-%W")) %>%
  group_by(Promo, Promo2, StoreType, Week) %>%
  summarise(MeanSales = mean(Per_Day)) %>%
  ggplot(aes(x = Week, y = MeanSales, color = StoreType, group = StoreType)) +
  geom_point() +
  geom_line() +
  theme_ipsum_rc() +
  scale_color_ipsum()

#### Zeitreihenanalyse ####
## Überprüfen der Autokorrelation
a <- join_data %>%
  filter(Promo == 0 & Promo2 == 0) %>%
  filter(StoreType == "a") %>%
  group_by(Date) %>%
  summarise(MeanSales = mean(Sales)) %>%
  select(MeanSales) %>%
  ggAcf() +
  coord_cartesian(ylim = c(-0.8 , 0.8)) +
  ggtitle("Autokorrelationen für verschiedene Storetypen", subtitle = "A") +
  theme_ipsum_rc()

b <- join_data %>%
  filter(Promo == 0 & Promo2 == 0) %>%
  filter(StoreType == "b") %>%
  group_by(Date) %>%
  summarise(MeanSales = mean(Sales)) %>%
  select(MeanSales) %>%
  ggAcf() +
  coord_cartesian(ylim = c(-0.8 , 0.8)) +
  ggtitle("", subtitle = "B") +
  theme_ipsum_rc()

c <- join_data %>%
  filter(Promo == 0 & Promo2 == 0) %>%
  filter(StoreType == "c") %>%
  group_by(Date) %>%
  summarise(MeanSales = mean(Sales)) %>%
  select(MeanSales) %>%
  ggAcf() +
  coord_cartesian(ylim = c(-0.8 , 0.8)) +
  ggtitle("", subtitle = "C") +
  theme_ipsum_rc()

d <- join_data %>%
  filter(Promo == 0 & Promo2 == 0) %>%
  filter(StoreType == "d") %>%
  group_by(Date) %>%
  summarise(MeanSales = mean(Sales)) %>%
  select(MeanSales) %>%
  ggAcf() +
  coord_cartesian(ylim = c(-0.8 , 0.8)) +
  ggtitle("", subtitle = "D") +
  theme_ipsum_rc()

plot_grid(a,b,c,d)
# Die Autokorrelationen von den mittleren Einnahmen in Abhängigkeit der vier Storetypen überschreiten
# das 95½-Konfidenzintervall und zeigen an, dass diese mit denen vorangegangener Tagen korreliert sind.

### Stationarität erzeugen
store1_ts <- join_data %>%
  filter(Store == 1) %>%
  mutate(Week = format(Date, "%Y-%m-%d")) %>%
  group_by(Week) %>%
  summarise(MeanSales = mean(Sales)) %>%
  select(MeanSales) %>%
  ts(frequency = 365)

store1_ts %>% ggAcf()

diff(join_data %>%
   filter(Store == 1) %>%
   mutate(Week = format(Date, "%Y-%m-%d")) %>%
   group_by(Week) %>%
   summarise(MeanSales = mean(Sales)) %>%
   select(MeanSales) %>%
   ts(frequency = 365), differences = 1) %>% ggAcf()

join_data %>%
  filter(Store == 10) %>%
  mutate(Week = format(Date, "%Y-%m-%d")) %>%
  group_by(Week) %>%
  summarise(MeanSales = mean(Sales)) %>%
  select(MeanSales) %>%
  ts(frequency = 365) %>%
  diff(., differences = 1) %>%
  tseries::adf.test()

# Stationarität gegeben.



#### Trainieren verschiedener Modelle ####
## Besipiel für Store 1
# Wie sieht der Trend beispielhaft für Store 1 aus?
join_data %>%
    filter(Store == 1) %>%
    select(Sales) %>%
    ts(Sales, start = decimal_date(ymd("2013-01-01")), end = decimal_date(ymd("2015-07-31")), frequency = 365) %>%
    decompose(type = "additive") %>%
    autoplot() +
    theme_ipsum_rc() +
    scale_color_ipsum()

## Datenvorbereitung
# Überprüfen, welche Spalte noch missing values hat
colnames(join_data)[colSums(is.na(join_data)) > 0]

# Um RAM freizuräumen: alte Datensätze entfernen
remove(store)
remove(train)
remove(train_clean)
remove(spalten)

# Daten vorbereiten für Vorhersage mit h2o (Features bearbeiten)
library(h2o)
library(data.table)
library(bit64)

join_data <- as.data.table(join_data)
join_data[, Month:=as.integer(format(Date, "%m"))]
join_data[, Year:=as.integer(format(Date, "%y"))]
join_data[, Weekday:=as.integer(format(Date, "%V"))]
join_data[, Promo:=as.factor(Promo)]
join_data[, StoreType:=as.factor(StoreType)]
join_data[, Promo2:=as.factor(Promo2)]
join_data[, SchoolHoliday:=as.factor(SchoolHoliday)]
join_data[, StateHoliday:=as.factor(StateHoliday)]
join_data[, Assortment:=as.factor(Assortment)]
join_data[, Store:=as.factor(Store)]
join_data[, PromoInterval:=as.factor(PromoInterval)]
# join_data[, Date:=NULL]

join_data <- join_data %>%
  mutate(Month_sin = sin(Month - 1)*(2*pi/12),
         Month_cos = cos(Month - 1)*(2*pi/12),
         Day_sin = sin(DayOfWeek - 1)*(2*pi/7),
         Day_cos = cos(DayOfWeek - 1)*(2*pi/7),
         Weekday_sin = sin(Weekday -1)*(2*pi/52),
         Weekday_cos = cos(Weekday -1)*(2*pi/52))

## Für ein besseres Handling mit wenigen RAM wird H2O genutzt
h2o.init(nthreads = -1,max_mem_size='6G')
## laden der Daten in den instanstiierten Cluster
trainHex <- as.h2o(join_data)
## Welche features möchten wir nutzen?
features <- colnames(join_data)[!(colnames(join_data) %in% c("Open", "Sales","logSales","Customers", "Per_Day", "Date"))]
## Random Forest Modell trainieren
rfHex <- h2o.randomForest(x = features,
                          y = "logSales", 
                          ntrees = 100,
                          max_depth = 15,
                          nbins_cats = 1115,
                          training_frame = trainHex,
                          ignore_const_cols = F,
                          nfolds = 5,
                          seed = 1
                          )
h2o.cross_validation_models(rfHex)
h2o.saveModel(object = rfHex, path = dirname(rstudioapi::getActiveDocumentContext()$path))
summary(rfHex)

h2o::h2o.varimp(rfHex) %>%
  as.data.frame() %>%
  mutate(variable = fct_reorder(variable, percentage)) %>%
  ggplot(aes(x = variable, y = percentage, color = "#000000", fill = "#000000")) +
  geom_segment(aes(xend = variable, yend = 0), show.legend = FALSE, size = 4, color = "#000000") +
  theme_ipsum_rc() +
  xlab("") +
  coord_flip() +
  labs(title = "Wichtigkeit der Features")

#### Vorhersage ####
## Modell laden
rfHex <- h2o.loadModel("/home/sebastian/Github/Rossmann_Store_Sales/DRF_model_R_1600772321784_1")

## Laden und angleichen der Test-Daten
test <- read_csv("test.csv")
sum(is.na(test))

spalten <- c("Store", "Open", "Promo", "StateHoliday", "SchoolHoliday")
test[, spalten] <- sapply(test[, spalten], as.factor)
test %>% filter(Open == 0)
test_clean <- test %>% filter(Open != 0)
test_clean <- test_clean %>% filter_all(all_vars(!is.infinite(.)))
test_clean[which(is.na(test_clean$StateHoliday)),"StateHoliday"] <- "0"
which(is.na(test_clean))

## Angleichen an Trainingsdatensatz
store <- as.data.frame(read_csv("store.csv"))
spalten <- c("Store", "StoreType", "Assortment", "Promo2")
store[,spalten] <- sapply(store[,spalten], as.factor)
store[which(is.na(store$PromoInterval)),"PromoInterval"] <- "Jan,Apr,Jul,Oct"
which(is.na(store$PromoInterval))
store <- store %>% select(!c(CompetitionOpenSinceMonth, CompetitionOpenSinceYear, Promo2SinceWeek, Promo2SinceYear))
store[which(is.na(store$CompetitionDistance)),"CompetitionDistance"]  <- c(5040, 1790, 5040)

## Zusammenführen
join_test <- left_join(test_clean, store, by = "Store")

## Features
join_vis <- join_test
join_test <- as.data.table(join_test)
join_test[, Month:=as.integer(format(Date, "%m"))]
join_test[, Year:=as.integer(format(Date, "%y"))]
join_test[, Weekday:=as.integer(format(Date, "%V"))]
join_test[, Promo:=as.factor(Promo)]
join_test[, StoreType:=as.factor(StoreType)]
join_test[, Promo2:=as.factor(Promo2)]
join_test[, SchoolHoliday:=as.factor(SchoolHoliday)]
join_test[, StateHoliday:=as.factor(StateHoliday)]
join_test[, Assortment:=as.factor(Assortment)]
join_test[, Store:=as.factor(Store)]
join_test[, PromoInterval:=as.factor(PromoInterval)]
join_test[, Date:=NULL]

join_test <- join_test %>%
  mutate(Month_sin = sin(Month - 1)*(2*pi/12),
         Month_cos = cos(Month - 1)*(2*pi/12),
         Day_sin = sin(DayOfWeek - 1)*(2*pi/7),
         Day_cos = cos(DayOfWeek - 1)*(2*pi/7),
         Weekday_sin = sin(Weekday -1)*(2*pi/52),
         Weekday_cos = cos(Weekday -1)*(2*pi/52))

testHex <- as.h2o(join_test)
## Vorhersagen speichern
predictions <- as.data.frame(h2o.predict(rfHex, testHex))
## log-Vorhersagen zurücktransformieren
pred <- expm1(predictions[,1])
summary(pred)

submission <- data.frame(Id = join_test$Id, Sales=pred)
write.csv(submission, "h2o_random_forest.csv",row.names=F)

#### Visualisierung der Vorhersagen ####
joined_pred <- left_join(join_vis, submission, by = "Id")
joined_pred <- as.data.frame(joined_pred)

joined_pred %>%
  #filter(Promo == 0 & Promo2 == 0) %>%
  group_by(StoreType, Date) %>%
  summarise(MeanSales = mean(Sales)) %>%
  ggplot(aes(x = Date, y = MeanSales, color = StoreType, group = StoreType)) +
  geom_line() +
  geom_point() +
  facet_wrap(~StoreType) +
  theme_ipsum_rc() +
  scale_color_ipsum()

# Alten Datensatz wiederherstellen
train <- read.csv("train.csv")
train <- train %>%
  mutate(logSales = log(Sales))
train <- train %>% mutate(Per_Day = Sales / Customers)
train_clean <- train %>% filter(Open != 0)
train_clean <- train_clean %>% filter_all(all_vars(!is.infinite(.)))
complete <- join_data %>% mutate(Date = as.Date(train_clean[,"Date"]))
complete <- complete %>% mutate(predicted = "Observed")

# um Vorhersagen ergänzen ##
joined_pred <- joined_pred %>% mutate(predicted = "Predicted")
joined_pred <- joined_pred %>% select(!Id)
complete_all <- complete %>% select(colnames(joined_pred))

complete_all <- rbind(complete_all, joined_pred)

# Visualisieren der Vorhersage der nächsten 6 Wochen
complete_all %>%
  group_by(Date, predicted) %>%
  summarise(MeanSales = mean(Sales)) %>%
  ggplot(aes(x = Date, y = MeanSales, color = predicted)) +
  geom_line() +
  #geom_point() +
  geom_vline(xintercept = as.numeric(as.Date("2015-08-01")), linetype = 2) +
  theme_ipsum_rc() +
  ggtitle("Vorhersage der gemittelten Erträge der nächsten 6 Wochen") +
  scale_color_manual(values = c("#b7ceda", "#0072b2"))

# Visualisieren der Vorhersage der nächsten 6 Wochen für einen Store

