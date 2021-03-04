#### ISSP data processing ####
# Joseph Bulbulia
# joseph.bulbulia@gmail.com
# 2021/MAR

#### notes ####
# martin van Randow sent the file in feb 2021: called "SASNZ2018 combined 20200619"; I renamed it
# issp2020

#### load libraries ####
#for reading spss files
library(haven) # working with wretched issp files
library(here) # file organisation
library(tidyverse) # data wrangling
library(readr) #  exporting/importing data


# read data in
df <-
  as.data.frame(haven::read_sav(here::here(
    "data_raw", "ISSP", "issp_nz_2019_2020.sav"
  )))

# process data, there are 350 repeated measures and 984 single measures

# data processing
lon1 <- df %>%
  dplyr::mutate(id = as.factor(1:nrow(df))) %>%
  dplyr::mutate(t0.measuredbothyears = c(rep(1, 350), rep(0, 984))) %>%
  dplyr::mutate(t1.measuredbothyears = c(rep(1, 350), rep(0, 984))) %>%
  dplyr::mutate(t0.notreligious = ifelse(t0.nowrelig == 0, 1, 0)) %>%
  dplyr::mutate(t0.rural = ifelse(t0.townsize == 1 |
                                    t0.townsize == 2 |
                                    t0.townsize == 3, 0, 1)) %>%
  dplyr::mutate(t0.religiosityAll = ifelse(t0.nowrelig == 0, 0, t0.religiousity)) %>%
  dplyr::mutate(t0.rightwing = t0.leftright) %>%
  dplyr::mutate(t0.male = ifelse(t0.gender == 1, 1, 0)) %>%
  dplyr::mutate(t0.religiousity = t0.religiousity) %>%
  dplyr::mutate(t1.notreligious = ifelse(t1.nowrelig == 0, 1, 0)) %>%
  dplyr::mutate(t1.rural = ifelse(t1.townsize == 1 |
                                    t1.townsize == 2 |
                                    t1.townsize == 3, 0, 1)) %>%
  dplyr::mutate(t1.religiosityAll = ifelse(t1.nowrelig == 0, 0, t1.religiousity)) %>%
  dplyr::mutate(t1.rightwing = t1.leftright) %>%
  dplyr::mutate(t0.age = as.numeric(2018 - t0.birth)) %>%
  dplyr::mutate(t1.age = as.numeric(2019 - t0.birth)) %>%
  dplyr::mutate(t1.weight = t0.weight) %>% ## hack
  dplyr::mutate(t1.male = ifelse(t1.gender == 1, 1, 0)) %>%
  dplyr::mutate(t1.religiousity = t1.religiousity) %>%
  dplyr::select(
    id,
    t0,
    t0.eduyears,
    t0.religaccept,
    t0.religiousity,
    # leftright,
    # Religious, #
    # Religion,
    t0.rural,
    t0.male,
    #  Euro,
    # nowdenomx,
    # townsize,
    t0.age,
    # birth,
    t0.gender,
    t0.nowrelig,
    t0.religinf,
    t0.religconflict,
    t0.religintolerance,
    t0.religpower,
    #religaccept,
    t0.sciencefaith,
    t0.religcomfort,
    t0.attitudechr,
    t0.attitudemus,
    t0.attitudehin,
    t0.attitudebud,
    t0.attitudejew,
    t0.attitudeath,
    t0.threatchr,
    t0.threatmus,
    t0.threathin,
    t0.threatbud,
    t0.threatjew,
    t0.threatath,
    # age,
    #  birth,
    t0.gender,
    t0.nzeuro,
    t0.religintolerance,
    t0.religaccept,
    t0.religiosityAll,
    t0.notreligious,
    t0.highestqual,
    t0.sciencefaith,
    t0.rightwing,
    t1,
    t1.eduyears,
    t1.religaccept,
    t1.religiousity,
    # leftright,
    # Religious, #
    # Religion,
    t1.rural,
    t1.male,
    #  Euro,
    # nowdenomx,
    # townsize,
    t1.age,
    # birth,
    # t1.gender,
    t1.nowrelig,
    t1.religinf,
    t1.religconflict,
    t1.religintolerance,
    t1.religpower,
    #religaccept,
    t1.sciencefaith,
    t1.religcomfort,
    t1.attitudechr,
    t1.attitudemus,
    t1.attitudehin,
    t1.attitudebud,
    t1.attitudejew,
    t1.attitudeath,
    t1.threatchr,
    t1.threatmus,
    t1.threathin,
    t1.threatbud,
    t1.threatjew,
    t1.threatath,
    # age,
    #  birth,
    t1.gender,
    t1.nzeuro,
    t1.religintolerance,
    t1.religaccept,
    t1.religiosityAll,
    t1.notreligious,
    t1.highestqual,
    t1.sciencefaith,
    t1.rightwing,
    t0.measuredbothyears,
    t1.measuredbothyears,
    t0.weight,
    t1.weight
  ) %>%
  arrange(id)




## make longitudinal data
## this coudl be more efficient
library(tidyr)
inner_join(
  lon1 %>%
    dplyr::select(c(id, ends_with('age'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, age, -id) %>%  #~ long form = 1 row per prod per seq   ~
    mutate(wave = recode_factor(
      wave,
      t0.age = "2018",
      t1.age = "2019"
    )) %>%
    arrange(id),

  lon1 %>%
    dplyr::select(c(id, ends_with('male'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, male, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.male = "2018",
      t1.male = "2019"
    )) %>%
    arrange(id)
) -> df1



df1



inner_join(
  df1,
  lon1 %>%
    dplyr::select(c(id, ends_with('religiousity'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, religiousity, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.religiousity = "2018",
      t1.religiousity = "2019"
    )) %>%
    arrange(id)
) -> df3

df3




inner_join(
  df3,
  lon1 %>%
    dplyr::select(c(id, ends_with('eduyears'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, eduyears, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.eduyears = "2018",
      t1.eduyears = "2019"
    )) %>%
    arrange(id)
) -> df4

df4



inner_join(
  df4,
  lon1 %>%
    dplyr::select(c(id, ends_with('rural'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, rural, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.rural = "2018",
      t1.rural = "2019"
    )) %>%
    arrange(id)
) -> df5

df5



inner_join(
  df5,
  lon1 %>%
    dplyr::select(c(id, ends_with('nzeuro'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, nzeuro, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.nzeuro = "2018",
      t1.nzeuro = "2019"
    )) %>%
    arrange(id)
) -> df6

df6


inner_join(
  df6,
  lon1 %>%
    dplyr::select(c(id, ends_with('highestqual'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, highestqual, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.highestqual = "2018",
      t1.highestqual = "2019"
    )) %>%
    arrange(id)
) -> df7

df7

inner_join(
  df7,
  lon1 %>%
    dplyr::select(c(id, ends_with('rightwing'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, rightwing, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.rightwing = "2018",
      t1.rightwing = "2019"
    )) %>%
    arrange(id)
) -> df7

df7



inner_join(
  df7,
  lon1 %>%
    dplyr::select(c(id, ends_with('attitudechr'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, attitudechr, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.attitudechr = "2018",
      t1.attitudechr = "2019"
    )) %>%
    arrange(id)
) -> df9

df9


inner_join(
  df9,
  lon1 %>%
    dplyr::select(c(id, ends_with('attitudemus'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, attitudemus, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.attitudemus = "2018",
      t1.attitudemus = "2019"
    )) %>%
    arrange(id)
) -> df10

df10

inner_join(
  df10,
  lon1 %>%
    dplyr::select(c(id, ends_with('attitudehin'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, attitudehin, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.attitudehin = "2018",
      t1.attitudehin = "2019"
    )) %>%
    arrange(id)
) -> df11

df11




inner_join(
  df11,
  lon1 %>%
    dplyr::select(c(id, ends_with('attitudebud'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, attitudebud, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.attitudebud = "2018",
      t1.attitudebud = "2019"
    )) %>%
    arrange(id)
) -> df12

df12


inner_join(
  df12,
  lon1 %>%
    dplyr::select(c(id, ends_with('attitudejew'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, attitudejew, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.attitudejew = "2018",
      t1.attitudejew = "2019"
    )) %>%
    arrange(id)
) -> df13

df13

inner_join(
  df13,
  lon1 %>%
    dplyr::select(c(id, ends_with('attitudeath'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, attitudeath, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.attitudeath = "2018",
      t1.attitudeath = "2019"
    )) %>%
    arrange(id)
) -> df14

df14



inner_join(
  df14,
  lon1 %>%
    dplyr::select(c(id, ends_with('threatchr'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, threatchr, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.threatchr = "2018",
      t1.threatchr = "2019"
    )) %>%
    arrange(id)
) -> d15

d15


inner_join(
  d15,
  lon1 %>%
    dplyr::select(c(id, ends_with('threatmus'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, threatmus, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.threatmus = "2018",
      t1.threatmus = "2019"
    )) %>%
    arrange(id)
) -> df16

df16

inner_join(
  df16,
  lon1 %>%
    dplyr::select(c(id, ends_with('threathin'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, threathin, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.threathin = "2018",
      t1.threathin = "2019"
    )) %>%
    arrange(id)
) -> df17

df17


inner_join(
  df17,
  lon1 %>%
    dplyr::select(c(id, ends_with('threatbud'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, threatbud, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.threatbud = "2018",
      t1.threatbud = "2019"
    )) %>%
    arrange(id)
) -> df18

df18




inner_join(
  df18,
  lon1 %>%
    dplyr::select(c(id, ends_with('threatjew'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, threatjew, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.threatjew = "2018",
      t1.threatjew = "2019"
    )) %>%
    arrange(id)
) -> df19

df19


inner_join(
  df19,
  lon1 %>%
    dplyr::select(c(id, ends_with('threatath'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, threatath, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.threatath  = "2018",
      t1.threatath = "2019"
    )) %>%
    arrange(id)
) -> df20

df20

inner_join(
  df20,
  lon1 %>%
    dplyr::select(c(id, ends_with('sciencefaith'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, sciencefaith, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.sciencefaith = "2018",
      t1.sciencefaith = "2019"
    )) %>%
    arrange(id)
) -> df21

df21



inner_join(
  df21,
  lon1 %>%
    dplyr::select(c(id, ends_with(
      'measuredbothyears'
    ))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, measuredbothyears, -id) %>%
    mutate(
      wave = recode_factor(
        wave,
        t0.measuredbothyears = "2018",
        t1.measuredbothyears = "2019"
      )
    ) %>%
    arrange(id)
) -> df22

nrow(df22)


inner_join(
  df22,
  lon1 %>%
    dplyr::select(c(id, ends_with('notreligious'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, notreligious, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.notreligious = "2018",
      t1.notreligious = "2019"
    )) %>%
    arrange(id)
) -> df23

df23

inner_join(
  df23,
  lon1 %>%
    dplyr::select(c(id, ends_with('weight'))) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(wave, weight, -id) %>%
    mutate(wave = recode_factor(
      wave,
      t0.weight = "2018",
      t1.weight = "2019"
    )) %>%
    arrange(id)
) -> df24

df24




## GET DATA IN ORDER

isdat <- df24 %>% mutate(
  Negative.Muslims = attitudemus,
  Negative.Hindus  = attitudehin,
  Negative.Buddhists = attitudebud,
  Negative.Christians = attitudechr,
  Negative.Jews = attitudejew,
  Negative.Atheists = attitudeath,
  Threatened.Muslims = 5 - threatmus,
  Threatened.Hindus = 5 - threathin,
  Threatened.Buddhists = 5 - threatbud,
  Threatened.Christians = 5 - threatchr,
  Threatened.Jews = 5 - threatjew,
  Threatened.Atheists = 5 - threatath
) %>%
  dplyr::mutate(
    age.c = scale(age, scale = F, center = T),
    age.decade.c = scale(age, scale = F, center = T) / 10,
    rural = factor(rural),
    male = factor(male),
    highestqual  = as.numeric(highestqual),
    # relaccept.S = scale(religaccept,center=TRUE,scale=T),
    rightwing.S = scale(rightwing, center = TRUE, scale = T),
    r_s = scale(religiousity),
    highestqual.c = scale(highestqual, center = T, scale =
                            T)
  ) %>%
  arrange(id)




summary(isdat$religiousity)

# For tables
tbdat <- isdat


table(tbdat$notreligious)



tbdat$Negative.Muslims <-
  factor(
    tbdat$Negative.Muslims ,
    levels = c(1:5),
    labels = c(
      "Very positive",
      "Somewhat positive",
      "Neither negative nor positive",
      "Somewhat negative",
      "Very Negative"
    )
  )



tbdat$Negative.Hindus <-
  factor(
    tbdat$Negative.Hindus ,
    levels = c(1:5),
    labels = c(
      "Very positive",
      "Somewhat positive",
      "Neither negative nor positive",
      "Somewhat negative",
      "Very Negative"
    )
  )



tbdat$Negative.Buddhists <-
  factor(
    tbdat$Negative.Buddhists ,
    levels = c(1:5),
    labels = c(
      "Very positive",
      "Somewhat positive",
      "Neither negative nor positive",
      "Somewhat negative",
      "Very Negative"
    )
  )



tbdat$Negative.Christians <-
  factor(
    tbdat$Negative.Christians ,
    levels = c(1:5),
    labels = c(
      "Very positive",
      "Somewhat positive",
      "Neither negative nor positive",
      "Somewhat negative",
      "Very Negative"
    )
  )




tbdat$Negative.Jews <- factor(
  tbdat$Negative.Jews ,
  levels = c(1:5),
  labels = c(
    "Very positive",
    "Somewhat positive",
    "Neither negative nor positive",
    "Somewhat negative",
    "Very Negative"
  )
)

tbdat$Negative.Atheists <-
  factor(
    tbdat$Negative.Atheists ,
    levels = c(1:5),
    labels = c(
      "Very positive",
      "Somewhat positive",
      "Neither negative nor positive",
      "Somewhat negative",
      "Very Negative"
    )
  )


tbdat$Threatened.Muslims <-
  factor(
    tbdat$Threatened.Muslims ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )



tbdat$Threatened.Hindus <-
  factor(
    tbdat$Threatened.Hindus ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )


tbdat$Threatened.Buddhists <-
  factor(
    tbdat$Threatened.Buddhists ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )



tbdat$Threatened.Christians <-
  factor(
    tbdat$Threatened.Christians ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )




tbdat$Threatened.Jews <-
  factor(
    tbdat$Threatened.Jews ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )



tbdat$Threatened.Atheists <-
  factor(
    tbdat$Threatened.Atheists ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )




str(tbdat)




# tables

library(table1) # very useful package

#
# head(short)


#not working
#tbdat$NZeuro <- factor(tbdat$nzeuro,
#                         levels = c(0,1),
#                         labels = c("Not European NZ", "European NZ"))
tbdat$male <- factor(tbdat$male ,
                     levels = c(0, 1),
                     labels = c("Not Male", "Male"))
#
tbdat$religious <-
  factor(
    tbdat$notreligious ,
    levels = c(0, 1),
    #note reverse coding here
    labels = c("Religious", "Not Religious")
  )
#
tbdat$rural <- factor(tbdat$rural ,
                      levels = c(0, 1),
                      labels = c("Not Rural", "Rural"))
#
# table1::label(short.0$age) <-"Age"
# table1::label(short.0$rightwing) <-"Rightwing"
# table1::label(short.0$religiosityAll) <-"Religiosity"
# table1::label(short.0$highestqual) <-"Highest Educational Qualification"
# table1::label(short.0$NZeuro) <-"NZ European"
# # table1::label(short.0$Negative.Muslims) <- "Negative to Muslims"
# # table1::label(short.0$Negative.Hindus) <- "Negative to Hindus"
# # table1::label(short.0$Negative.Buddhists) <- "Negative to Buddhists"
# # table1::label(short.0$Negative.Christians) <- "Negative to Christians"
# # table1::label(short.0$Negative.Jews) <- "Negative Jews"
# # table1::label(short.0$Negative.Atheists) <- "Negative to Atheists"
# # table1::label(short.0$Threatened.Muslims) <- "Threatened by Muslims"
# # table1::label(short.0$Threatened.Hindus) <- "Threatened by Hindus"
# table1::label(short.0$Threatened.Buddhists) <- "Threatened by Buddhists"
# table1::label(short.0$Threatened.Christians) <- "Threatened by Christians"
# table1::label(short.0$Threatened.Jews) <- "Threatened by Jews"
# table1::label(short.0$Threatened.Atheists) <- "Threatened by Atheists"



## demographics tables

tb <- tbdat %>%
  filter(measuredbothyears == 1)
table1::table1(
  ~ age + male + highestqual +  nzeuro + religious++rightwing + rural + Negative.Atheists + Negative.Buddhists + Negative.Christians +
    Negative.Hindus + Negative.Jews + Negative.Muslims +
    Threatened.Atheists + Threatened.Buddhists + Threatened.Christians +  Threatened.Hindus + Threatened.Jews + Threatened.Muslims |
    wave,
  data = tb,
  overall = F
)

tb$weight
##
library(sjPlot)
library(gridExtra)
library(gridarrange)

plot_frq(tbdat$Negative.Hindus, show.na = F)

# grouped data frame, all panels in one plot
test <- tb %>%
  group_by(wave) %>%
  plot_frq(Negative.Hindus, show.n = T, type = "bar")) %>%
  plot_grid()

test2 <- tb %>%
  group_by(wave) %>%
  plot_frq(
    Negative.Muslims,
    geom.colors = "red",
    show.n = T,
    type = "bar"
  ) %>%
  plot_grid()

library(patchwork)
grid.arrange(test,test2)



test3 <- tb %>%
  group_by(wave) %>%
  plot_frq(Threatened.Hindus, show.n = T, type = "bar") %>%
  plot_grid()

test4 <- tb %>%
  group_by(wave) %>%
  plot_frq(
    Threatened.Muslims,
    Threatened.Hindus,
    geom.colors = "red",
    show.n = T,
    type = "bar"
  ) %>%
  plot_grid()


grid.arrange(test3, test4, ncol = 2)
plot_grid(list(test3, test4))

####  Do differently ####


pl1 <- plot_xtab(
  tb$Negative.Hindus,
  tb$wave,
  show.n = F,
  show.prc = T ,
  show.total = F,
  drop.empty = F
) +
  # weight.by = tb$weight,
  xlab("Attitudes to Hindus") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
pl1

pl2 <-
  plot_xtab(
    tb$Negative.Muslims,
    tb$wave,
    show.total = F,
    show.n = F,
    #       weight.by = tb$weight,
    geom.colors = c("lightgreen", "darkred")
  ) +
  xlab("Attitudes to Muslims") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl2

pl3 <- plot_xtab(
  tb$Negative.Christians,
  tb$wave,
  show.total = F,
  #   weight.by = tb$weight,
  show.n = F,
  geom.colors = c("darkred", "darkblue")
) +
  xlab("Attitudes to Christians") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl3

pl4 <- plot_xtab(
  tb$Negative.Atheists,
  tb$wave,
  show.total = F,
  #weight.by = tb$weight,
  show.n = F,
  geom.colors = c("paired")
) +
  xlab("Attitudes to Atheists") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


pl4

pl5 <- plot_xtab(
  tb$Negative.Jews,
  tb$wave,
  show.total = F,
  #weight.by = tb$weight,
  show.n = F,
  geom.colors = c("yellow", "green")
) +
  xlab("Attitudes to Jews") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl5
pl6 <- plot_xtab(
  tb$Negative.Buddhists,
  tb$wave,
  show.total = F,
  #weight.by = tb$weight,
  show.n = F,
  geom.colors = c("orange", "lightblue")
) +
  xlab("Attitudes to Buddhists") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


pl1t <- plot_xtab(tb$Threatened.Hindus,
                  tb$wave,
                  show.total = F,
                  # weight.by = tb$weight,
                  show.n = F) +
  xlab("Threatened by Hindus") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl2t <- plot_xtab(
  tb$Threatened.Muslims,
  tb$wave,
  show.total = F,
  #   weight.by = tb$weight,
  show.n = F,
  geom.colors = c("lightgreen", "darkred")
) +
  xlab("Threatened by Muslims") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl3t <- plot_xtab(
  tb$Threatened.Christians,
  tb$wave,
  show.total = F,
  #   weight.by = tb$weight,
  show.n = F,
  geom.colors = c("darkred", "darkblue")
) +
  xlab("Threatened by Christians") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl4t <- plot_xtab(
  tb$Threatened.Atheists,
  tb$wave,
  show.total = F,
  #weight.by = tb$weight,
  show.n = F,
  geom.colors = c("paired")
) +
  xlab("Threatened by Atheists") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl5t <- plot_xtab(
  tb$Threatened.Jews,
  tb$wave,
  show.total = F,
  #weight.by = tb$weight,
  show.n = F,
  geom.colors = c("yellow", "green")
) +
  xlab("Threatened by Jews") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl5t
pl6t <- plot_xtab(
  tb$Threatened.Buddhists,
  tb$wave,
  show.total = F,
  #weight.by = tb$weight,
  show.n = F,
  geom.colors = c("orange", "lightblue")
) +
  xlab("Threatened by Buddhists") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

pl6t

library(patchwork)
pl1 + pl4 +
  pl5 + pl6 +
  pl3 + pl2 +
  plot_layout(nrow = 3, ncol = 2) +
  plot_annotation(title = "Longitudional Attitudes to Religious Groups pre/post Christchurch Mosque attacks (N = 350)")

pl1 + pl6 +
  pl5 +  pl2 +
  pl4 + pl3 +
  plot_layout(nrow = 3, ncol = 2) +
  plot_annotation(title = "Longitudional Attitudes to Religious Groups pre/post Christchurch Mosque attacks (N = 350)")

#
# # make wide data for seqHMM
# ## MAKE WidEVERSION
# widethreatmus <- tbdat %>%
#   select(id, wave, Threatened.Muslims)
# head(widethreatmus)
#
# # wide data
# hourswide <- spread(widethreatmus, wave, Threatened.Muslims)
# dim(hourswide)
# str(hourswide)
#
# colswide_Mus <- hourswide[, 2:3]
# colswide_Mus
#
# nrow(colswide_Mus) ## 66
#
#
#
#
# library("seqHMM")   # Note that these models are not stable.
# muslimPP = seqdef(colswide_Mus)
# muslimPP
# # notmuslimPP = seqdef(colsNot_Mus)
# # notmuslimPP
#
# plot(notmuslimPP)
# plot(muslimPP)
#
#
# sc_initmod_random <-
#   build_hmm(observations = muslimPP, n_states = 3)
# #sc_initmod_random <- build_hmm(observations = religious_seq, n_states = 3)
#
# sc_initmod_random$initial_probs
# sc_initmod <- build_hmm(
#   observations = muslimPP,
#   initial_probs = sc_initmod_random$initial_probs,
#   transition_probs = sc_initmod_random$transition_probs,
#   emission_probs = sc_initmod_random$emission_probs
# )
#
# sc_initmod
# sc_fit <- fit_model(sc_initmod)
# fm <- sc_fit$model
# fm
# summary(fm, conditional_se = FALSE)
# library(igraph)
# plot(
#   sc_fit$model,
#   #layout = layout_nicely,
#   # vertex.label = c("Highest= 23%", "Mixed= 77%", "Middle = 0.0032%"),
#   # hidden.states.labels =c("Hidden 1","Hidden 2"),
#   legend.prop = .2,
#   main = "Hidden Markov Model for Dynamics of Muslim Perceived Discrimination: 2015-2017",
#   #  layout = layout_nicely,
#   interactive = TRUE,
#   ncol.legend = 1
# )




#####
# LPA
library(tidyLPA)
tbdat$wave
lpa.threat.1.0 <- tbdat %>%
  dplyr::filter(wave == "2018") %>%
  dplyr::select(
    Threatened.Muslims,
    Threatened.Christians,
    Threatened.Hindus,
    Threatened.Buddhists,
    Threatened.Jews,
    Threatened.Atheists
  ) %>%
  dplyr::mutate(
    Threatened.Muslims = as.numeric(Threatened.Muslims),
    Threatened.Christians = as.numeric(Threatened.Christians),
    Threatened.Hindus = as.numeric(Threatened.Hindus),
    Threatened.Buddhists = as.numeric(Threatened.Buddhists),
    Threatened.Jews = as.numeric(Threatened.Jews),
    Threatened.Atheists = as.numeric(Threatened.Atheists)
  ) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(4)
# tidyLPA::estimate_profiles(c(2,3,4,5,6,7,8,9,10))
plot1 <- tidyLPA::plot_profiles(lpa.threat.1.0, add_line = T)

tidyLPA::get_fit(lpa.threat.1.0)
plot1
tbdat$Class0
gotn <- tidyLPA::get_data(lpa.threat.1.0)
tbdat$Class0 <- gotn$Class
table(tbdat$Class0)
mean(tbdat$Class0 == 1) #0.05
mean(tbdat$Class0 == 2) #0.32
mean(tbdat$Class0 == 3)# 0.50
mean(tbdat$Class0 == 4)# 0.13



tbdat$Class0 <- factor(
  tbdat$Class0,
  levels = c(1:4),
  labels = c(
    "Extreme Islamophobia ",
    "High Islamophobia",
    "low Islamophobia",
    "Pro Buddhism/ High Islamophobia"
  )
)


#
# lpa.threat.2.0<-tbdat%>%
#   dplyr::filter(wave == "2019")%>%
#   dplyr::select(Threatened.Muslims,Threatened.Christians,Threatened.Hindus,Threatened.Buddhists,
#                 Threatened.Jews,Threatened.Atheists)%>%
#   dplyr::mutate(Threatened.Muslims = as.numeric(Threatened.Muslims),
#                 Threatened.Christians = as.numeric(Threatened.Christians),
#                 Threatened.Hindus = as.numeric(Threatened.Hindus),
#                 Threatened.Buddhists = as.numeric(Threatened.Buddhists),
#                 Threatened.Jews = as.numeric(Threatened.Jews),
#                 Threatened.Atheists = as.numeric(Threatened.Atheists))%>%
#   tidyLPA::single_imputation() %>%
#   tidyLPA::estimate_profiles(4)
# # tidyLPA::estimate_profiles(c(2,3,4,5,6,7,8,9,10))
# plot2<-tidyLPA::plot_profiles(lpa.threat.2.0,add_line = T)
#
# plot2
# nrow(gotn)
saveRDS(lpa.threat.1.0, "lpa.threat.1.0")


# write dag
library(dagitty)

dg<-ggdag::dagify(
  thrt ~ age + rel + edu + right + male + rural + attack,
  rel ~ age + right + male + rural,
  right ~ religion + age + rural,
  edu ~ age,
  exposure = "attack",
  outcome = "thrt"
)

ggdag::ggdag_dseparated(dg, from =  "attack", to = "thrt", controlling_for = "rel",
                        collider_lines = TRUE)
ggdag::ggdag_collider(dg)

# attack will give an unbaised estimate

library(lme4)
m1- lmer(
  as.numeric(Threatened.Muslims) ~ wave +
    (1 | id),
  data = tbdat
)

plot1 <- plot_model(
  LMER.mod.threat.mus,
  terms = c("religiousity.S", "wave"),
  type = "pred",
  grid = T
)

plot1 + xlab("Religiosity (standarised)") + ylab("Threatened by Muslims (scale 1-4)") + scale_y_continuous(limits=c(1,4))



library(brms)
#library(brmstools)
library(rstan)
library(ggeffects)
library(sjPlot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores ())

mt <- brm(
  as.numeric(Threatened.Muslims) ~ wave * r_s +   (1 | id),
  family = cumulative("probit"),
  # family = sratio(),
  data = tbdat
)


test<-tbdat %>%dplyr::filter(!is.na(Threatened.Muslims))
ts<-plot(Threatened.Muslims ~ wave, data = test)
tab_model(ts)
mtI <- bf(Threatened.Muslims | mi() ~ wave + (1 | id))
head(test)
fit_imp2 <- brms::brm(mtI, data = tbdat)




plot_model(
  mt,
  #  show.values = T,
  show.ci = .9,
  type = 'est',
  value.offset = .3,
  show.intercept = TRUE,
  sort.est = F
) #+  theme_blank()


tab_model(mt, show.ci = .9) #+  theme_blank()
summary(mt)

library(tidybayes)
get_variables(mod.threat.mus)

mod.threat.mus %>%
  spread_draws(b_wave2019) %>%
  head(10)







## THIS IS COOL
# cons <- make_conditions(tbdat, vars = c("wave"))
# cons

me <- conditional_effects(
  mt,
  c("wave","r_s"),
  response = "Threatened.Muslims",
  prob = c(.89),
  categorical = TRUE,
  spaghetti = TRUE
 # conditions = cons
)


library(ggthemes)
library(viridisLite)
p <- plot(me)[[1]]+ theme_classic() + scale_fill_viridis_c()
p

pol <- plot(pe)[[1]]

pagep <- plot(agep)[[1]]


hip <- plot(hi)[[1]]

# p + guides(color=guide_legend("my title")) +  theme(legend.position = "none")
#
# pp <- p +theme(legend.position = "none")

#p<- plot(me)[[1]]
# pp  + ylab("Probability") + theme(legend.position = "bottom") +
#   xlab("Religiosity (Stanardised)") +
#   scale_fill_discrete(
#     name = "", labels = c("1 = Not at all", "2 = Not Very", "3 = Somewhat", "4 = Very"))
#


# p +
#   scale_fill_discrete(name = "", labels = c("1 = Not at all", "2 = Not Very", "3 = Somewhat", "4 = Very")) +
#   labs(title = "Fuel economy declines as weight increases",
#           subtitle = "(1973-74)",
#           caption = "Data from the 1974 Motor Trend US magazine.",
#           tag = "Figure 1",
#           x = "Weight (1000 lbs)",
#           y = "Fuel economy (mpg)",
#           colour = "Gears")



# pp<-p + labs(x="Religiosity (Standardised)", y="Probability",
#              title="Threatened by Muslims") +
#   scale_fill_discrete(name = "Level",
#                       labels = c("Not at all Threatened", "Not Very Threatened", "Somewhat Threatened", "Very Threatened")) +
#   guides(colour = FALSE) +
#   theme(legend.title = element_blank(),
#         legend.position = "right",
#         legend.text = element_text(size = 6) )
#
# pp
# scale_fill_discrete(name = "", labels = c("1 = Not at all", "2 = Not Very", "3 = Somewhat", "4 = Very"))
# labs(x="Religiosity (Standardised)", y="Perceived Threat",title="Threatened by Muslims", col ="Level")

# guides(fill = guide_legend(title = "LEFT", title.position = "left"))

#scale_fill_discrete(name = "Levels", labels = c("1 = Not at all", "2 = Not Very", "3 = Somewhat", "4 = Very"))

# theme_get(pp)
# pp #+ theme_minimal()
# ppp<-pp +  scale_fill_discrete(name = "", labels = c("1 = Not at all", "2 = Not Very", "3 = Somewhat", "4 = Very"))
# ppp + labs(x="Religiosity (Standardised)", y="Perceived Threat",title="Threatened by Muslims", col ="Level")
#
# theme_get(p)
# #pp + theme(legend.title = element_blank())


#threat.muslims.regression.religion## 1100 by 1100
d1 <- p +
  labs(
    title = "Perceived Level of Threat of Muslims *Reverses* to Acceptance among Religiously Committed",
    # subtitle = "(before Mosque attacks,after Mosque attacks)",
    #     caption = "Data from 2018/2019 ISSP Panel Survey",
    # tag = "Figure 1",
    x = "Religiosity (Standardised)",
    y = "Probability",
    colour = ""
  ) +
  #theme(legend.position = "none") +
  guides(colour = FALSE) +
  # scale_y_continuous(limits =c(0,.8)) +
  #  theme(legend.title = element_blank(),
  #     axis.title.x = element_text(size = 8),
  #     axis.title.y = element_text(size = 8),
  #      legend.position = "bottom",
  #     legend.text = element_text(size = 8),
  #      plot.title = element_text(size = 8, face = "bold")) +
  scale_fill_discrete(
    name = "",
    # labels = c("1 = Not at all", "2 = Not Very", "3 = Somewhat", "4 = Very")) +  #theme(legend.title = element_blank(),
    labels = c(
      "*Not at all Threatened",
      "Not Very Threatened",
      "*Somewhat Threatened",
      "Very Threatened"
    )
  )#+
# theme(legend.title = element_blank(),legend.position = "right",legend.text = element_text(size = 12) ) + theme(legend.position = "none")

d1




d1 <- p +
  labs(
    title = "Perceived Level of Threat of Muslims *Reverses* to Acceptance among Religiously Committed",
    # subtitle = "(before Mosque attacks,after Mosque attacks)",
    #     caption = "Data from 2018/2019 ISSP Panel Survey",
    # tag = "Figure 1",
    x = "Religiosity (Standardised)",
    y = "Probability",
    colour = ""
  ) +
  #theme(legend.position = "none") +
  guides(colour = FALSE) +
  scale_y_continuous(limits = c(0, .8)) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 8, face = "bold")
  ) +
  scale_fill_discrete(
    name = "",
    # labels = c("1 = Not at all", "2 = Not Very", "3 = Somewhat", "4 = Very")) +  #theme(legend.title = element_blank(),
    labels = c(
      "*Not at all Threatened",
      "Not Very Threatened",
      "*Somewhat Threatened",
      "Very Threatened"
    )
  )#+
# theme(legend.title = element_blank(),legend.position = "right",legend.text = element_text(size = 12) ) + theme(legend.position = "none")

d1


##  2019 Analysis
library(tidyverse)
library(sjPlot)
library(sjmisc)
#library(dplyr)
dat
table(dat2$Aid)
nrow(dat2.1)
lon$t0.leftright
nrow(dat)
# get data into shape
short.1 <- dat %>%
  arrange(Aid) %>%
  dplyr::mutate(id = as.factor(Aid)) %>%
  dplyr::mutate(notreligious = ifelse(nowrelig == 0, 1, 0)) %>%
  dplyr::mutate(Rural = ifelse(townsize == 1 |
                                 townsize == 2 | townsize == 3, 0, 1)) %>%
  dplyr::mutate(religiosityAll = ifelse(nowrelig == 0, 0, religiosity)) %>%
  dplyr::mutate(male = ifelse(
    gender == "M" |
      gender == "male" |  gender == "Male" | gender == "MALE",
    1,
    0
  )) %>%
  dplyr::mutate(year = rep(0, nrow(dat))) %>%
  dplyr::mutate(religiousity = (religiosity)) %>%
  dplyr::mutate(year = rep(0, nrow(dat))) %>%
  dplyr::select(
    id,
    #Negative.Christians,
    #                 Negative.Muslims,
    #                 Negative.Hindus,
    #                 Negative.Buddhists,
    #                 Negative.Jews,
    #                 Negative.Atheists,
    #                 Threatened.Christians,
    #                 Threatened.Muslims,
    #                 Threatened.Hindus,
    #                 Threatened.Buddhists,
    #                 Threatened.Jews,
    #                 Threatened.Atheists,
    eduyears,
    religaccept,
    religiousity,
    # leftright,
    # Religious, #
    # Religion,
    Rural,
    male,
    #  Euro,
    # nowdenomx,
    # townsize,
    age,
    # birth,
    gender,
    nowrelig,
    religinf,
    religconflict,
    religintolerance,
    religpower,
    #religaccept,
    sciencefaith,
    religcomfort,
    attitudechr,
    attitudemus,
    attitudehin,
    attitudebud,
    attitudejew,
    attitudeath,
    threatchr,
    threatmus,
    threathin,
    threatbud,
    threatjew,
    threatath,
    # age,
    #  birth,
    gender,
    nzeuro,
    religintolerance,
    religaccept,
    religiosityAll,
    notreligious,
    highestqual,
    sciencefaith,
    rightwing,
    year
  ) %>%
  #  mutate(measured = rep(1,nrow(dat)))%>%
  arrange(id)

tail(dat$Aid)
tail(dat2$Aid)
str(short.1$id)
str(short.1$nzeuro) # Bad

str(dat)
dat2$Aid

table(dat2$gender)
dat2$Aid

dat2$birth[dat2$birth == 1853] <- 1953  # fix problem


short.2 <- dat2 %>%
  arrange(Aid) %>%
  dplyr::mutate(id = as.factor(Aid)) %>%
  dplyr::mutate(notreligious = ifelse(nowrelig == 0, 1, 0)) %>%
  dplyr::mutate(religiosityAll = ifelse(nowrelig == 0, 0, religiousity)) %>%
  dplyr::mutate(Rural = ifelse(townsize == 1 |
                                 townsize == 2 | townsize == 3, 0, 1)) %>%
  dplyr::mutate(gender = factor(gender)) %>%
  dplyr::mutate(gender = na_if(gender, "")) %>%
  dplyr::mutate(male = ifelse(
    gender == "M" |
      gender == "male" |  gender == "Male" | gender == "MALE",
    1,
    0
  )) %>%
  dplyr::mutate(rightwing = leftright) %>%
  dplyr::mutate(year = rep(1, nrow(dat2))) %>%
  dplyr::mutate(age = as.numeric(2019 - birth)) %>%
  dplyr::select(
    c(
      id,
      #Negative.Christians,
      #                 Negative.Muslims,
      #                 Negative.Hindus,
      #                 Negative.Buddhists,
      #                 Negative.Jews,
      #                 Negative.Atheists,
      #                 Threatened.Christians,
      #                 Threatened.Muslims,
      #                 Threatened.Hindus,
      #                 Threatened.Buddhists,
      #                 Threatened.Jews,
      #                 Threatened.Atheists,
      eduyears,
      religaccept,
      religiousity,
      # leftright,
      # Religious, #
      # Religion,
      Rural,
      male,
      #  Euro,
      # nowdenomx,
      # townsize,
      age,
      # birth,
      gender,
      nowrelig,
      religinf,
      religconflict,
      religintolerance,
      religpower,
      #religaccept,
      sciencefaith,
      religcomfort,
      attitudechr,
      attitudemus,
      attitudehin,
      attitudebud,
      attitudejew,
      attitudeath,
      threatchr,
      threatmus,
      threathin,
      threatbud,
      threatjew,
      threatath,
      # age,
      # birth,
      gender,
      nzeuro,
      religintolerance,
      religaccept,
      religiosityAll,
      notreligious,
      highestqual,
      sciencefaith,
      rightwing,
      year
    )
  ) %>%
  #  mutate(measured = rep(1,nrow(dat2)))%>%
  arrange(id)




dim(short.2)
? table

short.2m <- short.2 %>% mutate(
  Negative.Muslims = attitudemus,
  Negative.Hindus  = attitudehin,
  Negative.Buddhists = attitudebud,
  Negative.Christians = attitudechr,
  Negative.Jews = attitudejew,
  Negative.Atheists = attitudeath,
  Threatened.Muslims = 5 - threatmus,
  Threatened.Hindus = 5 - threathin,
  Threatened.Buddhists = 5 - threatbud,
  Threatened.Christians = 5 - threatchr,
  Threatened.Jews = 5 - threatjew,
  Threatened.Atheists = 5 - threatath
) %>%
  arrange(id)


str(short.1m)
str(short.2m)
dat2.1$oldthreathin
dat2.1$oldthreatmus[dat2.1$oldthreatmus == 0] <- NA
dat2.1$oldthreathin[dat2.1$oldthreathin == 0] <- NA
table(dat2.1$oldthreatmus, useNA = "ifany")

dat2.1$oldattitudemus[dat2.1$oldattitudemus == 0] <- NA
table(dat2.1$oldattitudemus, useNA = "ifany")

table(short.2$threatmus, useNA = "ifany")

short.1m <- dat2.1 %>% mutate(
  Negative.Muslims = oldattitudemus,
  Negative.Hindus  = oldattitudehin,
  Negative.Buddhists = oldattitudebud,
  Negative.Christians = oldattitudechr,
  Negative.Jews = oldattitudejew,
  Negative.Atheists = oldattitudeath,
  Threatened.Muslims = 5 - oldthreatmus,
  Threatened.Hindus = 5 - oldthreathin,
  Threatened.Buddhists = 5 - oldthreatbud,
  Threatened.Christians = 5 - oldthreatchr,
  Threatened.Jews = 5 - oldthreatjew,
  Threatened.Atheists = 5 - oldthreatath
) %>%
  dplyr::mutate(
    age.c = scale(age, scale = F, center = T),
    age.decade.c = scale(age, scale = F, center = T) / 10,
    rural = factor(Rural),
    gender = factor(gender),
    male = factor(male),
    highestqual  = as.numeric(highestqual),
    relaccept.S = scale(religaccept, center = TRUE, scale =
                          T),
    rightwing.S = scale(rightwing, center = TRUE, scale = T),
    religiosityAll.S = scale(religiosityAll),
    highestqual.c = scale(highestqual, center = T, scale =
                            T)
  )

table(short.2m$Threatened.Muslims, useNA = "ifany")
table(short.2m$Threatened.Hindus, useNA = "ifany")
table(short.1m$Threatened.Muslims, useNA = "ifany")
table(short.1m$threatmus, useNA = "ifany")
table(short.1m$oldthreatmus, useNA = "ifany")
table
# short.1m$oldthreatmus[short.1m$oldthreatmus == 0] <- NA
# table(short.1m$oldthreatmus, useNA = "ifany")
#
# short.1m$oldthreatmus[short.1m$oldthreatmus == 0] <- NA
# table(short.1m$oldthreatmus, useNA = "ifany")

table(short.2$threatmus, useNA = "ifany")



table(dat2.1$oldthreatmus, useNA = "ifany")

table(dat2.1$threatmus, useNA = "ifany")
short.2


? table

#short.2.2<- short.2[1:335,]

(short.1$id)
str(short.2$year)
# summary(short.2$age) fixed
table(short.1$measured)

combo <- (rbind(short.1, short.2)) %>%
  mutate(
    religintolerance = na_if(religintolerance, 9),
    religinf = na_if(religinf, 9),
    religconflict = na_if(religconflict, 9),
    religintolerance = na_if(religintolerance, 9),
    religpower = na_if(religpower, 9),
    sciencefaith = na_if(sciencefaith, 9),
    religcomfort = na_if(religcomfort, 9),
    attitudechr = na_if(attitudechr, 9),
    attitudemus = na_if(attitudemus, 9),
    attitudehin = na_if(attitudehin, 9),
    attitudebud = na_if(attitudebud, 9),
    attitudejew = na_if(attitudejew, 9),
    attitudeath = na_if(attitudeath, 9),
    threatchr = na_if(threatchr, 9),
    threatmus = na_if(threatmus, 9),
    threathin = na_if(threathin, 9),
    threatbud = na_if(threatbud, 9),
    threatjew = na_if(threatjew , 9),
    threatath = na_if(threatath, 9),
    nzeuro = factor(nzeuro),
    notreligious = factor(notreligious),
    year = factor(year),
    id = factor(id)
  ) %>%
  mutate(
    Negative.Muslims = attitudemus,
    Negative.Hindus  = attitudehin,
    Negative.Buddhists = attitudebud,
    Negative.Christians = attitudechr,
    Negative.Jews = attitudejew,
    Negative.Atheists = attitudeath,
    Threatened.Muslims = 5 - threatmus,
    Threatened.Hindus = 5 - threathin,
    Threatened.Buddhists = 5 - threatbud,
    Threatened.Christians = 5 - threatchr,
    Threatened.Jews = 5 - threatjew,
    Threatened.Atheists = 5 - threatath
  ) %>%
  #mutate(measured = ifelse(year ==1 & measured ==1, 1, 0 ))%>%
  arrange(year, id)

table(combo$Threatened.Muslims)

? rbind
str(combo$id)
str(combo$year)
str(short.1$id)
str(short.2$gender)


combo.scaled <- combo %>%
  dplyr::mutate(
    age.c = scale(age, scale = F, center = T),
    age.decade.c = scale(age, scale = F, center = T) / 10,
    rural = factor(Rural),
    gender = factor(gender),
    male = factor(male),
    highestqual  = as.numeric(highestqual),
    relaccept.S = scale(religaccept, center = TRUE, scale =
                          T),
    rightwing.S = scale(rightwing, center = TRUE, scale = T),
    religiosityAll.S = scale(religiosityAll),
    highestqual.c = scale(highestqual, center = T, scale =
                            T)
  )

str(combo.scaled)
#hist(combo.scaled$highestqual)





# hist(short.scaled$sciencefaith)
# summary(short.scaled$sciencefaith)
# library(tidyr)
#
combo.scaled <- combo.scaled %>%
  dplyr::mutate(sciencefaith = na_if(sciencefaith, 9))
#
# summary(short.scaled$sciencefaith)
# hist(short.scaled$sciencefaith)


# Set theme for plots
theme_ggeffects(base_family = "blank")
theme_sjplot2(base_family = "blank")
set_theme(base = theme_sjplot2())
library(sjPlot)

str(dat$Threatened.Muslims)
str(short.1m$Negative.Muslims)
hist(short.2m$Threatened.Muslims)
table(short.1m$Threatened.Muslims)
# get lables in order
dat$Negative.Muslims <- factor(
  dat$Negative.Muslims ,
  levels = c(1:5),
  labels = c(
    "Very positive",
    "Somewhat positive",
    "Neither negative nor positive",
    "Somewhat negative",
    "Very Negative"
  )
)

#dat$Threatened.Muslims<- factor(dat$Threatened.Muslims ,levels = c(1:4),
labels = c(
  "Not threatening at all",
  "Not very threatening",
  "Somewhat threatening",
  "Very threatening"
))


short.2m$Negative.Muslims <-
  factor(
    short.2m$Negative.Muslims ,
    levels = c(1:5),
    labels = c(
      "Very positive",
      "Somewhat positive",
      "Neither negative nor positive",
      "Somewhat negative",
      "Very Negative"
    )
  )

short.1m$Threatened.Hindus <-
  factor(
    short.1m$Threatened.Hindus ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )


short.2m$Threatened.Muslims <-
  factor(
    short.2m$Threatened.Muslims ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )
table(short.1m$Threatened.Muslims)
table(short.2m$Threatened.Muslims)

short.1m$Threatened.Muslims <-
  factor(
    short.1m$Threatened.Muslims ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )

short.2m$Threatened.Muslims <-
  factor(
    short.2m$Threatened.Muslims ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )
table(short.2m$Threatened.Muslims)


short.1m$Threatened.Hindus <-
  factor(
    short.1m$Threatened.Hindus ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )
short.2m$Threatened.Hindus <-
  factor(
    short.2m$Threatened.Hindus ,
    levels = c(1:4),
    labels = c(
      "Not threatening at all",
      "Not very threatening",
      "Somewhat threatening",
      "Very threatening"
    )
  )
short.1m$Threatened.Hindus
table(short.1m$Threatened.Hindus)
table(short.2m$Threatened.Hindus)


plot_frq(short.1m$Negative.Hindus,
         show.na = F,
         weight.by = weight)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 2019 sub
plot_frq(short.2m$Negative.Hindus, show.na = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2018 Full
plot_frq(dat$Negative.Muslims, show.na = F)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2018 sub
plot_frq(short.1m$Negative.Muslims, show.na = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2019 sub
plot_frq(short.2m$Negative.Muslims, show.na = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sub2018

fh <- plot_frq(dat$Threatened.Hindus, show.na = F) ++xlab("Threaten by Hindus") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
p1
h1 <-
  plot_frq(short.1m$Threatened.Hindus, show.na = F) + theme_bw() + labs(title =
                                                                          "2018: pre-attacks, n= 325 (subset)")  +   xlab("Threaten by Hindus") +
  ylab("Frequency") +
  #  scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

h2 <-
  plot_frq(short.2m$Threatened.Hindus, show.na = F) + theme_bw() + labs(title =
                                                                          "2019: post-attacks, n= 325 (subset)")  +   xlab("Threaten by Hindus") +
  ylab("Frequency") +
  #  scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


full2019 <- plot_frq(dat$Threatened.Muslims, show.na = F)
full2019
sub2018 <- plot_frq(short.1m$Threatened.Muslims, show.na = F)
sub2018

short.1m$Threatened.Muslims

sub2019 <- plot_frq(short.2m$Threatened.Muslims, show.na = F)

sub2019


library(patchwork)
p1 <-
  full2019 +  theme_bw()  + labs(title = "2018: pre-attacks, N = 1335 (full)") +  xlab("Threaten by Muslims") +  ylab("Frequency") +
  #scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
p1

p2 <-
  sub2018 +  theme_bw() + labs(title = "2018: pre-attacks, n= 325 (subset)")  +   xlab("Threaten by Muslims") +
  ylab("Frequency") +
  #  scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p2

p3 <-
  sub2019 +  theme_bw() + labs(title = "2019: post-attacks, n= 325 (subset)")  + xlab("Threaten by Muslims") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
#scale_y_continuous(limits=c(0,7)) + #theme(plot.title = element_text(size=09)) +
#  theme(axis.text.x = element_text(angle = 45))
p3

p1 +
  {
    p2 + p3
  } + plot_layout(nrow = 2, ncol = 1) +
  plot_annotation(title = "Frequences of perceived threat of Muslims in pre/post attacks")


{
  p2 + p3
  } + plot_layout(nrow = 1, ncol = 2) +
  plot_annotation(title = "Frequences of perceived threat of Muslims in pre/post attacks")

{
  h1 + h2
  } + plot_layout(nrow = 1, ncol = 2) +
  plot_annotation(title = "Frequences of perceived threat of Hindus in pre/post attacks")


plot_frq(short.2$attitudemus, show.na = F)




# plot_frq(combo$Threatened.Muslims,show.na = F)
# plot_frq(dat$Threatened.Muslims,show.na = F)
#
# plot_frq(short$Threatened.Hindus,show.na = T)
# plot_frq(short$Threatened.Buddhists,show.na = T)
# plot_frq(short$Threatened.Jews,show.na = T)
# plot_frq(short$Threatened.Atheists,show.na = T)
#
#
#
# sjp.frq(short$Negative.Muslims,show.na = T)
# sjp.frq(short$Negative.Muslims,show.na = T)
# sjp.frq(short$Negative.Hindus,show.na = T)
# sjp.frq(short$Negative.Buddhists,show.na = T)
# sjp.frq(short$Negative.Jews,show.na = T)
# sjp.frq(short$Negative.Atheists,show.na = T)
#


# create tables

library(table1) # very useful package

#
# head(short)
# short.0<- combo.scaled
#
# short.0$NZeuro <- factor(short.0$Euro,
#                          levels = c(0,1),
#                          labels = c("Not European NZ", "European NZ"))
# short.0$Man<- factor(short.0$Man ,levels = c(0,1),
#                      labels = c("Not Male", "Male"))
#
# short.0$Religious<- factor(short.0$Religious ,levels = c(0,1),
#                            labels = c("Not Religious", "Religious"))
#
# short.0$Rural<- factor(short.0$Rural ,levels = c(0,1),
#                        labels = c("Not Rural", "Rural"))
#
# table1::label(short.0$age) <-"Age"
# table1::label(short.0$rightwing) <-"Rightwing"
# table1::label(short.0$religiosityAll) <-"Religiosity"
# table1::label(short.0$highestqual) <-"Highest Educational Qualification"
# table1::label(short.0$NZeuro) <-"NZ European"
# # table1::label(short.0$Negative.Muslims) <- "Negative to Muslims"
# # table1::label(short.0$Negative.Hindus) <- "Negative to Hindus"
# # table1::label(short.0$Negative.Buddhists) <- "Negative to Buddhists"
# # table1::label(short.0$Negative.Christians) <- "Negative to Christians"
# # table1::label(short.0$Negative.Jews) <- "Negative Jews"
# # table1::label(short.0$Negative.Atheists) <- "Negative to Atheists"
# # table1::label(short.0$Threatened.Muslims) <- "Threatened by Muslims"
# # table1::label(short.0$Threatened.Hindus) <- "Threatened by Hindus"
# table1::label(short.0$Threatened.Buddhists) <- "Threatened by Buddhists"
# table1::label(short.0$Threatened.Christians) <- "Threatened by Christians"
# table1::label(short.0$Threatened.Jews) <- "Threatened by Jews"
# table1::label(short.0$Threatened.Atheists) <- "Threatened by Atheists"



## demographics tables
table1::table1(
  ~ age + Man + highestqual +  NZeuro + Religious + religiosityAll + rightwing + Rural,
  data = short.0,
  overall = T
)

table1::table1( ~ sciencefaith | Religious, data = short.0,
                overall = T)




# note that we have more  religious people than not
table1::table1( ~ Religious, data = short.0, overall = T)
table1::table1( ~ factor(religiosityAll), data = short.0, overall = T)# which  agrees with this variable


#Table for Negative
table1::table1(
  ~ Negative.Atheists + Negative.Buddhists + Negative.Christians +
    Negative.Hindus + Negative.Jews + Negative.Muslims,
  data = short.0,
  overall = T
)


# Table for threat  Lots of missingness
table1::table1(
  ~ Threatened.Atheists + Threatened.Buddhists + Threatened.Christians +  Threatened.Hindus + Threatened.Jews + Threatened.Muslims,
  data = short.0,
  overall = T
)


## Greater missingness overall among Christians

table1::table1(
  ~ Negative.Atheists + Negative.Buddhists + Negative.Christians + Negative.Hindus + Negative.Jews + Negative.Muslims |
    Religious,
  data = short.0,
  overall = T
)


## Greater missingness overall compared with negativeity ratings and all and reveals greater missingness Muslims among Christians

Christians
table1::table1(
  ~ Threatened.Atheists + Threatened.Buddhists + Threatened.Christians +
    Threatened.Hindus + Threatened.Jews + Threatened.Muslims |
    Religious,
  data = short.0,
  overall = T
)


table1::table1(
  ~ Threatened.Atheists + Threatened.Buddhists + Threatened.Christians +
    Threatened.Hindus + Threatened.Jews + Threatened.Muslims,
  data = short.0,
  overall = T
)



## This suggests to me that we should multiply impute but lets do the non-imputed models first




str(short.0)


## simple models

library(sjPlot)
conflicts()

# short.scaled<- combo.scaled%>%
#   dplyr::mutate(age.c = scale(age,scale=F,center=T),
#                 age.decade.c = scale(age,scale=F,center=T)/10,
#                 gender = factor(gender),
#                 nzeuro = factor(nzeuro), # not working
#                 male = factor(male),
#                 rural = factor(rural),
#                 #religiosity.S = scale(religiosity), not all
#                 relaccept.S = scale(religaccept,center=TRUE,scale=T),
#                 rightwing.S = scale(rightwing, center=TRUE, scale = T),
#                 religiosityAll.S = scale(religiosityAll),
#                 highestqual.C = scale(highestqual, center=T,scale=F))%>%
#   dplyr::select(-c(notreligious,nzeuro,gender))

str(short.scaled)
`head(short.scaled)
library(ggeffects)
#library(glmmTMB)
library(lme4)


# Univariate models = same results
#
#
#
#
# ##======


### Lets multiply impute
library(Amelia)

head(short.0)
short.1 <- short %>%
  dplyr::select(
    -c(
      gender,
      nzeuro,
      notreligious,
      Religion,
      threatchr,
      threatmus,
      threathin,
      threatbud,
      threatjew,
      threatath
    )
  ) %>%
  mutate(
    Euro = factor(Euro),
    Rural = factor(Rural),
    Man = factor(Man),
    id = factor(1:nrow(short))
  )

str(short.1)
short.1$highestqual
missmap(short.1)

head(short.1)

short.out <- amelia(
  short.1,
  m = 20,
  noms = c("Man", "Euro", "Religious", "Rural"),
  idvars = "id"
)
#

str(short.out$imputations$imp1)

saveRDS(short.out, "short.out")
#short.out<-readRDS("short.out")
short.out$imputations$imp20
short.out.SC <- transform.amelia(
  short.out,
  Age.C.decade = scale(age, center = TRUE, scale =
                         FALSE) / 10,
  Man = factor(Man),
  NZeuro = factor(Euro),
  Rural = factor(Rural),
  Religiosity.S = scale(religiosityAll),
  Rightwing.S = scale(rightwing),
  Highestqual.C = scale(highestqual, center =
                          T, scale = F),
  id = factor(id)
)


#saveRDS(short.out.SC,"short.out.SC")
str(short.out.SC$imputations$imp20)
short.amelia.list <- list(
  short.out.SC$imputations$imp1,
  short.out.SC$imputations$imp2,
  short.out.SC$imputations$imp3,
  short.out.SC$imputations$imp4,
  short.out.SC$imputations$imp5,
  short.out.SC$imputations$imp6,
  short.out.SC$imputations$imp7,
  short.out.SC$imputations$imp8,
  short.out.SC$imputations$imp9,
  short.out.SC$imputations$imp10,
  short.out.SC$imputations$imp11,
  short.out.SC$imputations$imp12,
  short.out.SC$imputations$imp13,
  short.out.SC$imputations$imp14,
  short.out.SC$imputations$imp15,
  short.out.SC$imputations$imp16,
  short.out.SC$imputations$imp17,
  short.out.SC$imputations$imp18,
  short.out.SC$imputations$imp19,
  short.out.SC$imputations$imp20
)
head(short.out.SC$imputations$imp1)
str(short.out.SC$imputations$imp2)

#saveRDS(short.amelia.list,"short.amelia.list")
#short.amelia.list<-readRDS("short.amelia.list")


#### BRMS MODELS

library(brms)
#library(brmstools)
library(rstan)
library(ggeffects)
library(sjPlot)

conflicts()

dev.off()

threatchr, threatmus, threathin, threatbud, threatjew, threatath
attitudechr,
attitudemus,
attitudehin,
attitudebud,
attitudejew,

short.out.SC <- transform.amelia(
  short.out,
  Age.C.decade = scale(age, center = TRUE, scale =
                         FALSE) / 10,
  Man = factor(Man),
  NZeuro = factor(Euro),
  Rural = factor(Rural),
  Religiosity.S = scale(religiosityAll),
  Rightwing.S = scale(rightwing),
  Highestqual.C = scale(highestqual, center =
                          T, scale = F),
  id = factor(id)
)


str(combo.scaled)

combo.scaled <- combo %>%
  dplyr::mutate(
    age.c = scale(age, scale = F, center = T),
    age.decade.c = scale(age, scale = T, center = T) / 10,
    gender = factor(gender),
    male = factor(male),
    highestqual  = as.numeric(highestqual),
    highestqual.c = scale(age, scale = T, center = T),
    relaccept.S = scale(religaccept, center = TRUE, scale =
                          T),
    rightwing.S = scale(rightwing, center = TRUE, scale = T),
    religiosityAll.S = scale(religiosityAll)
  )


# Yola

threatchr,
threatmus,
threathin,
threatbud,
threatjew,
threatath,

year0 <- combo.scaled %>%
  filter(year == 0)

yearplus1 <- combo.scaled %>%
  filter(year == 1)

### now for threat
library(tidyLPA)
lpa.threat.1.0 <- year0 %>%
  dplyr::select(threatchr,
                threatmus,
                threathin,
                threatbud,
                threatjew,
                threatath) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(4)
# tidyLPA::estimate_profiles(c(2,3,4,5,6,7,8,9,10))
plot1 <- tidyLPA::plot_profiles(lpa.threat.1.0, add_line = T)

plot1


gotn <- tidyLPA::get_data(lpa.threat.1.0)
year0$Class <- gotn$Class
year0$Class <- gotn$Class
table(year0$Class)
mean(year0$Class == 1) #0.50
mean(year0$Class == 2) #0.05
mean(year0$Class == 3)# 0.34
mean(year0$Class == 4)# 0.11


year0$Class <- factor(
  year0$Class,
  levels = c(1:4),
  labels = c(
    "Low Islamophobia ",
    "Extreme Islamophobia",
    "Moderate Islamophobia",
    "Pro Buddhism/ High Islamophobia"
  )
)


saveRDS(lpa.threat.1.0, "lpa.threat.1.0")


head(year0$Class)

library(table1) # very useful package
table1::table1( ~ Class, data = year0,
                overall = T)




lpa.threat.2.0 <- yearplus1 %>%
  dplyr::select(threatchr,
                threatmus,
                threathin,
                threatbud,
                threatjew,
                threatath) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(4)
# tidyLPA::estimate_profiles(c(2,3,4,5,6,7,8,9,10))
plot2 <- tidyLPA::plot_profiles(lpa.threat.2.0, add_line = T)

plot2
#saveRDS(lpa.threat.2.0,"lpa.threat.2.0")

gotnn <- tidyLPA::get_data(lpa.threat.2.0)
yearplus1$Class <- gotnn$Class
yearplus1$Class <- gotnn$Class

table(yearplus1$Class)
mean(yearplus1$Class == 1) #0.40
mean(yearplus1$Class == 2) #0.45
mean(yearplus1$Class == 3)# 0.07
mean(yearplus1$Class == 4)# 0.08






head(short)
(year0$Class)



plot2



yearplus1$Class <- factor(
  yearplus1$Class,
  levels = c(1:4),
  labels = c(
    "Moderate-High Islamophobia",
    "2018-level low Islamophobia",
    "Very Low Islamophobia",
    "Recalcitrant Extreme Islamophobia"
  )
)



yearplus1$Class
table1::table1( ~ Class, data = yearplus1,
                overall = T)


#
# table1::label(short.0$age) <-"Age"
# table1::label(short.0$rightwing) <-"Rightwing"
# table1::label(short.0$religiosityAll) <-"Religiosity"
# table1::label(short.0$highestqual) <-"Highest Educational Qualification"
# table1::label(short.0$NZeuro) <-"NZ European"
# # table1::label(short.0$Negative.Muslims) <- "Negative to Muslims"
# table1::label(short.0$Negative.Hindus) <- "Negative to Hindus"
# table1::label(short.0$Negative.Buddhists) <- "Negative to Buddhists"
# table1::label(short.0$Negative.Christians) <- "Negative to Christians"
# table1::label(short.0$Negative.Jews) <- "Negative Jews"
# table1::label(short.0$Negative.Atheists) <- "Negative to Atheists"
# table1::label(short.0$Threatened.Muslims) <- "Threatened by Muslims"
# table1::label(short.0$Threatened.Hindus) <- "Threatened by Hindus"
# table1::label(short.0$Threatened.Buddhists) <- "Threatened by Buddhists"
# table1::label(short.0$Threatened.Christians) <- "Threatened by Christians"
# table1::label(short.0$Threatened.Jews) <- "Threatened by Jews"
# table1::label(short.0$Threatened.Atheists) <- "Threatened by Atheists"



##  tables
yearplus1$Class
table1::table1( ~ Class, data = year0,
                overall = T)


table1::table1( ~ Class, data = yearplus1,
                overall = T)




# note that we have more  religious people than not
table1::table1( ~ Religious, data = short.0, overall = T)
table1::table1( ~ factor(religiosityAll), data = short.0, overall = T)# which  agrees with this variable




l1

l2


plot_profiles()
get_estimates(lpa.threat.2.0)

tidyLPA::get_fit(lpa.threat.2.0)



library(patchwork)


plot1 + plot2  + plot_annotation(title = "Latent Profile Analysis of Perceived Threats of Religious Groups",
                                 subtitle = "0 and + 2 SD Religious id")  + plot_layout(nrow =
                                                                                          2, ncol = 1)




nrow(dat)



l1 <-
  plot1 +  theme_bw()  + labs(title = "2018: pre-attacks, n =1335") +
  scale_y_continuous(limits = c(0, 7)) + #theme(plot.title = element_text(size=09))
  theme(axis.text.x = element_text(angle = 45))
l1

l2 <- plot2 +  theme_bw() + labs(title = "2019: post-attacks, n= 325") +
  scale_y_continuous(limits = c(0, 7)) + #theme(plot.title = element_text(size=09)) +
  theme(axis.text.x = element_text(angle = 45))

l1 + l2 +  plot_layout(nrow = 1, ncol = 2) +
  plot_annotation(title = "LPA of Perceived Religious Threats Reveals Persisting Islamophobia in 2019")



#
#
# library(patchwork)
#
# {l1 + l2} +
#   plot1 +plot2 + plot_layout(nrow=2, ncol=2)






## Use five class model
lpa.threat.2 <- short %>%
  dplyr::select(
    Threatened.Christians,
    Threatened.Muslims,
    Threatened.Hindus,
    Threatened.Buddhists,
    Threatened.Jews,
    Threatened.Atheists
  ) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(5) %>%
  tidyLPA::plot_profiles(lpa.threat.2)





#######
##ask to run models on multiple cores

library(brms)
#library(brmstools)
library(rstan)
library(ggeffects)
library(sjPlot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores ())

vignette("brms_families")
vignette("brms_overview")
help("brmsformula")
help("brmsfamily")
#library(lazerhawk)  # goodtables, not used

mod.threat.mus <- brm(threatmus ~ year + (1 | id),
                      data = combo.scaled, chains = 1)

mod.threat.mus <- brm(
  threatmus ~ year * (
    rightwing.S + religiosityAll.S + age.decade.c +
      # nzeuro +
      highestqual.c + male + Rural
  ) +
    (1 | id),
  data = combo.scaled,
  chains = 1
)

library(lme4)
mod.threat.mus <- lmer(
  threatmus ~ year +
    rightwing.S + religiosityAll.S + age.decade.c +
    # nzeuro +
    highestqual.c + male + Rural +
    (1 | id),
  data = combo.scaled
)

mod.threat.mus <- lmer(
  threatmus ~ year *
    (rightwing.S + religiosityAll.S + age.decade.c) +
    # nzeuro +
    highestqual.c +
    male +
    rural +
    (1 | id),
  data = combo.scaled
)


# truct <- unique(subset(dat2, select=c(Aid, birth)))
# tr <- unique(subset(truct, birth<1900))
# dtr <- unique(subset(dat, Aid == "A5544"))
# dtr$birth

# library(car)
# influencePlot(mod.threat.mus,
#               id.method="identify",
#               main="Influence Plot",
#               sub="Circle size is proportial to Cook's Distance" )


plot_model(
  mod.threat.mus,
  #  show.values = T,
  show.ci = .9,
  type = 'est',
  value.offset = .3,
  show.intercept = TRUE,
  sort.est = F
) #+  theme_blank()

tab_model(mod.threat.mus) #+  theme_blank()


plot(ggpredict(mod.threat.mus, intervals = "prediction"))

plot_model(
  mod.threat.mus,
  terms = c("religiosityAll.S", "year"),
  type = "pred",
  grid = T
)

plot_model(
  mod.threat.mus,
  show.values = T,
  show.ci = .9,
  value.offset = .3,
  show.intercept = F,
  sort.est = F
) #+  theme_blank()
tab_model(mod.threat.mus)
plot(conditional_effects(
  mod.threat.mus,
  effects = c("age.decade.c:year"),
  probs = c(0.1, 0.9)
))


summary(dat$birthdeath)



plot_model(
  mod.omni.neg,
  show.values = T,
  show.ci = .9,
  value.offset = .3,
  show.intercept = F,
  sort.est = T
) #+  theme_blank()

get_model_data(mod.omni.neg)# see labels, etc.

tab_model(
  mod.omni.neg,
  show.ci = .9,
  auto.label = F,
  show.re.var = TRUE
)




mod.omni.neg <- brm(
  mvbind(
    attitudemus,
    attitudehin,
    attitudebud,
    attitudejew,
    attitudeath,
    attitudechr
  ) ~ year +
    rightwing.S + religiosityAll.S + age.decade.c +
    # nzeuro +
    highestqual.c + male + Rural + (1 | year),
  data = combo.scaled,
  chains = 1
)

(dat2$birth)


#mod.omni.neg <- brm_multiple(
# mvbind(Negative.Muslims, Negative.Hindus,Negative.Buddhists,Negative.Jews,Negative.Atheists,Negative.Christians) ~ Rightwing.S + Religiosity.S + Age.C.decade + NZeuro +Highestqual.C + Man + Rural, data=short.amelia.list, chains=1)


# Save model
#saveRDS(mod.omni.thr,"mod.omni.thr")
#mod.omni.neg<- readRDS("mod.omni.neg")

### Regression plot, used
plot_model(
  mod.omni.neg,
  show.values = T,
  show.ci = .9,
  value.offset = .3,
  show.intercept = TRUE,
  sort.est = T
) #+  theme_blank()
plot_model(
  mod.omni.neg,
  show.values = T,
  show.ci = .9,
  value.offset = .3,
  show.intercept = F,
  sort.est = T
) #+  theme_blank()


get_model_data(mod.omni.neg)# see labels, etc.

tab_model(
  mod.omni.neg,
  show.ci = .9,
  auto.label = F,
  show.re.var = TRUE
)



library(ggthemes)
library(purrr)
library(ggeffects)


# not run
#
# p0<-plot(marginal_effects(mod.omni.neg, effects = "NZeuro", response="Negative.Muslims",probs = c(0.1, 0.9)) )
# a1<-p0$NegativeMuslims.NegativeMuslims_NZeuro
# a2<-p0$NegativeHindus.NegativeHindus_NZeuro
# library(gridExtra)
# gridExtra::grid.arrange(a1,a2)


#  Not used
d1 <-
  plot(
    marginal_effects(
      mod.omni.neg,
      effects = "Highestqual.C",
      resp = "NegativeMuslims",
      probs = c(0.1, 0.9)
    )
  )
d1
#
d2 <-
  plot(
    marginal_effects(
      mod.omni.neg,
      effects = "Religiosity.S",
      resp = "NegativeMuslims",
      probs = c(0.1, 0.9)
    )
  )
d2
#
d3 <-
  plot(
    marginal_effects(
      mod.omni.neg,
      effects = "Rightwing.S",
      resp = "NegativeMuslims",
      probs = c(0.1, 0.9)
    )
  )
#
d4 <-
  plot(
    marginal_effects(
      mod.omni.neg,
      effects = "Age.C.decade",
      resp = "NegativeMuslims",
      probs = c(0.1, 0.9)
    )
  )
plot_grid(c(d1, d2, d3, d4))





#
### PANEL PLOTS USE THESE
library(sjmisc)
library(sjPlot)
library(brms)

theme_set(ggthemes::theme_stata())
library(ggthemes)
library(tidyverse)
library(ggplot2)

p1 <- purrr::map(
  c("Age.C.decade"),
  ~  ggeffects::ggpredict(mod.omni.neg, .x) %>% plot()  + ylab("Negativity") + xlab("Age in Decades") +  ggtitle("Predicted Effects of Age on Negativity")
)

p1
warnings()

warnings()
p2 <- purrr::map(
  c("Man"),
  ~  ggeffects::ggpredict(mod.omni.neg, .x) %>% plot() + theme_tufte() +
    #+ theme_stata() +
    ylab("Negativity") + xlab("Male")
  +  ggtitle("Predicted Effects of Male Gender on Negativity")
)


p3 <- purrr::map(
  c("Highestqual.C"),
  ~  ggeffects::ggpredict(mod.omni.neg, .x) %>% plot() + theme_tufte() +
    #+ theme_stata() +
    ylab("Negativity") + xlab("Hightest Qualification")
  +  ggtitle("Predicted Effects of Education on Negativity")
)



p4 <- purrr::map(
  c("Rightwing.S"),
  ~  ggeffects::ggpredict(mod.omni.neg, .x) %>% plot() + ylab("Negativity") + xlab("Rightwing Political Orientation ")
  +  ggtitle("Predicted Effects of Rightwing Orientation on Negativity")
)


p5 <- purrr::map(
  c("Religiosity.S"),
  ~  ggeffects::ggpredict(mod.omni.neg, .x) %>% plot() +
    #+ theme_stata() +
    ylab("Negativity") + xlab("Religious identification")
  +  ggtitle("Predicted Effects of Religious identification on Negativity")
)


p6 <- purrr::map(
  c("NZeuro"),
  ~  ggeffects::ggpredict(mod.omni.neg, .x) %>% plot()  +
    #+ theme_stata() +
    ylab("Negativity") + xlab("NZeuro") +  ggtitle("Predicted Effects of NZ European Ethnicity on Negativity")
)

p7 <- purrr::map(
  c("Rural"),
  ~  ggeffects::ggpredict(mod.omni.neg, .x) %>% plot() +
    #+ theme_stata() +
    ylab("Negativity") + xlab("Rural") +  ggtitle("Predicted Effects of Rural Residence on Negativity")
)

dev.of

plot_grid(c(p1, p3, p4, p5, p2, p6, p7))
plot_grid(c(p1, p3, p4, p5)) # smaller version



# not run
plot(
  marginal_effects(mod.omni.neg),
  effects = "Rightwing.S",
  resp = "NegativeMuslims",
  theme = theme_stata()
)
#
# plot(marginal_effects(mod.omni.neg),effects = "Rightwing.S",
#      resp="NegativeMuslims", theme=theme_stata())

# plot_grid(p)

# grid.arrange(p1,p2,p3,p4)  #not working
# #

#saveRDS(mod.omni.neg,"mod.omni.neg")
#mod.omni.neg<-readRDS("mod.omni.neg")


# for the table.
sum.neg <- summary(mod.omni.neg)
str(sum.neg)
head(sum.neg$pop)
fe.neg <- (sum.neg$fixed)
rescor.neg <- (sum.neg$rescor_pars)
fe.neg <- round(fe.neg, 2)
rescor.neg <- round(rescor.neg, 2)

library(stargazer)
stargazer(rescor.neg, type = "html", digits = 2)
stargazer(fe.neg, type = "html", digits = 2)




# not correct order !!!  do this manually
tab_model(
  mod.omni.neg,
  title = "x",
  linebreak = T,
  auto.label = F,
  dv.labels = list(
    c(
      "Negativity.Muslims",
      "Negativity.Hindus",
      "Negativity.Buddhists",
      "Negativity.Jews",
      "Negativity.Atheists",
      "Negative.Christians"
    )
  )
)



brms::stanplot(
  mod.omni.neg,
  type = "intervals",
  prob = .5,
  prob_outer = .95,
  point_est = "median",
  pars = "b_NegativeMuslims"
) +
  labs(title    = "",
       subtitle = "")
+ ggplot2::ylim(1.9, 3.2))


brms::stanplot(
  mod.omni.neg,
  type = "intervals",
  prob = .5,
  prob_outer = .95,
  point_est = "median",
  pars = "_Intercept"
) +
  labs(title    = "",
       subtitle = "")


brms::stanplot(
  mod.omni.neg,
  type = "intervals",
  prob = .5,
  prob_outer = .90,
  point_est = "median",
  pars = "rescor_"
) +
  labs(title    = "Residual Correlations of Negativity to Target Groups",
       subtitle = "")



#

library(ggplot2)

d1 <-
  plot(
    marginal_effects(
      mod.omni.neg,
      effects = "Highestqual.C",
      resp = "NegativeMuslims",
      probs = c(0.1, 0.9)
    )
  )


dd1 <- plot(d1[[1]] + ggplot2::ylim(1.9, 3.2))


d2 <-
  plot(
    marginal_effects(
      mod.omni.neg,
      effects = "Religiosity.S",
      resp = "NegativeMuslims",
      probs = c(0.1, 0.9)
    )
  )


dd2 <- plot(d2[[1]] + ggplot2::ylim(1.9, 3.2))


#
d3 <-
  plot(
    marginal_effects(
      mod.omni.neg,
      effects = "Rightwing.S",
      resp = "NegativeMuslims",
      probs = c(0.1, 0.9)
    )
  )

dd3 <- plot(d3[[1]] + ggplot2::ylim(1.9, 3.2))

#
d4 <-
  plot(
    marginal_effects(
      mod.omni.neg,
      effects = "Age.C.decade",
      resp = "NegativeMuslims",
      probs = c(0.1, 0.9)
    )
  )

dd4 <- plot(d4[[1]] + ggplot2::ylim(1.9, 3.2))


d5 <-
  plot(marginal_effects(
    mod.omni.neg,
    effects = "Man",
    resp = "NegativeMuslims",
    probs = c(0.1, 0.9)
  ))

dd5 <- plot(d5[[1]] + ggplot2::ylim(1.9, 3.2))

d6 <-
  plot(marginal_effects(
    mod.omni.neg,
    effects = "Rural",
    resp = "NegativeMuslims",
    probs = c(0.1, 0.9)
  ))

dd6 <- plot(d6[[1]] + ggplot2::ylim(1.9, 3.2))


str(dd4)
str(d4)

grid.arrange(dd1, dd2, dd3, dd4)










#####################################
# THREAT

mod.omni.thr <- brm_multiple(
  mvbind(
    Threatened.Muslims,
    Threatened.Hindus,
    Threatened.Buddhists,
    Threatened.Jews,
    Threatened.Atheists,
    Threatened.Christians
  ) ~ Rightwing.S + Religiosity.S + Age.C.decade + NZeuro + Highestqual.C + Man + Rural,
  data = short.amelia.list,
  chains = 1
)

#saveRDS(mod.omni.thr,"mod.omni.thr")
mod.omni.thr <- readRDS("mod.omni.thr")


# plot model
plot_model(
  mod.omni.thr,
  show.values = T,
  show.ci = .9,
  value.offset = .3,
  show.intercept = TRUE,
  sort.est = T
) #+  theme_blank()

plot_model(
  mod.omni.thr,
  show.values = T,
  show.ci = .9,
  value.offset = .3,
  show.intercept = F,
  sort.est = T
) #+  theme_blank()



dev.off()

### USE PLOT
plot_model(
  mod.omni.thr,
  show.values = T,
  show.ci = .9,
  value.offset = .3,
  Title = "Perceived Threat of Religious Groups",
  show.intercept = TRUE,
  sort.est = T
) +  theme_blank()


# Manually put in right labels!
tab_model(
  mod.omni.thr,
  show.ci = .9,
  auto.label = F,
  show.re.var = TRUE
)


## Try this
d1 <-
  plot(
    marginal_effects(
      mod.omni.thr,
      effects = "Highestqual.C",
      resp = "ThreatenedMuslims",
      probs = c(0.1, 0.9)
    )
  )



dd1 <- plot(d1[[1]] + ggplot2::ylim(1.4, 2.6))


#

library(ggplot2)
d2 <-
  plot(
    marginal_effects(
      mod.omni.thr,
      effects = "Religiosity.S",
      resp = "ThreatenedMuslims",
      probs = c(0.1, 0.9)
    )
  )


dd2 <- plot(d2[[1]] + ggplot2::ylim(1.4, 2.6))


#
d3 <-
  plot(
    marginal_effects(
      mod.omni.thr,
      effects = "Rightwing.S",
      resp = "ThreatenedMuslims",
      probs = c(0.1, 0.9)
    )
  )

dd3 <- plot(d3[[1]] + ggplot2::ylim(1.4, 2.6))

#
d4 <-
  plot(
    marginal_effects(
      mod.omni.thr,
      effects = "Age.C.decade",
      resp = "ThreatenedMuslims",
      probs = c(0.1, 0.9)
    )
  )

dd4 <- plot(d4[[1]] + ggplot2::ylim(1.4, 2.6))


d5 <-
  plot(marginal_effects(
    mod.omni.thr,
    effects = "Man",
    resp = "ThreatenedMuslims",
    probs = c(0.1, 0.9)
  ))

dd5 <- plot(d5[[1]] + ggplot2::ylim(1.4, 2.6))

d6 <-
  plot(
    marginal_effects(
      mod.omni.thr,
      effects = "Rural",
      resp = "ThreatenedMuslims",
      probs = c(0.1, 0.9)
    )
  )

dd6 <- plot(d6[[1]] + ggplot2::ylim(1.4, 2.6))


str(dd4)
str(d4)

grid.arrange(dd1, dd2, dd3, dd4)



# not used
# plot(marginal_effects(mod.omni.thr, effects = "Highestqual.C", resp="ThreatenedMuslims",probs = c(0.1, 0.9)))
# plot(marginal_effects(mod.omni.thr, effects = "Religiosity.S", resp="ThreatenedMuslims",probs = c(0.1, 0.9)))
# plot(marginal_effects(mod.omni.thr, effects = "Rightwing.S", resp="ThreatenedMuslims",probs = c(0.1, 0.9)))
# plot(marginal_effects(mod.omni.thr, effects = "Age.C.decade", resp="ThreatenedMuslims",probs = c(0.1, 0.9)))

sum.thr <- summary(mod.omni.thr, show.ci = 0.9)
str(sum.thr)

fe.thr <- (sum.thr$fixed)
fe.thr <- round(fe.thr, 2)
rescor.thr <- (sum.thr$rescor_pars)
rescor.thr <- round(rescor.thr, 2)
rescor.thr
library(stargazer)
stargazer(rescor.thr, type = "html", digits = 2)
stargazer(fe.thr, type = "html", digits = 2)



(mod.omni.thr$rhats) # assuring
=

  ### PANEL PLOTS USE THESE
  #theme_set(ggthemes::theme_stata())
  p1 <- purrr::map(
    c("Age.C.decade"),
    ~  ggeffects::ggpredict(mod.omni.thr, .x) %>% plot() + theme_tufte() + ylab("Perceived Threat") + xlab("Age in Decades") +  ggtitle("Predicted Effects of Age on Perceived Threat")
  )




p2 <- purrr::map(
  c("Man"),
  ~  ggeffects::ggpredict(mod.omni.thr, .x) %>% plot() + theme_tufte() +
    #+ theme_stata() +
    ylab("Perceived Threat") + xlab("Male")
  +  ggtitle("Predicted Effects of Male Gender on Perceived Threat")
)

p2

p3 <- purrr::map(
  c("Highestqual.C"),
  ~  ggeffects::ggpredict(mod.omni.thr, .x) %>% plot() + theme_tufte() +
    #+ theme_stata() +
    ylab("Perceived Threat") + xlab("Hightest Qualification")
  +  ggtitle("Predicted Effects of Education on Perceived Threat")
)



p4 <- purrr::map(
  c("Rightwing.S"),
  ~  ggeffects::ggpredict(mod.omni.thr, .x) %>% plot() + theme_tufte() + ylab("Perceived Threat") + xlab("Rightwing Political Orientation ")
  +  ggtitle("Predicted Effects of Rightwing Orientation on Perceived Threat")
)


p5 <- purrr::map(
  c("Religiosity.S"),
  ~  ggeffects::ggpredict(mod.omni.thr, .x) %>% plot() + theme_tufte() +
    #+ theme_stata() +
    ylab("Perceived Threat") + xlab("Religious identification")
  +  ggtitle(
    "Predicted Effects of Religious identification on Perceived Threat"
  )
)


p6 <- purrr::map(
  c("NZeuro"),
  ~  ggeffects::ggpredict(mod.omni.thr, .x) %>% plot() + theme_tufte() +
    #+ theme_stata() +
    ylab("Perceived Threat") + xlab("NZeuro") +  ggtitle("Predicted Effects of NZ European Ethnicity on Perceived Threat")
)

p7 <- purrr::map(
  c("Rural"),
  ~  ggeffects::ggpredict(mod.omni.thr, .x) %>% plot() + theme_tufte() +
    #+ theme_stata() +
    ylab("Perceived Threat") + xlab("Rural") +  ggtitle("Predicted Effects of Rural Residence on Perceived Threat")
)
dev.off()

plot_grid(c(p1, p3, p4, p5, p2, p6, p7))
plot_grid(c(p1, p3, p4, p5))

##### ANOTHER ANGLE



library(sjstats)
st <- summary(mod.omni.thr)
st
library(stargazer)
stargazer(st, title = "Results",  type = "text")


brms::stanplot(
  mod.omni.thr,
  type = "intervals",
  prob = .5,
  prob_outer = .90,
  point_est = "median",
  pars = "rescor_"
) +
  labs(title    = "Residual Correlations of Threat Targets",
       subtitle = "")


brms::stanplot(
  mod.omni.thr,
  type = "intervals",
  prob = .5,
  prob_outer = .9,
  point_est = "median",
  pars = "^b_"
) +
  labs(title    = "Coefficient plot: XYZ",
       subtitle = "")

brms::stanplot(
  mod.omni.thr,
  type = "intervals",
  prob = .5,
  prob_outer = .9,
  point_est = "median",
  pars = "b_ThreatenedMuslims"
) +
  labs(title    = "Coefficients for Perceived Threat to Muslims",
       subtitle = "")

brms::stanplot(
  mod.omni.thr,
  type = "intervals",
  prob = .5,
  prob_outer = .9,
  point_est = "median",
  pars = "_Intercept"
) +
  labs(title    = "Coefficients for Intercepts of Perceived Threat For Religious Groups",
       subtitle = "") #+ ylab("label")









#
# noprior<- brm_multiple(Negative.Muslims ~
#                          Rightwing.S +
#                          Religiosity.S +
#                          Age.C.decade +
#                          NZeuro +
#                          Highestqual.C +
#                          Man +
#                          Rural,
#                        family = gaussian, data=short.amelia.list, chains=1)# warmup = 10000, iter = 10^6, thin=10)sample_prior = TRUE)inits=0,
# summary(noprior)
# summary()
# #saveRDS(mod.ConSelf,"mod.ConSelf")
# #mod.ConSelf<- readRDS("mod.ConSelf")
# b0<-brms::stanplot(noprior,
#                    type = "intervals",
#                    prob = .5,
#                    prob_outer = .95,
#                    point_est = "median",
#                    pars = "^b_") +
#   labs(title    = "Coefficient plot: XYZ",
#        subtitle = "")
# b1
# b0
# summary(noprior)
#





### FINAL ANALYSISR
library("tidyLPA")
library("tidyverse")
library("dplyr")
head(short)
lpa.neg <- short %>%
  dplyr::select(
    Negative.Christians,
    Negative.Muslims,
    Negative.Hindus,
    Negative.Buddhists,
    Negative.Jews,
    Negative.Atheists
  ) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(c(2, 3, 4, 5, 6, 7, 8, 9, 10))


## Select classes on entropy statistic
tidyLPA::get_fit(lpa.neg)

# A tibble: 9 x 18
# Model Classes LogLik    AIC    AWE    BIC   CAIC    CLC    KIC  SABIC     ICL Entropy prob_min prob_max   n_min n_max BLRT_val
# <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>    <dbl>    <dbl>   <dbl> <dbl>    <dbl>
#   1     1       2 -9156. 18349. 18640. 18448. 18467. 18313. 18371. 18388. -18504.   0.939    0.972    0.989 0.367   0.633   3639.
# 2     1       3 -8674. 17400. 17798. 17535. 17561. 17350. 17429. 17452. -17620.   0.937    0.952    0.985 0.145   0.569    963.
# 3     1       4 -7853. 15772. 16278. 15943. 15976. 15708. 15808. 15838. -15962.   0.989    0.991    0.997 0.0846  0.496   1642.
# 4     1       5 -7211. 14503. 15117. 14711. 14751. 14425. 14546. 14584. -14716.   0.997    0.996    1.000 0.0270  0.487   1283.
# 5     1       6 -7089. 14273. 14994. 14517. 14564. 14181. 14323. 14368. -14563.   0.983    0.862    1.000 0.0270  0.486    244.
# 6     1       7 -7015. 14138. 14968. 14419. 14473. 14032. 14195. 14247. -14530.   0.957    0.827    1.000 0.0270  0.412    149.
# 7     1       8 -6839. 13801. 14738. 14118. 14179. 13681. 13865. 13924. -14229.   0.960    0.837    1.000 0.0127  0.418    352.
# 8     1       9 -6830. 13796. 14841. 14150. 14218. 13662. 13867. 13934. -14209.   0.976    0.906    1.000 0.00899 0.437     18.4
# 9     1      10 -6692. 13534. 14687. 13924. 13999. 13386. 13612. 13686. -14073.   0.953    0.763    1.000 0.0135  0.383    276.


# use four
lpa.neg.1 <- short %>%
  dplyr::select(
    Negative.Christians,
    Negative.Muslims,
    Negative.Hindus,
    Negative.Buddhists,
    Negative.Jews,
    Negative.Atheists
  ) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(4)


lpa.neg.1 <- short %>%
  dplyr::select(
    Negative.Christians,
    Negative.Muslims,
    Negative.Hindus,
    Negative.Buddhists,
    Negative.Jews,
    Negative.Atheists
  ) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(4)

# Graph
tidyLPA::plot_profiles(lpa.neg.1)

tidyLPA::get_fit(lpa.neg.1)

shortR <- short
gotn <- tidyLPA::get_data(lpa.neg.1)


shortR$Class <- gotn$Class
table(shortR$Class)
mean(shortR$Class == 1) #0.48
mean(shortR$Class == 2) #0.26
mean(shortR$Class == 3)# 0.16
mean(shortR$Class == 4)# 0.07




### now for threat
library(tidyLPA)

lpa.threat.2.0 <- short %>%
  dplyr::select(
    Threatened.Christians,
    Threatened.Muslims,
    Threatened.Hindus,
    Threatened.Buddhists,
    Threatened.Jews,
    Threatened.Atheists
  ) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(c(2, 3, 4, 5, 6, 7, 8, 9, 10))


plot_profiles()
get_estimates(lpa.threat.2.0)

tidyLPA::get_fit(lpa.threat.2.0)

## Use five class model
lpa.threat.2 <- short %>%
  dplyr::select(
    Threatened.Christians,
    Threatened.Muslims,
    Threatened.Hindus,
    Threatened.Buddhists,
    Threatened.Jews,
    Threatened.Atheists
  ) %>%
  tidyLPA::single_imputation() %>%
  tidyLPA::estimate_profiles(5)


# should prob not repeat object names, but in a

shortR <- short
got <- get_data(lpa.threat.2)
got
shortR$ClassT <- got$Class
table(shortR$ClassT)

str(shortR)

###
gotn <- tidyLPA::get_data(lpa.threat.2)
shortR$Class <- gotn$Class
shortR$Class <- gotn$Class
table(shortR$Class)
mean(shortR$Class == 1) #0.420
mean(shortR$Class == 2) #0.339
mean(shortR$Class == 3)# 0.110
mean(shortR$Class == 4)# 0.049
mean(shortR$Class == 5)# 0.080

plot_profiles(lpa.threat.2)
plot_density(lpa.threat.2)
get_data(lpa.threat.2)



### another approache
library(Rmixmod)  #NOT WORKING
lpa.neg.1 <- short %>%
  dplyr::select(
    Negative.Christians,
    Negative.Muslims,
    Negative.Hindus,
    Negative.Buddhists,
    Negative.Jews
  )
lpa.neg.1

lpa.neg.2 <- lpa.neg.1 %>%
  filter(
    complete.cases(
      Negative.Christians,
      Negative.Muslims,
      Negative.Hindus,
      Negative.Buddhists,
      Negative.Jews,
      Negative.Atheists
    )
  )
nrow(lpa.neg.2)
lpa.neg.2

out_lpaRmix <- mixmodCluster(lpa.neg.2, nbCluster = 2)
out_lpaRmix
plot(out_lpaRmix)
barplot(out_lpaRmix)
## BELOW, NOT USED

#
#
# rm(modList)
#
# # example prior  # no difference with priors  but might speed things up
#
# prior1 <- c(set_prior("normal(-2,2)", class = "b", coef = "Rightwing.S"),
#             set_prior("normal(-2,2)", class = "b", coef = "Religiosity.S"),
#             set_prior("normal(-2,2)", class = "b", coef = "Age.C.decade"),
#             set_prior("normal(-2,2)", class = "b", coef = "NZeuro1"),
#             set_prior("normal(-2,2)", class = "b", coef = "Man1"),
#             set_prior("normal(-2,2)", class = "b", coef = "Highestqual.C"),
#             set_prior("normal(-2,2)", class = "b", coef = "Rural1"))
# # #
# #
# prg<-get_prior(Negative.Muslims ~
#                  Rightwing.S +
#                  Religiosity.S +
#                  Age.C.decade +
#                  NZeuro +
#                  Highestqual.C +
#                  Man +
#                  Rural,
#                family = gaussian, data=short.amelia.list)
# #
# prg
#
#
# system.time(bmod.1 <- brm_multiple(Negative.Muslims ~
#                                      Rightwing.S +
#                                      Religiosity.S +
#                                      Age.C.decade +
#                                      NZeuro +
#                                      Highestqual.C +
#                                      Man +
#                                      Rural,
#                                    prior = prior1,
#                                    # warmup=3000,
#                                    # iter= 10^6,
#                                    # thin =5,
#                                    family = gaussian, data=short.amelia.list, chains=1))
#
# summary(bmod.1)
# #saveRDS(bmod.1,"bmod.1")
#
#
#
# system.time(bmod.2 <- brm_multiple(Negative.Hindus ~
#                                      Rightwing.S +
#                                      Religiosity.S +
#                                      Age.C.decade +
#                                      NZeuro +
#                                      Highestqual.C +
#                                      Man +
#                                      Rural,
#                                    prior = prior1,
#                                    # warmup=3000,
#                                    # iter= 10^6,
#                                    # thin =5,
#                                    family = gaussian, data=short.amelia.list, chains=1))
#
# summary(bmod.2)
# saveRDS(bmod.2,"bmod.2")
#
# system.time(bmod.3 <- brm_multiple(Negative.Buddhists ~
#                                      Rightwing.S +
#                                      Religiosity.S +
#                                      Age.C.decade +
#                                      NZeuro +
#                                      Highestqual.C +
#                                      Man +
#                                      Rural,
#                                    prior = prior1,
#                                    warmup=3000,
#                                    iter= 10^6,
#                                    thin =5,
#                                    family = gaussian, data=short.amelia.list, chains=1))
#
# summary(bmod.3)
# saveRDS(bmod.3,"bmod.3")
#
# system.time(bmod.4 <- brm_multiple(Negative.Jews ~
#                                      Rightwing.S +
#                                      Religiosity.S +
#                                      Age.C.decade +
#                                      NZeuro +
#                                      Highestqual.C +
#                                      Man +
#                                      Rural,
#                                    prior = prior1,
#                                    warmup=3000,
#                                    iter= 10^6,
#                                    thin =5,
#                                    family = gaussian, data=short.amelia.list, chains=1))
#
# summary(bmod.4)
# saveRDS(bmod.4,"bmod.4")
#
#
#
# system.time(bmod.5 <- brm_multiple( Negative.Atheists  ~
#                                       Rightwing.S +
#                                       Religiosity.S +
#                                       Age.C.decade +
#                                       NZeuro +
#                                       Highestqual.C +
#                                       Man +
#                                       Rural,
#                                     prior = prior1,
#                                     warmup=3000,
#                                     iter= 10^6,
#                                     thin =5,
#                                     family = gaussian, data=short.amelia.list, chains=1))
#
# summary(bmod.5)
# saveRDS(bmod.5,"bmod.5")
#
#
# system.time(bmod.6 <- brm_multiple(Negative.Christians ~
#                                      Rightwing.S +
#                                      Religiosity.S +
#                                      Age.C.decade +
#                                      NZeuro +
#                                      Highestqual.C +
#                                      Man +
#                                      Rural,
#                                    prior = prior1,
#                                    warmup=3000,
#                                    iter= 10^6,
#                                    thin =5,
#                                    family = gaussian, data=short.amelia.list, chains=1),
#
#             summary(bmod.6)
#             saveRDS(bmod.6,"bmod.6")
#
#             Negative.Muslims,Negative.Hindus,Negative.Buddhists,Negative.Jews,Negative.Atheists,Negative.Christians
#
#
#             ######################################
#             system.time(bmod. <- brm_multiple(Threatened.Muslims ~  +
#                                                 Rightwing.S +
#                                                 Religiosity.S +
#                                                 Age.C.decade +
#                                                 NZeuro +
#                                                 Highestqual.C +
#                                                 Man +
#                                                 Rural,
#                                               prior = prior1,
#                                               warmup=3000,
#                                               iter= 10^6,
#                                               thin =5,
#                                               family = gaussian, data=short.amelia.list, chains=1))
#
#             summary(bmod.)
#             saveRDS(bmod.,"bmod.")
#
#
#             system.time(bmod. <- brm_multiple(Threatened.Hindus ~
#                                                 Rightwing.S +
#                                                 Religiosity.S +
#                                                 Age.C.decade +
#                                                 NZeuro +
#                                                 Highestqual.C +
#                                                 Man +
#                                                 Rural,
#                                               prior = prior1,
#                                               warmup=3000,
#                                               iter= 10^6,
#                                               thin =5,
#                                               family = gaussian, data=short.amelia.list, chains=1))
#
#             summary(bmod.)
#             saveRDS(bmod.,"bmod.")
#
#             system.time(bmod. <- brm_multiple( Threatened.Buddhists ~
#                                                  Rightwing.S +
#                                                  Religiosity.S +
#                                                  Age.C.decade +
#                                                  NZeuro +
#                                                  Highestqual.C +
#                                                  Man +
#                                                  Rural,
#                                                prior = prior1,
#                                                warmup=3000,
#                                                iter= 10^6,
#                                                thin =5,
#                                                family = gaussian, data=short.amelia.list, chains=1))
#
#             summary(bmod.)
#             saveRDS(bmod.,"bmod.")
#
#
#             system.time(bmod. <- brm_multiple( Threatened.Jews ~
#                                                  Rightwing.S +
#                                                  Religiosity.S +
#                                                  Age.C.decade +
#                                                  NZeuro +
#                                                  Highestqual.C +
#                                                  Man +
#                                                  Rural,
#                                                prior = prior1,
#                                                warmup=3000,
#                                                iter= 10^6,
#                                                thin =5,
#                                                family = gaussian, data=short.amelia.list, chains=1))
#
#             summary(bmod.)
#             saveRDS(bmod.,"bmod.")
#
#
#             system.time(bmod. <- brm_multiple( Threatened.Atheists ~
#                                                  Rightwing.S +
#                                                  Religiosity.S +
#                                                  Age.C.decade +
#                                                  NZeuro +
#                                                  Highestqual.C +
#                                                  Man +
#                                                  Rural,
#                                                prior = prior1,
#                                                warmup=3000,
#                                                iter= 10^6,
#                                                thin =5,
#                                                family = gaussian, data=short.amelia.list, chains=1))
#
#             summary(bmod.)
#             saveRDS(bmod.,"bmod.")
#
#             system.time(bmod. <- brm_multiple(Threatened.Jews ~
#                                                 Rightwing.S +
#                                                 Religiosity.S +
#                                                 Age.C.decade +
#                                                 NZeuro +
#                                                 Highestqual.C +
#                                                 Man +
#                                                 Rural,
#                                               prior = prior1,
#                                               warmup=3000,
#                                               iter= 10^6,
#                                               thin =5,
#                                               family = gaussian, data=short.amelia.list, chains=1))
#
#
#             system.time(bmod. <- brm_multiple(Threatened.Christians ~
#                                                 Rightwing.S +
#                                                 Religiosity.S +
#                                                 Age.C.decade +
#                                                 NZeuro +
#                                                 Highestqual.C +
#                                                 Man +
#                                                 Rural,
#                                               prior = prior1,
#                                               warmup=3000,
#                                               iter= 10^6,
#                                               thin =5,
#                                               family = gaussian, data=short.amelia.list, chains=1))
#
#             summary(bmod.)
#             saveRDS(bmod.,"bmod.")
#


#
# #### JUST FOR ME  ####
#
# library(table1)
# %
# table1::table1(~sciencefaith|Religious, data = short.scaled,
#                overall =T)
#
# %
#
#
# lpa.science<-short%>%
#   dplyr::select(sciencefaith)%>%
#   tidyLPA::single_imputation() %>%
#   tidyLPA::estimate_profiles(4)
#
#
# tidyLPA::plot_profiles(lpa.science)
#
# tidyLPA::get_fit(lpa.science)
#
# shortR<- short
# gotn<-tidyLPA::get_data(lpa.science)
# shortR<- short
# shortR$ClassT<-gotn$Class
# table(shortR$ClassT)
# mean(shortR$ClassT == 1)#
# mean(shortR$ClassT == 2) #
# mean(shortR$ClassT == 3) #
# mean(shortR$ClassT == 4) #
#
#
#
#
#
# ### just for me
# mod.0 <- glm(sciencefaith ~
#                rightwing.S +
#                religiosityAll.S +
#                age.decade.c +
#                NZ_euro +
#                highestqual.C +
#                Man +
#                Rural,
#              data =short.scaled)
#
#
#
# tab_model(mod.0,show.r2 = F,
#           auto.label = FALSE,
#           pred.labels =c("Intercept","Rightwing","Religiousity (Scaled)","Age in Decades", "NZ European", "Highest Edu (Centered)", "Male", "Rural"))
#
# library(table1)
#
# ## We trust too much in
# #science and not enough in religious faith
# # 1 = strongly agree, 4 = strongly disagree
# plot_model(mod.0, axis.labels = c("Rightwing","Religiousity (Scaled)","Age in Decades", "NZ European", "Highest Edu (Centered)", "Male", "Rural"), show.intercept = T)
