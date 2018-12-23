library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(scales)

survey <- read_csv("kagglesurvey.csv")

str(survey)


character_vars <- lapply(survey, class) == "character"
survey[, character_vars] <- lapply(survey[, character_vars], as.factor)

colnames(survey)[2] <- "Gender"
colnames(survey)[4] <- "Age"
colnames(survey)[5] <- "Country"
colnames(survey)[6] <- "deglev"
colnames(survey)[7] <- "major"
colnames(survey)[8] <- "Job"
colnames(survey)[10] <- "industry"
colnames(survey)[13] <- "wage"
colnames(survey)[12] <- "exp"
colnames(survey)[14] <- "mach"
colnames(survey)[87] <- "lang"
colnames(survey)[127] <- "code"
colnames(survey)[130] <- "datasci"
colnames(survey)[278] <- "gather"
colnames(survey)[279] <- "clean"
colnames(survey)[280] <- "visual"
colnames(survey)[281] <- "model"
colnames(survey)[282] <- "prod"
colnames(survey)[283] <- "coms"
colnames(survey)[285] <- "SelfTaught"
colnames(survey)[286] <- "Online"
colnames(survey)[287] <- "Work"
colnames(survey)[288] <- "University"
colnames(survey)[289] <- "Kaggle"
colnames(survey)[290] <- "Other"
colnames(survey)[331] <- "onlineL"
colnames(survey)[332] <- "bootcamp"
colnames(survey)[264] <- "datacat"
colnames(survey)[306] <- "learn"
colnames(survey)[334] <- "fair"
colnames(survey)[308] <- "twitter"
colnames(survey)[309] <- "hacker news"
colnames(survey)[310] <- "redditml"
colnames(survey)[311] <- "kaggle"
colnames(survey)[312] <- "fastai"
colnames(survey)[313] <- "siraj"
colnames(survey)[314] <- "datatau"
colnames(survey)[315] <- "linear dig"
colnames(survey)[316] <- "cloudAI"
colnames(survey)[317] <- "five38"
colnames(survey)[318] <- "arxiv"
colnames(survey)[319] <- "journals"
colnames(survey)[320] <- "fastml"
colnames(survey)[321] <- "kdnuggest"
colnames(survey)[322] <- "O'Reilly"
colnames(survey)[323] <- "partialderiv"
colnames(survey)[324] <- "dataskeptic"
colnames(survey)[325] <- "medium"
colnames(survey)[326] <- "towardsdata"
colnames(survey)[327] <- "none"
colnames(survey)[328] <- "Otherarea"

 
sruvey1 <- survey %>% group_by(Gender) %>%
                          summarise(totl = n())

sruvey1

ggplot(sruvey1, aes(x = reorder(Gender, totl), y = totl)) + 
                                        geom_col(fill = "steelblue") + 
                                          coord_flip() + 
                                            labs(x = "Gender Response", y = "Total Respondants", title = "Most Respondants were Male") +
                                              theme(panel.background = element_blank())


survey2 <- survey %>% filter(Gender != "Prefer not to say") %>%
              filter(Gender != "Prefer to self-describe") %>%
                      group_by(Gender, Age) %>%
                        summarise(tot = n()) 

cols1 <- c("18-21" = "#C0392B", "22-24" = "#9B59B6", "25-29" = "#2980B9", "30-34" = "#1ABC9C","35-39" = "#F1C40F", "40-44" = "#E67E22",
           "45-49" = "#BDC3C7", "50-54" = "#E74C3C", "55-59" = "#8E44AD", "60-69" = "#3498DB", "70-79" = "#27AE60", "80+" = "#F39C12")

ggplot(survey2, aes(x = Gender, y= tot, fill = Age)) + 
                      geom_bar(position = "fill", stat = "identity") +
                        coord_flip() +
                          scale_fill_manual(values = cols1) +
                            scale_y_continuous(labels = scales::percent) + 
                              labs(x = "", y = "Respondants %", title = "Females taking the survey are younger") + 
                                theme_light()
                            

survey3 <- survey %>% group_by(Country) %>%
                            summarise(tot = n()) 

survey3

colnames(survey3)[1] <- "region"

world <- map_data("world")

world3 <- filter(world, region != "Antarctica")

wordl2 <- inner_join(world3, survey3, by = "region")

world4 <- ggplot(data = world3, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

elbow_room1 <- world4 + 
  geom_polygon(data = wordl2, aes(fill = tot), color = "white") +
  geom_polygon(color = "black", fill = NA) 

elbow_room1

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

 world6 <- elbow_room1 + ditch_the_axes
 
 world7 <- world6 + scale_fill_viridis(trans = "log10") + labs(title = "Respondants Locations") + 
                                guides(fill = guide_legend(title = "Total")) + 
                                    theme(panel.background = element_blank(), legend.position = c(0.2,0.2)) 
            
 
world7



fem6 <- survey %>% filter(Gender == "Female") %>%
                        group_by(Job) %>%
                          summarise(tot = n()) %>%
                              mutate(per = tot / 4010) %>%
                                mutate(Gender = "Female")

ma7 <- survey %>% filter(Gender == "Male") %>%
                          group_by(Job) %>%
                           summarise(tot = n()) %>%
                             mutate(per = tot / 19430) %>%
                                mutate(Gender = "Male")
  
survey6 <- bind_rows(fem6, ma7)

ggplot(survey6, aes(x = Job, y= per)) + 
  geom_col(fill = "steelblue") +
  facet_wrap(~Gender) +
  coord_flip() +
  scale_fill_manual(values = cols1) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "", y = "Respondants %", title = "Distribution of different jobs") + 
  theme_light()




survey5 <- survey %>% mutate(group = if_else(Job == "Business Analyst", 1, 
                                                             if_else(Job == "Data Analyst", 1, 
                                                                  if_else(Job == "Data Journalist", 1, 
                                                                           if_else(Job == "Data Scientist", 1,0))))) %>%
                                     filter(group == 1) %>%
                                        filter(wage != "I do not wish to disclose my approximate yearly compensation") %>%
                                        mutate(wage2 = if_else(wage == "0-10,000", 10000,
                                                       if_else(wage == "10-20,000", 20000,
                                                       if_else(wage == "20-30,000", 30000,
                                                       if_else(wage == "30-40,000", 40000,
                                                       if_else(wage == "40-50,000", 50000,
                                                       if_else(wage == "50-60,000", 60000,
                                                       if_else(wage == "60-70,000", 70000,
                                                       if_else(wage == "70-80,000", 80000,
                                                       if_else(wage == "80-90,000", 90000,
                                                       if_else(wage == "90-100,000", 100000,
                                                       if_else(wage == "100-125,000", 150000,
                                                       if_else(wage == "125-150,000", 150000,
                                                       if_else(wage == "150-200,000", 200000,
                                                       if_else(wage == "200-250,000", 250000, 
                                                       if_else(wage == "250-300,000", 300000,
                                                       if_else(wage == "300-400,000", 400000,
                                                       if_else(wage == "400-500,000", 500000,
                                                       if_else(wage == "500,000+",    500000, 0))))))))))))))))))) %>%
                                mutate(deglev2 = if_else(deglev == "Some colege/university study without earning a bachelor's degree", "Some Uni", 
                                                 if_else(deglev == "Professional degree", "Professional degree", 
                                                  if_else(deglev == "No formal education past high school", "High School", 
                                                  if_else(deglev == "Master's degree", "Master's degree", 
                                                  if_else(deglev == "Doctoral degree", "Doctoral Degree", 
                                                  if_else(deglev == "Bachelor's degree","Bachelor's degree", "na" ))))))) %>%
                                                        filter(deglev2 != "na")

cols <- c("Bachelor's degree" = "#66c2a5", "Doctoral Degree" = "#fc8d62", "High School" = "#8da0cb", "Master's degree" = "#e78ac3","Professional degree" = "#a6d854")

ggplot(survey5, aes(x = deglev2, y = wage2, fill = deglev2)) + 
                                                  geom_boxplot(alpha = 0.8) +
                                                    scale_fill_manual(values = cols) +
                                                        scale_y_continuous(labels = dollar) +
                                                          labs(x = "Education Level", y = "Wage", title = "Doctoral degree seems to lead to higher wages") +
                                                            guides(fill = FALSE) +
                                                              theme(panel.background = element_blank())

survey7 <- survey5 %>% group_by(deglev2) %>%
                          summarise(tot = n()) %>%
                          arrange(tot) %>%
                           mutate(deglev2 = factor(deglev2, levels = .$deglev2))
survey7

ggplot(survey7, aes(x = reorder(deglev2, tot), y = tot, col = deglev2)) + geom_segment(aes(x = deglev2, y = 0, xend = deglev2, yend = tot), color = "grey50") +
  geom_point(size = 5) + coord_flip() +
  scale_color_manual(values = cols) +
        labs(x = "", y = "Total Respondants") +
                 guides(color = FALSE) +
                   theme(panel.background = element_blank())




survey6 <- survey %>% filter(Country %in% c("India", "USA")) %>%
                         mutate(deglev2 = if_else(deglev == "Some colege/university study without earning a bachelor's degree", "Some Uni", 
                           if_else(deglev == "Professional degree", "Professional degree", 
                                   if_else(deglev == "No formal education past high school", "High School", 
                                           if_else(deglev == "Master's degree", "Master's degree", 
                                                   if_else(deglev == "Doctoral degree", "Doctoral Degree", 
                                                           if_else(deglev == "Bachelor's degree","Bachelor's degree", "na" ))))))) %>%
                                  filter(deglev2 != "na") %>%
                          group_by(Country, deglev2) %>%
                            summarise(tot = n())




survey6



survey8 <- survey %>% group_by(Job) %>%
                          summarise(tot = n()) %>%
                            arrange(desc(tot)) %>%
                              filter(Job != "")


ggplot(survey8, aes(x = reorder(Job, tot), y = tot)) + 
  geom_col(fill = "steelblue") + 
  coord_flip() + 
  labs(x = "Job", y = "Total Respondants", title = "The Most Popular Job is Data scientist") +
  theme(panel.background = element_blank())


survey9 <- survey %>% filter(Job %in% c("Data Scientist", "Data Analyst")) %>%
                        mutate(degreecat = if_else(major == "A business discipline (accounting, economics, finance, etc.)", "Non STEM",
                                            if_else(major == "Computer science (software engineering, etc.)", "STEM",
                                                    if_else(major == "Engineering (non-computer focused)", "STEM", 
                                                    if_else(major == "Environmental science or geology", "STEM", 
                                                     if_else(major == "Fine arts or performing arts", "Non STEM",
                                                      if_else(major == "Humanities (history, literature, philosophy, etc.)", "Non STEM",
                                                       if_else(major == "I never declared a major", "No Major", 
                                                        if_else(major == "Information technology, networking, or system administration", "STEM",
                                                                if_else(major == "Mathematics or statistics", "STEM",
                                                                        if_else(major == "Medical or life sciences (biology, chemistry, medicine, etc.)", "STEM",
                                                                                if_else(major == "Other", "Non STEM",
                                                                                        if_else(major == "Physics or astronomy", "STEM",
                                                                                          if_else(major =="Social sciences (anthropology, psychology, sociology, etc.)", "Non STEM", "na")))))))))))))) %>%
                              filter(degreecat != "na") %>%
                                group_by(Job, degreecat) %>%
                                  summarise(tot = n())

survey9

col4 <- c("No Major" = "#F97D10", "Non STEM" = "#1BB831", "STEM" = "steelblue")

                    
ggplot(survey9, aes(x = Job, y= tot, fill = degreecat)) + 
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = col4) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "", y = "Respondants %", title = "Significantly more data analysts are from NON STEM majors") + 
  theme_light()



survey10 <- survey %>% filter(Job == "Data Scientist") %>%
                          group_by(Job, industry) %>%
                            filter(industry != "NA") %>%
                            summarise(tot = n()) %>%
                              mutate(per = tot/4137)


survey11 <- survey %>% filter(Job == "Data Analyst") %>%
                       group_by(Job, industry) %>%
                        filter(industry != "NA") %>%
                       summarise(tot = n()) %>%
                        mutate(per = tot/1922)

survey12 <- bind_rows(survey10, survey11)


col5 <- c("Data Analyst" = "#1BB831", "Data Scientist" = "steelblue")

ggplot(survey12, aes(x = reorder(industry, per), y = per, col = Job)) + 
  geom_segment(aes(x = industry, y = 0, xend = industry, yend = per), color = "grey50") +
                            geom_point(size = 3) + 
                              scale_color_manual(values = col5) +
                              coord_flip() +
                                  facet_wrap(~Job) +
                                    scale_y_continuous(labels = percent) +
                                        labs(x = "Industry", y = "") +
                                         theme(panel.background = element_blank(), legend.position = c(0.9, 0.9))



survey14 <- survey %>% filter(Job %in% c("Data Scientist", "Data Analyst")) %>%
  filter(wage != "I do not wish to disclose my approximate yearly compensation") %>%
  mutate(wage2 = if_else(wage == "0-10,000", 10000,
                         if_else(wage == "10-20,000", 20000,
                                 if_else(wage == "20-30,000", 30000,
                                         if_else(wage == "30-40,000", 40000,
                                                 if_else(wage == "40-50,000", 50000,
                                                         if_else(wage == "50-60,000", 60000,
                                                                 if_else(wage == "60-70,000", 70000,
                                                                         if_else(wage == "70-80,000", 80000,
                                                                                 if_else(wage == "80-90,000", 90000,
                                                                                         if_else(wage == "90-100,000", 100000,
                                                                                                 if_else(wage == "100-125,000", 150000,
                                                                                                         if_else(wage == "125-150,000", 150000,
                                                                                                                 if_else(wage == "150-200,000", 200000,
                                                                                                                         if_else(wage == "200-250,000", 250000, 
                                                                                                                                 if_else(wage == "250-300,000", 300000,
                                                                                                                                         if_else(wage == "300-400,000", 400000,
                                                                                                                                                 if_else(wage == "400-500,000", 500000,
                                                                                                                                                         if_else(wage == "500,000+",    500000, 0))))))))))))))))))) %>%
  filter(exp != 0)

ggplot(survey14, aes(x = exp, y = wage2, col = Job)) + geom_smooth(method = "lm", se = FALSE) +
                            scale_color_manual(values = col5) +
                              scale_y_continuous(labels = dollar) +
                                labs(x = "Experience (years)", y ="Wage", title = "Data Scientists earn significantly more" ) +
                                    theme(panel.background = element_blank(), legend.position = c(0.2,0.8))


survey15 <- survey %>% filter(Job %in% c("Data Scientist", "Data Analyst")) %>%
                        group_by(Job, mach) %>%
                              summarise(N = n()) %>%
                                  filter(mach != "NA")

ggplot(survey15, aes(x = Job, y = mach, size = N, col = Job)) + 
                            geom_point() +
                              scale_color_manual(values = col5) +
                                labs(y = "", title = "Comparison ML Usage") +
                                    guides(col = F) +
                                  theme(panel.background = element_blank())


survey16 <- survey %>% filter(Job %in% c("Data Scientist", "Data Analyst")) %>%
                          mutate(lang2 = if_else(lang == "R", "R", 
                                                 if_else(lang == "Python", "Python", "Other"))) %>%
                                filter(lang2 != "NA") %>%
                                group_by(Job, lang2) %>%
                                  summarise(n = n())

col6 <- c("R" = "#1BB831", "Python" = "steelblue", "Other" = "grey")

ggplot(survey16, aes(x = Job, y= n, fill = lang2)) + 
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = col6) +
  scale_y_continuous(labels = scales::percent) + 
  guides(fill  = guide_legend(title = "Programing Languge")) +
  labs(x = "", y = "Respondants %", title = "Data Scientists Use Python") + 
  theme_light()     

survey17 <- survey %>% filter(Job == "Data Analyst") %>%
                              group_by(code) %>%
                                    summarise(per = n()/1922) %>%
                                        filter(code != "NA") %>%
                                            mutate(Job = "Data Analyst")


survey18 <- survey %>% filter(Job == "Data Scientist") %>%
  group_by(code) %>%
  summarise(per = n()/4137) %>%
  filter(code != "NA") %>%
  mutate(Job = "Data Scientist")


survey19 <- bind_rows(survey17, survey18)






ggplot(survey19, aes(x =code, y= per, fill = Job)) + geom_col() + 
              facet_wrap(~Job) + 
                coord_flip() +
                  scale_fill_manual(values = col5) +
                    scale_y_continuous(labels = percent) +
                      labs(x = "Coding Time", y = "Percent of Respondants", title = "Data scientists spend more time coding")+
                        theme(panel.background = element_blank(), legend.position = c(0.8, 0.2))
    


survey20 <- survey %>% filter(Job == "Data Analyst") %>%
  group_by(datasci) %>%
  summarise(per = n()/1922) %>%
  filter(datasci != "NA") %>%
  mutate(Job = "Data Analyst")


survey21 <- survey %>% filter(Job == "Data Scientist") %>%
  group_by(datasci) %>%
  summarise(per = n()/4137) %>%
  filter(datasci != "NA") %>%
  mutate(Job = "Data Scientist")


survey22 <- bind_rows(survey20, survey21)



ggplot(survey22, aes(x = Job, y = datasci, size = per, col = Job)) + geom_point() + 
                                scale_size_continuous(labels = percent) +
                                  guides(size = guide_legend(title = "Respondants %"), color = FALSE) +
                                    scale_color_manual(values = col5) +
                                      labs(y = "Do you think you're a Data Scientist?", title = "Data Analyst are undecided if they are Data Scientists") +
                                        theme(panel.background = element_blank(), legend.position = c(0.9, 0.5))

cols8 <- c("Gather Data" = "#C0392B", "Cleaning Data" = "#9B59B6", "Visualising" = "#2980B9", "Building Model" = "#1ABC9C","Producing Model" = "#F1C40F", "Comunicating Insights" = "#E67E22")

survey23 <- survey %>% filter(Job == "Data Analyst") %>%
                          select(Job, gather, clean, visual, model, prod, coms) %>%
                          gather("Stage", "value", -Job) %>%
                            mutate(stage2 = if_else(Stage == "gather", 1, 
                                                   if_else(Stage == "clean", 2, 
                                                           if_else(Stage == "visual", 3, 
                                                                   if_else(Stage == "model", 4,
                                                                           if_else(Stage == "prod", 5,
                                                                                   if_else(Stage == "coms", 6, 0))))))) %>%
                    mutate(stagedes = if_else(Stage == "gather", "Gather Data", 
                          if_else(Stage == "clean", "Cleaning Data", 
                                  if_else(Stage == "visual", "Visualising", 
                                          if_else(Stage == "model", "Building Model",
                                                  if_else(Stage == "prod", "Producing Model",
                                                          if_else(Stage == "coms", "Comunicating Insights", "NA")))))))

ggplot(survey23, aes(x= stage2, y = value, fill = stagedes)) + geom_boxplot() + 
                                  scale_fill_manual(values = cols8) +
                                      labs(x = "", y = "% Time Spent on Each Stage", title = "Data Analyst spend a lot of time cleaning data") +
                                        coord_flip() +
                                        guides(fill = guide_legend(title = "Workflow Stage")) +
                                        theme(panel.background = element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank())
  
  

survey24 <- survey %>% filter(Job == "Data Scientist") %>%
  select(Job, gather, clean, visual, model, prod, coms) %>%
  gather("Stage", "value", -Job) %>%
  mutate(stage2 = if_else(Stage == "gather", 1, 
                          if_else(Stage == "clean", 2, 
                                  if_else(Stage == "visual", 3, 
                                          if_else(Stage == "model", 4,
                                                  if_else(Stage == "prod", 5,
                                                          if_else(Stage == "coms", 6, 0))))))) %>%
  mutate(stagedes = if_else(Stage == "gather", "Gather Data", 
                            if_else(Stage == "clean", "Cleaning Data", 
                                    if_else(Stage == "visual", "Visualising", 
                                            if_else(Stage == "model", "Building Model",
                                                    if_else(Stage == "prod", "Producing Model",
                                                            if_else(Stage == "coms", "Comunicating Insights", "NA")))))))



ggplot(survey24, aes(x= stage2, y = value, fill = stagedes)) + geom_boxplot() + 
  scale_fill_manual(values = cols8) +
  labs(x = "", y = "% Time Spent on Each Stage", title = "Data Scientists spend time producing models") +
  coord_flip() +
  guides(fill = guide_legend(title = "Workflow Stage")) +
  theme(panel.background = element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank())

  

survey25 <- survey %>% filter(Job %in% c("Data Scientist", "Data Analyst")) %>%
                            select(Job, SelfTaught, Online, Work, University, Kaggle, Other) %>%
                              gather("LearnMeth", "value" , -Job) %>%
                                group_by(Job, LearnMeth) %>%
                                  summarise(mean = (mean(value, na.rm= T)/100)) %>%
                                      mutate(learnme = if_else(LearnMeth == "Kaggle", "Kaggle", 
                                                               if_else(LearnMeth == "Online", "Online",
                                                                       if_else(LearnMeth == "Other", "Other",
                                                                               if_else(LearnMeth == "SelfTaught", "Self-Taught",
                                                                                       if_else(LearnMeth == "University", "University",
                                                                                               if_else(LearnMeth == "Work", "Work", "na")))))))


  
cols9 <- c("Kaggle" = "#C0392B", "Online" = "#9B59B6", "Other" = "#2980B9", "Self-Taught" = "#1ABC9C","University" = "#F1C40F", "Work" = "#E67E22")

                         


ggplot(survey25, aes(x= Job, y = mean, fill = learnme))  + 
                                                geom_col()   + 
                                                  scale_fill_manual(values = cols9) +
                                                    scale_y_continuous(labels = percent) +
                                                        labs(x = "", title = "Data Scientist's Report they Learn on the Job", y = ) +
                                                          guides(fill = guide_legend(title = "Learning Method")) +
                                                              theme(panel.background = element_blank())




survey26 <- survey %>%  mutate(lang2 = if_else(lang == "R", "R", "Other")) %>%
  filter(lang2 == "Other") %>%
  group_by(lang2, Age) %>%
  summarise(n = n()) %>%
  mutate(perc = n/16446)


survey27 <- survey %>%  mutate(lang2 = if_else(lang == "R", "R", "Other")) %>%
  filter(lang2 == "R") %>%
  group_by(lang2, Age) %>%
  summarise(n = n()) %>%
  mutate(perc = n/2342)



survey28 <- bind_rows(survey26, survey27)

cols10 <- c("R" = "#f248ae", "Other" = "#48f28c")

ggplot(survey28, aes(x = Age, y = perc, col = lang2)) + 
  geom_point(size = 4) +
  scale_color_manual(values = cols10) +
  labs(title = "R Users are generally older then users of other languages") +
  scale_y_continuous(labels = percent) +
  guides(col = guide_legend(title = "Programing Language")) +
    theme(panel.background = element_blank())



survey29 <- survey %>% filter(lang == "R") %>%
                        select(lang, onlineL, bootcamp) %>%
                          gather("learn", "view", -lang) %>%
                            group_by(learn, view) %>%
                              summarise(tot = n()) %>%
                                filter(view != "NA") %>%
                                  mutate(learn2 = if_else(learn == "bootcamp", "Boot Camp Learning", "Online Learning"))

cols11 <- c("Boot Camp Learning" = "#f248ae", "Online Learning" = "#48f28c")

ggplot(survey29, aes(x = learn2, y = view, size = tot, col = learn2)) + 
                                    geom_point() +
                                      scale_color_manual(values = cols11) +
                                        guides(size = guide_legend(title = "Total"), color = FALSE) +
                                                labs(x = "Learning Style", y = "", title = "R Users Mostly prefer other education to historical institutions") +
                                                    theme(panel.background = element_blank())


survey30 <- survey %>% mutate(lang2 = if_else(lang == "R", "R", "Other")) %>%
                          filter(lang2 == "R") %>%
                            filter(datacat != "NA") %>%
                             group_by(lang2, datacat) %>%
                              summarise(n = n()) %>%
                                mutate(per = n/ 1839) 
                             
survey31 <- survey %>% mutate(lang2 = if_else(lang == "R", "R", "Other")) %>%
  filter(lang2 == "Other") %>%
  filter(datacat != "NA") %>%
  group_by(lang2, datacat) %>%
  summarise(n = n()) %>%
  mutate(per = n/ 12039)  %>%
  filter(datacat != "NA")

survey32 <- bind_rows(survey30, survey31)

cols2 <- c("Audio Data" = "#C0392B", "Categorical Data" = "#9B59B6", "Genetic Data" = "#2980B9", "Geospatial Data" = "#1ABC9C","Image Data" = "#F1C40F", "Numerical Data" = "#E67E22",
           "Other Data" = "#BDC3C7", "Sensor Data" = "#E74C3C", "Tabular Data" = "#8E44AD", "Text Data" = "#3498DB", "Time Series Data" = "#27AE60", "Video Data" = "#F39C12")

ggplot(survey32, aes(x = lang2, y =per, fill = datacat)) + 
                        geom_col() +
                          scale_fill_manual(values = cols2) +
                            scale_y_continuous(labels = percent) +
                              guides(fill = guide_legend(title = "Data Category")) +
                                labs(x = "language", y = "Percent of Respndants", title = "Comparison of what each Language is used for") +
                                theme(panel.background = element_blank()) 



survey33 <- survey %>% filter(lang == "R") %>%
                       group_by(learn) %>%
                          summarise(n = n()) %>%
                            filter(learn != "NA")


ggplot(survey33, aes(x = reorder(learn, n), y = n)) + 
                              geom_col(fill = "steelblue") + 
                                          coord_flip() +
                                          labs(x = "Learning Platform", y = "No. of Respondants", title = "R users favourite learning platforms") +
                                            theme(panel.background = element_blank())


survey34 <- survey %>% mutate(mach2 = if_else(mach == "We use ML methods for generating insights (but do not put working models into production)", "Use ML for insights",
                                              if_else(mach == "We recently started using ML methods (i.e., models in production for less than 2 years)", "Recently started using ML", 
                                                      if_else(mach == "We have well established ML methods (i.e., models in production for more than 2 years)", "Established ML", 
                                                             if_else(mach == "We are exploring ML methods (and may one day put a model into production)", "Exploring ML",
                                                                     if_else(mach == "No (we do not use ML methods)", "ML not Used", 
                                                                             if_else(mach == "I do not know", "Dont Know", "NA"))) )))) %>%
                        group_by(mach2, fair) %>%
                        summarise(n = n()) %>%
                        filter(fair != "NA") %>%
                          filter(mach2 != "NA")

ggplot(survey34, aes(x = mach2, y = fair, size = n)) + 
                                    geom_point(col = "Steelblue") + 
                                      coord_flip() + 
                                      labs(x = "ML Usage", y = "View on fairness/ bias in ML algorithms", title = "Most understand how bias is important in ML ") +
                                        theme(panel.background = element_blank())



                       
survey35 <- survey %>% select(308:329) %>%
                        gather() %>%
                          group_by(value) %>%
                            summarise(n= n()) %>%
                            arrange(desc(n)) %>%
                              filter(value != "NA")

ggplot(survey35, aes(x = reorder(value, n), y = n)) + 
                              geom_col(fill = "steelblue") + 
                                coord_flip() +
                                labs(x = "source", y = "No. Respondants", title = "Kaggle to most popular source") +
                                    theme(panel.background = element_blank())
                                  
