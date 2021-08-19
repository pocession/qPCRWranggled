library(dplyr)

# Do change the working dir
dir <- getwd()
raw <- read.csv(file.path(dir, "qPCRWranggled","20210817.csv"))

# Specify the cell type
Cell <- "Th1"
raw$Cell <- Cell

# Add Target and Sample Annotation
data <- raw %>%
  mutate(Target = case_when(Target.ID == 1 ~ "Bcl2l11CNS-9_1",
                            Target.ID == 2 ~ "Bcl2l11CNS-9_2",
                            Target.ID == 3 ~ "Ifng+18",
                            Target.ID == 4 ~ "Il4ra-8",
                            Target.ID == 5 ~ "Il4ra-12",
                            Target.ID == 6 ~ "Gmpr",
                            Target.ID == 7 ~ "Rad50+7",
                            Target.ID == 8 ~ "Ccr7+2",
                            Target.ID == 9 ~ "Ccr7+4",
                            TRUE ~ as.character(Target.ID))) %>%
  mutate(Sample = case_when(Sample.ID == 1 ~ "WT",
                            Sample.ID == 2 ~ "KO",
                            Sample.ID == 3 ~ "WT_input",
                            Sample.ID == 4 ~ "KO_input",
                            Sample.ID == 5 ~"NTC",
                            TRUE ~ as.character(Sample.ID)
                           )) %>%
  mutate(Ct = case_when(Ct.CP. %in% "--" ~ 40,
                        TRUE ~ as.numeric(Ct.CP.))) %>%
  mutate(Tm = case_when(Tm..1 %in% "--" ~ 60,
                        TRUE ~ as.numeric(Tm..1))) %>%
  filter(!is.na(Sample.ID)) %>%
  select(Cell,Sample,Target,Ct,Tm)

write.csv(data,file.path(dir,"qPCRWranggled",paste(Cell,".csv")))
