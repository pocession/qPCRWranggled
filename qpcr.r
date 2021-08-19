library(dplyr)

# Do change the working dir
dir <- getwd()
raw <- read.csv(file.path(dir, "qPCRWranggled","Raw","20210817_2.csv"))

# Specify the cell type
Cell <- "Th2"
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
  mutate(Genotype = case_when(Sample.ID == 1 | Sample.ID == 3 ~ "WT",
                              Sample.ID == 2 | Sample.ID == 4 ~ "KO",
                            Sample.ID == 5 ~"NTC",
                            TRUE ~ as.character(Sample.ID))) %>%
  mutate(Sample = case_when(Sample.ID == 1 | Sample.ID == 2 ~ "Sample",
                             Sample.ID == 3 | Sample.ID == 4 ~ "Input",
                             Sample.ID == 5 ~"NTC",
                             TRUE ~ as.character(Sample.ID))) %>%
  mutate(Ct = case_when(Ct.CP. %in% "--" ~ 40,
                        TRUE ~ as.numeric(Ct.CP.))) %>%
  mutate(Tm = case_when(Tm..1 %in% "--" ~ 60,
                        TRUE ~ as.numeric(Tm..1))) %>%
  filter(!is.na(Sample.ID)) %>%
  select(Cell,Genotype,Sample,Target,Ct,Tm)

Input <- data %>%
  filter(Sample == "Input") %>%
  group_by(Target,Sample,Genotype) %>%
  summarise(mean(Ct))

Sample <- data %>%
  filter(Sample == "Sample") %>%
  left_join(Input, by = c("Target","Genotype"))

colnames(Sample) <- c("Cell","Genotype","Sample","Target","Ct","Tm","Input","Mean_Ct")

Sample <- Sample %>%
  mutate(deltaCt = Ct - Mean_Ct - log(50,2)) %>%
  mutate(relative_exp = 2^-(deltaCt)) %>%
  group_by(Target,Genotype)

write.csv(Sample,file.path(dir,"qPCRWranggled","Result",paste(Cell,"_summary.csv")))
