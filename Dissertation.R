#### find and set working directory ###
getwd()
setwd("~/Desktop/Cambridge/Project/3.4Working directory") 

### load packages ###
library('tidyverse')
library('extrafont')
library('gridExtra')
library('svglite')

### import data ###
data <- read_csv('Baselineedit.csv')

### Remove N/As from columns with demographic information ###
columns_to_check <- c("Add in", "Q1", "Q2", "Q3...6", "Q4...7", "Q7_1","Q9_1")
data <- data[complete.cases(data[, columns_to_check]), ]

###Change data class to factors and numeric where needed###
numeric <- c("Q7_1", "Q9_1", "Q47_1", "Q47_2", "Q47_3", "Q47_4",
             "Q121_1", "Q121_2", "Q121_3", "Q121_4",
             "Q48_1", "Q48_2", "Q48_3", "Q48_4",
             "Q49_1", "Q49_2", "Q49_3", "Q49_4")

data <- data %>%
  mutate(
    # Convert character columns to factors
    across(where(is.character), ~ factor(na_if(., ""), levels = unique(na_if(., "")))),
    # Convert numeric columns to numeric after cleaning
    across(all_of(numeric), ~ as.numeric(gsub("[^0-9.]", "", .)))
  )

### Check data ###
summary(data)

### Participant ages - graphs and summary statistics ###
countsA <- data %>%
  count(`Add in`) %>%
  mutate(percentage = n / sum(n) * 100)

pA <- ggplot(countsA, aes(x = `Add in`, y = percentage)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Age Group (years)") +
  ylab("Percentage (%)") +
  ggtitle("Participant Ages") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  


### Participant location - graphs and summary statistics ###
countsR <- data %>%
  count(`Q1`) %>%
  mutate(percentage = n / sum(n) * 100)

pR <- ggplot(countsR, aes(x = `Q1`, y = percentage)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Region") +
  ylab("Percentage (%)") +
  ggtitle("Participant Location by Region") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  


pR <- pR + scale_x_discrete(labels = label_wrap_gen(width = 10))

### Grouping qualifications ###
summary(data$Q2)

qualification_groups <- c(
  "Prefer not to say" = "None/Unknown",
  "University or CNAA higher degree (e.g. M.Sc, Ph.D)" = "Postgraduate Qualifications",
  "Nursing qualification (e.g. SEN, SRN, SCM, RGN)" = "Professional Qualifications",
  "University diploma" = "Undergraduate Qualifications",
  "GCE A level or Higher Certificate" = "School Qualifications",
  "University or CNAA first degree (e.g. BA, B.Sc, B.Ed)" = "Undergraduate Qualifications",
  "City & Guilds certificate" = "Professional Qualifications",
  "Don't know" = "None/Unknown",
  "Recognised trade apprenticeship completed" = "Professional Qualifications",
  "CSE grade 1, GCE O level, GCSE, School Certificate" = "School Qualifications",
  "Other technical, professional or higher qualification" = "Professional Qualifications",
  "Clerical and commercial" = "Professional Qualifications",
  "Teaching qualification (not degree)" = "Professional Qualifications",
  "No formal qualifications" = "None/Unknown",
  "CSE grades 2-5" = "School Qualifications",
  "City & Guilds certificate - advanced" = "Professional Qualifications"
)

data <- data %>%
  mutate(Q2_grouped = recode(Q2, !!!qualification_groups))


### Participants Highest Qual - graphs and summary statistics ###
countsE <- data %>%
  count(`Q2_grouped`) %>%
  mutate(percentage = n / sum(n) * 100) 

desired_order <- c("None/Unknown",
                   "School Qualifications",
                   "Professional Qualifications",
                   "Undergraduate Qualifications",
                   "Postgraduate Qualifications")
countsE$Q2_grouped <- factor(countsE$Q2_grouped, levels = desired_order)

pQual <- ggplot(countsE, aes(x = `Q2_grouped`, y = percentage)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Qualfication") +
  ylab("Percentage (%)") +
  ggtitle("Highest Qualifcation \nAchieved by Participants") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  

pQual <- pQual + scale_x_discrete(labels = label_wrap_gen(width = 15))


### Participant Age when left education - graphs and summary statistics ###
countsE1 <- data %>%
  count(`Q3...6`) %>%
  mutate(percentage = n / sum(n) * 100) 

desired_order <- c("15 or under",
                   "16",
                   "17-18",
                   "19",
                   "20+",
                   "Still at school/Full time student",
                   "Can't remember")
countsE1$Q3...6 <- factor(countsE1$Q3...6, levels = desired_order)

pALE <- ggplot(countsE1, aes(x = `Q3...6`, y = percentage)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Age (years)") +
  ylab("Percentage (%)") +
  ggtitle("Age when Participants \nleft Full-time Education") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  


pALE <- pALE + scale_x_discrete(labels = label_wrap_gen(width = 12))

### Arrange graphs for age, region and education on grid ###
g <- grid.arrange(
  pR, pA, pQual, pALE, nrow = 2, ncol = 2)

#### save as .svg ###
ggsave("output.svg", g, device = "svg")


### Gene Risk - graphs and summary statistics ###
counts <- data %>%
  count(`Gene`) %>%
  mutate(percentage = n / sum(n) * 100) 

desired_order <- c("High",
                   "Mod",
                   "Both",
                   "NA")
counts$Gene <- factor(counts$Gene, levels = desired_order)

gene_labels <- c("High", "Moderate", "Both", "Unknown")

pGene <- ggplot(counts, aes(x = `Gene`, y = percentage)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Gene Risk") +
  ylab("Percentage (%)") +
  ggtitle("Number of Participants being tested for\nHigh or Moderate Risk Genes") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5) +
  scale_x_discrete(labels = gene_labels)

#### save as .svg ###
ggsave("GR1.svg", pGene, device = "svg", width = 8, height = 6)

### Healthcare - summary statistics and Shapiro-Wilks test ###
summary(data$Healthcare)
shapiro.test(data$Healthcare) ### results is sig (<0.05), data not normally distrubuted)

### Genetics Appt - graphs and summary statistics ###
counts <- data %>%
  count(`Q132`) %>%
  mutate(percentage = n / sum(n) * 100) 

pGA <- ggplot(counts, aes(x = `Q132`, y = percentage)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Attendance of Genetics Appointment") +
  ylab("Percentage (%)") +
  ggtitle("Number of Participants who have Attended a Genetics Appointment") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)


#### save as .svg ###
ggsave("GA3.svg", pGA, device = "svg", width = 8, height = 6)


### Perception of health - graphs and summary statistics ###
counts <- data %>%
  count(`Q19`) %>%
  mutate(percentage = n / sum(n) * 100) 

counts$Q19 <- trimws(counts$Q19) 
counts$Q19 <- tolower(counts$Q19) 
desired_order <- c('poor', 'fair', 'good', 'very good', 'excellent')
counts$Q19 <- factor(counts$Q19, levels = desired_order)
counts <- counts[!is.na(counts$Q19), ]


pHealth <- ggplot(counts, aes(x = `Q19`, y = percentage)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Participant Perception of their General Health") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)

#### save as .svg ###
ggsave("Health.svg", pHealth, device = "svg", width = 12, height = 6)

### Family History (FHx) - load data ###
datacam <- read_csv('Baselinecam.csv')

### FHx - Remove NAs from necessary columns ###
columns_to_check <- c("Add in", "Q1", "Q2", "Q3...6", "Q4...7", "Q7_1","Q9_1",
                      "BC1", "BC2", "BC3", "BC4", 
                      "OC1", "OC2", "OC3", "OC4")
datacam <- datacam[complete.cases(datacam[, columns_to_check]), ]


### FHx - Change data class to factors and numeric where needed ###
numericcam <- c("Q7_1", "Q9_1", "Q47_1", "Q47_2", "Q47_3", "Q47_4",
                "Q121_1", "Q121_2", "Q121_3", "Q121_4",
                "Q48_1", "Q48_2", "Q48_3", "Q48_4",
                "Q49_1", "Q49_2", "Q49_3", "Q49_4", 
                "BC1", "BC2", "BC3", "BC4", 
                "OC1", "OC2", "OC3", "OC4")


datacam <- datacam %>%
  mutate(
    across(where(is.character), ~ factor(na_if(., ""), levels = unique(na_if(., "")))),
    across(all_of(numericcam), ~ as.numeric(gsub("[^0-9.]", "", .)))
  )


### BC FHx - graphs and summary statistics ###
countsBC1 <- datacam %>%
  count(`BC1`) %>%
  mutate(percentage = n / sum(n) * 100)


countsBC2 <- datacam %>%
  count(`BC2`) %>%
  mutate(percentage = n / sum(n) * 100)

countsBC3 <- datacam %>%
  count(`BC3`) %>%
  mutate(percentage = n / sum(n) * 100)

countsBC4 <- datacam %>%
  count(`BC4`) %>%
  mutate(percentage = n / sum(n) * 100)

countsBC1 <- countsBC1 %>% rename(`Number of Relatives` = BC1)
countsBC2 <- countsBC2 %>% rename(`Number of Relatives` = BC2)
countsBC3 <- countsBC3 %>% rename(`Number of Relatives` = BC3)
countsBC4 <- countsBC4 %>% rename(`Number of Relatives` = BC4)

countsBC1 <- countsBC1 %>% mutate(RelativeType = "1st Degree")
countsBC2 <- countsBC2 %>% mutate(RelativeType = "2nd Degree")
countsBC3 <- countsBC3 %>% mutate(RelativeType = "3rd Degree")
countsBC4 <- countsBC4 %>% mutate(RelativeType = "4th Degree")

combined_dataBC <- bind_rows(countsBC1, countsBC2, countsBC3, countsBC4)

color_palette <- c("deepskyblue4", "dodgerblue3", "steelblue", "skyblue", "lightblue", "powderblue")

combined_dataBC$RelativeType <- factor(combined_dataBC$RelativeType, 
                                       levels = c("1st Degree", "2nd Degree", "3rd Degree", "4th Degree"))

pBC <- ggplot(combined_dataBC, aes(x = factor(`Number of Relatives`), y = percentage, fill = factor(`Number of Relatives`))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, position = position_dodge(0.9), size = 3) +
  scale_fill_manual(values = color_palette) +
  labs(x = "Number of Relatives", y = "Percentage (%)") +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100), expand = c(0, 0)) +
  ggtitle("Family History of Breast Cancer") +
  theme_minimal(base_family = "Arial") +
  facet_grid(. ~ RelativeType, scales = "free_x", space = "free_x") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center the title
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none" 
  )

### FHx of OC - grpahs and summary statistics ###
countsOC1 <- datacam %>%
  count(OC1, name = "n") %>%
  mutate(percentage = n / sum(n) * 100) %>%
  rename(`Number of Relatives` = OC1) %>%
  mutate(RelativeType = "1st Degree")

countsOC2 <- datacam %>%
  count(OC2, name = "n") %>%
  mutate(percentage = n / sum(n) * 100) %>%
  rename(`Number of Relatives` = OC2) %>%
  mutate(RelativeType = "2nd Degree")

countsOC3 <- datacam %>%
  filter(!is.na(OC3)) %>%
  count(OC3, name = "n") %>%
  mutate(percentage = n / sum(n) * 100) %>%
  rename(`Number of Relatives` = OC3) %>%
  mutate(RelativeType = "3rd Degree")

countsOC4 <- datacam %>%
  filter(!is.na(OC4)) %>%
  count(OC4, name = "n") %>%
  mutate(percentage = n / sum(n) * 100) %>%
  rename(`Number of Relatives` = OC4) %>%
  mutate(RelativeType = "4th Degree")


combined_dataOC <- bind_rows(countsOC1, countsOC2, countsOC3, countsOC4)

combined_dataOC$`Number of Relatives` <- as.numeric(combined_dataOC$`Number of Relatives`)

color_palette <- c("palegreen","mediumseagreen", "mediumspringgreen", "springgreen", "seagreen", "lightgreen")

combined_dataOC$RelativeType <- factor(combined_dataOC$RelativeType, 
                                       levels = c("1st Degree", "2nd Degree", "3rd Degree", "4th Degree"))

pOC <- ggplot(combined_dataOC, aes(x = factor(`Number of Relatives`), y = percentage, fill = factor(`Number of Relatives`))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, position = position_dodge(0.9), size = 3) +
  scale_fill_manual(values = color_palette) +
  labs(x = "Number of Relatives", y = "Percentage (%)") +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100), expand = c(0, 0)) +
  ggtitle("Family History of Ovarian Cancer") +
  theme_minimal(base_family = "Arial") +
  facet_grid(. ~ RelativeType, scales = "free_x", space = "free_x") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center the title
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 
  )

### Arrange FHx of BC and OC graphs in grid ###
g1 <- grid.arrange(
  pBC, pOC, nrow = 1, ncol = 2)

#### save as .svg ###
ggsave("F.svg", g1, device = "svg", width = 12, height = 6)

### Numerical Risk Q - graphs and summary statistics###
counts <- data %>%
  count(`Q4...8`) %>%
  mutate(percentage = n / sum(n) * 100) 

desired_order <- c("1 in 10", "1 in 100", "1 in 1000")
counts$Q4...8 <- factor(counts$Q4...8, levels = desired_order)

pRQ <- ggplot(counts, aes(x = `Q4...8`, y = percentage)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Participant Answers to 'Which of the following numbers \ndo you think represents the biggest risk of getting a disease?'") +
  theme_minimal(base_family = "Arial") +  #
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)

#### save as .svg ###
ggsave("RQ.svg", pRQ, device = "svg", width = 8, height = 6)


### 10yr and lifetime risk (words) - graphs and summary statistics ###
counts_Q6 <- data %>%
  count(Q6) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  rename(response = Q6, n_Q6 = n, percentage_Q6 = percentage)  


desired_order <- c("Very unlikely", "Unlikely", "Neither likely or unlikely", "Likely", "Very likely")
counts_Q6$response <- factor(counts_Q6$response, levels = desired_order)

counts_Q8 <- data %>%
  count(Q8) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  rename(response = Q8, n_Q8 = n, percentage_Q8 = percentage) 


counts_Q8$response <- factor(counts_Q8$response, levels = desired_order)


combined_counts <- bind_rows(
  mutate(counts_Q6, type = "Q6"),
  mutate(counts_Q8, type = "Q8")
)

p_combined <- ggplot(combined_counts, aes(x = response, y = ifelse(type == "Q6", percentage_Q6, percentage_Q8), fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Participant Perceived Likelihood of getting Breast Cancer") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c("Q6" = "lightblue", "Q8" = "palegreen"), 
                    labels = c("Q6" = "10 year risk", "Q8" = "Lifetime risk")) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", ifelse(type == "Q6", n_Q6, n_Q8))),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  guides(fill = guide_legend(title = "Risk Type")) 

#### save as .svg ###
ggsave("R2.svg", p_combined, device = "svg", width = 10, height = 6)


### 10yr and lifetime risk (numerical) - graph and summary statistics ###
new_df <- select(data,'Q7_1', 'Q9_1')

colnames(new_df)
colnames(new_df) <- c('10 Year Risk', 'Lifetime Risk')

data_long <- new_df %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

p_numerical <- ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(title = "Comparison of Numerical Perceptions for\n10 Year and Lifetime Risk", 
       x = "Risk", 
       y = "Response") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),  
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#### save as .svg ###
ggsave("R4.svg", p_numerical, device = "svg", width = 6, height = 6)


### 10yr and lifetime risk (numerical) - Shapiro-Wilks Test ###
shapiro.test(data$Q7_1) # p=<0.01 - data is not normally distributed 
shapiro.test(data$Q9_1) # p=<0.01 - data is not normally distributed 

### 10yr and lifetime risk (numerical) - Wilcoxon Signed Rank Test ###
wilcox.test(data$Q7_1, data$Q9_1, conf.int = TRUE) # p=<0.01 - difference is staistaclly signficant, reject the null 


### Comparative risk - graphs and summary statistics ###
counts_Q10 <- data %>%
  filter(!is.na(Q10)) %>%
  count(Q10) %>%
  mutate(percentage_Q10 = n / sum(n) * 100) %>%
  rename(response = Q10, n_Q10 = n)  

desired_order <- c('Much below average', 'Below average', 'Same average risk as other women your age', 'Above average', 'Much above average')
counts_Q10$response <- factor(counts_Q10$response, levels = desired_order)

counts_Q11 <- data %>%
  filter(!is.na(Q11)) %>%
  count(Q11) %>%
  mutate(percentage_Q11 = n / sum(n) * 100) %>%
  rename(response = Q11, n_Q11 = n)  

counts_Q11$response <- factor(counts_Q11$response, levels = desired_order)

combined_counts <- bind_rows(
  mutate(counts_Q10, type = "Q10"),
  mutate(counts_Q11, type = "Q11")
)

p_combined <- ggplot(combined_counts, aes(x = response, y = ifelse(type == "Q10", percentage_Q10, percentage_Q11), fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Participant Perceived Likelihood of getting Breast Cancer \ncompared to other Women of their Age and Ethnicity") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5, margin = margin(t = 0.5, r = 0, b = 0, l = 0)),  # Adjust angle and alignment
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c("Q10" = "lightblue", "Q11" = "palegreen"),  
                    labels = c("Q10" = "10 year risk", "Q11" = "Lifetime risk")) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", ifelse(type == "Q10", n_Q10, n_Q11))),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  guides(fill = guide_legend(title = "Risk Type"))  

p_combined + scale_x_discrete(labels = label_wrap_gen(width = 15))

#### save as .svg ###
ggsave("CR.svg", p_combined, device = "svg", width = 10, height = 6)

### Create dataframe for factors affecting understanding of risk information ###
udata <- select(data,'Q4...8', 'Add in', "Q1", "Q2", "Q3...6", "Q4...7", "Q132", "Healthcare" )

### Convert responses to 1 (correct) or 0 (incorrect) ###
data <- udata %>%
  mutate(Understanding_correct = ifelse(`Q4...8` == "1 in 10", 1, 0))

### Change column names of dataframe ### 
colnames(udata) <- c("Q4..8", "Age", "Location", "Education", "Education1", "Ethnicity", "GA", "Healthcare", "Understanding_correct")

### Impact of age on understanding ###
contingency_table <- table(udata$Age, udata$Understanding_correct)
fishers_test <- fisher.test(contingency_table) # p=>0.05 - not significant 

### Impact of highest qualification on understanding ###
contingency_table <- table(udata$Education, udata$Understanding_correct)
fisher.test(contingency_table, simulate.p.value = TRUE) # p=>0.05 - not significant 

### Impact of age when left education on understanding ###
contingency_table <- table(udata$Education1, udata$Understanding_correct)
fishers_test <- fisher.test(contingency_table) # p=>0.05 - not significant 

### Impact of attendance of genetics appointment on understanding ###
contingency_table <- table(udata$GA, udata$Understanding_correct)
chisq_test <- chisq.test(contingency_table) # p=>0.05 - not significant 

### Create dataframe for factors perceived 10 year risk ###
tenyr_data <- select(data, "Q7_1", "Add in", "Gene", "Q1", "Q2_grouped", "Q3...6", "Q4...7", "Q132", "Q19", "Healthcare")
colnames(tenyr_data) <- c("Risk", "Age", "Gene", "Location", "Education", "Education1", "Ethnicity", "GA", "Health", "Visits")

### Impact of highest qualification on perceived 10 year risk ###
kruskal_test <- kruskal.test(Risk ~ Education, data = tenyr_data) # p= <0.05 -signficant 

### Impact of highest qualification on perceived 10 year risk - graph and summary statistics ###
summary_stats <- tenyr_data %>%
  group_by(Education) %>%
  summarise(
    count = n(),
    median = median(Risk, na.rm = TRUE),
    IQR = IQR(Risk, na.rm = TRUE)
  )

education_levels <- c("None/Unknown",
                      "School Qualifications",
                      "Professional Qualifications",
                      "Undergraduate Qualifications",
                      "Postgraduate Qualifications")

tenyr_data$Education <- factor(tenyr_data$Education, levels = education_levels)

group_counts <- table(tenyr_data$Education)
group_labels <- paste0("n=", group_counts[education_levels])

ped <- ggplot(tenyr_data, aes(x = factor(Education, levels = education_levels), y = Risk)) +
  geom_boxplot() +
  ylab("Perceived 10 Year Risk of Breast Cancer") +
  xlab("Highest Qualification Achieved") +
  ggtitle("Perceived 10 Year Risk of Breast Cancer depending on \nHighest Qualification Achieved") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"), 
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  annotate("text", x = 1:length(group_labels), y = min(tenyr_data$Risk) - 1, label = group_labels, size = 3, hjust = 0.5)

p_ed <- ped + scale_x_discrete(labels = label_wrap_gen(width = 15))

### Impact of age when left education on perceived 10 year risk ###
education1_levels <- c("15 or under",
                       "16",
                       "17-18",
                       "19",
                       "20+")

filtered2 <- tenyr_data %>%
  filter(!(`Education1` %in% c("Still at school/Full time student", "Can't remember"))) %>%
  droplevels() %>%
  mutate(Education1 = factor(Education1, levels = education1_levels))

kruskal_test <- kruskal.test(Risk ~ Education1, data = filtered2) # p= <0.05 -signficant 

### Impact of highest qualification on perceived 10 year risk - graph and summary statistics ###

group2_counts <- table(filtered2$Education1)
group2_labels <- paste0("n=", group2_counts)

ped1 <- ggplot(filtered2, aes(x = factor(Education1, levels = education1_levels), y = Risk)) +
  geom_boxplot() +
  ylab("Perceived 10 Year Risk of Breast Cancer") +
  xlab("Age when left Full-time Education (years)") +
  ggtitle("Perceived 10 Year Risk of Breast Cancer depending on Age \nwhen left Full-time Education") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),  
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  annotate("text", x = 1:length(group2_labels), y = min(filtered2$Risk) - 1, label = group2_labels, size = 3, hjust = 0.5)

### Arrange graphs ped and ped1 as grid ###
ged <- grid.arrange(p_ed, ped1, nrow = 1, ncol = 2)

#### save as .svg ###
ggsave("ged2.svg", ged, device = "svg", width = 12, height = 6)

### Impact of no. of healthcare visits on perceived 10 year risk ###
spearman_corr <- cor.test(tenyr_data$Visits, tenyr_data$Risk, method = "spearman") # p= <0.05 -signficant 

### Impact of perceived health on perceived 10 year risk ###
kruskal_test <- kruskal.test(Risk ~ Health, data = tenyr_data) # p= <0.05 -signficant

### Impact of perceived health on perceived 10 year risk - graphs  ###
Health_levels <- c("Poor", "Fair", "Good", "Very good", "Excellent")

filtered3 <- tenyr_data %>%
  filter(!(`Health` %in% c(NA))) %>%
  droplevels() %>%
  mutate(Health = factor(Health, levels = Health_levels))

group3_counts <- table(filtered3$Health)
group3_labels <- paste0("n=", group3_counts)


phealth <- ggplot(filtered3, aes(x = factor(Health, levels = Health_levels), y = Risk)) +
  geom_boxplot() +
  ylab("Perceived 10 Year Risk of Breast Cancer") +
  xlab("Perception of Health") +
  ggtitle("Perceived 10 Year Risk of Breast Cancer depending on \nPerception of Health") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),  # Set font to Arial
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  annotate("text", x = 1:length(group3_labels), y = min(filtered3$Risk) - 1, label = group3_labels, size = 3, hjust = 0.5)

#### save as .svg ###
ggsave("health.svg", phealth, device = "svg", width = 10, height = 6)


### Impact of BC FHx on 10yr RP ###
tenyr_datacam <- select(datacam, "Q7_1", "BC1", "BC2", "BC3", "BC4", 
                        "OC1", "OC2", "OC3", "OC4")

colnames(tenyr_datacam) <- c("Risk","BC1", "BC2", "BC3", "BC4", 
                             "OC1", "OC2", "OC3", "OC4")

spearman_corr <- cor.test(tenyr_datacam$Sum_BC, tenyr_datacam$Risk, method = "spearman")  # p=>0.05 - not significant 

tenyr_datacam <- tenyr_datacam %>%
  mutate(
    BC1_Status = ifelse(BC1 != 0, "Yes", "No"),
    BC2_Status = ifelse(BC2 != 0, "Yes", "No"),
    BC3_Status = ifelse(BC3 != 0, "Yes", "No"),
    BC4_Status = ifelse(BC4 != 0, "Yes", "No")
  )

mann_whitney_test <- wilcox.test(Risk ~ BC1_Status, data = tenyr_datacam)  # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ BC2_Status, data = tenyr_datacam)  # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ BC3_Status, data = tenyr_datacam)  # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ BC4_Status, data = tenyr_datacam)  # p=>0.05 - not significant 


### Impact of OC FHx on 10yr RP ###

spearman_corr <- cor.test(tenyr_datacam$Sum_OC, tenyr_datacam$Risk, method = "spearman") # p=>0.05 - not significant 

tenyr_datacam <- tenyr_datacam %>%
  mutate(
    OC1_Status = ifelse(OC1 != 0, "Yes", "No"),
    OC2_Status = ifelse(OC2 != 0, "Yes", "No"),
    OC3_Status = ifelse(OC3 != 0, "Yes", "No"),
    OC4_Status = ifelse(OC4 != 0, "Yes", "No")
  )

mann_whitney_test <- wilcox.test(Risk ~ OC1_Status, data = tenyr_datacam)  # p=<0.05 - significant 
mann_whitney_test <- wilcox.test(Risk ~ OC2_Status, data = tenyr_datacam)  # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ OC3_Status, data = tenyr_datacam)  # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ OC4_Status, data = tenyr_datacam)  # p=>0.05 - not significant 

### Impact of OC FHx on 10yr RP - graph ###
group_sizes <- tenyr_datacam %>%
  group_by(OC1_Status) %>%
  summarize(count = n())

pOC10 <- ggplot(tenyr_datacam, aes(x = factor(OC1_Status), y = Risk)) +
  geom_boxplot() +
  ylab("Perceived 10 Year Risk of Breast Cancer") +
  xlab("First degree relative with Ovarian Cancer") +
  ggtitle("Impact of a first degree relative with Ovarian Canceer on \nperceived 10 Year Risk of Breast Cancer") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"), 
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  geom_text(data = group_sizes, aes(x = factor(OC1_Status), y = -0.5, label = paste("n=", count)), size = 3, vjust = 1.5)

#### save as .svg ###
ggsave("OC2.svg", pOC10, device = "svg", width = 6, height = 6)

### Impact of age on perceived 10 year risk ###
kruskal_test <- kruskal.test(Risk ~ Age, data = tenyr_data) # p=>0.05 - not significant 

### Impact of gene on perceived 10 year risk ###
filtered <- tenyr_data %>%
  filter(Gene %in% c("High", "Mod"))

mann_whitney_test <- wilcox.test(Risk ~ Gene, data = filtered) # p=>0.05 - not significant 

### Impact of region on perceived 10 year risk ###
kruskal_test <- kruskal.test(Risk ~ Location, data = tenyr_data) # p=>0.05 - not significant 

### Impact of attendance of genetics appt on perceived 10 year risk ###
mann_whitney_test <- wilcox.test(Risk ~ GA, data = tenyr_data) # p=>0.05 - not significant 

### Create dataframe for factors and perceived lifetime risk ###
life_data <- select(data, "Q9_1", "Add in", "Gene", "Q1", "Q2_grouped", "Q3...6", "Q4...7", "Q132", "Q19", "Healthcare")
colnames(life_data) <- c("Risk", "Age", "Gene", "Location", "Education", "Education1", "Ethnicity", "GA", "Health", "Visits")

### Impact of age on perceived lifetime risk ###
kruskal_test <- kruskal.test(Risk ~ Age, data = life_data) # p=<0.05 - significant 

### Impact of age on perceived lifetime risk - graphs ###
age_levels <- c("18-24", "25-29", "30-39", "40-49", "50+")

life_data$Age <- factor(life_data$Age, levels = age_levels)

group_counts <- table(life_data$Age)
group_labels <- paste0("n=", group_counts[age_levels])

page <- ggplot(life_data, aes(x = factor(Age, levels = age_levels), y = Risk)) +
  geom_boxplot() +
  ylab("Perceived Lifetime Risk of Breast Cancer") +
  xlab("Age (years)") +
  ggtitle("Perceived Lifetime Risk of Breast Cancer depending on \nAge Group") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),  # Set font to Arial
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  annotate("text", x = 1:length(group_labels), y = min(life_data$Risk) - 2, label = group_labels, size = 3, hjust = 0.5)

#### save as .svg ###
ggsave("age1.svg", page, device = "svg", width = 10, height = 6)


### Impact of BC FHx on perceived lifetime risk ### 
life_datacam <- select(datacam, "Q9_1", "BC1", "BC2", "BC3", "BC4", 
                       "OC1", "OC2", "OC3", "OC4")

colnames(life_datacam) <- c("Risk","BC1", "BC2", "BC3", "BC4", 
                            "OC1", "OC2", "OC3", "OC4")

spearman_corr <- cor.test(life_datacam$Sum_BC, life_datacam$Risk, method = "spearman") # p=<0.05 - significant 

life_datacam <- life_datacam %>%
  mutate(
    BC1_Status = ifelse(BC1 != 0, "Yes", "No"),
    BC2_Status = ifelse(BC2 != 0, "Yes", "No"),
    BC3_Status = ifelse(BC3 != 0, "Yes", "No"),
    BC4_Status = ifelse(BC4 != 0, "Yes", "No")
  )

mann_whitney_test <- wilcox.test(Risk ~ BC1_Status, data = tenyr_datacam) # p=<0.05 - significant 
mann_whitney_test <- wilcox.test(Risk ~ BC2_Status, data = tenyr_datacam) # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ BC3_Status, data = tenyr_datacam) # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ BC4_Status, data = tenyr_datacam) # p=>0.05 - not significant 

### Impact of BC FHx on perceived lifetime risk - graphs ### 
group_sizes <- life_datacam %>%
  group_by(BC1_Status) %>%
  summarize(count = n())

pBCL <- ggplot(life_datacam, aes(x = factor(BC1_Status), y = Risk)) +
  geom_boxplot() +
  ylab("Perceived lifetime Year Risk of Breast Cancer") +
  xlab("First degree relative with Breast Cancer") +
  ggtitle("Impact of a first degree relative with Breast Canceer on \nperceived lifetime Risk of Breast Cancer") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),  
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  geom_text(data = group_sizes, aes(x = factor(BC1_Status), y = -0.5, label = paste("n=", count)), size = 3, vjust = 1.5)

#### save as .svg ###
ggsave("BCL.svg", pBCL, device = "svg", width = 6, height = 6)

### Impact of OC FHx on perceived lifetime risk ### 
life_datacam <- life_datacam %>%
  mutate(
    OC1_Status = ifelse(OC1 != 0, "Yes", "No"),
    OC2_Status = ifelse(OC2 != 0, "Yes", "No"),
    OC3_Status = ifelse(OC3 != 0, "Yes", "No"),
    OC4_Status = ifelse(OC4 != 0, "Yes", "No")
  )

mann_whitney_test <- wilcox.test(Risk ~ OC1_Status, data = life_datacam) # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ OC2_Status, data = life_datacam) # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ OC3_Status, data = life_datacam) # p=>0.05 - not significant 
mann_whitney_test <- wilcox.test(Risk ~ OC4_Status, data = life_datacam) # p=>0.05 - not significant 

### Impact of gene on perceived lifetime risk ###
filtered <- life_data %>%
  filter(Gene %in% c("High", "Mod"))

mann_whitney_test <- wilcox.test(Risk ~ Gene, data = filtered) # p=>0.05 - not significant 

### Impact of region on perceived lifetime risk ###
kruskal_test <- kruskal.test(Risk ~ Location, data = life_data) # p=>0.05 - not significant 

### Impact of highest qual on perceived lifetime risk ###
kruskal_test <- kruskal.test(Risk ~ Education, data = life_data) # p=>0.05 - not significant 

### Impact of age left education on perceived lifetime risk ###
kruskal_test <- kruskal.test(Risk ~ Education1, data = life_data) # p=>0.05 - not significant 

### Impact of attending a genetics appt on perceived lifetime risk ###
mann_whitney_test <- wilcox.test(Risk ~ GA, data = life_data) # p=>0.05 - not significant 

### Impact of no. of interactions with healthcare on perceived lifetime risk ###
spearman_corr <- cor.test(life_data$Visits, life_data$Risk, method = "spearman") # p=>0.05 - not significant 

### Impact of perceived health on perceived lifetime risk ###
kruskal_test <- kruskal.test(Risk ~ Health, data = life_data) # p=>0.05 - not significant 


### Worry about BC risk - graphs and statistics ###
counts_Q12 <- data %>%
  filter(!is.na(Q12)) %>%
  count(Q12) %>%
  mutate(percentage_Q12 = n / sum(n) * 100) %>%
  rename(response = Q12, n_Q12 = n)  # Rename columns

desired_order <- c('Not at all worried', 'Slightly worried', 'Somewhat worried', 'Worried', 'Very worried')
counts_Q12$response <- factor(counts_Q12$response, levels = desired_order)

counts_Q13 <- data %>%
  filter(!is.na(Q13)) %>%
  count(Q13) %>%
  mutate(percentage_Q13 = n / sum(n) * 100) %>%
  rename(response = Q13, n_Q13 = n)  # Rename columns

counts_Q13$response <- factor(counts_Q13$response, levels = desired_order)

combined_counts <- bind_rows(
  mutate(counts_Q12, type = "Q12"),
  mutate(counts_Q13, type = "Q13")
)

p_combined <- ggplot(combined_counts, aes(x = response, y = ifelse(type == "Q12", percentage_Q12, percentage_Q13), fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Levels of Participant Worry about getting \nBreast Cancer") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5, margin = margin(t = 0.5, r = 0, b = 0, l = 0)),  # Adjust angle and alignment
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c("Q12" = "lightblue", "Q13" = "palegreen"),  
                    labels = c("Q12" = "10 year risk", "Q13" = "Lifetime risk")) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", ifelse(type == "Q12", n_Q12, n_Q13))),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  guides(fill = guide_legend(title = "Risk Type")) 

p_combined + scale_x_discrete(labels = label_wrap_gen(width = 15))

#### save as .svg ###
ggsave("Worry.svg", p_combined, device = "svg", width = 10, height = 6)

### Frequency of BC thoughts - graphs and summary statistics ###
counts_Q14 <- data %>%
  filter(!is.na(Q14)) %>%
  count(Q14) %>%
  mutate(percentage_Q14 = n / sum(n) * 100)

desired_order <- c('Not at all', 'Rarely', 'Sometimes', 'Often', 'Almost all the time')
counts_Q14$Q14 <- factor(counts_Q14$Q14, levels = desired_order)

pthink <- ggplot(counts_Q14, aes(x = `Q14`, y = percentage_Q14)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("How often participants thought about their chance \n of developing breast cancer in the previous month") +
  theme_minimal(base_family = "Arial") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5) 

#### save as .svg ###
ggsave("Think.svg", pthink, device = "svg", width = 10, height = 6)

### Impact on mood - graphs and summary statistics ###
counts_Q15 <- data %>%
  filter(!is.na(Q15)) %>%
  count(Q15) %>%
  mutate(percentage_Q15 = n / sum(n) * 100)

desired_order <- c('Not at all', 'A little', 'Somewhat', 'A lot')
counts_Q15$Q15 <- factor(counts_Q15$Q15, levels = desired_order)

pmood <- ggplot(counts_Q15, aes(x = `Q15`, y = percentage_Q15)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("How often do thoughts of your chance of \ndeveloping breast cancer affect your mood?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  

### Impact on activity - graphs and summary statistics ###
counts_Q16 <- data %>%
  filter(!is.na(Q16)) %>%
  count(Q16) %>%
  mutate(percentage_Q16 = n / sum(n) * 100)

desired_order <- c('Not at all', 'A little', 'Somewhat', 'A lot')
counts_Q16$Q16 <- factor(counts_Q16$Q16, levels = desired_order)

pactivity <- ggplot(counts_Q16, aes(x = `Q16`, y = percentage_Q16)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("How often do thoughts of your chance of \ndeveloping breast cancer \n affect your ability to perform daily activities?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  # Annotate counts above the bars

### Arrange graphs as a grid ###
impact <- grid.arrange(pmood, pactivity, nrow = 1, ncol = 2)

#### save as .svg ###
ggsave("impact1.svg", impact, device = "svg", width = 12, height = 6)

### Levels of inform - graphs and summary statistics ###
data$Q40 <- trimws(data$Q40)  # Remove leading/trailing whitespace

desired_order <- c('Yes, very well informed', 'Kind of informed', 'No, I have no idea of options are available')

data$Q40 <- factor(data$Q40, levels = desired_order)

data_summary <- data %>%
  filter(!is.na(Q40)) %>%
  group_by(Q40) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p40 <- ggplot(data = data_summary, aes(x = Q40, y = percentage, label = paste("n =", count))) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Participant awareness of the options available to women\n with a pathogenic gene change with an increased risk of certain types of cancer") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", count)), vjust = -0.5)  

#### save as .svg ###
ggsave("p401.svg", p40, device = "svg", width = 10, height = 6)

### Levels of inform by GA - graphs and summary statistics ###
data_summary_q132 <- data %>%
  filter(!is.na(Q40)) %>%
  filter(Q132 == "Yes" | Q132 == "No") %>%
  group_by(Q40, Q132) %>%
  summarise(count = n()) %>%
  group_by(Q40) %>%
  mutate(percentage = count / sum(count) * 100)

colors_q132 <- c("Yes" = "lightblue", "No" = "palegreen")

p_q132 <- ggplot(data = data_summary_q132, aes(x = Q40, y = percentage, fill = Q132, label = paste("n =", count))) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = colors_q132, name = "Attended genetics \nappointment?") +  
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Participant awareness of the options available to women\n with a pathogenic gene change with an increased risk of certain types of cancer\n(Genetics Appointment)") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(position = position_dodge(width = 0.9), aes(group = Q132), vjust = -0.5, size = 3.5)  

#### save as .svg ###
ggsave("p132.svg", p_q132, device = "svg", width = 10, height = 6)

### different options for RM - graphs and summary statistics###
responses <- pull(data, "Q42")
split_responses <- str_split(responses, ",")

all_responses <- unlist(split_responses)
response_df <- data.frame(option = all_responses)

total_population <- n_distinct(data$`Q3...1`)

response_counts <- response_df %>%
  filter(!is.na(option)) %>%
  group_by(option) %>%
  summarise(frequency = n()) %>%
  mutate(percentage = (frequency / 337) * 100,  
         label = paste("n =", frequency)) %>%  
  arrange(desc(frequency))

popts <- ggplot(response_counts, aes(x = fct_reorder(option, percentage), y = percentage, fill = option, label = label)) + 
  geom_bar(stat = "identity", color = "black", fill = "gray") +  
  geom_text(aes(label = label), size = 3.5, hjust = -0.2) +  
  labs(x = "Response Option", y = "Percentage (%)") +  
  ggtitle("Percentage of Participants Selecting Each Management Option") +  
  theme_minimal(base_family = "Arial") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none",  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100))

popts <- popts + scale_x_discrete(labels = label_wrap_gen(width = 25))

#### save as .svg ###
ggsave("popts2.svg", popts, device = "svg", width = 12, height = 8)

### different options for RM by GA - graphs and summary statistics ###
yes_data <- data %>% filter(Q132 == "Yes")
no_data <- data %>% filter(Q132 == "No")

process_responses <- function(data_subset, group_label) {
  responses <- pull(data_subset, "Q42")
  split_responses <- str_split(responses, ",")
  all_responses <- unlist(split_responses)
  response_df <- data.frame(option = all_responses)
  
  total_population <- n_distinct(data_subset$`Q3...1`)
  
  response_counts <- response_df %>%
    filter(!is.na(option)) %>%
    group_by(option) %>%
    summarise(frequency = n()) %>%
    mutate(
      percentage = (frequency / total_population) * 100,  
      label = paste("n =", frequency),  
      group = group_label  
    ) %>%
    arrange(desc(frequency))
  
  return(response_counts)
}

yes_response_counts <- process_responses(yes_data, "Yes")
no_response_counts <- process_responses(no_data, "No")

combined_response_counts <- bind_rows(yes_response_counts, no_response_counts)

combined_response_counts$group <- factor(combined_response_counts$group, levels = c("Yes", "No"))

combined_response_counts$group <- factor(combined_response_counts$group, levels = c("No", "Yes"))

poptsGA <- ggplot(combined_response_counts, aes(x = fct_reorder(option, percentage), y = percentage, fill = group, label = label)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Use dodge to place bars side by side
  geom_text(aes(label = label), size = 3.5, position = position_dodge(width = 0.9), hjust = -0.1) +  # Adjust label position
  labs(x = "Response Option", y = "Percentage (%)", fill = "Attended genetics appointment?") +  
  ggtitle("Percentage of Participants Selecting Each Management Option \nby Attendance at Genetics Appointment") +  
  theme_minimal(base_family = "Arial") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c("Yes" = "lightblue", "No" = "palegreen")) +  
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  scale_x_discrete(labels = label_wrap_gen(width = 25)) +
  guides(fill = guide_legend(reverse = TRUE))  

#### save as .svg ###
ggsave("poptsGA.svg", poptsGA, device = "svg", width = 12, height = 8)

### Q41 thematic analysis + graph ###
na_count_all <- sum(is.na(data$Q41))

categories <- c("Healthcare", "Self-research", "Family and Friends", "Media or Social Media", 
                "Education or Career", "Word of Mouth", "Haven't Accessed Information")
counts <- c(150, 85, 91, 5, 8, 3, 91)

percentages <- (counts / 337) * 100

df <- data.frame(Category = categories, Percentage = percentages, Count = counts)

pinfo <- ggplot(df, aes(x = reorder(Category, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  geom_text(aes(label = paste0("n=", Count)), vjust = -0.5, size = 3.5) +  
  labs(x = "Soutce of information", y = "Percentage (%)") +
  ggtitle("Sources of information about risk management options") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) as.integer(x), limits = c(0, 100)) +
  scale_x_discrete(labels = label_wrap_gen(width = 25))

#### save as .svg ###
ggsave("pinfo.svg", pinfo, device = "svg", width = 10, height = 6)

### Thoughts about ABS ###
counts_Q4511<- data %>%
  filter(!is.na(`Q45#1_1`)) %>%
  count(`Q45#1_1`) %>%
  mutate(percentage_Q4511 = (n / 337) * 100)

desired_order <- c('Yes', 'No')
counts_Q4511$`Q45#1_1` <- factor(counts_Q4511$`Q45#1_1`, levels = desired_order)

pabs1 <- ggplot(counts_Q4511, aes(x = `Q45#1_1`, y = percentage_Q4511)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Have participants thought \nabout annual breast screening?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  


counts_Q4521<- data %>%
  filter(!is.na(`Q45#2_1`)) %>%
  count(`Q45#2_1`) %>%
  mutate(percentage_Q4521 = (n / 337) * 100)

desired_order <- c("Yes", "No", "Don't Know")
counts_Q4521$`Q45#2_1` <- factor(counts_Q4521$`Q45#2_1`, levels = desired_order)

pabs2 <- ggplot(counts_Q4521, aes(x = `Q45#2_1`, y = percentage_Q4521)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Would participants consider \nhaving annual breast screening?") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  


pabs <- grid.arrange(pabs1, pabs2, nrow = 1, ncol = 2)

counts_Q4531<- data %>%
  filter(!is.na(`Q45#3_1`)) %>%
  count(`Q45#3_1`) %>%
  mutate(percentage_Q4531 = (n / 337) * 100)

desired_order <- c("I have already done this and plan on continuing screening",
                   "As soon as possible. I am seeking an appointment, or have one booked.",
                   "In the next 6 months",
                   "In the future, but not in the next 6 months",
                   "I'm not sure")
counts_Q4531$`Q45#3_1` <- factor(counts_Q4531$`Q45#3_1`, levels = desired_order)


pabs3 <- ggplot(counts_Q4531, aes(x = `Q45#3_1`, y = percentage_Q4531)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("When would participants consider starting annual breast screening?") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)


pabs3 <- pabs3 + scale_x_discrete(labels = label_wrap_gen(width = 25))

pabsO <- grid.arrange(pabs, pabs3, nrow = 2, ncol = 1)


#### save as .svg ###
ggsave("pabs.svg", pabsO, device = "svg", width = 10, height = 8)

### Thoughts on CP ###
counts_Q12011<- data %>%
  filter(!is.na(`Q120#1_1`)) %>%
  count(`Q120#1_1`) %>%
  mutate(percentage_Q12011 = (n / 337) * 100)

desired_order <- c('Yes', 'No')
counts_Q12011$`Q120#1_1` <- factor(counts_Q12011$`Q120#1_1`, levels = desired_order)

pcp1 <- ggplot(counts_Q12011, aes(x = `Q120#1_1`, y = percentage_Q12011)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Have participants thought \nabout chemoprevention?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  


counts_Q12021<- data %>%
  filter(!is.na(`Q120#2_1`)) %>%
  count(`Q120#2_1`) %>%
  mutate(percentage_Q12021 = (n / 337) * 100)

desired_order <- c("Yes", "No", "Don't Know")
counts_Q12021$`Q120#2_1` <- factor(counts_Q12021$`Q120#2_1`, levels = desired_order)

pcp2 <- ggplot(counts_Q12021, aes(x = `Q120#2_1`, y = percentage_Q12021)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Would participants consider \nusing chemoprevention?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  


pcp <- grid.arrange(pcp1, pcp2, nrow = 1, ncol = 2)

counts_Q12031<- data %>%
  filter(!is.na(`Q120#3_1`)) %>%
  count(`Q120#3_1`) %>%
  mutate(percentage_Q12031 = (n / 337) * 100)

desired_order <- c("I have already done this but would not do it again",
                   "I have already done this and plan on continuing",
                   "As soon as possible. I am seeking an appointment, or have one booked to discuss this.",
                   "In the next 6 months",
                   "In the future, but not in the next 6 months",
                   "I'm not sure")
counts_Q12031$`Q120#3_1` <- factor(counts_Q12031$`Q120#3_1`, levels = desired_order)


pcp3 <- ggplot(counts_Q12031, aes(x = `Q120#3_1`, y = percentage_Q12031)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("When would participants consider starting chemoprevention?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)


pcp3 <- pcp3 + scale_x_discrete(labels = label_wrap_gen(width = 25))

pcpO <- grid.arrange(pcp, pcp3, nrow = 2, ncol = 1)

#### save as .svg ###
ggsave("pcp.svg", pcpO, device = "svg", width = 10, height = 8)

### Thoughts on RRBS ###
counts_Q4411<- data %>%
  filter(!is.na(`Q44#1_1`)) %>%
  count(`Q44#1_1`) %>%
  mutate(percentage_Q4411 = (n / 337) * 100)

desired_order <- c('Yes', 'No')
counts_Q4411$`Q44#1_1` <- factor(counts_Q4411$`Q44#1_1`, levels = desired_order)

prbs1 <- ggplot(counts_Q4411, aes(x = `Q44#1_1`, y = percentage_Q4411)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Have participants thought \nabout risk reducing breast surgery?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  

counts_Q4421<- data %>%
  filter(!is.na(`Q44#2_1`)) %>%
  count(`Q44#2_1`) %>%
  mutate(percentage_Q4421 = (n / 337) * 100)

desired_order <- c("Yes", "No", "Don't Know")
counts_Q4421$`Q44#2_1` <- factor(counts_Q4421$`Q44#2_1`, levels = desired_order)

prbs2 <- ggplot(counts_Q4421, aes(x = `Q44#2_1`, y = percentage_Q4421)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Would participants consider \nhaving risk reducing breast surgery?") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  


prbs <- grid.arrange(prbs1, prbs2, nrow = 1, ncol = 2)

counts_Q4431<- data %>%
  filter(!is.na(`Q44#3_1`)) %>%
  count(`Q44#3_1`) %>%
  mutate(percentage_Q4431 = (n / 337) * 100)

desired_order <- c("I have already done this",
                   "As soon as possible. I am seeking an appointment, or have one booked.",
                   "In the next 6 months",
                   "In the future, but not in the next 6 months",
                   "I'm not sure")
counts_Q4431$`Q44#3_1` <- factor(counts_Q4431$`Q44#3_1`, levels = desired_order)


prbs3 <- ggplot(counts_Q4431, aes(x = `Q44#3_1`, y = percentage_Q4431)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("When would participants consider having risk reducing breast surgery?") +
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)


prbs3 <- prbs3 + scale_x_discrete(labels = label_wrap_gen(width = 25))

prbsO <- grid.arrange(prbs, prbs3, nrow = 2, ncol = 1)

#### save as .svg ###
ggsave("prbs.svg", prbsO, device = "svg", width = 10, height = 8)

### Thoughts on RROS ###
counts_Q4412 <- data %>%
  filter(Gene == "High") %>%  
  filter(!is.na(`Q44#1_2`)) %>% 
  count(`Q44#1_2`) %>%
  mutate(percentage_Q4412 = (n / nrow(data %>% filter(Gene == "High"))) * 100)

desired_order <- c('Yes', 'No')
counts_Q4412$`Q44#1_2` <- factor(counts_Q4412$`Q44#1_2`, levels = desired_order)

pros1 <- ggplot(counts_Q4412, aes(x = `Q44#1_2`, y = percentage_Q4412)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Have participants thought \nabout risk reducing ovarian surgery?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  

counts_Q4422 <- data %>%
  filter(Gene == "High") %>%
  filter(!is.na(`Q44#2_2`)) %>%
  count(`Q44#2_2`) %>%
  mutate(percentage_Q4422 = (n / nrow(data %>% filter(Gene == "High"))) * 100)  

desired_order <- c("Yes", "No", "Don't Know")
counts_Q4422$`Q44#2_2` <- factor(counts_Q4422$`Q44#2_2`, levels = desired_order)

pros2 <- ggplot(counts_Q4422, aes(x = `Q44#2_2`, y = percentage_Q4422)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("Would participants consider \nhaving risk reducing ovarian surgery?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)  

pros <- grid.arrange(pros1, pros2, nrow = 1, ncol = 2)

counts_Q4432 <- data %>%
  filter(Gene == "High") %>%
  filter(!is.na(`Q44#3_2`)) %>%
  count(`Q44#3_2`) %>%
  mutate(percentage_Q4432 = (n / nrow(data %>% filter(Gene == "High"))) * 100) 

desired_order <- c("I have already done this",
                   "As soon as possible. I am seeking an appointment, or have one booked.",
                   "In the next 6 months",
                   "In the future, but not in the next 6 months",
                   "I'm not sure")
counts_Q4432$`Q44#3_2` <- factor(counts_Q4432$`Q44#3_2`, levels = desired_order)

pros3 <- ggplot(counts_Q4432, aes(x = `Q44#3_2`, y = percentage_Q4432)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab("Response") +
  ylab("Percentage (%)") +
  ggtitle("When would participants consider having risk reducing ovarian surgery?") +
  theme_minimal(base_family = "Arial") +  # Set Arial font
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(as.integer(x)), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5) +
  scale_x_discrete(labels = label_wrap_gen(width = 25))

# Arrange all plots in a grid
pro <- grid.arrange(pros, pros3, nrow = 2, ncol = 1)

#### save as .svg ###
ggsave("pro.svg", pro, device = "svg", width = 10, height = 8)

### q46 thematic analysis + graphs ###
categories <- c("Waiting to complete family", "Don't feel ready", "Fear of procedure", "Impact of procedure", 
                "Other health concerns", "Lack of information", "Educational, career or financial reasons", "Family responsibilities",
                "Waiting for genetic test resutls", "Not sure", "Too old", "Too young", "Waiting for menopause")
counts <- c(57, 20, 2, 7, 4, 36, 7, 10, 63, 17, 2, 45, 7) 

percentages <- (counts / 337) * 100

df <- data.frame(Category = categories, Percentage = percentages, Count = counts)

df <- df[order(-df$Percentage), ]


pwait <- ggplot(df, aes(x = reorder(Category, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  geom_text(aes(label = paste0("n=", Count)), vjust = -0.5, size = 3.5) +  # Add count labels
  labs(x = "Reason", y = "Percentage (%)") +
  ggtitle("Reasons for considering risk management options beyond the next six months") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate labels for better readability
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) as.integer(x), limits = c(0, 100)) +
  scale_x_discrete(labels = label_wrap_gen(width = 25))


#### save as .svg ###
ggsave("wait.svg", pwait, device = "svg", width = 10, height = 6)


### Beneficial/Harmful ###

data_long <- data.frame(
  Participant = 1:nrow(data),
  Q47_sum = data$Q47_1,
  Q121_sum = data$Q121_1,
  Q48_sum = data$Q48_1,
  Q49_sum = data$Q49_1
)

data_long <- pivot_longer(
  data_long, 
  cols = c(Q47_sum, Q121_sum, Q48_sum, Q49_sum),
  names_to = "Question",
  values_to = "Sum"
)


data_long$Question <- recode(data_long$Question, 
                             Q121_sum = "Chemoprevention",
                             Q47_sum = "Annual Breast Screening",
                             Q48_sum = "Risk Reducing Breast Surgery",
                             Q49_sum = "Risk Reducing Ovarian Surgery")


p1 <- ggplot(data_long, aes(x = Question, y = Sum)) +
  geom_boxplot() +
  labs(title = "Participant Attitudes Towards Management Options\n(where 1 = Beneficial and 10 = Harmful)", 
       x = "Option", 
       y = "Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Arial"),  # Set font to Arial
        plot.title = element_text(hjust = 0.5, face = "bold"))


### Important/Unimportant ###

data_long <- data.frame(
  Participant = 1:nrow(data),
  Q47_sum = data$Q47_2,
  Q121_sum = data$Q121_2,
  Q48_sum = data$Q48_2,
  Q49_sum = data$Q49_2
)

data_long <- pivot_longer(
  data_long, 
  cols = c(Q47_sum, Q121_sum, Q48_sum, Q49_sum),
  names_to = "Question",
  values_to = "Sum"
)


data_long$Question <- recode(data_long$Question, 
                             Q121_sum = "Chemoprevention",
                             Q47_sum = "Annual Breast Screening",
                             Q48_sum = "Risk Reducing Breast Surgery",
                             Q49_sum = "Risk Reducing Ovarian Surgery")


p2 <- ggplot(data_long, aes(x = Question, y = Sum)) +
  geom_boxplot() +
  labs(title = "Participant Attitudes Towards Management Options\n(where 1 = Important and 10 = Unimportant)", 
       x = "Option", 
       y = "Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Arial"),  # Set font to Arial
        plot.title = element_text(hjust = 0.5, face = "bold"))


### Good thing/Bad thing ###

data_long <- data.frame(
  Participant = 1:nrow(data),
  Q47_sum = data$Q47_3,
  Q121_sum = data$Q121_3,
  Q48_sum = data$Q48_3,
  Q49_sum = data$Q49_3
)

data_long <- pivot_longer(
  data_long, 
  cols = c(Q47_sum, Q121_sum, Q48_sum, Q49_sum),
  names_to = "Question",
  values_to = "Sum"
)


data_long$Question <- recode(data_long$Question, 
                             Q121_sum = "Chemoprevention",
                             Q47_sum = "Annual Breast Screening",
                             Q48_sum = "Risk Reducing Breast Surgery",
                             Q49_sum = "Risk Reducing Ovarian Surgery")


p3 <- ggplot(data_long, aes(x = Question, y = Sum)) +
  geom_boxplot() +
  labs(title = "Participant Attitudes Towards Management Options\n(where 1 = Good thing and 10 = Bad thing)", 
       x = "Option", 
       y = "Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Arial"),  # Set font to Arial
        plot.title = element_text(hjust = 0.5, face = "bold"))



### Pleasant/Unpleasant ###

data_long <- data.frame(
  Participant = 1:nrow(data),
  Q47_sum = data$Q47_4,
  Q121_sum = data$Q121_4,
  Q48_sum = data$Q48_4,
  Q49_sum = data$Q49_4
)

data_long <- pivot_longer(
  data_long, 
  cols = c(Q47_sum, Q121_sum, Q48_sum, Q49_sum),
  names_to = "Question",
  values_to = "Sum"
)


data_long$Question <- recode(data_long$Question, 
                             Q121_sum = "Chemoprevention",
                             Q47_sum = "Annual Breast Screening",
                             Q48_sum = "Risk Reducing Breast Surgery",
                             Q49_sum = "Risk Reducing Ovarian Surgery")


p4 <- ggplot(data_long, aes(x = Question, y = Sum)) +
  geom_boxplot() +
  labs(title = "Participant Attitudes Towards Management Options\n(where 1 = Pleasant and 10 = Unplesant)", 
       x = "Option", 
       y = "Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Arial"),  # Set font to Arial
        plot.title = element_text(hjust = 0.5, face = "bold"))



pattitude <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

#### save as .svg ###
ggsave("attitude1.svg", pattitude, device = "svg", width = 10, height = 8)

#### q50 thematic analysis ####
categories <- c("Lack of information", "Family history and experience of cancer", "Age", "Self-research", 
                "Impact of surgery", "Impact of chemoprevention", "Desire to reduce cancer risk", "Wanting to complete family", 
                "Impact on family", "Personal preference", "To prolong life", "Concerns about screening", "Level of invasiveness",
                "Insufficent risk")


counts <- c(60, 52, 15, 11, 60, 18, 45, 6, 10, 11, 10, 10, 14, 5)

percentages <- (counts / 337) * 100

df <- data.frame(Category = categories, Percentage = percentages, Count = counts)

df <- df[order(-df$Percentage), ]

pattitude <- ggplot(df, aes(x = reorder(Category, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  geom_text(aes(label = paste0("n=", Count)), vjust = -0.5, size = 3.5) +  # Add count labels
  labs(x = "Reason", y = "Percentage (%)") +
  ggtitle("Factors that impact participant attitudes towards \nrisk management options") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate labels for better readability
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = function(x) as.integer(x), limits = c(0, 100)) +
  scale_x_discrete(labels = label_wrap_gen(width = 25))

#### save as .svg ###
ggsave("effect.svg", pattitude, device = "svg", width = 10, height = 6)
