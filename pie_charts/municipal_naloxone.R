library(dplyr)
library(tidyr)
library(ggplot2)
library(rempsyc)


# Load the data
data <- read.csv('/Users/ethan/Documents/PHP1810/harm_reduction_project/data/Municipal_Count_of_Naloxone_Kits_Distributed_by_Year_(All_Sources).csv')

# Define the list of county names
providence_county <- c("Central_Falls", "Cranston", "East_Providence", "Foster", "Johnston", "Lincoln", "North_Providence", "Pawtucket", "Providence", "Scituate", "Smithfield", "Woonsocket", "Burrillville", "Glocester", "North_Smithfield", "Cumberland")
population <- c(22192,82654,47171,4505,29550,22415,33935,75200,188812,10413,21855,43044,16186,10007,12537,36186)


# Filter the data for 2022 and select only the columns in providence_county
providence_overdose <- data %>% 
  filter(Year == 2022) %>% 
  select(one_of(providence_county))

# Convert to long format and convert Overdoses to numeric
providence_overdose_long <- providence_overdose %>% 
  gather(key = "Municipality", value = "Overdoses") %>%
  mutate(Overdoses = as.numeric(Overdoses))

providence_overdose_table = data.frame(providence_overdose_long,population, rate = (providence_overdose_long$Overdoses*10000)/population)


# Create a bar chart with rotated x-axis labels
ggplot(providence_overdose_table, aes(x = Municipality, y = rate, fill = Municipality)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Naloxone Kits Distributed per 10,000 people in Providence County (2022)", x = "Municipality", y = "Number of Naloxone Kits Distributed") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

