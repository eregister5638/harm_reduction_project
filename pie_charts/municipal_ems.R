library(dplyr)
library(tidyr)
library(ggplot2)


# Load the data
data <- read.csv('/Users/ethan/Documents/PHP1810/harm_reduction_project/data/Municipal_Count_of_Opioid_Overdose_Related_Emergency_Medical_Services_Runs_by_Year.csv')

# Define the list of county names
providence_county <- c("Central_Falls", "Cranston", "East_Providence", "Foster", "Johnston", "Lincoln", "North_Providence", "Pawtucket", "Providence", "Scituate", "Smithfield", "Woonsocket", "Burrillville", "Glocester", "North_Smithfield", "Cumberland")

# Filter the data for 2022 and select only the columns in providence_county
providence_overdose <- data %>% 
  filter(Year == 2022) %>% 
  select(one_of(providence_county))

# Convert to long format and convert Overdoses to numeric
providence_overdose_long <- providence_overdose %>% 
  gather(key = "Municipality", value = "Overdoses") %>%
  mutate(Overdoses = as.numeric(Overdoses))


# Create a bar chart with rotated x-axis labels
ggplot(providence_overdose_long, aes(x = Municipality, y = Overdoses, fill = Municipality)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Providence County Municipal Count of Opiod-Related EMS Runs (2022)", x = "Municipality", y = "Number of EMS Runs") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
