library(tidyverse)

## Importing Data --------------------------------
pop_fert <- read.csv("promotional-bootcamp/data/popdata.csv")
read.csv("promotional-bootcamp/data/popdata2.csv", skip = 5)
readxl::read_excel("promotional-bootcamp/data/ant.xlsx")


## Understanding our Data -----------------------------
### Data Preview ------------------------
head(pop_fert)
tail(pop_fert)
car::some(pop_fert)

dim(pop_fert) # dimension
str(pop_fert) # structure

summary(pop_fert) # summary

unique(pop_fert$clone)
unique(pop_fert$fert)
unique(pop_fert$block)
pop_fert

pop_fert$block <- factor(pop_fert$block)
pop_fert$fert <- factor(pop_fert$fert)
pop_fert$clone <- factor(pop_fert$clone)

str(pop_fert)
## Descriptive Statistics ------------------------
min(pop_fert$cutw)
max(pop_fert$cutw)

quantile(pop_fert$cutw)
IQR(pop_fert$cutw)
range(pop_fert$cutw)

### Measure of Central Tendency--------------------------
mean(pop_fert$cutw)
median(pop_fert$cutw)

### Graphs and Visualization
plot(pop_fert)

plot(
  pop_fert$fert,
  xlab = "Fertilizer",
  ylab = "Count",
  ylim = c(0, 100)
)

plot(pop_fert$height)
plot(pop_fert$height)
plot(pop_fert$clone,pop_fert$height)
plot(pop_fert$dia, pop_fert$height)

# Packages -------------------------------------------
ggplot(pop_fert, aes(fert, height, fill = clone)) +
  geom_boxplot(position = "dodge") +
  facet_wrap(~block) +
  scale_fill_viridis_d()

# Statistical Inference
pop_fert <- pop_fert |> 
  as_tibble() |> 
  mutate(
    hd_ratio = height/dia
  )


pop_fert |> 
  summarize(
    .by = c(block, clone, fert),
    mean_height = mean(height),
    mean_hd_ratio = mean(hd_ratio),
    mean_dia = mean(dia)
  ) |> 
  print(n = 100) |> 
  ggplot(aes(clone, mean_hd_ratio, fill = fert)) +
  geom_boxplot()

## T-test
t.test(hd_ratio ~ fert, data = pop_fert)
aov(hd_ratio ~ fert, data = pop_fert)
lm(hd_ratio ~ fert , data = pop_fert)
