

# Anaylsis of Covariance --------------------------------------------------

# Loading Packages
library(pacman)
p_load(tidyverse, rstatix, readxl, gridExtra, emmeans, car)

# Importing data
monofilament <- read_excel("D:/Documents/R-Studio Programms/Design/covariates.xlsx")
head(monofilament)

# Summary statictics for the dependent variable strength
monofilament |>
  group_by(Machine) |>
  get_summary_stats(Strength, type = "common")

#Summary statictics for independent variable diameter

monofilament |>
  group_by(Machine) |>
  get_summary_stats(Diameter, type = "common")

#Visualiazing the Data set
p1 <- ggplot(monofilament, aes(Diameter, Strength, colour = Machine)) +
  geom_point(size = 3) +
  theme(legend.position = "top") +
  theme_minimal()
p1
# Perform the Anaylsis of Covariance (One Way Classification - ANCOVA)
ancov <- anova_test(data = monofilament, formula = Strength ~  Diameter + Machine, 
                    type = 3, detailed = TRUE) # type 3 SS should be used in ANCOVA
get_anova_table(ancov)

# Adjusted Means
adjMeans <- emmeans_test(data = monofilament, formula = Strength ~ Machine,
                         covariate = Diameter)
get_emmeans(adjMeans)


# Nested Design -----------------------------------------------------------
ND <- read_excel("D:/Documents/R-Studio Programms/Design/ND.xlsx")

# The Model
Process <- as.fixed(ND$Process)
Batch <- as.random(ND$Batch)
burningRate <- ND$burningRate  
data_aov1 <- aov(burningRate ~ Process + Process : Batch)
anova(data_aov1)
# Another Nested

ND2 <- read_excel("D:/Documents/R-Studio Programms/Design/ND2.xlsx")

Sites <- as.random(ND2$Sites)
Batches <- as.random(ND2$Batches)
Tablets <- ND2$Tablets
data_aov <- aov(Tables ~ Sites + Sites:Batches)  

# Balanced Incomplete Block Design ----------------------------------------



# Partially Balanced Incomplete Block Design ---------------------------------------
p_load(PBIBD)

v = 9; r = 4; k = 3;l=c(2,1);n=c(4,4)

P1 <- matrix(c(1,2,2,2),nrow = 2)
P2 <- matrix(c(2,2,2,1), nrow = 2)
P <- list(P1,P2)
apbibd(v,r,k,l,n,P)
