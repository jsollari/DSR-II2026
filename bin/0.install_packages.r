#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 11.03.2026

pack0 = c(
  "tidyverse",      #collection of packages for "tidyverse"
  "rpart",          #package for decision tree models
  "rpart.plot",     #package for plotting decision tree models
  "nnet",           #package for neural network models
  "NeuralNetTools"  #package for tools to neural network models
)

pack1 = c(
  "nycflights13",   #dataset nycflights13
  "skimr"           #package for descriptive statistics
)
pack2 = c(
  "ggbeeswarm",     #package for "bee swarm" plots
  "gridExtra",      #package for extra features of ggplot2
  "hexbin",         #package for "hexagonal binning" plots
  "lvplot",         #package for "letter value" plots
  "tidymodels"      #collection of of packages for "tidymodels"
)
pack3 = c(
  "dotwhisker",     #package for "dot whisker" plots
  "GGally",         #package for quick data visualization
  "glmnet",         #package for GLMs with penalized maxL
  "ranger",         #package for random forest models
  "vip"             #package for "variable importance" plots for ML models
)

install.packages(c(pack0, pack1,pack2,pack3), repos = "http://mirror.ibcp.fr/pub/CRAN/")
