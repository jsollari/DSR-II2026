#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 18.11.2025

# 0. INDEX
{
# 1. TRANSFORM
# 1.1. PACKAGE dplyr
# 2. EXPLORATION
# 2.1. PACKAGE ggplot2
# 3. MODELLING
# 3.1. PACKAGE stats
# 3.2. PACKAGE performance
# 3.3. MODELS

}
# 1. TRANSFORM
{
library("tidyverse")

## 1.1. PACKAGE dplyr
?diamonds

diamonds |>                         #use data "diamonds"
  select(price, carat, cut) |>      #select "price", "carat" and "cut"
  filter(carat < 3) |>              #filter for smaller diamonds
  mutate(lprice = log10(price)) |>  #create variable "lprice"
  group_by(cut) |>                  #group by "cut"
  summarize(
    mean_lprice = mean(lprice),     #calculate mean of "lprice"
    mean_carat = mean(carat)        #calculate mean of "carat"
  ) |>
  arrange(desc(mean_lprice))        #arrange by "mean_lprice"

}
# 2. EXPLORATION
{
## 2.1. PACKAGE ggplot2
set.seed(1984)
f1 <- "media/fig2_ggplot2.png"
p1 <- diamonds |>                             #use data "diamonds"
  filter(carat < 3) |>                        #filter for smaller diamonds
  slice_sample(n=500, by=cut) |>              #sample for 500 obs per cut
  ggplot(aes(x=carat, y=price, color=cut)) +  #aesthetics mapping
  geom_point(alpha=0.1, size=1) +             #geometric object
  stat_smooth(                                #statistical transformation
    method="lm",
    formula="y ~ x + I(x^2) + I(x^3)",
    se=FALSE) +
  scale_x_continuous(trans="log10") +         #scale for x-axis
  scale_y_continuous(trans="log10") +         #scale for y-axis
  labs(x="Weight", y="Price", color="Cut") +  #scale for labels
  theme_minimal()                                   #change theme
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

}
# 3. MODELLING
{
library("performance")
library("see")
library("gridExtra")
library("rpart")
library("nnet")
library("NeuralNetTools")

## 3.1. PACKAGE stats
set.seed(1984)
diamonds2 <- diamonds |>                      #use data "diamonds"
  filter(
    (x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10), #filter out outliers
    carat < 3) |>                             #filter out big diamonds
  slice_sample(n=1000) |>                     #sample for 1000 observations
  select(price, carat, cut) |>                #select "price", "carat" and "cut"
  mutate(
    lprice = log10(price),                    #create variable "lprice"
    lcarat = log10(carat),                    #create variable "lcarat"
    fct_cut = factor(cut, ordered=FALSE))     #make variable "cut" into factor

mod1 <- formula("lprice ~ lcarat + fct_cut")  #specify model
res1 <- lm(mod1, data=diamonds2)              #fit model to data

summary(res1)                                 #get summary

## 3.2. PACKAGE performance
f1 <- "media/fig3_assumptions.png"
png(f1, width=17.2, height=11.3, units="cm", res=72, type="cairo")
check_model(res1, check=c(
  #MLR.1 The population model is linear in the parameters
  "linearity",
  #MLR.3 No multicollinearity between predictors
  "vif",
  #MLR.5 The error has constant variance given any values of the parameters
  "homogeneity",
  #MLR.6 The error is independent of the predictors and is normally distributed
  "qq"              
))
dev.off()

## 3.3. MODELS

#simple linear model
set.seed(1984)
beta0 <- -1.6
beta1 <- 0.03
tb_lm <- tibble(
  x = runif(20, min=18, max=60),
  y = beta0 + beta1*x + rnorm(20, mean=0, sd=0.1)
)

res_lm <- lm(y ~ x, data=tb_lm)

f1 <- "media/fig4_slr.png"
p1 <- tb_lm |>
  mutate(
    preds = predict(res_lm),
    resids = residuals(res_lm)
  ) |>
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  stat_smooth(method="lm", formula="y ~ x", se=FALSE, color="blue") +
  geom_segment(aes(xend=x, yend=preds), color="red")
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

#multiple linear model
set.seed(1984)
beta0 <- -1.6
beta1 <- 0.03
beta2 <- -1.0
tb_lm2 <- tibble(
  x1 = runif(20, min=18, max=60),
  x2 = rbinom(20, size=1, prob=0.5),
  y = beta0 + beta1*x1 + beta2*x2 + rnorm(20, mean=0, sd=0.1)
) |>
  mutate(
    x2 = factor(x2)
)

res_lm2 <- lm(y ~ x1 + x2, data=tb_lm2)
v1 <- res_lm2$coef

f1 <- "media/fig5_mlr.png"
p1 <- tb_lm2 |>
  mutate(
    preds = predict(res_lm2),
    resids = residuals(res_lm2)
  ) |>
  ggplot(aes(x=x1, y=y, color=x2)) +
  geom_point() +
  geom_abline(slope=v1[2], intercept=v1[1], color="#F8766D") +
  geom_abline(slope=v1[2], intercept=sum(v1[c(1, 3)]), color="#00BFC4") +
  geom_segment(aes(xend=x1, yend=preds), color="red")
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

#logistic regression model
set.seed(1984)
beta0 <- -1.6
beta1 <- 0.03
tb_glm <- tibble(
  x = runif(20, min=18, max=60),
  z = beta0 + beta1*x,
  pi_y = exp(z)/(1 + exp(z)),
  y = rbinom(20, size=1, prob=pi_y)
)

res_glm <- glm(y ~ x, binomial(link='logit'), data=tb_glm)

f1 <- "media/fig6_glm.png"
p1 <- tb_glm |>
  mutate(
    preds = predict(res_glm, type="resp"),
    resids = residuals(res_glm)
  ) |> 
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  stat_smooth(method="glm", formula="y ~ x", se=FALSE, color="blue",
    method.args = list(family="binomial")) +
  geom_segment(aes(xend=x, yend=preds), color="red") +
  geom_hline(yintercept=c(0.4, 0.5, 0.6), linetype="dashed", color="gray")
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

#ml: data
set.seed(1984)
tb_ml <- tibble(
  x1 = ceiling(runif(20, min=0, max=4)),    #x1 = {1, 2, 3, 4}
  x2 = ceiling(runif(20, min=0, max=2)),    #x2 = {1, 2}
  x3 = ceiling(runif(20, min=0, max=2)),    #x3 = {1, 2}
  x = x1 - (x2 + x3),                       #x = [-3, 2]
  pi_y = exp(x)/(1 + exp(x)),               #pi_y = [0, 1]
  y = factor(rbinom(20, size=1, prob=pi_y)) #y = {0, 1}
)

f1 <- "media/fig7_lm_data.png"
p1 <- tb_ml |>
  ggplot(aes(x=x1, y=y)) +
  geom_jitter(height=0.05, width=0.05, alpha=0.5) +
  facet_grid(x3 ~ x2, labeller = "label_both")
p2 <- tb_ml |>
  ggplot(aes(x=pi_y, y=y)) +
  geom_jitter(height=0.05, width=0.05, alpha=0.5) +
  geom_vline(xintercept=c(0, 0.5, 1), linetype=3, linewidth=0.5)
p3 <- arrangeGrob(p1, p2, nrow = 1)
ggsave(f1, p3, "png", width=21.0, height=11.3, units="cm", dpi=72)

#ml: decision tree
set.seed(1984)
res_rf <- rpart(y ~ x1 + x2 + x3, data = tb_ml, method = "class",
  control = rpart.control(minsplit = 5)
)
table(predict(res_rf, type="class"), tb_ml$y)

f1 <- "media/fig8_lm_rf.png"
png(f1, width = 17.2, height = 11.3, units = "cm", res = 72, type = "cairo")
plot(res_rf, branch = 0, margin = 0.02, branch.lwd = 2, branch.col = "blue")
text(res_rf, minlength = 2, use.n = TRUE, fancy = TRUE, fwidth = 1.5,
  fheight = 1, cex = 0.8, font = 2)
dev.off()

#ml: neural networks
set.seed(1984)
res_nnet <- nnet(y ~ x1 + x2 + x3, data = tb_ml, size = 1, trace = FALSE)
table(predict(res_nnet, type="class"), tb_ml$y)

f1 <- "media/fig9_lm_nnet.png"
png(f1, width = 17.2, height = 11.3, units = "cm", res = 72, type = "cairo")
plotnet(res_nnet, pos_col="blue", neg_col="red")
dev.off()

}
