#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 16.03.2026

# 0. INDEX
{
# 1. TRANSFORM
# 1.1. PACKAGE dplyr
# 2. EXPLORATION
# 2.1. PACKAGE ggplot2
# 3. MODELLING
# 3.1. SIMPLE LINEAR REGRESSION
# 3.2. MULTIPLE LINEAR REGRESSION
# 3.3. LOGISTIC REGRESSION MODEL
# 3.4. MACHINE LEARNING MODELS
  
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
library("tidyverse")

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
  theme_minimal()                             #change theme
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

}
# 3. MODELLING
{
library("tidyverse")
library("rpart")
library("rpart.plot")
library("nnet")
library("NeuralNetTools")

## 3.1. SIMPLE LINEAR REGRESSION
set.seed(1984)
beta0 <- -1.6
beta1 <- 0.03
tb_lm <- tibble(
  x = runif(20, min=18, max=60),
  y = beta0 + beta1*x + rnorm(20, mean=0, sd=0.1)
)

res_lm <- lm(y ~ x, data=tb_lm)

f1 <- "media/fig3_slr.png"
p1 <- tb_lm |>
  mutate(
    preds = predict(res_lm),
    resids = residuals(res_lm)
  ) |>
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  stat_smooth(method="lm", formula="y ~ x", se=FALSE, color="blue") +
  geom_segment(aes(xend=x, yend=preds), color="red") +
  theme_minimal()
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

## 3.2. MULTIPLE LINEAR REGRESSION
set.seed(1984)
beta0 <- -1.6
beta1 <- 0.03
beta2 <- -1.0
tb_lm2 <- tibble(
  x1 = runif(20, min=18, max=60),
  x2 = rbinom(20, size=1, prob=0.5),
  y = beta0 + beta1*x1 + beta2*x2 + rnorm(20, mean=0, sd=0.1)
)

res_lm2 <- lm(y ~ x1 + x2, data=tb_lm2)
b <- res_lm2$coef

f1 <- "media/fig4_mlr.png"
p1 <- tb_lm2 |>
  mutate(
    x2 = factor(x2),
    preds = predict(res_lm2),
    resids = residuals(res_lm2)
  ) |>
  ggplot(aes(x=x1, y=y, color=x2)) +
  geom_point() +
  geom_abline(slope=b[2], intercept=b[1], color="blue") +
  geom_abline(slope=b[2], intercept=sum(b[c(1, 3)]), color="blue") +
  geom_segment(aes(xend=x1, yend=preds), color="red") +
  theme_minimal()
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

## 3.3. LOGISTIC REGRESSION MODEL
set.seed(1984)
beta0 <- -6
beta1 <- 0.15
tb_glm <- tibble(
  x = runif(20, min=18, max=60),
  z = beta0 + beta1*x,             # z = [-Inf, Inf]
  p_y = exp(z)/(1 + exp(z)),       # p_y = [0, 1]
  y = rbinom(20, size=1, prob=p_y) # y = {0, 1}
)

res_glm <- glm(y ~ x, binomial(link='logit'), data=tb_glm)

f1 <- "media/fig5_glm.png"
p1 <- tb_glm |>
  mutate(
    preds = predict(res_glm, type="resp"),
    class = if_else(preds >= 0.5, "1", "0"),
    resids = residuals(res_glm)
  ) |> 
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  stat_smooth(method="glm", formula="y ~ x", se=FALSE, color="blue",
    method.args = list(family="binomial")) +
  geom_point(aes(y=preds, color=class)) +
  geom_segment(aes(xend=x, yend=preds), color="red") +
  geom_hline(yintercept=c(0.4, 0.5, 0.6), linetype="dashed", color="darkgray") +
# geom_vline(xintercept=-beta0/beta1, linetype="dotted", color="darkgray") +
  labs(color = expression(widehat(y))) +
  theme_minimal()
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

## 3.4. MACHINE LEARNING MODELS

#data
set.seed(1984)
beta0 <- -6
beta1 <- 0.25
beta2 <- -0.1
tb_ml <- tibble(
  x1 = runif(40, min=18, max=60),
  x2 = runif(40, min=18, max=60),
  z = beta0 + beta1*x1 + beta2*x2,
  p_y = exp(z)/(1 + exp(z)),
  y = factor(rbinom(40, size=1, prob=p_y))
)

f1 <- "media/fig6_ml_data.png"
b <- -beta0/beta2 #intercept
m <- -beta1/beta2 #slope
p1 <- tb_ml |>
  ggplot(aes(x=x1, y=x2, color=y)) +
  geom_point() +
  geom_abline(intercept=b, slope=m, linetype="dashed", color="darkgray") + 
  scale_colour_manual(values=c("#fb6a4a", "#84bcdb")) +
  theme_minimal()
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

#decision tree
res_rf <- rpart(
  y ~ x1 + x2, data = tb_ml, method = "class", 
  control = rpart.control(minsplit = 6)
)
table(predict(res_rf, type="class"), tb_ml$y)

f1 <- "media/fig7_ml_rf1.png"
png(f1, width = 17.2, height = 11.3, units = "cm", res = 72, type = "cairo")
rpart.plot(res_rf, extra=101, digits=3, box.palette="RdBu")
dev.off()

f1 <- "media/fig8_ml_rf2.png"
splits <- as_tibble(res_rf$splits) |>
  mutate(diff = count - lead(count)) |>
  filter(diff == 0) |>
  pull(index)
tb_rect <- tibble(
  xmin =  c(     -Inf, splits[2], splits[1], splits[1], splits[4]),
  xmax =  c(splits[1],       Inf, splits[2], splits[4], splits[2]),
  ymin =  c(     -Inf,      -Inf,      -Inf, splits[3], splits[3]),
  ymax =  c(      Inf,       Inf, splits[3],       Inf,       Inf),
  fill =  c(      "0",       "1",       "1",       "0",       "1")
)
tb_segm <- tibble(
  x =    c(splits[1], splits[2], splits[1], splits[4]),
  y =    c(     -Inf,      -Inf, splits[3], splits[3]),
  xend = c(splits[1], splits[2], splits[2], splits[4]),
  yend = c(      Inf,       Inf, splits[3],       Inf)
)
p1 <- tb_ml |>
  ggplot() +
  geom_point(aes(x=x1, y=x2, colour=y)) +
  geom_rect(data=tb_rect, 
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
    color=NA, alpha=0.1) +
  geom_segment(data=tb_segm,
    aes(x=x, y=y, xend=xend, yend=yend),
    linetype = "dashed", color = "darkgray") +
  scale_colour_manual(
    values=c("0" = "#fb6a4a", "1" = "#84bcdb"),
    aesthetics = c("colour", "fill")) +
  labs(fill = expression(widehat(y))) +
  theme_minimal()
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

#neural networks
set.seed(1981)
res_nnet <- nnet(y ~ x1 + x2, data=tb_ml, size=1, trace=FALSE)
table(predict(res_nnet, type="class"), tb_ml$y)

f1 <- "media/fig9_ml_nnet1.png"
png(f1, width = 17.2, height = 11.3, units = "cm", res = 72, type = "cairo")
plotnet(res_nnet, bord_col="black", pos_col="black", neg_col="gray")
text(c(-0.15, 0.65), 0.9, round(res_nnet$wts[c(1, 4)], 1)) #Bias
text(-0.6, c(0.8, 0.2), round(res_nnet$wts[2:3], 1))       #Input
text(0.2, 0.6, round(res_nnet$wts[5], 1))                  #Output
dev.off()

f1 <- "media/fig10_ml_nnet2.png"
C <- sapply(-res_nnet$wts[4]/res_nnet$wts[5], \(y) log(y/(1-y))) - res_nnet$wts[1]
b1 <- res_nnet$wts[2]
b2 <- res_nnet$wts[3]
b <- C/b2  #intercept
m <- -b1/b2 #slope
x1_18 <- (18 - b)/m
x1_60 <- (60 - b)/m
x1min <- min(18, x1_18)
x1max <- max(60, x1_60)
tb_trap <- tibble(
  x1 =   c(x1min, x1_60, x1_18,  x1min, x1_60,  x1max,  x1max, x1_18),
  x2 =   c(   60,    60,    18,     18,    60,     60,     18,    18),
  fill = c(  "0",   "0",   "0",    "0",   "1",    "1",    "1",   "1")
)
p1 <- tb_ml |> ggplot(aes(x=x1, y=x2)) +
  geom_point(aes(color = y)) +
  geom_polygon(data=tb_trap, aes(fill=fill), alpha=0.1) +
  geom_abline(intercept=b, slope=m, linetype="dashed", color="darkgray") +
  scale_colour_manual(
    values=c("0" = "#fb6a4a", "1" = "#84bcdb"),
    aesthetics = c("colour", "fill")) +
  labs(fill = expression(widehat(y))) +
  theme_minimal()
ggsave(f1, p1, "png", width=17.2, height=11.3, units="cm", dpi=72)

}
