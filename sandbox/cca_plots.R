library(tidyverse)
library(viridis)
library(cowplot)

textsize = theme(axis.title = element_text(size = 20), 
                 axis.text = element_text(size = 18),
                 strip.text = element_text(size = 18), 
                 legend.title = element_text(size = 18), 
                 legend.text = element_text(size = 18))

sort(unlist(lapply(c(.1, .2, .5, .8, 1, 1.5), 
       FUN = function(x) log(seq(1.5, 5, by = .5))/x)))

# fig1 = tibble(odds_ratio = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5)) %>%
fig1 = tibble(psi = seq(.4, 7.5, length.out = 150)) %>%
  crossing(#delta_t = c(.1, .2, .3), 
           pi0 = 0.1 * 0:9, 
           n = c(60, 80, 100, 150), 
           rr = c(0, .25, .5, 1)) %>%
  mutate(s2 = 4/n * (1 + rr), 
         #psi = odds_ratio * delta_t, 
         delta = pi0 * s2 * psi,
         nlab = paste0("v == 4/", n), 
         tau2lab = paste0("tau^2/v == ", rr), 
         nlab = fct_relevel(nlab, "v == 4/60", "v == 4/80"))

p1 = fig1 %>%
  # filter(delta_t == 0.2) %>%
  ggplot() +
    geom_line(aes(pi0, delta, 
                  color = psi, group = factor(psi)), 
              size = 1.0) +
    scale_color_viridis(expression(psi[1]), 
                        breaks = 1 + 2*0:6, 
                        option = "cividis") +
    facet_grid(nlab ~ tau2lab, 
               labeller = label_parsed) +
    scale_x_continuous(expression("Probability of Missingness" ~ H[1](0)), 
                     breaks = 0:3 * .25,
                     labels = scales::percent_format(accuracy = 5L)) +
    scale_y_continuous(expression("Bias of" ~ hat(beta)[0][C] ~ "(Cohen's d)"), 
                       breaks = 0.2 * 0:4) + 
    theme_bw() +
    textsize
p1
ggsave(plot = p1,
       filename = "./writeup/cca_paper/graphics/delta_plot_cts.pdf", 
       height = 7, width = 11)



p1pres = fig1 %>%
  # filter(delta_t == 0.2) %>%
  ggplot() +
  geom_line(aes(pi0, delta, 
                color = psi, group = factor(psi)), 
            size = 1.0) +
  scale_color_viridis(expression(psi[1]), 
                      breaks = 1 + 2*0:6, 
                      option = "cividis") +
  facet_grid(nlab ~ tau2lab, 
             labeller = label_parsed) +
  scale_x_continuous("Probability of Missingness Given X, v", 
                     breaks = 0:3 * .25,
                     labels = scales::percent_format(accuracy = 5L)) +
  scale_y_continuous(expression("Bias" ~ delta[i] ~ "(Cohen's d)"), 
                     breaks = 0.3 * 0:5) + 
  theme_bw() +
  textsize
ggsave(plot = p1pres,
       filename = "./writeup/cca_paper/presentation/delta_plot_cts.jpg", 
       height = 7, width = 11)

###########################################################

fig2 = tibble(psi1 = seq(0.4, 7.5, length.out = 150)) %>%
  crossing(pd = seq(0, 0.5, length.out = 100), 
           n = c(60, 80, 100, 150), 
           r = c(0, .25, .5, 1)) %>%
  mutate(s2 = (4/n) * (1 + r),
         bias = s2 * pd * psi1, 
         nlab = paste0("v == 4/", n), 
         tau2lab = paste0("tau^2/v == ", r), 
         nlab = fct_relevel(nlab, "v == 4/60", "v == 4/80"))

p2a = ggplot(fig2) + 
  geom_line(aes(pd, bias, color = psi1, group = psi1), size = .9) +
  scale_color_viridis(expression(psi[1]),
                      breaks = 1 + 2*0:6,
                      option = "cividis") +
  scale_x_continuous(expression("Differential Missingness" ~ H[1](1) - H[1](0)), 
                     breaks = c(0, .2, .4)) +
  scale_y_continuous(expression("Bias" ~ hat(beta)[1][C] ~ "(Cohen's d)"), 
                     breaks = 0.2 * 0:2) +
  facet_grid(nlab ~ tau2lab,
             labeller = label_parsed) +
  theme_bw() + textsize
# +
#   theme(axis.title = element_text(size = 24), 
#         axis.text = element_text(size = 22),
#         strip.text = element_text(size = 22), 
#         legend.title = element_text(size = 22), 
#         legend.text = element_text(size = 22), 
#         legend.key.height = unit(1.5, "cm"))
p2a
ggsave(plot = p2a,
       filename = "./writeup/cca_paper/graphics/bias_beta1_ex1.pdf", 
       width = 11, height = 7)


p2p = ggplot(fig2 %>% 
               filter(psi3 == 0)) + 
  geom_line(aes(pd, bias, color = psi1, group = psi1), size = 1.1) +
  scale_color_viridis(expression(psi[1]),
                      breaks = 1 + 2*0:6,
                      option = "cividis") +
  scale_x_continuous(expression("Differential Missingness"),
                     breaks = -3:3 * .25,
                     labels = scales::percent_format(accuracy = 5L)) +
  scale_y_continuous("Bias (Cohen's d)",
                     breaks = .1 * -3:3) +
  # facet_grid( ~ pdlab,
  #            labeller = label_parsed) +
  theme_bw() +
  theme(axis.title = element_text(size = 24), 
        axis.text = element_text(size = 22),
        strip.text = element_text(size = 22), 
        legend.title = element_text(size = 22), 
        legend.text = element_text(size = 22), 
        legend.key.height = unit(1.5, "cm"))
p2p
ggsave(plot = p2p,
       filename = "./writeup/cca_paper/presentation/bias_beta1_ex1.jpg", 
       width = 12, height = 8)

fig2b = tibble(psi1 = c(1, 3, 5, 7)) %>%
  crossing(psi3 = c(-1, 0, 1), 
           p1 = seq(0, .99, length.out = 20), 
           p0 = seq(0, .99, length.out = 20), 
           s2 = 4/150 + 1/150) %>%
  mutate(bias = s2 * (p1 - p0) * psi1 + s2 * p1 * psi3)

ggplot(fig2b) + 
  geom_tile(aes(p0, p1, fill = bias)) + 
  scale_fill_gradient2(low = "#210233", 
                       mid = "white", 
                       high = "#013f4a") + 
  facet_grid(psi1 ~ psi3)




############################################################################

fig3 = tibble(beta2 = seq(-.5, .5, length.out = 50)) %>%
  crossing(pi0 = c(0.25, .5, .75),#seq(0.1, .9, by = 0.001), 
           corr = c(0, 0.1, 0.3, 0.5)) %>%
  mutate(bias0 = beta2 * pi0, 
         bias1 = beta2 * corr)


p3a = ggplot(fig3) +
  geom_line(aes(beta2, bias0, color = factor(pi0)), 
            size = 1.1) +
  scale_color_viridis(expression(pi[0][1]),
                      discrete = TRUE,
                      option = "plasma") +
  labs(x = expression(beta[2]), 
       y = expression("Bias of " ~ beta[0])) +
  theme_bw() +
  textsize
p3a

p3b = ggplot(fig3) +
  geom_line(aes(beta2, bias1, color = factor(corr)), size = 1.1) +
  scale_color_viridis(expression(rho[1][2]),
                      discrete = TRUE) +
  labs(x = expression(beta[2]), 
       y = expression("Bias of " ~ beta[1])) +
  ylim(-.5, .5) +
  theme_bw() +
  textsize

p3b

p3grid = plot_grid(p3a, p3b, ncol = 2)
p3grid
ggsave(plot = p3grid,
       filename = "./writeup/cca_paper/graphics/omitted_var_bias.pdf", 
       width = 12, height = 6)

############################################################################

ebp <- read.csv("./data/cca_bias_table.csv") # here to check

cc_ebp <- ebp %>%
  select(`beta[0]` = bias_b0CC, 
         `beta[1]` = bias_b1CC) %>%
  pivot_longer(cols = `beta[0]`:`beta[1]`, names_to = "parameter") 

p4 <- ggplot(cc_ebp) + 
  stat_density(aes(value)) +
  geom_vline(data = cc_ebp %>% group_by(parameter) %>% summarize(pl = mean(value)),
             aes(xintercept = pl), color = "red") +
  facet_wrap(~parameter, labeller = label_parsed) +
  labs(x = "Bias", y = "Density") +
  theme_bw() + 
  scale_x_continuous(breaks = c(0, 0.05, 0.1)) +
  textsize

ggsave(plot = p4,
       filename = "./writeup/cca_paper/graphics/emp_miss_bias.pdf", 
       width = 12, height = 6)

sc_ebp <- ebp %>%
  select(`beta[0]` = omv_bias_b0, 
         `beta[1]` = omv_bias_b1) %>%
  pivot_longer(cols = `beta[0]`:`beta[1]`, names_to = "parameter") 


p5 <- ggplot(sc_ebp) + 
  stat_density(aes(value)) +
  geom_vline(data = sc_ebp %>% group_by(parameter) %>% summarize(pl = mean(value)),
             aes(xintercept = pl), color = "red") +
  facet_wrap(~parameter, labeller = label_parsed) +
  labs(x = "Bias", y = "Density") +
  theme_bw() + 
  # scale_x_continuous(breaks = c(0, 0.05, 0.1)) +
  textsize

p5

ggsave(plot = p5,
       filename = "./writeup/cca_paper/graphics/emp_omv_bias.pdf", 
       width = 12, height = 6)


tot_emp <- sc_ebp %>% rename(vv1 = value) %>%
  bind_cols(cc_ebp %>% select(vv2 = value)) %>%
  mutate(
    value = vv1 + vv2
    )

p6 <- ggplot(tot_emp) + 
  stat_density(aes(value)) +
  geom_vline(data = tot_emp %>% group_by(parameter) %>% summarize(pl = mean(value)),
             aes(xintercept = pl), color = "red") +
  facet_wrap(~parameter, labeller = label_parsed) +
  labs(x = "Bias", y = "Density") +
  theme_bw() + 
  # scale_x_continuous(breaks = c(0, 0.05, 0.1)) +
  textsize

p6

ggsave(plot = p6,
       filename = "./writeup/cca_paper/graphics/emp_tot_bias.pdf", 
       width = 12, height = 6)


p7dat <- tot_emp %>%
  mutate(bias = "Total Bias") %>%
  bind_rows(cc_ebp %>% mutate(bias = "Missingness Bias"), 
            sc_ebp %>% mutate(bias = "Omitted Var. Bias"))

p7 <- ggplot(p7dat) + 
  stat_density(aes(value, color = bias), geom = "line") +
  # geom_vline(data = tot_emp %>% group_by(parameter) %>% summarize(pl = mean(value)),
  #            aes(xintercept = pl), color = "red") +
  facet_wrap(~parameter, labeller = label_parsed) +
  labs(x = "Bias", y = "Density") +
  theme_bw() + 
  # scale_x_continuous(breaks = c(0, 0.05, 0.1)) +
  textsize
p7




dat = tibble(psi2 = c(1.1, 1.45, 2.5, 4.13)) %>%
  crossing(pi0 = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), 
           v = 4/c(80), 
           rr = c(0, 0.25, 0.5)) %>%
  mutate(s2 = (1 + rr) * v, 
         bias = psi2 * pi0 * s2, 
         psi2lab = paste0("psi[2] == ", psi2))

ggplot(dat) +
  geom_line(aes(pi0, bias, color = factor(s2))) + 
  facet_grid(~ psi2lab, 
             labeller = label_parsed) +
  scale_color_viridis(expression(tau^2 + v), 
                      discrete = TRUE) + 
  scale_x_continuous("P[R = 0 | X = 0]", 
                     breaks = 0:3 * .2,
                     labels = scales::percent_format(accuracy = 5L)) +
  theme_bw()


dat2 = tibble(psi2 = c(1.1, 1.45, 2.5, 4.13)) %>%
  crossing(d = c(0.2, 0.5, 0.8), 
           v = 4/80, 
           rr = c(0, 0.25, 0.5), 
           pi0 = 0:6 * .1) %>%
  mutate(ps2 = psi2 * d, 
         s2 = v * (1 + rr), 
         bias = ps2 * s2 * pi0, 
         dlab = paste0("d == ", d),
         psi2lab = paste0("psi[2] ==", psi2),
         pslab = paste0("psi[2] == ", ps2))

ggplot(dat2) + 
  geom_line(aes(pi0, bias, color = factor(s2))) + 
  facet_grid(dlab ~ psi2lab, 
             labeller = label_parsed) +
  scale_color_viridis(expression(tau^2 + v), 
                      discrete = TRUE) + 
  scale_x_continuous("P[R = 0 | X = 0]", 
                     breaks = 0:3 * .2,
                     labels = scales::percent_format(accuracy = 5L)) +
  theme_bw()


datb1 = tibble(psi2 = c(1.45, 2.5, 4.13)) %>%
  crossing(psi3 = c(0, .25, .5, 1, 2), 
           d = c(0.2, 0.5, 0.8), 
           v = 4/80, 
           pi0 = 0:6 * .1, 
           a = c(.5, 1, 2)) %>%
  mutate(ps2 = psi2 * d, 
         ps3 = psi3 * d,
         bias = pi0 * v * ((a - 1) * psi2 + a * psi3), 
         dlab = paste0("d == ", d),
         psi2lab = paste0("psi[2] ==", psi2),
         psi3lab = paste0("psi[3] ==", psi3))

datb1 %>% filter(d == 0.2) %>% 
  mutate(psi2lab = paste0("psi[2] == ", d * psi2),
         psi3lab = paste0("psi[3] == ", d * psi3)) %>%
ggplot() + 
  geom_line(aes(pi0, bias, color = factor(a))) +
  facet_grid(psi2lab ~ psi3lab, 
             labeller = label_parsed) +
  scale_color_viridis(expression(frac("P[R = 0 | X = 0]", 
                                      "P[R = 0 | X = 1]")), 
                      discrete = TRUE) + 
  scale_x_continuous("P[R = 0 | X = 0]", 
                     breaks = 0:3 * .2,
                     labels = scales::percent_format(accuracy = 5L)) +
  theme_bw()
