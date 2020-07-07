library(tidyverse)
library(viridis)

textsize = theme(axis.title = element_text(size = 20), 
                 axis.text = element_text(size = 18),
                 strip.text = element_text(size = 18), 
                 legend.title = element_text(size = 18), 
                 legend.text = element_text(size = 18))

# fig1 = tibble(odds_ratio = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5)) %>%
fig1 = tibble(psi = seq(.15, 1.35, length.out = 8)) %>%
  crossing(#delta_t = c(.1, .2, .3), 
           pi0 = 0.1 * 0:9, 
           n = c(60, 80, 100, 150), 
           rr = c(0, .25, .5, 1)) %>%
  mutate(s2 = 4/n * (1 + rr), 
         #psi = odds_ratio * delta_t, 
         delta = pi0 * s2 * psi,
         nlab = paste0("n == ", n), 
         tau2lab = paste0("tau^2/v == ", rr), 
         nlab = fct_relevel(nlab, "n == 60", "n == 80"))

p1 = fig1 %>%
  # filter(delta_t == 0.2) %>%
  ggplot() +
    geom_line(aes(pi0, delta, 
                  color = psi, group = factor(psi)), 
              size = 1.0) +
    scale_color_viridis(expression(psi[1]), 
                        breaks = 0.2 * 1:6) +
    facet_grid(nlab ~ tau2lab, 
               labeller = label_parsed) +
    scale_x_continuous(expression("Probability of Missingness" ~ H[i]), 
                     breaks = 0:3 * .25,
                     labels = scales::percent_format(accuracy = 5L)) +
    scale_y_continuous(expression("Bias" ~ delta[i] ~ "(Cohen's d)"), 
                       breaks = 0.05 * 0:3) + 
    theme_bw() +
    textsize
p1
ggsave(plot = p1,
       filename = "./writeup/cca_paper/graphics/delta_plot.pdf", 
       height = 7, width = 11)

###########################################################

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
