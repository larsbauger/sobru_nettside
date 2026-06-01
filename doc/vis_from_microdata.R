library(tidyverse)
library(ggplot2)
library(tidyr)
Sys.setlocale("LC_ALL","no_NB.utf8")
# Eksempeldata for 2+ år NEET
data_2år <- data.frame(
  region = c("Telemark", "Agder", "Buskerud", "Vestfold", "Resten av Norge"),
  `Ikke NEET` = c(80.49, 81.55, 83.41, 79.46, 85.31),
  `NEET 2+` = c(19.55, 18.36, 16.59, 20.55, 14.69)
)
pivo
# Omform data til lang format
data_2år_long <- pivot_longer(data_2år, cols = -region, names_to = "status", values_to = "prosent")

ggplot(data_2år_long, aes(x = region, y = prosent, fill = status)) +
  geom_bar(stat = "identity") +
  labs(title = "Andel NEET (2+ år) per region", x = "Region", y = "Prosent") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

library(ggplot2)

# Data for NEET (2+ år) per region
data_neet_2år <- data.frame(
  region = c("Agder", "Buskerud", "Resten av Norge", "Telemark", "Vestfold"),
  neet_2år = c(18.36, 16.59, 14.69, 19.55, 20.55)
)

# Lag stolpediagram
ggplot(data_neet_2år, aes(x = region, y = neet_2år, fill = region)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Andel NEET (2+ år) per region",
    x = "Region",
    y = "Prosent"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = round(neet_2år, 1)), vjust = -0.5, size = 3.5) +
  ylim(0, max(data_neet_2år$neet_2år) * 1.1)

ggplot(data_neet_2år, aes(x = region, y = neet_2år, color = region)) +
  geom_point(size = 4) +
  labs(
    title = "Andel NEET (2+ år) per region",
    x = "Region",
    y = "Prosent"
  ) +
  theme_minimal() +
  geom_text(aes(label = round(neet_2år, 1)), vjust = -1, size = 3.5) +
  ylim(0, max(data_neet_2år$neet_2år) * 1.1)


library(tidyverse)

# Data for NEET (2+, 3+, 4+ år) per region
data_neet <- data.frame(
  region = c("Agder", "Buskerud", "Resten av Norge", "Telemark", "Vestfold"),
  neet_2år = c(18.36, 16.59, 14.69, 19.55, 20.55),
  neet_3år = c(11.35, 9.81, 8.42, 11.99, 13.08),
  neet_4år = c(7.63, 6.50, 5.37, 7.66, 8.90)
)

# Omform data til lang format for visualisering
data_neet_long <- pivot_longer(
  data_neet,
  cols = c(neet_2år, neet_3år, neet_4år),
  names_to = "kategori",
  values_to = "prosent"
)
ggplot(data_neet_long, aes(x = reorder(region, + prosent), y = prosent, fill = kategori)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Andel NEET (2+, 3+, 4+ år) per region",
    x = "Region",
    y = "Prosent",
    fill = "NEET-kategori"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    aes(label = round(prosent, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  )


library(tidyverse)

# Data for NEET (2+, 3+, 4+ år) per region + landsgjennomsnitt
data_neet <- data.frame(
  region = c("Telemark", "Agder", "Buskerud", "Vestfold", "Landsgjennomsnitt"),
  neet_2år = c(19.65, 19.45, 17.56, 21.18, 15.88),  # Oppdatert 2+ år
  neet_3år = c(12.50, 12.36, 11.17, 13.76, 9.52),    # Oppdatert 3+ år
  neet_4år = c(8.29, 8.64, 7.47, 9.54, 6.23)         # Oppdatert 4+ år
)

# Omform data til lang format for visualisering
data_neet_long <- pivot_longer(
  data_neet,
  cols = c(neet_2år, neet_3år, neet_4år),
  names_to = "kategori",
  values_to = "prosent"
)
ggplot(data_neet_long, aes(x = reorder(region, + prosent), y = prosent, fill = kategori)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Andel NEET (2+, 3+, 4+ år) per region og landsgjennomsnitt",
    x = "Region",
    y = "Prosent",
    fill = "NEET-kategori"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    aes(label = round(prosent, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  )

ggplot(data_neet_long, aes(x = reorder(region, + prosent), y = prosent, color = kategori, group = kategori)) +
  geom_point(size = 4) +
  labs(
    title = "Andel som er NEET (2+, 3+, 4+ år fra 2018-2023) per region og landsgjennomsnitt",
    x = "Region",
    y = "Prosent",
    color = "NEET-kategori"
  ) +
  theme_minimal() +
  geom_text(
    aes(label = round(prosent, 2)),
    vjust = -1,
    size = 3.5
  )

ggplot(data_neet_long, aes(x = reorder(region, + prosent), y = prosent, fill = region)) +
  geom_bar(stat = "identity",) +
  facet_wrap(~ kategori) +
  labs(
    title = "Andel NEET per kategori, region og landsgjennomsnitt",
    x = "Region",
    y = "Prosent"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  geom_text(
    aes(label = round(prosent, 1)),
    vjust = -0.5,
    size = 3.5
  )
stacked_plot <- ggplot(data_neet_long, aes(x = prosent, y = reorder(region, -prosent, FUN = max), fill = kategori)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(
    title = "Andel NEET (2+, 3+, 4+ år) per region",
    x = "Prosent",
    y = "Region",
    fill = "NEET-kategori"
  ) +
  theme_minimal(base_size = 16) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(
    aes(label = round(prosent, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.1,
    size = 3.5
  )
ggsave(here::here("img/neet_stacked_plot.png"), stacked_plot, width = 8, height = 6, dpi = 300)

library(tidyverse)
library(gt)

# Data for antall personer per alder og region
data_alder <- data.frame(
  Alder = c("27 År (Født 1996)", "28 År (Født 1995)", "29 År (Født 1994)", "Total"),
  Telemark = c(1988, 1940, 2043, 5985),
  Agder = c(4083, 3917, 3949, 11947),
  Buskerud = c(3052, 3156, 3356, 9560),
  Vestfold = c(2680, 2917, 2893, 8490),
  "Resten Av Norge" = c(64265, 64291, 64395, 192955),
  Total = c(76070, 76226, 76630, 228928)
)

# Lag tabellen med gt
tabell_alder <- gt(data_alder) %>%
  tab_header(title = "Antall personer per fødselskohort (1994–1996) og region i 2023") %>%
  fmt_number(columns = c(Telemark, Agder, Buskerud, Vestfold, `Resten.Av.Norge`, Total),
             decimals = 0,
             use_seps = FALSE) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  tab_options(column_labels.hidden = FALSE)

# Vis tabellen
tabell_alder
gtsave(tabell_alder, here::here("img/antall_personer_per_alder_region.html"))
gtsave(tabell_alder, here::here("img/antall_personer_per_alder_region.png"))

library(tidyverse)

library(tidyverse)

# Data for NEET (3+ år) per kommune
data_neet_3år <- data.frame(
  kommune = c("Horten", "Holmestrand", "Tønsberg", "Sandefjord", "Larvik", "Færder"),
  neet_3år = c(16.58, 12.75, 11.54, 13.88, 13.64, 17.24)
)

# Data for NEET (4+ år) per kommune
data_neet_4år <- data.frame(
  kommune = c("Horten", "Holmestrand", "Tønsberg", "Sandefjord", "Larvik", "Færder"),
  neet_4år = c(10.69, 9.72, 7.66, 9.25, 10.08, 12.07)
)

# Kombiner dataene
data_neet_3år$kategori <- "NEET 3+ år"
data_neet_4år$kategori <- "NEET 4+ år"

data_neet_kombinert <- bind_rows(data_neet_3år, data_neet_4år) |> 
  janitor::clean_names()

library(ggplot2)

plot_kom <- ggplot(data_neet_kombinert, aes(x = reorder(kommune, + neet_3ar) , 
                                y = ifelse(kategori == "NEET 3+ år",
                                           neet_3ar, neet_4ar), 
                                fill = kategori)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(
    title = "Andel NEET (3+ år og 4+ år) per kommune i Vestfold",
    x = "Kommune",
    y = "Prosent",
    fill = "Kategori"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(
    aes(label = ifelse(kategori == "NEET 3+ år", round(neet_3ar, 1), round(neet_4ar, 1))),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3.5
  )
ggsave(here::here("img/neet_per_kommune_vestfold.png"), 
       plot_kom, width = 8, height = 6, dpi = 300)


library(tidyverse)

# Koeffisienter og standardfeil
coef <- c(0.33851, 0.340253, 0.150853, 0.122723, -1.080036, 0.85163)
se <- c(0.018475, 0.039242, 0.028776, 0.019967, 0.045463, 0.021606)
variables <- c("Kvinne", "Vestfold", "Innvandrer", "Høyutd_foreldre", "høyutd", "Barnevernserfaring")

# Regn ut odds ratio og konfidensintervall
results <- data.frame(
  Variabel = variables,
  Coef = coef,
  OR = exp(coef),
  `2.5 %` = exp(coef - 1.96 * se),
  `97.5 %` = exp(coef + 1.96 * se),
  p_verdi = c(0, 0, 0, 0, 0, 0)  # Alle p-verdier er 0
) |> 
  janitor::clean_names()

# Vis tabellen
results

library(ggplot2)

ggplot(results, aes(x = variabel, y = or, ymin = x2_5, ymax = x97_5)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Odds Ratio for Langvarig NEET (3+ År)",
    x = "Variabel",
    y = "Odds Ratio (log-skala)"
  ) +
  scale_y_log10() +
  theme_minimal() +
  coord_flip()



library(tidyverse)

# Koeffisienter og standardfeil for nasjonal analyse
coef_nasjonal <- c(0.33851, 0.150853, 0.122723, -1.080036, -0.072727, 0.85163)
se_nasjonal <- c(0.018475, 0.028776, 0.019967, 0.045463, 0.001238, 0.021606)
variables <- c("kjønn", "innvandrer", "høyutd_foreldre", "høyutd", "grunnskolepoeng", "bvern_2")

# Koeffisienter og standardfeil for Vestfold
coef_vestfold <- c(0.259671, -0.130046, 0.199546, -0.806727, -0.059825, 0.811092)
se_vestfold <- c(0.077553, 0.131049, 0.083447, 0.20823, 0.005261, 0.090196)

# Lag en dataramme med begge analysene
results <- data.frame(
  variabel = variables,
  coef_nasjonal = coef_nasjonal,
  lower_nasjonal = coef_nasjonal - 1.96 * se_nasjonal,
  upper_nasjonal = coef_nasjonal + 1.96 * se_nasjonal,
  coef_vestfold = coef_vestfold,
  lower_vestfold = coef_vestfold - 1.96 * se_vestfold,
  upper_vestfold = coef_vestfold + 1.96 * se_vestfold
)

# Omform data til lang format for plotting
results_long <- results %>%
  pivot_longer(
    cols = c(coef_nasjonal, lower_nasjonal, upper_nasjonal, coef_vestfold, lower_vestfold, upper_vestfold),
    names_to = "type",
    values_to = "value"
  ) %>%
  separate(type, into = c("measure", "group"), sep = "_") %>%
  pivot_wider(names_from = measure, values_from = value)
library(ggplot2)

ggplot(results_long, aes(x = variabel, y = coef, color = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Sammenligning av koeffisienter: Nasjonal vs. Vestfold",
    x = "Variabel",
    y = "Koeffisient",
    color = "Gruppe"
  ) +
  scale_color_manual(values = c("nasjonal" = "blue", "vestfold" = "red")) +
  theme_minimal() +
  coord_flip()
# Regn ut odds ratio og konfidensintervall
results_or <- results %>%
  mutate(
    or_nasjonal = exp(coef_nasjonal),
    lower_or_nasjonal = exp(lower_nasjonal),
    upper_or_nasjonal = exp(upper_nasjonal),
    or_vestfold = exp(coef_vestfold),
    lower_or_vestfold = exp(lower_vestfold),
    upper_or_vestfold = exp(upper_vestfold)
  )

# Omform data til lang format for odds ratio
results_or_long <- results_or %>%
  pivot_longer(
    cols = c(or_nasjonal, lower_or_nasjonal, upper_or_nasjonal, or_vestfold, lower_or_vestfold, upper_or_vestfold),
    names_to = "type",
    values_to = "value"
  ) %>%
  separate(type, into = c("measure", "group"), sep = "_") %>%
  pivot_wider(names_from = measure, values_from = value)

# Lag forest plot for odds ratio
ggplot(results_or_long, aes(x = variabel, y = or, color = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Sammenligning av odds ratio: Nasjonal vs. Vestfold",
    x = "Variabel",
    y = "Odds Ratio",
    color = "Gruppe"
  ) +
  scale_color_manual(values = c("nasjonal" = "blue", "vestfold" = "red")) +
  scale_y_log10() +
  theme_minimal() +
  coord_flip()
# Regn ut odds ratio og konfidensintervall
results_or <- results %>%
  mutate(
    or_nasjonal = exp(coef_nasjonal),
    lower_or_nasjonal = exp(lower_nasjonal),
    upper_or_nasjonal = exp(upper_nasjonal),
    or_vestfold = exp(coef_vestfold),
    lower_or_vestfold = exp(lower_vestfold),
    upper_or_vestfold = exp(upper_vestfold)
  )
library(tidyverse)

# Odds ratioer og standardfeil
odds_ratio <- c(1.402856, 1.405304, 1.162826, 1.130571, 0.339583, 0.929854, 2.343463)
std_error <- c(0.025918, 0.055147, 0.033461, 0.022574, 0.015438, 0.001151, 0.050632)
variables <- c("Kjønn", "Vestfold24", "Innvandrer", "Høyutd. foreldre", "Høy utdanning", "Grunnskolepoeng", "Barneverntiltak")

# Regn ut konfidensintervall
lower_ci <- odds_ratio * exp(-1.96 * std_error / odds_ratio)
upper_ci <- odds_ratio * exp(1.96 * std_error / odds_ratio)

# Lag en dataramme
results <- data.frame(
  variabel = variables,
  odds_ratio = odds_ratio,
  lower_ci = lower_ci,
  upper_ci = upper_ci
)

plot_reg <- ggplot(results, aes(x = variabel, y = odds_ratio)) +
  geom_point(size = 2.5, color = "#7a0177") +
  geom_errorbar(aes(ymin 
                    = lower_ci, ymax = upper_ci), width = 0.2, linewidth =.5, 
                color = "#c51b8a") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_y_log10(breaks = seq(0.3, 2.5, 0.2))+
  labs(
    title = "Forest Plot: Odds Ratio for Langvarig NEET (3+ År)",
    x = "Variabel",
    y = "Odds Ratio (log-skala)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  coord_flip()
ggsave(here::here("img/forest_plot_neet_3ar.png"), plot_reg, width = 8, height = 6, dpi = 300)
