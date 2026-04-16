if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, psych, mirt, knitr, janitor, naniar, performance, kableExtra, lme4, lmerTest, see, parameters, effectsize, modelsummary)
df <- read.csv("math_data.csv")
summary(df)

#Braki danych####
missing <- df %>% 
  arrange(pomiar) %>%
  vis_miss() +
  theme_minimal(base_family = "sans") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
  labs(title = "Wykres 1. Mapa braków danych (sortowana wg pomiaru)") +
  ylab("Obserwacje")
missing
#####

#Przygotowanie bazy i opis zmiennych####
df1 <- df %>%
  mutate(
    grupa = factor(grupa, levels = c(0, 1), labels = c("Kontrolna", "Eksperymentalna")),
    pomiar = factor(pomiar, levels = c(1, 2), labels = c("Pretest", "Posttest")),
    id_szkoly = as.factor(id_szkoly),
    id_ucznia = as.factor(id_ucznia),
    score_total = rowSums(select(., starts_with("mat_")), na.rm = TRUE)
  )
df_pre <- df1 %>% filter(pomiar == "Pretest") %>% select(where(~ !all(is.na(.))))
df_post <- df1 %>% filter(pomiar == "Posttest") %>% select(where(~ !all(is.na(.))))
#####

#czy wszyscy uczniowie wzięli udział w obu pomiarach?####
full <- df1 %>%
  group_by(id_ucznia) %>%
  summarise(
    liczba_pomiarow = n(),
    liczba_szkol = n_distinct(id_szkoly),
    pomiary = paste(sort(unique(pomiar)), collapse = ",")
  ) 
test <- full %>% 
  summarise(
    Min_liczba_pomiarow = min(liczba_pomiarow),
    Max_liczba_pomiarow = max(liczba_pomiarow),
    Min_liczba_szkol = min(liczba_szkol),
    Max_liczba_szkol = max(liczba_szkol)
  )
test1 <- kable(test, align = "lcccc",
               caption = "Minimalne i maksymalne liczby pomiarów i szkół przypadających na ucznia",
  col.names = c("Min. liczba pomiarów", "Maks. liczba pomiarów", "Min. liczba szkół", "Maks. liczba szkół")) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(latex_options = "hold_position")
test1
#####

#Rozkłady częstości####
tabela_zbiorcza <- df1 %>%
  group_by(grupa, pomiar) %>%
  summarise(Szkoly = n_distinct(id_szkoly), Uczniowie = n_distinct(id_ucznia), .groups = "drop") %>%
  pivot_wider(names_from = pomiar, values_from = c(Szkoly, Uczniowie), names_glue = "{pomiar} {.value}") %>%
  rename(Grupa = grupa) %>%
  adorn_totals("row", name = "Suma") %>%
  adorn_percentages("col") %>%        
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")
czestosci <- kable(tabela_zbiorcza, align = "lcccc",
                   caption = "Liczba szkół i uczniów w badaniu z uwzględnieniem podziału na grupy") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(latex_options = "hold_position")
czestosci
#####

#Błąd związany z doborem próby####
sampling_error <- function(data, pomiar_label) {
  model <- lmer(score_total ~ 1 + (1 | id_szkoly), data = data)
  icc_val <- as.numeric(performance::icc(model)$ICC_adjusted)
  n_total <- nrow(data)
  n_clusters <- n_distinct(data$id_szkoly)
  m <- n_total / n_clusters
  deff <- 1 + (m - 1) * icc_val
  se_cluster <- (sd(data$score_total, na.rm = TRUE) / sqrt(n_total)) * sqrt(deff)
  data.frame(
    Pomiar = pomiar_label, N = n_total, Szkoly = n_clusters,
    Sr_rozmiar_klastra = round(m, 2), ICC = round(icc_val, 3), Deff_c = round(deff, 3),
    SE_klastrowy = round(se_cluster, 3), Margines_bledu_95 = round(1.96 * se_cluster, 3)
  )
}

tabela_bledow <- bind_rows(
  sampling_error(df_pre, "Pretest"),
  sampling_error(df_post, "Posttest")
)
bledy <- kable(tabela_bledow, align = "lcccc",
               caption = "Parametry doboru próby i oszacowania błędu standardowego z uwzględnieniem schematu losowania zespołowego",
               col.names = c("Pomiar", "Uczniowie", "Szkoły",
                             "Śr. wielk. klastra", "ICC", "Deff(c)",
                             "SE (skoryg.)", "Margines błędu")) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(latex_options = "hold_position")
bledy
#####

#Ogólne wyniki punktowe - wykres####
sumy <- df1 %>%
  filter(!is.na(score_total)) %>%
  group_by(pomiar) %>%
  summarise(
    Srednia = mean(score_total),
    Odchylenie = sd(score_total)
  ) %>%
  mutate(
    Etykieta = paste0("M = ", round(Srednia, 2), "\nSD = ", round(Odchylenie, 2)),
    Wyrownanie_X = ifelse(pomiar == "Pretest", 1.1, -0.1) 
  )
sumy
wykres_pkt <- ggplot(df1 %>% filter(!is.na(score_total)), 
                             aes(x = score_total, fill = pomiar)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = sumy, aes(xintercept = Srednia, color = pomiar), 
             linetype = "dashed", linewidth = 1) +
  geom_text(data = sumy, 
            aes(x = Srednia, y = 0.075, label = Etykieta, color = pomiar, hjust = Wyrownanie_X), 
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Pretest" = "#3498db", "Posttest" = "#2ecc71")) +
  scale_color_manual(values = c("Pretest" = "#2980b9", "Posttest" = "#27ae60")) +
  labs(
    title = "Wykres 2. Rozkład sumy punktów w obu pomiarach",
    x = "Suma punktów z testu matematycznego",
    y = "Gęstość prawdopodobieństwa",
    fill = "Pomiar",
    color = "Pomiar"
  ) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "bottom")
wykres_pkt
#####

#Analiza równoległa - wykresy osypiska####
osyp <- function(data, title) {
  items <- data %>% 
    select(starts_with("mat_")) %>% 
    select(where(~any(!is.na(.))))
  parallel_res <- fa.parallel(items, cor = "tet", fa = "fa", plot = FALSE)
  return(parallel_res)
}
pa_pre <- osyp(df_pre, "Pretest")
pa_post <- osyp(df_post, "Posttest")
df_scree_pre <- data.frame(
  Czynnik = 1:length(pa_pre$fa.values),
  Rzeczywiste = pa_pre$fa.values,
  Symulowane = pa_pre$fa.sim
) %>%
  pivot_longer(cols = c(Rzeczywiste, Symulowane), 
               names_to = "Typ", 
               values_to = "Wartosc_wlasna")
osypisko_pre <- ggplot(df_scree_pre, aes(x = Czynnik, y = Wartosc_wlasna, color = Typ, shape = Typ)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") + 
  scale_color_manual(values = c("Rzeczywiste" = "#2c3e50", "Symulowane" = "#e74c3c")) +
  labs(
    title = "Wykres 3. Wykres osypiska z analizy równoległej (Pretest)",
    x = "Numer czynnika",
    y = "Wartość własna (Eigenvalue)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "bottom", legend.title = element_blank())
osypisko_pre
df_scree_post <- data.frame(
  Czynnik = 1:length(pa_post$fa.values),
  Rzeczywiste = pa_post$fa.values,
  Symulowane = pa_post$fa.sim
) %>%
  pivot_longer(cols = c(Rzeczywiste, Symulowane), 
               names_to = "Typ", 
               values_to = "Wartosc_wlasna")
osypisko_post <- ggplot(df_scree_post, aes(x = Czynnik, y = Wartosc_wlasna, color = Typ, shape = Typ)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") + 
  scale_color_manual(values = c("Rzeczywiste" = "#2c3e50", "Symulowane" = "#e74c3c")) +
  labs(
    title = "Wykres 4. Wykres osypiska z analizy równoległej (Posttest)",
    x = "Numer czynnika",
    y = "Wartość własna (Eigenvalue)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "bottom", legend.title = element_blank())
osypisko_post
#####

#Analiza pozycji testowych####
get_psych <- function(data, pomiar_label) {
  items <- data %>% 
    select(starts_with("mat_")) %>% 
    select(where(~any(!is.na(.)) && sd(., na.rm = TRUE) > 0))
  tet_matrix <- psych::tetrachoric(items)$rho
  rel_alpha <- psych::alpha(tet_matrix)
  rel_omega <- psych::omega(items, nfactors = 1, poly = TRUE, plot = FALSE)
  opisowe <- data.frame(
    Zadanie = colnames(items),
    Trudnosc = colMeans(items, na.rm = TRUE),
    SD = apply(items, 2, sd, na.rm = TRUE)
  )
  total_score <- rowSums(items, na.rm = TRUE)
  bis_cor <- sapply(seq_len(ncol(items)), function(i) {
    psych::biserial(total_score - items[[i]], items[[i]])
  })
  dyskryminacja <- data.frame(
    Zadanie = colnames(items),
    Dyskryminacja_bis = as.numeric(bis_cor)
  )
  item_stats <- opisowe %>%
    left_join(dyskryminacja, by = "Zadanie") %>%
    mutate(Pomiar = pomiar_label) %>%
    select(Pomiar, Zadanie, Trudnosc, SD, Dyskryminacja_bis)
  list(
    Alpha = rel_alpha$total$raw_alpha, 
    Omega = rel_omega$omega.tot, 
    ItemStats = item_stats
  )
}
psycho_pre <- get_psych(df_pre, "Pretest")
psycho_post <- get_psych(df_post, "Posttest")
note_pre <- paste0("Pretest: Alfa = ", round(psycho_pre$Alpha, 2), ", Omega (total) = ", round(psycho_pre$Omega, 2))
note_post <- paste0("Posttest: Alfa = ", round(psycho_post$Alpha, 2), ", Omega (total) = ", round(psycho_post$Omega, 2))
parametry_pre <- psycho_pre$ItemStats %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  arrange(desc(Trudnosc)) %>%
  select(-Pomiar) %>%
  kbl(
    col.names = c("Zadanie", "Trudność", "SD", "Dyskryminacja"),
    caption = "Parametry psychometryczne zadań - Pretest",
    booktabs = TRUE,
    row.names = FALSE,
    align = "lccc"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(
    general = note_pre,
    general_title = "Rzetelność skali: ",
    footnote_as_chunk = FALSE)
parametry_post <- psycho_post$ItemStats %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  arrange(desc(Trudnosc)) %>%
  select(-Pomiar) %>%
  kbl(
    col.names = c("Zadanie", "Trudność", "SD", "Dyskryminacja"),
    caption = "Parametry psychometryczne zadań - Posttest",
    booktabs = TRUE,
    row.names = FALSE,
    align = "lccc"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(
    general = note_post,
    general_title = "Rzetelność skali: ",
    footnote_as_chunk = FALSE)
parametry_pre
parametry_post
#####

#Trudność i moc dyskryminacyjna - wykres####
stats_all <- bind_rows(
  psycho_pre$ItemStats,
  psycho_post$ItemStats
)
wykres_rozrzut = ggplot(stats_all, aes(x = Trudnosc, y = Dyskryminacja_bis, color = Pomiar)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Pretest" = "#3498db", "Posttest" = "#2ecc71")) +
  facet_wrap(~Pomiar) +
  labs(
    title = "Wykres 5. Trudność zadań, a ich moc dyskryminacyjna",
    x = "Trudność zadania",
    y = "Dyskryminacja"
  ) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "none")
wykres_rozrzut
#####

#####Wskaźniki umiejętności matematycznych (IRT)####
get_irt <- function(data) {
  items <- data %>% 
    select(starts_with("mat_")) %>% 
    select(where(~any(!is.na(.)) && sd(., na.rm=TRUE) > 0))
  model <- mirt(items, 1, itemtype = '2PL', verbose = FALSE)
  theta <- as.numeric(fscores(model, method = "EAP"))
  er <- round(as.numeric(fscores(model, method = "EAP", returnER = TRUE)), 3)
  attr(theta, "er") <- er 
  return(theta)
}
df_pre_final <- df_pre
df_pre_final$theta <- get_irt(df_pre)

df_post_final <- df_post
df_post_final$theta <- get_irt(df_post)

df_analysis <- full_join(
  df_pre_final %>% select(id_ucznia, id_szkoly, grupa, pre_theta = theta),
  df_post_final %>% select(id_ucznia, post_theta = theta),
  by = "id_ucznia"
)
irt_opis <- df_analysis %>%
  group_by(grupa) %>%
  summarise(
    N = n(),
    M_pre = mean(pre_theta, na.rm = TRUE),
    SD_pre = sd(pre_theta, na.rm = TRUE),
    M_post = mean(post_theta, na.rm = TRUE),
    SD_post = sd(post_theta, na.rm = TRUE)
  ) %>%
  mutate(
    Rzetelnosc_Pre = as.numeric(attr(df_pre_final$theta, "er")),
    Rzetelnosc_Post = as.numeric(attr(df_post_final$theta, "er"))
  )
irt_opis
opis <- kable(irt_opis, digits = 3, 
      caption = "Statystyki opisowe wskaźników IRT oraz rzetelność empiryczna testu",
      col.names = c("Grupa", "N", "Śr. pre", "SD pre", "Śr. post","SD post",
                    "Rzetelność pre", "Rzetelność post"),
      booktabs = TRUE) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(latex_options = "hold_position")
opis
#####

#Dwupoziomowe modele liniowe - testowanie hipotezy
model_1 <- lmer(post_theta ~ pre_theta + grupa + (1 | id_szkoly), 
                 data = df_analysis)
model_2 <- lmer(post_theta ~ pre_theta * grupa + (1 | id_szkoly), 
                 data = df_analysis)
summary(model_1)
summary(model_2)
wyniki_modelu1 <- model_parameters(model_1, standardize = "refit")
print(wyniki_modelu1)
wyniki_modelu2 <- model_parameters(model_2, standardize = "refit")
print(wyniki_modelu2)
r2_modelu <- r2(model_1)
print(r2_modelu)
modele <- list(
  "Model I" = model_1,
  "Model II" = model_2
)
regresje <- modelsummary(
  modele,
  stars = TRUE,
  coef_map = c(
    "(Intercept)" = "Stała",
    "pre_theta" = "Pretest",
    "grupaEksperymentalna" = "Grupa eksperymentalna",
    "pre_theta:grupaEksperymentalna" = "Pretest × Grupa"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "r2.marginal", "clean" = "R2 marginalne", "fmt" = 3),
    list("raw" = "r2.conditional", "clean" = "R2 warunkowe", "fmt" = 3),
    list("raw" = "icc", "clean" = "ICC", "fmt" = 3)
  ),
  title = "Porównanie modeli regresji wielopoziomowej - zmienną zależną jest wskaźnik umiejętności matematycznych z posttestu"
)
regresje
#####

#Walidacja modelu - wykresy####
check_model(model_1)
plot6 <- plot(check_heteroscedasticity(model_1)) +
  labs(title = "Wykres 6. Sprawdzenie jednorodności wariancji", subtitle = NULL, x = "Dopasowane", y = "Reszty") +
  theme_minimal(base_family = "sans")
plot7 <- plot(check_normality(model_1), type = "qq") +
  labs(title = "Wykres 7. Wykres Q-Q reszt", subtitle = NULL, x = "Teoretyczne", y = "Obserwowane") +
  theme_minimal(base_family = "sans")
plot6
plot7
#####