#-------------------------------------------------
#0. Installation des packages
#--------------------------------------------------
library(readxl)
library(openssl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(purrr)
library(gridExtra)
library(grid)
library(reshape2)
#------------------------------------------------------------
#1. Ouverture et lecture des fichiers
data= "/Users/baharkhodadustan/Documents/memoire/20240401-20250401-Statistiques.xlsx"
df <- read_excel(data)

-------------------------------------------------------
  #2. Préparation des données 
  #------------------------------------------------------------
# 3. Anonymiser les données patients

library(openssl)

# Fonction d’anonymisation
mask <- function(string, seed) {
  set.seed(seed)
  stuff <- c(letters, LETTERS, 0:9)
  new <- chartr(paste0(stuff, collapse=''), paste0(sample(stuff), collapse=''), string)
  md5 <- openssl::md5(new)
  return(md5)
}

names(df)


# Appliquer l'anonymisation à la colonne Patient_ID
df$Patient_ID_hashed <- mask(df$Patient_ID, seed = 105)

# Supprimer ou masquer la colonne d’origine si nécessaire
# df$Patient_ID <- NULL  # Décommente cette ligne si tu veux supprimer la colonne originale




# Chargement des bibliothèques
library(dplyr)
library(ggplot2)
library(lubridate)
install.packages("writexl")

library(hms)
library(writexl)


#------------------------------------------------------------
# ANONYMISATION de Id_medecin en identifiants numériques
#------------------------------------------------------------
library(dplyr)
# Créer la table de correspondance
medecin_mapping <- df %>%
  distinct(Id_medecin) %>%
  mutate(Id_medecin_anon = as.integer(factor(Id_medecin)))

# Joindre au dataframe principal
df <- df %>%
  left_join(medecin_mapping, by = "Id_medecin")

# (optionnel) Export de la table de correspondance
# write_xlsx(medecin_mapping, "correspondance_id_medecin.xlsx")

#------------------------------------------------------------
# COLONNES TEMPORELLES pour analyses par mois
#------------------------------------------------------------
library(lubridate)

df <- df %>%
  mutate(
    date_time = ymd_hms(RDV),
    year_month = format(date_time, "%Y-%m"),              # Pour tri
    label_mois = format(date_time, "%B %Y")               # Pour affichage
  )

# Tri chronologique de l’axe X
month_order <- df %>%
  distinct(year_month, label_mois) %>%
  arrange(year_month)

df <- df %>%
  mutate(label_mois = factor(label_mois, levels = month_order$label_mois))

#------------------------------------------------------------
# 3.1 Nombre de non-venus par mois
#------------------------------------------------------------
library(ggplot2)

df_summary_nonvenus <- df %>%
  filter(Pasvenu == "Oui") %>%
  group_by(label_mois) %>%
  summarise(cumul_non_venus = n(), .groups = "drop")

ggplot(df_summary_nonvenus, aes(x = label_mois, y = cumul_non_venus)) +
  geom_point(stat = "identity", color = "red", size = 5) +
  labs(x = "Mois", y = "Cumul des non-venus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#------------------------------------------------------------
# 3.1.2 Combien de patients ont manqué au moins un rendez-vous
#------------------------------------------------------------

n_total_patients <- df %>% distinct(Patient_ID_hashed) %>% nrow()

n_patients_no_show <- df %>%
  filter(Pasvenu == "Oui") %>%
  distinct(Patient_ID_hashed) %>%
  nrow()

pourcentage_patients_no_show <- n_patients_no_show / n_total_patients * 100

nb_exact_1_noshow <- df %>%
  filter(Pasvenu == "Oui") %>%
  group_by(Patient_ID_hashed) %>%
  summarise(nb_no_show = n()) %>%
  filter(nb_no_show == 1) %>%
  nrow()

pourcentage_1_noshow <- nb_exact_1_noshow / n_patients_no_show * 100

cat("Nombre total de patients uniques :", n_total_patients, "\n")
cat("Nombre de patients ayant eu au moins un no-show :", n_patients_no_show, "\n")
cat("→ Pourcentage :", round(pourcentage_patients_no_show, 1), "%\n\n")

cat("Nombre de patients ayant exactement 1 no-show :", nb_exact_1_noshow, "\n")
cat("→ Pourcentage parmi les no-shows :", round(pourcentage_1_noshow, 1), "%\n")

# Résumé du nombre de no-shows par mois, trié
df_summary_top <- df %>%
  filter(Pasvenu == "Oui") %>%
  group_by(label_mois) %>%
  summarise(nb_no_show = n(), .groups = "drop") %>%
  arrange(desc(nb_no_show)) %>%
  mutate(Rang = row_number()) %>%
  select(Rang, Mois = label_mois, `Nombre de no-show` = nb_no_show)

# Afficher le tableau
print(df_summary_top)

# Calcul du nombre de no-shows par patient
no_show_dist <- df %>%
  mutate(no_show = ifelse(Pasvenu == "Oui", 1, 0)) %>%
  group_by(Patient_ID_hashed) %>%
  summarise(nb_no_show = sum(no_show), .groups = "drop") %>%
  group_by(nb_no_show) %>%
  summarise(
    `Nombre de patients` = n(),
    .groups = "drop"
  ) %>%
  mutate(
    `Proportion (%)` = round(`Nombre de patients` / sum(`Nombre de patients`) * 100, 2)
  )

# Affichage du tableau
print(no_show_dist)

# Graphique
library(ggplot2)
ggplot(no_show_dist, aes(x = factor(nb_no_show), y = `Nombre de patients`)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = paste0(`Proportion (%)`, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Répartition des patients selon leur nombre de no-shows",
    x = "Nombre de no-shows",
    y = "Nombre de patients"
  ) +
  theme_minimal()

# Temps perdu total et par médecin #

print(time_duration_nonvenus)
# 254:50:00 = 254 heures + 50 minutes ≈ 254.83 heures

jours_perdus <- as.numeric(duree_non_venus) / 60 / 7  # minutes → heures → jours
jours_perdus

duree_par_medecin <- df %>%
  filter(Pasvenu == "Oui") %>%
  group_by(Id_medecin_anon) %>%
  summarise(duree_perdue = sum(Duree, na.rm = TRUE) / 60 / 7)





#------------------------------------------------------------
# 3.2 Total consultations et proportions par mois
#------------------------------------------------------------

df_summary <- df %>%
  group_by(label_mois) %>%
  summarise(
    cumul_non_venus = sum(Pasvenu == "Oui"),
    cumul_venus = sum(Pasvenu == "Non"),
    total_consultations = n(),
    .groups = "drop"
  ) %>%
  mutate(pourcentage_non_venus = (cumul_non_venus / total_consultations) * 100)

#------------------------------------------------------------
# 3.3 Graphique combiné : Non-venus / Total / Pourcentage
#------------------------------------------------------------

p <- ggplot(df_summary, aes(x = label_mois)) +
  geom_bar(aes(y = total_consultations, fill = "Total des consultations"), stat = "identity", alpha = 0.7) +
  geom_bar(aes(y = cumul_non_venus, fill = "Non-venus"), stat = "identity", alpha = 0.9) +
  scale_fill_manual(
    values = c("Total des consultations" = "gray", "Non-venus" = "blue"),
    name = "Catégorie"
  ) +
  geom_text(
    aes(y = cumul_non_venus, label = paste0(round(pourcentage_non_venus, 1), "%")),
    vjust = -0.5,
    color = "black",
    size = 4
  ) +
  labs(x = "Mois", y = "Nombre de consultation(s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

print(p)

# Assurez-vous d’avoir les bons packages
library(dplyr)
library(ggplot2)

# ---- CALCUL DU NOMBRE DE NO-SHOWS PAR MEDECIN ----

prevalence_medecin <- df %>%
  group_by(Id_medecin_anon) %>%
  summarise(
    nb_consultations = n(),
    nb_non_venus = sum(Pasvenu == "Oui", na.rm = TRUE),
    nb_venus = sum(Pasvenu == "Non", na.rm = TRUE),
    pourcentage_non_venus = (nb_non_venus / nb_consultations) * 100,
    .groups = "drop"
  )

# ---- TRI DU PLUS AU MOINS ABSENTÉISTE ----

prevalence_medecin <- prevalence_medecin %>%
  arrange(desc(pourcentage_non_venus)) %>%
  mutate(Id_medecin_anon = factor(Id_medecin_anon, levels = unique(Id_medecin_anon)))

# ---- GRAPHIQUE ----

# Chargement des bibliothèques
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# ---- Liste des médecins résidents (fixes) ----
residents_ids <- c(6, 8, 3, 1, 2)

# ---- Préparation des données ----
prevalence_bar <- df %>%
  group_by(Id_medecin_anon) %>%
  summarise(
    total = n(),
    no_show = sum(Pasvenu == "Oui", na.rm = TRUE),
    show = sum(Pasvenu == "Non", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    no_show_pct = no_show / total,
    show_pct = show / total,
    type = ifelse(Id_medecin_anon %in% residents_ids, "Médecin fixe", "Assistant"),
    display_name = paste0(type, " ", ave(Id_medecin_anon, type, FUN = function(x) rank(x)))
  )

# ---- Passage au format long + pour étiquettes ----
prevalence_long <- prevalence_bar %>%
  select(display_name, type, no_show_pct, show_pct) %>%
  pivot_longer(
    cols = c(no_show_pct, show_pct),
    names_to = "Statut",
    values_to = "Valeur"
  ) %>%
  mutate(
    Statut = recode(Statut,
                    no_show_pct = "No-Show",
                    show_pct = "Venu"),  # ici on remplace "Yes" par "Venu"
    Statut = factor(Statut, levels = c("No-Show", "Venu"))
  )

# ---- Graphique avec étiquettes + facettes ----
ggplot(prevalence_long, aes(x = Valeur, y = reorder(display_name, Valeur), fill = Statut)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(Valeur * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_fill_manual(
    values = c("No-Show" = "indianred", "Venu" = "lightblue"),
    name = NULL
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Pourcentage de No-Show par médecin (2024–2025)",
    x = "Pourcentage de consultations",
    y = "Médecin"
  ) +
  facet_wrap(~ type, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(face = "bold")
  )

#------------------------------------------------------------
# 4. Prévalence annuelle des non-venus et durée
#------------------------------------------------------------
library(hms)

# Statistique brute venus / non-venus
table(df$Pasvenu)

# Conversion de la durée si besoin
df$Duree <- as.numeric(df$Duree)

# Durée totale perdue par les absences
duree_non_venus <- df %>%
  filter(Pasvenu == 'Oui') %>%
  summarise(total_Duree = sum(Duree, na.rm = TRUE)) %>%
  pull(total_Duree)

# Affichage en hh:mm:ss
time_duration_nonvenus <- as.hms(duree_non_venus * 60)
print(time_duration_nonvenus)

#------------------------------------------------------------
# Fréquence des visites par patient
#------------------------------------------------------------
df <- rename(df, Patient = "Calendar item ID")

visit_freq <- df %>%
  group_by(Patient) %>%
  summarise(
    visit_frequency = n(),
    .groups = "drop"
  )

mean(visit_freq$visit_frequency)
median(visit_freq$visit_frequency)

#------------------------------------------------------------
# Durée cumulée des visites par patient
#------------------------------------------------------------

visit_dur <- df %>%
  group_by(Patient) %>%
  summarise(
    visit_frequency = n(),
    total_duration = sum(Duree, na.rm = TRUE),
    .groups = "drop"
  )

mean(visit_dur$total_duration)
median(visit_dur$total_duration)

# Affichage de la durée médiane en format hms
time_duration_median <- as.hms(median(visit_dur$total_duration) * 60)
print(time_duration_median)
