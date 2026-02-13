##* *****************************************#
#* Script for data importing and cleaning
#* MHDP Project
#* Oct 2025
##* ****************************************#

# access data from the shared drive folder
drive_auth()
d_file <- drive_get("MH_Data_Cleaned Dataset (With HMIS Codes).xlsx")
file_path <- tempfile(fileext = ".xlsx")

drive_download(d_file,
  path = file_path,
  overwrite = T
)

raw_data <- read_excel(file_path,
  col_names = T, trim_ws = T,
  sheet = "hmc Clean dataset"
)


# 1. Initial cleaning of variable names and structure

raw_data <- raw_data %>%
  remove_empty() %>%
  clean_names() %>%
  separate_wider_delim(mh_district_subcounty,
    delim = ":",
    names = c("mh_subcounty", "mh_district")
  ) %>%
  relocate(mh_parish, .after = mh_village) %>%
  relocate(mh_subcounty, mh_district, .after = mh_parish)


# 2. Add a leading mh_ to all variable names if not already present for consistency

colnames(raw_data) <- ifelse(grepl("^mh_", colnames(raw_data)),
  colnames(raw_data),
  paste0("mh_", colnames(raw_data))
)


# 3. Drop variables with no data and system variables that are not part of data variables

data_cl1 <- raw_data %>%
  dplyr::select(where(~ !all(is.na(.)))) %>%
  distinct() %>%
  select(-c(
    mh_referral_hospitals, mh_fever, mh_nin,
    mh_nok_phone_contact, mh_last_updated_on, mh_event_status
  ))


# 4.  Identify and drop duplicates if any using the unique identifier variable

data_cl1 <- data_cl1 %>%
  distinct(mh_unique_id, .keep_all = TRUE)

# Assumptions
# - A patient cannot come to the same facility on the same date with the same classification more than once

data_clean_v1 <- data_cl1 %>%
  unite(mh_patientcode, mh_organisation_unit_name, mh_date_of_visit, mh_patient_name, mh_classification,
    sep = "_",
    remove = F
  ) %>%
  group_by(mh_patientcode) %>%
  mutate(mh_visit_count = n()) %>%
  ungroup() %>%
  relocate(mh_patientcode, mh_visit_count, .after = mh_organisation_unit_name) %>%
  relocate(mh_date_of_visit, mh_patient_name, mh_classification, .after = mh_visit_count) %>%
  filter(mh_visit_count == 1) # drop those with visit count > 1


# 5. Clean the address variables, Create age groups  and month, year variables from date

data_clean_v1 <- data_clean_v1 %>%
  mutate(
    mh_district = gsub("District", "", mh_district),
    mh_subcounty = stringr::str_remove_all(mh_subcounty, "Subcounty"),
    mh_date_of_visit = ymd(mh_date_of_visit),
    mh_age_group = cut(mh_age_in_years,
      breaks = c(0, 4, 9, 14, 19, 35, 45, 55, Inf),
      labels = c(
        "< 5yrs", "5-9", "10-14", "15-19",
        "20-35", "36-45", "46-55", " > 55"
      )
    ),
    mh_month = month(mh_date_of_visit),
    mh_year = year(mh_date_of_visit)
  )

# 6. Generate depression and anxiety variables based on the HMIS sub-categorizations
# and drop those with missing diagnosis
data_clean_v1 <- data_clean_v1 %>%
  filter(!is.na(mh_diagnosis)) %>%
  mutate(
    mh_depression = if_else(mh_mh08_bipolar_affective_disorder == 1, "Yes",
      if_else(mh_mh09_major_depressive_disorder == 1, "Yes",
        "No"
      )
    ),
    mh_anxiety = if_else(mh_mh10_anxiety_phobic_disorders == 1, "Yes",
      if_else(mh_mh11_anxiety_social_anxiety_disoders == 1, "Yes",
        if_else(mh_mh12_anxiety_panic_disorder == 1, "Yes",
          if_else(mh_mh13_anxiety_generalized_anxiety_disorder == 1, "Yes",
            if_else(mh_mh14_obsessive_compulsive_disorder == 1,
              "Yes", "No"
            )
          )
        )
      )
    )
  )


# 7. Create a unique patient ID using hash algorithm and visit_log---------

data_clean <- data_clean_v1 %>%
  mutate(mh_sex = if_else(mh_sex == "female", "Female", mh_sex)) %>%
  mutate(across(
    c(mh_village, mh_patient_name),
    tolower
  )) %>%
  mutate(across(
    c(mh_prescription, mh_diagnosis),
    str_squish
  )) %>%
  mutate(
    combined = paste(mh_village, mh_patient_name, mh_sex, sep = "_"),
    patient_id = sapply(
      combined,
      function(x) digest(x, algo = "sha256")
    )
  ) %>%
  select(patient_id, everything()) %>%
  arrange(patient_id, mh_date_of_visit) %>%
  group_by(patient_id) %>%
  mutate(mh_visits_total = n()) %>%
  ungroup() %>%
  mutate(across(
    c(mh_depression, mh_anxiety),
    as.factor
  ))


# de-identify the data, drop unnecessary variables, organize variables and save the clean dataset

data_clean <- data_clean %>%
  select(-c(
    combined, mh_unique_id, mh_patientcode, mh_phone_contact, mh_duplicate,
    mh_nok_given_name, mh_nok_surname, mh_visit_count, mh_missing_diagnosis
  )) %>%
  select(
    patient_id, mh_organisation_unit_name, mh_patient_name,
    mh_village, mh_visits_total, mh_date_of_visit, mh_classification,
    mh_month, mh_year, mh_age_in_years, mh_sex, mh_age_group, mh_district, mh_subcounty,
    mh_depression, mh_anxiety,
    everything()
  )


write_csv(data_clean, here::here("data", "data_clean.csv"))

#Select  depression and anxiety and only entries in the study period (2022-2024)and save the dataset
data_dep_anx <- data_clean %>%
  mutate(mh_year = as.numeric(as.character(mh_year))) %>%
  filter(mh_depression == "Yes" | mh_anxiety == "Yes") %>%
  filter(mh_year >= 2022 & mh_year <= 2024) %>%
  group_by(patient_id) %>%
  arrange(mh_date_of_visit, .by_group = T) %>%
  mutate(mh_visit_no = row_number()) %>%
  ungroup() %>%
  relocate(mh_visit_no, .before = mh_visits_total) %>%
  select(-c(mh_patient_name, mh_village))%>%
  write_csv(here::here("data", "data_dep_anx.csv"))

# Print a completion message
message("Data cleaning completed and saved to 'data/data_clean.csv'")



# 8. clean prescriptions data and generate variables for specific medications--------

data_clean <- read_csv(here::here("data", "data_clean.csv"))

cabs <- paste("carbamaz", "carbomaz", "cbz", "carba", "calb", "amzapi", "cab", "rmazepine",
  "canba", "canaba", "bamazep", "caarba", "caeba", "",
  "craba", "cb2", "cb", "cbu", "carb", "carm",
  sep = "|"
)

chlo <- paste("chlor", "cro", "oprom", "cpz", "cp2", "cpt", "poma", "c pz", "c.pc",
  "c.p.z", "c/z", "rprom",
  sep = "|"
) # how 2 include c[z

artn <- paste("arta", "alt", "arten", "tane", "atrane", "altan", "aane", "arkane", "artene",
  sep = "|"
)
amit <- paste("amit", "amiit", "trip", "hrip", "line ", "amir", "amty",
  sep = "|"
)
haldol <- paste("hald", "dol ", "peridol", "peridal", "halld", "hali", "hald", "holdo", "holda",
  "hadl", "haido", "halop", "halpeldo",
  sep = "|"
)
foli <- paste("fol", "foic", "foii", "f/z", "f/a ", "f\a", "acid",
  "olic", "foil", "floric", "f/", "fia", "f-a",
  sep = "|"
)

trifluop <- paste("trif", "stala", "ster", "triplo", "trflo", "trithr", "terazine", "stelaz", "talazine",
  "briflo", "stalzine", "stalin", "relazine", "stale", "tritmo",
  "tripela", "salrall",
  sep = "|"
)
fluphen <- paste("flup", "fenazine", "femaz", "phemaz", "fenaaz", "phenazine", "phenaz",
  sep = "|"
)
benzh <- paste("benz", "hexol", "exal", "benh", "bezo", "bon", "exal", "beno", "bend", "benazhe", "bendrox",
  sep = "|"
)
fluxt <- paste("fluox", "xetin", "xtin", "xitin", "floxc", "fluctotine", "fluox", "fluctine",
  sep = "|"
)
pheny <- paste("pheny", "toin", "yto", "pney", "plenton", "phentyo", "fenyoin", "phent",
  "phynetion", "ytion", "phylot", "phey", "phenston",
  sep = "|"
)
sodium <- paste("sod", "sodium", "valp", "Na+", "poric", "orate", "diam", "nav", "ssod",
  sep = "|"
)
barbito <- paste("pheno", "babi", "bito", "phemib", "phano", "nob", "p/b", "phreno", "preno",
  "phenab", "obab", "phanob", "pneno", "barbi",
  sep = "|"
)
tegret <- paste("tegre", "gret", "grit",
  sep = "|"
)
risper <- paste("risp", "resp", "risip", "resi", "ridone",
  sep = "|"
)
alpraz <- paste("alpraz", sep = "|")
vitbcomp <- paste("vitb", "complex", "mult", "vbc", "vit b", "vit  b",
  sep = "|"
)
diazep <- paste("diaz", "dias", "daiz", "diaepum", "dais",
  sep = "|"
)
olanzap <- paste("olanz", "olazapine",
  sep = "|"
)
prometh <- paste("prometh", "methazin",
  sep = "|"
)
imipra <- paste("imip", "immip", "pramin", "plamin", "imp", "immp",
  sep = "|"
)
clopi <- paste("pixol", "clopi", "hclopi",
  sep = "|"
)
donep <- paste("donez", "donep", "denop", "pezil", "zepil", "donap",
  sep = "|"
)
clonaz <- paste("clonaz", "chlonaz", "onazepam",
  sep = "|"
)
lamito <- paste("lamitor", "lomitor", "grine", "trig",
  sep = "|"
)


data_pr <- data_clean %>%
  mutate(mh_prescription = tolower(mh_prescription)) %>%
  mutate(
    mh_prescription_caba = if_else(
      str_detect(mh_prescription, cabs), "carbamzapine", ""
    ),
    mh_prescription_chlo = if_else(
      (str_detect(mh_prescription, chlo) & !str_detect(mh_prescription, "promet")),
      "cpz", ""
    ),
    mh_prescription_arta = if_else(
      str_detect(mh_prescription, artn), "artane", ""
    ),
    mh_prescription_amt = if_else(
      str_detect(mh_prescription, amit), "amitri", ""
    ),
    mh_prescription_hald = if_else(
      str_detect(mh_prescription, haldol), "haldol", ""
    ),
    mh_prescription_folic = if_else(
      str_detect(mh_prescription, foli), "folic acid", ""
    ),
    mh_prescription_trifluop = if_else(
      str_detect(mh_prescription, trifluop), "trifluoperazine", ""
    ),
    mh_prescription_benzho = if_else(
      str_detect(mh_prescription, benzh), "benzhhexol", ""
    ),
    mh_prescription_fluphena = if_else(
      str_detect(mh_prescription, fluphen), "fluphenazine", ""
    ),
    mh_prescription_fluoxet = if_else(
      str_detect(mh_prescription, fluxt), "fluoxetine", ""
    ),
    mh_prescription_phenytoin = if_else(
      str_detect(mh_prescription, pheny), "phenytoin", ""
    ),
    mh_prescription_sodiumval = if_else(
      str_detect(mh_prescription, sodium), "Sodium_val", ""
    ),
    mh_prescription_phenobarbit = if_else(
      str_detect(mh_prescription, barbito), "phenobarbitone", ""
    ),
    mh_prescription_tegretol = if_else(
      str_detect(mh_prescription, tegret), "tegretol", ""
    ),
    mh_prescription_risperid = if_else(
      str_detect(mh_prescription, risper), "risperidone", ""
    ),
    mh_prescription_alprazolam = if_else(
      str_detect(mh_prescription, alpraz), "alprazolam", ""
    ),
    mh_prescription_vitbcomplex = if_else(
      str_detect(mh_prescription, vitbcomp), "vitamin b complex", ""
    ),
    mh_prescription_diazepam = if_else(
      str_detect(mh_prescription, diazep), "diazepam", ""
    ),
    mh_prescription_olanzapine = if_else(
      str_detect(mh_prescription, olanzap), "olanzapine", ""
    ),
    mh_prescription_promethazine = if_else(
      str_detect(mh_prescription, prometh), "promethazine", ""
    ),
    mh_prescription_imipramine = if_else(
      str_detect(mh_prescription, imipra), "imipramine", ""
    ),
    mh_prescription_clopixol = if_else(
      str_detect(mh_prescription, clopi), "clopixol", ""
    ),
    mh_prescription_donepezil = if_else(
      str_detect(mh_prescription, donep), "donepezil", ""
    ),
    mh_prescription_clonazepam = if_else(
      str_detect(mh_prescription, clonaz), "clonazepam", ""
    ),
    mh_prescription_lamitor = if_else(
      str_detect(mh_prescription, lamito), "lamitor", ""
    )
  ) %>%
  select(
    mh_diagnosis,
    mh_prescription,
    mh_prescription_caba,
    mh_prescription_chlo,
    mh_prescription_arta,
    mh_prescription_amt,
    mh_prescription_hald,
    mh_prescription_folic, mh_prescription_trifluop, mh_prescription_benzho,
    mh_prescription_fluphena, mh_prescription_fluoxet, mh_prescription_phenytoin,
    mh_prescription_sodiumval, mh_prescription_phenobarbit, mh_prescription_tegretol,
    mh_prescription_risperid, mh_prescription_alprazolam, mh_prescription_vitbcomplex,
    mh_prescription_diazepam, mh_prescription_olanzapine,
    mh_prescription_promethazine, mh_prescription_imipramine,
    mh_prescription_clopixol, mh_prescription_donepezil, mh_prescription_clonazepam,
    mh_prescription_lamitor
  )
write_csv(data_pr, here::here("data", "data_prescription.csv"))
dfSummary(data_pr)
