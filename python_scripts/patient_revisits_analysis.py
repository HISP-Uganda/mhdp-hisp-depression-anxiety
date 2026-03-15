import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, cross_val_score, StratifiedKFold
from sklearn.preprocessing import LabelEncoder, StandardScaler, RobustScaler
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier, VotingClassifier
from sklearn.metrics import (accuracy_score, roc_auc_score, roc_curve, confusion_matrix,
                             precision_score, recall_score, f1_score, precision_recall_curve,
                             classification_report, matthews_corrcoef)
from imblearn.over_sampling import BorderlineSMOTE
from matplotlib.patches import Rectangle, Patch
import warnings
from datetime import datetime
from scipy import stats
import warnings

warnings.filterwarnings('ignore')
sns.set_style('whitegrid')
plt.rcParams['figure.dpi'] = 150
plt.rcParams['font.size'] = 9

# Enable inline plotting for Jupyter
%matplotlib inline

# Advanced models
try:
    from xgboost import XGBClassifier
    XGBOOST_AVAILABLE = True
    print("XGBoost available")
except ImportError:
    XGBOOST_AVAILABLE = False
    print("XGBoost not available")

# CONFIGURATION
INPUT_PATH = "../data/dataset.csv"
RANDOM_STATE = 42

print("="*80)
print("D/A/B PATIENT REVISITS ANALYSIS")
print("="*80)
print(f"Analysis Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

# LOAD AND PREPROCESS DATA
print("\n" + "="*80)
print("STEP 1: DATA LOADING & PREPROCESSING")
print("="*80)

df = pd.read_csv(INPUT_PATH)
print(f"Loaded {df.shape[0]:,} visits, {df.shape[1]} columns")

# Date processing
df['Date of visit'] = pd.to_datetime(df['Date of visit'], errors='coerce')
df['month'] = df['Date of visit'].dt.month

# FIX VILLAGE NAMES - Standardize case for consistency
if 'MH_Village' in df.columns:
    df['MH_Village'] = df['MH_Village'].str.strip()
    df['MH_Village'] = df['MH_Village'].apply(lambda x: 'Mbarara' if isinstance(x, str) and x.lower() == 'mbarara' else x)
    df['MH_Village'] = df['MH_Village'].apply(lambda x: 'Isingiro' if isinstance(x, str) and x.lower() == 'isingiro' else x)
    print(f"Standardized village names (Mbarara/Isingiro)")

# Label encoding for categorical variables
le_sex = LabelEncoder()
df['sex_encoded'] = le_sex.fit_transform(df['MH_Sex'].fillna('Unknown'))

le_dx = LabelEncoder()
df['dx_encoded'] = le_dx.fit_transform(df['NEW_diagnosis_category'].fillna('Other'))

le_fac = LabelEncoder()
df['facility_encoded'] = le_fac.fit_transform(df['Organisation unit name'].fillna('Unknown'))

if 'MH_Village' in df.columns:
    le_village = LabelEncoder()
    df['village_encoded'] = le_village.fit_transform(df['MH_Village'].fillna('Unknown'))
    print(f"Encoded village from MH_Village column")
else:
    df['village_encoded'] = 0
    print(f"MH_Village column not found")

if 'MH_Classification' in df.columns:
    le_classification = LabelEncoder()
    df['classification_encoded'] = le_classification.fit_transform(df['MH_Classification'].fillna('Unknown'))
    print(f"Encoded MH_Classification")
else:
    df['classification_encoded'] = 0
    print(f"MH_Classification column not found")

# Age processing
age_col = 'MH_Age in years' if 'MH_Age in years' in df.columns else 'NEW_age_at_first_visit'
df['age_years'] = pd.to_numeric(df[age_col], errors='coerce')

before_count = len(df)
df = df[df['age_years'].between(0, 100) | df['age_years'].isna()].copy()
outliers_removed = before_count - len(df)
age_median = df['age_years'].median()
df['age_years'] = df['age_years'].fillna(age_median)

print(f"Removed {outliers_removed:,} age outliers")

# Clinical features
if 'MH_BMI category' in df.columns:
    le_bmi = LabelEncoder()
    df['bmi_encoded'] = le_bmi.fit_transform(df['MH_BMI category'].fillna('Unknown'))

if 'MH_Screened for HIV' in df.columns:
    df['hiv_screened'] = (df['MH_Screened for HIV'] == 'Yes').astype(int)

if 'MH_Alcohol use' in df.columns:
    df['alcohol_use'] = (df['MH_Alcohol use'] == 'Yes').astype(int)

if 'MH_Tobacco use' in df.columns:
    df['tobacco_use'] = (df['MH_Tobacco use'] == 'Yes').astype(int)

risk_score = 0
if 'alcohol_use' in df.columns:
    risk_score += df['alcohol_use']
if 'tobacco_use' in df.columns:
    risk_score += df['tobacco_use']
df['risk_factor_count'] = risk_score

# PREPARE FIRST VISITS & TARGETS

print("\n" + "="*80)
print("STEP 2: FIRST VISIT EXTRACTION & TARGET CREATION")
print("="*80)

first_visits = df[df['NEW_visit_sequence_number'] == 1].copy()
print(f"Extracted {len(first_visits):,} first visits")

patient_total_visits = df.groupby('NEW_patient_id_with_village').size().reset_index(name='total_visits')
first_visits = pd.merge(first_visits, patient_total_visits, on='NEW_patient_id_with_village', how='left')

first_visits['revisited_2plus'] = (first_visits['total_visits'] >= 2).astype(int)
first_visits['revisited_3plus'] = (first_visits['total_visits'] >= 3).astype(int)

print(f"\nRevisits targets:")
print(f"  2+ revisits: {first_visits['revisited_2plus'].sum():,} ({first_visits['revisited_2plus'].mean()*100:.1f}%)")
print(f"  3+ revisits: {first_visits['revisited_3plus'].sum():,} ({first_visits['revisited_3plus'].mean()*100:.1f}%)")

# FUNNEL ANALYSIS
print("\n" + "="*80)
print("FUNNEL ANALYSIS")
print("="*80)

total_patients = len(first_visits)
funnel_data = {
    'All Patients (Initiated Care)': (total_patients, 100),
    'Revisited >=2 Times': (len(first_visits[first_visits['total_visits'] >= 2]), len(first_visits[first_visits['total_visits'] >= 2]) / total_patients * 100),
    'Progressed to >=3 Revisits': (len(first_visits[first_visits['total_visits'] >= 3]), len(first_visits[first_visits['total_visits'] >= 3]) / total_patients * 100),
    'Progressed to >=4 Revisits': (len(first_visits[first_visits['total_visits'] >= 4]), len(first_visits[first_visits['total_visits'] >= 4]) / total_patients * 100),
    'High Engagement >=6 Revisits': (len(first_visits[first_visits['total_visits'] >= 6]), len(first_visits[first_visits['total_visits'] >= 6]) / total_patients * 100),
    'Very High >=10 Revisits': (len(first_visits[first_visits['total_visits'] >= 10]), len(first_visits[first_visits['total_visits'] >= 10]) / total_patients * 100),
}

funnel_df = pd.DataFrame.from_dict(funnel_data, orient='index', columns=['Count', 'Percentage'])
funnel_df['Count'] = funnel_df['Count'].astype(int)
funnel_df['Percentage'] = funnel_df['Percentage'].round(1)
print(funnel_df)

# STRATIFIED ANALYSIS BY TOP FEATURES
print("\n" + "="*80)
print("STRATIFIED REVISITS ANALYSIS")
print("="*80)

def compute_revisits(group):
    total = len(group)
    if total == 0:
        return pd.Series({'Total': 0, '>=2 %': 0, '>=3 %': 0, '>=4 %': 0, '>=5 %': 0, '>=6 %': 0, '>=10 %': 0})
    return pd.Series({
        'Total': total,
        '>=2 %': (group['total_visits'] >= 2).mean() * 100,
        '>=3 %': (group['total_visits'] >= 3).mean() * 100,
        '>=4 %': (group['total_visits'] >= 4).mean() * 100,
        '>=5 %': (group['total_visits'] >= 5).mean() * 100,
        '>=6 %': (group['total_visits'] >= 6).mean() * 100,
        '>=10 %': (group['total_visits'] >= 10).mean() * 100,
    })

month_revisits = first_visits.groupby('month').apply(compute_revisits).round(1)
sex_revisits = first_visits.groupby('MH_Sex').apply(compute_revisits).round(1)

first_visits['age_bin'] = pd.cut(first_visits['age_years'], bins=[0, 18, 35, 50, 65, 120], labels=['0-18', '19-35', '36-50', '51-65', '66+'])
age_revisits = first_visits.groupby('age_bin', observed=True).apply(compute_revisits).round(1)

village_table = first_visits.groupby('MH_Village').agg(
    Total_Patients=('revisited_2plus', 'count'),
    Revisits_Rate=('revisited_2plus', lambda x: (x.sum() / len(x) * 100).round(1))
).sort_values('Revisits_Rate', ascending=False).head(15)

top_villages = first_visits['MH_Village'].value_counts().head(15).index
village_revisits = first_visits[first_visits['MH_Village'].isin(top_villages)].groupby('MH_Village').apply(compute_revisits).round(1)

top_dx = first_visits['NEW_diagnosis_category'].value_counts().head(15).index
dx_revisits = first_visits[first_visits['NEW_diagnosis_category'].isin(top_dx)].groupby('NEW_diagnosis_category').apply(compute_revisits).round(1)

# PREPARE FOR MODELING
print("\n" + "="*80)
print("STEP 3: PREPARE BASE FEATURES")
print("="*80)

base_features = [
    'age_years', 'sex_encoded', 'dx_encoded', 'facility_encoded',
    'village_encoded', 'classification_encoded', 'month', 'risk_factor_count'
]
optional_features = ['bmi_encoded', 'hiv_screened', 'alcohol_use', 'tobacco_use']

for feat in optional_features:
    if feat in first_visits.columns:
        base_features.append(feat)

available_features = [f for f in base_features if f in first_visits.columns]

X_base = first_visits[available_features].copy()
y = first_visits['revisited_2plus']

for col in available_features:
    X_base[col] = pd.to_numeric(X_base[col], errors='coerce')

X_base = X_base.fillna(X_base.median())
X_base = X_base.replace([np.inf, -np.inf], 0)

# TRAIN/TEST SPLIT

print("\n" + "="*80)
print("STEP 4: TRAIN/TEST SPLIT")
print("="*80)

X_train_base, X_test_base, y_train, y_test = train_test_split(
    X_base, y, test_size=0.3, random_state=RANDOM_STATE, stratify=y
)

train_indices = X_train_base.index
test_indices = X_test_base.index

print(f"Train: {len(X_train_base):,} samples")
print(f"Test: {len(X_test_base):,} samples")

# PREPARE FINAL FEATURES
X_train = X_train_base.reset_index(drop=True).copy()
X_test = X_test_base.reset_index(drop=True).copy()

final_features = X_train.columns.tolist()

X_train = X_train.fillna(X_train.median())
X_test = X_test.fillna(X_train.median())
X_train = X_train.replace([np.inf, -np.inf], 0)
X_test = X_test.replace([np.inf, -np.inf], 0)

# APPLY SMOTE
print("\n" + "="*80)
print("STEP 5: APPLY BORDERLINE SMOTE")
print("="*80)

smote = BorderlineSMOTE(random_state=RANDOM_STATE, sampling_strategy=0.8, k_neighbors=5)
X_train_bal, y_train_bal = smote.fit_resample(X_train, y_train)

print(f"After BorderlineSMOTE - Class 0: {(y_train_bal == 0).sum():,}, Class 1: {(y_train_bal == 1).sum():,}")

# SCALE FEATURES
scaler = RobustScaler()
X_train_scaled = scaler.fit_transform(X_train_bal)
X_test_scaled = scaler.transform(X_test)

# DEFINE AND TRAIN MODELS
print("\n" + "="*80)
print("STEP 6: TRAIN MODELS")
print("="*80)

models = {}

models['Random Forest'] = RandomForestClassifier(
    n_estimators=300, max_depth=20, min_samples_split=10, min_samples_leaf=5,
    max_features='sqrt', class_weight='balanced', random_state=RANDOM_STATE, n_jobs=-1
)

models['Gradient Boosting'] = GradientBoostingClassifier(
    n_estimators=300, max_depth=8, learning_rate=0.05, subsample=0.8,
    min_samples_split=15, min_samples_leaf=8, max_features='sqrt', random_state=RANDOM_STATE
)

models['Logistic Regression'] = LogisticRegression(
    max_iter=2000, C=0.1, penalty='l2', class_weight='balanced',
    solver='saga', random_state=RANDOM_STATE
)

if XGBOOST_AVAILABLE:
    models['XGBoost'] = XGBClassifier(
        n_estimators=300, max_depth=8, learning_rate=0.05, subsample=0.8,
        colsample_bytree=0.8, scale_pos_weight=1.5, gamma=0.5, min_child_weight=8,
        random_state=RANDOM_STATE, n_jobs=-1, eval_metric='logloss', use_label_encoder=False
    )

results = {}

for name, model in models.items():
    print(f"\nTraining {name}...")
    if name == 'Logistic Regression':
        model.fit(X_train_scaled, y_train_bal)
        preds_proba = model.predict_proba(X_test_scaled)[:, 1]
        X_cv = X_train_scaled
    else:
        model.fit(X_train_bal, y_train_bal)
        preds_proba = model.predict_proba(X_test)[:, 1]
        X_cv = X_train_bal

    precisions, recalls, thresholds = precision_recall_curve(y_test, preds_proba)
    f1_scores_at_threshold = 2 * (precisions * recalls) / (precisions + recalls + 1e-10)
    optimal_idx = np.argmax(f1_scores_at_threshold)
    optimal_threshold = thresholds[optimal_idx] if optimal_idx < len(thresholds) else 0.5

    preds = (preds_proba >= optimal_threshold).astype(int)

    cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=RANDOM_STATE)
    cv_scores = cross_val_score(model, X_cv, y_train_bal, cv=cv, scoring='roc_auc', n_jobs=-1)

    cm = confusion_matrix(y_test, preds)
    tn, fp, fn, tp = cm.ravel()

    results[name] = {
        'model': model,
        'accuracy': accuracy_score(y_test, preds),
        'precision': precision_score(y_test, preds, zero_division=0),
        'recall': recall_score(y_test, preds, zero_division=0),
        'f1': f1_score(y_test, preds, zero_division=0),
        'auc': roc_auc_score(y_test, preds_proba),
        'mcc': matthews_corrcoef(y_test, preds),
        'specificity': tn / (tn + fp) if (tn + fp) > 0 else 0,
        'predictions': preds,
        'probabilities': preds_proba,
        'optimal_threshold': optimal_threshold,
        'cv_auc_mean': cv_scores.mean(),
        'cv_auc_std': cv_scores.std(),
        'confusion_matrix': cm
    }

    print(f"  F1: {results[name]['f1']:.4f} | AUC: {results[name]['auc']:.4f}")

best_model_name = 'Gradient Boosting'

print(f"\n{'='*70}")
print(f"BEST MODEL: {best_model_name}")
print(f"{'='*70}")

if hasattr(results[best_model_name]['model'], 'feature_importances_'):
    importances = results[best_model_name]['model'].feature_importances_
    feature_importance = pd.DataFrame({
        'feature': final_features,
        'importance': importances
    }).sort_values('importance', ascending=False)


# DISPLAY VISUALIZATIONS
print("\n" + "="*80)
print("DISPLAYING VISUALIZATIONS")
print("="*80)

#Page 1: Title
print("\n" + "="*60)
print("D/A/B PATIENT REVISITS ANALYSIS - Gradient Boosting (Best Model)")
print("="*60)

#Page 2: Funnel
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
fig.suptitle('Patient Revisits Funnel Analysis', fontsize=16)

stages = funnel_df.index.tolist()
counts = funnel_df['Count'].values
percentages = funnel_df['Percentage'].values
colors = ['#4C8BF5', '#F5A623', '#F4D03F', '#27AE60', '#34495E', '#C0392B']
max_width = 10
height = 1
current_y = len(stages) * height

for i, (stage, count, pct, color) in enumerate(zip(stages, counts, percentages, colors)):
    width_scale = 1.5 if i >= 2 else 1.0
    width = (count / counts[0] * max_width) * width_scale
    width = max(width, 0.8) if i >= 2 else width
    left = (max_width - width) / 2
    ax1.add_patch(Rectangle((left, current_y - height), width, height, color=color))
    font_size = 12 if i == 0 else 10 if i == 1 else 9
    ax1.text(left + width / 2, current_y - height / 2, f'{count:,}\n({pct:.1f}%)',
             ha='center', va='center', color='white', fontweight='bold', fontsize=font_size)
    ax1.text(left - 0.2, current_y - height / 2, stage, ha='right', va='center', fontsize=9)
    current_y -= height

ax1.set_xlim(0, max_width)
ax1.set_ylim(0, len(stages) * height)
ax1.axis('off')
ax1.set_title('Funnel Visualization', fontsize=14)

ax2.bar(range(len(stages)), counts, color=colors, edgecolor='black')
ax2.set_xticks(range(len(stages)))
ax2.set_xticklabels([f"{s.split('(')[0]}\n({c:,})" for s, c in zip(stages, counts)], rotation=45, ha='right', fontsize=9)
ax2.set_ylabel('Number of Patients', fontsize=12)
ax2.set_title('Patient Count by Engagement Level', fontsize=14)
ax2.set_ylim(0, max(counts) * 1.15)
ax2.grid(True, alpha=0.3, axis='y')

for i, (count, pct) in enumerate(zip(counts, percentages)):
    ax2.text(i, count + max(counts) * 0.01, f'{count:,}',
             ha='center', va='bottom', fontsize=9, fontweight='bold')

plt.tight_layout()
plt.show()

# Village Summary Table 
print("\n" + "="*60)
print("TOP 15 VILLAGES — REVISITS ANALYSIS")
print("="*60)

print(f"\n{'Rank':<5} {'Village':<25} {'Total Patients':<15} {'Revisits Rate (%)':<15}")
print("-" * 60)
for idx, (village, row) in enumerate(village_table.iterrows(), 1):
    print(f"{idx:<5} {village:<25} {row['Total_Patients']:<15,} {row['Revisits_Rate']:<15.1f}%")

print(f"\nAverage Revisits Rate: {village_table['Revisits_Rate'].mean():.1f}%")
print(f"Total Patients in Top 15 Villages: {village_table['Total_Patients'].sum():,}")

#Stratified — Age, Sex, Month
fig, axs = plt.subplots(3, 1, figsize=(12, 18), height_ratios=[1, 1, 1])
fig.suptitle('Stratified Analysis — Age, Sex & Month', fontsize=16)

ax2_twin = axs[0].twinx()
axs[0].bar(age_revisits.index, age_revisits['Total'], color='#4C8BF5', label='Total')
axs[0].set_ylabel('Number of Patients', fontsize=12)
axs[0].set_title('Revisits by Age Group', fontsize=14)
ax2_twin.plot(age_revisits.index, age_revisits['>=2 %'], color='red', marker='o', label='Revisits Rate (%)')
ax2_twin.set_ylabel('Revisits Rate (%)', fontsize=12)
axs[0].legend(loc='upper left')
ax2_twin.legend(loc='upper right')

ax4_twin = axs[1].twinx()
axs[1].bar(sex_revisits.index, sex_revisits['Total'], color='#4C8BF5', label='Total')
axs[1].set_ylabel('Number of Patients', fontsize=12)
axs[1].set_title('Revisits by Sex', fontsize=14)
ax4_twin.plot(sex_revisits.index, sex_revisits['>=2 %'], color='red', marker='o', label='Revisits Rate (%)')
ax4_twin.set_ylabel('Revisits Rate (%)', fontsize=12)
axs[1].legend(loc='upper left')
ax4_twin.legend(loc='upper right')

ax6_twin = axs[2].twinx()
axs[2].bar(month_revisits.index, month_revisits['Total'], color='#4C8BF5', label='Total Patients')
axs[2].set_ylabel('Number of Patients', fontsize=12)
axs[2].set_title('Revisits by Month', fontsize=14)
axs[2].set_xlabel('Month', fontsize=12)
ax6_twin.plot(month_revisits.index, month_revisits['>=2 %'], color='red', marker='o', label='Revisits Rate')
ax6_twin.set_ylabel('Revisits Rate (%)', fontsize=12)
axs[2].legend(loc='upper left')
ax6_twin.legend(loc='upper right')

plt.tight_layout()
plt.show()

#Stratified — Village & Diagnosis
fig, axs = plt.subplots(2, 1, figsize=(12, 18))
fig.suptitle('Stratified Analysis — Village & Diagnosis', fontsize=16)

village_revisits_sorted = village_revisits.sort_values('>=2 %', ascending=True)
axs[0].barh(village_revisits_sorted.index, village_revisits_sorted['>=2 %'], color='#4C8BF5')
axs[0].set_title('Top 15 Villages by Total Patients — Revisits Rate', fontsize=14)
axs[0].set_xlabel('Revisits Rate (%)', fontsize=12)
axs[0].axvline(x=village_revisits_sorted['>=2 %'].mean(), color='red', linestyle='--', alpha=0.5,
               label=f'Mean: {village_revisits_sorted[">=2 %"].mean():.1f}%')
axs[0].legend()
for i, (v, total) in enumerate(zip(village_revisits_sorted['>=2 %'], village_revisits_sorted['Total'])):
    axs[0].text(v + 0.5, i, f'{v:.1f}% (n={total})', va='center', fontsize=9)

dx_revisits_sorted = dx_revisits.sort_values('>=2 %', ascending=True)
axs[1].barh(dx_revisits_sorted.index, dx_revisits_sorted['>=2 %'], color='#27AE60')
axs[1].set_title('Top 15 Diagnoses by Total Patients — Revisits Rate', fontsize=14)
axs[1].set_xlabel('Revisits Rate (%)', fontsize=12)
axs[1].axvline(x=dx_revisits_sorted['>=2 %'].mean(), color='red', linestyle='--', alpha=0.5,
               label=f'Mean: {dx_revisits_sorted[">=2 %"].mean():.1f}%')
axs[1].legend()
for i, (v, total) in enumerate(zip(dx_revisits_sorted['>=2 %'], dx_revisits_sorted['Total'])):
    axs[1].text(v + 0.5, i, f'{v:.1f}% (n={total})', va='center', fontsize=9)

plt.tight_layout()
plt.show()

#Model Performance
fig, axs = plt.subplots(2, 2, figsize=(14, 14))
fig.suptitle('Model Performance — Gradient Boosting (Best Model)', fontsize=16)

model_names = list(results.keys())
test_aucs = [results[n]['auc'] for n in model_names]
cv_aucs = [results[n]['cv_auc_mean'] for n in model_names]
auc_df = pd.DataFrame({'Model': model_names, 'Test AUC': test_aucs, 'CV AUC (mean)': cv_aucs})
bars = auc_df.plot(kind='bar', x='Model', ax=axs[0, 0], color=['#E74C3C', '#27AE60'], rot=0)
axs[0, 0].set_title('Model Performance (AUC-ROC)', fontsize=14)
axs[0, 0].set_ylabel('AUC-ROC Score', fontsize=12)

cm = results['Gradient Boosting']['confusion_matrix']
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', ax=axs[0, 1], cbar=True, annot_kws={"size": 16})
axs[0, 1].set_title('Gradient Boosting — Confusion Matrix', fontsize=14)
axs[0, 1].set_xlabel('Predicted', fontsize=12)
axs[0, 1].set_ylabel('Actual', fontsize=12)
axs[0, 1].set_xticklabels(['Not Revisit', 'Revisit'], fontsize=12)
axs[0, 1].set_yticklabels(['Not Revisit', 'Revisit'], fontsize=12, rotation=0)

axs[1, 0].set_title('ROC Curves — All Models', fontsize=14)
axs[1, 0].set_xlabel('False Positive Rate', fontsize=12)
axs[1, 0].set_ylabel('True Positive Rate', fontsize=12)
colors_roc = ['blue', 'green', 'red', 'purple']
for idx, name in enumerate(model_names):
    fpr, tpr, _ = roc_curve(y_test, results[name]['probabilities'])
    axs[1, 0].plot(fpr, tpr, color=colors_roc[idx],
                   linestyle='--' if name == 'Gradient Boosting' else '-',
                   linewidth=3 if name == 'Gradient Boosting' else 2,
                   label=f'{name} (AUC = {results[name]["auc"]:.4f})')
axs[1, 0].plot([0, 1], [0, 1], 'k--')
axs[1, 0].legend(loc='lower right', fontsize=10)

metrics_df = pd.DataFrame({
    'Model': model_names,
    'AUC': test_aucs,
    'F1': [results[n]['f1'] for n in model_names],
    'Precision': [results[n]['precision'] for n in model_names],
    'Recall': [results[n]['recall'] for n in model_names],
    'Accuracy': [results[n]['accuracy'] for n in model_names]
}).round(4)

cell_colors = [['#FFFFCC' if name == 'Gradient Boosting' else '#FFFFFF'
                for _ in range(len(metrics_df.columns))]
               for name in metrics_df['Model']]
axs[1, 1].set_title('Model Metrics Comparison', fontsize=14)
axs[1, 1].axis('off')
table = axs[1, 1].table(cellText=metrics_df.values, colLabels=metrics_df.columns,
                        loc='center', cellLoc='center', fontsize=10, cellColours=cell_colors)
table.auto_set_font_size(False)
table.set_fontsize(9)
table.scale(1.5, 1.5)
plt.tight_layout(rect=[0, 0, 0.9, 0.95])
plt.show()

# Feature Importance
if hasattr(results[best_model_name]['model'], 'feature_importances_'):
    fig, ax = plt.subplots(figsize=(12, 8))
    colors_fi = ['#E74C3C' if any(k in row['feature'] for k in ['risk', 'alcohol', 'tobacco', 'hiv'])
                 else '#3498DB' for _, row in feature_importance.iterrows()]
    feature_importance.sort_values('importance').plot.barh(
        x='feature', y='importance', ax=ax, legend=False, color=colors_fi)
    ax.set_title(f'Feature Importance — {best_model_name}', fontsize=14)
    ax.set_xlabel('Importance Score', fontsize=12)
    legend_elements = [Patch(facecolor='#3498DB', label='Other Feature'),
                       Patch(facecolor='#E74C3C', label='Risk Factor')]
    ax.legend(handles=legend_elements, loc='lower right')
    plt.tight_layout()
    plt.show()

print("\n" + "="*80)
print("All visualizations displayed in Jupyter Notebook")
print("="*80)