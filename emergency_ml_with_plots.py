# emergency_ml_with_plots.py
# ML pipeline for converted.csv: triage and department prediction, evaluation, and plots

import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix, roc_auc_score, accuracy_score
from sklearn.preprocessing import LabelEncoder
import matplotlib.pyplot as plt
import seaborn as sns

# 1. Load data with semicolon delimiter and encoding fallback, skip bad lines
print('Loading data...')
try:
    df = pd.read_csv('converted.csv', sep=';', encoding='utf-8', on_bad_lines='skip')
except UnicodeDecodeError:
    try:
        df = pd.read_csv('converted.csv', sep=';', encoding='latin1', on_bad_lines='skip')
    except Exception:
        df = pd.read_csv('converted.csv', sep=';', encoding='ISO-8859-9', on_bad_lines='skip')

# 2. Feature engineering
print('Preprocessing features...')
for col in ['visit.date', 'birth.date']:
    if col in df.columns:
        df[col] = pd.to_datetime(df[col], errors='coerce')
if 'visit.date' in df.columns and 'birth.date' in df.columns:
    df['age'] = ((df['visit.date'] - df['birth.date']).dt.days / 365.25).astype(int)
if 'visit.date' in df.columns:
    df['visit_month'] = df['visit.date'].dt.month
if 'sex' in df.columns:
    df['sex'] = df['sex'].map({'M': 0, 'W': 1}).fillna(-1).astype(int)

# Select relevant features
features = [
    'age', 'sex', 'visit_month',
    'heart.rate', 'bp.sys', 'o2.saturation', 'body.temperatur',
    'sonography.date', 'echo.cardiography.date', 'ecg.date', 'laboratory.date', 'test.urine.date',
    'cct.date', 'ct.date', 'mri.date', 'trauma.scan.date', 'xray.extremity.date', 'xray.basin.date',
    'xray.spine.date', 'xray.thorax.date', 'xray.misc.date'
]
features = [f for f in features if f in df.columns]
print('Selected features:', features)

# Convert department date columns to binary (visited or not)
for col in features:
    if col.endswith('.date'):
        df[col] = df[col].notna().astype(int)

X = df[features]

# Targets
if 'triage' in df.columns:
    # Extract numeric part from triage strings like "1 ROT", "4 GRÜN"
    y_triage = df['triage'].astype(str).str.extract(r'(\d+)')[0].astype(float)
    print('Triage value distribution:')
    print(y_triage.value_counts().sort_index())
    # Filter out triage value 6 and convert to 0-4 for proper indexing
    y_triage = y_triage[y_triage <= 5]
    y_triage = y_triage - 1  # Convert 1-5 to 0-4
else:
    raise ValueError("No 'triage' column found in the data.")

dept_cols = [c for c in features if c.endswith('.date')]
if dept_cols:
    df['main_department'] = df[dept_cols].idxmax(axis=1)
    y_dept = df['main_department']
else:
    y_dept = None

# Remove rows with missing targets and filter X accordingly
mask = y_triage.notna() & y_dept.notna()
print('Rows before filtering:', len(X))
print('Rows with non-missing triage:', y_triage.notna().sum())
print('Rows with non-missing department:', y_dept.notna().sum())
print('Rows after filtering:', mask.sum())
if mask.sum() == 0:
    print('No samples left after filtering!')
    print('Missing triage:', y_triage.isna().sum())
    print('Missing department:', y_dept.isna().sum())
    import sys; sys.exit(1)
X = X[mask]
y_triage = y_triage[mask]
y_dept = y_dept[mask]

print('Final triage distribution (0-4):')
print(y_triage.value_counts().sort_index())
print('Number of unique departments:', y_dept.nunique())

# Encode department labels before split
dept_encoder = LabelEncoder()
y_dept_encoded = dept_encoder.fit_transform(y_dept)

# 3. Train/test split with stratification
X_train, X_test, y_triage_train, y_triage_test, y_dept_train_encoded, y_dept_test_encoded = train_test_split(
    X, y_triage, y_dept_encoded, test_size=0.2, random_state=42, stratify=y_triage
)

# 4. Model training (XGBoost)
print('Training triage model...')
triage_model = xgb.XGBClassifier(objective='multi:softprob', num_class=5, eval_metric='mlogloss')
triage_model.fit(X_train, y_triage_train)

print('Training department model...')
dept_model = xgb.XGBClassifier(objective='multi:softprob', eval_metric='mlogloss')
dept_model.fit(X_train, y_dept_train_encoded)

# 5. Predictions
y_triage_pred = triage_model.predict(X_test)
y_triage_proba = triage_model.predict_proba(X_test)
y_dept_pred_encoded = dept_model.predict(X_test)
y_dept_proba = dept_model.predict_proba(X_test)
y_dept_pred = dept_encoder.inverse_transform(y_dept_pred_encoded)
y_dept_test_decoded = dept_encoder.inverse_transform(y_dept_test_encoded)

# 6. Evaluation metrics
print('--- TRIAGE GROUP PREDICTION ---')
print(classification_report(y_triage_test, y_triage_pred))
print('Accuracy:', accuracy_score(y_triage_test, y_triage_pred))
y_triage_test_bin = pd.get_dummies(y_triage_test)
auc_triage = roc_auc_score(y_triage_test_bin, y_triage_proba, multi_class='ovr')
print('AUC (Triage):', auc_triage)

print('--- DEPARTMENT PREDICTION ---')
print(classification_report(y_dept_test_decoded, y_dept_pred))
print('Accuracy:', accuracy_score(y_dept_test_decoded, y_dept_pred))

# Fix AUC calculation for departments
try:
    y_dept_test_bin = pd.get_dummies(y_dept_test_decoded)
    # Ensure the one-hot encoding matches the probability array
    if y_dept_test_bin.shape[1] == y_dept_proba.shape[1]:
        auc_dept = roc_auc_score(y_dept_test_bin, y_dept_proba, multi_class='ovr')
        print('AUC (Department):', auc_dept)
    else:
        print('AUC (Department): Cannot calculate - dimension mismatch')
        auc_dept = None
except Exception as e:
    print('AUC (Department): Error calculating -', str(e))
    auc_dept = None

# 7. Save plots
print('Saving plots...')
plt.figure(figsize=(8,6))
sns.heatmap(confusion_matrix(y_triage_test, y_triage_pred), annot=True, fmt='d', cmap='Blues')
plt.title('Triage Group Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.savefig('triage_confusion_matrix.png')
plt.close()

plt.figure(figsize=(10,8))
sns.heatmap(confusion_matrix(y_dept_test_decoded, y_dept_pred), annot=True, fmt='d', cmap='Greens')
plt.title('Department Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.savefig('department_confusion_matrix.png')
plt.close()

importances = triage_model.feature_importances_
indices = np.argsort(importances)[::-1]
feature_names = X.columns
plt.figure(figsize=(10,6))
sns.barplot(x=importances[indices][:10], y=np.array(feature_names)[indices][:10], hue=np.array(feature_names)[indices][:10], legend=False, palette='viridis')
plt.title('Top 10 Features for Triage Prediction')
plt.xlabel('Importance')
plt.ylabel('Feature')
plt.savefig('triage_feature_importance.png')
plt.close()

risk_scores = y_triage_proba[:, 0]  # Probability for class 1
plt.figure(figsize=(8,5))
sns.histplot(risk_scores, bins=30, kde=True, color='red')
plt.title('Distribution of Risk Scores for Triage Group 1')
plt.xlabel('Risk Score (Probability of Group 1)')
plt.ylabel('Number of Patients')
plt.savefig('triage_risk_score_distribution.png')
plt.close()

# Additional risk score plots for Triage Groups 2, 3, 4, 5
triage_colors = ['red', 'orange', 'yellow', 'green', 'blue']
triage_names = ['Triage Group 1', 'Triage Group 2', 'Triage Group 3', 'Triage Group 4', 'Triage Group 5']

for i in range(5):
    risk_scores = y_triage_proba[:, i]  # Probability for each class
    plt.figure(figsize=(8,5))
    sns.histplot(risk_scores, bins=30, kde=True, color=triage_colors[i])
    plt.title(f'Distribution of Risk Scores for {triage_names[i]}')
    plt.xlabel(f'Risk Score (Probability of Group {i+1})')
    plt.ylabel('Number of Patients')
    plt.savefig(f'triage_risk_score_group_{i+1}.png')
    plt.close()

# Combined risk score plot for all triage groups
plt.figure(figsize=(12,8))
for i in range(5):
    risk_scores = y_triage_proba[:, i]
    sns.histplot(risk_scores, bins=30, kde=True, color=triage_colors[i], alpha=0.6, label=triage_names[i])
plt.title('Combined Risk Score Distribution for All Triage Groups')
plt.xlabel('Risk Score (Probability)')
plt.ylabel('Number of Patients')
plt.legend()
plt.savefig('triage_risk_score_all_groups.png')
plt.close()

print('All ML evaluation plots have been saved as PNG files in the current directory.') 