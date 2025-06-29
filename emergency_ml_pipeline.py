# Emergency Department ML Pipeline
# Predicts both triage group and department using XGBoost
# Includes: data loading, preprocessing, model training, evaluation, feature importance, risk scoring, and staffing recommendation

# 1. Install required packages (uncomment if needed)
# !pip install xgboost scikit-learn pandas numpy matplotlib seaborn

# 2. Import libraries
import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix, roc_auc_score, accuracy_score
import matplotlib.pyplot as plt
import seaborn as sns

# 3. Load and prepare data
# Replace 'your_data.csv' with your actual data file
# Make sure to adjust feature and target names as needed

df = pd.read_csv('your_data.csv')

# Example: select features (customize as needed)
features = [
    'age', 'sex', 'vital_sign1', 'vital_sign2', 'symptom1', 'symptom2'
    # ... add all relevant features
]
X = df[features]
X = pd.get_dummies(X, drop_first=True)  # Encode categorical variables

# Targets
y_triage = df['triage_group']  # e.g., 1-5
y_dept = df['department']      # e.g., most visited department

# 4. Train/test split
X_train, X_test, y_triage_train, y_triage_test, y_dept_train, y_dept_test = train_test_split(
    X, y_triage, y_dept, test_size=0.2, random_state=42
)

# 5. Model training (XGBoost)
triage_model = xgb.XGBClassifier(objective='multi:softprob', num_class=5, eval_metric='mlogloss', use_label_encoder=False)
triage_model.fit(X_train, y_triage_train)

dept_model = xgb.XGBClassifier(objective='multi:softprob', eval_metric='mlogloss', use_label_encoder=False)
dept_model.fit(X_train, y_dept_train)

# 6. Predictions
y_triage_pred = triage_model.predict(X_test)
y_triage_proba = triage_model.predict_proba(X_test)

y_dept_pred = dept_model.predict(X_test)
y_dept_proba = dept_model.predict_proba(X_test)

# 7. Evaluation metrics
print('--- TRIAGE GROUP PREDICTION ---')
print(classification_report(y_triage_test, y_triage_pred))
print('Accuracy:', accuracy_score(y_triage_test, y_triage_pred))

plt.figure(figsize=(8,6))
sns.heatmap(confusion_matrix(y_triage_test, y_triage_pred), annot=True, fmt='d', cmap='Blues')
plt.title('Triage Group Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.show()

y_triage_test_bin = pd.get_dummies(y_triage_test)
auc_triage = roc_auc_score(y_triage_test_bin, y_triage_proba, multi_class='ovr')
print('AUC (Triage):', auc_triage)

print('--- DEPARTMENT PREDICTION ---')
print(classification_report(y_dept_test, y_dept_pred))
print('Accuracy:', accuracy_score(y_dept_test, y_dept_pred))

plt.figure(figsize=(10,8))
sns.heatmap(confusion_matrix(y_dept_test, y_dept_pred), annot=True, fmt='d', cmap='Greens')
plt.title('Department Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.show()

y_dept_test_bin = pd.get_dummies(y_dept_test)
auc_dept = roc_auc_score(y_dept_test_bin, y_dept_proba, multi_class='ovr')
print('AUC (Department):', auc_dept)

# 8. Feature importance
importances = triage_model.feature_importances_
indices = np.argsort(importances)[::-1]
feature_names = X.columns

print('Top 10 Features for Triage Prediction:')
for f in range(10):
    print(f'{f+1}. {feature_names[indices[f]]}: {importances[indices[f]]:.4f}')

# 9. Risk score calculation (for triage group 1)
risk_scores = y_triage_proba[:, 0]  # Probability for class 1
df_test = X_test.copy()
df_test['risk_score'] = risk_scores
print(df_test[['risk_score']].head())

# 10. Staffing recommendation (example)
# Suppose: 120 predicted patients for CT, 1 doctor can see 6 patients/hour, 8-hour shift
predicted_ct_patients = sum(y_dept_pred == 'CT')
patients_per_doctor_per_shift = 6 * 8
required_doctors = np.ceil(predicted_ct_patients / patients_per_doctor_per_shift)
print(f'Predicted CT patients: {predicted_ct_patients}')
print(f'Recommended number of doctors per shift: {required_doctors}') 