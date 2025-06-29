# save_ml_plots.py
# This script assumes you have already run emergency_ml_pipeline.py and have the following variables in memory:
# y_triage_test, y_triage_pred, y_dept_test, y_dept_pred, importances, indices, feature_names, risk_scores

import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import confusion_matrix
import numpy as np

# 1. Triage Group Confusion Matrix
plt.figure(figsize=(8,6))
sns.heatmap(confusion_matrix(y_triage_test, y_triage_pred), annot=True, fmt='d', cmap='Blues')
plt.title('Triage Group Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.savefig('triage_confusion_matrix.png')
plt.close()

# 2. Department Confusion Matrix
plt.figure(figsize=(10,8))
sns.heatmap(confusion_matrix(y_dept_test, y_dept_pred), annot=True, fmt='d', cmap='Greens')
plt.title('Department Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.savefig('department_confusion_matrix.png')
plt.close()

# 3. Feature Importance Plot (Triage)
plt.figure(figsize=(10,6))
sns.barplot(x=importances[indices][:10], y=np.array(feature_names)[indices][:10], palette='viridis')
plt.title('Top 10 Features for Triage Prediction')
plt.xlabel('Importance')
plt.ylabel('Feature')
plt.savefig('triage_feature_importance.png')
plt.close()

# 4. Risk Score Distribution (Triage Group 1)
plt.figure(figsize=(8,5))
sns.histplot(risk_scores, bins=30, kde=True, color='red')
plt.title('Distribution of Risk Scores for Triage Group 1')
plt.xlabel('Risk Score (Probability of Group 1)')
plt.ylabel('Number of Patients')
plt.savefig('triage_risk_score_distribution.png')
plt.close()

print('All ML evaluation plots have been saved as PNG files in the current directory.') 