cat > python_scripts/README.md << 'EOF'
# Python Scripts — DAB Patient Revisits Analysis

This folder contains Python scripts for analysing depression, anxiety, and bipolar (DAB)
patient revisit patterns using mental health data from Uganda's DHIS2 system.

## Script

### patient_revisits_analysis.py
Predicts whether a patient will return for follow-up care after their first visit,
using ensemble machine learning models.

## Setup
pip install -r requirements.txt

## Usage
1. Export your DHIS2 mental health dataset as a CSV file
2. Set INPUT_PATH in the script to your filename
3. Run: python patient_revisits_analysis.py

## Contributor
Awinosarah — Uganda Digital Mental Health Programme (MHDP)
