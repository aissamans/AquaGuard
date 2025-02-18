# AquaGuard  
## _Intelligent Leak Detection Automation with R for Lydec Company (Industrial Engineering Final Year Project)_

AquaGuard is an intelligent leak detection system developed as part of my Engineering Graduation Project and final internship for my state engineering diploma at LYDEC—Lyonnaise des Eaux de Casablanca in Morocco, the operator of Casablanca’s 4,500 km water distribution network. This repository contains *R scripts* that integrate data correction, forecasting, data mining, and ML-driven anomaly detection to optimize network performance and reduce water losses.

---

## Repository Structure

- **forecasting.R**  
  Implements time series analysis to predict water flow and detect potential leaks.

- **correction.R**  
  Cleans and estimates missing or erroneous water flow data.

- **clustering_kmeans.R**  
  Applies the K-Means clustering algorithm for exploratory data mining.

- **random_forest_classification.R**  
  Employs a Random Forest model to predict clusters for anomaly detection.

---

## Project Overview

- **Objective:**  
  Automate leak detection by integrating data correction, forecasting, and data mining.

- **Methods:**  
  - **Data Correction:** Clean data and handle missing or negative values.  
  - **Forecasting:** Use time series analysis (`auto.arima`) to predict water flow.  
  - **Data Mining:** Employ K-Means clustering and Random Forest classification for anomaly detection.

- **Outcome:**  
  A web application was built to visualize predictive insights and enhance operational efficiency at LYDEC.

---

## How to Run the Scripts

1. **Prerequisites:**  
   - R (and optionally RStudio) installed on your computer.  
   - Required packages: `forecast`, `randomForest`, and `qcc`. Install them with:
     ```r
     install.packages(c("forecast", "randomForest", "qcc"))
     ```

2. **Running a Script:**  
   - Open a script (e.g., `forecasting.R`) in R or RStudio.  
   - Adjust file paths in the script as needed to point to your data files.  
   - Run the script to generate outputs (e.g., CSV files with predictions or corrected data).

---

## License

The code is available for personal/educational use.

## Contact

Feel free to reach out if you have any questions or would like to discuss collaboration opportunities.

---

Thank you for visiting my repository!
