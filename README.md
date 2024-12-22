# **pmp_tm1_bonds**

This repository contains the files and data necessary for working on bond-related excess return strategies using R. Follow the instructions below to set up the repository in your RStudio environment and start working with the provided data.

---

## **Getting Started with This Repository in RStudio**

### **Step 1: Clone the Repository**
1. Open RStudio.
2. Go to **File > New Project > Version Control > Git**.
3. In the dialog box:
   - **Repository URL**: Paste the URL of this repository:  
     `https://github.com/tallosfarkas/pmp_tm1_bonds.git`
   - **Project Directory Name**: Choose a folder name for the project.
   - **Create Project As Subdirectory Of**: Select the folder where you'd like to save the project.
4. Click **Create Project**.

This will clone the repository and set it up as a new RStudio project.

---

### **Step 2: Open the Project**
1. After cloning, RStudio will open the project automatically.
2. If it doesn't, navigate to the folder you cloned and double-click the `.Rproj` file (e.g., `TM1_PortfolioStrategies.Rproj`).

---

### **Step 3: Install Required Libraries**
Ensure that the necessary R libraries are installed. Open the R Console and run the following:
```R
install.packages(c("tidyverse", "readxl"))
```
### **Step 4: Load the Data**
The data for this project is saved in an .RData file named Bond_all.RData. To load it into your R environment:

Open the file load_and_e_ret.R in RStudio.
Add the following code at the start of the file:
```R
load("Bond_all.RData")
```
Run the script to load the data into the R environment.
### **Step 5: Start Working**
The loaded data will now be available in your R environment. You can use the variables and objects within load_and_e_ret.R to calculate excess returns or analyze bond strategies. Follow the comments in the script for further steps.
