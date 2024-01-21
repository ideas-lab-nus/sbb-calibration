# sbb-calibration
This repository contains the research compendium for our latest work, "Evaluating Different Levels of Information on the Calibration of Building Energy Simulation Models." This study investigates and quantifies the effect of different levels of information on the accuracy and predictive 
performance of calibrated models. It incorporates field measurements, evidence-based sequential calibration, parametric building energy simulations, and a robustness test on the results across different operating conditions.

The compendium includes all the data and code needed to reproduce the analysis. More details can be found in our paper:

> xxx, xxx, xxx, xxx and xxx, (2024).
> *Evaluating Different Levels of Information on the Calibration of Building Energy Simulation Models*.
> *Building Simulation*. <https://doi.org/xxx/xxx>


## Citation

Please cite this compendium as:
```
@article{2024evaluating,
  title={Evaluating Different Levels of Information on the Calibration of Building Energy Simulation Models},
  author={xxx},
  year={2024},
  note={In Revision}
}
```

## Repository Structure

```
./
├── requirement.txt                               # File containing the list of Python packages the project uses
├── R                                             # 
│   ├── measurement.R                             # R script for measurement data preprocessing, including ACH calculation & thermal conductance cal & EPW weather file generation
│   ├── uncertainty.R                             # R script for measurement uncertainty analysis
│   ├── baseline_idfs.Rmd                         # R script for baseline EnergyPlus model development by incorporating different levels of information 
│   ├── model_accuracy_validation.R               # R script for validating the predictive accuracy of Models 1 to 6 against measured data
│   └── ECM_sim.R                                 # R script for ECM analysis using parametric simulations
|   └── model_evaluation.R                        # R script for model predictive performance evaluation and result visualization
├── data                                          # 
│   ├── csv                                       # Dataset summarizing papers reviewed
│   │   ├── sf6.csv                               # SF6 concentration measurements
│   │   ├── heat_flux.csv                         # Heat-flux sensor measurements
│   │   ├── pi_data.csv                           # Energy Management System data and logged data set during the experiment period
│   │   └── experiment_schedule.csv               # Experiment schedule
│   └── idf                                       # 
│       ├── epwfiles                              # EnergyPlus weather files
│       │   └── ...                               # 
│       ├── iddfiles                              # IDD files
│       │   └── ...                               # 
│       └── idffiles                              # Baseline IDFs
│           └── ...                               #                               
└── paper                                         # 
    ├── figures                                   # Figures used in the paper
    │   └── ...                                   #
    ├── tex                                       # LaTeX source document
    └── bib                                       # Bibliographic information file 
```

### Licenses

**Text and figures :**  [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [LICENSE](LICENSE) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) attribution requested in reuse
