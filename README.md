# NYUS.2
NYUS.2 is an automated machine learning-empowered prediction model for grapevine freezing tolerance. This repo includes all the source code for feature extraction, model training and model deployment along with the original training data as parts of the __open-source__ study.
## Folders and files description
### [Raw data processing and feature extraction](https://github.com/imbaterry11/AutoLT50.1/tree/main/Raw%20data%20processing%20and%20feature%20extraction)
__Feature_extraction.R__ is an R script to extract features from daily temperature for the training and prediction of NYUS.2 <br>
__Cultivars.Rdata__ contains all the names for the Boolean-type cultivar columns <br>
__daily_temperature_data_example.csv__ is an example file to be processed by __Feature_extraction.R__ to generate features <br>
__daily_temperature_data_example_feature_extracted.csv__ is a resulting file that contains necessary features <br>
### [Training and feature importance quantification](https://github.com/imbaterry11/AutoLT50.1/tree/main/Training%20and%20feature%20importance%20quantification)
__Autogluon_model_training_feature_importance.ipynb__ is a notebook for the training of NYUS.2 and feature importance quantification <br>
__All_training_data_9_sites.csv__ is the entire LT50 dataset contributed by nine research facilities for NYUS.2 model training and testing <br>
![LT50_dataset_composition](https://github.com/imbaterry11/NYUS.2/assets/73162287/59875cde-8834-427f-93dc-eaadcd7be584)

### [Using model](https://github.com/imbaterry11/AutoLT50.1/tree/main/Using%20model) 
__NYUS.1_using_the_model.ipynb__ is a notebook for the prediction using NYUS.2 <br>
__daily_temperature_data_example_feature_extracted.csv__ is an example file that contains necessary features for prediction <br>
__LT50_pred.csv__ is the model prediction <br>
## Additional information:
Ready-to-use model can be download at [here](https://drive.google.com/drive/folders/1ZUXO9TCKzXt9-r7k1gZ5Oj0VDRyFb12N?usp=sharing). <br>
The current model was deployed at [the Cornell grape freezing tolerance prediction app](https://grapecoldhardiness.shinyapps.io/grape_freezing_tolerance/): <br>
![Shiny_app_current_UI](https://github.com/imbaterry11/NYUS.2/assets/73162287/1d3ffae6-62cb-48b1-bfd5-4d1ccfce2644)

Upon the use of the tools provided in this repo, please cite:  
