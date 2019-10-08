# Neuropsychological-Assessment

This repository has been created to give an overview of my project at **UMC (Utrecht Medical Center)**.

I've been working on raw *JSON* data that are linked to personal patients' information, therefore for privacy reasons these cannot be shared. 

The main steps of the projects were the following:

- As there was a single *JSON* file for each patient and for each test (190 patients, 17 tests), the first step was to extract useful information from the *JSON* data into *CSV* files. Analyses have followed using *RStudio*.

- The file `ExampleRawFile_OnePatient_OneTest.json` provides an example of the raw file for one patient and one test.
Have a look at the script `RAVLT_data_extraction.R` to have an example of how I extracted raw _JSON_ data into a _.csv_ file for one specific test.

- `RAVLT_file_structure.png` shows the data structure of the *JSON* files for the test _RAVLT_.

- Working closely together with the neuropsychologists that were leading the project, I've developed specific new outcome measures for each test. The goal was to measure the performance fluctuation over time, with the goal to define the kind and gravity of each patient's impairment.

- The script `Slower_comparable_faster.R` gives an idea of how analyses were performed.

- Results have been summarised as to be presented to the neuropsychologists working at the project as both *.csv* files and *.pptx* files with useful visualisations. 
`RAVLT_timebetweenwords_summary.pdf` and `RAVLT_patients_range_example.pdf` are examples of that.

