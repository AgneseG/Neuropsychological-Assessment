# Neuropsychological-Assessment

This repository has been created to give an overview of my project at **UMC (Utrecht Medical Center)**.

I've been working on raw *JSON* data that are linked to personal patients' information, therefore for privacy reasons these cannot be shared. The main steps of the projects were the following:

- As there was a single *JSON* file for each patient and for each test (190 patients, 17 tests), the first step was to extract useful information from the *JSON* data into *CSV* files. From there, analyses have followed using *RStudio*.

- Working closely together with the neuropsychologists that were leading the project, I've developed specific new outcome measures for each test. The goal was to measure the performance fluctuation over time, with the goal to define the kind and gravity of each patient's impairment.

- Results have been summarised as to be presented to the neuropsychologists working at the project as both *.csv* files and *.pptx* files with useful visualisations.


- Have a look at the script `RAVLT_data_extraction.R` to have an example of how I extracted raw _JSON_ data into a _.csv_ file for one specific test, or look at `Slower_comparable_faster.R` to get an idea of how analyses were performed afterwards.
