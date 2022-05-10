<!-- Add banner here -->
![Banner](https://github.com/camille-alice/MARS_DATA3888_reefC4/blob/main/ReefC4_banner.png)
<!-- Contacts -->

&nbsp; 

# Investigating Coral Bleaching 

Repository for MARS/DATA3888 Reef Project for group C4. 

### Table of Contents

- [Project Description](#project-description)
- [Ethical Considerations](#ethical-considerations)
    - [Why It Matters](#why-it-matters)
    - [Stakeholder Analysis](#stakeholder-analysis)
    - [Raw Data](#raw-data)
- [Key Files](#key-files)
- [Project Deliverables](#project-deliverables)
    - [Presentation](#presentation)
    - [Report](#report)
- [Variables](#variables)
- [Group members and contacts](#group-members-and-contacts)
- [References](#references)
- [License](#license)

&nbsp;



# Project Description

Coral bleaching is when coral reefs, under environmental stress (such as change in temperature), release algae in their tissues, causing them to turn white. Bleached corals become susceptible to disease and vulnerable to death.

Purpose: We are researching Coral Bleaching for DATA/MARS3888 at the University of Sydney.

Study aim:

- Investigating rugosity of coral and the effect on bleaching severity and frequency. 

- Predicting which Reef is likely to be suspectable to bleaching

- Build interactive Shiny App to display predictions 

The bleaching data used is from a public dataset taken from the paper ["A global analysis of coral bleaching over the past twodecades"](https://doi.org/10.1038/s41467-019-09238-2) published in Nature communications in 2019. Authors are: Sully, S., Burkepile, D. E., Donovan, M. K., Hodgson, G., & Van Woesik, R. The author has curated coral bleaching events at 3351 sites in 81 countries from 1998 to 2017 and a suite of environmental variables at each site. The raw data can be found at the [Reef Check website](https://www.reefcheck.org/) or on the [GitHub repository for the Institute for Global Ecology](https://github.com/InstituteForGlobalEcology/Coral-bleaching-a-global-analysis-of-the-past-two-decades).

We also used data gathered from [Allen Coral Atlas](https://allencoralatlas.org/atlas/) to build our rugosity dataset. Full extraction method can be found [here](https://github.com/camille-alice/MARS_DATA3888_reefC4/blob/main/camille_ida.Rmd). *Note: The full dataset was downloaded locally to be processed as the zipped archive was over 13GB* 

&nbsp; 
# Ethical Considerations

(Not sure if necessary) 

## Why It Matters

(Importance of predicting coral bleaching) 


## Stakeholder Analysis

(Who are we aiming to convince of what?) 

## Raw Data

We are combining two datasets via shape data: matching polygons from [Allen Coral Atlas](https://allencoralatlas.org/atlas) to points from processed dataset [Reef_Check_with_cortad_variables_with_annual_rate_of_SST_change.csv](https://github.com/InstituteForGlobalEcology/Coral-bleaching-a-global-analysis-of-the-past-two-decades/blob/master/Reef_Check_with_cortad_variables_with_annual_rate_of_SST_change.csv). 

The rugosity of reefs are associated with their geomorphic class. Hence, we are using the [geomporhic data](https://storage.googleapis.com/coral-atlas-static-files/download-package-materials/Class-Descriptions-Geomorphic-Maps-v3.pdf) taken from Allen Coral Atlas to identify the area, then using a literature review to obtain the mean rugosity for that geomorphic region. 

*Note about merging data sets: From investigation of the data sets on the map, it looks like the reef dataset is not as precise as the Allen Coral dataset - hence points not touching some polygons. As such, to class all the reef points, we tried to classify the points using a boarder of 200m. However, this seemed to be too uncertain as it ended up classifying different points with multiple classes. Hence, we used `reef_geomorphic_joined_all.gpkg` and drop the NA's.* 

<!-- Add buttons here -->



&nbsp;
# Key Files

* Processed Reef Bleaching file: [Reef_Check_with_cortad_variables_with_annual_rate_of_SST_change.csv](https://github.com/camille-alice/MARS_DATA3888_reefC4/blob/main/Data/Reef_Check_with_cortad_variables_with_annual_rate_of_SST_change.csv)
* Joined Geomorphic Reef file (with NA's): reef_geomorphic_joined_all.gpkg
* Joined Geomorphic Reef file (with multiple classes): reef_geomorphic_joined_nas.gpkg
* Cleaned Rugosity Reef file (without NA's): reef_rugosity_cleaned.gpkg 

&nbsp;

# Project Deliverables

### Presentation

- The slides for our presentation can be found here

### Report

- Report can be found here 

&nbsp;

# Variables

<table>
    <tr>
        <td>Variables name</td>
        <td>Explanation</td>
        <td>Source</td> 
    </tr>
    <tr>
        <td>Reef.ID</td>
        <td>Unique Reef Identifier</td>
        <td>[Reef Check](https://www.reefcheck.org/) </td>
    </tr>
    <tr>
        <td>SSTA_Frequency_Standard_Deviation</td>
        <td>The standard deviation of Sea Surface Temperature Anomaly Frequency (SSTA_Frequency: number of times over the previous 52 weeks that SSTA >= 1 degree C) over the entire time period</td>
        <td> [NOAA’s Coral Reef Temperature Anomaly Database](www.nodc.noaa.gov/sog/cortad/Version6/)</td>
    </tr>
    <tr>
        <td>rate_of_SST_change</td>
        <td>The average annual rate of sea surface temperature change in degrees Celsius</td>
        <td>[NOAA Optimum Interpolation (OI) Sea Surface Temperature (SST) V2](https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html)</td>
    </tr>
    <tr>
        <td>Depth</td>
        <td>Provided in meters</td>
        <td>[Reef Check](https://www.reefcheck.org/)</td>
    </tr>
    <tr>
        <td>Diversity</td>
        <td>The number of coral species confirmed present in an ecoregion</td>
        <td>[Corals of the World](http://www.coralsoftheworld.org/page/overview-of-coral-taxonomy/) and J.E.N Veron (personal
communication by original authors)</td>
    </tr>
    <tr>
        <td>Class</td>
        <td>Geomorphic zone of reef site</td>
        <td>[Allen Coral Atlas](https://storage.googleapis.com/coral-atlas-static-files/download-package-materials/Class-Descriptions-Geomorphic-Maps-v3.pdf)</td>
    </tr>
    <tr>
        <td>Average_Bleaching</td>
        <td>Percent of reef corals that were recorded as bleached</td>
        <td>[Reef Check](https://www.reefcheck.org/)</td>
    </tr>
    <tr>
        <td>Bleached</td>
        <td>Binary expression (0 or 1) to indicate whether reef has been bleached or not</td>
        <td>Based on Average_Bleaching</td>
    </tr>
    <tr>
        <td>Rugosity</td>
        <td>Estimate of rugosity value for each reef site based on geomorphic zone (class)</td>
        <td>Multiple sources from literature review</td>
    </tr>
    
</table>

&nbsp;

# Group Members and Contacts

 <table>
       <tr>
           <td> Team Member </td>
           <td> Personal Email </td>
        </tr>
        <tr>
            <td> Camille Karski </td>
            <td> c.a.karski@gmail.com </td>
        </tr>
        <tr>
            <td> Kathryn Avieson </td>
            <td> EMAIL </td>
        </tr>
        <tr>
            <td> Joshua Borgnolo </td>
            <td> borgnolojosh@gmail.com </td>
        </tr>
        <tr>
            <td> Alex Burton </td>
            <td> EMAIL </td>
        </tr>
        <tr>
            <td> Nathan Cheung </td>
            <td> EMAIL </td>
        </tr>
        <tr>
            <td> Patrick Chang </td>
            <td> EMAIL </td>
        </tr>
        <tr>
            <td> Daniel Wright </td>
            <td> EMAIL </td>
        </tr>
 </table>
 
&nbsp;

# References

- Sully, S., Burkepile, D.E., Donovan, M.K. et al. A global analysis of coral bleaching over the past two decades. Nat Commun 10, 1264 (2019). https://doi.org/10.1038/s41467-019-09238-2

&nbsp;

# License

[GNU General Public License version 3](https://opensource.org/licenses/GPL-3.0)
