

**README. Reproducible results.Integrating agent-based disease, mobility, and wastewater models:** 
*Dealing with differences in spatiotemporal resolutions*

Néstor DelaPaz-Ruíz, Ellen-Wien Augustijn, Mahdi Farnaghi, Raul Zurita-Milla. June, 2024

-   [About](#about)
-   [Getting Started](#getting-started)
    -   [Pre-requirements](#pre-requirements)
-   [Quick reproducibility](#quick-reproducibility)
-   [Support](#support)
-   [License](#license)
-   [Acknowledgements](#acknowledgements)

Affiliation: Department of Geo-Information Process (GIP), Faculty of
Geo-Information Science and Earth Observation (ITC), University of
Twente, Drienerlolaan 5, 7522 NB Enschede, The Netherlands

## About

This repository provides access to the materials (code-data),
software-environments (NetLogo, Rstudio, Docker containers), and the
steps for reproducing the results from the publication: *Integrating
agent-based disease, mobility, and wastewater models: Dealing with
differences in spatiotemporal resolutions*.

The information below guides you to execute two NetLogo simulations. The
main outputs are the figures and tables representing the results
assessment of the mentioned publication.

## Getting Started

### Pre-requirements

Used computational environment:

    Platform: x86_64-w64-mingw32/x64 (64-bit) 
    Running under: Windows 11 x64 (build 22631)
    Processor:  Intel(R) Core(TM) i7-10750H CPU @ 2.60GHz   2.59 GHz
    Logical processors: 12 
    Installed RAM   32.0 GB (31.6 GB usable)
    System type 64-bit operating system, x64-based processor

    Docker Desktop v.4.16.3

    R version 4.3.1 (2023-06-16 ucrt)
    RStudio 2023.12.1

    NetLogo 6.3

You will need the following:

1.  Data:

-   Download or copy this repository to a local folder .

1.  Open-source software:

Quick reproducibility (requires NetLogo/Rstudio experience):

-   Install and run Git Bash. See: <https://gitforwindows.org/>
-   Install and run NetLogo. See:
    <https://ccl.northwestern.edu/netlogo/6.3>
-   Install Rstudio. See:
    <https://dailies.rstudio.com/version/2023.12.1+402/>
-   NetLogo note: For this option copy the provided `NetLogo.cfg` file
    and replace it at the folder: `C:\Program Files\NetLogo 6.1.1\app`.
    In this way, you are free to use several processors. Please, refer
    to the following link: [FAQ: How big can my model be? How many
    turtles, patches, procedures, buttons, and so on can my model
    contain?](http://ccl.northwestern.edu/netlogo/docs/faq.html#how-big-can-my-model-be-how-many-turtles-patches-procedures-buttons-and-so-on-can-my-model-contain).
    After the NetLogo installation, make sure to execute the
    `. ./code/newfiles.txt` in Git Bash.,

Long-term reproducibility (On hold. Rrequires some familiarity with
Docker):

-   Install and run Git Bash. See: <https://gitforwindows.org/>
-   Install and run Docker Desktop. See:
    <https://www.docker.com/products/docker-desktop/>
-   NetLogo note: Before building and running NetLogo images and
    containers in Docker Desktop consider that you can run the NetLogo
    models once you install NetLogo 6.1.1. Running NetLogo in Docker
    Desktop can take a couple of hours. Running NetLogo with your own
    NetLogo 6.1.1 installation can take around 25 minutes.

1.  Libraries and extensions:

-   NetLogo: The first time running the Netlogo file, you will get a
    windows to install the time extension. Follow the instructions to
    install it.
-   Rstudio: The first time that you open the R project execute the
    `renv::restore()` in the R console to load the required libraries.
    You can explore the library requirements in the `renv.lock` file.

## Quick reproducibility

This section is for Windows users familiar with NetLogo, Rstudio, and
Git bash. Mac or Linux users can also try to reproduce results. Jump to
the long-term reproducibility (usage) section, which uses Docker to
reproduce results as the alternative for non-experienced users.

Steps for a quick reproduction:

1.  In a new folder, execute the following commands in git bash:

<!-- -->

    git clone https://github.com/delapazruiz/domestic.wastewater.variability.git

    . ./code/newfiles.txt

1.  Open the NetLogo (v. 6.3) file and run the experiment. In
    Tools/BehaviorSpace, select number of processors to run the
    experiment. (time: around 30 min)

<!-- -->

    s3.integrating.wastewater.and.diseases.nlogo

1.  Open the Rstudio project file and verify the library requirements.

R Project file:

    dw.disease.infected.wastewater.Rproj

Make sure that ‘renv’ is installed and loaded.

    install.packages("renv")

Load the ‘renv’ library.

    library(renv)

Run the following in the R console to install the required libraries.

    renv::restore()

1.  Open the Quarto file (.qmd) and render it to generate the report:
    (time: around 10 min) 6

<!-- -->

    Disease spread.qmd

## Support

This repository is expected to be in continuous improvement. For major
changes, please open an issue first to discuss what you would like to
improve.

## License

This project is licensed under the MIT license. Feel free to edit and
distribute this template as you like. See LICENSE for more information.

[MIT](https://choosealicense.com/licenses/mit/)

## Acknowledgements

The authors wish to express their gratitude for the valuable support in
the local implementation of this study, without which this research
cannot be consolidated: Carlos Pailles, Ana Velasco, Lucía Guardamino,
Rodrigo Tapia-McClung, Araceli Chávez, Diana Ramos, Daniela Goméz, José
Luis Pérez, Natalia Volkow, and the anonymous facilitators from Mexico
City, citizens of Tepeji del Río, and the INEGI department of microdata.
Scholarship funder: CONACYT-Alianza FiiDEM.