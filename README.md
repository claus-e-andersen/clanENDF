# What is clanENDF?
clanENDF is an R package for reading IAEA nuclear data in the ENDF format.
The package contains photonuclear data for a few selected isotopes.

For each ENDF file, one manually needs to identify the data of interest by means of
the start and stop lines in the file. The file name and the line.start and line.stop
are the key parameters to the call to the main function:
```
read.endf()
```

The read.endf function can only read the pure data. It cannot read meta data from the endf file.

Meta data can, however, be assigned to the data. This facilitates
data from different isotopes or processes etc.

# Sample call
The path to the data is needed. If you are reading the data provided with the package
you can probably find the data using the second path defined below:
```
pn.full <- paste(getwd(),"/data/",sep="") 

pn.full <- paste(.libPaths(),"/clanENDF","/data/",sep="")
``` 
The read.endf can then be called as follows;
```
df.H2.abs <- read.endf(
                pn = pn.full,
                fn =  "g_1-H-2_0128.endf",
                line.start = 91,
                line.stop = 100,
                Z = 1,
                A = 2,
                element = "H",
                isotope = "H-2",
                process = "total",
                what = "cross section for (gamma,abs)")
```                

The data read by the function should look like this:

```
#    MeV      barn              file Z A element isotope process
#1 2.224 0.0000000 g_1-H-2_0128.endf 1 2       H     H-2   total
#2 2.300 0.0006581 g_1-H-2_0128.endf 1 2       H     H-2   total
#3 2.600 0.0011830 g_1-H-2_0128.endf 1 2       H     H-2   total
#4 3.000 0.0018280 g_1-H-2_0128.endf 1 2       H     H-2   total
#5 3.500 0.0022800 g_1-H-2_0128.endf 1 2       H     H-2   total
#6 4.000 0.0024540 g_1-H-2_0128.endf 1 2       H     H-2   total
#
#                           what
#1 cross section for (gamma,abs)
#2 cross section for (gamma,abs)
#3 cross section for (gamma,abs)
#4 cross section for (gamma,abs)
#5 cross section for (gamma,abs)
#6 cross section for (gamma,abs)
```
Note that the two first columns are the actual data from the ENDF file:
(i) energy in MeV and (ii) cross section in barn (1 barn = 1e-24 cm2).

The data were extracted from line 91 to 100 in the g_1-H-2_0128.endf file (please
note the annotations in the right side of the text below):

```
 IAEA/PD-2019  H -  2                                                1 0  0
 1.002000+3 1.996800+0         -1          0         38          1 128 1451
 0.000000+0 0.000000+0          0          0          0          6 128 1451
 0.000000+0 1.400000+8          0          0          0       2019 128 1451
 0.000000+0 0.000000+0          0          0         76          4 128 1451
 1- H -  2  NFD       EVAL-AUG95 T.Murata                          128 1451
NDS 163 109 (2020)    DIST-JUN20                                   128 1451
----IAEA/PD-2019      MATERIAL  128                                128 1451
-----PHOTO-NUCLEAR DATA                                            128 1451
------ENDF-6 FORMAT                                                128 1451
                                                                   128 1451
History                                                            128 1451
1995-08 Evaluation was performed by T.Murata (NFD)                 128 1451
1995-08 Compiled by S. Chiba (JAERI)                               128 1451

...
                                                                   128 1451
MF=3  Photon Cross Sections                                        128 1451
  MT=3  Total photo-absorption cross section                       128 1451 MF3 data description
        Incident energy below 10 MeV :                             128 1451
        Experimental cross sections/2/ were analyzed with a        128 1451
        simplified Marshall & Guth model/3/.                       128 1451
        Incident energy above 10 MeV :                             128 1451
        Partovi/4/ gave theoretical cross section which reproduced 128 1451
        experimental values fairly well. The theoretical results   128 1451
        were basically fitted with the same model as described     128 1451
        above.                                                     128 1451
                                                                   128 1451
  MT=50 (g,n0) cross section                                       128 1451 MF50 data description
  MT=50 (g,n0) cross section                                       128 1451 
        Same as the data of MT=3.                                  128 1451
                                                                   128 1451
                                                                   128 1451
References                                                         128 1451
 1) Murata, T. : JAERI-M 94-019, p.330 (1994).                     128 1451
 2) Allen, L.Jr.: Phys. Rev. 98, 705 (1955),                       128 1451
    Alexandrov, I.A.et al.: Soviet Phys. JETP 6, 472 (1958),       128 1451
    Whalin, B.A. et al.: Phys. Rev. 101, 377 (1956),               128 1451
...
 6) Dedrick, K.G.: Rev. Mod. Phys. 34, 429 (1962).                 128 1451
                                                                   128 1451
                                1        451         84          1 128 1451
                                3          3         13          0 128 1451
                                3         50         13          0 128 1451
                                6         50         65          0 128 1451
                                                                   128 1  0
                                                                   128 0  0
 1.002000+3 1.996800+0          0          0          0          0 128 3  3
 0.000000+0 0.000000+0          0          0          1         28 128 3  3
         28          3                                             128 3  3
 2.224000+6 0.000000+0 2.300000+6 6.581000-4 2.600000+6 1.183000-3 128 3  3 MF3 data line 91
 3.000000+6 1.828000-3 3.500000+6 2.280000-3 4.000000+6 2.454000-3 128 3  3 MF3 data line 92
 4.500000+6 2.474000-3 5.000000+6 2.416000-3 5.500000+6 2.318000-3 128 3  3 MF3 data line 93
 6.000000+6 2.205000-3 8.000000+6 1.753000-3 1.000000+7 1.373000-3 128 3  3 MF3 data line 94
 1.500000+7 8.528000-4 2.000000+7 5.882000-4 2.500000+7 4.350000-4 128 3  3 MF3 data line 95
 3.000000+7 3.376000-4 3.500000+7 2.714000-4 4.000000+7 2.242000-4 128 3  3 MF3 data line 96
 4.500000+7 1.893000-4 5.000000+7 1.629000-4 5.500000+7 1.425000-4 128 3  3 MF3 data line 97
 6.000000+7 1.264000-4 7.000000+7 1.031000-4 8.000000+7 8.744000-5 128 3  3 MF3 data line 98
 9.000000+7 7.622000-5 1.000000+8 6.764000-5 1.200000+8 5.483000-5 128 3  3 MF3 data line 99
 1.400000+8 4.558000-5                                             128 3  3 MF3 data line 100
                                                                   128 3  0
 1.002000+3 1.996800+0          0          0          0          0 128 3 50 
-2.224000+6-2.224000+6          0          0          1         28 128 3 50
         28          3                                             128 3 50
 2.224000+6 0.000000+0 2.300000+6 6.581000-4 2.600000+6 1.183000-3 128 3 50 MF50 data
 3.000000+6 1.828000-3 3.500000+6 2.280000-3 4.000000+6 2.454000-3 128 3 50 MF50 data
 4.500000+6 2.474000-3 5.000000+6 2.416000-3 5.500000+6 2.318000-3 128 3 50 MF50 data
 6.000000+6 2.205000-3 8.000000+6 1.753000-3 1.000000+7 1.373000-3 128 3 50 MF50 data
...
```

# Use case (TOPAS evaluation)
I used this package (and the data it contains) to evaluate the ability of the Monte-Carlo software TOPAS to
model photonuclear reactions. The results have been uploaded (March 2023) to the Topas user forum.

# Photonuclear data from IAEA (2019)
Data in ENDF-format can be found here: 
```
https://www−nds.iaea.org/photonuclear/
```

```
https://www−nds.iaea.org/photonuclear/pdfilelist.html
```

# Photonuclear data included in package
The clanENDF package contain a small collection of cross section data from the IAEA photonuclear (2019)
data base for a few isotopes:

```
H-2, O-12, Al-27, W-180, W-182, W-183, W-184, W-186.
```
You can find a plot of these data in the Github clanENDF folder:
```
IAEA-photonuclear-data-plot-001.pdf
```
The data file:

```
IAEA-photonuclear-2019.txt
```
contains data for all isotopes given above. This file is in the subfolder
called data.

In most cases I have first read the total cross section for the
(gamma, any nuclear event)-reaction. This is the (gamma,abs)
cross section as the gamma is absorbed.

Secondly, I have normally read the cross section for neutron production:
(gamma, n) where n represents any number of neutrons produced by
the event, regardless of any additional particles (protons, alphas
etc.).

I have manually identified the relevant (hopefully correct)
parts of the IAEA ENDF files and assigned meta data to them. Finally,
I have joined everything into a single file.

# How to install the package?
You will need devtools to install the package. You will need clanTools, dplyr and lattice to run the package.
```
install.packages("devtools")
install.packages("dplyr")
install.packages("lattice")
library(devtools)
library(dplyr)
library(lattice)
install_github("claus-e-andersen/clanTools)
install_github("claus-e-andersen/clanENDF)
library(clanENDF)
library(clanTools)
```
To run the code using the data included with the package, please see the help for the demo function:
```
?IAEA.photonuclear.demo()
IAEA.photonuclear.demo()
```

