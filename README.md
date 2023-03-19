# What is clanENDF?
clanENDF is an R package for reading IAEA nuclear data in the ENDF format.
The package contains photonuclear data for a few selected isotopes.

Simple read function for nuclear data from IAEA files in ENDF format.

For each ENDF file, one manually needs to identify the data of interest by means of
the start and stop lines in the file. The file name and the line.start and line.stop
are the key parameters to the call to the main function:
```
read.endf()
```

The read.endf function can only read the pure data. It cannt read meta data from the endf file.

Meta data can, however, be assigned to the data. This facilitates
data from different isotopes or processes etc.

# Sample call
pn is the path to the data. If you are reading the data provided with the package
you can probably find the path as:
```
pn.full <- paste(getwd(),"/data/",sep="")}

 
```
df.H2.abs <- read.endf(
                pn = pn.full,
                fn="g_1-H-2_0128.endf",
                line.start=91,
                line.stop=100,
                Z = 1,
                A = 2,
                element="H",
                isotope="H-2",
                process="total",
                what="cross section for (gamma,abs)")
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
Collection of cross section data from the IAEA photonuclear (2019)
data base for interesting isotopes:

```
H-2, O-12, Al-27, W-180, W-182, W-183, W-184, W-186.
```

In most cases I first read the total cross section for the
(gamma, any nuclear event)-reaction. This is the (gamma,abs)
cross section as the gamma is absorbed.

Secondly, I normally get the cross section for neutron production:
(gamma, n) where we n represent any number of neutrons produced by
the event, regardless of any additional particles (protons, alphas
etc.).

Here I have manually identified the relevant (hopefully correct)
parts of the IAEA ENDF files and assigned meta data to them. Finally,
I join everything into a single dataframe.

# How to install the package?
You will need devtools to install the package. You will need clanTools and dplyr to run the package.
```
install.packages("devtools","dplyr")
library(devtools)
library(dplyr)
install_github("claus-e-andersen/clanTools)
install_github("claus-e-andersen/clanENDF)
library(clanENDF)
library(dplyr)
library(clanTools)
```
To run the code using the data included with the package, please see the help for the demo function:
```
?IAEA.photonuclear.demo()
IAEA.photonuclear.demo()
```

