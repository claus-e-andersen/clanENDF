# What is clanENDF?
clanENDF is an R package for reading IAEA nuclear data in the ENDF format.
The package contains photonuclear data for a few selected isotopes.

Simple read function for nuclear data from IAEA files in ENDF format.

# Photonuclear data from IAEA (2019)
Data in ENDF-format can be found here: 
'''
https://www−nds.iaea.org/photonuclear/

https://www−nds.iaea.org/photonuclear/pdfilelist.html
'''

# Photonuclear data included in package
Collection of cross section data from the IAEA photonuclear (2019)
data base for interesting isotopes:

H-2, O-12, Al-27, W-180, W-182, W-183, W-184, W-186.

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

The line.start and line.stop can be found by manual inspection
of the endf data file. The read.endf function can only read
the pure data (no meta data).

Meta data can be assigned to the data. This facilitates
data from different isotopes or processes etc.



