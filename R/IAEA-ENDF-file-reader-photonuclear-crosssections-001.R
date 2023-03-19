
require(dplyr)
require(lattice)
require(clanTools)
require(clanLattice)

#########################################################
# Helper function to identify endf-numbers
#########################################################
#' @title endf.number
#' @description  Helper function for ENDF file numbers.
#' @details
#' # Convert ENDF data to a floating point number.
#' A raw ENDF number is written as 2.224000+6 - i.e. without any E.
#' This function adds the E (e.g. 2.224000E+6) and convert to a float.
#' Simple read function for nuclear data from IAEA files in ENDF format.
#'
#'
#' @param x = first part of number (e.g. x="4.555" if number is x="4.555-3").
#' @param y = last part of number (e.g. y="-3" if number is x="4.555-3").
#' @return a float.
#' @export
endf.number <- function(x="4.555",y="-3"){
# Convert ENDF data to a floating point number.
# A raw ENDF number is written as 2.224000+6 - i.e. without any E.
# This function adds the E (e.g. 2.224000E+6) and convert to a float.
x <- clanTools::trim.whitespace(x)
y <- clanTools::trim.whitespace(y)
xx <- NA
if(!x==""){
if(!y==""){
  xx <- as.numeric(paste(x,"E",y,sep=""))
  } else {
    xx <- as.numeric(x)
  }
}
return(xx)
}# function


#' @title read.endf
#' @description  Simple read function for nuclear data from IAEA files in ENDF format.
#' @details
#' Simple read function for nuclear data from IAEA files in ENDF format.
#'
#' The line.start and line.stop can be found by manual inspection
#' of the endf data file. The read.endf function can only read
#' the pure data (no meta data).
#'
#' Meta data can be assigned to the data. This facilitates
#' data from different isotopes or processes etc.
#'
#' \preformatted{
#' #########################################################
#'  Main function read endf-files
#'  The user has to manually identify the part of the ENDF file which
#'  is of interest by given the start and stop lines.
#'  The user can assign meta data to the results.
#' ########################################################
#'  Created: March 10, 2023
#' Revised: March 11, 2023
#' Name   : Claus E. Andersen
#' Motivation:
#' To read IAEA 2019 photonuclear data.
#
#' Input:
#' pn = path to where the ENDF file is
#' fn = file name of ENDF file
#' line.start = Where to start reading from the fine (line number)
#' line.stop  = Where to stop reading from the fine (line number)
#' Z = Atomic number (meta data to be added to the output)
#' A = Atomic mass (meta data to be added to the output)
#' element = Element name (meta data to be added to the output)
#' isotope = Isotope name (meta data to be added to the output)
#' process = Process name (meta data to be added to the output)
#' what    = Description (meta data to be added to the output)
#
#' Output:
#' A data frame like this:
#
#'     MeV      barn              file Z A element isotope process
#' 1 2.224 0.0000000 g_1-H-2_0128.endf 1 2       H     H-2   total
#' 2 2.300 0.0006581 g_1-H-2_0128.endf 1 2       H     H-2   total
#' 3 2.600 0.0011830 g_1-H-2_0128.endf 1 2       H     H-2   total
#' 4 3.000 0.0018280 g_1-H-2_0128.endf 1 2       H     H-2   total
#' 5 3.500 0.0022800 g_1-H-2_0128.endf 1 2       H     H-2   total
#' 6 4.000 0.0024540 g_1-H-2_0128.endf 1 2       H     H-2   total
#'
#'                            what
#' 1 cross section for (gamma,abs)
#' 2 cross section for (gamma,abs)
#' 3 cross section for (gamma,abs)
#' 4 cross section for (gamma,abs)
#' 5 cross section for (gamma,abs)
#' 6 cross section for (gamma,abs)
#' }
#' @param pn = path to folder with endf data files (ending with a /).
#' @param fn = file name (e.g. "g_1-H-2_0128.endf")
#' @param line.start = fist line in the endf file that you need to extract.
#' @param line.start =  = last line in the endf file that you need to extract.
#' @param Z = atomic number (meta data).
#' @param A = atomic mass (meta data).
#' @param element = element name (e.g. "H") (meta data).
#' @param isotope = isotope name (e.g. "H-2") (meta data).
#' @param process = name of process covered by cross section (meta data).
#' @param what = free text description (meta data).
#' @return a data frame with data from endf file + meta data
#'
#' @export
read.endf <- function(pn="",
                      fn="g_1-H-2_0128.endf",
                      line.start=91,
                      line.stop=100,
                      Z = 1,
                      A = 2,
                      element="H",
                      isotope="H-2",
                      process="",
                      what="crosssection for (gamma,n)"){
#########################################################
# Main function read endf-files
# The user has to manually identify the part of the ENDF file which
# is of interest by given the start and stop lines.
# The user can assign meta data to the results.
#########################################################
# Created: March 10, 2023
# Revised: March 11, 2023
# Name   : Claus E. Andersen
# Motivation:
# To read IAEA 2019 photonuclear data.
#
# Input:
# pn = path to where the ENDF file is
# fn = file name of ENDF file
# line.start = Where to start reading from the fine (line number)
# line.stop  = Where to stop reading from the fine (line number)
# Z = Atomic number (meta data to be added to the output)
# A = Atomic mass (meta data to be added to the output)
# element = Element name (meta data to be added to the output)
# isotope = Isotope name (meta data to be added to the output)
# process = Process name (meta data to be added to the output)
# what    = Description (meta data to be added to the output)
#
# Output:
# A data frame like this:
#
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


print("Welcome to read.endf")
print(paste("Now reading file:",fn,"at this location:"))
print(pn)
TT <- readLines(paste(pn,fn,sep=""))
pos.start <- 1
pos.stop <- 66
TT <- TT[line.start:line.stop]
xvec <- NULL
for(line in 1:length(TT)){
#  print(line)
  TT[line] <- substring(TT[line],pos.start,pos.stop)
  SS <- TT[line]
  pp <- 2
  x1 <- substring(SS,pp,pp+7)
  y1 <- substring(SS,pp+8,pp+9)
  pp <- pp + 11
  x2 <- substring(SS,pp,pp+7)
  y2 <- substring(SS,pp+8,pp+9)
  pp <- pp + 11
  x3 <- substring(SS,pp,pp+7)
  y3 <- substring(SS,pp+8,pp+9)
  pp <- pp + 11
  x4 <- substring(SS,pp,pp+7)
  y4 <- substring(SS,pp+8,pp+9)
  pp <- pp + 11
  x5 <- substring(SS,pp,pp+7)
  y5 <- substring(SS,pp+8,pp+9)
  pp <- pp + 11
  x6 <- substring(SS,pp,pp+7)
  y6 <- substring(SS,pp+8,pp+9)

  xx1 <- endf.number(x1,y1)
  xx2 <- endf.number(x2,y2)
  xx3 <- endf.number(x3,y3)
  xx4 <- endf.number(x4,y4)
  xx5 <- endf.number(x5,y5)
  xx6 <- endf.number(x6,y6)

  xvec0 <- c(xx1,xx2,xx3,xx4,xx5,xx6)
 if(is.null(xvec)){xvec <- xvec0} else {xvec <- c(xvec,xvec0)}
}

NN <- length(xvec)
# We assume that the numbers are pairs of energy in eV (1) and cross section (2) in barn.
df <- data.frame(xvec,what=c(1,2))
ok <- df$what==1
EE <- df$xvec[ok]
ok <- df$what==2
DD <- df$xvec[ok]
#ok <- !is.na(xvec)
#xvec <- xvec[ok]

# The base dataframe is in MeV and barn
df <- data.frame(MeV = EE/1e6,barn=DD)

# Remove NAs and add meta data
df %>%
filter(!is.na(MeV)) %>%
filter(!is.na(barn)) %>%
mutate(file=fn) %>%
mutate(Z=Z) %>%
mutate(A=A) %>%
mutate(element=element) %>%
mutate(isotope=isotope) %>%
mutate(process=process) %>%
mutate(what=what) %>%
data.frame(.) -> df

print(paste("Number of records in final data.frame:",nrow(df)))
print("ByeBye from read.endf")
return(df)
} # end function


#' @title read.selected.IAEA.photonuclear.data
#' @description  Helper function for ENDF file numbers.
#' @details
#' \preformatted{
#' Created: March 10, 2023
#' Revised: March 11, 2023
#' Name   : Claus E. Andersen
#' Motivation:
#' To read selected IAEA 2019 photonuclear data.

#' ####################################################
#' Collection of cross section data from the IAEA photonuclear (2019)
#' data base for interesting isotopes:
#' H-2, O-12, Al-27, W-180, W-182, W-183, W-184, W-186.
#'
#' In most cases I first read the total cross section for the
#' (gamma, any nuclear event)-reaction. This is the (gamma,abs)
#' cross section as the gamma is absorbed.
#'
#' Secondly, I normally get the cross section for neutron production:
#' (gamma, n) where we n represent any number of neutrons produced by
#' the event, regardless of any additional particles (protons, alphas
#' etc.).
#'
#' Here I have manually identified the relevant (hopefully correct)
#' parts of the IAEA ENDF files and assigned meta data to them. Finally,
#' I join everything into a single dataframe.
#' }
#' @param pn = path to folder with endf data files (ending with a /).
#' @param save.data.to.file = boolean (TRUE/FALSE).
#' @param save.fn = file name with the extracted data (e.g. "IAEA-photonuclear-2019.txt").
#' @return a float.
#' @export
read.selected.IAEA.photonuclear.data <- function(pn="", save.data.to.file=TRUE, save.fn="IAEA-photonuclear-2019.txt"){
# Created: March 10, 2023
# Revised: March 11, 2023
# Name   : Claus E. Andersen
# Motivation:
# To read selected IAEA 2019 photonuclear data.

#####################################################
# Collection of cross section data from the IAEA photonuclear (2019) data base for interesting
# isotopes (H-2, O-12, Al-27, W-180, W-182, W-183, W-184, W-186).

# In most cases I first read the total cross section for the (gamma, any nuclear event)-reaction.
# This is the (gamma,abs) cross section as the gamma is absorbed.

# Secondly, I normally get the cross section for neutron production: (gamma, n) where
# we n represent any number of neutrons produced by the event, regardless of any
# additional particles (protons, alphas etc.).

# Here I have manually identified the relevant (hopefully correct) parts of the IAEA ENDF files
# and assigned meta data to them. Finally, I join everything into a single dataframe.

print("Welcome to read.selected.IAEA.photonuclear.data")

df.H2.abs <- read.endf(
                pn = pn,
                fn="g_1-H-2_0128.endf",
                line.start=91,
                line.stop=100,
                Z = 1,
                A = 2,
                element="H",
                isotope="H-2",
                process="total",
                what="cross section for (gamma,abs)")

df.H2.n <- read.endf(
                pn = pn,
                fn="g_1-H-2_0128.endf",
                line.start=105,
                line.stop=114,
                Z = 1,
                A = 2,
                element="H",
                isotope="H-2",
                process="neutron production",
                what="cross section for (gamma,n)")

df.Al27.abs <- read.endf(
                pn = pn,
                fn="g_13-Al-27_1325.endf",
                line.start=153,
                line.stop=185,
                Z = 13,
                A = 27,
                element="Al",
                isotope="Al-27",
                process="total",
                what="cross section for (gamma,abs)"
                )

df.Al27.n <- read.endf(
                pn = pn,
                fn="g_13-Al-27_1325.endf",
                line.start=227,
                line.stop=255,
                Z = 13,
                A = 27,
                element="Al",
                isotope="Al-27",
                process="neutron production",
                what="cross section for (gamma,n)"
                )

df.O16.abs <- read.endf(
                pn = pn,
                fn="g_8-O-16_0825.endf",
                line.start=183,
                line.stop=246,
                Z = 8,
                A = 16,
                element="O",
                isotope="O-16",
                process="total",
                what="cross section for (gamma,abs)"
                )

df.W180.abs <- read.endf(
                pn = pn,
                fn="g_74-W-180_7425.endf",
                line.start=131,
                line.stop=164,
                Z = 74,
                A = 180,
                element="W",
                isotope="W-180",
                process="total",
                what="cross section for (gamma,abs)"
                )

df.W180.n <- read.endf(
                pn = pn,
                fn="g_74-W-180_7425.endf",
                line.start=169,
                line.stop=197,
                Z = 74,
                A = 180,
                element="W",
                isotope="W-180",
                process="neutron production",
                what="cross section for (gamma,n)"
                )

df.W182.abs <- read.endf(
                pn = pn,
                fn="g_74-W-182_7431.endf",
                line.start=136,
                line.stop=169,
                Z = 74,
                A = 182,
                element="W",
                isotope="W-182",
                process="total",
                what="cross section for (gamma,abs)"
                )

df.W182.n <- read.endf(
                pn = pn,
                fn="g_74-W-182_7431.endf",
                line.start=174,
                line.stop=202,
                Z = 74,
                A = 182,
                element="W",
                isotope="W-182",
                process="neutron production",
                what="cross section for (gamma,n)"
                )

df.W183.abs <- read.endf(
                pn = pn,
                fn="g_74-W-183_7434.endf",
                line.start=131,
                line.stop=164,
                Z = 73,
                A = 183,
                element="W",
                isotope="W-183",
                process="total",
                what="cross section for (gamma,abs)"
                )

df.W183.n <- read.endf(
                pn = pn,
                fn="g_74-W-183_7434.endf",
                line.start=169,
                line.stop=198,
                Z = 74,
                A = 183,
                element="W",
                isotope="W-183",
                process="neutron production",
                what="cross section for (gamma,n)"
                )

df.W184.abs <- read.endf(
                pn = pn,
                fn="g_74-W-184_7437.endf",
                line.start=143,
                line.stop=176,
                Z = 74,
                A = 184,
                element="W",
                isotope="W-184",
                process="total",
                what="cross section for (gamma,abs)"
                )

df.W184.n <- read.endf(
                pn = pn,
                fn="g_74-W-184_7437.endf",
                line.start=181,
                line.stop=209,
                Z = 74,
                A = 184,
                element="W",
                isotope="W-184",
                process="neutron production",
                what="cross section for (gamma,n)"
                )

df.W186.abs <- read.endf(
                pn = pn,
                fn="g_74-W-186_7443.endf",
                line.start=163,
                line.stop=196,
                Z = 74,
                A = 186,
                element="W",
                isotope="W-186",
                process="total",
                what="cross section for (gamma,abs)"
                )

df.W186.n <- read.endf(
                pn = pn,
                fn="g_74-W-186_7443.endf",
                line.start=202,
                line.stop=230,
                Z = 74,
                A = 186,
                element="W",
                isotope="W-186",
                process="neutron production",
                what="cross section for (gamma,n)"
                )


# I cannot identify the clean neutron cross section data for O-16
df.IAEA.photonuclear.abs <- rbind(df.H2.abs, df.Al27.abs, df.O16.abs, df.W180.abs, df.W182.abs, df.W183.abs, df.W184.abs, df.W186.abs)
df.IAEA.photonuclear.n   <- rbind(df.H2.n,   df.Al27.n,               df.W180.n,   df.W182.n,   df.W183.n,   df.W184.n,   df.W186.n)

df.IAEA.photonuclear <- rbind(df.IAEA.photonuclear.abs,df.IAEA.photonuclear.n)

if(save.data.to.file){
  print(paste("!! The dataframe with IAEA photonuclear data was save to:",save.fn))
  write.table(df.IAEA.photonuclear,save.fn,row.names=FALSE)
}

print("ByeBye from read.selected.IAEA.photonuclear.data")
return(df.IAEA.photonuclear)
} # End function





#' @title IAEA.photonuclear.plot
#' @description  Read file with all data and make a trellis plot.
#' @details
#' \preformatted{
#' # Created: March 10, 2023
#' Revised: March 11, 2023
#' Revised: March 13, 2023
#' Name: Claus E. Andersen
#' Objective take the IAEA data from the IAEA in the file file fn,
#' and show the results.
#' }
#' @param pn = path to folder with endf data files (ending with a /).
#' @param fn = file name with the data to read (e.g. "IAEA-photonuclear-2019.txt").
#' @param log.wanted = TRUE or FALSE.
#' @param MeV.min = lower energy interval in MeV.
#' @param MeV.max = upper energy interval in MeV.
#' @return a float.
#' @export
IAEA.photonuclear.plot <- function(pn="",
                                   fn="IAEA-photonuclear-2019.txt",
log.wanted = TRUE,
MeV.min=0, MeV.max = 20.1){
# Created: March 10, 2023
# Revised: March 11, 2023
# Revised: March 13, 2023
# Name: Claus E. Andersen
# Objective take the IAEA data from the IAEA in the file file fn,
# and show the results.
print("Welcome to IAEA.photonuclear.plot")

print(paste("Read data file:",fn))
if(!file.exists(fn)){
  # No file to read
  print("No such file was found.")
  print("This file need to be in the same folder as you R script.")
  print("You can get the file from the R-package or you can produce it with")
  print("the function read.selected.IAEA.photonuclear.data.")
  print("Be careful with the file names!")
  } else {
# A file exists. Now read and plot it:
df <- read.table(fn,header=TRUE)
print("Head of data file:")
print(head(df))

cex.lab <- 1.5
cex.scale <- 1.1
clanLattice::set.trellis(col=c("blue","red"),pch=c(16,1),cex=c(0.6,0.5))

if(log.wanted){
plt.log <- lattice::xyplot(log10(barn)~MeV|isotope,
auto.key=list(columns=2),
main="IAEA photonuclear data (2019) \nCross section vs. energy stratified by target isotope and nuclear process",
xlab=list("Energy [MeV]",cex=cex.lab),
ylab=list("Log10( Crosssection [barn])",cex=cex.lab),
scales=list(cex=cex.scale),
data=df,
type="b",
subset = (MeV > MeV.min) & (MeV < MeV.max),
groups=process,
panel=function(x,y,groups=groups,subscripts=subscripts,...){
  panel.grid(h=-1,v=-1)
  panel.superpose(x,y,groups=groups,subscripts=subscripts,...)
  #panel.abline(h=1,lty="dashed")
  #panel.abline(v=0,lty="dashed")
}
) #xyplot
print(plt.log)
} # log.wanted


if(!log.wanted){
plt.lin <- lattice::xyplot(barn~MeV|isotope,
auto.key=list(columns=2),
main="IAEA photonuclear data (2019) \nCross section vs. energy stratified by target isotope and nuclear process",
xlab=list("Energy [MeV]",cex=cex.lab),
ylab=list("Crosssection [barn]",cex=cex.lab),
scales=list(cex=cex.scale,rot=0,y=list(relation="free")),
data=df,
type="b",
subset = (MeV > MeV.min) & (MeV < MeV.max),
groups=process,
panel=function(x,y,groups=groups,subscripts=subscripts,...){
  panel.grid(h=-1,v=-1)
  panel.superpose(x,y,groups=groups,subscripts=subscripts,...)
  #panel.abline(h=1,lty="dashed")
  #panel.abline(v=0,lty="dashed")
}
) #xyplot
print(plt.lin)
} # nor log.wanted


}# else (file exists
print("ByeBye from IAEA.photonuclear.plot")
} # IAEA.photonuclear.plot



#' @title IAEA.photonuclear.demo
#' @description  Demonstration function
#' @details
#' Simple read function for nuclear data from IAEA files in ENDF format.
#'
#' Location of the IAEA 2019 ENDF-files. Could be here:
#'
#'    \preformatted{
#'     pn.full <- paste(getwd(),"/data/",sep="")}
#'
#' Or if the data are coming with the clanENDF package you may
#' be able to get it using:
#'
#'    \preformatted{
#'    #' pn.full <- paste(.libPaths(),"/clanENDF","/data/",sep="")}
#'
#' How to read the data
#'
#'    \preformatted{
#'    df <- read.selected.IAEA.photonuclear.data(
#'    pn = pn.full,
#'    save.data.to.file=TRUE,
#'    save.fn="IAEA-photonuclear-2019.txt")}
#'
#' Then call the demo function:
#'   \preformatted{
#'     IAEA.photonuclear.demo()}
#'
#' @export
IAEA.photonuclear.demo<- function(){
print(" Make sure the function can find: IAEA-photonuclear-2019.txt !!! See help.")
# Plot the data
IAEA.photonuclear.plot(
fn="IAEA-photonuclear-2019.txt",
log.wanted = FALSE,
MeV.min=0,
MeV.max = 50)

# Plot the data
IAEA.photonuclear.plot(
fn="IAEA-photonuclear-2019.txt",
log.wanted = TRUE,
MeV.min=0,
MeV.max = 50)
}# IAEA.photonuclear.plot

#' @title clanENDF
#' @description  Package for reading IAEA nuclear data files.
#' This is a simple dummy function to ease access to the help index.
#'
#' \preformatted{
#'  clanENDF()
#'
#'  ?clanENDF # gives you an index of functions in package
#'   }
#'
#' Main functions for computation of the density-effect correction are:
#' Simple read function for nuclear data from IAEA files in ENDF format.
#'
#' Location of the IAEA 2019 ENDF-files. Could be here:
#'
#'    \preformatted{
#'     pn.full <- paste(getwd(),"/data/",sep="")}
#'
#' Or if the data are coming with the clanENDF package you may
#' be able to get it using:
#'
#'    \preformatted{
#'    #' pn.full <- paste(.libPaths(),"/clanENDF","/data/",sep="")}
#'
#' How to read the data
#'
#'    \preformatted{
#'    df <- read.selected.IAEA.photonuclear.data(
#'    pn = pn.full,
#'    save.data.to.file=TRUE,
#'    save.fn="IAEA-photonuclear-2019.txt")}
#'
#' Then call the demo function:
#'   \preformatted{
#'     IAEA.photonuclear.demo()}
#' @export
clanENDF <- function(){
  # Dummy function
}
