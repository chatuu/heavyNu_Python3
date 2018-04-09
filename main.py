from functions import print_zeroth_norms

print('\n Starting Heavy Neutrino Analysis')


nvar = 47  # Number of Variables Plotted
print('\n Number of Variables Plotted was set to: ' + str(nvar))

ncuts = 31  # Number of Cuts Made
print('\n Number of Cuts was set to: ' + str(ncuts))

nmc = 24  # Number of MonteCarlo Types
print('\n Number of MonteCarlo Types was set to: ' + str(nmc))

ntyp = 7  # Number of MC generator types
print('\n Number of MonteCarlo Generator Types was set to: ' + str(ntyp))

nevtyp = 4  # Number of event class types
print('\n Number of event class Types was set to: ' + str(nevtyp))

ndetsec = 4  # Number of detector sections
print('\n Number of Detector Sections was set to: ' + str(ndetsec))

nncand = 4  # Number of ncand types
print('\n Number of ncand was set to: ' + str(nncand))

nextyp = 4  # Number of extra cuts
print('\n Number of extra Cuts was set to: ' + str(nextyp))


'''   File Unit Numbers:
       (Fortran allows unit number range  1 - 99. Units 5 & 6 are reserved  for standard input / output.)
      integer hfileID #- - > For histograms
      integer ntID #- - > For ntuples
      integer evtID(2) #- - > Event printout
      -------------------------------------
      -------------------------------------
'''
'''
       For User Customization
       (Explanation Below)
      integer maxevts
      integer usents
      integer fullnts
      integer runtype(0: nmc)
      integer cuthists(ncuts)
      integer nbintyp(nvar)
      integer nttypobg
      integer gtyp(0: nmc)
      double precision zetaS
      #- - > 2nd index gives full(1) or baby(0) ntuple datacards
      character * 100 datacard(0: nmc][ 0: 1)
      #- - > 2nd index gives mc generator type(gtyp)
      character * 11 dname(0: nmc][ ntyp)
      #- - > 2nd index gives mc generator type(gtyp)
      character * 11 bname(0: nmc][ ntyp)
      character * 20 filetag(0: nmc)
      character * 31 mctyp(0: nmc)
      character * 20 ttag(0: nmc)
      character * 11 dstr(0: nmc)
      character * 3 dtyp(ntyp)
      integer drep(0: nmc)
      integer ndetsw
      integer detsw(ndetsec)
      integer detidx(ndetsec)
      integer nncndsw
      integer ncndsw(nncand)
      integer ncndidx(nncand)
      integer nevtsw
      integer evtsw(nevtyp)
      integer evtidx(nevtyp)
      integer ncuthists
      integer extrapsw
      integer evtpicsw

       Cut Counts & Normalizations
      double precision gennt(nmc][ ntyp)  Number of generated events in each ntuple
                                     Second index gives MC generator type(gtyp)
      double precision norm(0: 1][ 0: nmc)  In the 1st index: 0 = zeroth][ 1 = final
      double precision cuts(3][ ncuts][ 0: nmc][ nevtyp][ nextyp][ nncand][ ndetsec)  In the 1st index:
                                                 1 = raw(wt=1.0)
                                                 2 = mc - z weighting
                                                 3 = Any extra corrections / weighting
                                                2nd index is for cut number
                                                3rd index is for ntuple type
                                                In the 4th index:
                                                 0 = All NuMuCC
                                                 1 = OS DiMu
                                                 2 = LS DiMu
                                                 3 = OS  Mu + <other >

      -------------------------------
       Correction Functions:

       Index of the corrected MC:
      integer iCorr

       2D Correction:
      integer usecorr2D
      integer nbin2Dmass
      integer nbin2Dzeta
      nbin2Dmass=10)
      nbin2Dzeta=10)
      double precision DScorr2D(nbin2Dmass][ nbin2Dzeta)
      double precision binlim2Dmass(nbin2Dmass + 1)
      double precision binlim2Dzeta(nbin2Dzeta + 1)

       Parent mass correction:
      integer usecorrMass
      integer nbinMass
      nbinMass=20)
      double precision DScorrMass(nbinMass)
      double precision binlimMass(nbinMass + 1)

       Parent zeta correction:
      integer usecorrZeta
      integer nbinZeta
      nbinZeta=20)
      double precision DScorrZeta(nbinZeta)
      double precision binlimZeta(nbinZeta + 1)

       For renormalization factor:
       (so that correction functions change only the shape  of the MC][ and not the number of events)
      integer formRenorm #- - > Form renormalization factor?
      double precision renorm #- - > CCDIS renormalization factor
      double precision reCounts(2)  1 = Raw number of CCDIS
                                    2 = Corrected number of CCDIS

       Misc.:
      character * 80 filename
      -------------------------------

      ---------------------------------------------------------------------------
      0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0
      0o0o0o0o0o0o0o0o0o0o0 #- -- BEGIN USER INPUT #- --0o0o0o0o0o0o0o0o0o0o0
      0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0

       *----#- -= == == == == == == == == == == == == == == == == == == == == == == ------*
       *--#- -= == == Set Above(or elsewhere): == == = ----*
       *#- -= == == == == == == == == == == == == == == == == == == --*
       nvar = Number of Kinematic variables  {nvar < 100 for histogram ID scheme}

       ncuts = Number of cuts exacted

       nmc = Number of MC components < Data are 0th and 1st components >
             With 0th Data included the total number of components is (nmc + 1)

       ntyp = Number of possible MC generator types][ or possible combinations
              of the basic generators

       nextyp = Number of extra cut types to plot.  Extra cut types give plots
               which have additional kinematic cuts applied.
               {should be no larger than 99 for current plotting scheme}

       nbintyp(nvar) = Number of bin variations for each variable.
                       (Set in bookhistos.F.  Maximum Value=nbinvarMAX)

       *#- -= == == == == == == == == == == == == == == == == == == --*
       *--#- -= == == == == == == == == == == == == == == == == == == == == ----*
       *----#- -= == == == == == == == == == == == == == == == == == == == == == == ------*

       Histogram numbering scheme:
            Histograms are numbered internally using sequential
            numbers.  At the user level histograms are numbered with a
            set of parameters][ shown below.  The parameters are mapped
            to the sequential set using the getid.F subroutines.
       *****************************
       *** ID = cciieeddnnbbvv ** *
       *****************************
         cc = Event Type(0=All CC][ 1=Opposite - sign DiMu][ 2=Like - sign DiMu)
         ii = Cut number
         ee = Extra Cut type(00=none)
         dd = Detector section
         nn = Ncand type
         bb = Bin Variation
         vv = Variable Number

'''
zetaS = 0.075  # Gives the zeta value that differentiates the signal and background regions.

# Maximum number of events to use per ntuple:

maxevts = 0  # 0 = Use all events

# Number of ntuples to use for each type:

usents = 0  # 0 = Use all NTS
# 1 = Use only 1 Ntuple
# 2 = Use only 2 Ntuples(or the maximum available)
# 3 = Use only 3 Ntuples...

fullnts = 0  # 0 = Use Baby NTs
# 1 = Use Full NTs

nttypobg = 4  # Index of the OBG(outside background) contribution

evtpicsw = 0  # Print event picture output? (1 or 0)

extrapsw = 1  # Use my track extrapolation for 2 or more muons? (1 or 0)


# = == == == == == == == == == == == == == == == == == == == == ==
# = == == == == == == == == == == == == == == == == == == == == ==
# DS Correction Function Settings

# Index for the corrected MC:

iCorr = 1  # (CCDIS)

# Form CCDIS Renormalization factor?

formRenorm = 0

# Use 2D Correction?

usecorr2D = 0

# Use Mass Correction?

usecorrMass = 0

# Use Zeta Correction?
usecorrZeta = 0

#= == == == == == == == == == == == == == == == == == == == == ==
#= == == == == == == == == == == == == == == == == == == == == ==

#-------------------------------------------
#-------------------------------------------
# Plotting switches:

detsw = {}

# Set detector sections on / off
detsw[1] = 1  # DC
detsw[2] = 0  # Other
detsw[3] = 0  # Coil
detsw[4] = 0  # Upstream

#-------------------------------------------
# Set ncand types on / off

ncndsw = {}

ncndsw[1] = 1  # ncand = 2
ncndsw[2] = 0  # ncand = 3
ncndsw[3] = 0  # ncand = 4
ncndsw[4] = 0  # ncand = {3][ 4}

#-------------------------------------------
# Set event topologies on / off

evtsw = {}

evtsw[1] = 0  # CC - DIS
evtsw[2] = 1  # OS DiMu
evtsw[3] = 0  # LS DiMu
evtsw[4] = 0  # OS Mu + X
#-------------------------------------------
# Initialize Post - cut histogram switches:
cuthists = {}

for ii in range(1, ncuts):
    cuthists[ii] = 0

cuthists[ncuts] = 1  # Plot after last cut

# Turn on / off other Post - cut histograms:

cuthists[1] = 0

# Calculate the number of "on" switches:
# ndetsw = sumintarr1d(ndetsec][ detsw)
ndetsw = sum(detsw)
# nncndsw = sumintarr1d(nncand][ ncndsw)
nncndsw = sum(ncndsw)
# nevtsw = sumintarr1d(nevtyp][ evtsw)
nevtsw = sum(evtsw)
# ncuthists = sumintarr1d(ncuts][ cuthists)
ncuthists = sum(cuthists)
#-------------------------------------------

# To run or not to run:
runtype = {}

runtype[0] = 0  # - - > Data
runtype[1] = 1  # - - > CC - DIS
runtype[2] = 1  # - - > NC - DIS
runtype[3] = 1  # - - > J / Psi
runtype[4] = 1  # - - > OBG
runtype[5] = 1  # - - > CohPi+
runtype[6] = 1  # - - > CohRho+
runtype[7] = 1  # - - > aNumu - CC
runtype[8] = 1  # - - > QE[ < 1 event]
runtype[9] = 1  # - - > CohPi0[0 events]
runtype[10] = 1  # - - >  Nue-CC  [< 1 event]
runtype[11] = 1  # - - > aNue - CC[0 events]
runtype[12] = 1  # - - > aNumu-NC [< 1 event]
runtype[13] = 1  # - - > CohRho0  [< 10 events]
runtype[14] = 1  # - - > Res-CC   [< 1 event]
runtype[15] = 1  # - - > CohPhi0  [< 10 events]
runtype[16] = 0  # - - > Heavy Neutrino Parameter
runtype[17] = 0  # - - > Heavy Neutrino Parameter
runtype[18] = 0  # - - > Heavy Neutrino Parameter
runtype[19] = 1  # - - > Heavy Neutrino Parameter
runtype[20] = 0  # - - > Heavy Neutrino Parameter
runtype[21] = 0  # - - > Heavy Neutrino Parameter
runtype[22] = 0  # - - > Heavy Neutrino Parameter
runtype[23] = 0  # - - > Heavy Neurtino Parameter
runtype[24] = 1  # - - > CohPi-

#= == == == == == == == == == == == == == == == == == == == == == == == == == =

'''Set MC generator types:
        1 = NUAGE
        2 = NEGLIB
        3 = GENIE
        4 = NUAGE + NEGLIB
        5 = NUAGE + GENIE
        6 = NEGLIB + GENIE
        7 = NUAGE + NEGLIB + GENIE
'''


gtyp = {}

gtyp[0] = 3  # - - > Data[all choices identical]
gtyp[1] = 3  # - - > CC - DIS
gtyp[2] = 3  # - - > NC - DIS
gtyp[3] = 3  # - - > J / Psi
gtyp[4] = 3  # - - > OBG
gtyp[5] = 1  # - - > CohPi+
gtyp[6] = 3  # - - > CohRho+
gtyp[7] = 3  # - - > aNumu - CC
gtyp[8] = 3  # - - > QE
gtyp[9] = 3  # - - > CohPi0
gtyp[10] = 3  # - - > Nue - CC
gtyp[11] = 3  # - - > aNue - CC
gtyp[12] = 3  # - - > aNumu - NC
gtyp[13] = 3  # - - > CohRho0
gtyp[14] = 3  # - - > Res - CC
gtyp[15] = 3  # - - > CohPhi0
gtyp[16] = 3  # - - > Heavy Neutrino
gtyp[17] = 3  # - - > Heavy Neutrino
gtyp[18] = 3  # - - > Heavy Neutrino
gtyp[19] = 3  # - - > Heavy Neutrino
gtyp[20] = 3  # - - > Heavy Neutrino
gtyp[21] = 3  # - - > Heavy Neutrino
gtyp[22] = 3  # - - > Heavy Neutrino
gtyp[23] = 3  # - - > Heavy Neutrino
gtyp[24] = 1  # - - > CohPi - Allthough it's Calisto Standard / sm
#= == == == == == == == == == == == == == == == == == == == == == == == == == =

#= == == == == == == == == == == == == == == ==
# Table Type Tags

ttag = {}

ttag[0] = ' Data   '
ttag[1] = ' CCDIS  '
ttag[2] = ' NCDIS  '
ttag[3] = ' JPsi   '
ttag[4] = ' OBG    '
ttag[5] = ' CohPi+ '
ttag[6] = ' CohRho+'
ttag[7] = ' aNuMu CC'
ttag[8] = ' QE     '
ttag[9] = ' CohPi0 '
ttag[10] = '  Nue CC'
ttag[11] = ' aNue CC'
ttag[12] = ' aNuMu NC'
ttag[13] = ' CohRho0'
ttag[14] = ' Res    '
ttag[15] = ' CohPhi0'
# Added for heavy neutrino Analysis
ttag[16] = ' HeavyNu_0.250'
# Added for heavy neutrino Analysis
ttag[17] = ' HeavyNu_0.350'
# Added for heavy neutrino Analysis
ttag[18] = ' HeavyNu_0.500'
# Added for heavy neutrino Analysis
ttag[19] = ' HeavyNu_1.000'
# Added for heavy neutrino Analysis
ttag[20] = ' HeavyNu_1.500'
# Added for heavy neutrino Analysis
ttag[21] = ' HeavyNu_2.000'
# Added for heavy neutrino Analysis
ttag[22] = ' HeavyNu_3.000'
# Added for heavy neutrino Analysis
ttag[23] = ' HeavyNu_4.000'
ttag[24] = ' CohPi- '
# = == == == == == == == == == == == == == == ==

#  ******************************************************************
#  *****************----- Generated NT Events #- ----****************
#  *****************---------------------------------****************
#  ------------------------------------------------------------------

gennt = {}

# CCDIS:
gennt[1, 1] = 1.67094362e6  # - - > [NUAGE]
gennt[1, 2] = 4.4777175e6  # - -- > [NEGLIB]
gennt[1, 3] = 4.116629e6  # - --- > [GENIE]

#------------------------------------------------------------------
# NCDIS:
gennt[2, 1] = 1.34534462e6  # - - > [NUAGE]
gennt[2, 2] = 3.2419925e6  # - -- > [NEGLIB]
gennt[2, 3] = 2.45185225e6  # - - > [GENIE]

#------------------------------------------------------------------
# CohJ / Psi[My NEGLIB / Nuage - reader]:
gennt[3, 1] = 337363.344
gennt[3, 2] = 337363.344
gennt[3, 3] = 337363.344
#------------------------------------------------------------------
# OBG[from data]:
gennt[4, 1] = 1.0
gennt[4, 2] = 1.0
gennt[4, 3] = 1.0
#------------------------------------------------------------------
# CohPi + :
#  -- RS Model: --
gennt[5, 1] = 362605.312  # - --- > [NUAGE]
gennt[5, 2] = 412962.812  # - --- > [NEGLIB]
# -- BK Model: --
gennt[5, 1] = 406748.031  # - - > [NUAGE]
gennt[5, 3] = 824186.438  # --- > GENIE][BK?]
#  -- BS Model: --
gennt[5, 1] = 409103.781  # - - > [NUAGE]
#------------------------------------------------------------------
# CohRho+ [My NEGLIB/Nuage-reader]:

gennt[6, 1] = 147680.422
gennt[6, 2] = 147680.422
gennt[6, 3] = 147680.422
#------------------------------------------------------------------
# aNumu-CC:
gennt[7, 1] = 0.234835203e6  # --> [NUAGE]
gennt[7, 2] = 0.3733310e6  # ----> [NEGLIB]
gennt[7, 3] = 0.384881812e6  # --> [GENIE]
#------------------------------------------------------------------
# QE-CC:
gennt[8, 1] = 0.204318438e6  # --> [NUAGE]
gennt[8, 2] = 9.8879750e6  # -----> [NEGLIB]
gennt[8, 3] = 0.418257531e6  # ---> [GENIE]
#------------------------------------------------------------------
# CohPi0 [no GENIE]:
#-- RS Model: --

gennt[9, 1] = 529168.75  # ----> [NUAGE]
gennt[9, 2] = 38022.6055  # ---> [NEGLIB]
#-- BK Model: --
gennt[9, 1] = 179844.781  # --> [NUAGE]
#-- BS Model: --
gennt[9, 1] = 23723.3262  # --> [NUAGE]
# Use NUAGE BK for GENIE default:
gennt[9, 3] = 179844.781  # ----> [NUAGE]
#------------------------------------------------------------------
# Nue-CC:
gennt[10, 1] = 0.173752453e6  # --> [NUAGE]
gennt[10, 2] = 0.425471188e6  # --> [NEGLIB]
gennt[10, 3] = 0.396271906e6  # --> [GENIE]
#------------------------------------------------------------------
# aNue-CC:
gennt[11, 1] = 0.104198117e6  # --> [NUAGE]
gennt[11, 2] = 0.183138078e6  # --> [NEGLIB]
gennt[11, 3] = 0.193616453e6  # --> [GENIE]
#------------------------------------------------------------------
# aNumu-NC:
gennt[12, 1] = 0.091272578e6  # --> [NUAGE]
gennt[12, 2] = 0.184929312e6  # --> [NEGLIB]
gennt[12, 3] = 0.189598922e6  # --> [GENIE]
#------------------------------------------------------------------
# CohRho0 [My NEGLIB/Nuage-reader]
gennt[13, 1] = 25235.83
gennt[13, 2] = 25235.83
gennt[13, 3] = 25235.83
#------------------------------------------------------------------
# Res-CC:
gennt[14, 1] = 0.0863934e6  # ----> [NUAGE]
gennt[14, 2] = 5.600115e6  # ----> [NEGLIB]
gennt[14, 3] = 0.934139812e6  # --> [GENIE]
#------------------------------------------------------------------
# CohPhi0 [My NEGLIB/Nuage-reader]
gennt[15, 1] = 4075.93
gennt[15, 2] = 4075.93
gennt[15, 3] = 4075.93
#------------------------------------------------------------------
# Heavy Neutrino Analysis:
gennt[16, 1] = 4500.00
gennt[16, 2] = 4500.00
gennt[16, 3] = 4500.00

gennt[17, 1] = 4500.00
gennt[17, 2] = 4500.00
gennt[17, 3] = 4500.00

gennt[18, 1] = 4500.00
gennt[18, 2] = 4500.00
gennt[18, 3] = 4500.00

gennt[19, 1] = 4500.00
gennt[19, 2] = 4500.00
gennt[19, 3] = 4500.00

gennt[20, 1] = 4500.00
gennt[20, 2] = 4500.00
gennt[20, 3] = 4500.00

gennt[21, 1] = 4500.00
gennt[21, 2] = 4500.00
gennt[21, 3] = 4500.00

gennt[22, 1] = 4500.00
gennt[22, 2] = 4500.00
gennt[22, 3] = 4500.00

gennt[23, 1] = 4500.00
gennt[23, 2] = 4500.00
gennt[23, 3] = 4500.00

gennt[24, 1] = 824186.438
gennt[24, 2] = 824186.438
gennt[24, 3] = 824186.438


#  ------------------------------------------------------------------
''' Now add events for combined types.  Info for 2nd index:
        1 = NUAGE
        2 = NEGLIB
        3 = GENIE
        4 = NUAGE  + NEGLIB
        5 = NUAGE  + GENIE
        6 = NEGLIB + GENIE
        7 = NUAGE  + NEGLIB  + GENIE
'''
# do ii=1][nmc

for ii in range(1, nmc + 1):
    gennt[ii, 4] = gennt[ii, 1] + gennt[ii, 2]  # Nuage+Neglib
    gennt[ii, 5] = gennt[ii, 1] + gennt[ii, 3]  # Nuage+Genie
    gennt[ii, 6] = gennt[ii, 2] + gennt[ii, 3]  # Neglib+Genie
    gennt[ii, 7] = gennt[ii, 1] + gennt[ii, 2] + gennt[ii, 3]  # All 3

    # ------------------------------------------------------------------
    #*****************---------------------------------****************
    #*****************---------------------------------****************
    #******************************************************************

    #******************************************************************
    #*****************-----  Zeroth Level Norms  -----*****************
    #*****************--------------------------------*****************

#    print(str(gennt))

norm = {}

norm[0, 0] = 1.0  # --------------------> Data
norm[0, 1] = 1.44e6  # -------------------> CCDIS
norm[0, 2] = (0.38 * 1.44e6)  # ----------> NC
norm[0, 3] = (500.0 * 0.0593)  # -------> CohJ/Psi-mumu
norm[0, 4] = 1.0  # --------------------> OBG (From Data]
norm[0, 5] = 10000.0  # ----------------> CohPi+
norm[0, 6] = (1000.0 / 0.1355)  # CohRho+ (sin2=0.2397-->0.1355]
norm[0, 7] = (1.44e6 * 0.025)  # ---------> aNumu-CC
norm[0, 8] = 32000.0  # ----------------> QE-CC
norm[0, 9] = 5000.0  # ----------------> CohPi0
norm[0, 10] = (1.44e6 * 0.015)  # ----------> Nue-CC
norm[0, 11] = (1.44e6 * 0.0015)  # ---------> aNue-CC
norm[0, 12] = (0.38 * 1.44e6 * 0.025)  # --> aNumu-NC
norm[0, 13] = 1000.0  # ----------------> CohRho0
norm[0, 14] = (1.44e6 * 0.035)  # --------> Res-CC
norm[0, 15] = 200.0  # -----------------> CohPhi0
norm[0, 16] = 100.0  # -----------------> Heavy Neutrino
norm[0, 17] = 100.0  # -----------------> Heavy Neutrino
norm[0, 18] = 100.0  # -----------------> Heavy Neutrino
norm[0, 19] = 100.0  # -----------------> Heavy Neutrino
norm[0, 20] = 100.0  # -----------------> Heavy Neutrino
norm[0, 21] = 100.0  # -----------------> Heavy Neutrino
norm[0, 22] = 100.0  # -----------------> Heavy Neutrino
norm[0, 23] = 100.0  # -----------------> Heavy Neutrino
norm[0, 24] = 2000.0  # -----------------> CohPi-

#print('Test= '+str(gennt[1][3]))

# Print normalization settings to .tex file before scaling
print_zeroth_norms(nmc, nttypobg, ttag, gtyp, norm, ntyp, gennt)
# Set zeroth normalization based on generated events in NT:
#        do ii=1][nmc
#         norm(0][ii) = norm(0][ii)/gennt(ii][gtyp(ii))
#        enddo
#*****************--------------------------------*****************
#******************************************************************

#*******************************************************
#****************----  Final Norms  ----****************
#****************-----------------------****************
norm[1, 0] = 1.0    # Data
norm[1, 1] = 1.0    # CC
norm[1, 2] = 1.0  # NC
norm[1, 3] = 1.0  # CohJ/Psi
norm[1, 4] = 0.22  # From CohRho analysis  OBG [non Ph2Mu]
norm[1, 5] = 0.985  # CohPi+ [from 2v0 pi0 analysis]
norm[1, 6] = 0.669  # CohRho+ from CohRho0 measurement
norm[1, 7] = 1.0  # Anumu-CC
norm[1, 8] = 1.0  # QE
norm[1, 9] = 0.985  # CohPi0 [from 2v0 analysis]
norm[1, 10] = 1.0  # Nue-CC
norm[1, 11] = 1.0  # Anue-CC
norm[1, 12] = 1.0  # Anumu-NC
norm[1, 13] = 0.669  # CohRho0 [from measurement]
norm[1, 14] = 1.0  # Res-CC
norm[1, 15] = 1.0  # CohP
norm[1, 16] = 1.0  # Heavy Neutrino
norm[1, 17] = 1.0  # Heavy Neutrino
norm[1, 18] = 1.0  # Heavy Neutrino
norm[1, 19] = 1.0  # Heavy Neutrino
norm[1, 20] = 1.0  # Heavy Neutrino
norm[1, 21] = 1.0  # Heavy Neutrino
norm[1, 22] = 1.0  # Heavy Neutrino
norm[1, 23] = 1.0  # Heavy Neutrino
norm[1, 24] = 1.0  # CohPi-

# Apply normalizations:

for ii in range(1, nmc + 1):
    norm[1, ii] = norm[1, ii] * norm[0, ii]

    #****************-----------------------****************
    #*******************************************************

    #|--------------------------------------------------|
    #|--------------------------------------------------|
    #| Set Datacard Locations (Full NTs):               |


'''    This section sets datacard filenames. If "drep" |
    is 1][ then the value of "dstr" is repeated      |
    exaclty for all file variations][ and the value  |
    of "gtyp" will have no effect on the ntuples    |
    used.  For "drep" of 0 the main name will have  |
    additional tags referring to variations in MC   |
    generator(s) used.                              |

'''  # |
'''    Set standardised datacard names:                 |
   drep = 1  --> Repeats the same "dstr" name for
                 all MC generator types
                 (no variations)
   drep = 0  --> Puts MC generator type tag at end
                    of each "dstr" name
'''


dstr = {}
drep = {}

dstr[0] = 'datafull'  # |
drep[0] = 1
dstr[1] = 'ccfull'  # |
drep[1] = 0
dstr[2] = 'ncfull'  # |
drep[2] = 0
dstr[3] = 'jpsi-mumu'  # |
drep[3] = 1
dstr[4] = 'obgfull'  # |
drep[4] = 1
dstr[5] = 'cohpip'    # [Special][ set below]    #|
drep[5] = 1
dstr[6] = 'cohrhop'  # |
drep[6] = 1
dstr[7] = 'anmcc'  # |
drep[7] = 0
dstr[8] = 'qecc'  # |
drep[8] = 0
dstr[9] = 'cohpi0'    # [Special][ set below]    #|
drep[9] = 1
dstr[10] = 'nuecc'  # |
drep[10] = 0
dstr[11] = 'anecc'  # |
drep[11] = 0
dstr[12] = 'anmnc'  # |
drep[12] = 0
dstr[13] = 'cohrho0'  # |
drep[13] = 1
dstr[14] = 'rescc'  # |
drep[14] = 0
dstr[15] = 'cohphi0'  # |
drep[15] = 1
dstr[16] = 'Hnu_0.250'
drep[16] = 1
dstr[17] = 'Hnu_0.350'
drep[17] = 1
dstr[18] = 'Hnu_0.500'
drep[18] = 1
dstr[19] = 'Hnu_1.000'
drep[19] = 1
dstr[20] = 'Hnu_1.500'
drep[20] = 1
dstr[21] = 'Hnu_2.000'
drep[21] = 1
dstr[22] = 'Hnu_3.000'
drep[22] = 1
dstr[23] = 'Hnu_4.000'
drep[23] = 1
dstr[24] = 'cohpimin'
drep[24] = 1

# Set MC type strings:

dtyp = {}

dtyp[1] = 'nua'  # --> NUAGE
dtyp[2] = 'neg'  # --> NEGLIB
dtyp[3] = 'gen'  # --> GENIE
dtyp[4] = 'nn'  # --> NUAGE + NEGLIB
dtyp[5] = 'ng'  # --> NUAGE + GENIE
dtyp[6] = 'gn'  # --> NEGLIB + GENIE
dtyp[7] = 'nng'  # --> NUAGE + NEGLIB + GENIE

# Make standardized set of names:                  #|

dname = {}

for ii in range(0, nmc + 1):  # |
    ldstr = len(dstr[ii])
    #|
    for jj in range(1, ntyp + 1):  # |
        if drep[ii] == 0:  # |
            dname[ii, jj] = dstr[ii] + '/' + dtyp[jj]  # |

        elif drep[ii] == 1:  # |
            dname[ii, jj] = dstr[ii]  # |


# Modify special cases:
# ..................................................|
#  CohPi+:                                          |
# -- RS Model: --                                |

#dname[5, 1] = 'cohpipnuars'  # [NUAGE]              |
dname[5, 2] = 'cohpipnegrs'  # [NEGLIB]             |
dname[5, 3] = 'cohpipgen'  # [GENIE]              |

#-- BK Model: --                                |
#dname[5, 1] = 'cohpipnuabk'  # [NUAGE]            |
#-- BS Model: --                                |
dname[5, 1] = 'cohpipnuabs'  # [NUAGE]            |

for ii in range(4, ntyp + 1):
    dname[5, ii] = 'NOT ALLOWED'

# ..................................................|

# CohPi0 (no GENIE):                              |
#-- RS Model: --                                |
dname[9, 1] = 'cohpi0nuars'  # [NUAGE]  |
dname[9, 2] = 'cohpi0negrs'  # [NEGLIB] |
#  -- BK Model: --        |
#dname[9, 1] = 'cohpi0nuabk'  # [NUAGE]|
# -- BS Model: --        |
#dname[9, 1] = 'cohpi0nuabs'  # [NUAGE]|
# Use NUAGE BK for GENIE default:     |
dname[9, 3] = 'cohpi0nuabk'  # [NUAGE]   |

for ii in range(4, ntyp + 1):
    dname[9, ii] = 'NOT ALLOWED'

# ..................................................|
# Set full NT datacard locations:                  |

for ii in range(0, nmc + 1):  # |
    datacard[ii, 1] = 'datacard/' + dname[ii, gtyp[ii]]
    #if ii == 16:
        #datacard[ii, 1] = 'datacard/heavyneutrino'

        # ..................................................|
        # ..................................................|

        # |-----------------------------------------------|
        # |-----------------------------------------------|
        # | Set Datacard Locations [Baby NTs]:            |
        # |...............................................|
        # | Initialize locations to full ntuples:         |

for ii in range(0, nmc + 1):  # |
    datacard[ii, 0] = datacard[ii, 1]  # |
    #if ii == 16:
        #datacard[ii, 0] = 'datacard/heavyneutrino'

    for jj in range(1, ntyp + 1):  # |
        bname[ii, jj] = dname[ii, jj]  # |

# ...............................................|
# ...............................................|
# Set babynt name tags:
dstr[0] = 'datababy'  # |
dstr[0] = 'datacoil'  # -70 < zvr < 405       |
dstr[1] = 'ccbaby'  # |
dstr[2] = 'ncbaby'  # |
dstr[4] = 'obgbaby'  # |
dstr[7] = 'amccbaby'  # |
dstr[8] = 'qeccbaby'  # |
dstr[10] = 'neccbaby'  # |
dstr[14] = 'resbaby'  # |
# Make standardized set of names:     |
for ii in range(0, nmc + 1):  # |
    for jj in range(1, ntyp + 1):  # |
        if dstr[ii] != bname[ii, jj]:
            if drep[ii] == 0:  # |
                bname[ii, jj] = dstr[ii, 1:ldstr] // dtyp[jj]  # |
            elif drep[ii] == 1:  # |
                bname[ii, jj] = dstr[ii, 1:ldstr]  # |

         # ...............................................|
         # ...............................................|
# Set baby NT datacard locations:     |
for ii in range(0, nmc + 1):      # |
    datacard[ii, 0] = 'datacard/' + bname[ii, gtyp[ii]]  # |
    #if ii == 16:
        #datacard[16, 0] = 'datacard/heavyneutrino'

        #   |
        # ...............................................|
        # ...............................................|

   # ************************************
        # Set output histogram filenames:
filetag[0] = 'cohjpsi-mumu_data   '
filetag[1] = 'cohjpsi-mumu_ccdis  '
filetag[2] = 'cohjpsi-mumu_ncdis  '
filetag[3] = 'cohjpsi-mumu_jpsi   '
filetag[4] = 'cohjpsi-mumu_obg    '
filetag[5] = 'cohjpsi-mumu_cohpip '
filetag[6] = 'cohjpsi-mumu_cohrhop'
filetag[7] = 'cohjpsi-mumu_anumucc'
filetag[8] = 'cohjpsi-mumu_qe     '
filetag[9] = 'cohjpsi-mumu_cohpi0 '
filetag[10] = 'cohjpsi-mumu_nuecc  '
filetag[11] = 'cohjpsi-mumu_anuecc '
filetag[12] = 'cohjpsi-mumu_anumunc'
filetag[13] = 'cohjpsi-mumu_cohrho0'
filetag[14] = 'cohjpsi-mumu_res    '
filetag[15] = 'cohjpsi-mumu_cohphi0'
filetag[16] = 'heavy_neutrino_0.250'
filetag[17] = 'heavy_neutrino_0.350'
filetag[18] = 'heavy_neutrino_0.500'
filetag[19] = 'heavy_neutrino_1.000'
filetag[20] = 'heavy_neutrino_1.500'
filetag[21] = 'heavy_neutrino_2.000'
filetag[22] = 'heavy_neutrino_3.000'
filetag[23] = 'heavy_neutrino_4.000'
filetag[24] = 'CohPi-   '
#  ************************************

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Screen Output Strings:
mctyp[0] = '|===        Data           ===|'
mctyp[1] = '|===        CCDIS          ===|'
mctyp[2] = '|===        NCDIS          ===|'
mctyp[3] = '|===         JPsi          ===|'
mctyp[4] = '|===        OBG            ===|'
mctyp[5] = '|===       CohPi+          ===|'
mctyp[6] = '|===      CohRho+          ===|'
mctyp[7] = '|===      aNuMu CC         ===|'
mctyp[8] = '|===         QE            ===|'
mctyp[9] = '|===       CohPi0          ===|'
mctyp[10] = '|===       Nue CC          ===|'
mctyp[11] = '|===      aNue CC          ===|'
mctyp[12] = '|===      aNuMu NC         ===|'
mctyp[13] = '|===      CohRho0          ===|'
mctyp[14] = '|===        Res            ===|'
mctyp[15] = '|===      CohPhi0          ===|'
mctyp[16] = '|===  HeavyNeutrino_0.250  ===|'
mctyp[17] = '|===  HeavyNeutrino_0.350  ===|'
mctyp[18] = '|===  HeavyNeutrino_0.500  ===|'
mctyp[19] = '|===  HeavyNeutrino_1.000  ===|'
mctyp[20] = '|===  HeavyNeutrino_1.500  ===|'
mctyp[21] = '|===  HeavyNeutrino_2.000  ===|'
mctyp[22] = '|===  HeavyNeutrino_3.000  ===|'
mctyp[23] = '|===  HeavyNeutrino_4.000  ===|'
mctyp[24] = '|===        CohPi-         ===|'





#      @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# -------------------------------------
 # Set user index values for plotting:
# -------------------------------------
detidx={}

detidx[1] = 0  # DC
detidx[2] = 1  # Other
detidx[3] = 2  # Coil
detidx[4] = 3  # Upstream
# -------------------------------------
ncndidx={}

ncndidx[1] = 2 # ncand = 2
ncndidx[2] = 3 # ncand = 3
ncndidx[3] = 4 # ncand = 4
ncndidx[4] = 5 # ncand = {3,4}
# -------------------------------------
evtidx={}

evtidx[1] = 0  # CC DIS
evtidx[2] = 1  # OS DiMu
evtidx[3] = 2  # LS DiMu
evtidx[4] = 3  # OS Mu+X
# -------------------------------------

      #0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0
      #0o0o0o0o0o0o0o0o0o0o0---       END USER INPUT      ---0o0o0o0o0o0o0o0o0o0o0
      #0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0
      #---------------------------------------------------------------------------


      call timestamp('start') #--> Print timestamp for program start

'''


      #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       Check user settings for errors, and print to file:
      call check_print_settings(nvar,ncuts,nmc,
     |                 maxevts,usents,fullnts,runtype,cuthists,
     |                 norm,datacard,filetag,gtyp,gennt,
     |                 usecorr2D,usecorrMass,usecorrZeta,
     |                 formRenorm,iCorr,
     |                 ndetsec,nncand,nevtyp,
     |                 detsw,ncndsw,evtsw,
     |                 ndetsw,nncndsw,nevtsw,ncuthists)
      #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


      #--------------------------------------
      # Initialize Variables:
       call HLIMIT(nhbook)
       call initvar(nmc][ncuts][nevtyp][nextyp][nncand][ndetsec][cuts)
       extyp=nextyp*nncndsw*ndetsw
      #--------------------------------------


      # ......................................................
      # ......................................................
      # Initialize the renormalization factor:

      # Read current renorm if not making a new one:
      if(formRenorm.eq.0) then
       open(22][file='outputs/renorm.txt'][status='old')
        read(22]['(F12.8)') renorm
       close(22)
      endif
       If forming renorm][ or if not using
       any correction functions][ set the
       renormalization factor to unity.
      if(formRenorm.eq.1.or.
     | ( usecorr2D  .eq.0 .and.
     |   usecorrMass.eq.0 .and.
     |   usecorrZeta.eq.0  )) then
        renorm=1.0D0
      endif
       Initialize Counts
      do ii=1][2
        reCounts(ii) = 0.0
      enddo
       If forming normalizations run over
       only the MC component that has
       correction functions applied][ and
       requires renormalization:
      if(formRenorm.eq.1) then
      do ii=0][nmc
       runtype(ii) = 0
      enddo
      runtype(iCorr)=1
      endif
      # ......................................................
      # ......................................................


      # OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
      # OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
      # Load Correction Functions and Binnings:
      if(runtype(iCorr).eq.1) then

        2D Correction:
       if(usecorr2D.eq.1) then
          Read Correction:
         filename='2ddscorr.txt'
         call read_2dcorr(filename][nbin2Dmass][nbin2Dzeta][DScorr2d)
          Read Equipopulated Binnings:
         filename='2d-mass_binning.txt'
         call load_bins(filename][nbin2Dmass][binlim2Dmass)
         filename='2d-zeta_binning.txt'
         call load_bins(filename][nbin2Dzeta][binlim2Dzeta)
       endif
       # --------------------
        # Mass Correction:
       if(usecorrMass.eq.1) then
          Read Correction:
         filename='masscorr.txt'
         call read_1dcorr(filename][nbinMass][DScorrMass)
          Read Equipopulated Binnings:
         filename='mass_binning.txt'
         call load_bins(filename][nbinMass][binlimMass)
       endif
       # --------------------
        # Zeta Correction:
       if(usecorrZeta.eq.1) then
          # Read Correction:
         filename='zetacorr.txt'
         call read_1dcorr(filename][nbinZeta][DScorrZeta)
          # Read Equipopulated Binnings:
         filename='zeta_binning.txt'
         call load_bins(filename][nbinZeta][binlimZeta)
       endif

      endif
     # OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
     # OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

      print("\n")
      print("\n")
      print("\n")
      print("0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0")
      print("0o0o0o                             o0o0o0")
      print("0o0o0o      Performing Analysis    o0o0o0")
      print("0o0o0o    And Filling Histograms   o0o0o0")
      print("0o0o0o                             o0o0o0")
      print("0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0")
      print("\n")
      print("\n")



      do nttyp=0][nmc --> Loop over ntuple types (DL-01)

         if (runtype(nttyp).eq.0) goto 88 --> Skip unwanted NT types


             print("\n")
             print("\n")
             print'(a31)'       ]['|=============================|'
             print'(a20][i2][a9)']['|===   Working type '][nttyp][':    ===|'
             print'(a31)'       ][      mctyp(nttyp)
             print'(a31)'       ]['|=============================|'
             print("\n")

          # ----------------------------------------------------------
            # Read Ntuple Location Datacards:
            call FindOpenUnit(UnitNum)

            print*]['UnitNum is: '][UnitNum


            print*]['Datacard is: '][datacard(nttyp][fullnts)


            open(UnitNum][file=datacard(nttyp][fullnts)][status='old')
            read(UnitNum][*) ntnum
            if(ntnum.gt.MaxNTS) then
             call printerror(1][482773842]['main.F              ')
             print*]['Too many ntuples. Increse ntname size.'
             print*]['(MaxNTS is too small)'
             print*]['ntnum: '][ntnum
             print*]['nttyp: '][nttyp
             print*]['MaxNTS: '][MaxNTS
             print*]['Datacard: '][datacard(nttyp][fullnts)
             print("\n")
             stop
            endif
            do ii=1][ntnum
              read(UnitNum][55) ntname(ii)
            enddo
            close(UnitNum)
55          format(a100)
            Change the number of ntuples to be read if desired:
            if(usents.gt.0 .and. usents.lt.ntnum) ntnum = usents
         #  ----------------------------------------------------------


         #  ---------------------------------------------------------
            Open output histogram file and book histograms:
             length=LEN_TRIM(filetag(nttyp))
             call FindOpenUnit(hfileID)
             call hropen(hfileID]['OUTPUT']['outputs/hbook/'//
     |                  filetag(nttyp)(1:length)//'.h']['N'][1024][istat)
             call hcdir('//OUTPUT'][' ')
              Histogram Booking:
             call bookhistos(nvar][extyp][ncuts][zetaS][nbintyp][cuthists][
     |                 ndetsec][nncand][nevtyp][nextyp][
     |                 detsw][ncndsw][evtsw][
     |                 ndetsw][nncndsw][nevtsw][ncuthists][
     |                 detidx][ncndidx][evtidx)
         #  ---------------------------------------------------------


         #  ooooooooooooooooooooooooooooooooooooooooo
            Open event picture printout files:
             if (nttyp.eq.0.and.evtpicsw.eq.1)
     |                      call openevtpic(evtID][1)
         #  ooooooooooooooooooooooooooooooooooooooooo



       do ifile=1][ntnum --> Loop over Ntuples (DL-02)


        #    ------------------------
             Set NT header ID
             1000 for Full Ntuples
             501 for Baby Ntuples
            id = 500

            if(fullnts.eq.0      .and.
     |         datacard(nttyp][0).ne.
     |         datacard(nttyp][1) )
     |      id=501
        #    ------------------------

        #    ------------------------------------------------------
             Open Ntuple:
            call FindOpenUnit(ntID)
            call openntuple(ntID][ntname(ifile)][id)
 67         format(1a1][' Working on file '][I3][' of '][I3][': '][a100][$)
            if(ifile.eq.1) print("\n")
            Refresh line:
            print 67][ char(13)][ifile][ntnum][ntname(ifile)
            if(ifile.eq.ntnum) print("\n")
            call hnoent(id][nevents) --> Get # of events in NT
             Change the number of event if desired:
            if(maxevts.gt.0.and.maxevts.lt.nevents) nevents=maxevts
        #    ------------------------------------------------------


        do ievent = 1][nevents   Loop over Events of an Ntuple (DL-03)


          call checkeventerr(id][ievent][err) --> Returns err=0 if OK
          if (err.ne.0) goto 77


        # ============================================================
        # ````````````````````````````````````````````````````````````
          Impose Cuts and Fill Histograms
          call process_event(nmc][nttyp][extyp][nvar][ncuts][
     |                ndetsec][nncand][nevtyp][nextyp][
     |                cuts][norm][
     |                nttypobg][zetaS][cuthists][nbintyp][
     |                usecorr2D][nbin2Dmass][nbin2Dzeta][DScorr2D][
     |                binlim2Dmass][binlim2Dzeta][
     |                usecorrMass][nbinMass][DScorrMass][binlimMass][
     |                usecorrZeta][nbinZeta][DScorrZeta][binlimZeta][
     |                formRenorm][renorm][reCounts][iCorr][evtID][
     |                detsw][ncndsw][evtsw][extrapsw][evtpicsw)
        # ][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][
        # ============================================================


77      continue --> Skip bad events
        enddo --> Loop over Events (DL-03)

            call closentuple(ntID) --> Close ntuple

       enddo --> Loop over Ntuples (DL-02)


      #-----------------------------
       Close output histogram file
       call hcdir('//OUTPUT'][' ')
       call hrout(0][icycle][' ')
       call hrend('OUTPUT')
       close(hfileID)
       call hdelet(0)
      #-----------------------------

      #ooooooooooooooooooooooooooooooooooooooooo
       Close event picture printout files:
        if (nttyp.eq.0.and.evtpicsw.eq.1) then
           Close run/evt file:
          close(evtID(1))
          call closeevtpic(evtID)
        endif
      #ooooooooooooooooooooooooooooooooooooooooo

       Print Cut Table:
      call cut_table(nmc][filetag][ttag][ncuts][
     |           nevtyp][nextyp][nncand][ndetsec][
     |           detsw][ncndsw][evtsw][cuts][norm][nttyp)



88    continue --> Skip unwanted event types
      enddo --> Loop over ntuple types (DL-01)

       Print renormalization factor:
      if (formRenorm.eq.1) call print_renorms(reCounts)


      call timestamp('stop ') #--> Print timestamp for program end
      print("\n")
      print("\n")
      print('**********************')
      print('***    JOB DONE    ***')
      print('**********************')
      print("\n")
      print("\n")

'''
