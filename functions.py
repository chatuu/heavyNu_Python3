import sys
import datetime
import time


def print_zeroth_norms(nmc, nttypobg, ttag, gtyp, norm, ntyp, gennt):
    print(' ')
    print('|-----------------------|')
    print('| Printing Zeroth Norms |')
    print('|-----------------------|')
    print(' ')
    f = open('outputs/zeroth_norms.tex', 'w+')
    f.write('\n\\begin{table}[h!]\\centering')
    f.write('\n{\\large{')
    f.write('\n\\begin{tabular}{||l|r|r||}')
    f.write('\n\\hline')
    f.write('\n\\hline')
    f.write('\n      & Gen Number   & Zroth Norm  \\\\')
    f.write('\n\\hline')
    for ii in range(1, nmc + 1):
        if ii != nttypobg:
            name = str(ttag[ii])
            f.write('\n {:20s} & {:10.1f} & {:10.1f} \\\\'.format(
                name.replace("_", "\_"), gennt[ii, gtyp[ii]], norm[0, ii]))
    f.write('\n\\hline')
    f.write('\n\\hline')
    f.write('\n\\end{tabular}')
    f.write('\n\\caption{Generated Number of MC Events}')
    f.write('\n\\label{tab-gen-numbers}')
    f.write('\n}}')
    f.write('\n\\end{table}')
    f.write('\n\\endinput')
    f.close()


def printerror(typ, id, filename):

    # typ -->  =1 for error (stops program)
    #          =2 for warning
    # id --> Error/warning number
    #        (9 or fewer digits)
    # filename --> name of subroutine calling printerror
    #=======================================#
    # Check input                           #
    if typ != 1 and typ != 2:
        print('\n')
        print('\n')
        print('Error 3885399')
        print('Bad typ input into printerror.F')
        print('typ: ' + str(typ))
        print('id: ' + str(id))
        print('filename: ' + str(filename))
        print('\n')
        sys.exit('Program Quit..!')

    if id < 1 or id > 999999999:
        print('\n')
        print('\n')
        print('Error 599488377')
        print('Bad id input into printerror.F')
        print('id: ' + str(id))
        print('typ: ' + str(typ))
        print('filename: ' + str(filename))
        print('\n')
        sys.exit('Program Quit..!')

#=======================================#

# Error printing:
    if typ == 1:
        print('\n')
        print('\n')
        print('{:35s}'.format('|*********************************|'))
        print('{:11s}{:20s}{:2s}'.format('| Error in: ', filename, '  |'))
        print('{:35s}'.format('|                                 |'))
        print('{:12s}{:9d}{:12s}'.format('|      Id No:', id, '            |'))
        print('{:35s}'.format('|                                 |'))
        print('{:35s}'.format('|       STOPPING PROGRAM..!       |'))
        print('{:35s}'.format('|*********************************|'))
        print('\n')
        sys.exit('Program Quit..!')
# Warning printing:
    elif typ == 2:

        print('\n')
        print('\n')
        print('{:35s}'.format('|*********************************|'))
        print('{:11s}{:20s}{:2s}'.format('|Warning in: ', filename, ' |'))
        print('{:35s}'.format('|                                 |'))
        print('{:12s}{:9d}{:12s}'.format('|      Id No:', id, '            |'))
        print('{:35s}'.format('|                                 |'))
        print('{:35s}'.format('|       PROGRAM CONTINUES..!      |'))
        print('{:35s}'.format('|*********************************|'))
        print('\n')

    else:
        print('\n')
        print('\n')
        print('Error 39960299' + str(typ))
        print('\n')
        sys.exit('Program Quit..!')


def timestamp(startstop):

    today = datetime.date.today()
    currentDate = " {:%b %d %Y} ".format(today)
    currentTime = time.strftime(' %a %H:%M:%S ')
    dateTimeStamp = currentDate + currentTime

    if startstop == 'start':

        f = open('outputs/timestamp_start.txt', 'w+')
        print('Program Started on: ' + dateTimeStamp)
        f.write(dateTimeStamp)
        f.close()

    elif startstop == 'stop':
        f = open('outputs/timestamp_stop.txt', 'w+')
        print('Program Finished on: ' + dateTimeStamp)
        f.write(dateTimeStamp)
        f.close()

    else:
        printerror(2, 487739021, 'timestamp.F')
        print('\n startstop value incorrect: ' + startstop)
        print('\n Aborting timestamp!')
        print('\n')


def check_print_settings(nvar, ncuts, nmc, maxevts, usents, fullnts, runtype, cuthists, norm, datacard, filetag, gtyp,
                         gennt, usecorr2D, usecorrMass, usecorrZeta, formRenorm, iCorr, ndetsec, nncand, nevttyp, detsw,
                         ncndsw, evtsw, ndetsw, nncndsw, nevtsw, ncuthists):

    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
    # Check user input for errors:                                    !
    if nvar < 1 or nvar > 99:                                  #
        printerror(1, 677854432, 'main.F')              #
        print('nvar out of range: ' + str(nvar)
              )                                #
        print('Must modify histogram booking for nvar>99')               #
        print('')                                                        #
        #
        sys.exit('Program Quit..!')

    if ncuts < 1:                                              #
        printerror(1, 577483986, 'main.F')              #
        print('ncuts out of range: ' + str(ncuts)
              )                              #
        print('')                                                        #
        #
        sys.exit('Program Quit..!')

    if nmc < 1:                                                #
        printerror(1, 229388471, 'main.F')              #
        print('nmc out of range: ' + str(nmc)
              )                                  #
        #
        sys.exit('Program Quit..!')
        #
    if maxevts < 0:                                             #
        printerror(1, 847711119, 'main.F')              #
        print('maxevts out of range: ' + str(maxevts)
              )                          #
        #
        sys.exit('Program Quit..!')
        #
    if usents < 0:                                              #
        printerror(1, 111992854, 'main.F')              #
        print('usents out of range: ' + str(usents)
              )                            #
        #
        sys.exit('Program Quit..!')
        #
    if fullnts != 1 and fullnts != 0:                            #
        printerror(1, 222999331, 'main.F')              #
        print('fullnts out of range: ' + str(fullnts)
              )                          #
        print('\n')                                                        #
        #
        sys.exit('Program Quit..!')
        #
    for ii in range(0, nmc + 1):                                                       #
        if runtype[ii] != 1 and runtype[ii] != 0:                    #
            printerror(1, 122211847, 'main.F')              #
            print('runtype out of range#')                                   #
            print('ii, runtype[ii]: ' + str(ii) + ' ' +
                  str(runtype[ii]))                    #
            #
            print('\n')
            #
            sys.exit('Program Quit..!')
            #
    for ii in range(1, ncuts + 1):                                                     #
        if cuthists[ii] != 1 and cuthists[ii] != 0:                  #
            printerror(1, 448833001, 'main.F')              #
            print('cuthists out of range#')                                  #
            print('ii, cuthists[ii]: ' + str(ii) + ' ' +
                  str(cuthists[ii]))                  #
            #
            sys.exit('Program Quit..!')
            #
    for ii in range(0, 2):                                                         #
        for jj in range(0, nmc + 1):                                                       #
            if norm[ii, jj] < 0.0:                                     #
                printerror(1, 687775921, 'main.F')              #
                #
                print('norm out of range#')
                print('ii, jj, norm(ii,jj): ' + str(ii) + ' ' +
                      str(jj) + ' ' + str(norm[ii, jj]))         #
                #
                print('\n')
                #
                sys.exit('Program Quit..!')

    for ii in range(0, nmc + 1):                                                       #
        if gtyp[ii] < 1 or gtyp[ii] > 7:                           #
            printerror(1, 668740012, 'main.F')              #
            print('gtyp out of range#')                                      #
            print('ii, gtyp[ii]: ' + str(ii) + ' ' +
                  str(gtyp[ii]))                          #
            #
            print('\n')
            #
            sys.exit('Program Quit..!')

    for ii in range(1, nmc + 1):                                                       #
        if gennt[ii, gtyp[ii]] < 0.0:                              #
            printerror(1, 100029837, 'main.F')              #
            print('gennt out of range#')                                     #
            print('ii, gtyp[ii]: ' + str(ii) + ' ' +
                  str(gtyp[ii]))                          #
            print('gennt(' + str(ii) + ',' +
                  str(gtyp[ii]) + '): ' + str(gennt[ii, gtyp[ii]]))         #
            #
            print('\n')
            #
            sys.exit('Program Quit..!')

    if usecorr2D != 0 and usecorr2D != 1:                        #
        printerror(1, 774665367, 'main.F')              #
        print('usecorr2D out of range: ' +
              str(usecorr2D))                      #
        #
        sys.exit('Program Quit..!')
        #
    if usecorrMass != 0 and usecorrMass != 1:                    #
        printerror(1, 111982763, 'main.F')              #
        print('usecorrMass out of range: ' +
              str(usecorrMass))                  #
        #
        sys.exit('Program Quit..!')
        #
    if usecorrZeta != 0 and usecorrZeta != 1:                    #
        printerror(1, 222988777, 'main.F')              #
        print('usecorrZeta out of range: ' +
              str(usecorrZeta))                  #
        #
        sys.exit('Program Quit..!')
        #
    if formRenorm != 0 and formRenorm != 1:                      #
        printerror(1, 229938000, 'main.F')              #
        print('formRenorm out of range: ' +
              str(formRenorm))                    #
        #
        sys.exit('Program Quit..!')
        #
    if iCorr < 0 or iCorr > nmc:                               #
        printerror(1, 228837641, 'main.F')              #
        print('iCorr out of range: ' + str(iCorr)
              )                              #
        #
        sys.exit('Program Quit..!')
        #
    if ndetsec < 1:                                             #
        printerror(1, 487652108, 'main.F')              #
        print('ndetsec out of range: ' + str(ndetsec)
              )                          #
        print('\n')                                                        #
        #
        sys.exit('Program Quit..!')
        #
    for ii in range(1, ndetsec + 1):                                                   #
        if detsw[ii] != 1 and detsw[ii] != 0:                        #
            printerror(1, 115458857, 'main.F')              #
            print('detsw out of range#')                                     #
            print('ii, detsw[ii]: ' + str(ii) + ' ' +
                  str(detsw[ii]))                        #
            sys.exit('Program Quit..!')
            #
    if ndetsw < 1:                                              #
        printerror(1, 909087623, 'main.F')              #
        print('ndetsw out of range: ' + str(ndetsw)
              )                            #
        print('\n')                                                        #
        #
        sys.exit('Program Quit..!')
        #
    if nncand < 1:                                              #
        printerror(1, 228327738, 'main.F')              #
        print('nncand out of range: ' + str(nncand)
              )                            #
        print('\n')                                                        #
        sys.exit('Program Quit..!')
        #
    for ii in range(1, nncand + 1):                                                    #
        if ncndsw[ii] != 1 and ncndsw[ii] != 0:                      #
            printerror(1, 3994881, 'main.F')              #
            print('ncndsw out of range#')                                    #
            print('ii, ncndsw[ii]: ' + str(ii) + ' ' +
                  str(ncndsw[ii]))                      #
        #
            sys.exit('Program Quit..!')
            #
            #
    if nncndsw < 1:                                             #
        printerror(1, 98768761, 'main.F')              #
        print('nncndsw out of range: ' + str(nncndsw)
              )                          #
        print('\n')                                                        #
        sys.exit('Program Quit..!')
        #
    if nevttyp < 1:                                             #
        printerror(1, 333728875, 'main.F')              #
        print('nevttyp out of range: ' + str(nevttyp)
              )                          #
        print('\n')                                                        #
        #
        sys.exit('Program Quit..!')
        #
    for ii in range(1, nevttyp + 1):                                                   #
        if evtsw[ii] != 1 and evtsw[ii] != 0:                        #
            printerror(1, 577462198, 'main.F')              #
            print('evtsw out of range#')                                     #
            print('ii, evtsw[ii]: ' + str(ii) + ' ' +
                  str(evtsw[ii]))                        #
        #
            sys.exit('Program Quit..!')
            #
            #
    if nevtsw < 1:                                              #
        printerror(1, 577648910, 'main.F')              #
        print('nevtsw out of range: ' + str(nevtsw)
              )                            #
        print('\n')                                                        #
        #
        sys.exit('Program Quit..!')
        #
    if ncuthists < 1:                                           #
        printerror(1, 222938816, 'main.F')              #
        print('ncuthists out of range: ' +
              str(ncuthists))                      #
        print('\n')                                                        #
        #
        sys.exit('Program Quit..!')
        #
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

    if maxevts != 0:
        print('\n')
        print('\n')
        print('\n')
        print('************************************************')
        print('***                 WARNING#                 ***')
        print('*** May not be using the full set of events# ***')
        print('************************************************')
        print('\n')
        print('maxevts: ' + str(maxevts))
        print('\n')
        print('\n')

    if usents != 0:
        print('\n')
        print('\n')
        print('\n')
        print('*************************************************')
        print('***                   WARNING#                ***')
        print('*** May not be using the full set of ntuples# ***')
        print('*************************************************')
        print('\n')
        print('usents: ' + str(usents))
        print('\n')
        print('\n')

# Print settings to output:

    f = open('outputs/settings.txt', 'w+')
    f.write('\nUser Settings:')
    f.write('\n')
    f.write('\n{:42s}{:3d}'.format(
        'nmc (# of MC components):                 ', nmc))
    f.write('\n{:42s}{:3d}'.format(
        'fullnts (full (1) or baby (0) ntuples):   ', fullnts))
    f.write('\n{:42s}{:3d}'.format(
        'maxevts (# of events to process (0=all)): ', maxevts))
    f.write('\n{:42s}{:3d}'.format(
        'usents (# of ntuples to process (0=all)): ', usents))
    f.write('\n{:42s}{:3d}'.format(
        'nvar (# of variables plotted):            ', nvar))
    f.write('\n{:42s}{:3d}'.format(
        'ncuts (# of cuts exacted):                ', ncuts))
    f.write('\n{:42s}{:3d}'.format('usecorr2D:          ', usecorr2D))
    f.write('\n{:42s}{:3d}'.format('usecorrMass:        ', usecorrMass))
    f.write('\n{:42s}{:3d}'.format('usecorrZeta:        ', usecorrZeta))
    f.write('\n{:42s}{:3d}'.format('formRenorm:         ', formRenorm))
    f.write('\n{:42s}{:3d}'.format('iCorr:              ', iCorr))
    f.write('\n-------------------------------------------------')
    f.write('\nruntypes (1=run, 0=skip):')

    for ii in range(0, nmc + 1):
        f.write('\n{:8s}{:2d}{:3s}{:2d}'.format(
            'runtype(', ii, '): ', runtype[ii]))
    f.write('\n-------------------------------------------------')
    f.write('\nDatacards used:')
    for ii in range(0, nmc + 1):
        if runtype[ii] != 0:
            f.write('\n{:9s}{:2d}{:1s}{:2d}{:4s}{:20s}'.format('datacard(', ii, ',', fullnts, ') : ',
                                                                  datacard[ii, fullnts]))
    f.write('\n-------------------------------------------------')
    f.write('\nMC types used (1=Nuage, 2=Neglib, 3=Genie):      ')
    f.write('\n              ( 4-7 are permutations)            ')

    for ii in range(0, nmc + 1):
        if runtype[ii] != 0:
            f.write('\n{:5s}{:2d}{:3s}{:2d}'.format(
                'gtyp(', ii, '): ', gtyp[ii]))

    f.write('\n-------------------------------------------------')
    f.write('\nGenerated number of MC events in ntuples:        ')

    for ii in range(1, nmc + 1):
        if runtype[ii] != 0:
            f.write('\n{:6s}{:2d}{:3s}{:12.2f}'.format(
                'gennt(', ii, '): ', gennt[ii, gtyp[ii]]))

    f.write('\n-------------------------------------------------')
    f.write('\nNormalizations used (norm0 then normTot):')

    for ii in range(0, nmc + 1):
        if runtype[ii] != 0:
            f.write('\n{:7s}{:2d}{:3s}{:10.6f}{:1s}{:10.6f}'.format('norm(*,', ii, '): ', norm[0, ii],
                                                                       ' ', norm[1, ii]))

    f.write('\n-------------------------------------------------')
    f.write('\ncuthists (1=plot for cut, 0=skip:')

    for ii in range(1, ncuts + 1):
        f.write('\n{:9s}{:2d}{:3s}{:2d}'.format(
            'cuthists(', ii, '): ', cuthists[ii]))

    f.write('\n-------------------------------------------------')
    f.write('\nHistogram naming tags:')

    for ii in range(0, nmc + 1):
        f.write('\n{:8s}{:2d}{:3s}{:20s}'.format(
            'filetag(', ii, '): ', filetag[ii]))

    f.write('\n-------------------------------------------------')
    f.write('\nEND OF SETTINGS')
    f.close()
