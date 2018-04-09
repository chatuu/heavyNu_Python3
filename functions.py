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
    currentDate=" {:%b %d %Y} ".format(today)
    currentTime=time.strftime(' %a %H:%M:%S ')
    dateTimeStamp=currentDate+currentTime

    if startstop == 'start':

        f = open('outputs/timestamp_start.txt', 'w+')
        print('Program Started on: '+dateTimeStamp)
        f.write(dateTimeStamp)
        f.close()

    elif startstop == 'stop':
        f = open('outputs/timestamp_stop.txt', 'w+')
        print('Program Finished on: '+dateTimeStamp)
        f.write(dateTimeStamp)
        f.close()

    else:
        printerror(2, 487739021, 'timestamp.F')
        print('\n startstop value incorrect: ' + startstop)
        print('\n Aborting timestamp!')
        print('\n')
