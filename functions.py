#def FindOpenUnit(UN):


def print_zeroth_norms(nmc,nttypobg,ttag,gtyp,norm,ntyp,gennt):
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
    for ii in range(1,nmc+1):
        if ii!=nttypobg:
            name=str(ttag[ii])
            f.write('\n {:20s} & {:10.1f} & {:10.1f} \\\\'.format(name.replace("_","\_"),gennt[ii,gtyp[ii]],norm[0,ii]))
    f.write('\n\\hline')
    f.write('\n\\hline')
    f.write('\n\\end{tabular}')
    f.write('\n\\caption{Generated Number of MC Events}')
    f.write('\n\\label{tab-gen-numbers}')
    f.write('\n}}')
    f.write('\n\\end{table}')
    f.write('\n\\endinput')
    f.close()


'''
    55    format(a10,' & ',F10.1,' & ',F10.1,' \\\\')
    do ii=1,nmc
     if(ii.ne.nttypobg)
    |   write(UN,55) ttag(ii), gennt(ii,gtyp(ii)), norm(0,ii)
    enddo


'''
