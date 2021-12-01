!November 2021 posted in ClarionLive Skype Chat by Mark Goldberg in reply to Ted Stevens
!
!Note: the Prototype places the passed Amount in a REAL which I would change to a STRING or *DECIMAL.
!I would be concerned about the REAL getting a penny wrong, probably not but there is a reason Clarion added DECIMALs.
!The REAL is assinged to a lcl:InputNum DECIMAL(28,2) so no potential for penny errors.

!=====================================================================================
SpellNum                  PROCEDURE(REAL pInputNum)!,STRING
!Region Documentation
! ============================================================
! Purpose    : Spell out a number
!                  Assume Dollars and Cents
!                      ex: 63.46 -> Sixty Three Dollars and 46 Cents
!                  Assume American System of Numbers
!                      I`ve read that 10^9 is Milliard not Billion in the British System of Numbers
!                      I`ve read that trillion in the British System is 10^18 not 10^12
!                        and that each unit goes up by 10^6 not 10^3
!
! Prototype  : spellnum(real pInputNum),string
!
! Created    :  5/05/96 by Mark Goldberg, Monolith Custom Computing, Inc.
! Updates    : 01/13/97 Copied into Brickwin
!              02/10/97 Added Commas to text for every 3 digits (per HG)
!              03/27/01 Noticed a double comma for 1,000,000.00
!
! Limitations: String Length of return value,
!                 if desired, change function to a PROCEDURE with a pass by address string for the 'return' value
!              Numbers > 10^45 - 1 not supported,
!                 there is nothing intrinsic in the code to stop this support, but I had to stop somewhere
!                 note: 10^45 - 1 is one HUGE number
!
!              387,345,345.14 -> came out with 13/100 Dollars
!
! samples    :    235.23  Two Hundred Thirty Five and 23/100 Dollars
!                  14.15  Fourteen Dollars and 15/100 Dollars
!           1,234,134.34  One Million, Two Hundred Thirty Four Thousand, One Hundred Thirty Four and 34/100 Dollars
!
!
! see http://www.jimloy.com/math/billion.htm
! ============================================================
!EndRegion Documentation

lcl:SpelledNum   STRING(511) !arbitrary limit in string size, hopefully not a problem
lcl:SpelledTmp   STRING(100) !should see what max could be, this is minor overkill
lcl:ThousandsGrouping  BYTE
lcl:Units        STRING(13)  !size of lcl:UnitsArray[x]
lcl:UnitsValue   short       !Integer 0 to 999
lcl:InputNum     Decimal(28,2) !<----------------------------- limitation 2/10/97

                    !123456789 123
UnitValues   STRING('             ' & |
                    ' Thousand    ' & |
                    ' Million     ' & |
                    ' Billion     ' & |
                    ' Trillion    ' & |
                    ' Quadrillion ' & |
                    ' Quintillion ' & |
                    ' Sextillion  ' & |
                    ' Septillion  ' & |
                    ' Octillion   ' & |
                    ' Nonillion   ' & |
                    ' Decillion   ' & |
                    ' Undecillion ' & |
                    ' Duodecillion' & |
                    ' Tredecillion'   |
                   )
                   group,over(UnitValues),pre
lcl:UnitsArray  STRING(13),dim(15)    ! dim(x) where x = size(ArrayValues) / 13
                   end
                      !123456
DigitValues    STRING('      ' & |
                      ' One  ' & |
                      ' Two  ' & |
                      ' Three' & |
                      ' Four ' & |
                      ' Five ' & |
                      ' Six  ' & |
                      ' Seven' & |
                      ' Eight' & |
                      ' Nine '   |
                     )
Unique1           group,over(DigitValues),pre
lcl:DigitArray      STRING(6),dim(10)
                  end

                    !123456789 123
TeenValues   STRING(' Ten      ' & |
                    ' Eleven   ' & |
                    ' Twelve   ' & |
                    ' Thirteen ' & |
                    ' Fourteen ' & |
                    ' Fifteen  ' & |
                    ' Sixteen  ' & |
                    ' Seventeen' & |
                    ' Eighteen ' & |
                    ' Nineteen '   |
                   )
Unique2           group,over(TeenValues),pre
lcl:TeensArray      STRING(10),dim(10)
                  end

                       !123456789 123
EveryTenValues  STRING(' Twenty ' & |
                       ' Thirty ' & |
                       ' Forty  ' & |
                       ' Fifty  ' & |
                       ' Sixty  ' & |
                       ' Seventy' & |
                       ' Eighty ' & |
                       ' Ninety '   |
                      )

Unique3             group,over(EveryTenValues),pre
lcl:EveryTenArray       STRING(8),dim(8)
                    end
lcl:Comma            uShort

  code
   lcl:InputNum   = pInputNum
   !====================================== handle decimal portion
   lcl:UnitsValue = 100 * (lcl:InputNum - int(lcl:InputNum))
   lcl:SpelledNum = ' ' & FORMAT(lcl:UnitsValue,@N02) & '/100 Dollars'  !based on a recent Income Tax return <G>

  !for 'Cents' support, Rem the line above and UnRem the following lines in 'decimal portion'

  ! case lcl:UnitsValue
  !   of 0; lcl:SpelledNum =                            'No Cents'
  !   of 1; lcl:SpelledNum =                             '1 Cent'
  !   else  lcl:SpelledNum = FORMAT(lcl:UnitsValue,@N2) & ' Cents'
  ! end
   lcl:InputNum = int(lcl:InputNum)

   if lcl:InputNum > 1
  !    lcl:SpelledNum = ' Dollars and' & lcl:SpelledNum
       lcl:SpelledNum =         ' and' & lcl:SpelledNum
   end
   !====================================== handle decimal portion --end

   lcl:ThousandsGrouping = 1
   loop while lcl:InputNum
      lcl:UnitsValue = lcl:InputNum % 100
      case lcl:UnitsValue
          of  0 to  9;   lcl:SpelledTmp =      lcl:DigitArray    [lcl:UnitsValue           + 1]
          of 10 to 19;   lcl:SpelledTmp =      lcl:TeensArray    [lcl:UnitsValue           - 9]

          of 20 to 99;   lcl:SpelledTmp = CLIP(lcl:EveryTenArray [int(lcl:UnitsValue / 10) - 1]) & |
                                               lcl:DigitArray    [(lcl:UnitsValue % 10)    + 1]
      end !end case


      if lcl:InputNum % 1000 > 99
         lcl:SpelledTmp  = CLIP(lcl:DigitArray[int((lcl:InputNum % 1000)/100)+1]) & ' Hundred' & lcl:SpelledTmp
      end

      if lcl:InputNum % 1000
        if lcl:ThousandsGrouping <= maximum(lcl:UnitsArray,1)
             lcl:Units = lcl:UnitsArray[lcl:ThousandsGrouping]
        else lcl:Units = ' ERROR' !<-------------------------------- POLICY
        end
        lcl:SpelledNum = CLIP(lcl:SpelledTmp) & CLIP(lcl:Units) & lcl:SpelledNum
      end

      lcl:InputNum = int(lcl:InputNum / 1000)
      lcl:ThousandsGrouping += 1
      if lcl:InputNum !and lcl:ThousandsGrouping >= ????
         lcl:SpelledNum = ',' & lcl:SpelledNum
      end
   end !end loop while lcl:InputNum

   lcl:SpelledNum = lcl:SpelledNum[2 : size(lcl:SpelledNum) - 1]

   loop
      lcl:Comma =  INSTRING(',,',lcl:SpelledNum,1)
      if ~lcl:Comma then break end
      lcl:SpelledNum = lcl:SpelledNum[             1 : lcl:Comma            ] & |
                       lcl:SpelledNum[ lcl:Comma + 2 : size(lcl:SpelledNum) ]
   end

   return(lcl:SpelledNum)
