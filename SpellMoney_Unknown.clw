!Not sure of the author of this code, it is used in some very old accounting that I maintain.
!Released under the MIT License.

Money2Words procedure(string Num)!,string    !Calls Number2Words( "Cents")
    code
    return Number2Words(Num, 102)     !2= do pennies as " and ## Cents", 100=omit cents

Number2Words         FUNCTION (string InNum, BYTE CentsFormat=1)!,string ! Declare Procedure
DecimalNumber        Decimal(31,2)
!CentsFormat's
Cents:OmitAlways     equate(0) !do not include Cents at all
Cents:00_100         equate(1) !and ##/100
Cents:00_Cents       equate(2) !and ## cents
Cents:Words          equate(3) !spell out cents: and Eighty-Five Cents
Cents:Flag_NotIfZero equate(100) !Add 100 to Omit cents if Zero

Cents2s              STRING(2) !the field for Cents
DecimalPosition      LONG
Finger               LONG
J                    LONG
OnesData             STRING('One  Two  ThreeFour Five Six  SevenEightNine {1}')
Ones                 STRING(5),DIM(9),OVER(OnesData)
TeensData            STRING('Ten      Eleven   Twelve   Thirteen Fourteen Fifteen  Sixteen  SeventeenEighteen Nineteen {1}')
Teens                STRING(9),DIM(10),OVER(TeensData)
TensData             STRING('Twenty Thirty Forty  Fifty  Sixty  SeventyEighty Ninety {1}')
Tens                 STRING(7),DIM(8),OVER(TensData)
MagnitudeData        STRING('Hundred ThousandMillion Billion Trillion')
Magnitude            STRING(8),DIM(5),OVER(MagnitudeData)
NumberAmount         STRING(255)
WordAmount           STRING(512)
WordSegment          STRING(128)
QSegment             QUEUE,PRE(QSG)
Number               STRING(3)
                     END

  CODE                                                     ! Begin processed code
   DecimalNumber = Deformat(InNum)
   IF DecimalNumber = 0 THEN RETURN('Zero Dollars and No Cents').
   CLEAR(WordAmount)
   NumberAmount = LEFT(FORMAT(DecimalNumber,@N26.2)) ! change picture here for larger numbers
!find decimal position and then move left until we see a comma or reach the beginning, store 3 digit groups into queue
   DecimalPosition = INSTRING('.',NumberAmount,1,1)
   Finger = DecimalPosition-1
   J = 3
   CLEAR(QSegment)
   LOOP
      IF Finger = 0 THEN BREAK.
      IF NumberAmount[Finger] = ','
         QSG:Number = FORMAT(QSG:Number,@n03)
         ADD(QSegment)
         J = 3
         CLEAR(QSegment)
      ELSE
         QSG:Number[J] = NumberAmount[Finger]
         J -= 1
      END ! IF
      Finger -= 1
   END ! LOOP
   IF QSG:Number   ! pickup partial group
      QSG:Number = FORMAT(QSG:Number,@n03)
      ADD(QSegment)
   END ! IF
! back thru 3 digit groups and convert to text
   LOOP J = RECORDS(QSegment) TO 1 BY -1
      GET(QSegment,J)
      DO ConvertThreeDigitGroup
      WordAmount = CLIP(WordAmount) & ' ' & WordSegment
      IF J > 1 AND CLIP(WordSegment) THEN WordAmount = CLIP(WordAmount) & ' ' & CLIP(Magnitude[J]) . !& ','
   END ! LOOP
   IF DecimalNumber >= 2
      WordAmount = CLIP(WordAmount) & ' Dollars'
   ELSE
      IF DecimalNumber >= 1
         WordAmount = CLIP(WordAmount) & ' Dollar'
      ELSE
         WordAmount = 'Zero Dollars'
      END ! IF
   END ! IF

! pickup any cents
   Cents2s = NumberAmount[DecimalPosition+1 : DecimalPosition+2]
   IF CentsFormat >= Cents:Flag_NotIfZero        !do nothing if cents are zero
      CentsFormat -= Cents:Flag_NotIfZero        !strip off the Flag
      IF Cents2s=0 Then goto ExitLabel.
   END

   Case CentsFormat
   OF Cents:OmitAlways               !do not include Cents at all
      !goto ExitLabel
   OF Cents:00_100                   !and ##/100
      WordAmount = CLIP(WordAmount)&' and ' & Cents2s & '/100'
   OF Cents:00_Cents                 !and ## cents
      WordAmount = CLIP(WordAmount)&' and ' & Cents2s & choose(Cents2s='01',' Cent',' Cents')
   OF Cents:Words                    !spell out cents: and Eighty-Five Cents
      DO WordCentsRtn
   END

ExitLabel
   FREE(QSegment)
   RETURN(LEFT(WordAmount))
!
WordCentsRtn    routine
   CLEAR(QSegment)
   IF NumberAmount[DecimalPosition+1:DecimalPosition+2] = '00'
      WordAmount = CLIP(WordAmount)&' and NO Cents'
   ELSE
      QSG:Number = FORMAT(NumberAmount[DecimalPosition+1:DecimalPosition+2],@N03)
      DO ConvertThreeDigitGroup
      IF NumberAmount[DecimalPosition+1:DecimalPosition+2] > 1
         WordAmount = CLIP(WordAmount)&' and '&CLIP(WordSegment) & ' Cents'
      ELSE
         WordAmount = CLIP(WordAmount)&' and '&CLIP(WordSegment) & ' Cent'
      END ! IF
   END ! IF

!
!
ConvertThreeDigitGroup  ROUTINE
    CLEAR(WordSegment)
    IF QSG:Number[1] > 0 THEN WordSegment = CLIP(Ones[QSG:Number[1]]) & ' Hundred'.
    IF QSG:Number[2:3] > 19   ! over the teens
       WordSegment = CLIP(WordSegment) &' '& Tens[QSG:Number[2]-1]
       IF QSG:Number[3] > 0 THEN WordSegment = CLIP(WordSegment) &'-'& Ones[QSG:Number[3]].
    ELSE
       IF QSG:Number[2:3] > 9   ! teens
          WordSegment = CLIP(WordSegment) &' '& Teens[QSG:Number[3]+1]
       ELSE
          IF QSG:Number[3] > 0 THEN WordSegment = CLIP(WordSegment) &' '& Ones[QSG:Number[3]].
       END ! IF
    END ! IF
    WordSegment = LEFT(WordSegment)
    Exit



