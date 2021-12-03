!----------------------------------------------------------------------------------------------
! To verify these functions from 3 different authors work compare them to each other for all numbers 1 to 9,999,999
! Original wording had some diffs
! 1. Carl 1: Two Hundred Thirty-Four Thousand Five Hundred Sixty-Seven and 89/100           No Dollars
! 2. Test 2: Two Hundred Thirty-Four Thousand Five Hundred Sixty-Seven Dollars and 89/100   Dollars diff
! 3. Mark 3: Two Hundred Thirty Four Thousand, Five Hundred Sixty Seven and 89/100 Dollars  Dollars diff, commas, no hyphens 
!
! Differences that were changed
! #1 the gold standard so not modified                  AmountInWords()
! #2 take out word "Dollars"                            Number2Words_T2()
! #3 take out word "Dollars" and commas, add hyphens    SpellNum_T3()
!
!   By Carl Barnes - https://github.com/CarlTBarnes
!
!----------------------------------------------------------------------------------------------
    PROGRAM
    include 'EQUATES.CLW'
    include 'KEYCODES.CLW'
    MAP
Test3MoneyWords PROCEDURE()
CardinalNumber  PROCEDURE(LONG InNumber),STRING  !Numbers < 1 billion
AmountInWords   PROCEDURE(STRING Amount2Word, USHORT CentsFormat=0),STRING  !Decimal Amount in Words 
Number2Words_T2 FUNCTION(string InNum, BYTE CentsFormat=1),string !Cents 1=and ##/100
SpellNum_T3     PROCEDURE(REAL pInputNum),STRING    !Mark Goldberg Version
    END
    CODE
    Test3MoneyWords()
!=====================================    
Test3MoneyWords PROCEDURE()    

OneLen      LONG
MaxSize     LONG
MaxChoice   LONG
MaxLText    STRING(256)
TimeBeg     LONG
AmountNum   DECIMAL(15,2,1234567.89)
AmountRng1  DECIMAL(15,2,      1.23)
AmountRng2  DECIMAL(15,2,9999999.23)
AmountRngStep   DECIMAL(9,2, 1.00)
AmountRngNdx    DECIMAL(15,2),AUTO
AmountRngNum    DECIMAL(15,2),AUTO
MaxAmount       DECIMAL(15,2)
CheckQ   QUEUE,Pre(ChkQ)
Amount      DECIMAL(15,2)
Failed      STRING(7)       !ChkQ:Failed
LText       STRING(256)     !Carl's AmountInWords
LTip        STRING(256)     
LText_T2    STRING(256)     !Unknown Number2Words_T2
LText_T3    STRING(256)     !Mark G SpellNum_T3

        END
CentsHow  USHORT(0)
CentsZero USHORT(0)
CentsFormat USHORT(0)

Window WINDOW('Test Money to Words for 3 Functions'),AT(,,456,170),GRAY,IMM,SYSTEM,MAX,ICON(ICON:Thumbnail), |
            FONT('Segoe UI',10),RESIZE
        PROMPT('Note: if the words for all 3 functions do not match the program will stop with a Mes' & |
                'sage()'),AT(9,3),USE(?TestFYI)
        PROMPT('&Amount to Wordify:'),AT(9,19),USE(?AmPrompt1)
        SPIN(@n14.2),AT(9,31,72,10),USE(AmountNum),HVSCROLL,TIP('Limit is 999 Million'),ALRT(EnterKey)
        GROUP,AT(90,16,252,28),USE(?RangeGroup)
            PROMPT('&Range:'),AT(95,19,23),USE(?AmxPrompt2),RIGHT
            SPIN(@n14.2),AT(122,18,72,10),USE(AmountRng1),HVSCROLL
            STRING('To:'),AT(95,32,23),USE(?AmxToStr),RIGHT
            SPIN(@n14.2),AT(122,31,72,10),USE(AmountRng2),HVSCROLL
            STRING('By'),AT(211,19),USE(?AmxByStr)
            ENTRY(@n8.2),AT(200,31,32,10),USE(AmountRngStep),CENTER
            BUTTON('&Test<13,10>Range'),AT(248,18,40,23),USE(?AmountRangeBtn),TIP('Max length found ' & |
                    'will be on clipboard<13,10>and highlighted in the list')
            BUTTON('&Empty'),AT(299,25,35,12),USE(?AmountEmptyBtn),TIP('Empty the Queue')
        END
        PROMPT('Cents Format:'),AT(350,5),USE(?CentsPrompt1),FONT(,9)
        LIST,AT(396,4,56,10),USE(CentsHow),TIP('Only applies to Carl''s function.'),DROP(9), |
                FROM('xx/100|#0|xx Cents|#1|Worded|#2')
        PROMPT('Zero Cents:'),AT(358,17),USE(?CentsPrompt2),FONT(,9)
        LIST,AT(396,16,56,10),USE(CentsZero),TIP('How to show Zero Cents<13,10>Only applies to Carl''' & |
                's function.'),DROP(9),FROM('Show Cents|#0|Omit Cents|#1|"No Cents"|#2|"Exactly"|#4')
        LIST,AT(6,47),FULL,USE(?List:CheckQ),VSCROLL,FROM(CheckQ),FORMAT('64R(2)|M~Amount~C(0)@n14.2' & |
                '@34L(2)|M~Failed~C(0)@s7@220L(2)|MP~Check Amount Worded  (Shows Fails and Longest) ' & |
                '~L(1)@s255@')
    END
DOO CLASS
CheckQAssign    PROCEDURE(STRING TheAmount) 
    END
    CODE
    SYSTEM{7A58h}=1  !C10 PROP:PropVScroll 
    SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
    OPEN(Window)
    ?List:CheckQ{PROP:LineHeight}=1+?List:CheckQ{PROP:LineHeight}
    ACCEPT
        CASE EVENT()
        OF EVENT:Rejected 
            Message('Your entry is ' & CHOOSE(REJECTCODE(),'TOO HIGH.','TOO LOW.','OUT OF RANGE.','INVALID.') & |
                    '||Entry: ' & ?{PROP:ScreenText},'Rejected')
           DISPLAY(?) ; SELECT(?) ; CYCLE
        OF EVENT:AlertKey
           IF KEYCODE()=EnterKey THEN
              UPDATE ; POST(EVENT:Accepted,?)
           END
        OF EVENT:Accepted
        OROF EVENT:NewSelection

            CASE FIELD()

            !--- Amount in Words -------------------------------------------------------------
            OF  ?CentsHow OROF ?CentsZero ; CentsFormat = CentsHow + CentsZero * 100h
            OF  ?AmountNum  
                 DOO.CheckQAssign(AmountNum) 
                 AmountNum += AmountRngStep
                 ADD(CheckQ,1) 
                 ?List:CheckQ{PROP:Selected} = 1
                 SELECT(?AmountNum)
                 DISPLAY

            OF ?AmountRangeBtn
                FREE(CheckQ)
                MaxSize=0 ; MaxLText = '' ; MaxAmount=0 ; TimeBeg=CLOCK()
                DISABLE(?RangeGroup)
                LOOP AmountRngNdx = AmountRng1 TO AmountRng2 BY AmountRngStep
                     AmountRngNum = AmountRngNdx
                     DOO.CheckQAssign(AmountRngNum)
                     IF ChkQ:Failed THEN ADD(CheckQ).       !If add all success Q gets too big
                     OneLen = len(clip(ChkQ:LText)) 
                     IF maxSize < OneLen THEN
                        IF ~ChkQ:Failed THEN ADD(CheckQ).   !Add the bigger ones
                        maxSize   = OneLen
                        MaxAmount = AmountRngNum
                        MaxLText  = ChkQ:LText
                        MaxChoice = records(CheckQ)
                        ?List:CheckQ{PROP:Selected} = MaxChoice
                        DISPLAY
                     end
                END
                DISPLAY
                ?AmountRangeBtn{PROP:Tip}='Longest: ' & MaxAmount &'<13,10>Words: "' & CLIP(MaxLText) & '"'& |
                                    '<13,10>STRING(' & maxSize & ') in Range ' & AmountRng1 &' to '& AmountRng2
                Message('Range test ' & AmountRng1 &' to '& AmountRng2 &' completed in '& (CLOCK()-TimeBeg)/100 & ' seconds.' & | 
                        '||' & ?AmountRangeBtn{PROP:Tip} ,'Range Test')                                   
                ENABLE(?RangeGroup)
            OF ?AmountRng1
                IF AmountRng2 < AmountRng1 THEN
                   AmountRng2 =AmountRng1 + 100 ; DISPLAY
                END
            OF ?AmountRng2      ; IF AmountRng2 < AmountRng1 THEN AmountRng1=AmountRng2 ; DISPLAY.                
            OF ?AmountRngStep   ; IF AmountRngStep < 0 THEN AmountRngStep=1. ; DISPLAY
            OF ?List:CheckQ                
                IF KEYCODE()=MouseRight AND EVENT()=EVENT:NewSelection THEN 
                   GET(CheckQ,CHOICE(?List:CheckQ))
                   SETKEYCODE(0)
                   EXECUTE POPUP('Copy Words|Copy Amount|-|Copy Both')
                     SETCLIPBOARD('T1: '& CLIP(ChkQ:LText) &'<13,10>T2: '& CLIP(ChkQ:LText_T2) &'<13,10>T3: '& CLIP(ChkQ:LText_T3) )
                     SETCLIPBOARD(ChkQ:Amount)
                     SETCLIPBOARD(ChkQ:Amount &'<13,10>T1: '& CLIP(ChkQ:LText) &'<13,10>T2: '& CLIP(ChkQ:LText_T2) &'<13,10>T3: '& CLIP(ChkQ:LText_T3) )
                   END
                END            
            OF ?AmountEmptyBtn ; FREE(CheckQ) ; DISPLAY
            END !Case Field() of Accepted and New Selection
        END !CASE EVENT
    END
!--------------
DOO.CheckQAssign PROCEDURE(STRING TheAmount) 
    CODE
    ChkQ:Amount   = TheAmount
    ChkQ:LText    = AmountInWords(TheAmount,CentsFormat)
    ChkQ:LTip     = ChkQ:LText 
    ChkQ:LText_T2 = Number2Words_T2(TheAmount)
    ChkQ:LText_T3 = SpellNum_T3(TheAmount)
    IF ChkQ:LText=ChkQ:LText_T2 AND ChkQ:LText=ChkQ:LText_T3 THEN
       ChkQ:Failed=''
    ELSE
       ChkQ:Failed='Fail '& CHOOSE(ChkQ:LText=ChkQ:LText_T2,'','2') & |
                          CHOOSE(ChkQ:LText=ChkQ:LText_T3,'','3')
       Message('Does not match for $' & TheAmount & |
            '||={90}' & |
            '||Carl 1:   ' & CLIP(ChkQ:LText) & |
             '|Test 2:   ' & CLIP(ChkQ:LText_T2) & |
             '|Mark 3: '   & CLIP(ChkQ:LText_T3) & |
            '','Amount Words Fail')
    END 
    RETURN    
!Region Carl's AmountInWords
!======================================================== 
!Cardinal Numbers would be used for an Amount on a Check 
!Note: Limited to under $1 Billion. Could be changed by passing Number as STRING and using DECIMAL(12) instead of LONG
CardinalNumber PROCEDURE(LONG Number)!,STRING 
!or Cardinal...PROCEDURE(CONST *DECIMAL Number) as a Decimal may be better
Cards   STRING('One      Two      Three    Four     Five     Six      Seven    Eight    Nine     ' & |
               'Ten      Eleven   Twelve   Thirteen Fourteen Fifteen  Sixteen  SeventeenEighteen ' & |
               'Nineteen Twenty   Thirty   Forty    Fifty    Sixty    Seventy  Eighty   Ninety   ')
Cardinal  STRING(9),DIM(27),OVER(Cards)
NumString STRING(12),AUTO     !Make this 3 larger than the largest number
Num3Idx   LONG,AUTO
Magnitude LONG,AUTO
Hundreds  LONG,AUTO
Tens      LONG,AUTO
Ones      LONG,AUTO
RetNum    PSTRING(256)
    CODE
    IF Number > 999999999 THEN  !999,999,999  !LONG Max 2.147 Billion
       RETURN( CLIP(LEFT(FORMAT(Number,@n13))) )
    END
    IF Number=0 THEN RETURN('Zero').
    NumString = FORMAT(Number,@n09)
    LOOP Magnitude = 1 TO 3                         !Take number in chunks of Three
         Num3Idx = Magnitude * 3 - 2
         IF NumString[Num3Idx : Num3Idx + 2] > 0
            Hundreds  = NumString[Num3Idx]
            Tens      = NumString[Num3Idx + 1]
            Ones      = NumString[Num3Idx + 2]
            DO ThreeDigitsRtn
            RetNum=RetNum &' '& CHOOSE(Magnitude,'Million','Thousand','')
         END !IF NumString > 0
    END
    IF ~RetNum THEN RetNum='?? ' & Number &' ??'.   !Should NEVER happen, something went very wrong
    RETURN(CLIP(LEFT(RetNum)))

ThreeDigitsRtn  ROUTINE
    IF Hundreds
       RetNum=RetNum &' '& CLIP(Cardinal[Hundreds]) & ' Hundred' !!!CHOOSE(Tens+Ones+MoreLater=0,' Hundredth',' Hundred')
    END
    CASE Tens
    OF 0 ; IF Ones THEN 
              RetNum=RetNum &' '& CLIP(Cardinal[Ones])
           END
    OF 1 ;    RetNum=RetNum &' '& CLIP(Cardinal[10+Ones])
    ELSE                              !20-99
           IF Ones                    !21,22,23..
              RetNum=RetNum &' '& CLIP(Cardinal[18+Tens]) & '-' & |
                                  CLIP(Cardinal[Ones])
           ELSE                       !20,30,40,50
              RetNum=RetNum &' '& CLIP(Cardinal[18+Tens])
           END
    END
    EXIT
!==================================================================================
!12/01/21 Added below to handle Check Amount. 
!         It's just Integer as Cardinal number Worded plus Cents
!         The Cents option should be adapted by you toy what you like
!----------------------------------------------------------------------------------
AmountInWords PROCEDURE(STRING Amount2Word, USHORT CentsFormat=0)!,STRING
DecimalAmt DECIMAL(31,2),AUTO
DollarAmt  DECIMAL(31,0),AUTO
CentsAmt   DECIMAL(3,2),AUTO
Pennies2   STRING(2),AUTO
AndCents   PSTRING(64)            !Longest 24: " and seventy-three Cents"
SayCentz   STRING(5)
Cents:00_100         equate(0)    !and ##/100
Cents:00_Cents       equate(1)    !and ## Cents
Cents:Words          equate(2)    !spell out cents: and Eighty-Five Cents
Cents:IfZero_Omit    equate(100h) !Omit 00/100 or 00 Cents
Cents:IfZero_NoCents equate(200h) !Show "No Cents" if Zero
Cents:IfZero_Exactly equate(400h) !Show "Exactly" if Zero
    CODE
    DecimalAmt=DEFORMAT(Amount2Word)
    IF DecimalAmt=0 THEN RETURN('Zero Dollars and No Cents').
    DollarAmt=INT(DecimalAmt)
    CentsAmt =DecimalAmt-DollarAmt 
    Pennies2 =FORMAT(CentsAmt*100,@n02)
    SayCentz =CHOOSE(CentsAmt=.01,'Cent','Cents')
    CASE BAND(CentsFormat,0FFh)
    OF Cents:Words    ; AndCents=' and '& lower(CardinalNumber(Pennies2)) &' '& SayCentz !E.g. and eighty Cents'
    OF Cents:00_Cents ; AndCents=' and '& Pennies2                        &' '& SayCentz !E.g. and 22 Cents'
    ELSE              ; AndCents=' and '& Pennies2 &'/100'                               !E.g. and 22/100       Default Cents:00_100
    END
    CentsFormat=BAND(CentsFormat,0FF00h)
    IF CentsAmt=0 AND CentsFormat THEN
       CASE BAND(CentsFormat,0FF00h)
       OF Cents:IfZero_Omit    ; AndCents=''
       OF Cents:IfZero_NoCents ; AndCents=' and No Cents'
       OF Cents:IfZero_Exactly ; AndCents=' Exactly'     
      !OF Cents:IfZero_SayZero ; AndCents=' and Zero Cents'  !Possible
       END
    END
    RETURN CardinalNumber(DollarAmt) & AndCents
!EndRegion Carl's AmountInWords

!Region Number2Words from SpellMoney_Unknown.clw
!##############################################################################################
! From SpellMoney_Unknown.clw
!##############################################################################################
!Not sure of the author of this code, it is used in some very old accounting that I maintain.
!Released under the MIT License.

!Money2Words procedure(string Num)!,string    !Calls Number2Words( "Cents")
!    code
!    return Number2Words(Num, 102)     !2= do pennies as " and ## Cents", 100=omit cents

Number2Words_T2 FUNCTION(string InNum, BYTE CentsFormat=1)!,string ! Declare Procedure
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
!Carl No for Test==>      WordAmount = CLIP(WordAmount) & ' Dollars'
   ELSE
      IF DecimalNumber >= 1
!Carl No for Test==>         WordAmount = CLIP(WordAmount) & ' Dollar'
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

!EndRegion Number2Words from SpellMoney_Unknown.clw


!Region SpellNum from SpellNumber_MarkGoldberg.clw
!##############################################################################################
! From SpellNumber_MarkGoldberg.clw

!November 2021 posted in ClarionLive Skype Chat by Mark Goldberg in reply to Ted Stevens
!
!Note: the Prototype places the passed Amount in a REAL which I would change to a STRING or *DECIMAL.
!I would be concerned about the REAL getting a penny wrong, probably not but there is a reason Clarion added DECIMALs.
!The REAL is assinged to a lcl:InputNum DECIMAL(28,2) so no potential for penny errors.

!=====================================================================================
SpellNum_T3 PROCEDURE(REAL pInputNum)!,STRING
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
FormatWithCommas    EQUATE(False)       !Carl Test remove commas to match other 2
  code
   lcl:InputNum   = pInputNum
   !====================================== handle decimal portion
   lcl:UnitsValue = 100 * (lcl:InputNum - int(lcl:InputNum))
   lcl:SpelledNum = ' ' & FORMAT(lcl:UnitsValue,@N02) & '/100' & |  !based on a recent Income Tax return <G>
                                                        '' !Carl Test no ==> ' Dollars'
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
                                          CHOOSE(lcl:UnitsValue % 10=0,' ','-')                  & | !Carl Hyphen when not 20,30,40...
                                          LEFT(lcl:DigitArray    [(lcl:UnitsValue % 10)    + 1])     !Carl Left removes lead space
!Carl code before Without Hyphens         lcl:DigitArray    [(lcl:UnitsValue % 10)    + 1]           !Carl original no hyphen code
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
      if lcl:InputNum AND FormatWithCommas !and lcl:ThousandsGrouping >= ????
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
    