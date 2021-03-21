!----------------------------------------------------------------------------------------------
! Ordinal Number functions take a number and return it worded in "Ordinal" form
!
!   Ordinal Example of 151 is "One Hundred Fifty-First"
!   Compare to Cardinal 151   "One Hundred Fifty-One"
!   Meant for short numbers like Race Meet day "Seventy-Second Racing Day"
!   It has been extended to allow for large numbers (< 1 billion) e,g, 1,100,155=One Million One Hundred Thousand One Hundred Fifty-Fifth
!   It could be easily extended to do Billions, Trillions, ...
!   But wait ... also included CardinalNumber function
!
!   By Carl Barnes - https://github.com/CarlTBarnes
!
!----------------------------------------------------------------------------------------------
    PROGRAM
    include 'EQUATES.CLW'
    include 'KEYCODES.CLW'
    MAP
TestWords       PROCEDURE()
OrdinalNumber   PROCEDURE(LONG InNumber),STRING  !Numbers < 1 billion
OrdinalSmall    PROCEDURE(LONG InNumber),STRING  !Numbers up to 9,999 
CardinalNumber  PROCEDURE(LONG InNumber),STRING  !Numbers < 1 billion
DateOrdinal     PROCEDURE(LONG InDate, BYTE DayOf=0, BYTE YearWorded=0),STRING
DateOrdSmall    PROCEDURE(LONG InDate, BYTE DayOf=0),STRING
    END
    CODE
    TestWords()
!=====================================    
TestWords PROCEDURE()    
SingleNum   LONG(151)
Range1      LONG(1)
Range2      LONG(9999)
RangeStep   LONG(1)
RangeNdx    LONG,AUTO
RangeNum    LONG,AUTO

TestQ   QUEUE,Pre(TesQ)
Num         LONG
SText       STRING(256)     !Small limit 9,999
STip        STRING(256)
LText       STRING(256)     !about 128 bytes cover it e.g. Seven Hundred Seventy-Seven Million Three Hundred Seventy-Three Thousand Three Hundred Seventy-Seventh  (102 bytes)
LTip        STRING(256) 
CText       STRING(256)     !Cardinal                 e.g. Seven Hundred Seventy-Seven Million Three Hundred Seventy-Three Thousand Three Hundred Seventy-Seven
CTip        STRING(256) 
        END
OneLen      LONG
MaxSize     LONG
MaxNumber   LONG
MaxChoice   LONG
MaxLText    STRING(256)
SmallShows  BYTE
SmallWidth  SHORT
SingleDate   LONG
DateInput   BYTE
DateOfDay   BYTE
DateYrWords BYTE

Window WINDOW('Ordinal Number Test'),AT(,,456,170),GRAY,IMM,SYSTEM,MAX,ICON(ICON:Thumbnail), |
            FONT('Segoe UI',10),RESIZE
        PROMPT('&Number to Wordify:'),AT(4,6),USE(?Prompt1)
        SPIN(@n13),AT(4,18,71,10),USE(SingleNum),HVSCROLL,ALRT(EnterKey)
        SPIN(@d02),AT(4,18,71,10),USE(SingleDate),HIDE,HVSCROLL,ALRT(EnterKey)
        PROMPT('&Range:'),AT(100,6,23),USE(?Prompt2),RIGHT
        SPIN(@n12),AT(127,5,63,10),USE(Range1),HVSCROLL
        STRING('To:'),AT(100,19,23),USE(?ToStr),RIGHT
        SPIN(@n12),AT(127,18,63,10),USE(Range2),HVSCROLL
        STRING('By'),AT(201,6),USE(?ByStr)
        ENTRY(@n4),AT(195,18,21,10),USE(RangeStep),CENTER
        BUTTON('&Generate<13,10>Range'),AT(228,5,40,23),USE(?RangeBtn),TIP('Max length found will be' & |
                ' on clipboard<13,10>and highlighted in the list')
        BUTTON('&Empty'),AT(291,11,35,12),USE(?Empty),TIP('Empty the Queue')
        CHECK('Show Small Ordinal'),AT(347,5),USE(SmallShows),TIP('Show Small Ordinal function colum' & |
                'n, limit 9,999')
        CHECK('Date Oridinal'),AT(347,13),USE(DateInput),TIP('Show Date Ordinal')
        CHECK('Day Of'),AT(356,22),USE(DateOfDay),TRN,HIDE,FONT(,9),TIP('Format as "Xxx day of Month"')
        CHECK('Year Worded'),AT(397,22),USE(DateYrWords),TRN,HIDE,FONT(,9),TIP('Year in Words')
        LIST,AT(1,34),FULL,USE(?List:TestQ),VSCROLL,FROM(TestQ),FORMAT('44L(2)|M~Number~C(0)@n12@141' & |
                'L(2)|MP~Small Ordinal  (max 9,999)~L(1)@s255@220L(2)|MP~Oridinal Number~L(1)@s255@2' & |
                '20L(2)P~Cardinal~L(1)@s255@')
    END
DOO CLASS
TestQAssign     PROCEDURE(LONG TheNum)
SmallShowSync   PROCEDURE() 
DateInputSync   PROCEDURE()
InitSample       PROCEDURE()
    END
    CODE
    SYSTEM{7A58h}=1  !C10 PROP:PropVScroll
    SingleDate=TODAY()
    OPEN(Window)
    ?List:TestQ{PROP:LineHeight}=1+?List:TestQ{PROP:LineHeight}
    DOO.SmallShowSync()
    DOO.InitSample()
    ACCEPT
        CASE EVENT()
        OF EVENT:AlertKey
           IF KEYCODE()=EnterKey THEN
              UPDATE ; POST(EVENT:Accepted,?)
           END
        OF EVENT:Accepted
        OROF EVENT:NewSelection

            CASE FIELD()
            OF  ?SingleNum  
                 DOO.TestQAssign(SingleNum) 
                 SingleNum += RangeStep
                 ADD(TestQ,1) 
                 ?List:TestQ{PROP:Selected} = 1
                 SELECT(?SingleNum)
                 DISPLAY

            OF  ?SingleDate
                 IF SingleDate < 4 THEN SingleDate=TODAY().
                 DOO.TestQAssign(SingleDate) 
                 SingleDate += RangeStep
                 ADD(TestQ,1) 
                 ?List:TestQ{PROP:Selected} = 1
                 SELECT(?SingleDate)
                 DISPLAY
                 
            OF ?RangeBtn
                FREE(TestQ)
                MaxSize=0 ; MaxLText = '' ; MaxNumber=0
                LOOP RangeNdx = Range1 TO Range2 BY RangeStep
                     RangeNum=CHOOSE(~DateInput,RangeNdx,RangeNdx+SingleDate)
                     DOO.TestQAssign(RangeNum)
                     ADD(TestQ)
                     OneLen = len(clip(TesQ:LText)) 
                     IF maxSize < OneLen THEN 
                        maxSize   = OneLen
                        MaxNumber = RangeNum
                        MaxLText  = TesQ:LText
                        MaxChoice = records(TestQ)
                        ?List:TestQ{PROP:Selected} = MaxChoice
                        DISPLAY
                     end
                END
                DISPLAY
                ?RangeBtn{PROP:Tip}='Longest ' & MaxNumber &' "' & CLIP(MaxLText) & '"'& |
                                    '<13,10>STRING(' & maxSize & ') in Range ' & Range1 &' to '& Range2
            
            OF ?Range1
                IF Range2 < Range1 THEN
                   Range2 =Range1 + 100 ; DISPLAY
                END
            OF ?RangeStep   ; IF RangeStep < 1 THEN RangeStep=1. ; DISPLAY
            OF ?List:TestQ                
                IF KEYCODE()=MouseRight AND EVENT()=EVENT:NewSelection THEN 
                   GET(TestQ,CHOICE(?List:TestQ))
                   SETKEYCODE(0)
                   EXECUTE POPUP('Copy Oridinal|Copy Cardinal|-|Copy Both')
                     SETCLIPBOARD(TesQ:LText)
                     SETCLIPBOARD(TesQ:CText)
                     SETCLIPBOARD(TesQ:Num &'<13,10>'& CLIP(TesQ:LText) &'<13,10>'&  CLIP(TesQ:CText))
                   END
                END
            OF ?Empty       ; FREE(TestQ) ; DISPLAY
            OF ?SmallShows  ; DOO.SmallShowSync()
            OF ?DateInput   ; DOO.DateInputSync()
            END !Case Field() of Accepted and New Selection
        END !CASE EVENT
    END
!--------------
DOO.TestQAssign PROCEDURE(LONG TheNum) 
    CODE
    IF ~DateInput 
        TesQ:Num   = TheNum
        TesQ:SText = OrdinalSmall(TheNum) 
        TesQ:LText = OrdinalNumber(TheNum)
        TesQ:CText = CardinalNumber(TheNum)
    ELSE 
        TesQ:Num   = TheNum
        TesQ:SText = DateOrdSmall(TheNum,DateOfDay) 
        TesQ:LText = DateOrdinal(TheNum,DateOfDay, DateYrWords)
        TesQ:CText = FORMAT(TheNum,@d02) &' - '& FORMAT(TheNum,@d4)
    END 
    TesQ:STip=TesQ:SText
    TesQ:LTip=TesQ:LText 
    TesQ:CTip=TesQ:CText 
    RETURN
!--------------
DOO.SmallShowSync PROCEDURE()
NewWidth SHORT
    CODE
    IF ~SmallShows THEN 
        SmallWidth=?List:TestQ{PROPLIST:width,2}        
    ELSE 
        NewWidth=SmallWidth
    END
    ?List:TestQ{PROPLIST:width,2}=NewWidth
    RETURN 
!----------------  
DOO.DateInputSync PROCEDURE()
    CODE
    IF DateInput THEN
       HIDE(?SingleNum)
       UNHIDE(?SingleDate) ; SELECT(?SingleDate)
       UNHIDE(?DateOfDay,?DateYrWords)
    ELSE
       HIDE(?SingleDate)
       UNHIDE(?SingleNum) ; SELECT(?SingleNum)
       HIDE(?DateOfDay,?DateYrWords)    
    END
    RETURN
!----------------
DOO.InitSample PROCEDURE()
Unit PSTRING(' Second')
Ndx  LONG
NumT LONG
    CODE
    LOOP Ndx=1 TO 9  
        NumT=CHOOSE(Ndx,5,15,50,55,100,150,151,0)
        IF ~NumT THEN BREAK.
        DOO.TestQAssign(NumT) ; ADD(TestQ)
    END
    NumT=TODAY()
    DateInput=1 ; DOO.TestQAssign(NumT) ; ADD(TestQ)
    DateYrWords=1 ; DOO.TestQAssign(NumT) ; ADD(TestQ) ; DateYrWords=0
    DateOfDay=1 ; DOO.TestQAssign(NumT) ; ADD(TestQ)
    DateYrWords=1 ; DOO.TestQAssign(NumT) ; ADD(TestQ)
    DateInput=0 ; DateOfDay=0; DateYrWords=0    
    NumT = (CLOCK()-1)/100     
    LOOP 3 TIMES 
        DOO.TestQAssign(NumT)
        TesQ:SText=CLIP(TesQ:SText) & Unit&' since Midnight'  ; TesQ:STip=FORMAT(CLOCK(),@T6 )
        TesQ:LText=CLIP(TesQ:LText) & Unit&' since Midnight'  ; TesQ:LTip=TesQ:LText
        TesQ:CText=CLIP(TesQ:CText) & Unit&'s since Midnight' ; TesQ:CTip=TesQ:CText
        ADD(TestQ)
        NumT /= 60 
        Unit=CHOOSE(Unit[2]='S',' Minute',' Hour')
    END
    NumT=TODAY()
    Unit=' Day'
    LOOP 3 TIMES 
       DOO.TestQAssign(NumT)
       TesQ:SText=CLIP(TesQ:SText) & Unit&' since 12-28-1800'  ; TesQ:STip=FORMAT(TODAY(),@d4)
       TesQ:LText=CLIP(TesQ:LText) & Unit&' since 12-28-1800'  ; TesQ:LTip=TesQ:LText
       TesQ:CText=CLIP(TesQ:CText) & Unit&'s since 12-28-1800' ; TesQ:CTip=TesQ:CText
       ADD(TestQ)
       IF Unit[2]='D'
          Unit=' Month' ; NumT=TODAY()/30.4
       ELSE 
          Unit=' Year' ; NumT=YEAR(TODAY())-1800 
       END
    END
    RETURN
!========================================================
OrdinalNumber PROCEDURE(LONG Number)!,STRING

Ords    STRING('First      Second     Third      Fourth     Fifth      Sixth      Seventh    ' & |
               'Eighth     Ninth      Tenth      Eleventh   Twelfth    Thirteenth Fourteenth ' & |
               'Fifteenth  Sixteenth  SeventeenthEighteenth Nineteenth Twentieth  Thirtieth  ' & |
               'Fortieth   Fiftieth   Sixtieth   Seventieth Eightieth  Ninetieth  ')
Ordinal STRING(11),DIM(27),OVER(Ords)

Cards   STRING('One      Two      Three    Four     Five     Six      Seven    Eight    Nine     ' & |
               'Ten      Eleven   Twelve   Thirteen Fourteen Fifteen  Sixteen  SeventeenEighteen ' & |
               'Nineteen Twenty   Thirty   Forty    Fifty    Sixty    Seventy  Eighty   Ninety   ')
Cardinal   STRING(9),DIM(27),OVER(Cards)

NumString   STRING(12),AUTO     !Make this 3 larger than the largest number
Num3Idx     LONG,AUTO
Magnitude   LONG,AUTO
Hundreds    LONG,AUTO
Tens        LONG,AUTO
Ones        LONG,AUTO
Remainder   LONG,AUTO
MoreLater   LONG,AUTO           !If there is a Remainder or we are > 999 there is more coming so no Ordinals
RetNum      PSTRING(256)
    CODE
    IF Number > 999999999 THEN
       RETURN( CLIP(LEFT(FORMAT(Number,@n13))) & CHOOSE(Number % 10,'st','nd','rd','th') )
    END

    NumString = FORMAT(Number,@n09)
    LOOP Magnitude = 1 TO 3                         !Take number in chunks of Three
         Num3Idx = Magnitude * 3 - 2
         IF NumString[Num3Idx : Num3Idx + 2] > 0
            Hundreds  = NumString[Num3Idx]
            Tens      = NumString[Num3Idx + 1]
            Ones      = NumString[Num3Idx + 2]
            Remainder = NumString[Num3Idx + 3 : SIZE(NumString)]
            MoreLater = CHOOSE(Remainder>0 OR Magnitude<3,1,0)
            DO ThreeDigitsRtn
            IF Remainder = 0                                   !Remainder zero, time for Ordinal suffix
               RetNum=RetNum &' '& CHOOSE(Magnitude,'Millionth','Thousandth','')
            ELSE                                               !If more to do then Cardinal
               RetNum=RetNum &' '& CHOOSE(Magnitude,'Million','Thousand','')
            END
         END !IF NumString > 0
    END
    IF ~RetNum THEN RETURN('Zeroth').
    RETURN(CLIP(LEFT(RetNum)))

ThreeDigitsRtn  ROUTINE
    IF Hundreds
       RetNum=RetNum &' '& CLIP(Cardinal[Hundreds]) & CHOOSE(Tens+Ones+MoreLater=0,' Hundredth',' Hundred')
    END
    CASE Tens
    OF 0 ; IF Ones THEN |
              RetNum=RetNum &' '& CLIP(CHOOSE(~MoreLater,Ordinal[Ones]   ,Cardinal[Ones]   )).
    OF 1 ;    RetNum=RetNum &' '& CLIP(CHOOSE(~MoreLater,Ordinal[10+Ones],Cardinal[10+Ones]))
    ELSE                                                                !20-99
           IF Ones                                                      !21,22,23..
              RetNum=RetNum &' '& CLIP(Cardinal[18+Tens]) & '-' & |
                                  CLIP(CHOOSE(~MoreLater,Ordinal[Ones]   ,Cardinal[Ones]))
           ELSE                                                        !20,30,40,50
              RetNum=RetNum &' '& CLIP(CHOOSE(~MoreLater,Ordinal[18+Tens],Cardinal[18+Tens]))
           END
    END
    EXIT
!========================================================
!----------------------------------------------------------------------------------------------
!--SMALL this version can be much simpler since it only allows up to 9999
!   It also does Teens for 1100 to 1999, e.g.  Thriteen Hundredth

OrdinalSmall PROCEDURE(LONG Number)!,STRING   !Limit of 9999

Ords    STRING('First      Second     Third      Fourth     Fifth      Sixth      Seventh    ' & |
               'Eighth     Ninth      Tenth      Eleventh   Twelfth    Thirteenth Fourteenth ' & |
               'Fifteenth  Sixteenth  SeventeenthEighteenth Nineteenth Twentieth  Thirtieth  ' & |
               'Fortieth   Fiftieth   Sixtieth   Seventieth Eightieth  Ninetieth  ')
Ordinal STRING(11),DIM(27),OVER(Ords)

Cards   STRING('One      Two      Three    Four     Five     Six      Seven    Eight    Nine     ' & |
               'Ten      Eleven   Twelve   Thirteen Fourteen Fifteen  Sixteen  SeventeenEighteen ' & |
               'Nineteen Twenty   Thirty   Forty    Fifty    Sixty    Seventy  Eighty   Ninety   ')
Cardinal   STRING(9),DIM(27),OVER(Cards)

NumString   STRING(4),AUTO     
Thousands   LONG,AUTO
Hundreds    LONG,AUTO
Tens        LONG,AUTO
Ones        LONG,AUTO
RetNum      PSTRING(256)
    CODE
    IF Number > 9999 THEN
       RETURN( CLIP(LEFT(FORMAT(Number,@n13))) & CHOOSE(Number % 10,'st','nd','rd','th') )
    END

    NumString = FORMAT(Number,@n04)
    Hundreds  = NumString[1:2]
    Tens      = NumString[3]  
    Ones      = NumString[4]  
    IF Hundreds = 10 OR Hundreds > 19
       Thousands = Hundreds / 10
       Hundreds %= 10
       RetNum=RetNum &' '& CLIP(Cardinal[Thousands]) & CHOOSE(Hundreds+Tens+Ones=0,' Thousandth',' Thousand')
    END
    IF Hundreds
       RetNum=RetNum &' '& CLIP(Cardinal[Hundreds]) & CHOOSE(Tens+Ones=0,' Hundredth',' Hundred')
    END
    CASE Tens
    OF 0          
        IF Ones THEN RetNum=RetNum &' '& CLIP(Ordinal[Ones]).
    OF 1          
        RetNum=RetNum &' '& CLIP(Ordinal[10+Ones])    !Teens?
    ELSE
        IF Ones
           RetNum=RetNum &' '& CLIP(Cardinal[18+Tens]) & '-' & CLIP(Ordinal[Ones])
        ELSE
           RetNum=RetNum &' '& CLIP(Ordinal[18+Tens])
        END
    END
    IF ~RetNum THEN RetNum='Zeroth'.
    RETURN(LEFT(RetNum))

!======================================================== 
!Cardinal Numbers would be used for an Amount on a Check
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
    IF ~RetNum THEN RETURN('Zero').
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
!========================================================
DateOrdinal PROCEDURE(LONG InDate, BYTE DayOf=0, BYTE pYearWorded=0)!,STRING
D4      STRING(40),AUTO
Comma   BYTE,AUTO
RetDate PSTRING(84),AUTO
DayOrd  PSTRING(16),AUTO     !Twenty-Seventh
MoName  PSTRING(16),AUTO
YYYY    PSTRING(42),AUTO     !Three Thousand Three Hundred Seventy-Seven
    CODE                     !  -4321012
    D4=FORMAT(InDate,@d04)    !Mmmm DD, YYYY
    Comma=INSTRING(',',D4)
    IF ~Comma OR InDate<4 THEN RETURN CLIP(D4). !Only if invalid Date 
    DayOrd=OrdinalNumber(SUB(D4,Comma-2,2))
    MoName=SUB(D4,1,Comma-4)
    YYYY=SUB(D4,Comma+2,4)
    IF pYearWorded THEN YYYY=CardinalNumber(YYYY).
    IF ~DayOf
       RetDate=MoName &' '& DayOrd &', '& YYYY
    ELSE 
       RetDate=DayOrd &' day of '& MoName &', '& YYYY
    END
    RETURN RetDate

DateOrdSmall PROCEDURE(LONG InDate, BYTE DayOf=0)!,STRING
D4      STRING(40),AUTO
Comma   BYTE,AUTO
RetDate PSTRING(84),AUTO
DayNum  BYTE,AUTO
DayOrd  PSTRING(5),AUTO     ! 31st
MoName  PSTRING(16),AUTO   
    CODE                     !   -4321012
    D4=FORMAT(InDate,@d04)    !Mmmm DD, YYYY
    Comma=INSTRING(',',D4)
    IF ~Comma THEN RETURN CLIP(D4). !Only if invalid Date 
    DayNum=SUB(D4,Comma-2,2)
    DayOrd=DayNum & CHOOSE(DayNum % 10,'st','nd','rd','th')
    MoName=SUB(D4,1,Comma-4)
    IF ~DayOf
       RetDate=MoName &' '& DayOrd & SUB(D4,Comma,6)
    ELSE 
       RetDate=DayOrd &' day of '& MoName  & SUB(D4,Comma,6)
    END
    RETURN RetDate
