'+---------------+---------------------------------------------------+
'| ###### ###### |     .--. .         .-.                            |
'| ##  ## ##   # |     |   )|        (   ) o                         |
'| ##  ##  ##    |     |--' |--. .-.  `-.  .  .-...--.--. .-.        |
'| ######   ##   |     |  \ |  |(   )(   ) | (   ||  |  |(   )       |
'| ##      ##    |     '   `'  `-`-'  `-'-' `-`-`|'  '  `-`-'`-      |
'| ##     ##   # |                            ._.'                   |
'| ##     ###### |  Sources & Documents placed in the Public Domain. |
'+---------------+---------------------------------------------------+
'|                                                                   |
'| === lzwpacker.bm ===                                              |
'|                                                                   |
'| == A small library providing adaptive (9-12 bits) LZW packing and |
'| == unpacking for QB64. Packing and unpacking is done from string  |
'| == to string, so it's easy to use for short texts as well as for  |
'| == file packing, if the whole file is loaded into a single string.|
'|                                                                   |
'| == The functions should run without problems for input data sizes |
'| == upto approx. 100MB. Bigger sizes may cause an "Out of memory"  |
'| == error especially for LzwUnpack$(), as here in the worst case   |
'| == temporarily 5 times the input size may be required for buffer  |
'| == swapping. If you really handle such big sizes in your project, |
'| == then you should cut it into smaller chunks and run each chunk  |
'| == separatly through compression/decompression.                   |
'|                                                                   |
'+-------------------------------------------------------------------+
'| Done by RhoSigma, R.Heyder, provided AS IS, use at your own risk. |
'| Find me in the QB64 Forum or mail to support@rhosigma-cw.net for  |
'| any questions or suggestions. Thanx for your interest in my work. |
'+-------------------------------------------------------------------+

'=====================================================================
' Run source data through LZW compression and return the packed data.
'---------------------------------------------------------------------
' Syntax: packed$ = LzwPack$(SourceData$, MinRatio%)
'
' Result: packed$ --> The LZW compressed data or empty string, if
'                     compression ratio does not reach the desired
'                     result (minimum ratio).
'
' Inputs: SourceData$ --> The data you want to compress, may contain
'                         control chars incl. zero bytes. If the data
'                         size is less than 25 bytes, then no packing
'                         is done at all, hence result will be empty.
'                         Such small sizes wouldn't compress anyways.
'         MinRatio%   --> The minimum compression ratio, ie. how many
'                         percent the source data should be reduced
'                         at least, to consider it a successful run.
'                         Can be zero, then just one byte less than
'                         source length is considered a success.
'                         However, regular values should be 1-50%.
'
' Notes: This function does produce raw LZW output, it doesn't use CRCs
'        and does not implement any kind of identifier for its counter-
'        part function LzwUnpack$() to verify if it's called with valid
'        compressed data. If required you've to implement appropriate
'        checks on the parent program level.
'=====================================================================
FUNCTION LzwPack$ (SourceData$, MinRatio%)
    '--- so far, return failure ---
    LzwPack$ = ""

    '--- get/check source ---
    SourceLen& = LEN(SourceData$)
    IF SourceLen& < 25 THEN EXIT FUNCTION
    SourcePos& = 1
    '--- set/check target ---
    IF MinRatio% < 0 THEN MinRatio% = 0
    IF MinRatio% > 99 THEN MinRatio% = 99
    TargetBuf$ = SPACE$((SourceLen& * ((100 - MinRatio%) / 100)) - 1)
    TargetLen& = LEN(TargetBuf$)
    IF TargetLen& = 0 THEN EXIT FUNCTION
    TargetPos& = 1

    '--- init LZW ---
    REDIM PrefArr%(6576), SuffArr%(6576), CodeArr%(6576), UsedArr&(4096)

    gate& = 0 'bitshift gateway to target
    gateBit% = 0 'current bit position in gate&
    codeSize% = 9 'start with 9 bits code size
    maxCode% = 512 'maximum code that fits in 9 bits

    'Compression codes:
    'Code 256 = end of file
    'Code 257 = increase code size
    'Code 258 = rebuild table
    'Code 259 - 4095 = available for strings
    firstCode% = 259 'first LZW code available
    nextCode% = 259

    '--- clear Hash table & get first byte ---
    GOSUB ClearTable
    prefix% = ASC(SourceData$, SourcePos&)
    SourcePos& = SourcePos& + 1

    '--- main compression loop ---
    DO
        DO
            IF SourcePos& > SourceLen& THEN 'done?
                pcCode% = prefix%: GOSUB PutCode 'put final byte to output
                pcCode% = 256: GOSUB PutCode 'send "end of file" code to decompressor
                pcCode% = 0: GOSUB PutCode: GOSUB PutCode 'and flush entire gateway
                ERASE PrefArr%, SuffArr%, CodeArr%, UsedArr&
                LzwPack$ = LEFT$(TargetBuf$, TargetPos& - 1)
                EXIT FUNCTION
            ELSE
                suffix% = ASC(SourceData$, SourcePos&)
                SourcePos& = SourcePos& + 1
                '--- search prefix:suffix on Hash table ---
                stPre% = prefix%: stSuf% = suffix%
                GOSUB SearchTable
                index% = stIdx%: found% = stFlg%
                '--- update values, if found ---
                IF found% THEN
                    prefix% = CodeArr%(index%)
                    UsedArr&(prefix%) = UsedArr&(prefix%) + 1
                END IF
            END IF
        LOOP WHILE found%

        '--- for following output, increase code size if required ---
        WHILE prefix% >= maxCode% AND codeSize% < 12
            pcCode% = 257: GOSUB PutCode 'send "increase code size" code to decompressor
            maxCode% = maxCode% * 2
            codeSize% = codeSize% + 1
        WEND

        '--- if no Hash entry was found above, then put prefix byte to output ---
        pcCode% = prefix%: GOSUB PutCode
        '--- then put the new string on Hash table ---
        PrefArr%(index%) = prefix%
        SuffArr%(index%) = suffix%
        CodeArr%(index%) = nextCode%
        '--- and finally set prefix to the byte that caused the search failure ---
        prefix% = suffix%

        '--- if there are too many strings then rebuild the Hash table ---
        nextCode% = nextCode% + 1
        IF nextCode% > 4096 THEN
            pcCode% = 258: GOSUB PutCode 'send "rebuild table" code to decompressor
            '-----
            GOSUB RebuildTable: newEntries% = rtNewEnt%
            nextCode% = newEntries% + firstCode%
            '-----
            IF nextCode% > 4096 THEN
                GOSUB ClearTable
                nextCode% = firstCode% 'reset next code to top of tree
            END IF
            '--- reset gateway controls ---
            codeSize% = 9
            maxCode% = 512
        END IF
    LOOP
    '--- we should never reach this point ---
    ERROR 97
    STOP

    '---------------
    '--- PutCode ---
    '---------------
    '--- This subroutine throws one multi-bit code to the output. It will
    '--- also check if the target length is exceeded according to the desired
    '--- minimum compression ratio and, if so, it will abort compression.
    '-----
    '--- Inputs: - Place code into pcCode% before the GOSUB call.
    '---------------------------------------------------------------------
    PutCode:
    gate& = gate& + (pcCode% * (2 ^ gateBit%))
    gateBit% = gateBit% + codeSize%
    WHILE gateBit% > 7
        MID$(TargetBuf$, TargetPos&) = CHR$(gate& AND 255)
        TargetPos& = TargetPos& + 1
        IF TargetPos& > TargetLen& THEN
            ERASE PrefArr%, SuffArr%, CodeArr%, UsedArr&
            EXIT FUNCTION
        END IF
        gate& = gate& \ 256
        gateBit% = gateBit% - 8
    WEND
    RETURN

    '------------------
    '--- ClearTable ---
    '------------------
    '--- This subroutine will clear the Hash table.
    '---------------------------------------------------------------------
    ClearTable:
    FOR ctI% = 0 TO 6576
        PrefArr%(ctI%) = -1
        SuffArr%(ctI%) = -1
        CodeArr%(ctI%) = -1
    NEXT ctI%
    RETURN

    '-------------------
    '--- SearchTable ---
    '-------------------
    '--- This subroutine attempts to find the given prefix:suffix string
    '--- on the Hash table.
    '-----
    '--- Inputs:  - Place prefix and suffix into stPre% and stSuf% respectivly
    '---            before the GOSUB call.
    '--- Results: - The stFlg% boolean flags, if an entry was found or not,
    '---            if true, then stIdx% holds the found Hash index.
    '---------------------------------------------------------------------
    SearchTable:
    stIdx% = ((stPre% * 256) XOR stSuf%) MOD 6577 'XOR hashing
    IF stIdx% = 0 THEN 'is index lucky enough to be 0?
        stOff% = 1 'Set offset to 1, because 6577-0=6577
    ELSE
        stOff% = 6577 - stIdx%
    END IF
    DO 'until we find a match or don't
        IF CodeArr%(stIdx%) = -1 THEN 'is there nothing here?
            stFlg% = 0 'yup, not found
            RETURN
        ELSEIF PrefArr%(stIdx%) = stPre% AND SuffArr%(stIdx%) = stSuf% THEN 'is this what we're looking for?
            stFlg% = -1 'yup, found
            RETURN
        ELSE 'retry until we find what we're looking for or we find a blank entry
            stIdx% = stIdx% - stOff%
            IF stIdx% < 0 THEN 'is index too far down?
                stIdx% = stIdx% + 6577 'yup, bring it up then
            END IF
        END IF
    LOOP
    RETURN 'well, another point we shouln't reach

    '--------------------
    '--- RebuildTable ---
    '--------------------
    '--- This subroutine eliminates any strings which are not used in the
    '--- Hash table. The usual result of doing this is greater compression.
    '-----
    '--- Results: - rtNewEnt% holds the new number of entries on Hash table.
    '---------------------------------------------------------------------
    RebuildTable:
    REDIM rtPreArr%(4096), rtSufArr%(4096), rtUseArr&(4096)
    REDIM rtPnArr%(4096), rtCodArr%(4096), rtLocArr%(4096)

    rtNumEnt% = 0

    '--- build a temporary Hash table ---
    FOR rtI% = 0 TO 6576
        rtCod% = CodeArr%(rtI%)
        IF rtCod% <> -1 THEN 'valid code?
            IF UsedArr&(rtCod%) > 0 THEN 'was it used at all?
                UsedArr&(rtCod%) = 0
                rtPre% = PrefArr%(rtI%)
                rtSuf% = SuffArr%(rtI%)
                rtPreArr%(rtNumEnt%) = rtPre% 'put it on temporary table
                rtSufArr%(rtNumEnt%) = rtSuf%
                rtUseArr&(rtNumEnt%) = rtPre% * 4096 + rtSuf%
                rtCodArr%(rtCod%) = rtNumEnt%
                rtNumEnt% = rtNumEnt% + 1
            END IF
        END IF
    NEXT rtI%

    '--- init help indices ---
    rtNumEnt% = rtNumEnt% - 1
    FOR rtI% = 0 TO rtNumEnt%
        rtPnArr%(rtI%) = rtI%
    NEXT rtI%
    '--- sort the table according to it's prefix:suffix ---
    rtMid% = rtNumEnt% \ 2
    DO
        FOR rtI% = 0 TO rtNumEnt% - rtMid%
            IF rtUseArr&(rtPnArr%(rtI%)) > rtUseArr&(rtPnArr%(rtI% + rtMid%)) THEN
                SWAP rtPnArr%(rtI%), rtPnArr%(rtI% + rtMid%)
                rtCmpLO% = rtI% - rtMid%
                rtCmpHI% = rtI%
                WHILE rtCmpLO% >= 0
                    IF rtUseArr&(rtPnArr%(rtCmpLO%)) > rtUseArr&(rtPnArr%(rtCmpHI%)) THEN
                        SWAP rtPnArr%(rtCmpLO%), rtPnArr%(rtCmpHI%)
                        rtCmpHI% = rtCmpLO%
                        rtCmpLO% = rtCmpLO% - rtMid%
                    ELSE
                        EXIT WHILE
                    END IF
                WEND
            END IF
        NEXT rtI%
        rtMid% = rtMid% \ 2
    LOOP WHILE rtMid% > 0
    '--- get new locations ---
    FOR rtI% = 0 TO rtNumEnt%
        rtLocArr%(rtPnArr%(rtI%)) = rtI%
    NEXT

    GOSUB ClearTable
    '--- put each prefix:suffix back into the main Hash table ---
    FOR rtJ% = 0 TO rtNumEnt%
        rtI% = rtPnArr%(rtJ%)
        '-----
        rtPre% = rtPreArr%(rtI%)
        rtSuf% = rtSufArr%(rtI%)
        IF rtPre% >= firstCode% THEN 'is it pointing twards a string?
            rtPre% = firstCode% + rtLocArr%(rtCodArr%(rtPre%)) 'yup, update the pointer
        END IF
        IF rtSuf% >= firstCode% THEN
            rtSuf% = firstCode% + rtLocArr%(rtCodArr%(rtSuf%))
        END IF
        '--- where does this prefix:suffix go? ---
        stPre% = rtPre%: stSuf% = rtSuf%: GOSUB SearchTable: rtIdx% = stIdx%
        '-----
        PrefArr%(rtIdx%) = rtPre% 'put it there
        SuffArr%(rtIdx%) = rtSuf%
        CodeArr%(rtIdx%) = rtJ% + firstCode%
    NEXT rtJ%

    rtNewEnt% = rtNumEnt% + 1 'number of entries on the Hash table now

    ERASE rtPreArr%, rtSufArr%, rtUseArr&
    ERASE rtPnArr%, rtCodArr%, rtLocArr%
    RETURN
END FUNCTION

'=====================================================================
' Decompress the given LZW packed data back to its original data format.
'---------------------------------------------------------------------
' Syntax: unpacked$ = LzwUnpack$(LzwData$)
'
' Result: unpacked$ --> The original (uncompressed) data or empty string,
'                       if decompression did error out for some reason.
'
' Inputs: LzwData$  --> The LZW compressed data you want to decompress,
'                       usually the result of the LzwPack$() function.
'
' Notes: This function can process raw LZW input only, as produced by
'        its counterpart function LzwPack$(). Any checks, whether data
'        is compressed or not, must be implemented on the parent program
'        level, if required.
'=====================================================================
FUNCTION LzwUnpack$ (LzwData$)
    '--- so far, return failure ---
    LzwUnpack$ = ""

    '--- get/check source ---
    LzwLen& = LEN(LzwData$)
    IF LzwLen& = 0 THEN EXIT FUNCTION
    LzwPos& = 1
    '--- set/check target ---
    TargetBuf$ = SPACE$(LzwLen& * 2)
    TargetLen& = LEN(TargetBuf$)
    IF TargetLen& = 0 THEN EXIT FUNCTION
    TargetPos& = 1

    '--- init LZW ---
    REDIM PrefArr%(4096), SuffArr%(4096), UsedArr&(4096)

    gate& = 0 'bitshift gateway from input
    gateBits% = 0 'available data bits in gate&
    codeSize% = 9 'start with 9 bits code size

    REDIM stack%(4096) 'reverse output stack
    stackCnt% = 0 'current stack index

    'Compression codes:
    'Code 256 = end of file
    'Code 257 = increase code size
    'Code 258 = rebuild table
    'Code 259 - 4095 = available for strings
    firstCode% = 259 'first LZW code available
    nextCode% = 259

    '--- get first code ---
    GOSUB GetCode: code% = gcCode%
    curCode% = code%
    oldCode% = code%
    finChar% = code%
    pbByte% = finChar%: GOSUB PutByte

    '--- main decompression loop ---
    DO
        '--- get next code from input ---
        GOSUB GetCode: code% = gcCode%
        '--- process codes ---
        SELECT CASE code%
            CASE 256 'code "end of file"
                LzwUnpack$ = LEFT$(TargetBuf$, TargetPos& - 1)
                ERASE PrefArr%, SuffArr%, UsedArr&, stack%
                EXIT FUNCTION
            CASE 257 'code "increase code size"
                codeSize% = codeSize% + 1
            CASE 258 'code "rebuild table"
                GOSUB RebuildTable: newEntries% = rtNewEnt%
                nextCode% = newEntries% + firstCode%
                codeSize% = 9 'reset code size
                '-----
                IF nextCode% > 4096 THEN
                    nextCode% = firstCode%
                    '-----
                    GOSUB GetCode: code% = gcCode%
                    '-----
                    curCode% = code%
                    oldCode% = code%
                    finChar% = code%
                    pbByte% = finChar%: GOSUB PutByte
                ELSE
                    ignore% = -1 'prevent invalid codes entering the table
                END IF
            CASE ELSE 'string code
                curCode% = code%
                inCode% = code%
                '--- do we have this string yet? ---
                IF code% >= nextCode% THEN
                    '--- if code%>nextCode% then stop decompression, ---
                    '--- because this can't be right! ---
                    IF code% > nextCode% THEN
                        ERASE PrefArr%, SuffArr%, UsedArr&, stack%
                        EXIT FUNCTION
                    END IF
                    '--- trick decompressor to use last code ---
                    UsedArr&(code%) = UsedArr&(code%) + 1
                    curCode% = oldCode%
                    stack%(stackCnt%) = finChar%
                    stackCnt% = stackCnt% + 1
                END IF
                '--- does this code represent a string? ---
                IF curCode% >= firstCode% THEN
                    '--- get each byte from the table and push it onto the stack, ---
                    '--- keep on doing this until we have a normal ASCII (0-255)  ---
                    DO
                        UsedArr&(curCode%) = UsedArr&(curCode%) + 1
                        stack%(stackCnt%) = SuffArr%(curCode%)
                        stackCnt% = stackCnt% + 1
                        curCode% = PrefArr%(curCode%)
                    LOOP UNTIL curCode% <= 255
                END IF
                finChar% = curCode%
                stack%(stackCnt%) = finChar%
                '--- pop all the codes of the stack and put them to output ---
                FOR i% = stackCnt% TO 0 STEP -1
                    pbByte% = stack%(i%): GOSUB PutByte
                NEXT i%
                stackCnt% = 0
                '--- put the new string on the table ---
                IF ignore% THEN
                    ignore% = 0
                ELSE
                    PrefArr%(nextCode%) = oldCode%
                    SuffArr%(nextCode%) = finChar%
                    nextCode% = nextCode% + 1
                END IF
                oldCode% = inCode%
        END SELECT
    LOOP
    '--- we should never reach this point ---
    ERROR 97
    STOP

    '---------------
    '--- PutByte ---
    '---------------
    '--- This subroutine throws one byte to the output. It will also check
    '--- if the target buffer must be expanded.
    '-----
    '--- Inputs: - Place byte into pbByte% before the GOSUB call.
    '---------------------------------------------------------------------
    PutByte:
    MID$(TargetBuf$, TargetPos&) = CHR$(pbByte%)
    TargetPos& = TargetPos& + 1
    IF TargetPos& > TargetLen& THEN
        pbTemp$ = TargetBuf$
        TargetBuf$ = SPACE$(TargetLen& * 1.25)
        TargetLen& = LEN(TargetBuf$)
        MID$(TargetBuf$, 1) = pbTemp$
        pbTemp$ = "" 'important, free temp storage
    END IF
    RETURN

    '---------------
    '--- GetCode ---
    '---------------
    '--- This subroutine reads one multi-bit code from the input.
    '-----
    '--- Results: - gcCode% holds the next valid code.
    '---------------------------------------------------------------------
    GetCode:
    IF gateBits% = 0 THEN
        gcTemp% = ASC(LzwData$, LzwPos&)
        LzwPos& = LzwPos& + 1
        gateBits% = 8
    END IF
    gate& = gcTemp% \ (2 ^ (8 - gateBits%))
    WHILE codeSize% > gateBits%
        gcTemp% = ASC(LzwData$, LzwPos&)
        LzwPos& = LzwPos& + 1
        gate& = gate& OR (gcTemp% * (2 ^ gateBits%))
        gateBits% = gateBits% + 8
    WEND
    gateBits% = gateBits% - codeSize%
    gcCode% = gate& AND ((2 ^ codeSize%) - 1)
    RETURN

    '--------------------
    '--- RebuildTable ---
    '--------------------
    '--- This subroutine eliminates any strings which are not used in the
    '--- Hash table. The usual result of doing this is greater compression.
    '-----
    '--- Results: - rtNewEnt% holds the new number of entries on Hash table.
    '---------------------------------------------------------------------
    RebuildTable:
    REDIM rtPreArr%(4095), rtSufArr%(4095), rtUseArr&(4095)
    REDIM rtPnArr%(4095), rtCodArr%(4095), rtLocArr%(4095)

    rtNumEnt% = 0

    '--- build a temporary Hash table ---
    FOR rtI% = firstCode% TO 4095
        IF UsedArr&(rtI%) > 0 THEN 'code used at all?
            UsedArr&(rtI%) = 0
            rtPre% = PrefArr%(rtI%)
            rtSuf% = SuffArr%(rtI%)
            rtPreArr%(rtNumEnt%) = rtPre% 'put it on temporary table
            rtSufArr%(rtNumEnt%) = rtSuf%
            rtUseArr&(rtNumEnt%) = rtPre% * 4096 + rtSuf%
            rtCodArr%(rtI%) = rtNumEnt%
            rtNumEnt% = rtNumEnt% + 1
        END IF
    NEXT rtI%

    '--- init help indices ---
    rtNumEnt% = rtNumEnt% - 1
    FOR rtI% = 0 TO rtNumEnt%
        rtPnArr%(rtI%) = rtI%
    NEXT rtI%
    '--- sort the table according to it's prefix:suffix ---
    rtMid% = rtNumEnt% \ 2
    DO
        FOR rtI% = 0 TO rtNumEnt% - rtMid%
            IF rtUseArr&(rtPnArr%(rtI%)) > rtUseArr&(rtPnArr%(rtI% + rtMid%)) THEN
                SWAP rtPnArr%(rtI%), rtPnArr%(rtI% + rtMid%)
                rtCmpLO% = rtI% - rtMid%
                rtCmpHI% = rtI%
                WHILE rtCmpLO% >= 0
                    IF rtUseArr&(rtPnArr%(rtCmpLO%)) > rtUseArr&(rtPnArr%(rtCmpHI%)) THEN
                        SWAP rtPnArr%(rtCmpLO%), rtPnArr%(rtCmpHI%)
                        rtCmpHI% = rtCmpLO%
                        rtCmpLO% = rtCmpLO% - rtMid%
                    ELSE
                        EXIT WHILE
                    END IF
                WEND
            END IF
        NEXT rtI%
        rtMid% = rtMid% \ 2
    LOOP WHILE rtMid% > 0
    '--- get new locations ---
    FOR rtI% = 0 TO rtNumEnt%
        rtLocArr%(rtPnArr%(rtI%)) = rtI%
    NEXT

    '--- put each prefix:suffix back into the main Hash table ---
    FOR rtJ% = 0 TO rtNumEnt%
        rtI% = rtPnArr%(rtJ%)
        '-----
        rtPre% = rtPreArr%(rtI%)
        rtSuf% = rtSufArr%(rtI%)
        IF rtPre% >= firstCode% THEN 'is it pointing twards a string?
            rtPre% = firstCode% + rtLocArr%(rtCodArr%(rtPre%)) 'yup, update the pointer
        END IF
        IF rtSuf% >= firstCode% THEN
            rtSuf% = firstCode% + rtLocArr%(rtCodArr%(rtSuf%))
        END IF
        '-----
        PrefArr%(rtJ% + firstCode%) = rtPre%
        SuffArr%(rtJ% + firstCode%) = rtSuf%
    NEXT rtJ%

    IF oldCode% >= firstCode% THEN
        oldCode% = firstCode% + rtLocArr%(rtCodArr%(oldCode%))
    END IF

    rtNewEnt% = rtNumEnt% + 1 'number of entries on the Hash table now

    ERASE rtPreArr%, rtSufArr%, rtUseArr&
    ERASE rtPnArr%, rtCodArr%, rtLocArr%
    RETURN
END FUNCTION


