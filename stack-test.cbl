       IDENTIFICATION DIVISION.
       PROGRAM-ID. stack-test.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(5)V9(2).
       01 NUM2 PIC 9(5)V9(2).
       01 NUM3 PIC 9(5)V9(2).
       01 WS-STR1 PIC X(12).
       01 WS-STR2 PIC X(12).
       01 WS-STR3 PIC X(12).
       01 WS-EXPR1 PIC X(12) VALUE '+,2,5'.
       01 WS-EXPR2 PIC X(12) VALUE '-,5,2'.
       01 WS-EXPR3 PIC X(12) VALUE '*,2,5'.
       01 WS-EXPR4 PIC X(12) VALUE '/,4,2'.
       LOCAL-STORAGE SECTION.
       COPY stack.
           COPY node-info.
           COPY node-info2.
           COPY node-info3.
       COPY stack REPLACING stack BY new-stack.
       PROCEDURE DIVISION.
       DISPLAY WS-EXPR1.
       UNSTRING WS-EXPR1 DELIMITED BY ','
           INTO WS-STR1, WS-STR2, WS-STR3.
       END-NSTRING.
       CALL "push" USING
       BY REFERENCE stack
       BY CONTENT WS-STR3
       END-CALL
       CALL "push" USING
       BY REFERENCE stack
       BY CONTENT WS-STR2 
       END-CALL
       CALL "push" USING
       BY REFERENCE stack
       BY CONTENT WS-STR1
       END-CALL    
           CALL "pop" USING
           BY REFERENCE stack
           BY REFERENCE node-info
           END-CALL
           CALL "pop" USING
           BY REFERENCE stack
           BY REFERENCE node-info2
           END-CALL
           CALL "pop" USING
           BY REFERENCE stack
           BY REFERENCE node-info3
           END-CALL
       IF node-info = "+" THEN
           COMPUTE NUM1 = FUNCTION NUMVAL (node-info2).
           COMPUTE NUM2 = FUNCTION NUMVAL (node-info3).
           ADD NUM1 NUM2 to NUM3
           DISPLAY NUM3.
       ELSE
       IF node-info = "-" THEN
           COMPUTE NUM1 = FUNCTION NUMVAL (node-info2).
           COMPUTE NUM2 = FUNCTION NUMVAL (node-info3).
           SUBTRACT NUM1 NUM2 to NUM3
           DISPLAY NUM3.
       ELSE
       IF node-info = "*" THEN
           COMPUTE NUM1 = FUNCTION NUMVAL (node-info2).
           COMPUTE NUM2 = FUNCTION NUMVAL (node-info3).
           MULTIPLY NUM1 NUM2 to NUM3
           DISPLAY NUM3.
        ELSE
               IF node-info = "/" THEN
           COMPUTE NUM1 = FUNCTION NUMVAL (node-info2).
           COMPUTE NUM2 = FUNCTION NUMVAL (node-info3).
           DIVIDE NUM1 NUM2 to NUM3
           DISPLAY NUM3.
           ELSE
           DISPLAY "BAD BINOP".
       END-IF.
       STOP RUN.
       END PROGRAM stack-test.

       COPY stack-utilities.
