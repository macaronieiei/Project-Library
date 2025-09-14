      ******************************************************************
      * Author:
      * Date:
      * Purpose:Library
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Library-Project.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MEMBER-FILE ASSIGN TO "MEMBERS.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT BOOK-FILE ASSIGN TO "BOOKS.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT BORROW-FILE  ASSIGN TO "BORROW.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RETURN-FILE  ASSIGN TO "RETURN.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT TEMP-BORROW-FILE ASSIGN TO "TEMP_BORROW.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT TEMP-BOOK-FILE ASSIGN TO "TEMP_BOOK.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  MEMBER-FILE.
       01  MEMBER-FILE-REC.
           05 MEM-ID       PIC X(18).
           05 MEM-SID-ID   PIC X(23).
           05 MEM-NAME     PIC X(35).
           05 MEM-PHONE    PIC X(10).

       FD  BOOK-FILE.
       01  BOOK-FILE-REC.
           05 BOOK-ISBN        PIC X(17).
           05 BOOK-NAME        PIC X(55).
           05 BOOK-AUTHOR      PIC X(27).
           05 BOOK-CALL-NUM    PIC X(25).
           05 BOOK-CATEGORY    PIC X(22).
           05 BOOK-STATUS      PIC A.

       FD  BORROW-FILE.
       01  BORROW-FILE-REC.
           05 BORROW-ID-FILE       PIC X(15).
           05 BORROW-MEM-ID        PIC X(14).
           05 BORROW-BOOK-ISBN     PIC X(20).
           05 BORROW-DATE          PIC X(21).
           05 RETURN-DATE          PIC X(10).

       FD  RETURN-FILE.
       01  RETURN-FILE-REC.
           05 RETURN-ID-FILE            PIC X(15).
           05 BORROW-ID-R             PIC X(15).
           05 RETURN-MEM-ID     PIC X(14).
           05 RETURN-BOOK-ISBN  PIC X(20).
           05 BORROW-DATE-R     PIC X(21).
           05 RETURN-DATE-R     PIC X(21).

       FD  TEMP-BORROW-FILE.
       01  TEMP-BORROW-REC.
           05 TEMP-BORROW-ID       PIC X(15).
           05 TEMP-BORROW-MEM-ID   PIC X(14).
           05 TEMP-BORROW-ISBN     PIC X(20).
           05 TEMP-BORROW-DATE     PIC X(21).
           05 TEMP-RETURN-DATE     PIC X(21).

       FD  TEMP-BOOK-FILE.
       01  TEMP-BOOK-REC.
           05 TEMP-BOOK-ISBN        PIC X(17).
           05 TEMP-BOOK-NAME        PIC X(55).
           05 TEMP-BOOK-AUTHOR      PIC X(27).
           05 TEMP-BOOK-CALL-NUM    PIC X(25).
           05 TEMP-BOOK-CATEGORY    PIC X(22).
           05 TEMP-BOOK-STATUS      PIC A.

       WORKING-STORAGE SECTION.
       01  DUMMY PIC X.
       01  PROG-CONTROL.
           05 WS-CHOICE PIC 9.

       01  LOGIN-REGIS.
           05 STD-MEM-ID   PIC X(14).
           05 STD-ID       PIC X(10).
           05 STD-NAME     PIC X(30).
           05 STD-PHONE    PIC X(10).
           05 STD-CHECK-DIGIT  PIC X.
           05 PREFIX           PIC X(3) VALUE "200".
           05 USER-MEM-ID  PIC X(14).
           05 WS-LOGIN-REGIS.
         10 LOGIN-REGIS-CHOICE   PIC 9.
         10 WS-FOUND-ID            PIC A   VALUE "Y".
         10 WS-SUM       PIC 99.
         10 WS-POS       PIC 99.
         10 WS-CHAR      PIC X.
         10 WS-DIGIT     PIC 9.
         10 WS-CHECK-DIGIT-NUM   PIC 9.
         10 WS-INT-DIV   PIC 9(3).

       01  BORROW-BOOK-SYS.
           05 BORROW-ISBN  PIC X(17).
           05 BORROW-ID    PIC X(11).
           05 PREFIX-BORROW   PIC A VALUE"B".
           05 BORROW-CURRENT-DATE-TIME PIC X(21).
           05 BORROW-DATE-INT PIC 9(8).
           05 BORROW-YEAR  PIC 9(4).
           05 BORROW-MONTH PIC 9(2).
           05 BORROW-DAY PIC 9(2).
           05 BORROW-HOUR PIC 9(2).
           05 BORROW-MINUTE PIC 9(2).
           05 BORROW-SECOND PIC 9(2).
           05 NOW-BORROW   PIC X(17).
           05 RETURN-DATE-PRE  PIC  X(8).
           05 RETURN-DAY-PRE   PIC X(2).
           05 RETURN-MONTH-PRE   PIC X(2).
           05 RETURN-YEAR-PRE   PIC X(4).
           05 WS-BORROW-BOOK-SYS.
               10 WS-FOUND-ISBN    PIC A   VALUE "N".

       01  RETURN-BOOK-SYS.
           05 RETURN-ISBN  PIC X(17).
           05 RETURN-ID    PIC X(11).
           05 PREFIX-RETURN   PIC A VALUE "R".
           05 RETURN-CURRENT-DATE-TIME PIC X(21).
           05 RETURN-YEAR  PIC 9(4).
           05 RETURN-MONTH PIC 9(2).
           05 RETURN-DAY PIC 9(2).
           05 RETURN-HOUR PIC 9(2).
           05 RETURN-MINUTE PIC 9(2).
           05 RETURN-SECOND PIC 9(2).
           05 NOW-RETURN   PIC X(21).

       01  WS-DATE-CALC.
           05 BRW-YYYY        PIC 9(4).
           05 BRW-MM          PIC 9(2).
           05 BRW-DD          PIC 9(2).
           05 BRW-YYYYMMDD    PIC 9(8).
           05 INT-BRW         PIC 9(9).
           05 INT-RTN         PIC 9(9).
           05 DAYS-USED       PIC 9(5).
           05 LATE-DAYS       PIC 9(5).
           05 FINE-BAHT       PIC 9(7).

       01  WS-BOOK-DISPLAY-REC.
           05 WS-NO         PIC Z(2).
           05 WS-ISBN       PIC X(17).
           05 WS-NAME       PIC X(55).
           05 WS-TITLE      PIC X(55).
           05 WS-AUTHOR     PIC X(27).
           05 WS-CALLNO     PIC X(25).
           05 WS-CATEGORY   PIC X(22).
           05 WS-STATUS     PIC X.
           05 STRING-BOOK-LIST  PIC X(170).

       01  WS-BOOK-COUNT    PIC 9(3) VALUE 0.
       01  NOZERO-WS-BOOK-COUNT PIC ZZ9.

       01  WS-FILE-STATUS        PIC XX     VALUE SPACES.

           77  SEQ-NUMBER    PIC 9(9) VALUE 1.
           77  TMP-ID             PIC 9(10).
           77  DAILY-FINE         PIC 9(2) VALUE 05.

           77  RTN-SEQ-NUM      PIC 9(10) VALUE 0.
           77  RTN-TMP-ID       PIC 9(10).

       01  WS-RETURN-FLAGS.
           05 WS-FOUND-BORROW             PIC A   VALUE "N".
           05 CURRENT-BORROW-ID           PIC X(15).

       01  WS-EOF-FLAG          PIC A(1)    VALUE 'N'.
           88 WS-END-OF-FILE               VALUE 'Y'.

       01  WS-HISTORY-COUNTER   PIC 9(3) VALUE 0.
       01  WS-HIS-CNT-FORMAT    PIC ZZ9.
       01  WS-TOTAL-RET-FORMAT  PIC Z,ZZ9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM MENU-LOOP UNTIL WS-CHOICE = 7.
           STOP RUN.

      *>  MENU
       MENU-LOOP.
           DISPLAY "===============================================".
           DISPLAY "            LIBRARY SYSTEM - MENU".
           DISPLAY "===============================================".
           DISPLAY " 1. LOGIN".
           DISPLAY " 2. REGISTER".
           DISPLAY " 3. BORROW BOOKS".
           DISPLAY " 4. RETURN BOOKS".
           DISPLAY " 5. ALL BOOKS LIST".
           DISPLAY " 6. BORROW & RETURN HISTORY".
           DISPLAY " 7. EXIT PROGRAM".
           DISPLAY "===============================================".
           DISPLAY "Enter Your Choice :"
           ACCEPT WS-CHOICE.
           EVALUATE WS-CHOICE
             WHEN 1  PERFORM LOGIN
             WHEN 2  PERFORM REGISTER
             WHEN 3  PERFORM BORROW-BOOK
             WHEN 4  PERFORM RETURN-BOOK
             WHEN 5  PERFORM BOOK-LIST
             WHEN 6  PERFORM HISTORY
             WHEN 7  PERFORM EXIT-PROGRAM
             WHEN OTHER DISPLAY "Invalid choice "
             ",Press Enter To Try Again !"
             ACCEPT DUMMY
           END-EVALUATE.

      *>  1. Login
       LOGIN.
           DISPLAY "===============================================".
           DISPLAY "           WELCOME TO LOGIN SYSTEM".
           DISPLAY "===============================================".
           DISPLAY "Enter Your Student ID (10 digits): "
           ACCEPT STD-ID.

           OPEN INPUT MEMBER-FILE

           MOVE "N" TO WS-FOUND-ID
           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE OR WS-FOUND-ID = "Y"
               READ MEMBER-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       IF MEM-SID-ID = STD-ID
                           DISPLAY "Login Successful. Welcome, "MEM-NAME
                           MOVE "Y" TO WS-FOUND-ID
                           MOVE MEM-ID TO USER-MEM-ID
                 END-IF
               END-READ
           END-PERFORM

           CLOSE MEMBER-FILE

           IF WS-FOUND-ID = "N"
               DISPLAY "Student ID not found. Please register first."
               PERFORM REGISTER
           END-IF.

      *>  2. Register
       REGISTER.
           DISPLAY "==============================================="
           DISPLAY "         WELCOME TO REGISTER SYSTEM"
           DISPLAY "==============================================="
           DISPLAY "Enter STUDENT-ID (10 digits): "
           ACCEPT STD-ID

           OPEN INPUT MEMBER-FILE
           MOVE "N" TO WS-FOUND-ID
           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE OR WS-FOUND-ID = "Y"
               READ MEMBER-FILE
             AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       IF MEM-SID-ID = STD-ID
                           DISPLAY "This Student ID is already"
                              " registered."
                     DISPLAY "Go to Login System"
                     MOVE "Y" TO WS-FOUND-ID
                 END-IF
               END-READ
           END-PERFORM
           CLOSE MEMBER-FILE

           IF WS-FOUND-ID = "Y"
               PERFORM LOGIN
           ELSE
               DISPLAY "Enter Name: "
               ACCEPT STD-NAME
               DISPLAY "Enter Phone: "
               ACCEPT STD-PHONE

               MOVE 0 TO WS-SUM

               PERFORM VARYING WS-POS FROM 1 BY 1 UNTIL WS-POS > 10
                    MOVE STD-ID(WS-POS:1) TO WS-CHAR
                    IF WS-CHAR NUMERIC
                        COMPUTE WS-DIGIT = FUNCTION NUMVAL(WS-CHAR)
                        ADD WS-DIGIT TO WS-SUM
                   END-IF
               END-PERFORM

               COMPUTE WS-INT-DIV = WS-SUM / 10
               COMPUTE WS-CHECK-DIGIT-NUM = WS-SUM - (WS-INT-DIV * 10)
               MOVE WS-CHECK-DIGIT-NUM TO STD-CHECK-DIGIT
               STRING PREFIX DELIMITED BY SIZE
                      STD-ID DELIMITED BY SIZE
                      STD-CHECK-DIGIT DELIMITED BY SIZE
                      INTO STD-MEM-ID
               END-STRING
               MOVE STD-MEM-ID TO MEM-ID
               MOVE STD-ID   TO MEM-SID-ID
               MOVE STD-NAME TO MEM-NAME
               MOVE STD-PHONE TO MEM-PHONE

               OPEN EXTEND MEMBER-FILE
               WRITE MEMBER-FILE-REC
               DISPLAY "==============================================="
               DISPLAY "Registration completed successfully!!!"
               DISPLAY "Your Library Member ID is : "STD-MEM-ID
               DISPLAY "==============================================="
               CLOSE MEMBER-FILE
               MOVE STD-MEM-ID TO USER-MEM-ID
           END-IF.

      *> 3. Borrow Book
       BORROW-BOOK.
           IF USER-MEM-ID = SPACES
               DISPLAY "You must login first !!!"
               PERFORM LOGIN
           ELSE
              DISPLAY "================================================"
              DISPLAY "       WELCOME TO BORROW BOOKS SYSTEM"
              DISPLAY "==============================================="
               DISPLAY "Member ID : " USER-MEM-ID
               DISPLAY "Please enter book ISBN number : "
               ACCEPT BORROW-ISBN

               OPEN INPUT BOOK-FILE

               MOVE "N" TO WS-FOUND-ISBN
               MOVE "N" TO WS-EOF-FLAG
               PERFORM UNTIL WS-END-OF-FILE OR WS-FOUND-ISBN = "Y"
                   READ BOOK-FILE
                       AT END MOVE "Y" TO WS-EOF-FLAG
                       NOT AT END
                           IF BORROW-ISBN = BOOK-ISBN
                               DISPLAY SPACE
                               MOVE "Y" TO WS-FOUND-ISBN
               DISPLAY "Book Found"
               DISPLAY "Title      : " BOOK-NAME
               DISPLAY "Author     : " BOOK-AUTHOR
               DISPLAY "Call Number: " BOOK-CALL-NUM
               DISPLAY "Category   : " BOOK-CATEGORY
               DISPLAY "Status     : " BOOK-STATUS
                      IF BOOK-STATUS = "A"
                         DISPLAY "This book is available"
                                 " for borrowing."
                          MOVE BORROW-ISBN TO NOW-BORROW
                             ELSE
                                DISPLAY "Sorry, this book is"
                                        " currently not available."
                      END-IF
                           END-IF

                   END-READ
               END-PERFORM
               CLOSE BOOK-FILE

               IF WS-FOUND-ISBN = "N"
                   DISPLAY "Book with ISBN " BORROW-ISBN " not found."
               ELSE
                   IF BOOK-STATUS = "A"
                       PERFORM ADD-BORROW
                       PERFORM UPDATE-BOOK-STATUS-TO-BORROWED
                   END-IF
               END-IF
           END-IF.

      *>  Return book
       RETURN-BOOK.
            IF USER-MEM-ID = SPACES
               DISPLAY "You must login first !!!"
               PERFORM LOGIN
            ELSE
               DISPLAY "==============================================="
               DISPLAY "       WELCOME TO RETURN BOOKS SYSTEM"
               DISPLAY "==============================================="
               DISPLAY "Member ID : " USER-MEM-ID
               DISPLAY "Enter book ISBN to return : "
               ACCEPT RETURN-ISBN

               OPEN INPUT BORROW-FILE

               MOVE "N" TO WS-FOUND-BORROW
               MOVE "N" TO WS-EOF-FLAG
               MOVE SPACES TO CURRENT-BORROW-ID

               PERFORM UNTIL WS-END-OF-FILE OR WS-FOUND-BORROW = "Y"
                   READ BORROW-FILE
                       AT END MOVE "Y" TO WS-EOF-FLAG
                       NOT AT END

                    IF RETURN-ISBN = BORROW-BOOK-ISBN(1:17)
                       IF USER-MEM-ID = BORROW-MEM-ID
                          IF RETURN-DATE(1:4) = "NULL"
                             MOVE "Y"  TO WS-FOUND-BORROW

                             MOVE BORROW-ID-FILE TO CURRENT-BORROW-ID
                          ELSE
                              DISPLAY "This book was already RETURN"
                          END-IF
                       ELSE
                         DISPLAY "This borrow BELONGS to another member"
                       END-IF
                    END-IF
                   END-READ
               END-PERFORM

               CLOSE BORROW-FILE

               IF WS-FOUND-BORROW = "N"
               DISPLAY "No outstanding borrow found for this ISBN."
               DISPLAY "Press Enter to continue..."
               ACCEPT DUMMY
               ELSE
                  DISPLAY "Borrow found. Processing return..."
                  PERFORM UPDATE-BORROW-STATUS
                  PERFORM ADD-RETURN
                  PERFORM UPDATE-BOOK-STATUS-TO-AVAILABLE
               END-IF
            END-IF.

      *> Update book status to borrowed (B)
       UPDATE-BOOK-STATUS-TO-BORROWED.
           OPEN INPUT BOOK-FILE
           OPEN OUTPUT TEMP-BOOK-FILE

           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE
               READ BOOK-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       IF BOOK-ISBN = BORROW-ISBN
                           MOVE BOOK-ISBN TO TEMP-BOOK-ISBN
                           MOVE BOOK-NAME TO TEMP-BOOK-NAME
                           MOVE BOOK-AUTHOR TO TEMP-BOOK-AUTHOR
                           MOVE BOOK-CALL-NUM TO TEMP-BOOK-CALL-NUM
                           MOVE BOOK-CATEGORY TO TEMP-BOOK-CATEGORY
                           MOVE "B" TO TEMP-BOOK-STATUS
                       ELSE
                           MOVE BOOK-ISBN TO TEMP-BOOK-ISBN
                           MOVE BOOK-NAME TO TEMP-BOOK-NAME
                           MOVE BOOK-AUTHOR TO TEMP-BOOK-AUTHOR
                           MOVE BOOK-CALL-NUM TO TEMP-BOOK-CALL-NUM
                           MOVE BOOK-CATEGORY TO TEMP-BOOK-CATEGORY
                           MOVE BOOK-STATUS TO TEMP-BOOK-STATUS
                       END-IF
                       WRITE TEMP-BOOK-REC
               END-READ
           END-PERFORM

           CLOSE BOOK-FILE
           CLOSE TEMP-BOOK-FILE

           *> Replace original file with temp file
           OPEN INPUT TEMP-BOOK-FILE
           OPEN OUTPUT BOOK-FILE

           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE
               READ TEMP-BOOK-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       MOVE TEMP-BOOK-ISBN TO BOOK-ISBN
                       MOVE TEMP-BOOK-NAME TO BOOK-NAME
                       MOVE TEMP-BOOK-AUTHOR TO BOOK-AUTHOR
                       MOVE TEMP-BOOK-CALL-NUM TO BOOK-CALL-NUM
                       MOVE TEMP-BOOK-CATEGORY TO BOOK-CATEGORY
                       MOVE TEMP-BOOK-STATUS TO BOOK-STATUS
                       WRITE BOOK-FILE-REC
               END-READ
           END-PERFORM

           CLOSE TEMP-BOOK-FILE
           CLOSE BOOK-FILE.

      *> Update book status to available (A)
       UPDATE-BOOK-STATUS-TO-AVAILABLE.
           OPEN INPUT BOOK-FILE
           OPEN OUTPUT TEMP-BOOK-FILE

           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE
               READ BOOK-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       IF BOOK-ISBN = RETURN-ISBN
                           MOVE BOOK-ISBN TO TEMP-BOOK-ISBN
                           MOVE BOOK-NAME TO TEMP-BOOK-NAME
                           MOVE BOOK-AUTHOR TO TEMP-BOOK-AUTHOR
                           MOVE BOOK-CALL-NUM TO TEMP-BOOK-CALL-NUM
                           MOVE BOOK-CATEGORY TO TEMP-BOOK-CATEGORY
                           MOVE "A" TO TEMP-BOOK-STATUS
                       ELSE
                           MOVE BOOK-ISBN TO TEMP-BOOK-ISBN
                           MOVE BOOK-NAME TO TEMP-BOOK-NAME
                           MOVE BOOK-AUTHOR TO TEMP-BOOK-AUTHOR
                           MOVE BOOK-CALL-NUM TO TEMP-BOOK-CALL-NUM
                           MOVE BOOK-CATEGORY TO TEMP-BOOK-CATEGORY
                           MOVE BOOK-STATUS TO TEMP-BOOK-STATUS
                       END-IF
                       WRITE TEMP-BOOK-REC
               END-READ
           END-PERFORM

           CLOSE BOOK-FILE
           CLOSE TEMP-BOOK-FILE

           *> Replace original file with temp file
           OPEN INPUT TEMP-BOOK-FILE
           OPEN OUTPUT BOOK-FILE

           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE
               READ TEMP-BOOK-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       MOVE TEMP-BOOK-ISBN TO BOOK-ISBN
                       MOVE TEMP-BOOK-NAME TO BOOK-NAME
                       MOVE TEMP-BOOK-AUTHOR TO BOOK-AUTHOR
                       MOVE TEMP-BOOK-CALL-NUM TO BOOK-CALL-NUM
                       MOVE TEMP-BOOK-CATEGORY TO BOOK-CATEGORY
                       MOVE TEMP-BOOK-STATUS TO BOOK-STATUS
                       WRITE BOOK-FILE-REC
               END-READ
           END-PERFORM

           CLOSE TEMP-BOOK-FILE
           CLOSE BOOK-FILE.

      *> Update borrow status
       UPDATE-BORROW-STATUS.

           MOVE FUNCTION CURRENT-DATE TO RETURN-CURRENT-DATE-TIME.
           MOVE RETURN-CURRENT-DATE-TIME(1:4) TO RETURN-YEAR
           MOVE RETURN-CURRENT-DATE-TIME(5:2) TO RETURN-MONTH
           MOVE RETURN-CURRENT-DATE-TIME(7:2) TO RETURN-DAY
           MOVE RETURN-CURRENT-DATE-TIME(9:2) TO RETURN-HOUR
           MOVE RETURN-CURRENT-DATE-TIME(11:2) TO RETURN-MINUTE
           MOVE RETURN-CURRENT-DATE-TIME(13:2) TO RETURN-SECOND
           STRING RETURN-YEAR DELIMITED BY SIZE
              "-" DELIMITED BY SIZE
              RETURN-MONTH DELIMITED BY SIZE
              "-" DELIMITED BY SIZE
              RETURN-DAY DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              RETURN-HOUR DELIMITED BY SIZE
              ":" DELIMITED BY SIZE
             RETURN-MINUTE DELIMITED BY SIZE
              ":" DELIMITED BY SIZE
              RETURN-SECOND DELIMITED BY SIZE
              INTO NOW-RETURN
           END-STRING.

           OPEN INPUT BORROW-FILE
           OPEN OUTPUT TEMP-BORROW-FILE

           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE
               READ BORROW-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       IF BORROW-ID-FILE = CURRENT-BORROW-ID
                           MOVE BORROW-ID-FILE TO TEMP-BORROW-ID
                           MOVE BORROW-MEM-ID TO TEMP-BORROW-MEM-ID
                           MOVE BORROW-BOOK-ISBN TO TEMP-BORROW-ISBN
                           MOVE BORROW-DATE TO TEMP-BORROW-DATE
                           MOVE NOW-RETURN TO TEMP-RETURN-DATE
                       ELSE
                           MOVE BORROW-ID-FILE TO TEMP-BORROW-ID
                           MOVE BORROW-MEM-ID TO TEMP-BORROW-MEM-ID
                           MOVE BORROW-BOOK-ISBN TO TEMP-BORROW-ISBN
                           MOVE BORROW-DATE TO TEMP-BORROW-DATE
                           MOVE RETURN-DATE TO TEMP-RETURN-DATE
                       END-IF
                       WRITE TEMP-BORROW-REC
               END-READ
           END-PERFORM

           CLOSE BORROW-FILE
           CLOSE TEMP-BORROW-FILE

           *> Replace original file with temp file
           OPEN INPUT TEMP-BORROW-FILE
           OPEN OUTPUT BORROW-FILE

           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE
               READ TEMP-BORROW-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       MOVE TEMP-BORROW-ID TO BORROW-ID-FILE
                       MOVE TEMP-BORROW-MEM-ID TO BORROW-MEM-ID
                       MOVE TEMP-BORROW-ISBN TO BORROW-BOOK-ISBN
                       MOVE TEMP-BORROW-DATE TO BORROW-DATE
                       MOVE TEMP-RETURN-DATE TO RETURN-DATE
                       WRITE BORROW-FILE-REC
               END-READ
           END-PERFORM

           CLOSE TEMP-BORROW-FILE
           CLOSE BORROW-FILE.

      *>  Add return record
       ADD-RETURN.
      *> Generate Return ID
           MOVE 0 TO RTN-SEQ-NUM

           OPEN INPUT RETURN-FILE

           MOVE "N" TO WS-EOF-FLAG
           PERFORM UNTIL WS-END-OF-FILE
               READ RETURN-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                   MOVE RETURN-ID-FILE(2:10) TO RTN-TMP-ID
                   IF RTN-TMP-ID IS NUMERIC
                      COMPUTE RTN-SEQ-NUM = FUNCTION NUMVAL(RTN-TMP-ID)
                   END-IF
               END-READ
           END-PERFORM
           CLOSE RETURN-FILE

           ADD 1 TO RTN-SEQ-NUM
           MOVE RTN-SEQ-NUM TO RTN-TMP-ID
           MOVE "R"        TO RETURN-ID(1:1)
           MOVE RTN-TMP-ID TO RETURN-ID(2:10).

           *> Get borrow information
           OPEN INPUT BORROW-FILE
           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-END-OF-FILE
               READ BORROW-FILE
                   AT END MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       IF BORROW-ID-FILE = CURRENT-BORROW-ID
                           MOVE BORROW-ID-FILE TO BORROW-ID-R
                           MOVE BORROW-MEM-ID TO RETURN-MEM-ID
                           MOVE BORROW-BOOK-ISBN TO RETURN-BOOK-ISBN
                           MOVE BORROW-DATE TO BORROW-DATE-R
                       END-IF
               END-READ
           END-PERFORM
           CLOSE BORROW-FILE

           OPEN EXTEND RETURN-FILE

           MOVE RETURN-ID TO RETURN-ID-FILE
           MOVE NOW-RETURN TO RETURN-DATE-R

           WRITE RETURN-FILE-REC
           CLOSE RETURN-FILE

           DISPLAY "==============================================="
           DISPLAY "             RETURN SUCCESSFUL !"
           DISPLAY "==============================================="
           DISPLAY "Return ID : " RETURN-ID
           DISPLAY "Borrow ID : " BORROW-ID-R
           DISPLAY "Member ID : " RETURN-MEM-ID
           DISPLAY "ISBN      : " RETURN-BOOK-ISBN
           DISPLAY "Borrowed  : " BORROW-DATE-R
           DISPLAY "Returned  : " RETURN-DATE-R
           DISPLAY "==============================================="
           DISPLAY "Press Enter to continue..."
           ACCEPT DUMMY.

      *>  Add borrow record
       ADD-BORROW.
           PERFORM FORMAT-DATE-TIME.
           *> Generate new borrow ID
           MOVE 0 TO SEQ-NUMBER
           OPEN INPUT BORROW-FILE
           MOVE "N" TO WS-EOF-FLAG
           PERFORM UNTIL WS-END-OF-FILE
               READ BORROW-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                  NOT AT END
                       MOVE BORROW-ID-FILE(2:10) TO TMP-ID
                       IF TMP-ID IS NUMERIC
                          COMPUTE SEQ-NUMBER = FUNCTION NUMVAL(TMP-ID)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE BORROW-FILE

           ADD 1 TO SEQ-NUMBER
           MOVE SEQ-NUMBER TO TMP-ID
           MOVE PREFIX-BORROW TO BORROW-ID(1:1)
           MOVE TMP-ID TO BORROW-ID(2:10)
           *> Convert borrow date into integer days
           COMPUTE BORROW-DATE-INT =
               FUNCTION INTEGER-OF-DATE(BORROW-YEAR * 10000
                                        + BORROW-MONTH * 100
                                        + BORROW-DAY)

           *> Add due date depending on category
           IF BOOK-CATEGORY = "Cartoon"
               COMPUTE BORROW-DATE-INT = BORROW-DATE-INT + 5
           ELSE
               COMPUTE BORROW-DATE-INT = BORROW-DATE-INT + 14
           END-IF

           MOVE FUNCTION DATE-OF-INTEGER(BORROW-DATE-INT)
           TO RETURN-DATE-PRE

           MOVE RETURN-DATE-PRE(1:4) TO RETURN-YEAR-PRE
           MOVE RETURN-DATE-PRE(5:2) TO RETURN-MONTH-PRE
           MOVE RETURN-DATE-PRE(7:2) TO RETURN-DAY-PRE

           STRING RETURN-YEAR-PRE DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  RETURN-MONTH-PRE DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  RETURN-DAY-PRE DELIMITED BY SIZE
                  INTO RETURN-DATE
           END-STRING
           *> Write borrow record
           MOVE BORROW-ID TO BORROW-ID-FILE
           MOVE USER-MEM-ID TO BORROW-MEM-ID
           MOVE NOW-BORROW TO BORROW-BOOK-ISBN

           OPEN EXTEND BORROW-FILE
           WRITE BORROW-FILE-REC
           CLOSE BORROW-FILE
           PERFORM FORMAT-DATE-TIME.

           *> Display borrow information
           DISPLAY "==============================================="
           DISPLAY "             BORROW SUCCESSFUL !"
           DISPLAY "==============================================="
           DISPLAY "Borrow ID     : " BORROW-ID
           DISPLAY "Member ID     : " BORROW-MEM-ID
           DISPLAY "Book ISBN     : " BORROW-ISBN
           DISPLAY "Borrowed Date : " BORROW-DATE(1:10)
           DISPLAY "Borrowed Time : " BORROW-DATE(12:8)
           DISPLAY "Reture Date   : " RETURN-DATE
           DISPLAY "Book Status   : Changed to 'Borrowed (B)'"
           DISPLAY "==============================================="
           DISPLAY "Press Enter to continue..."
           ACCEPT DUMMY.

       FORMAT-DATE-TIME.
           *> Get current date and time
           MOVE FUNCTION CURRENT-DATE TO BORROW-CURRENT-DATE-TIME.
           MOVE BORROW-CURRENT-DATE-TIME(1:4) TO BORROW-YEAR
           MOVE BORROW-CURRENT-DATE-TIME(5:2) TO BORROW-MONTH
           MOVE BORROW-CURRENT-DATE-TIME(7:2) TO BORROW-DAY
           MOVE BORROW-CURRENT-DATE-TIME(9:2) TO BORROW-HOUR
           MOVE BORROW-CURRENT-DATE-TIME(11:2) TO BORROW-MINUTE
           MOVE BORROW-CURRENT-DATE-TIME(13:2) TO BORROW-SECOND

           *> Format date and time string
           STRING BORROW-YEAR DELIMITED BY SIZE
              "-" DELIMITED BY SIZE
              BORROW-MONTH DELIMITED BY SIZE
              "-" DELIMITED BY SIZE
              BORROW-DAY DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              BORROW-HOUR DELIMITED BY SIZE
              ":" DELIMITED BY SIZE
              BORROW-MINUTE DELIMITED BY SIZE
              ":" DELIMITED BY SIZE
              BORROW-SECOND DELIMITED BY SIZE
              INTO BORROW-DATE
           END-STRING.
      *>  All books list
       BOOK-LIST.
           OPEN INPUT BOOK-FILE
           MOVE 0 TO WS-BOOK-COUNT
           MOVE "N" TO WS-EOF-FLAG
           DISPLAY "================================================="
           "=========================================================="
           "============================================="
           DISPLAY "                                               "
           "                  ALL BOOKS LIST                           "

           DISPLAY "================================================="
           "=========================================================="
           "============================================="
           DISPLAY "No ISBN                  Title                   "
           "                            Author "
           "                     Call Number   "
           "     Category            Status"
           DISPLAY "================================================="
           "=========================================================="
           "============================================="

           PERFORM UNTIL WS-END-OF-FILE
               READ BOOK-FILE
                   AT END SET WS-END-OF-FILE TO TRUE
                   NOT AT END ADD 1 TO WS-BOOK-COUNT

                       MOVE WS-BOOK-COUNT  TO WS-NO
                       MOVE BOOK-ISBN      TO WS-ISBN
                       MOVE BOOK-NAME      TO WS-TITLE
                       MOVE BOOK-AUTHOR    TO WS-AUTHOR
                       MOVE BOOK-CALL-NUM  TO WS-CALLNO
                       MOVE BOOK-CATEGORY  TO WS-CATEGORY
                       MOVE BOOK-STATUS    TO WS-STATUS

              STRING
              WS-NO DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              WS-ISBN DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              WS-TITLE DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              WS-AUTHOR DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              WS-CALLNO DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              WS-CATEGORY DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              WS-STATUS DELIMITED BY SIZE
              INTO STRING-BOOK-LIST
           END-STRING
                   DISPLAY STRING-BOOK-LIST
               END-READ

           END-PERFORM
           MOVE WS-BOOK-COUNT TO NOZERO-WS-BOOK-COUNT
           DISPLAY " "
           DISPLAY "================================================="
           "=========================================================="
           "============================================="
           DISPLAY "Total books : " NOZERO-WS-BOOK-COUNT
        CLOSE BOOK-FILE.
          DISPLAY " ".
          DISPLAY "Press Enter to continue..."
          ACCEPT DUMMY .

      *> History - Show borrow and return history
       HISTORY.
          IF USER-MEM-ID = SPACES
              DISPLAY "You must login first !!!"
              PERFORM LOGIN
          ELSE
              DISPLAY "======================================"
              "======================================"
              DISPLAY
              "                         BORROW & RETURN HISTORY"
              DISPLAY "======================================"
              "======================================"
              DISPLAY "Member ID : " USER-MEM-ID

              MOVE 0 TO WS-HISTORY-COUNTER

              DISPLAY "BORROW RECORDS:"
              DISPLAY "======================================"
              "======================================"
              DISPLAY
              "Borrow ID       ISBN                 Date Borrowed"
              "         Return Status"
              DISPLAY "======================================"
              "======================================"

              OPEN INPUT BORROW-FILE
              MOVE "N" TO WS-EOF-FLAG

              PERFORM UNTIL WS-END-OF-FILE
                  READ BORROW-FILE
                      AT END MOVE "Y" TO WS-EOF-FLAG
                      NOT AT END
                          IF BORROW-MEM-ID = USER-MEM-ID
                              ADD 1 TO WS-HISTORY-COUNTER
                              IF RETURN-DATE(1:4) = "NULL"
                                  DISPLAY BORROW-ID-FILE " "
                                        BORROW-BOOK-ISBN " "
                                        BORROW-DATE " " "NOT RETURNED"
                              ELSE
                                  DISPLAY BORROW-ID-FILE " "
                                        BORROW-BOOK-ISBN " "
                                        BORROW-DATE " " "RETURNED"
                              END-IF
                          END-IF
                  END-READ
              END-PERFORM

              CLOSE BORROW-FILE

              DISPLAY " "
              DISPLAY "RETURN RECORDS:"
              DISPLAY "======================================"
              "======================================"
              DISPLAY "Return ID       Borrow ID       ISBN            "
              "      Date Returned"
              DISPLAY "======================================"
              "======================================"

              OPEN INPUT RETURN-FILE
              MOVE "N" TO WS-EOF-FLAG

               PERFORM UNTIL WS-END-OF-FILE
            READ RETURN-FILE
                AT END MOVE "Y" TO WS-EOF-FLAG
                NOT AT END
                    IF RETURN-MEM-ID = USER-MEM-ID
                        ADD 1 TO RTN-SEQ-NUM
                        DISPLAY RETURN-ID-FILE " "
                              BORROW-ID-R " "
                              RETURN-BOOK-ISBN " "
                              RETURN-DATE-R
                    END-IF
            END-READ
        END-PERFORM

        CLOSE RETURN-FILE

        IF RTN-SEQ-NUM = 0
            DISPLAY "No return records found."
        END-IF

        MOVE WS-HISTORY-COUNTER TO WS-HIS-CNT-FORMAT
        MOVE RTN-SEQ-NUM TO WS-TOTAL-RET-FORMAT
        DISPLAY " "
        DISPLAY "======================================"
              "======================================"
        DISPLAY "Total Borrow Records: " WS-HIS-CNT-FORMAT
        DISPLAY "Total Return Records: " WS-TOTAL-RET-FORMAT
        DISPLAY "======================================"
              "======================================"
        DISPLAY "Press Enter to continue..."
        ACCEPT DUMMY
       END-IF.

      *>  exit program
       EXIT-PROGRAM.
               DISPLAY "Thank you for using the service, "MEM-NAME
               DISPLAY "***************** END PROGRAM *****************"
                STOP RUN.

       END PROGRAM Library-Project.
