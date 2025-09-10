      ******************************************************************
      * Author:
      * Date:
      * Purpose:
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
       DATA DIVISION.
       FILE SECTION.
       FD  MEMBER-FILE.
       01  MEMBER-FILE-REC.
           05 MEM-ID       PIC X(18).
           05 MEM-SID-ID   PIC X(23).
           05 MEM-NAME     PIC X(35).
           05 MEM-PHONE    PIC X(10).

       FD BOOK-FILE.
       01 BOOK-FILE-REC.
           05 BOOK-ISBN        PIC X(17).
           05 BOOK-NAME        PIC X(55).
           05 BOOK-AUTHOR      PIC X(27).
           05 BOOK-CALL-NUM    PIC X(25).
           05 BOOK-CATEGORY    PIC X(22).
           05 BOOK-STATUS      PIC A.

       FD BORROW-FILE.
       01  BORROW-FILE-REC.
           05 BORROW-ID-FILE       PIC X(15).
           05 BORROW-MEM-ID        PIC X(14).
           05 BORROW-BOOK-ISBN     PIC X(20).
           05 BORROW-DATE          PIC X(21).
           05 RETURN-DATE          PIC X(8).

       WORKING-STORAGE SECTION.
       01 PROG-CONTROL.
           05 WS-CHOICE PIC 9.

       01 LOGIN-REGIS.
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

       01 BORROW-BOOK-SYS.
           05 BORROW-ISBN  PIC X(17).
           05 BORROW-ID    PIC X(11).
           05 PREFIX-BORROW   PIC A VALUE"B".
           05 BORROW-CURRENT-DATE-TIME PIC X(21).
           05 BORROW-YEAR  PIC 9(4).
           05 BORROW-MONTH PIC 9(2).
           05 BORROW-DAY PIC 9(2).
           05 BORROW-HOUR PIC 9(2).
           05 BORROW-MINUTE PIC 9(2).
           05 BORROW-SECOND PIC 9(2).
           05 NOW-BORROW   PIC X(17).
           05 WS-BORROW-BOOK-SYS.
               10 WS-FOUND-ISBN    PIC A   VALUE "N".
       01  WS-FILE-STATUS        PIC XX     VALUE SPACES.
       77 SEQ-NUMBER    PIC 9(9) VALUE 1.
       77 TMP-ID             PIC 9(10).
       01 WS-EOF-FLAG          PIC A(1)    VALUE 'N'.
           88 WS-END-OF-FILE               VALUE 'Y'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM MENU-LOOP UNTIL WS-CHOICE = 6.
           STOP RUN.
      *>  MENU
       MENU-LOOP.
           DISPLAY "===============================================".
           DISPLAY "   LIBRARY SYSTEM - MENU".
           DISPLAY "===============================================".
           DISPLAY " 1. Login".
           DISPLAY " 2. Register".
           DISPLAY " 3. Borrow Book".
           DISPLAY " 4. Return Book".
           DISPLAY " 5. Book List".
           DISPLAY " 6. History".
           DISPLAY " 7. Exit Program".
           DISPLAY "===============================================".
           DISPLAY "Enter Your Choice :"
           ACCEPT WS-CHOICE.
           EVALUATE WS-CHOICE
             WHEN 1  PERFORM LOGIN
             WHEN 2  PERFORM REGISTER
             WHEN 3  PERFORM BORROW-BOOK
             WHEN 7  PERFORM  EXIT-PROGRAM
      *>        WHEN OTHER DISPLAY "Invalid choice."
           END-EVALUATE.
      *>  1. Login
       LOGIN.
           DISPLAY "============================".
           DISPLAY "  Wellcome To Login System  ".
           DISPLAY "============================".
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
                           MOVE MEM-SID-ID TO USER-MEM-ID
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
           DISPLAY "=============================="
           DISPLAY "  Welcome To Register System  "
           DISPLAY "=============================="
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
                           DISPLAY "This Student ID is alread"
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
               DISPLAY "Registration completed successfully."
               DISPLAY "Your Library Member ID is : "STD-MEM-ID
               CLOSE MEMBER-FILE
           END-IF.

       *> 3. Borrow Book
           BORROW-BOOK.
           IF USER-MEM-ID = SPACE
               DISPLAY "You must login first !!!"
               PERFORM LOGIN

           ELSE
               DISPLAY "=================================="
               DISPLAY "  Wellcome To Borrow Book System  "
               DISPLAY "=================================="
               DISPLAY "Member ID : " MEM-ID
               DISPLAY "Pleasr enter book ISBN number : "
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
                               DISPLAY "Book Found:"
                               DISPLAY "Title :" BOOK-NAME
                               DISPLAY "Author :" BOOK-AUTHOR
                               DISPLAY "Call Number:" BOOK-CALL-NUM
                               DISPLAY "Category:" BOOK-CATEGORY
                               DISPLAY "Status :" BOOK-STATUS
                                   IF BOOK-STATUS = "A"
                                       DISPLAY "This book is available"
                                           "for borrowing."
                                          MOVE BORROW-ISBN TO NOW-BORROW
                                       PERFORM ADD-BORROW
                                   ELSE
                                       DISPLAY "Sorry, this book is"
                                           "currently not available."
                                   END-IF
                           END-IF
                   END-READ
               END-PERFORM
               IF WS-FOUND-ISBN = "N"
                   DISPLAY "Book with ISBN " BORROW-ISBN " not found."
               END-IF
               CLOSE BOOK-FILE

           END-IF.

       ADD-BORROW.
           MOVE FUNCTION CURRENT-DATE TO BORROW-CURRENT-DATE-TIME.
           MOVE BORROW-CURRENT-DATE-TIME(1:4) TO BORROW-YEAR
           MOVE BORROW-CURRENT-DATE-TIME(5:2) TO BORROW-MONTH
           MOVE BORROW-CURRENT-DATE-TIME(7:2) TO BORROW-DAY
           MOVE BORROW-CURRENT-DATE-TIME(9:2) TO BORROW-HOUR
           MOVE BORROW-CURRENT-DATE-TIME(11:2) TO BORROW-MINUTE
           MOVE BORROW-CURRENT-DATE-TIME(13:2) TO BORROW-SECOND

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

           MOVE 0 TO SEQ-NUMBER
           OPEN INPUT BORROW-FILE
           PERFORM UNTIL WS-END-OF-FILE
               READ BORROW-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                  NOT AT END
                       MOVE BORROW-ID-FILE(2:10) TO TMP-ID
                       COMPUTE SEQ-NUMBER = FUNCTION NUMVAL(TMP-ID)
               END-READ
           END-PERFORM
           CLOSE BORROW-FILE
           ADD 1 TO SEQ-NUMBER
           MOVE ZEROES TO TMP-ID
           MOVE SEQ-NUMBER TO TMP-ID(10 - FUNCTION LENGTH(SEQ-NUMBER) +
                1 : FUNCTION LENGTH(SEQ-NUMBER))
           MOVE PREFIX-BORROW TO BORROW-ID(1:1)
           MOVE TMP-ID TO BORROW-ID(2:10)
           DISPLAY SPACE
           DISPLAY "BORROW ID IS :" BORROW-ID

           MOVE BORROW-ID TO BORROW-ID-FILE
           MOVE USER-MEM-ID TO BORROW-MEM-ID
           MOVE NOW-BORROW TO BORROW-BOOK-ISBN
           MOVE "NULL" TO RETURN-DATE


           OPEN EXTEND BORROW-FILE
           WRITE BORROW-FILE-REC
           DISPLAY "USER " BORROW-MEM-ID
           DISPLAY "Borrow "BORROW-ISBN
           DISPLAY "At "BORROW-DATE
           DISPLAY "you borrow successed!!"
           CLOSE BORROW-FILE.

       EXIT-PROGRAM.
           STOP RUN.

       END PROGRAM Library-Project.
