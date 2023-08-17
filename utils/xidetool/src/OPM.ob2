(* ETH Oberon, Copyright 2001 ETH Zuerich Institut fuer Computersysteme, ETH Zentrum, CH-8092 Zuerich.
Refer to the "General ETH Oberon System Source License" contract available at: http://www.oberon.ethz.ch/ *)

MODULE OPM;   (** non-portable *)

(* Machine dependent constants needed before code generation *)
(* Host interface *)
(* RC, NM, pjm, prk *)

IMPORT SYSTEM;

CONST (* i386 *)
   (* basic type sizes *)
   ByteSize* = 1;   (* SYSTEM.BYTE *)
   CharSize* = 1;   (* CHAR *)
   BoolSize* = 1;   (* BOOLEAN *)
   SetSize* = 4;    (* SET *)
   SIntSize* = 1;   (* SHORTINT *)
   IntSize* = 2;    (* INTEGER *)
   LIntSize* = 4;   (* LONGINT *)
   HIntSize* = 8;   (* HUGEINT *)
   RealSize* = 4;   (* REAL *)
   LRealSize* = 8;  (* LONGREAL *)
   ProcSize* = 4;   (* PROCEDURE type *)
   PointerSize* = 4;   (* POINTER type *)

   (* value of constant NIL *)
   nilval* = 0;

   (* target machine minimum values of basic types expressed in host machine format: *)
   MinSInt* = -80H;
   MinInt* = -8000H;
   MinLInt* =  80000000H;   (* i386: -2147483648*)
   MinRealPat = 0FF7FFFFFH;   (* most  negative, 32-bit pattern *)
   MinLRealPatL = 0FFFFFFFFH;   (* most  negative, lower 32-bit pattern *)
   MinLRealPatH = 0FFEFFFFFH;   (* most  negative, higher 32-bit pattern *)

   (* target machine maximum values of basic types expressed in host machine format: *)
   MaxSInt* = 7FH;
   MaxInt* = 7FFFH;
   MaxLInt* = 7FFFFFFFH;   (* i386: 2147483647*)
   MaxSet* = 31;   (* must be >= 15, else the bootstraped compiler cannot run (IN-tests) *)
   MaxRealPat = 7F7FFFFFH;   (* most positive, 32-bit pattern *)
   MaxLRealPatL = 0FFFFFFFFH;   (* most positive, lower 32-bit pattern *)
   MaxLRealPatH = 7FEFFFFFH;      (* most positive, higher 32-bit pattern *)

   (* maximal index value for array declaration: *)
   MaxIndex* = MaxLInt;

   (* parametrization of numeric scanner: *)
   MaxHDig* = 8;   (* maximal hexadecimal longint length *)
   MaxRExp* = 38;   (* maximal real exponent *)
   MaxLExp* = 308;   (* maximal longreal exponent *)

   (* inclusive range of parameter of standard procedure HALT: *)
   MinHaltNr* = 20;
   MaxHaltNr* = MAX(LONGINT);

   (* inclusive range of register number of procedures SYSTEM.GETREG and SYSTEM.PUTREG: *)
   MinRegNr* = 0;   (* interpretation is left to the back-end, e.g. for i386: *)
   MaxRegNr* = 31;   (* F0-F7 if second parameter is REAL or LONGREAL, R0-R7 else *)

   (* maximal value of flag used to mark interface structures: *)
   MaxSysFlag* = 0;   (* i386: only 0 is valid, not used *)

   (* maximal condition value of parameter of SYSTEM.CC: *)
   MaxCC* = -1;   (* not implemented interpretation is left to the back-end *)

   (* initialization of linkadr field in ObjDesc, must be different from any valid link address: *)
   LANotAlloc* = -1;

   (* initialization of constant address, must be different from any valid constant address: *)
   ConstNotAlloc* = -1;   (* i386: only strings are allocated *)

   (* initialization of tdadr field in StrDesc, must be different from any valid address: *)
   TDAdrUndef* = -1;

   (* maximal number of cases in a case statement: *)
   MaxCases* = 128;

   (* maximal range of a case statement (higher label - lower label ~ jump table size): *)
   MaxCaseRange* = 512;

   (* whether field leaf of pointer variable p has to be set to FALSE, when NEW(p) or SYSTEM.NEW(p, n) is used: *)

   NEWusingAdr* = FALSE;

   (* mask for the TD associated with an object instance: mask away GC information *)
   TDMask* = 0 (*0FFFFFFF8H*);   (* set to 0 to disable masking *)

   (*Configuration -- temp *)
   OptimizeSelf* = TRUE;   (* if possible, make self pointer-based in methods *)
   WarnUnsafe* = FALSE;   (* generate warnings for "unsafe" language features *)

   (* special character (< " ") returned by procedure Get, if end of text reached *)
   Eot* = 0X;
   SetLen = MAX(SET)+1;
   NumErrors = (912+SetLen) DIV SetLen * SetLen;

   PathChar = "/";

   NewFileFormat* = FALSE;

TYPE
   Reader* = POINTER TO ReaderDesc;
   ReaderDesc* = RECORD
      eot*: BOOLEAN
   END;

VAR
   MinReal*, MaxReal*: REAL;
   MinLReal*, MaxLReal*: LONGREAL;
   noerr*: BOOLEAN;   (* no error found until now *)
   curpos*, errpos*: LONGINT;   (* character and error position in source file *)
   breakpos*: LONGINT;   (* set by Mark *)
   breakpc*: LONGINT;   (*set by OPV.Init*)
   errors: ARRAY NumErrors DIV SetLen OF SET;
   errName*: ARRAY 32 OF CHAR;
   first: BOOLEAN;

   LRealPat: RECORD L, H: SYSTEM.CARD32 END;
   lastpos: LONGINT;   (* last position error in source file *)
   pat: SYSTEM.CARD32;
   inR: Reader;

(* ---------------- Abstract Reader ----------------------------- *)

PROCEDURE (r: Reader) Init*;
BEGIN r.eot := FALSE
END Init;

PROCEDURE (r: Reader) Read* (VAR ch: CHAR);
BEGIN ch := Eot; r.eot := TRUE
END Read;

PROCEDURE (r: Reader) GetPos* (VAR pos: LONGINT);
BEGIN pos := -1
END GetPos;

PROCEDURE Init* (source: Reader);
VAR i: SHORTINT;
BEGIN inR := source;
   noerr := TRUE; inR.GetPos (curpos); errpos := curpos; lastpos := curpos-10;
   FOR i := 0 TO NumErrors DIV SetLen - 1 DO errors[i] := {} END
END Init;

PROCEDURE Get* (VAR ch: CHAR);   (* read next character from source text, Eot if no more *)
BEGIN
   inR.Read (ch);
   IF inR.eot THEN ch := Eot
   ELSE INC(curpos)
   END
END Get;

(* ------------------------- Log Output ------------------------- *)

PROCEDURE LogW* (ch: CHAR);
BEGIN
END LogW;

PROCEDURE LogWStr* (s: ARRAY OF CHAR);
BEGIN
END LogWStr;

PROCEDURE LogWNum* (i, len: LONGINT);
BEGIN
END LogWNum;

PROCEDURE LogWHex* (i: LONGINT);
BEGIN
END LogWHex;

PROCEDURE LogWLn*;
BEGIN
END LogWLn;

PROCEDURE ErrorMsg (n: INTEGER);
VAR (*s: Texts.Scanner;*) ch: CHAR;  e: ARRAY 127 OF CHAR;  i: SHORTINT;
BEGIN
   IF (n >= NumErrors) OR ~((n MOD SetLen) IN errors[n DIV SetLen]) THEN
      IF (n < NumErrors) THEN INCL(errors[n DIV SetLen], n MOD SetLen) END;
(*
      Oberon.OpenScanner(s, "OP2.Errors");
      IF s.class # Texts.Inval THEN
         REPEAT Texts.Scan(s) UNTIL s.eot OR (s.class = Texts.Int) & (s.i = 0);
         WHILE ~s.eot & ((s.class # Texts.Int) OR (s.i # n)) DO Texts.Scan(s) END;
         IF ~s.eot THEN Texts.Read(s, ch);  Texts.Write(W, 9X);  i := 0;
            WHILE ~s.eot & (ch # 0DX) DO e[i] := ch; INC(i); Texts.Read(s, ch) END;
            e[i] := 0X;  LogWStr(e)
         END
      ELSE
         IF first THEN
            LogWStr("Oberon.Text - OP2.Errors not found");  LogWLn;
            first := FALSE
         END
      END
*)
   END
END ErrorMsg;

PROCEDURE Mark* (n: INTEGER; pos: LONGINT);
BEGIN
   IF n >= 0 THEN
      noerr := FALSE;
      IF (pos < lastpos) OR (lastpos + 9 < pos) OR (n>=450) & (n<=460) OR (n=155) THEN lastpos := pos;
         LogWLn; LogW(9X); LogW(9X);
         IF (n<450) OR (n>460) THEN
            LogWStr("pos"); LogWNum(pos, 7);
            IF n = 255 THEN LogWStr("  pc "); LogWHex(breakpc); breakpos := pos
            ELSIF n = 254 THEN LogWStr("  pc not found")
            ELSE LogWStr("  err"); LogWNum(n, 4); ErrorMsg(n)
            END
         ELSE LogWStr(errName)
         END
      END
(*   ELSIF warning IN parserOptions THEN
      LogWLn; LogW(9X); LogW(9X); LogWStr("pos"); LogWNum(pos, 7); LogWStr("  warning"); LogWNum(-n, 4);
      ErrorMsg(-n)*)
   END
END Mark;

PROCEDURE err* (n: INTEGER);
BEGIN Mark(n, errpos)
END err;

BEGIN
   pat := MinRealPat; SYSTEM.MOVE(SYSTEM.ADR(pat), SYSTEM.ADR(MinReal), 4);   (*i386: -3.40282346E38*)
   pat := MaxRealPat; SYSTEM.MOVE(SYSTEM.ADR(pat), SYSTEM.ADR(MaxReal), 4);   (*i386: 3.40282346E38*)
   LRealPat.L := MinLRealPatL; LRealPat.H := MinLRealPatH;
   SYSTEM.MOVE(SYSTEM.ADR(LRealPat), SYSTEM.ADR(MinLReal), 8);   (*i386: -1.7976931348623157D308*)
   LRealPat.L := MaxLRealPatL; LRealPat.H := MaxLRealPatH;
   SYSTEM.MOVE(SYSTEM.ADR(LRealPat), SYSTEM.ADR(MaxLReal), 8);   (*i386: 1.7976931348623157D308*)
   first := TRUE
END OPM.
