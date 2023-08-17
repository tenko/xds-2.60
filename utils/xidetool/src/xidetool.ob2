<* +MAIN *>
MODULE xidetool;

IMPORT SYSTEM, Printf, Out, Project, OPS, OPM, xFilePos;
IMPORT RndFile, SeqFile, RawIO, ChanConsts, TextIO, IOChan, IOResult, ProgramArgs;

TYPE
    FilePos = RndFile.FilePos;
    File = RndFile.ChanId;

TYPE
    (* Reader implementation reading from RndFile *)
    FileReader = POINTER TO RECORD (Project.ReaderDesc)
        fh: RndFile.ChanId;
        ch: CHAR;         (* last character read by ReadPrev *)
    END;

PROCEDURE (r: FileReader) Read2 (VAR ch: CHAR);
(* Read the next character from RndFile. *)
BEGIN
    RawIO.Read(r.fh, ch);
    r.eot := IOChan.ReadResult(r.fh) # IOResult.allRight;
    IF r.eot THEN
        RndFile.Close(r.fh);
    END;
END Read2;

PROCEDURE (r: FileReader) ReadFrom (pos: LONGINT);
(* Setup the reader so that the next Read call will read from position pos. *)
    VAR fpos : FilePos;
BEGIN
   ASSERT (pos >= 0, 20);
   xFilePos.IntToPos(fpos, pos);
   RndFile.SetPos(r.fh, fpos)
END ReadFrom;

PROCEDURE (r: FileReader) ReadPrev;
(* Read the previous character from the file. *)
BEGIN
    IF r.eot THEN
        r.ch := 0X
    ELSE
        RndFile.SetPos(r.fh, RndFile.NewPos(r.fh, -1, 1, RndFile.CurrentPos(r.fh)));
        RawIO.Read(r.fh, r.ch);
        IF IOChan.ReadResult(r.fh) # IOResult.allRight THEN
            r.ch := 0X
        END;
    END;
END ReadPrev;

PROCEDURE (r: FileReader) Init ();
BEGIN
   r.Init^;
   r.ch := 0X;
END Init;

PROCEDURE ParseFile* (file : File; VAR m: Project.Module);
(** Call Parse using the s file as the data source. *)
    VAR r: FileReader;
BEGIN
    NEW (r); r.Init; r.fh := file;
    Project.Parse (r, m);
END ParseFile;

PROCEDURE openFile(filename : ARRAY OF CHAR);
VAR
    m : Project.Module;
    fh : File;
    res: RndFile.OpenResults;
    n  : INTEGER;
    itm: Project.Item;

    PROCEDURE FlatListing(itm: Project.Item; kind : ARRAY OF CHAR);
        VAR n : LONGINT;
    BEGIN 
        n := 0; 
        LOOP
            IF itm = NIL THEN EXIT END;
            IF n > 0 THEN Out.String(','); Out.Ln; END;
            Printf.printf('{"name" : "%s", "kind":"%s", "start":%d, "end":%d}',
                          itm.name, kind, itm.pos, itm.endPos);
            INC (n);
            itm := itm.next
        END;
    END FlatListing;
BEGIN
    RndFile.OpenOld(fh, filename, RndFile.read + RndFile.raw, res);
    IF res # ChanConsts.opened THEN
        Out.String('['); Out.String(']'); Out.Ln;
        RETURN;
    END;
    ParseFile(fh, m);
    IF m = NIL THEN
        Out.String('['); Out.String(']'); Out.Ln;
        RETURN;
    END;
    Out.String('['); Out.Ln;
    (* IMPORT *)
    FlatListing(m.import, "namespace");
    (* TYPE *)
    IF m.type # NIL THEN Out.String(','); Out.Ln; END;
    FlatListing(m.type, "interface");
    (* CONST *)
    IF m.const # NIL THEN Out.String(','); Out.Ln; END;
    FlatListing(m.const, "constant");
    (* VAR *)
    IF m.var # NIL THEN Out.String(','); Out.Ln; END;
    FlatListing(m.var, "variable");
    (* PROCEDURE *)
    IF m.proc # NIL THEN Out.String(','); Out.Ln; END;
    FlatListing(m.proc, "function");
    Out.Ln; Out.String(']'); Out.Ln;
END openFile;

VAR
    args : SeqFile.ChanId;
    filename : ARRAY 256 OF CHAR;
BEGIN
    args := ProgramArgs.ArgChan();
    IF NOT ProgramArgs.IsArgPresent() THEN
        Out.String('['); Out.String(']'); Out.Ln;
    ELSE
        TextIO.ReadString(args, filename);
        openFile(filename);
    END;
END xidetool.