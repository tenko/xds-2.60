MODULE Project;

(* ---------------------------------------------------------------------------
 * (C) 2008 - 2010 by Alexander Iljin
 * --------------------------------------------------------------------------- *)

IMPORT OPS,OPM;--,Debug;

(** ---------------------------------------------------------------------------
  * This module provides means to parse an Oberon project and access
  * information about it: names of exported types and procedures, etc.
  * All lists are sorted by Item.name.
  * There are two ways to make a list of Items:
  * 1) using the Item.next field (cf. Add procedure) - this way has less
  *    dynamic memory overhead, so it is generally preferred, but an item can't
  *    exist in more than one such list at a time;
  * 2) using the List type (cf. AddList procedure), which is basically a list of
  *    references.
  * The functionality is generic, to make use of this module one must
  * implement a Reader extension with Read2 method, and supply some real data.
  * To work with groups of modules the Locator type must be extended to keep
  * information about the module storage location and provide Readers.
  * -------------------------------------------------------------------------- *)

TYPE
   Name* = OPS.Name;    (* name of an Item, an Oberon identifier         *)
   StringPtr* = POINTER TO OPS.String;
   NamePtr* = POINTER TO Name;

   (** Locator is an abstract type describing a Module's storage location and
     * providing the ability to check if the module was modified since the
     * last parsing. The Locator is not implemented in this module to ensure
     * portability. *)
   Locator* = POINTER TO LocatorDesc;
   LocatorDesc* = RECORD END;

   Module* = POINTER TO ModuleDesc;

   (* A general sorted linked list item *)
   Item* = POINTER TO ItemDesc;
   ItemDesc = RECORD
      name-: Name;      (* name of the item, items are sorted by name    *)
      mod-: Module;     (* Module this item is defined in                *)
      pos-: LONGINT;    (* position of this item's name in the text      *)
      endPos-: LONGINT; (* pos..endPos - scope for local variables, etc. *)
      exp-: BOOLEAN;    (* is this an exported item?                     *)
      next-: Item       (* next item in the sorted list                  *)
   END;

   (* A general sorted linked list of references to Items *)
   List* = POINTER TO RECORD
      item-: Item;      (* item reference, # NIL                         *)
      next-: List;      (* next item in the sorted reference list        *)
   END;

   (* Constant *)
   Const* = POINTER TO RECORD (ItemDesc)
      val-: OPS.String; (* constant value (as written in declaration)    *)
   END;

   (* An abstract type record *)
   Type* = POINTER TO TypeDesc;
   TypeDesc = RECORD
   END;

   (* A named type identifier declared in the source code *)
   TypeItem* = POINTER TO RECORD (ItemDesc)
      type-: Type;      (* actual type descriptor, # NIL                 *)
   END;

   (* Type reference by [qualified] name *)
   TypeRef* = POINTER TO TypeRefDesc;
   TypeRefDesc = RECORD (TypeDesc)
      parent-: Item;    (* Module or Proc containing this item           *)
      modName-: NamePtr;(* module with the referenced type               *)
      refName-: Name;   (* referenced type name                          *)
   END;

   (* Procedural type *)
   ProcType* = POINTER TO ProcTypeDesc;
   ProcTypeDesc = RECORD (TypeDesc)
      par-: OPS.String; (* parameter list as declared                    *)
   END;

   (* One-dimension array type *)
   ArrayType* = POINTER TO ArrayTypeDesc;
   ArrayTypeDesc = RECORD (TypeDesc)
      size-: StringPtr; (* array size specification, may be empty        *)
      type-: Type;      (* ARRAY OF type                                 *)
   END;

   PtrType* = POINTER TO PtrTypeDesc;
   PtrTypeDesc = RECORD (TypeDesc)
      type-: Type;      (* POINTER TO type                               *)
   END;

   (* A built-in type like INTEGER or CHAR *)
   StdType* = POINTER TO StdTypeDesc;
   StdTypeDesc = RECORD (TypeDesc)
      id-: SHORTINT;    (* index in the stdTypeIds                       *)
   END;

   (* Local variable, procedure parameter or record field *)
   Var* = POINTER TO VarDesc;
   VarDesc* = RECORD (ItemDesc)
      type-: Type       (* variable type                                 *)
   END;

   (* Procedure *)
   Proc* = POINTER TO RECORD (ItemDesc)
      parent-: Item;    (* Module or Proc containing this item           *)
      val-: OPS.String; (* parameter list as declared                    *)
      const-: Const;    (* local constants                               *)
      type-: TypeItem;  (* local types                                   *)
      var-: Var;        (* local variables and parameters of the proc    *)
      proc-: Proc;      (* local procedures                              *)
   END;

   (* Structured type, i.e. RECORD *)
   Struct* = POINTER TO RECORD (TypeDesc)
      inh-: Type;       (* inherits from this type, NIL if first         *)
      var-: Var;        (* field list declared as part of this type      *)
      proc-: Proc       (* list of procedures bound to this type         *)
   END;

   (* A module in the IMPORT list of a Module. *)
   ImportedModule* = POINTER TO RECORD (ItemDesc)
      (* Item.name = alias in the importer name space *)
      modName-: Name;   (* full name of the module                       *)
      module-: Module   (* link to the module definition, may be NIL     *)
   END;

   (* Module definition *)
   ModuleDesc = RECORD (ItemDesc)
      loc*: Locator;    (* see Locator description                       *)
      import-: ImportedModule; (* list of imported modules               *)
      const-: Const;    (* list of global constants                      *)
      type-: TypeItem;  (* list of declared global types                 *)
      var-: Var;        (* list of global variables                      *)
      proc-: Proc       (* list of procedures excluding type-bound       *)
   END;

   (* Abstract reader dumping the last symbol raw data into rawSymData *)
   Reader* = POINTER TO ReaderDesc;
   ReaderDesc* = RECORD (OPM.ReaderDesc)
      prevCh: CHAR;
   END;

   (* This procedure returns a Locator instance for the given module name, or
    * NIL if the module can't be found. *)
   GetModuleLocatorProc* = PROCEDURE (VAR modName: Name): Locator;

VAR
   sym: SHORTINT; (* last symbol received from OPS *)
   rawSymData: OPS.String; (* raw text read as the last symbol by OPS *)
   rsdPos: INTEGER; (* current index for adding new characters to rawSymData *)
   (* it is possible to capture a series of rawSymData values into capturedRsd
    * to enter capturing mode, set doCaptureRsd to TRUE *)
   capturedRsd: OPS.String; (* a series of rawSymData with a bit of pretty-print *)
   capturedRsdLen: INTEGER; (* current length of capturedRsd *)
   doCaptureRSD: BOOLEAN; (* set to TRUE to enable capturing *)
   typeRefParent: Item; (* parent Item for created TypeRef instances,
      * must be Module or Proc. Used later to find the referenced type *)
   root: Module; (* fake root for the global parsed module list *)
   currModule: Module; (* the Module currently being parsed  *)
   stdTypeIds-: ARRAY 12 OF Name; (* reserved type identifiers, e.g. INTEGER *)

PROCEDURE Add (root, i: Item);
(* Add new item i to the list. The list is sorted by name, ascending.
 * root is a fake root. The i.mod is automatically set to currModule value. *)

   -- PROCEDURE WriteItem (i: Item; next: BOOLEAN);
   -- BEGIN
      -- IF (i = NIL) OR next & (i.next = NIL) THEN
         -- Debug.WriteStr ('NIL')
      -- ELSE
         -- IF next THEN i := i.next END;
         -- Debug.WriteStr ('"'); Debug.WriteStr (i.name); Debug.WriteStr ('"')
      -- END
   -- END WriteItem;

BEGIN
   -- Debug.WriteStr ('Add (');
   -- WriteItem (list, TRUE); Debug.WriteStr (', '); WriteItem (i, FALSE);
   -- Debug.WriteStr (')'); Debug.WriteLn;
   ASSERT (i # NIL, 20);
   ASSERT (i.next = NIL, 21);
   ASSERT (i.name # '', 22);
   ASSERT (root # NIL, 23);
   ASSERT (root.name = '', 24);
   WHILE (root.next # NIL) & (i.name > root.next.name) DO
      root:= root.next;
   END;
   i.next := root.next;
   root.next := i;
   i.mod := currModule;
END Add;

PROCEDURE AddItem (root: List; item: Item);
(* Add new item i to the list. The list is sorted by name, ascending.
 * root is a fake list root. *)
VAR new: List;
BEGIN
   ASSERT (item # NIL, 20);
   ASSERT (item.name # '', 21);
   ASSERT (root # NIL, 22);
   ASSERT (root.item = NIL, 23);
   NEW (new);
   new.item := item;
   WHILE (root.next # NIL) & (item.name > root.next.item.name) DO
      root := root.next;
   END;
   new.next := root.next;
   root.next := new;
END AddItem;

PROCEDURE Find* (start: Item; VAR name: Name): Item;
(** Find an Item with the given name in the list, beginning with 'start'.
  * Return NIL if not found. *)
BEGIN
   WHILE (start # NIL) & (start.name < name) DO
      start := start.next
   END;
   IF (start # NIL) & (start.name = name) THEN
      RETURN start
   ELSE
      RETURN NIL
   END;
END Find;

PROCEDURE FindItem* (list: List; name: Name): Item;
(** Find the name in the list. Return NIL if not found. *)
BEGIN
   WHILE (list # NIL) & (list.item.name < name) DO
      list := list.next
   END;
   IF (list # NIL) & (list.item.name = name) THEN
      RETURN list.item
   ELSE
      RETURN NIL
   END;
END FindItem;

PROCEDURE EnlistItems (src: Item; root: List; exportedOnly: BOOLEAN);
(* Make a list of references to all items from src. root is a fake root of the
 * list of references. If exportedOnly = TRUE, then only enlist the items with
 * "exp" = TRUE. *)
BEGIN
   ASSERT (root # NIL, 20);
   ASSERT (root.item = NIL, 21);
   IF exportedOnly THEN
      WHILE src # NIL DO
         IF src.exp THEN
            AddItem (root, src);
         END;
         src := src.next
      END;
   ELSE
      WHILE src # NIL DO
         AddItem (root, src);
         src := src.next
      END;
   END;
END EnlistItems;

PROCEDURE AddRawSymData (VAR dst: OPS.String; VAR dstLen: INTEGER);
(* Append contents of the rawSymData to the string dst and clear rawSymData.
 * On entry and on exit the dstLen is the length of the dst string. *)
VAR i: INTEGER;
BEGIN
   i := 0;
   WHILE (i < rsdPos) & (dstLen < LEN (dst) - 1) DO
      dst [dstLen] := rawSymData [i];
      INC (dstLen); INC (i);
   END;
   dst [dstLen] := 0X;
   rawSymData := '';
   rsdPos := 0;
END AddRawSymData;

PROCEDURE AddChar (VAR dst: ARRAY OF CHAR; VAR dstLen: INTEGER; ch: CHAR);
(* Add a single ch character to the string dst. dstLen is the current length
 * of the string, new length is returned in dstLen. If there is not enough
 * room for another characted, do nothing. The dst is 0X-terminated. *)
BEGIN
   IF dstLen < LEN (dst) - 1 THEN
      dst [dstLen] := ch;
      INC (dstLen);
      dst [dstLen] := 0X;
   END;
END AddChar;

PROCEDURE AddStr (VAR dst: ARRAY OF CHAR; VAR dstLen: INTEGER; str: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
   i := 0;
   WHILE str [i] # 0X DO
      AddChar (dst, dstLen, str [i]);
      INC (i);
   END;
END AddStr;

PROCEDURE (loc: Locator) MarkAsUpdated* ();
(** Remember the current state of the guarded entity as being the most recent.
  * If the entity was not modified after this procedure is called,
  * loc.RequiresUpdate should return FALSE. *)
BEGIN (* to be implemented by a Locator extension *)
END MarkAsUpdated;

PROCEDURE (loc: Locator) RequiresUpdate* (): BOOLEAN;
(** Return TRUE if the guarded entity (e.g. a file) was modified since the
  * last call to loc.Updated. *)
BEGIN RETURN FALSE (* to be implemented by a Locator extension *)
END RequiresUpdate;

PROCEDURE (loc: Locator) SameAs* (loc2: Locator): BOOLEAN;
(** Return TRUE if loc and loc2 point to the same entity (e.g. a file). *)
BEGIN RETURN loc = loc2 (* to be implemented by a Locator extension *)
END SameAs;

PROCEDURE (loc: Locator) NewReader* (): Reader;
(** Create and return an appropriace reader for the entity. *)
VAR res: Reader;
BEGIN
   (* to be implemented by a Locator extension *)
   NEW (res); res.Init;
   RETURN res
END NewReader;

PROCEDURE (r: Reader) Init* ();
BEGIN
   r.Init^;
   r.prevCh := 0X;
END Init;

PROCEDURE (r: Reader) Read2* (VAR ch: CHAR);
BEGIN (* To be implemented by actual readers *)
END Read2;

PROCEDURE (r: Reader) Read* (VAR ch: CHAR);
(** Read the next character and add it to the rawSymData global variable.
  * If OPM.errpos = OPM.curpos - 1, then the previous character is the first in
  * the new symbol data. *)
BEGIN
   r.Read2 (ch);
   (* Make a dump copy of the text of the symbol read by the OPS scanner. *)
   IF (r.prevCh # 0X) & (rsdPos < LEN (rawSymData) - 1) THEN
      rawSymData [rsdPos] := r.prevCh;
      INC (rsdPos);
      rawSymData [rsdPos] := 0X;
   END;
   IF ~r.eot THEN
      IF OPM.errpos = OPM.curpos - 1 THEN
         IF r.prevCh # 0X THEN
            rawSymData [0] := r.prevCh; rsdPos := 1;
         ELSE
            rsdPos := 0;
         END;
         rawSymData [rsdPos] := 0X;
      END;
      r.prevCh := ch;
   ELSE
      r.prevCh := 0X
   END;
END Read;

PROCEDURE GetSym ();
(* Get the next symbol into the global 'sym' variable. *)
BEGIN
   IF doCaptureRSD THEN
      AddRawSymData (capturedRsd, capturedRsdLen);
      CASE sym OF
      |  OPS.var, OPS.array, OPS.of, OPS.pointer, OPS.to, OPS.comma,
         OPS.colon, OPS.semicolon:
            AddChar (capturedRsd, capturedRsdLen, ' ');
      ELSE
      END;
   END;
   OPS.Get (sym);
END GetSym;

PROCEDURE CheckSym (s: SHORTINT): BOOLEAN;
(* Get the next symbol and return TRUE if it is equal to s. *)
BEGIN
   GetSym;
   RETURN sym = s;
END CheckSym;

PROCEDURE ParseImport (root: ImportedModule);
VAR imp: ImportedModule;
BEGIN
   WHILE sym = OPS.ident DO
      NEW (imp);
      imp.exp := FALSE;
      imp.name := OPS.name;
      imp.pos := OPM.curpos;
      IF CheckSym (OPS.becomes) THEN
         IF CheckSym (OPS.ident) THEN
            imp.modName := OPS.name;
            GetSym
         ELSE
            imp := NIL
         END
      ELSE
         imp.modName := imp.name
      END;
      IF imp # NIL THEN imp.endPos := imp.pos + LENGTH(imp.modName); Add (root, imp) END;
      IF sym = OPS.comma THEN GetSym END;
   END;
   WHILE (sym # OPS.semicolon) & (sym # OPS.eof) DO
      GetSym
   END;
END ParseImport;

PROCEDURE SkipLanguageSpecification ();
(* Skip direct language specification, e.g. ["Modula"] or ["C"]. *)
BEGIN
   IF sym = OPS.lbrak THEN
      REPEAT
         GetSym;
      UNTIL (sym = OPS.rbrak) OR (sym = OPS.eof);
      GetSym;
   END;
END SkipLanguageSpecification;

PROCEDURE ^ ParseVar (root: Var);
PROCEDURE ^ ParseProcPar (VAR val: OPS.String; varRoot: Var);
PROCEDURE ^ GetStructType (type: Type; getLocator: GetModuleLocatorProc): Struct;

PROCEDURE ParseTypeVal (VAR type: Type);
(* Parse a type declaration either after a colon in a variable list (most
 * likely to be a name reference, but not necessarily) or after the equal sign
 * in a type declaration. The result is returned in the 'type' (created). *)
VAR
   proc: ProcType;
   arr: ArrayType;
   ptr: PtrType;
   str: Struct;
   ref: TypeRef;
   ok: BOOLEAN;

   PROCEDURE GetStdType (VAR name: Name): Type;
   VAR
      id: SHORTINT;
      res: StdType;
   BEGIN
      id := 0;
      WHILE (id < LEN (stdTypeIds)) & (stdTypeIds [id] # name) DO
         INC (id);
      END;
      IF id < LEN (stdTypeIds) THEN
         NEW (res);
         res.id := id;
         RETURN res
      ELSE
         RETURN NIL
      END;
   END GetStdType;

BEGIN
   CASE sym OF
   | OPS.procedure:
      NEW (proc);
      GetSym;
      SkipLanguageSpecification;
      IF sym = OPS.lparen THEN
         GetSym;
         ParseProcPar (proc.par, NIL);
         type := proc;
      END;
   | OPS.array:
      NEW (arr);
      REPEAT GetSym; (* TODO: read the array dimensions properly *)
      UNTIL (sym = OPS.of) OR (sym = OPS.eof);
      IF sym = OPS.of THEN
         GetSym;
         ParseTypeVal (arr.type);
         type := arr;
      END;
   | OPS.pointer:
      GetSym;
      SkipLanguageSpecification;
      IF sym = OPS.to THEN
         NEW (ptr);
         GetSym;
         ParseTypeVal (ptr.type);
         type := ptr;
      END;
   | OPS.record:
      NEW (str);
      GetSym;
      SkipLanguageSpecification;
      IF sym = OPS.lparen THEN
         GetSym;
         ParseTypeVal (str.inh);
         ok := sym = OPS.rparen;
         IF ok THEN GetSym END;
      ELSE
         ok := TRUE;
      END;
      IF ok & (sym # OPS.eof) THEN
         IF sym # OPS.end THEN
            NEW (str.var);
            ParseVar (str.var);
            IF str.var.next # NIL THEN str.var := str.var.next (Var) ELSE str.var := NIL END;
         END;
         IF sym = OPS.end THEN
            GetSym;
            type := str;
         END;
      END;
   | OPS.ident:
      type := GetStdType (OPS.name);
      IF type = NIL THEN
         NEW (ref);
         ASSERT (typeRefParent # NIL, 20);
         ASSERT ((typeRefParent IS Module) OR (typeRefParent IS Proc), 21);
         ref.parent := typeRefParent;
         ref.refName := OPS.name;
         IF CheckSym (OPS.period) & CheckSym (OPS.ident) THEN
            NEW (ref.modName);
            ref.modName^ := ref.refName;
            ref.refName := OPS.name;
            GetSym;
         END;
         type := ref;
      ELSE
         GetSym;
      END;
   ELSE
   END;
END ParseTypeVal;

PROCEDURE ParseVar (root: Var);
(* Parse variable list with type. If root = NIL, don't create the objects in memory,
 * just skip the declaration text. *)
VAR
   list: List;   (* a list of comma-separated variables *)
   var: Var;     (* new variable to add                 *)
   type: Type;   (* type of the last variable list      *)
   more: BOOLEAN;
BEGIN
   (* Language specification does not permit it, but XDS compiler sometimes
    * tolerates excess semicolons, particularily in RECORD TYPE declarations,
    * but interestingly not in formal parameters section. *)
   WHILE sym = OPS.semicolon DO
      GetSym;
   END;
   IF root # NIL THEN
      WHILE sym = OPS.ident DO
         (* get a comma-separated list of variables *)
         NEW (list);
         REPEAT
            NEW (var);
            var.name := OPS.name;
            var.pos := OPM.curpos;
            GetSym;
            SkipLanguageSpecification;
            var.exp := (sym = OPS.times) OR (sym = OPS.minus);
            IF var.exp THEN
               GetSym;
            END;
            more := sym = OPS.comma;
            IF more THEN
               GetSym;
               more := sym = OPS.ident;
            END;
            var.endPos := var.pos + LENGTH(var.name);
            Add (root, var);
            AddItem (list, var);
         UNTIL ~more;
         (* get raw type declaration *)
         IF sym = OPS.colon THEN
            type := NIL;
            GetSym;
            ParseTypeVal (type);
            IF type # NIL THEN
               (* assign the type to the variables *)
               WHILE list.next # NIL DO
                  list := list.next;
                  list.item (Var).type := type;
               END;
            END;
         END;
         WHILE sym = OPS.semicolon DO (* used to be IF, relaxed *)
            GetSym;
         END;
      END;
   ELSE
      WHILE sym = OPS.ident DO
         (* get a comma-separated list of variables *)
         REPEAT
            GetSym;
            IF (sym = OPS.times) OR (sym = OPS.minus) THEN GetSym END;
            more := sym = OPS.comma;
            IF more THEN GetSym; more := sym = OPS.ident END;
         UNTIL ~more;
         (* get raw type declaration *)
         IF sym = OPS.colon       THEN GetSym; ParseTypeVal (type) END;
         WHILE sym = OPS.semicolon DO GetSym END; (* used to be IF, relaxed *)
      END;
   END;
END ParseVar;

PROCEDURE ParseConst (root: Const);
VAR
   c   : Const;
   i   : INTEGER;
   done: BOOLEAN;
BEGIN
   WHILE sym = OPS.ident DO
      NEW (c);
      c.name := OPS.name;
      c.pos := OPM.curpos;
      GetSym;
      SkipLanguageSpecification;
      c.exp := (sym = OPS.minus) OR (sym = OPS.times);
      IF c.exp THEN GetSym END;
      IF sym = OPS.eql THEN
         c.val := ''; i := 0;
         REPEAT
            GetSym;
            done := FALSE;
            CASE sym OF
            |  OPS.times..OPS.rbrace, OPS.lparen..OPS.not, OPS.number..OPS.ident:
               AddRawSymData (c.val, i)
            ELSE
               done := TRUE
            END;
         UNTIL done;
         c.val [i] := 0X;
      ELSE
         c := NIL
      END;
      IF c # NIL THEN c.endPos := OPM.curpos;Add (root, c) END;
      IF sym = OPS.semicolon THEN GetSym END;
   END;
END ParseConst;

PROCEDURE ParseType (root: TypeItem);
(* Parse TYPE section, make a list of types. *)
VAR
   name: Name;
   pos: LONGINT;
   exp: BOOLEAN;
   type: Type;
   item: TypeItem;
BEGIN
   WHILE sym = OPS.ident DO
      name := OPS.name;
      pos := OPM.curpos;
      GetSym;
      SkipLanguageSpecification;
      exp := sym = OPS.times;
      IF exp THEN GetSym END;
      IF sym = OPS.eql THEN
         type := NIL;
         GetSym;
         ParseTypeVal (type);
         IF type # NIL THEN
            NEW (item);
            item.name := name;
            item.pos := pos;
            item.exp := exp;
            item.type := type;
            item.endPos := OPM.curpos;
            Add (root, item);
         END;
      END;
      IF sym = OPS.semicolon THEN GetSym END;
   END;
END ParseType;

PROCEDURE ParseProcPar (VAR val: OPS.String; varRoot: Var);
(* Parse a procedure parameter list. The 'sym' contains the first symbol after
 * the left parenthesis. On return 'val' will contain the declaration as is,
 * and 'varRoot' will contain the list of parameter variables. If varRoot =
 * NIL, the variable list won't be generated. *)
BEGIN
   ASSERT (~doCaptureRSD, 20);
   capturedRsdLen := 1;
   capturedRsd := '(';
   doCaptureRSD := TRUE;
   IF sym = OPS.var THEN
      GetSym;
   END;
   WHILE sym = OPS.ident DO
      ParseVar (varRoot);
      IF sym = OPS.var THEN
         GetSym;
      END;
   END;
   IF sym = OPS.rparen THEN
      GetSym;
      IF sym = OPS.colon THEN
         (* function result *)
         GetSym;
         WHILE (sym # OPS.semicolon) & (sym # OPS.eof) DO
            GetSym;
         END;
      END;
   END;
   ASSERT (doCaptureRSD, 60);
   doCaptureRSD := FALSE;
   val := capturedRsd;
   IF sym = OPS.semicolon THEN
      GetSym;
   END;
END ParseProcPar;

PROCEDURE ParseProc (root: Proc);
VAR
   p: Proc;
   method: BOOLEAN;  (* type-bound procedure? *)
   boundTo: Struct;  (* IF method *)
   forward: BOOLEAN; (* forward declaration? *)
   oldTypeRefParent: Item;

   PROCEDURE MakeFakeRoot (VAR root: Proc);
   VAR p: Proc;
   BEGIN
      NEW (p); p.name := '';
      p.next := root;
      root := p;
   END MakeFakeRoot;

   PROCEDURE ParseLocalDeclarations ();
   (* Parse local declarations until BEGIN keyword or EOF is met. *)
   BEGIN
      oldTypeRefParent := typeRefParent;
      typeRefParent := p;
      REPEAT
         CASE sym OF
         |  OPS.var      : GetSym; ParseVar   (p.var  );
         |  OPS.procedure: GetSym; ParseProc  (p.proc );
         |  OPS.type     : GetSym; ParseType  (p.type );
         |  OPS.const    : GetSym; ParseConst (p.const);
         ELSE (* something unexpected - skip it *)
            GetSym;
         END;
      UNTIL (sym = OPS.begin) OR (sym = OPS.eof);
      ASSERT (typeRefParent = p, 60);
      typeRefParent := oldTypeRefParent;
   END ParseLocalDeclarations;

BEGIN
   SkipLanguageSpecification;
   forward := sym = OPS.arrow;
   IF forward THEN
      GetSym;
   END;
   NEW (p); NEW (p.var); (* p.var is needed to parse forward declarations *)
   method := sym = OPS.lparen;
   IF method THEN
      (* this is a method *)
      GetSym;
      ParseProcPar (p.val, p.var);
      IF p.var.next # NIL THEN
         boundTo := GetStructType (p.var.next (Var).type, NIL);
      ELSE (* syntax error *)
         method := FALSE;
      END;
   END;
   IF sym = OPS.ident THEN
      ASSERT (typeRefParent # NIL, 20);
      ASSERT ((typeRefParent IS Module) OR (typeRefParent IS Proc), 21);
      p.name := OPS.name;
      p.pos := OPM.curpos;
      p.parent := typeRefParent;
      p.val := '';
      GetSym;
      p.exp := (sym = OPS.times) OR (sym = OPS.minus);
      IF p.exp THEN
         GetSym;
      END;
      IF sym = OPS.lparen THEN
         GetSym;
         ParseProcPar (p.val, p.var);
      END;
      IF ~forward THEN
         NEW (p.const); NEW (p.type); NEW (p.proc);
         IF (sym # OPS.begin) & (sym # OPS.eof) THEN
            ParseLocalDeclarations;
         END;
         IF sym = OPS.begin THEN
            (* parse the body *)
            GetSym; (* wait for "END ident" *)
            REPEAT
               WHILE ~((sym = OPS.end) OR (sym = OPS.eof)) DO GetSym END;
               GetSym;
            UNTIL (sym = OPS.ident) OR (sym = OPS.eof);
            p.endPos := OPM.curpos;
            IF sym = OPS.ident THEN
               (* the procedure is parsed successfully *)
               IF p.var.next   # NIL THEN p.var   := p.var.next   (Var     ) ELSE p.var   := NIL END;
               IF p.const.next # NIL THEN p.const := p.const.next (Const   ) ELSE p.const := NIL END;
               IF p.type.next  # NIL THEN p.type  := p.type.next  (TypeItem) ELSE p.type  := NIL END;
               IF p.proc.next  # NIL THEN p.proc  := p.proc.next  (Proc    ) ELSE p.proc  := NIL END;
               IF method THEN
                  IF boundTo # NIL THEN
                     (* bind method to the boundTo type *)
                     ASSERT (p # NIL, 61);
                     MakeFakeRoot (boundTo.proc);
                     Add (boundTo.proc, p);
                     boundTo.proc := boundTo.proc.next (Proc);
                  END;
               ELSE
                  Add (root, p); (* add the procedure to the root list *)
               END;
               IF CheckSym (OPS.semicolon) THEN (* skip trailing semicolon *)
                  GetSym;
               END;
            END;
         END;
      END; (* ~forward *)
   END;
END ParseProc;

PROCEDURE ParseModule (m: Module);
BEGIN
   ASSERT (typeRefParent = NIL, 20);
   typeRefParent := m;
   ASSERT (currModule = NIL, 21);
   currModule := m;
   NEW (m.const); NEW (m.import); NEW (m.type); NEW (m.var); NEW (m.proc);
   REPEAT
      CASE sym OF
      |  OPS.const    : GetSym; ParseConst  (m.const);
      |  OPS.import   : GetSym; ParseImport (m.import);
      |  OPS.procedure: GetSym; ParseProc   (m.proc);
      |  OPS.type     : GetSym; ParseType   (m.type);
      |  OPS.var      : GetSym; ParseVar    (m.var);
      ELSE
         GetSym;
      END;
   UNTIL sym = OPS.eof;
   IF m.const.next  # NIL THEN m.const  := m.const.next  (Const)          ELSE m.const  := NIL END;
   IF m.import.next # NIL THEN m.import := m.import.next (ImportedModule) ELSE m.import := NIL END;
   IF m.type.next   # NIL THEN m.type   := m.type.next   (TypeItem)       ELSE m.type   := NIL END;
   IF m.var.next    # NIL THEN m.var    := m.var.next    (Var)            ELSE m.var    := NIL END;
   IF m.proc.next   # NIL THEN m.proc   := m.proc.next   (Proc)           ELSE m.proc   := NIL END;
   ASSERT (typeRefParent = m, 60);
   typeRefParent := NIL;
   ASSERT (currModule = m, 61);
   currModule := NIL;
END ParseModule;

PROCEDURE Parse* (in: OPM.Reader; VAR m: Module);
(** Parse the data from in and create the Module structure. *)
BEGIN
   m := NIL;
   OPM.Init (in); OPS.Init;
   IF CheckSym (OPS.module) & CheckSym (OPS.ident) THEN
      NEW (m); m.name := OPS.name; m.pos := OPM.curpos;
      IF CheckSym (OPS.semicolon) THEN
         ParseModule (m);
         NEW (m.loc);
      ELSE
         m := NIL;
      END
   END
END Parse;

PROCEDURE Length (VAR s: ARRAY OF CHAR): LONGINT;
(* Return length of the string s without the terminating 0X. *)
VAR res: LONGINT;
BEGIN
   res := 0;
   WHILE s [res] # 0X DO
      INC (res)
   END;
   RETURN res
END Length;

PROCEDURE NamesLength* (list: List): LONGINT;
(* Return length of a string that would fit all the names separated by spaces
 * plus one for the terminating 0X. *)
VAR res: LONGINT;
BEGIN
   IF list = NIL THEN
      res := 1
   ELSE
      res := 0;
      REPEAT
         INC (res, Length (list.item.name));
         INC (res);
         list := list.next;
      UNTIL list = NIL;
   END;
   RETURN res
END NamesLength;

PROCEDURE NamesToString* (list: List; VAR s: ARRAY OF CHAR);
(** Copy all names from the list's Items to string s, space-separated *)
VAR i,c: LONGINT;
BEGIN
   IF list = NIL THEN
      s [0] := 0X
   ELSE
      i := 0;
      REPEAT
         c := 0;
         ASSERT (list.item # NIL, 100);
         ASSERT (list.item.name [0] # 0X, 101);
         WHILE list.item.name [c] # 0X DO
            s [i] := list.item.name [c];
            INC (c); INC (i);
         END;
         s [i] := ' '; INC (i);
         list := list.next
      UNTIL list = NIL;
      DEC (i); s [i] := 0X;
   END;
END NamesToString;

PROCEDURE TypeToDeclString* (type: Type; currMod, itemMod: Module; VAR s: ARRAY OF CHAR);
(** Fill s with the type declaration of type: 'POINTER TO ARRAY OF ...', etc.
  * currMod is the module in which the type hint is to be displayed;
  * itemMod is the module in which the item, whose type is being examined, is declared. *)
VAR
   len: INTEGER;
   next: Type;

   PROCEDURE AddTypeRef (tref: TypeRef);
   VAR
      i: Item;
      nm: Name;
   BEGIN
      nm := '';
      IF tref.modName # NIL THEN
         nm := tref.modName^;
         IF itemMod # NIL THEN
            i := Find (itemMod.import, tref.modName^);
            IF i # NIL THEN
               nm := i (ImportedModule).modName;
            END;
         END;
      ELSIF currMod # itemMod THEN
         nm := itemMod.name;
      END;
      IF nm # '' THEN
         AddStr (s, len, nm);
         AddChar (s, len, '.');
      END;
      AddStr (s, len, tref.refName);
   END AddTypeRef;

BEGIN (* TypeToDeclString *)
   s [0] := 0X;
   len := 0;
   WHILE type # NIL DO
      next := NIL;
      WITH type: TypeRef DO
         AddTypeRef (type);
      | type: StdType DO
         AddStr (s, len, stdTypeIds [type.id]);
      | type: ProcType DO
         AddStr (s, len, 'PROCEDURE ');
         AddStr (s, len, type.par);
      | type: ArrayType DO
         AddStr (s, len, 'ARRAY ');
         IF type.size # NIL THEN
            AddStr (s, len, type.size^);
         END;
         AddStr (s, len, 'OF ');
         next := type.type;
      | type: PtrType DO
         AddStr (s, len, 'POINTER TO ');
         next := type.type;
      | type: Struct DO
         AddStr (s, len, 'RECORD ');
         IF (type.inh # NIL) & (type.inh IS TypeRef) THEN
            AddChar (s, len, '(');
            AddTypeRef (type.inh (TypeRef));
            AddStr (s, len, ') ');
         END;
         IF type.var # NIL THEN
            AddStr (s, len, '... ');
         END;
         AddStr (s, len, 'END');
      END;
      type := next;
   END;
END TypeToDeclString;

PROCEDURE GetModule* (VAR name: Name; loc: Locator): Module;
(** Find the module with name "name" in the global parsed module list and set
  * "loc" as its Locator (if loc = NIL, then delete the module). If the
  * requested module is found in the list, then update it and return the
  * result. If the module was not found, then try to parse it and add to the
  * global list on success.
  * In any case the result is either NIL or the most recent version of the
  * module, and the global list is updated. *)
VAR
   prev, res, m: Module;
   r: Reader;
BEGIN
   ASSERT (name # '', 20);
   res := root;
   REPEAT
      prev := res;
      IF res.next # NIL THEN
         res := res.next (Module);
      ELSE
         res := NIL;
      END;
   UNTIL (res = NIL) OR (res.name >= name);
   IF (res # NIL) & (res.name > name) THEN
      res := NIL
   END;
   IF (res = NIL) OR res.loc.RequiresUpdate () OR (loc = NIL) OR ~loc.SameAs (res.loc) THEN
      m := NIL;
      IF loc # NIL THEN
         r := loc.NewReader ();
         IF r # NIL THEN
            Parse (r, m);
            IF m.name # name THEN
               m := NIL;
            END;
         END;
      END;
      IF res # NIL THEN (* replacing an existing module *)
         IF m # NIL THEN (* insert m in place of res *)
            prev.next := m;
            m.next := res.next;
         ELSE (* nothing to replace with - delete res *)
            prev.next := res.next;
         END;
      ELSE (* adding a new module *)
         IF m # NIL THEN
            Add (root, m);
         END;
      END;
      IF m # NIL THEN
         loc.MarkAsUpdated;
         m.loc := loc;
      END;
      res := m;
   END;
   RETURN res
END GetModule;

PROCEDURE GetStructType (type: Type; getLocator: GetModuleLocatorProc): Struct;
(* Find out if the type is a RECORD at some point, e.g. a POINTER TO RECORD or
 * an ARRAY OF RECORD, etc. If type IS Struct, then return type. If there is
 * no Struct in the type chain, return NIL. *)
VAR next: Type;

   PROCEDURE GetReferencedType (ref: TypeRef): Type;
   (* The Type Guard Bug: if you apply a type guard directly to a procedure
    * result, the procedure will be called twice. *)
   VAR
      parent, next, i: Item;
      res: Type;
      mod: Module;
   BEGIN
      res := NIL;
      IF ref.modName = NIL THEN
         (* the referenced type is in the same module with the reference *)
         i := NIL;
         parent := ref.parent;
         WHILE (i = NIL) & (parent # NIL) DO
            next := NIL;
            WITH parent: Proc DO
               i := Find (parent.type, ref.refName); (* beware of the Type Guard Bug *)
               IF i = NIL THEN
                  next := parent.parent;
               END;
            | parent: Module DO
               i := Find (parent.type, ref.refName); (* beware of the Type Guard Bug *)
            END;
            parent := next;
         END;
         IF i # NIL THEN
            res := i (TypeItem).type;
         END;
      ELSIF getLocator # NIL THEN
         (* the referenced type is in a different module; find the parent Module *)
         i := ref.parent;
         WHILE ~(i IS Module) DO
            i := i (Proc).parent;
         END;
         (* find the aliased module name *)
         i := Find (i (Module).import, ref.modName^);
         IF i # NIL THEN
            mod := GetModule (i (ImportedModule).modName, getLocator (i (ImportedModule).modName));
            IF mod # NIL THEN
               (* find the requested type *)
               i := Find (mod.type, ref.refName); (* beware of the Type Guard Bug *)
               IF i # NIL THEN
                  res := i (TypeItem).type;
               END;
            END;
         END;
      END;
      RETURN res
   END GetReferencedType;

BEGIN
   WHILE (type # NIL) & ~(type IS Struct) DO
      next := NIL;
      WITH type: PtrType   DO next := type.type;
      |    type: ArrayType DO next := type.type;
      |    type: TypeRef   DO next := GetReferencedType (type);
      |    type: ProcType  DO next := NIL; (* not supported yet *)
      |    type: StdType   DO next := NIL; (* do nothing *)
      END;
      type := next;
   END;
   IF type = NIL THEN
      RETURN NIL
   ELSE
      RETURN type (Struct)
   END;
END GetStructType;

PROCEDURE RemoveDuplicateItems (root: List);
BEGIN
   WHILE root.next # NIL DO
      IF root.item = root.next.item THEN
         root.next := root.next.next;
      ELSE
         root := root.next
      END;
   END;
END RemoveDuplicateItems;

PROCEDURE ListSubitems* (itm: Item; currMod: Module; getLocator: GetModuleLocatorProc; pos: LONGINT; VAR list: List);
(** List all subitems of the item 'itm', if any. Subitem is anything that can
  * be accessed by writing a period after the item's name in the source file,
  * e.g. a list of fields and methods if (itm IS Struct), etc.
  * currMod determines if only the exported items should be enlisted.
  * pos is the caret position within module source text; if pos > 0, the list
  * should contain local variables of procedures accessible at that point. *)
VAR
   exportedOnly: BOOLEAN;
   mod: Module;
   type, next: Type;
   typeItem: TypeItem;
   struct: Struct;

   PROCEDURE EnlistProcSubitems (proc: Proc);
   BEGIN
       WHILE proc # NIL DO
         IF (proc.pos < pos) & (pos < proc.endPos) THEN
            EnlistItems (proc.const, list, FALSE);
            EnlistItems (proc.type , list, FALSE);
            EnlistItems (proc.var  , list, FALSE);
            EnlistItems (proc.proc , list, FALSE);
            EnlistProcSubitems (proc.proc)
         END;
         IF proc.next # NIL THEN
            proc := proc.next (Proc);
         ELSE
            proc := NIL;
         END;
      END;
   END EnlistProcSubitems;

BEGIN
   IF itm # NIL THEN
      NEW (list); list.item := NIL;
      WITH itm: Var DO
         type := itm.type;
         REPEAT
            (* the item is variable, find out if it is a RECORD at some point,
             * e.g. a POINTER TO RECORD or an ARRAY OF RECORD, etc. *)
            type := GetStructType (type, getLocator);
            IF type # NIL THEN
               (* it is a RECORD; enlist type.var, recurse to type.inh *)
               WITH type: Struct DO
                  IF type.var # NIL THEN
                     EnlistItems (type.var, list, type.var.mod # currMod)
                  END;
                  IF type.proc # NIL THEN
                     EnlistItems (type.proc, list, type.proc.mod # currMod)
                  END;
                  next := type.inh;
               END;
               type := next;
            END;
         UNTIL type = NIL;
      | itm: Module DO
         exportedOnly := currMod # itm;
         EnlistItems (itm.const, list, exportedOnly);
         EnlistItems (itm.type , list, exportedOnly);
         EnlistItems (itm.var  , list, exportedOnly);
         EnlistItems (itm.proc , list, exportedOnly);
         IF ~exportedOnly THEN
            EnlistItems (itm.import, list, FALSE);
            IF pos > 0 THEN
               (* enlist local items of a top-level procedure *)
               EnlistProcSubitems (itm.proc);
               (* enlist local items of a type-bound procedure *)
               typeItem := itm.type;
               WHILE typeItem # NIL DO
                  struct := GetStructType (typeItem.type, NIL);
                  IF struct # NIL THEN
                     EnlistProcSubitems (struct.proc);
                  END;
                  IF typeItem.next = NIL THEN
                     typeItem := NIL;
                  ELSE
                     typeItem := typeItem.next (TypeItem);
                  END;
               END;
               (* local items of a method may get listed multiple times, if
                * there are several global named types referring to the
                * corresponding RECORD, e.g. POINTER TO the same type or
                * ARRAY OF it, etc. *)
               RemoveDuplicateItems (list);
            END;
         END;
      | itm: ImportedModule DO
         mod := GetModule (itm.modName, getLocator (itm.modName));
         IF mod # NIL THEN
            ListSubitems (mod, currMod, getLocator, 0, list.next);
         END;
      ELSE
      END;
      list := list.next;
   ELSE
      list := NIL;
   END;
END ListSubitems;

PROCEDURE RemoveInheritedMethods* (VAR list: List);
(** Remove inherited type-bound procedures from the 'from' list, leaving only
  * the last one of each chain. *)
VAR prev, curr: List;
BEGIN
   (* The implementation assumes that inherited type-bound procedures are
    * listed in the order from the most abstract to the last extension,
    * because of the way ListSubitems traverses inherited types and the way
    * AddItem adds a duplicate item closer to the beginning of the list. *)
   IF list # NIL THEN
      prev := NIL; curr := list;
      WHILE curr.next # NIL DO
         IF (curr.item IS Proc) & (curr.next.item IS Proc)
            & (curr.item (Proc).name = curr.next.item (Proc).name)
         THEN
            (* curr.next is a method inherited from curr *)
            IF prev # NIL THEN
               (* drop the curr by attaching curr.next to prev *)
               prev.next := curr.next;
            ELSE
               (* there is no previous element - move the list head *)
               list := list.next;
            END;
         ELSE
            prev := curr;
         END;
         curr := curr.next;
      END;
   END;
END RemoveInheritedMethods;

BEGIN
   NEW (root); root.name := ''; root.next := NIL;
   (* XDS Oberon keywords*)
   stdTypeIds [0] := 'INTEGER';
   stdTypeIds [1] := 'CHAR';
   stdTypeIds [2] := 'BOOLEAN';
   stdTypeIds [3] := 'SHORTINT';
   stdTypeIds [4] := 'LONGINT';
   stdTypeIds [5] := 'SET';
   stdTypeIds [6] := 'REAL';
   stdTypeIds [7] := 'LONGREAL';
   stdTypeIds [8] := 'LONGLONGREAL';
   (* XDS Modula keywords*)
   stdTypeIds [9] := 'CARDINAL';
   stdTypeIds [10] := 'SHORTCARD';
   stdTypeIds [11] := 'LONGCARD';
END Project.
