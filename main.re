open Core;

/* Stream */
type stream = {
  mutable chr: option char,
  mutable line_num: int,
  chan: in_channel
};

let open_stream file => {chr: None, line_num: 1, chan: open_in file};

let close_stream stm => close_in stm.chan;

let read_char stm =>
  switch stm.chr {
  | None =>
    let c = input_char stm.chan;
    if (c == '\n') {
      let _ = stm.line_num = stm.line_num + 1;
      c
    } else {
      c
    }
  | Some c =>
    stm.chr = None;
    c
  };

let unread_char stm c => stm.chr = Some c;

/* character */
let is_digit c => Char.code c >= Char.code '0' && Char.code c <= Char.code '9';

let is_alpha c =>
  Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z' ||
  Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z';

/* token */
type token =
  | Begin
  | End
  | Identifier string
  | Read
  | Write
  | Literal int
  | Assign
  | LeftParen
  | RightParen
  | AddOp
  | SubOp
  | Comma
  | Semicolon;

type scanner = {
  mutable last_token: option token,
  stm: stream
};

exception Syntax_error string;

let syntax_error s msg => raise (Syntax_error (msg ^ " on line " ^ string_of_int s.stm.line_num));

/* skip all blank and new line characters */
let rec skip_blank_chars stm => {
  let c = read_char stm;
  if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
    skip_blank_chars stm
  } else {
    unread_char stm c
  };
  ()
};

let scan s => {
  let stm = s.stm;
  let c = read_char stm;
  let rec scan_iden acc => {
    let nc = read_char stm;
    if (is_alpha nc || is_digit nc || nc == '_') {
      scan_iden (acc ^ Char.escaped nc)
    } else {
      let _ = unread_char stm nc;
      let lc = String.lowercase acc;
      if (lc == "begin") {
        Begin
      } else if (lc == "end") {
        End
      } else if (lc == "read") {
        Read
      } else if (
        lc == "write"
      ) {
        Write
      } else {
        Identifier acc
      }
    }
  };
  let rec scan_lit acc => {
    let nc = read_char stm;
    if (is_digit nc) {
      scan_lit (acc ^ Char.escaped nc)
    } else {
      let _ = unread_char stm nc;
      Literal (int_of_string acc)
    }
  };
  if (is_alpha c) {
    scan_iden (Char.escaped c)
  } else if (is_digit c) {
    scan_lit (Char.escaped c)
  } else if (
    c == '+'
  ) {
    AddOp
  } else if (
    c == '-'
  ) {
    SubOp
  } else if (
    c == ','
  ) {
    Comma
  } else if (
    c == ';'
  ) {
    Semicolon
  } else if (
    c == '('
  ) {
    LeftParen
  } else if (
    c == ')'
  ) {
    RightParen
  } else if (
    c == ':' && read_char stm == '='
  ) {
    Assign
  } else {
    syntax_error s "couldn't identify the token"
  }
};

let new_scanner stm => {last_token: None, stm};

let match_next s =>
  switch s.last_token {
  | None =>
    let _ = skip_blank_chars s.stm;
    scan s
  | Some tn =>
    s.last_token = None;
    tn
  };

let match_token s t => match_next s == t;

let next_token s =>
  switch s.last_token {
  | None =>
    skip_blank_chars s.stm;
    let t = scan s;
    s.last_token = Some t;
    t
  | Some t => t
  };

/* Code generation */
type generator = {
  vars: Hashtbl.t string int,
  file: string,
  chan: out_channel
};

let new_generator file => {
  let fs = Filename.chop_extension file ^ ".s";
  {vars: Hashtbl.create 100, file: fs, chan: open_out fs}
};

let close_generator g => close_out g.chan;

let gen g v => {
  output_string g.chan v;
  output_string g.chan "\n"
};

let bottom_var _ g =>
  Hashtbl.fold
    (
      fun _ v c =>
        if (v >= c) {
          v + 4
        } else {
          c
        }
    )
    g.vars
    0;

let empty_var s g i => bottom_var s g + 4 * (i - 1);

let var_addr s g v =>
  if (String.length v > 6 && String.sub v 0 6 == "__temp") {
    let i = String.sub v 6 (String.length v - 6);
    "[esp+" ^ i ^ "]"
  } else {
    try ("[esp+" ^ string_of_int (Hashtbl.find g.vars v) ^ "]") {
    | Not_found => syntax_error s ("identifier " ^ v ^ " not defined")
    }
  };

let var s g v => "dword" ^ var_addr s g v;

let temp_var s g i => Identifier ("__temp" ^ string_of_int (empty_var s g i));

let is_alloc_var _ g v => Hashtbl.mem g.vars v;

let alloc_var s g v =>
  if (is_alloc_var s g v) {
    var s g v
  } else {
    let _ = Hashtbl.replace g.vars v (empty_var s g 1);
    var s g v
  };

let token_var s g v =>
  switch v {
  | Identifier i => var s g i
  | _ => syntax_error s "identifier expected"
  };

let op g opcode a => gen g ("    " ^ opcode ^ "  " ^ a);

let op2 g opcode a b => gen g ("    " ^ opcode ^ "  " ^ a ^ ", " ^ b);

let push g a => op g "push" a;

let generate_begin _ g =>
  gen
    g
    "extern printf\nextern scanf\n\nsection .data\n    inf: db '%d', 0\n    ouf: db '%d', 10, 0\n\nsection .text\n    global main\n\nmain:\n    sub   esp, 1000";

let generate_end _ g =>
  gen g "    add   esp, 1000\nexit:\n    mov  eax, 1 ; sys_exit\n    mov  ebx, 0\n    int  80h";

let generate_read s g id =>
  switch id {
  | Identifier i =>
    op2 g "lea" "eax" (var_addr s g i);
    push g "eax";
    push g "inf";
    op g "call" "scanf";
    op2 g "add " "esp" "8"
  | _ => syntax_error s "generate read called with invalid argument"
  };

let rec generate_reads s g => List.iter (generate_read s g);

let generate_write s g id =>
  switch id {
  | Identifier i =>
    push g (var s g i);
    push g "ouf";
    op g "call" "printf";
    op2 g "add " "esp" "8"
  | _ => syntax_error s "generate write called with invalid argument"
  };

let generate_copy s g a b =>
  switch a {
  | Identifier i =>
    switch b {
    | Identifier i2 =>
      op2 g "mov " "eax" (var s g i2);
      op2 g "mov " (var s g i) "eax"
    | Literal l => op2 g "mov " (var s g i) (string_of_int l)
    | _ => syntax_error s "generate copy called with invalid argument"
    }
  | _ => syntax_error s "generate copy called with invalid argument"
  };

let generate_assign s g a b =>
  switch a {
  | Identifier i =>
    let _ = alloc_var s g i;
    generate_copy s g a b
  | _ => syntax_error s "generate assign called with invalid argument"
  };

let generate_add s g d id1 id2 =>
  switch (id1, id2) {
  | (Identifier i1, Identifier i2) =>
    let v = temp_var s g d;
    let vi = token_var s g v;
    let _ = generate_copy s g v id1;
    let _ = op2 g "add " vi (var s g i2);
    v
  | (Identifier i1, Literal l2) =>
    let v = temp_var s g d;
    let vi = token_var s g v;
    let _ = generate_copy s g v id1;
    let _ = op2 g "add " vi (string_of_int l2);
    v
  | _ => syntax_error s "generate exp called with invalid argument"
  };

let generate_sub s g d id1 id2 =>
  switch (id1, id2) {
  | (Identifier i1, Identifier i2) =>
    let v = temp_var s g d;
    let vi = token_var s g v;
    let _ = generate_copy s g v id1;
    let _ = op2 g "sub " vi (var s g i2);
    v
  | (Identifier i1, Literal l2) =>
    let v = temp_var s g d;
    let vi = token_var s g v;
    let _ = generate_copy s g v id1;
    let _ = op2 g "sub " vi (string_of_int l2);
    v
  | (Literal l1, Identifier i2) =>
    let v = temp_var s g d;
    let vi = token_var s g v;
    let _ = generate_copy s g v id1;
    let _ = op2 g "sub " vi (var s g i2);
    v
  | _ => syntax_error s "generate exp called with invalid argument"
  };

let identifiers s => {
  let rec idens ids =>
    switch (next_token s) {
    | Identifier i =>
      let _ = match_next s;
      let n = next_token s;
      if (n == Comma) {
        let _ = match_token s Comma;
        idens [Identifier i, ...ids]
      } else {
        idens [Identifier i, ...ids]
      }
    | _ => ids
    };
  idens []
};

let addop s g d l r =>
  switch (l, r) {
  | (Literal l1, Literal l2) => Literal (l1 + l2)
  | (Identifier i1, Literal l2) => generate_add s g d l r
  | (Literal l1, Identifier i2) => generate_add s g d r l
  | _ => syntax_error s "expected literal or identifier for add operation"
  };

let subop s g d l r =>
  switch (l, r) {
  | (Literal l1, Literal l2) => Literal (l1 - l2)
  | (Identifier i1, Literal l2) => generate_sub s g d l r
  | (Literal l1, Identifier i2) => generate_sub s g d l r
  | _ => syntax_error s "expected literal or identifier for sub operation"
  };

let read s g =>
  if (match_token s Read) {
    if (match_token s LeftParen) {
      let ids = identifiers s;
      if (ids == []) {
        syntax_error s "read statement expects comma seperated identifier(s)"
      } else if (
        match_token s RightParen
      ) {
        let _ = generate_reads s g (List.rev ids);
        true
      } else {
        syntax_error s "right paren expected in read statement"
      }
    } else {
      syntax_error s "left paren expected in read statement"
    }
  } else {
    syntax_error s "read statement expected"
  };

let rec expression s g d => {
  let primary s =>
    switch (next_token s) {
    | LeftParen =>
      let _ = match_token s LeftParen;
      let e = expression s g (d + 1);
      if (match_token s RightParen) {
        Some e
      } else {
        syntax_error s "right paren expected in expression"
      }
    | Identifier i =>
      let _ = match_token s (Identifier i);
      Some (Identifier i)
    | Literal l =>
      let _ = match_token s (Literal l);
      Some (Literal l)
    | _ => None
    };
  let lp = primary s;
  switch lp {
  | Some l =>
    switch (next_token s) {
    | AddOp =>
      let _ = match_token s AddOp;
      addop s g d l (expression s g (d + 1))
    | SubOp =>
      let _ = match_token s SubOp;
      subop s g d l (expression s g (d + 1))
    | _ => l
    }
  | None => syntax_error s "literal or identifier expected"
  }
};

let write s g => {
  let rec expressions c => {
    let e = expression s g 1;
    if (
      switch e {
      | Identifier _ =>
        let _ = generate_write s g e;
        true
      | Literal _ =>
        let _ = generate_write s g e;
        true
      | _ => false
      }
    ) {
      if (next_token s == Comma) {
        let _ = match_token s Comma;
        expressions (c + 1)
      } else {
        c + 1
      }
    } else {
      c
    }
  };
  if (match_token s Write) {
    if (match_token s LeftParen) {
      if (expressions 0 > 0) {
        if (match_token s RightParen) {
          true
        } else {
          syntax_error s "right paren expected in write statement"
        }
      } else {
        syntax_error s "write statement expected atleast one expression"
      }
    } else {
      syntax_error s "left paren expected in write statement"
    }
  } else {
    syntax_error s "write statement expected"
  }
};

let assignment s g => {
  let id = match_next s;
  switch id {
  | Identifier i =>
    if (match_token s Assign) {
      let new_var =
        if (is_alloc_var s g i) {
          0
        } else {
          1
        };
      let id2 = expression s g (1 + new_var);
      switch id2 {
      | Literal l2 =>
        let _ = generate_assign s g id id2;
        true
      | Identifier i2 =>
        let _ = generate_assign s g id id2;
        true
      | _ => syntax_error s "literal or identifier expected"
      }
    } else {
      syntax_error s "assign symbol expected"
    }
  | _ => syntax_error s "identifier expected"
  }
};

let statement s g => {
  let t = next_token s;
  if (
    switch t {
    | Read => read s g
    | Write => write s g
    | Identifier i => assignment s g
    | _ => false
    }
  ) {
    if (match_token s Semicolon) {
      true
    } else {
      syntax_error s "statement must end with semicolon"
    }
  } else {
    false
  }
};

let rec statements s g =>
  if (statement s g) {
    statements s g
  } else {
    ()
  };

let program s g =>
  if (match_token s Begin) {
    let _ = generate_begin s g;
    let _ = statements s g;
    if (match_token s End) {
      let _ = generate_end s g;
      ()
    } else {
      syntax_error s "program should end with end keyword"
    }
  } else {
    syntax_error s "program should start with begin keyword"
  };

let parse stm g => {
  let s = new_scanner stm;
  try (program s g) {
  | End_of_file => syntax_error s "program reached end of file before end keyword"
  }
};

/* Compiling */
let compile file =>
  try {
    let g = new_generator file;
    let stm = open_stream file;
    let out = Filename.chop_extension file;
    parse stm g;
    close_stream stm;
    close_generator g;
    let _ = Sys.command ("nasm -f elf " ^ g.file);
    let _ = Sys.command ("gcc -o " ^ out ^ " " ^ out ^ ".o");
    ()
  } {
  | Syntax_error e =>
    Format.printf "syntax error: %s\n" e;
    Format.print_flush ()
  | Sys_error _ => print_string "no such file found\n"
  };

/* Command line util help */
let help () => print_string "micro <file>\n";

let () =
  if (Array.length Sys.argv == 1) {
    help ()
  } else {
    let file = Sys.argv.(1);
    Format.printf "compiling %s\n" file;
    Format.print_flush ();
    compile file
  };
