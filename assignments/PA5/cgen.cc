
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
Symbol curr_class_name, dispatch_class_name, curr_type;
std::map<Symbol, MethodList*> class_method_list_map;
std::map<Symbol, AttrList*> class_attr_list_map;
SymbolTable<Symbol, int> env;
int func_obj_num, label_num;
method_class *curr_method = nullptr;
#define ENABLE_DEBUG 1

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);


// code generation help function
int get_func_offset(Symbol class_name, Symbol func_name) {
  if (class_name == SELF_TYPE) {
    class_name = curr_class_name;
  }

  MethodList *list = class_method_list_map[class_name];
  ITERATE_LIST_REVERSE(list) {
    Method m = (*list)[i];
    if (func_name == m.second) {
      return i;
    }
  }
  return -1;
}

int get_attr_offset(Symbol attr_name) {
  if (dispatch_class_name == SELF_TYPE) {
    dispatch_class_name = curr_class_name;
  }

  AttrList *list = class_attr_list_map[dispatch_class_name];
  ITERATE_LIST(list) {
    Attr a = (*list)[i];
    if (attr_name == a.first) {
      return i + 3;
    }
  }
  return -1;
}

int get_env_offset(Symbol name) {
  int *p = env.lookup(name);
  if (p == nullptr) {
    return -1;
  } else {
    return *p;
  }
}

int get_formal_offset(Symbol name) {
  if (curr_method == nullptr) { // in code_init do not have current method
    return -1;
  }
  ITERATE_LIST_NODE(curr_method->formals) {
    auto f = dynamic_cast<formal_class*>(curr_method->formals->nth(i));
    if (f->name == name)
      return (curr_method->formals->len()-i) + 2;
  }
  return -1;
}

int symbol_in_method_list(Symbol symbol, MethodList &method_list) {
  for (int i = 0; i < method_list.size(); i++) {
    if (method_list[i].second == symbol) {
      return i;
    }
  }
  return -1;
}

int symbol_in_attr_list(Symbol symbol, AttrList &attr_list) {
  for (int i = 0; i < attr_list.size(); i++) {
    if (attr_list[i].first == symbol) {
      return i;
    }
  }
  return -1;
}

bool basic_type(Symbol type) {
  if (type == Int ||
      type == Bool ||
      type == Str) {
    return true;
  } else {
    return false;
  }
}

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_load_intval(char *dest_reg, char *source_reg, ostream& s) {
  emit_load(dest_reg, DEFAULT_OBJFIELDS, source_reg, s);
}

static void emit_store_intval(char *source_reg, char *dest_reg, ostream& s) {
  emit_store(source_reg, DEFAULT_OBJFIELDS, dest_reg, s);
}

static void emit_load_boolval(char *dest_reg, char *source_reg, ostream& s) {
  emit_load(dest_reg, DEFAULT_OBJFIELDS, source_reg, s);
}

static void emit_store_boolval(char *source_reg, char *dest_reg, ostream& s) {
  emit_store(source_reg, DEFAULT_OBJFIELDS, dest_reg, s);
}

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bnez(char *source, int label, ostream &s)
{
  s << BNEZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char *reg, ostream& str) {
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

static void emit_delete_env(ostream& str) {
  emit_addiu(SP, SP, 4, str);
  func_obj_num--;
  env.exitscope();
}

static void emit_add_env(Symbol name, ostream& str) {
  env.enterscope();
  emit_push(ACC, str);
  env.addid(name, new int(func_obj_num++));
}

static void emit_eq(char *reg, char *s1, char *s2, ostream& str) {
  str << SEQ << reg << " " << s1 << " " << s2 << endl;
}


//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_alloc_obj(Symbol name, ostream &s) {
  s << LA << ACC << " " << name << PROTOBJ_SUFFIX << endl;
  s << JAL << "Object.copy" << endl;
}

#define LOAD_TAB(TAB)                     \
  emit_load(ACC, 0, SELF, s);             \
  s << LA << T1 << " " << TAB << endl;    \
  emit_sll(ACC, ACC, 2, s);               \
  emit_add(ACC, ACC, T1, s);              \
  emit_load(ACC, 0, ACC, s);

static void emit_alloc_self_type(ostream &s) {
  LOAD_TAB(CLASSPROTOTAB)
  s << JAL << "Object.copy" << endl;
}

static void emit_init_self_type(ostream &s) {
  LOAD_TAB(CLASSINITTAB)
  emit_jalr(ACC, s);
}

static void emit_before_method(method_class *f, ostream &s) {
  curr_method = f;
  env.enterscope();
  func_obj_num = 0;
  emit_push(RA, s); // the RA may change in expr.code()
  emit_push(FP, s);  // store the frame pointer
  emit_move(FP, SP, s); // use FP to store the top stack
  s << endl;
}

static void emit_after_method(int num, ostream &s) {
  s << endl;
  emit_load(RA, 2, FP, s); // load ra back
  emit_load(FP, 1, FP, s); // load fp back
  emit_addiu(SP, SP, num, s); // add formals stack, ra stack, fp stack back
  emit_return(s);
  env.exitscope();
}

static void emit_debug_line_num(int line_num, ostream &s) {
#ifdef ENABLE_DEBUG
  s << "#######debug in line: " << line_num << endl;
#endif
}

static void emit_debug_symbol(Symbol name, ostream &s) {
#ifdef ENABLE_DEBUG
  s << "#######debug symbol: " << name << endl;
#endif
}

static void emit_debug_map(ostream &s) {
#ifdef ENABLE_DEBUG
  s << "#Class attrs" << endl;
  for (auto a : class_attr_list_map) {
    s << "#\tname: " << a.first << endl;
    auto list = a.second;
    ITERATE_LIST(list) {
      Attr a = (*list)[i];
      s << "#\t\t" << a.first << ": " << i << endl;
    }
  }

  s << "#Class funcs" << endl;
  for (auto a : class_method_list_map) {
    s << "#\tname: " << a.first << endl;
    auto list = a.second;
    ITERATE_LIST(list) {
      Method m = (*list)[i];
      s << "#\t\t" << m.first << METHOD_SEP << m.second << ": " << i << endl;
    }
  }
#endif
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/
      s << Str << DISPTAB_SUFFIX;

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
      s << Int << DISPTAB_SUFFIX;

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
      s << Bool << DISPTAB_SUFFIX;

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,getTag(Str));
  inttable.code_string_table(str,getTag(Int));
  code_bools(getTag(Bool));
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = getTag(Str);
   intclasstag = getTag(Int);
   boolclasstag = getTag(Bool);

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

int CgenNode::get_attr_size() {
  int parent_attr_size = 0;
  int attr_size = 0;

  if (parentnd != NULL)
    parent_attr_size = parentnd->get_attr_size();

  ITERATE_LIST_NODE(features) {
    auto f = dynamic_cast<attr_class*>(features->nth(i));
    if (f != nullptr)
      attr_size += 1;
  }

  return attr_size + parent_attr_size;
}

void CgenNode::code_prototype(ostream &s, CgenClassTable *table) {
  int obj_size = DEFAULT_OBJFIELDS;
  if (!basic()) {
    obj_size = obj_size + get_attr_size();
  } else if (name == Str) {
    obj_size = obj_size + STRING_SLOTS + 1; // 1 mean remain size
  } else if (name == Int ||
             name == Bool) {
    obj_size = obj_size + INT_SLOTS;
  } else {
    // IO
    obj_size = obj_size;
  }

  s << WORD << "-1" << endl;
  s << name << PROTOBJ_SUFFIX << LABEL
    << WORD << table->getTag(this) << endl
    << WORD << obj_size << endl;
  s << WORD << name << DISPTAB_SUFFIX << endl;

  if (!basic()) {
    AttrList *attr_list = new AttrList();
    class_attr_list_map[name] = attr_list;
    collect_attr_list(s, table, *attr_list);
    ITERATE_LIST(attr_list) {
      Symbol type = (*attr_list)[i].second;
      s << WORD;
      if (type == Int) {
        inttable.lookup_string("0")->code_ref(s);
      } else if (type == Bool) {
        falsebool.code_ref(s);
      } else if (type == Str) {
        stringtable.lookup_string("")->code_ref(s);
      } else {
        s << "0";
      }
      s << endl;
    }
  } else if (name == Str) {
    s << WORD; inttable.lookup_string("0")->code_ref(s); s << endl;
    s << BYTE << "0" << endl;
    s << ALIGN;
  } else if (name == Int ||
          name == Bool) {
    s << WORD << "0" << endl;
  }
}

void CgenNode::code_dispatch(ostream &s, CgenClassTable *table) {
  s << name << DISPTAB_SUFFIX << LABEL;

  MethodList *method_list = new MethodList();
  class_method_list_map[name] = method_list;
  collect_method_list(s, table, *method_list);

  ITERATE_LIST(method_list) {
    Method m = (*method_list)[i];
    s << WORD << m.first << METHOD_SEP << m.second << endl;
  }
}

void CgenNode::code_init(ostream &s, CgenClassTable *table) {
  curr_class_name = name;
  dispatch_class_name = name;
  s << name << CLASSINIT_SUFFIX << LABEL;
  if (!basic()) {
    emit_before_method(nullptr, s); // init do no have formal env
    attrs_init(s, table, DEFAULT_OBJFIELDS);
    emit_after_method(8, s);
  } else {
    emit_return(s);
  }
}

void CgenNode::code_methods(ostream &s, CgenClassTable *table) {
  curr_class_name = name;
  dispatch_class_name = name;
  ITERATE_LIST_NODE(features) {
    auto f = dynamic_cast<method_class*>(features->nth(i));
    if (f != nullptr) {
      s << name << METHOD_SEP << f->name << LABEL;
      emit_before_method(f, s);
      f->expr->code(s);
      emit_after_method(4*f->formals->len()+8, s);
    }
  }
}

void CgenNode::collect_attr_list(ostream &s, CgenClassTable *table, AttrList &attr_list) {
  if (parentnd != nullptr)
    parentnd->collect_attr_list(s, table, attr_list);

  ITERATE_LIST_NODE(features) {
    auto f = dynamic_cast<attr_class*>(features->nth(i));
    if (f != nullptr) {
      int offset;
      Attr a(f->name, f->type_decl);
      if ((offset = symbol_in_attr_list(f->name, attr_list) )== -1) {
        attr_list.push_back(a);
      } else {
        attr_list[offset] = a;
      }
    }
  }
}

void CgenNode::collect_method_list(ostream &s, CgenClassTable *table, MethodList &method_list) {
  if (parentnd != nullptr)
    parentnd->collect_method_list(s, table, method_list);

  ITERATE_LIST_NODE(features) {
    auto f = dynamic_cast<method_class*>(features->nth(i));
    if (f != nullptr) {
      int offset;
      Method m(name, f->name);
      if ((offset = symbol_in_method_list(f->name, method_list) )== -1) {
        method_list.push_back(m);
      } else {
        method_list[offset] = m;
      }
    }
  }
}

int CgenNode::attrs_init(ostream &s, CgenClassTable *table, int attr_pos) {
  if (parentnd != nullptr)
    attr_pos = parentnd->attrs_init(s, table, attr_pos);

  ITERATE_LIST_NODE(features) {
    auto f = dynamic_cast<attr_class*>(features->nth(i));
    if (f != nullptr) {
      curr_type = f->type_decl;
      emit_push(SELF, s);
      f->init->code(s);
      emit_pop(SELF, s);
      emit_store(ACC, attr_pos++, SELF, s);
    }
  }
  return attr_pos;
}

int CgenClassTable::getTag(CgenNode *node) {
  return getTag(node->get_name());
}

int CgenClassTable::getTag(Symbol s) {
  for (int i = 0; i < classList.size(); ++i) {
    if (classList[i] == s) {
      return i;
    }
  }

  classList.push_back(s);
  return classList.size()-1;
}

void CgenClassTable::code_prototype_objects() {
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    CgenNode *n = l->hd();
    n->code_prototype(str, this);
  }
}

void CgenClassTable::code_nameTab() {
  str << CLASSNAMETAB << LABEL;

  for (auto s: classList) {
    str << WORD;
    stringtable.lookup_string(s->get_string())->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::code_protoTab() {
  str << CLASSPROTOTAB << LABEL;

  for (auto s: classList) {
    str << WORD << s << PROTOBJ_SUFFIX << endl;
  }
}

void CgenClassTable::code_initTab() {
  str << CLASSINITTAB << LABEL;

  for (auto s: classList) {
    str << WORD << s << CLASSINIT_SUFFIX << endl;
  }
}

void CgenClassTable::code_dispatch_tables() {
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    CgenNode *n = l->hd();
    n->code_dispatch(str, this);
  }
}

void CgenClassTable::code_class_initializer() {
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    CgenNode *n = l->hd();
    n->code_init(str, this);
  }
}

void CgenClassTable::code_class_methods() {
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    CgenNode *n = l->hd();
    if (!n->basic())
      n->code_methods(str, this);
  }
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  code_prototype_objects();
  code_nameTab();
  code_protoTab();
  code_initTab();
  code_dispatch_tables();

  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  code_class_initializer();
  code_class_methods();
  emit_debug_map(str);
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

#define OP_SYMBOL(op)                                     \
  int offset;                                             \
  if ((offset = get_attr_offset(name)) != -1) {           \
    op(ACC, offset, SELF, s);                             \
  } else if ((offset = get_formal_offset(name)) != -1) {  \
    op(ACC, offset, FP, s);                               \
  } else if ((offset = get_env_offset(name)) != -1) {     \
    op(ACC, -offset, FP, s);                              \
  }

void assign_class::code(ostream &s) {
  expr->code(s);

  OP_SYMBOL(emit_store);
}

#define CHECK_DISPATCH_VOID()               \
  int next_label = label_num++;             \
  emit_bnez(SELF, next_label, s);           \
  emit_load_imm(T1, get_line_number(), s);  \
  emit_load_address(ACC, "str_const0", s);  \
  emit_jal(DISPATCH_VOID_ABORT, s);         \
  emit_label_def(next_label, s);


void static_dispatch_class::code(ostream &s) {
  s << endl;
  s << "#static dispatch start: " << name << endl;
  emit_push(SELF, s);

  ITERATE_LIST_NODE(actual) {
    auto a = actual->nth(i);
    a->code(s);
    emit_push(ACC, s);
  }

  expr->code(s);
  if (expr->type != SELF_TYPE && expr->type != self) {
    emit_move(SELF, ACC, s);
    s << endl;
  }
  CHECK_DISPATCH_VOID();

  dispatch_class_name = expr->type;
  s << JAL << type_name << METHOD_SEP << name << endl;
  dispatch_class_name = curr_class_name;

  s << endl;
  emit_pop(SELF, s);
  s << "#static dispatch ended: " << endl;
  s << endl;
}

void dispatch_class::code(ostream &s) {
  s << endl;
  s << "#dispatch start: " << name << endl;
  emit_push(SELF, s);

  ITERATE_LIST_NODE(actual) {
    auto a = actual->nth(i);
    a->code(s);
    emit_push(ACC, s);
  }

  expr->code(s);
  if (expr->type != SELF_TYPE && expr->type != self) {
    emit_move(SELF, ACC, s);
    s << endl;
  }
  CHECK_DISPATCH_VOID();

  emit_load(T2, DEFAULT_OBJFIELDS-1, SELF, s);
  emit_load(T1, get_func_offset(expr->get_type(), name), T2, s);

  dispatch_class_name = expr->type;
  emit_jalr(T1, s);
  dispatch_class_name = curr_class_name;

  s << endl;
  emit_pop(SELF, s);
  s << "#dispatch ended: " << endl;
  s << endl;
}

#define COND(then_code, else_code) \
  int else_label = label_num++;    \
  int next_label = label_num++;    \
  emit_beqz(ACC, else_label, s);   \
  then_code;                       \
  emit_branch(next_label, s);      \
  emit_label_def(else_label, s);   \
  else_code;                       \
  s << "#bool next fragment: " << endl; \
  emit_label_def(next_label, s);

void cond_class::code(ostream &s) {
  pred->code(s);
  emit_load_boolval(ACC, ACC, s);

  COND(then_exp->code(s), else_exp->code(s));
}

void loop_class::code(ostream &s) {
  s << endl;
  s << "#loop start: " << endl;

  int start_label = label_num++;
  int next_label = label_num++;

  emit_label_def(start_label, s);
  pred->code(s);
  emit_load_boolval(ACC, ACC, s); // like cond_class the pred expr return the boolean object
  emit_beqz(ACC, next_label, s); // chech acc if need jump to next fragment
  body->code(s);
  emit_branch(start_label, s);
  s << "#loop next fragment: " << endl;
  emit_label_def(next_label, s);

  s << "#loop ended: " << endl;
  s << endl;
}

void typcase_class::code(ostream &s) {
  s << endl;
  s << "#case start: " << endl;

  expr->code(s); // store the obj address, for compare it's tag
  int next_label = label_num++;
  int error_none = label_num++;
  int error_void = label_num++;

  std::map<int, int> cur_case_label_map, next_case_label_map;
  for (int i=0; i<cases->len(); i++) {
    cur_case_label_map[i] = label_num++;
    if (i == cases->len()-1) {
      next_case_label_map[i] = error_none;
    } else {
      next_case_label_map[i] = cur_case_label_map[i] + 1;
    }
  }

  // 1. first check if expr return address is void
  emit_beqz(ACC, error_void, s);

  // 2. check if case match
  ITERATE_LIST_NODE(cases) {
    auto b = dynamic_cast<branch_class*>(cases->nth(i));

    emit_label_def(cur_case_label_map[i], s);

    s << LA << " " T1 << " " << b->type_decl << PROTOBJ_SUFFIX << endl;
    emit_load(T1, TAG_OFFSET, T1, s);
    emit_load(T2, TAG_OFFSET, ACC, s); // acc seems don't change in case compare
    emit_bne(T1, T2, next_case_label_map[i], s);

    emit_add_env(b->name, s);
    b->expr->code(s);
    emit_delete_env(s);
    emit_branch(next_label, s);
  }

  // 3. put error_none at there, in case the cases.len() is 0
  s << "#case error none: " << endl;
  emit_label_def(error_none, s);
  emit_jal(CASE_ERROR_NONE, s);

  s << "#case error void: " << endl;
  emit_label_def(error_void, s);
  emit_load_imm(T1, get_line_number(), s);
  emit_load_address(ACC, "str_const0", s); // seem that str_const0 put the raw file name
  emit_jal(CASE_ERROR_VOID, s);

  s << "#case next fragment: " << endl;
  emit_label_def(next_label, s);

  s << "#case ended: " << endl;
  s << endl;
}

void block_class::code(ostream &s) {
  ITERATE_LIST_NODE(body) {
    auto a = body->nth(i);
    a->code(s);
  }
}

void let_class::code(ostream &s) {
  s << endl;
  s << "#let start: " << endl;

  curr_type = type_decl;
  init->code(s);
  emit_add_env(identifier, s);
  body->code(s);
  emit_delete_env(s);

  s << "#let ended: " << endl;
  s << endl;
}

#define ARITHMETIC(OP)           \
  e1->code(s);                   \
  emit_push(ACC, s);             \
  e2->code(s);                   \
  emit_load(T1, 1, SP, s);       \
  emit_load_intval(T1, T1, s);   \
  emit_load_intval(ACC, ACC, s); \
  OP(T2, T1, ACC, s);            \
  emit_alloc_obj(Int, s);        \
  emit_store_intval(T2, ACC, s); \
  emit_addiu(SP, SP, 4, s);

void plus_class::code(ostream &s) {
  ARITHMETIC(emit_add)
}

void sub_class::code(ostream &s) {
  ARITHMETIC(emit_sub)
}

void mul_class::code(ostream &s) {
  ARITHMETIC(emit_mul)
}

void divide_class::code(ostream &s) {
  ARITHMETIC(emit_div)
}

void neg_class::code(ostream &s) {
  e1->code(s);
  emit_load_intval(T2, ACC, s); // T2 will not change in emit_alloc_obj
  emit_neg(T2, T2, s);          // this way too hack, use stack based way
  emit_alloc_obj(Int, s);
  emit_store_intval(T2, ACC, s);
}

#define COMPARE_VALUE(op, s)  \
  e1->code(s);              \
  emit_push(ACC, s);        \
  e2->code(s);              \
  emit_load(T1, 1, SP, s);  \
  emit_load_intval(T1, T1, s);   \
  emit_load_intval(ACC, ACC, s); \
  s << "\t" << op << " " << ACC << " " << T1 << " " << ACC << endl; \
  emit_addiu(SP, SP, 4, s); \
  COND(emit_load_bool(ACC, truebool, s), emit_load_bool(ACC, falsebool, s));

void lt_class::code(ostream &s) {
  COMPARE_VALUE("slt", s);
}

void eq_class::code(ostream &s) {
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_move(T2, ACC, s);
  emit_pop(T1, s);

  if (basic_type(e1->type)) {
    emit_load_bool(ACC, truebool, s);
    emit_load_bool(A1, falsebool, s);
    emit_jal("equality_test", s);
  } else {
    emit_eq(ACC, T1, T2, s);
    COND(emit_load_bool(ACC, truebool, s), emit_load_bool(ACC, falsebool, s));
  }
}

void leq_class::code(ostream &s) {
  COMPARE_VALUE("sleu", s);
}

void comp_class::code(ostream &s) {
  e1->code(s);
  emit_load_boolval(T1, ACC, s);
  s << "\tnot" << " " << T1 << " " << T1 << endl;
  emit_store_boolval(T1, ACC, s);
}

void int_const_class::code(ostream& s)  {
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s) {
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s) {
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  if (type_name == SELF_TYPE) {
    emit_alloc_self_type(s);
  } else {
    emit_alloc_obj(type_name, s);
  }

  emit_push(SELF, s);
  emit_push(ACC, s);
  emit_move(SELF, ACC, s);

  if (type_name == SELF_TYPE) {
    emit_init_self_type(s);
  } else {
    s << JAL << type_name << CLASSINIT_SUFFIX << endl;
  }
  emit_pop(ACC, s);
  emit_pop(SELF, s);
}

void isvoid_class::code(ostream &s) {
  e1->code(s);
  COND(emit_load_bool(ACC, falsebool, s), emit_load_bool(ACC, truebool, s));
}

void no_expr_class::code(ostream &s) {
  if (basic_type(curr_type)) {
    emit_alloc_obj(curr_type, s);
  } else {
    emit_load_imm(ACC, 0, s);
  }
}

void object_class::code(ostream &s) {
  if (name == self || name == SELF_TYPE) {
    emit_move(ACC, SELF, s);
  } else {
    OP_SYMBOL(emit_load);
  }
}


