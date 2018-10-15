#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <typeinfo>


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
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
static void initialize_constants(void) {
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

std::vector<SymbolPair> getClassSymbolPairList(Class_ class_) {
	std::vector<SymbolPair> pairListResult;
	class__class *c = dynamic_cast<class__class*>(class_);
	Features features = c->get_features();
	ITERATE_LIST_NODE(features) {
		Feature f = features->nth(i);
		pairListResult.push_back(f->getSymbolPair());
	}
	return pairListResult;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
	// init the basic class tree & list
	install_basic_classes();

	for (int i = 0; i < classes->len(); i++) {
		class__class *c = dynamic_cast<class__class*>(classes->nth(i));
		classList_.push_back(c);
		Symbol name = c->get_name();
		Symbol parent = c->get_parent();
		classNameList_.push_back(name);
		parentChildMap_[parent].push_back(name);
	}

	checkInheritanceValid();
}

void ClassTable::install_basic_classes() {
	// The tree package uses these globals to annotate the classes built below.
	// curr_lineno  = 0;
	Symbol filename = stringtable.add_string("<basic class>");

	// The following demonstrates how to create dummy parse trees to
	// refer to basic Cool classes.  There's no need for method
	// bodies -- these are already built into the runtime system.

	// IMPORTANT: The results of the following expressions are
	// stored in local variables.  You will want to do something
	// with those variables at the end of this method to make this
	// code meaningful.

	//
	// The Object class has no parent class. Its methods are
	//        abort() : Object    aborts the program
	//        type_name() : Str   returns a string representation of class name
	//        copy() : SELF_TYPE  returns a copy of the object
	//
	// There is no need for method bodies in the basic classes---these
	// are already built in to the runtime system.

	Class_ Object_class =
					class_(Object,
								 No_class,
								 append_Features(
												 append_Features(
																 single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
																 single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
												 single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
								 filename);

	//
	// The IO class inherits from Object. Its methods are
	//        out_string(Str) : SELF_TYPE       writes a string to the output
	//        out_int(Int) : SELF_TYPE            "    an int    "  "     "
	//        in_string() : Str                 reads a string from the input
	//        in_int() : Int                      "   an int     "  "     "
	//
	Class_ IO_class =
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
								 filename);

	//
	// The Int class has no methods and only a single attribute, the
	// "val" for the integer.
	//
	Class_ Int_class =
					class_(Int,
								 Object,
								 single_Features(attr(val, prim_slot, no_expr())),
								 filename);

	//
	// Bool also has only the "val" slot.
	//
	Class_ Bool_class =
					class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

	//
	// The class Str has a number of slots and operations:
	//       val                                  the length of the string
	//       str_field                            the string itself
	//       length() : Int                       returns length of the string
	//       concat(arg: Str) : Str               performs string concatenation
	//       substr(arg: Int, arg2: Int): Str     substring selection
	//
	Class_ Str_class =
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
								 filename);
	classNameList_ = {Object, IO, Int, Bool, Str};
	parentChildMap_[Object] = {IO, Int, Bool, Str};
	rootGraph = new ClassGraph(Object, nullptr, getClassSymbolPairList(Object_class));
	rootGraph->childs.push_back(new ClassGraph(IO, rootGraph, getClassSymbolPairList(IO_class)));
	rootGraph->childs.push_back(new ClassGraph(Int, rootGraph, getClassSymbolPairList(Int_class)));
	rootGraph->childs.push_back(new ClassGraph(Bool, rootGraph, getClassSymbolPairList(Bool_class)));
	rootGraph->childs.push_back(new ClassGraph(Str, rootGraph, getClassSymbolPairList(Str_class)));
}

void ClassTable::checkInheritanceValid() {
	// check if we have the parent symbol logged
	for (const auto& pair : parentChildMap_) {
		Symbol p =  pair.first;
		if (!recognizeSymbol(p)) {
			semant_error() << p->get_string() << "\t" << "not defined";
		}
	}
}

ClassGraph* ClassTable::constructRootGraph() {
  // assume the parent declare first
  for (auto c : classList_) {
		Symbol name = c->get_name();
		Symbol parent = c->get_parent();
		rootGraph->appendClass(parent, name, getClassSymbolPairList(c));
	}

  return rootGraph;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c) {
	return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t) {
	error_stream << filename << ":" << t->get_line_number() << ": ";
	return semant_error();
}

ostream& ClassTable::semant_error() {
	semant_errors++;
	return error_stream;
}



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
	initialize_constants();

	ClassTable *classtable = new ClassTable(classes);

	ClassGraph* graph = classtable->constructRootGraph();
	SymbolTable<Symbol, Symbol> *env = new SymbolTable<Symbol, Symbol>();

	ITERATE_LIST_NODE(classes) {
		Class_ c = classes->nth(i);
		Symbol className = dynamic_cast<class__class*>(c)->get_name();

		env->enterscope();
		for (const auto& pair : graph->getAllSymbolPair(className)) {
			env->addid(pair.first, pair.second);
		}

	  if (c->SEMANTIC_CHECK()) {
			classtable->semant_error(c);
	  }

		env->exitscope();

		if (classtable->errors()) {
			cerr << "Compilation halted due to static semantic errors." << endl;
			exit(1);
		}
	}
}

SEMANTIC_CHECK_IMPLEMENT(class__class) {
  ITERATE_LIST_NODE(features) {
  	Feature f = features->nth(i);
  	if (f->SEMANTIC_CHECK()) { return true; }
  }

  return false;
}

SEMANTIC_CHECK_IMPLEMENT(method_class) {
	if (!graph->haveClass(return_type)) {
		cerr << "type not defined" << return_type << endl;
		return true;
	}

	ITERATE_LIST_NODE(formals) {
		Formal f = formals->nth(i);
	  if (f->SEMANTIC_CHECK()) { return true; }
	}

	env->enterscope();
	ITERATE_LIST_NODE(formals) {
		Formal f = formals->nth(i);
		SymbolPair pair = f->getSymbolPair();
		env->addid(pair.first, pair.second);
	}

	if (expr->EXPR_SEMANTIC_CHECK(return_type)) {
		cerr << "expr not valid" << endl;
	  expr->dump_with_types(cerr, 2);

		env->exitscope();
		return true;
	} else {

		env->exitscope();
		return false;
	}
}

SEMANTIC_CHECK_IMPLEMENT(attr_class) {
	if (!graph->haveClass(type_decl)) {
		cerr << "type not defined" << type_decl << endl;
		return true;
	}

	if (init->EXPR_SEMANTIC_CHECK(type_decl)) {
	  cerr << "init not valid" << endl;
		init->dump_with_types(cerr, 2);
		return true;
	}
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(formal_class) {
  if (!graph->haveClass(type_decl)) {
		cerr << "type not defined" << type_decl << endl;
  	return true;
  }

  return false;
}

SEMANTIC_CHECK_IMPLEMENT(assign_class) {
  if (expr->EXPR_SEMANTIC_CHECK(type)) {
    expr->dump_with_types(cerr, 2);
  	return true;
  } else {
  	set_type(expr->get_type());
  	env->addid(name, &type);
  	return false;
  }
}

SEMANTIC_CHECK_IMPLEMENT(branch_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(static_dispatch_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(dispatch_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(cond_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(loop_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(typcase_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(block_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(let_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(plus_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(sub_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(mul_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(divide_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(neg_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(lt_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(eq_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(leq_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(comp_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(int_const_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(bool_const_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(string_const_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(new__class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(isvoid_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(no_expr_class) {
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(object_class) {
	return false;
}
























