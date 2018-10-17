#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <typeinfo>


extern int semant_debug;
extern char *curr_filename;

const int INDENT = 2;
int error_num = 0;
char *current_method_name;

// This is some helper function
int symbolEqual(Symbol s1, Symbol s2) {
	return s1->equal_string(s2->get_string(), s2->get_len());
}

void error_place(tree_node* t) {
  cerr << curr_filename << ":" << t->get_line_number() << ": ";
  error_num += 1;
}

void error_type_mismatch(tree_node *t, Symbol type, Symbol declareType, Symbol id) {
	error_place(t);
  if (current_method_name == nullptr) {
		cerr << "Type " << type << " of assigned expression does not conform to ";
		cerr << "declared type " << declareType << " of identifier " << id << "." << endl;
  } else {
		cerr << "In call of method " << current_method_name << ", ";
		cerr << "Type " << type << " of parameter " << id << " ";
		cerr << "does not conform to declare type " << declareType << "." << endl;
  }
}

void error_undeclare(tree_node* t, Symbol s) {
	error_place(t);
	cerr << "Undeclared identifier " << s << "." << endl;
}

void check_class_exist(Symbol class_name, ClassGraph* graph, tree_node* t) {
  if (!graph->haveClass(class_name)) {
    error_undeclare(t, class_name);
  }
}

#define CHECK_CLASS_EXIST(name) check_class_exist(name, graph, this)

void debug() {
	cerr << "debug" << endl;
}

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

std::vector<ClassContent> ClassTable::getClassContent(Class_ class_) {
	std::vector<ClassContent> pairListResult;
	class__class *c = dynamic_cast<class__class*>(class_);
	Features features = c->get_features();
	ITERATE_LIST_NODE(features) {
		Feature f = features->nth(i);
		ClassContent content = f->getContent();
		pairListResult.push_back(content);
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
	rootGraph_ = new ClassGraph(Object, nullptr, getClassContent(Object_class));
	rootGraph_->childs.push_back(new ClassGraph(IO, rootGraph_, getClassContent(IO_class)));
	rootGraph_->childs.push_back(new ClassGraph(Int, rootGraph_, getClassContent(Int_class)));
	rootGraph_->childs.push_back(new ClassGraph(Bool, rootGraph_, getClassContent(Bool_class)));
	rootGraph_->childs.push_back(new ClassGraph(Str, rootGraph_, getClassContent(Str_class)));
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
		rootGraph_->appendClass(parent, name, getClassContent(c));
	}

  return rootGraph_;
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
		curr_filename = c->get_filename()->get_string();

		env->enterscope();
		for (const auto& pair : graph->getClassContent(className)) {
			env->addid(pair.id, pair.type);
		}
		env->addid(SELF_TYPE, &className);
		env->addid(self, &className);

	  c->SEMANTIC_CHECK();

		env->exitscope();

	  if (error_num > 0) {
	  	error_num = 0;
			cerr << "Compilation halted due to static semantic errors." << endl;
			exit(1);
	  }
	}
}

SEMANTIC_CHECK_IMPLEMENT(class__class) {
  ITERATE_LIST_NODE(features) {
  	features->nth(i)->SEMANTIC_CHECK();
  }
}

SEMANTIC_CHECK_IMPLEMENT(method_class) {
	CHECK_CLASS_EXIST(return_type);

	ITERATE_LIST_NODE(formals) {
		formals->nth(i)->SEMANTIC_CHECK();
	}

	env->enterscope();
	ITERATE_LIST_NODE(formals) {
		Formal f = formals->nth(i);
		std::pair<Symbol, Symbol> pair = f->getFormal();
		env->addid(pair.first, &pair.second);
	}

	expr->EXPR_SEMANTIC_CHECK(return_type);
  env->exitscope();
}

SEMANTIC_CHECK_IMPLEMENT(attr_class) {
	CHECK_CLASS_EXIST(type_decl);

	if (symbolEqual(name, self)) {
		error_place(this);
		cout << "'self' cannot be the name of an attribute." << endl;
	}

	init->EXPR_SEMANTIC_CHECK(type_decl);
}

SEMANTIC_CHECK_IMPLEMENT(formal_class) {
  CHECK_CLASS_EXIST(type_decl);
}

SEMANTIC_CHECK_IMPLEMENT(assign_class) {
  expr->EXPR_SEMANTIC_CHECK(typeRequired);

	Symbol assignType = expr->get_type();
	Symbol* declareType = env->lookup(name);
	if (declareType) {
		if (!graph->parentHaveChild(*declareType, assignType)) {
			error_type_mismatch(this, assignType, *declareType, name);
		}
	}

  set_type(assignType);
  env->addid(name, &assignType);
}

SEMANTIC_CHECK_IMPLEMENT(static_dispatch_class) {
	// check expr validity, then we can get the class type
	expr->EXPR_SEMANTIC_CHECK(nullptr);

	CHECK_CLASS_EXIST(type_name);

	Symbol exprType = expr->get_type();
	if (graph->parentHaveChild(type_name, exprType)) {
	  error_place(this);
		cerr << "Expression type " << type_name << " does not conform to ";
		cerr << "declared static dispatch type" << name << "." << endl;
	}

	std::vector<ClassContent> contentList = graph->getClassContent(type_name);

	for (const auto& content : contentList) {
		if (content.id->equal_string(name->get_string(), name->get_len())) {
			std::vector<std::pair<Symbol, Symbol> > methodFormals = content.methodFormals;
			if (actual->len() != methodFormals.size()) {
				cerr << "formals don't equal" << endl;
				error_num += 1;
			}

			current_method_name = name->get_string();
			ITERATE_LIST_NODE(actual) {
				actual->nth(i)->EXPR_SEMANTIC_CHECK(methodFormals[i].second);
			}

      set_type(*content.type);
			current_method_name = nullptr;
			return;
		}
	}

	error_place(this);
}

SEMANTIC_CHECK_IMPLEMENT(dispatch_class) {
	// check expr validity, then we can get the class type
	expr->EXPR_SEMANTIC_CHECK(nullptr);

	Symbol type = expr->get_type();
	type = symbolEqual(type, SELF_TYPE) ? *env->lookup(SELF_TYPE) : type;
	std::vector<ClassContent> contentList = graph->getClassContent(type);

	for (const auto& content : contentList) {
		if (content.id->equal_string(name->get_string(), name->get_len())) {
			std::vector<std::pair<Symbol, Symbol> > methodFormals = content.methodFormals;
			if (actual->len() != methodFormals.size()) {
				cerr << "formals don't equal" << endl;
				error_num += 1;
			}

			current_method_name = name->get_string();
			ITERATE_LIST_NODE(actual) {
				actual->nth(i)->EXPR_SEMANTIC_CHECK(methodFormals[i].second);
			}

			set_type(*content.type);
			current_method_name = nullptr;
      return;
		}
	}

	error_place(this);
	cerr << "Dispatch to undefined method " << name << "." << endl;
}

SEMANTIC_CHECK_IMPLEMENT(cond_class) {
	// pred must have type of bool
	pred->EXPR_SEMANTIC_CHECK(Bool);
	then_exp->EXPR_SEMANTIC_CHECK(nullptr);
	else_exp->EXPR_SEMANTIC_CHECK(nullptr);

	Symbol thenType = then_exp->get_type();
	Symbol elseType = else_exp->get_type();
	ClassGraph* sameParent = graph->getSameParentGraph(thenType, elseType);
	if (sameParent == nullptr) {
	  cerr << "This won't happen, every type have same parent: Object" << endl;
	  error_num += 1;
	} else if (!graph->parentHaveChild(typeRequired, sameParent->name)) {
	  cerr << "The type required don't have child of then/else type" << endl;
		cerr << "type required: " << typeRequired << " then/else type: " << sameParent << endl;
		error_num += 1;
	} else {
		set_type(sameParent->name);
	}
}

SEMANTIC_CHECK_IMPLEMENT(loop_class) {
	pred->EXPR_SEMANTIC_CHECK(Bool);
	body->EXPR_SEMANTIC_CHECK(Object);

	set_type(Object);
}

SEMANTIC_CHECK_IMPLEMENT(typcase_class) {
  expr->EXPR_SEMANTIC_CHECK(nullptr);

  ITERATE_LIST_NODE(cases) {
  	cases->nth(i)->EXPR_SEMANTIC_CHECK(nullptr);
  }
}

SEMANTIC_CHECK_IMPLEMENT(branch_class) {
  CHECK_CLASS_EXIST(type_decl);

	expr->EXPR_SEMANTIC_CHECK(type_decl);
	// TODO check the branch type
}

SEMANTIC_CHECK_IMPLEMENT(block_class) {
	env->enterscope();

	Symbol checkType;
  ITERATE_LIST_NODE(body) {
  	if (i == (body->len()-1)) {
  		checkType = typeRequired;
  	} else {
  		checkType = nullptr;
  	}
		body->nth(i)->EXPR_SEMANTIC_CHECK(checkType);
  }

	Symbol type = body->nth(body->len()-1)->get_type();
  set_type(type);
	env->exitscope();
}

SEMANTIC_CHECK_IMPLEMENT(let_class) {
  CHECK_CLASS_EXIST(type_decl);

	init->EXPR_SEMANTIC_CHECK(type_decl);

	env->addid(identifier, &type_decl);
	body->EXPR_SEMANTIC_CHECK(typeRequired);

	set_type(body->get_type());
}

SEMANTIC_CHECK_IMPLEMENT(plus_class) {
	e1->EXPR_SEMANTIC_CHECK(typeRequired);
	e2->EXPR_SEMANTIC_CHECK(e1->get_type());

	set_type(e1->get_type());
}

SEMANTIC_CHECK_IMPLEMENT(sub_class) {
	e1->EXPR_SEMANTIC_CHECK(typeRequired);
	e2->EXPR_SEMANTIC_CHECK(e1->get_type());

	set_type(e1->get_type());
}

SEMANTIC_CHECK_IMPLEMENT(mul_class) {
	e1->EXPR_SEMANTIC_CHECK(typeRequired);
	e2->EXPR_SEMANTIC_CHECK(e1->get_type());

	set_type(e1->get_type());
}

SEMANTIC_CHECK_IMPLEMENT(divide_class) {
	e1->EXPR_SEMANTIC_CHECK(typeRequired);
	e2->EXPR_SEMANTIC_CHECK(e1->get_type());

	set_type(e1->get_type());
}

SEMANTIC_CHECK_IMPLEMENT(neg_class) {
	e1->EXPR_SEMANTIC_CHECK(Bool);

	set_type(Bool);
}

SEMANTIC_CHECK_IMPLEMENT(lt_class) {
	e1->EXPR_SEMANTIC_CHECK(typeRequired);
	e2->EXPR_SEMANTIC_CHECK(e1->get_type());

	set_type(Bool);
}

SEMANTIC_CHECK_IMPLEMENT(eq_class) {
	e1->EXPR_SEMANTIC_CHECK(typeRequired);
	e2->EXPR_SEMANTIC_CHECK(e1->get_type());

	set_type(Bool);
}

SEMANTIC_CHECK_IMPLEMENT(leq_class) {
	e1->EXPR_SEMANTIC_CHECK(typeRequired);
	e2->EXPR_SEMANTIC_CHECK(e1->get_type());

	set_type(Bool);
}

SEMANTIC_CHECK_IMPLEMENT(comp_class) {
	e1->EXPR_SEMANTIC_CHECK(Int);

	set_type(e1->get_type());
}

SEMANTIC_CHECK_IMPLEMENT(int_const_class) {
	if (typeRequired != nullptr
			&& !graph->parentHaveChild(typeRequired, Int)) {
	  cerr << "int_const_class error" << endl;
		error_num += 1;
	}

	set_type(Int);
}

SEMANTIC_CHECK_IMPLEMENT(bool_const_class) {
	if (typeRequired != nullptr
			&& !graph->parentHaveChild(typeRequired, Bool)) {
		cerr << "bool_const_class error" << endl;
		error_num += 1;
	}

	set_type(Bool);
}

SEMANTIC_CHECK_IMPLEMENT(string_const_class) {
	if (typeRequired != nullptr
			&& !graph->parentHaveChild(typeRequired, Str)) {
		cerr << "str_const_class error" << endl;
		error_num += 1;
	}

	set_type(Str);
}

SEMANTIC_CHECK_IMPLEMENT(new__class) {
	// First, check if type_name exist
	CHECK_CLASS_EXIST(type_name);

	// Then, check if type_name is SELF_TYPE
	Symbol typeToSet = type_name;
	if (symbolEqual(type_name, *env->lookup(SELF_TYPE))) {
	  typeToSet = SELF_TYPE;
	}

	if (typeRequired == nullptr) {
		// If type required is null, just set type
		set_type(typeToSet);
	} else if (graph->parentHaveChild(typeRequired, type_name)) {
	  // Then check if type required is the type_name's parent
	  set_type(typeToSet);
	} else {
	  // TODO hint the error
		cerr << "new__class error" << endl;
	  error_num += 1;
	}
}

SEMANTIC_CHECK_IMPLEMENT(isvoid_class) {
  e1->EXPR_SEMANTIC_CHECK(nullptr);
  set_type(Bool);
}

SEMANTIC_CHECK_IMPLEMENT(no_expr_class) {
  set_type(No_type);
}

SEMANTIC_CHECK_IMPLEMENT(object_class) {
  error_num += 1;
  if (!env->lookup(name)) {
  	error_undeclare(this, name);
  } else if (!graph->parentHaveChild(typeRequired, *env->lookup(name))) {
    error_type_mismatch(this, *env->lookup(name), typeRequired, name);
	} else {
		set_type(*env->lookup(name));
	}
}
