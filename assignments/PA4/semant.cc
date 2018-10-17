#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <typeinfo>


extern int semant_debug;
extern char *curr_filename;

const int INDENT = 2;

// This is some helper function
int symbolEqual(Symbol s1, Symbol s2) {
	return s1->equal_string(s2->get_string(), s2->get_len());
}

void error_place(tree_node* t) {
  cerr << curr_filename << ":" << t->get_line_number() << ": ";
}

void error_type_mismatch(Symbol assignType, Symbol declareType, Symbol id) {
	cerr << "Type " << assignType << " of assigned expression does not conform to ";
	cerr << "declared type " << declareType << " of identifier " << id << "." << endl;
}

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
    if (symbolEqual(content.id, self)) {
      semant_error(c->get_filename(), f);
      cout << "'self' cannot be the name of an attribute." << endl;
    }
		pairListResult.push_back(f->getContent());
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

	if (classtable->errors()) {
		cerr << "Compilation halted due to static semantic errors." << endl;
		exit(1);
	}

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
  	if (f->SEMANTIC_CHECK()) {
  		return true;
  	}
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
		std::pair<Symbol, Symbol> pair = f->getFormal();
		env->addid(pair.first, &pair.second);
	}

	if (expr->EXPR_SEMANTIC_CHECK(return_type)) {
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
		init->dump_with_types(cerr, INDENT);
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
  if (expr->EXPR_SEMANTIC_CHECK(typeRequired)) {
    expr->dump_with_types(cerr, INDENT);
  	return true;
	} else if (env->lookup(name)) {
  	Symbol declareType = *env->lookup(name);
  	Symbol assignType = expr->get_type();

  	if (!graph->parentHaveChild(declareType, assignType)) {
			error_place(this);
			error_type_mismatch(assignType, declareType, name);
			return true;
  	}
  } else {
  	set_type(expr->get_type());
  	env->addid(name, &type);
  	return false;
  }
}

SEMANTIC_CHECK_IMPLEMENT(static_dispatch_class) {
	// check expr validity, then we can get the class type
	if (expr->EXPR_SEMANTIC_CHECK(nullptr)) {
		expr->dump_with_types(cerr, INDENT);
		return true;
	}

	if (!graph->haveClass(type_name)) {
		cerr << "type not defined" << type_name << endl;
		dump_with_types(cerr, INDENT);
		return true;
	}

	Symbol childType = expr->get_type();
	if (graph->parentHaveChild(type_name, childType)) {
		cerr << "The type of expr is not the child of static type" << endl;
		cerr << "Child: " << childType << " parent: " << type_name << endl;
		dump_with_types(cerr, INDENT);
		return true;
	}

	std::vector<ClassContent> contentList = graph->getClassContent(type_name);
	Symbol* method_return_type = checkActualValid(contentList, actual, name, graph, env);
	if (method_return_type) {
		set_type(*method_return_type);
		return false;
	} else {
		return true;
	}
}

SEMANTIC_CHECK_IMPLEMENT(dispatch_class) {
	// check expr validity, then we can get the class type
	if (expr->EXPR_SEMANTIC_CHECK(nullptr)) {
		expr->dump_with_types(cerr, INDENT);
		return true;
	}

	Symbol type = expr->get_type();
	type = symbolEqual(type, SELF_TYPE) ? *env->lookup(SELF_TYPE) : type;
	std::vector<ClassContent> contentList = graph->getClassContent(type);
	Symbol* method_return_type = checkActualValid(contentList, actual, name, graph, env);
	if (method_return_type) {
		set_type(*method_return_type);
		return false;
	} else {
		return true;
	}
}

SEMANTIC_CHECK_IMPLEMENT(cond_class) {
	// pred must have type of bool
	if (pred->EXPR_SEMANTIC_CHECK(Bool)) {
		cerr << "pred is not bool type" << endl;
		pred->dump_with_types(cerr, INDENT);
		return true;
	}

	if (then_exp->EXPR_SEMANTIC_CHECK(nullptr)) {
		cerr << "then_exp type check error" << endl;
		then_exp->dump_with_types(cerr, INDENT);
		return true;
	}

	if (else_exp->EXPR_SEMANTIC_CHECK(nullptr)) {
		cerr << "else_exp type check error" << endl;
		else_exp->dump_with_types(cerr, INDENT);
		return true;
	}

	Symbol thenType = then_exp->get_type();
	Symbol elseType = else_exp->get_type();
	ClassGraph* sameParent = graph->getSameParentGraph(thenType, elseType);
	if (sameParent == nullptr) {
	  cerr << "This won't happen, every type have same parent: Object" << endl;
	  return true;
	} else if (!graph->parentHaveChild(typeRequired, sameParent->name)) {
	  cerr << "The type required don't have child of then/else type" << endl;
		cerr << "type required: " << typeRequired << " then/else type: " << sameParent << endl;
		return true;
	} else {
		set_type(sameParent->name);
		return false;
	}
}

SEMANTIC_CHECK_IMPLEMENT(loop_class) {
	if (pred->EXPR_SEMANTIC_CHECK(Bool)) {
		cerr << "pred is not bool type" << endl;
		pred->dump_with_types(cerr, INDENT);
		return true;
	}

	if (body->EXPR_SEMANTIC_CHECK(Object)) {
		cerr << "pred is not bool type" << endl;
		body->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(Object);
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(typcase_class) {
  if (expr->EXPR_SEMANTIC_CHECK(nullptr)) {
    expr->dump_with_types(cerr, INDENT);
    return true;
  }

  ITERATE_LIST_NODE(cases) {
  	Case c = cases->nth(i);
  	if (c->EXPR_SEMANTIC_CHECK(nullptr)) {
  		c->dump_with_types(cerr, INDENT);
  		return true;
  	}
  }
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(branch_class) {
	if (!graph->haveClass(type_decl)) {
		cerr << "type not defined" << type_decl << endl;
		return true;
	}

	if (expr->EXPR_SEMANTIC_CHECK(type_decl)) {
		expr->dump_with_types(cerr, INDENT);
		return true;
	}

	return false;
}

SEMANTIC_CHECK_IMPLEMENT(block_class) {
	env->enterscope();

	Symbol checkType;
  ITERATE_LIST_NODE(body) {
  	Expression e = body->nth(i);
  	e->dump_with_types(cerr, INDENT);
  	if (i == (body->len()-1)) {
  		checkType = typeRequired;
  	} else {
  		checkType = nullptr;
  	}
  	if (e->EXPR_SEMANTIC_CHECK(checkType)) {
  	  e->dump_with_types(cerr, INDENT);
  	  return true;
  	}
  }

	Symbol type = body->nth(body->len()-1)->get_type();
  set_type(type);
	env->exitscope();
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(let_class) {
	if (!graph->haveClass(type_decl)) {
		cerr << "type not decalred: " << type_decl << endl;
		return true;
	}

	if (init->EXPR_SEMANTIC_CHECK(type_decl)) {
		cerr << "type mismatch: " << type_decl << endl;
		init->dump_with_types(cerr, INDENT);
		return true;
	}

	env->addid(identifier, &type_decl);
	if (body->EXPR_SEMANTIC_CHECK(typeRequired)) {
		cerr << "type mismatch: " << typeRequired << endl;
		body->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(body->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(plus_class) {
	if (e1->EXPR_SEMANTIC_CHECK(typeRequired)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}
	if (e2->EXPR_SEMANTIC_CHECK(e1->get_type())) {
		cerr << "type mismatch: " << typeRequired << endl;
		e2->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(sub_class) {
	if (e1->EXPR_SEMANTIC_CHECK(typeRequired)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}
	if (e2->EXPR_SEMANTIC_CHECK(e1->get_type())) {
		cerr << "type mismatch: " << typeRequired << endl;
		e2->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(mul_class) {
	if (e1->EXPR_SEMANTIC_CHECK(typeRequired)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}
	if (e2->EXPR_SEMANTIC_CHECK(e1->get_type())) {
		cerr << "type mismatch: " << typeRequired << endl;
		e2->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(divide_class) {
	if (e1->EXPR_SEMANTIC_CHECK(typeRequired)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}
	if (e2->EXPR_SEMANTIC_CHECK(e1->get_type())) {
		cerr << "type mismatch: " << typeRequired << endl;
		e2->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(neg_class) {
	if (e1->EXPR_SEMANTIC_CHECK(Bool)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(lt_class) {
	if (e1->EXPR_SEMANTIC_CHECK(typeRequired)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}
	if (e2->EXPR_SEMANTIC_CHECK(e1->get_type())) {
		cerr << "type mismatch: " << typeRequired << endl;
		e2->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(eq_class) {
	if (e1->EXPR_SEMANTIC_CHECK(typeRequired)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}
	if (e2->EXPR_SEMANTIC_CHECK(e1->get_type())) {
		cerr << "type mismatch: " << typeRequired << endl;
		e2->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(leq_class) {
	if (e1->EXPR_SEMANTIC_CHECK(typeRequired)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}
	if (e2->EXPR_SEMANTIC_CHECK(e1->get_type())) {
		cerr << "type mismatch: " << typeRequired << endl;
		e2->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(comp_class) {
	if (e1->EXPR_SEMANTIC_CHECK(Int)) {
		cerr << "type mismatch: " << typeRequired << endl;
		e1->dump_with_types(cerr, INDENT);
		return true;
	}

	set_type(e1->get_type());
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(int_const_class) {
	if (typeRequired != nullptr
			&& !symbolEqual(Int, typeRequired)) {
		cerr << "type required: " << typeRequired << " in int_const_class" << endl;
		return true;
	}

	set_type(Int);
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(bool_const_class) {
	if (typeRequired != nullptr
			&& !symbolEqual(Bool, typeRequired)) {
		cerr << "type required: " << typeRequired << " in bool_const_class" << endl;
		return true;
	}

	set_type(Bool);
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(string_const_class) {
	if (typeRequired != nullptr
			&& !symbolEqual(Str, typeRequired)) {
		cerr << "type required: " << typeRequired << " in string_const_class" << endl;
		return true;
	}

	set_type(Str);
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(new__class) {
	// First, check if type_name exist
	if (!graph->haveClass(type_name)) {
		cerr << "type not declared: " << type_name << endl;
		return true;
	}

	// Then, check if type_name is SELF_TYPE
	Symbol typeToSet = type_name;
	if (symbolEqual(type_name, *env->lookup(SELF_TYPE))) {
	  typeToSet = SELF_TYPE;
	}

	if (typeRequired == nullptr) {
		// If type required is null, just set type
		set_type(typeToSet);
		return false;
	} else if (graph->parentHaveChild(typeRequired, type_name)) {
	  // Then check if type required is the type_name's parent
	  set_type(typeToSet);
	  return false;
	} else {
	  return true;
	}
}

SEMANTIC_CHECK_IMPLEMENT(isvoid_class) {
  if (e1->EXPR_SEMANTIC_CHECK(nullptr)) {
    e1->dump_with_types(cerr, INDENT);
		return true;
  }

  set_type(Bool);
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(no_expr_class) {
  set_type(No_type);
	return false;
}

SEMANTIC_CHECK_IMPLEMENT(object_class) {
	set_type(*env->lookup(SELF_TYPE));
	return false;
}
