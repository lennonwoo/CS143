#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <vector>
#include <map>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods  .

class ClassTable {
public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  ClassGraph* constructRootGraph();

private:
  void checkInheritanceValid();

  bool recognizeSymbol(Symbol symbol) {
    for (auto s : classNameList_) {
      if (symbol->equal_string(s->get_string(), s->get_len()))
        return true;
    }
    return false;
  }

  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  std::vector<class__class* > classList_;
  std::vector<Symbol > classNameList_;
  std::map<Symbol, std::vector<Symbol > > parentChildMap_;
  std::map<Symbol, std::vector<SymbolPair > > classContentMap_;

  ClassGraph* rootGraph;
};


#endif

