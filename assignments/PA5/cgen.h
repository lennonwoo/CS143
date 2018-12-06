#include <assert.h>
#include <stdio.h>
#include <map>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

#define ITERATE_LIST_NODE(list) \
for (int i = 0; i < list->len(); ++i)

#define ITERATE_LIST(list) \
for (int i = 0; i < list->size(); ++i)

#define ITERATE_LIST_REVERSE(list) \
for (int i = list->size() - 1; i >= 0; --i)

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

typedef std::pair<Symbol, Symbol> Method;
typedef std::vector<Method> MethodList;

typedef std::pair<Symbol, Symbol> Attr;
typedef std::vector<Attr> AttrList;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
  std::map<Symbol, int> symbolTagMap;
  std::vector<Symbol> symbolTabList;
  int classTagIndex;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_prototype_objects();
   void code_nameTab();
   void code_dispatch_tables();
   void code_class_initializer();
   void code_class_methods();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
  int getTag(Symbol s);
  int getTag(CgenNode *node);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

  int get_attr_size();
  void code_prototype(ostream &s, CgenClassTable *table);
  void code_dispatch(ostream &s, CgenClassTable *table);
  void code_init(ostream &s, CgenClassTable *table);
  void code_methods(ostream &s, CgenClassTable *table);

  void collect_attr_list(ostream &s, CgenClassTable *table, AttrList &attr_list);
  void collect_method_list(ostream &s, CgenClassTable *table, MethodList &method_list);
  int attrs_init(ostream &s, CgenClassTable *table, int attr_pos);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

