import logging
from sre_compile import isstring
import sys
from tkinter.tix import Tree
import ply.lex as lex
import ply.yacc as yacc
from nodes import *

error = False

reserved = {
    'typedef': 'TYPEDEF',
    'extern': 'EXTERN',
    'static': 'STATIC',
    'auto': 'AUTO',
    'register': 'REGISTER',
    'char': 'CHAR',
    'short': 'SHORT',
    'int': 'INT',
    'long': 'LONG',
    'signed': 'SIGNED',
    'unsigned': 'UNSIGNED',
    'float': 'FLOAT',
    'double': 'DOUBLE',
    'const': 'CONST',
    'volatile': 'VOLATILE',
    'void': 'VOID',
    'struct': 'STRUCT',
    'union': 'UNION',
    'enum': 'ENUM',
    'ellipsis': 'ELLIPSIS',
    'case': 'CASE',
    'default': 'DEFAULT',
    'if': 'IF',
    'else': 'ELSE',
    'switch': 'SWITCH',
    'while': 'WHILE',
    'do': 'DO',
    'for': 'FOR',
    'goto': 'GOTO',
    'continue': 'CONTINUE',
    'break': 'BREAK',
    'return': 'RETURN',
    'sizeof': 'SIZEOF',
}

tokens = [
    'IDENTIFIER',
    'CONSTANT',
    'STRING_LITERAL',
    'PTR_OP',
    'INC_OP',
    'DEC_OP',
    'LEFT_OP',
    'RIGHT_OP',
    'LE_OP',
    'GE_OP',
    'EQ_OP',
    'NE_OP',
    'AND_OP',
    'OR_OP',
    'MUL_ASSIGN',
    'DIV_ASSIGN',
    'MOD_ASSIGN',
    'ADD_ASSIGN',
    'SUB_ASSIGN',
    'LEFT_ASSIGN',
    'RIGHT_ASSIGN',
    'AND_ASSIGN',
    'XOR_ASSIGN',
    'OR_ASSIGN',
    'TYPE_NAME',
] + list(reserved.values())


def t_IDENTIFIER(t):
    r'[a-zA-Z_]\w*'
    t.type = reserved.get(t.value, 'IDENTIFIER')    # Check for reserved words
    t.value = str(t.value)
    return t


def t_CONSTANT(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_STRING_LITERAL(t):
    r'"\w*"'
    t.value = t.value[1:-1]
    return t


def t_LEFT_ASSIGN(t):
    r'<<='
    return t


def t_RIGHT_ASSIGN(t):
    r'>>='
    return t


def t_INC_OP(t):
    r'\+\+'
    return t


def t_DEC_OP(t):
    r'--'
    return t


def t_LEFT_OP(t):
    r'<<'
    return t


def t_RIGHT_OP(t):
    r'>>'
    return t


def t_LE_OP(t):
    r'<='
    return t


def t_GE_OP(t):
    r'>='
    return t


def t_EQ_OP(t):
    r'=='
    return t


def t_NE_OP(t):
    r'!='
    return t


def t_AND_OP(t):
    r'&&'
    return t


def t_OR_OP(t):
    r'\|\|'
    return t


def t_MUL_ASSIGN(t):
    r'\*='
    return t


def t_DIV_ASSIGN(t):
    r'/='
    return t


def t_MOD_ASSIGN(t):
    r'%='
    return t


def t_ADD_ASSIGN(t):
    r'\+='
    return t


def t_SUB_ASSIGN(t):
    r'-='
    return t


def t_AND_ASSIGN(t):
    r'&='
    return t


def t_XOR_ASSIGN(t):
    r'^='
    return t


def t_OR_ASSIGN(t):
    r'\|='
    return t


def t_PTR_OP(t):
    r'->'
    return t


t_ignore = '  \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


literals = "+-*/(),;}{[]=~!%.|^&"

start = 'translation_unit'


def getp(p, index):
    p[index] if len(p) == index+1 else None


class Node:
    def __init__(self, type, children=None, leafs=None):
        self.type = type
        if children:
            self.children = children
        else:
            self.children = []
        if leafs:
            self.leafs = leafs
        else:
            self.leafs = []

    def __str__(self):
        string = ""
        string += self.type + "\n"
        string += "Children:" + "\n"
        for child in self.children:
            string += str(child) + " "
        string += "Leafs:" + "\n"
        for leaf in self.leafs:
            string += str(leaf) + " "
        return string + "\n"


class Expr:
    pass


class Primary(Expr):
    def __init__(self, expr):
        self.expr = expr


def go_deeper(p):
    if len(p) == 2:
        p[0] = p[1]
        return True
    return False


def p_primary_expression(p):
    '''primary_expression : IDENTIFIER
        | CONSTANT
        | STRING_LITERAL
        | '(' expression ')'
    '''
    if isstring(p[1]):
        if p[1][0] == '"' and p[1][-1] == '"':
            p[0] = None
        else:
            p[0] = IDENTIFIER(p[1])
    else:
        p[0] = INT_CONST(p[1])
    #print("PRIMARY EXPRESSION!", p[1])
    # if len(p) > 2:
    # p[0] = Node("prim_expr", [p[2]], [p[1], p[3]])
    # else:
    # p[0] = Node("prim_expr", None, [p[1]])
    pass


def p_postfix_expression(p):
    '''postfix_expression : primary_expression
        | postfix_expression '[' expression ']'
        | postfix_expression '(' ')'
        | postfix_expression '(' argument_expression_list ')'
        | postfix_expression '.' IDENTIFIER
        | postfix_expression PTR_OP IDENTIFIER
        | postfix_expression INC_OP
        | postfix_expression DEC_OP'''
    go_deeper(p)
    if len(p) == 5:
        if p[2] == '[':
            p[0] = ARRAY(p[1], p[3])
    pass


def p_argument_expression_list(p):
    '''argument_expression_list : assignment_expression
    | argument_expression_list ',' assignment_expression
    '''
    pass


def p_unary_expression(p):
    '''unary_expression : postfix_expression
    | INC_OP unary_expression
    | DEC_OP unary_expression
    | unary_operator cast_expression
    | SIZEOF unary_expression
    | SIZEOF '(' type_name ')'
    '''
    go_deeper(p)
    pass


# Ampersand
def p_unary_operator(p):
    '''unary_operator : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | '!'
    '''
    pass


def p_cast_expression(p):
    '''cast_expression : unary_expression
    | '(' type_name ')' cast_expression
    '''
    go_deeper(p)
    pass


def p_multiplicative_expression(p):
    '''multiplicative_expression : cast_expression
    | multiplicative_expression '*' cast_expression
    | multiplicative_expression '/' cast_expression
    | multiplicative_expression '%' cast_expression
    '''
    go_deeper(p)
    pass


def p_additive_expression(p):
    '''additive_expression : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression
    '''
    if not go_deeper(p):
        p[0] = PLUS_EXPR(p[1], p[2], p[3])
    pass


def p_shift_expression(p):
    '''shift_expression : additive_expression
    | shift_expression LEFT_OP additive_expression
    | shift_expression RIGHT_OP additive_expression
    '''
    if not go_deeper(p):
        p[0] = SHIFT_EXPR(p[1], p[2], p[3])
    pass


def p_relational_expression(p):
    '''relational_expression : shift_expression
    | relational_expression '<' shift_expression
    | relational_expression '>' shift_expression
    | relational_expression LE_OP shift_expression
    | relational_expression GE_OP shift_expression
    '''
    if not go_deeper(p):
        p[0] = REL_EXPR(p[1], p[2], p[3])
    pass


def p_equality_expression(p):
    '''equality_expression : relational_expression
    | equality_expression EQ_OP relational_expression
    | equality_expression NE_OP relational_expression
    '''
    if not go_deeper(p):
        p[0] = EQU_EXPR(p[1], p[2], p[3])
    pass


def p_and_expression(p):
    '''and_expression : equality_expression
    | and_expression '&' equality_expression
    '''
    if not go_deeper(p):
        p[0] = AND_EXPR(p[1], p[2], p[3])
    pass


def p_exclusive_or_expression(p):
    '''exclusive_or_expression : and_expression
    | exclusive_or_expression '^' and_expression
    '''
    if not go_deeper(p):
        p[0] = XOR_EXPR(p[1], p[2], p[3])
    pass


def p_inclusive_or_expression(p):
    '''inclusive_or_expression : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression
    '''
    if not go_deeper(p):
        p[0] = IOR_EXPR(p[1], p[2], p[3])
    pass


def p_logical_and_expression(p):
    '''logical_and_expression : inclusive_or_expression
    | logical_and_expression AND_OP inclusive_or_expression
    '''
    go_deeper(p)
    pass


def p_logical_or_expression(p):
    '''logical_or_expression : logical_and_expression
    | logical_or_expression OR_OP logical_and_expression
    '''
    go_deeper(p)
    pass


def p_conditional_expression(p):
    '''conditional_expression : logical_or_expression
    | logical_or_expression '?' expression ':' conditional_expression
    '''
    go_deeper(p)
    pass


def p_assignment_expression(p):
    '''assignment_expression : conditional_expression
    | unary_expression assignment_operator assignment_expression
    '''
    if not go_deeper(p):
        p[0] = ASSIGN_EXPR(p[1], p[2], p[3])
    pass


def p_assignment_operator(p):
    '''assignment_operator : '='
    | MUL_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    | ADD_ASSIGN
    | SUB_ASSIGN
    | LEFT_ASSIGN
    | RIGHT_ASSIGN
    | AND_ASSIGN
    | XOR_ASSIGN
    | OR_ASSIGN
    '''
    p[0] = p[1]
    pass


def p_expression(p):
    '''expression : assignment_expression
    | expression ',' assignment_expression
    '''
    go_deeper(p)
    pass


def p_constant_expression(p):
    '''constant_expression : conditional_expression
    '''
    go_deeper(p)
    pass


def p_declaration(p):
    '''declaration : declaration_specifiers ';'
    | declaration_specifiers init_declarator_list ';'
    '''
    init = p[len(p)-2]
    p[0] = DECLARATION(p[1], init)
    # if len(p) > 3:
    # p[0] = Node("decl", [p[1], p[2]], [p[3]])
    # else:
    # p[0] = Node("decl", [p[1]], [p[2]])
    pass


def p_declaration_specifiers(p):
    '''declaration_specifiers : storage_class_specifier
    | storage_class_specifier declaration_specifiers
    | type_specifier
    | type_specifier declaration_specifiers
    | type_qualifier
    | type_qualifier declaration_specifiers
    '''
    next = getp(p, 2)
    p[0] = DECL_SPEC(p[1], next)
    # if len(p) == 3:
    # p[0] = Node("decl_spec", [p[1], p[2]], None)
    # else:
    # p[0] = Node("decl_spec", [p[1]], None)
    pass


def p_init_declarator_list(p):
    '''init_declarator_list : init_declarator
    | init_declarator_list ',' init_declarator
    '''
    if len(p) > 2:
        p[0] = p[3]
    else:
        p[0] = p[1]
    pass


def p_init_declarator(p):
    '''init_declarator : declarator
    | declarator '=' initializer
    '''
    #print("INIT DECLARATOR")
    if len(p) == 2:
        p[0] = INIT_DECL(p[1], None)
    else:
        p[0] = INIT_DECL(p[1], p[3])
    # if len(p) > 2:
    # p[0] = ASSIGN_EXPR(p[1], p[3])
    # if len(p) > 2:
    # p[0] = Node("init_decl", [p[1], p[3]], [p[2]])
    # else:
    # p[0] = Node("init_decl", [p[1]])
    pass


def p_storage_class_specifier(p):
    '''storage_class_specifier : TYPEDEF
    | EXTERN
    | STATIC
    | AUTO
    | REGISTER
    '''
    p[0] = STORAGE_CLASS(p[1])
    pass


def p_type_specifier(p):
    '''type_specifier : VOID
    | CHAR
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | SIGNED
    | UNSIGNED
    | struct_or_union_specifier
    | enum_specifier
    | TYPE_NAME
    '''
    # Dodać obsługę structów i enumów
    p[0] = TYPE_SPEC(p[1])
    #p[0] = Node("type_spec", None, [p[1]])
    pass


def p_struct_or_union_specifier(p):
    '''struct_or_union_specifier : struct_or_union IDENTIFIER '{' struct_declaration_list '}'
    | struct_or_union '{' struct_declaration_list '}'
    | struct_or_union IDENTIFIER
    '''
    pass


def p_struct_or_union(p):
    '''struct_or_union : STRUCT
    | UNION
    '''
    pass


def p_struct_declaration_list(p):
    '''struct_declaration_list : struct_declaration
    | struct_declaration_list struct_declaration
    '''
    pass


def p_struct_declaration(p):
    '''struct_declaration : specifier_qualifier_list struct_declarator_list ';'
    '''
    pass


def p_specifier_qualifier_list(p):
    '''specifier_qualifier_list : type_specifier specifier_qualifier_list
    | type_specifier
    | type_qualifier specifier_qualifier_list
    | type_qualifier
    '''
    pass


def p_struct_declarator_list(p):
    '''struct_declarator_list : struct_declarator
    | struct_declarator_list ',' struct_declarator
    '''
    pass


def p_struct_declarator(p):
    '''struct_declarator : declarator
    | ':' constant_expression
    | declarator ':' constant_expression
    '''
    pass


def p_enum_specifier(p):
    '''enum_specifier : ENUM '{' enumerator_list '}'
    | ENUM IDENTIFIER '{' enumerator_list '}'
    | ENUM IDENTIFIER
    '''
    pass


def p_enumerator_list(p):
    '''enumerator_list : enumerator
    | enumerator_list ',' enumerator
    '''
    pass


def p_enumerator(p):
    '''enumerator : IDENTIFIER
    | IDENTIFIER '=' constant_expression
    '''
    pass


def p_type_qualifier(p):
    '''type_qualifier : CONST
    | VOLATILE
    '''
    p[0] = TYPE_QUAL(p[1])
    #p[0] = Node("type_qual", None, [p[1]])
    pass


def p_declarator(p):
    '''declarator : pointer direct_declarator
    | direct_declarator
    '''
    if len(p) > 2:
        p[0] = DECLARATOR(p[1], p[2])
    else:
        p[0] = DECLARATOR(None, p[1])
    pass


def p_direct_declarator(p):
    '''direct_declarator : IDENTIFIER
    | '(' declarator ')'
    | direct_declarator '[' constant_expression ']'
    | direct_declarator '[' ']'
    | direct_declarator '(' parameter_type_list ')'
    | direct_declarator '(' identifier_list ')'
    | direct_declarator '(' ')'
    '''
    #print("Direct")
    if len(p) == 2:
        p[0] = DIRECT_DECLARATOR(p[1])
    elif p[1] == '(':
        p[0] = p[2]
    elif p[2] == '[':
        p[0] = ARRAY_DECL(p[1], p[3])
    else:
        p[0] = p[1]
    pass


def p_pointer(p):
    '''pointer : '*'
    | '*' type_qualifier_list
    | '*' pointer
    | '*' type_qualifier_list pointer
    '''
    pass


def p_type_qualifier_list(p):
    '''type_qualifier_list : type_qualifier
    | type_qualifier_list type_qualifier
    '''
    pass


def p_parameter_type_list(p):
    '''parameter_type_list : parameter_list
    | parameter_list ',' ELLIPSIS
    '''
    pass


def p_parameter_list(p):
    '''parameter_list : parameter_declaration
    | parameter_list ',' parameter_declaration
    '''
    pass


def p_parameter_declaration(p):
    '''parameter_declaration : declaration_specifiers declarator
    | declaration_specifiers abstract_declarator
    | declaration_specifiers
    '''
    pass


def p_identifier_list(p):
    '''identifier_list : IDENTIFIER
    | identifier_list ',' IDENTIFIER
    '''
    pass


def p_type_name(p):
    '''type_name : specifier_qualifier_list
    | specifier_qualifier_list abstract_declarator
    '''
    pass


def p_abstract_declarator(p):
    '''abstract_declarator : pointer
    | direct_abstract_declarator
    | pointer direct_abstract_declarator
    '''
    pass


def p_direct_abstract_declarator(p):
    '''direct_abstract_declarator : '(' abstract_declarator ')'
    | '[' ']'
    | '[' constant_expression ']'
    | direct_abstract_declarator '[' ']'
    | direct_abstract_declarator '[' constant_expression ']'
    | '(' ')'
    | '(' parameter_type_list ')'
    | direct_abstract_declarator '(' ')'
    | direct_abstract_declarator '(' parameter_type_list ')'
    '''
    pass


def p_initializer(p):
    '''initializer : assignment_expression
    | '{' initializer_list '}'
    | '{' initializer_list ',' '}'
    '''
    p[0] = EXPR_STMT(p[1])
    pass


def p_initializer_list(p):
    '''initializer_list : initializer
    | initializer_list ',' initializer
    '''
    pass


def p_statement(p):
    '''statement : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    '''
    p[0] = p[1]
    pass


def p_labeled_statement(p):
    '''labeled_statement : IDENTIFIER ':' statement
    | CASE constant_expression ':' statement
    | DEFAULT ':' statement
    '''
    pass


def p_compound_statement(p):
    '''compound_statement : '{' '}'
    | '{' statement_list '}'
    | '{' declaration_list '}'
    | '{' declaration_list statement_list '}'
    '''
    if len(p) == 4:
        if type(p[2]) is STMT:
            p[0] = COMPOUND_STMT(None, p[2])
        if type(p[2]) is DECL_LIST:
            p[0] = COMPOUND_STMT(p[2], None)
    elif len(p) == 5:
        p[0] = COMPOUND_STMT(p[2], p[3])
    else:
        p[0] = COMPOUND_STMT(None, None)
    pass


def p_declaration_list(p):
    '''declaration_list : declaration
    | declaration_list declaration
    '''
    if len(p) > 2:
        p[0] = DECL_LIST(p[2], p[1])
    else:
        p[0] = DECL_LIST(p[1], None)
    pass


def p_statement_list(p):
    '''statement_list : statement
    | statement_list statement
    '''
    if len(p) == 2:
        p[0] = STMT(p[1], None)
    else:
        p[0] = STMT(p[2], p[1])
    pass


def p_expression_statement(p):
    '''expression_statement : ';'
    | expression ';'
    '''
    p[0] = EXPR_STMT(p[1])
    pass


def p_selection_statement(p):
    '''selection_statement : IF '(' expression ')' statement
    | IF '(' expression ')' statement ELSE statement
    | SWITCH '(' expression ')' statement
    '''
    if len(p) == 6:
        p[0] = IF(p[3], p[5], None)
    elif len(p) == 8:
        p[0] = IF(p[3], p[5], p[7])
    pass


def p_iteration_statement(p):
    '''iteration_statement : WHILE '(' expression ')' statement
    | DO statement WHILE '(' expression ')' ';'
    | FOR '(' expression_statement expression_statement ')' statement
    | FOR '(' expression_statement expression_statement expression ')' statement
    '''
    p[0] = WHILE(p[3], p[5])
    pass


def p_jump_statement(p):
    '''jump_statement : GOTO IDENTIFIER ';'
    | CONTINUE ';'
    | BREAK ';'
    | RETURN ';'
    | RETURN expression ';'
    '''
    pass


class TranslationUnit(Expr):
    def __init__(self, ext_decl):
        self.declarations = [ext_decl]

    def add(self, ext_decl):
        self.declarations.append(ext_decl)


def p_translation_unit(p):
    '''translation_unit : external_declaration
    | translation_unit external_declaration
    '''
    if len(p) > 2:
        p[0] = EXTERNAL_DECL(p[2], p[1])
    else:
        p[0] = EXTERNAL_DECL(p[1], None)
    pass


class ExternalDeclaration(Expr):
    def __init__(self, child):
        self.child = ()


def p_external_declaration(p):
    '''external_declaration : function_definition
    | declaration
    '''
    p[0] = p[1]
    pass


def p_function_definition(p):
    '''function_definition : declaration_specifiers declarator declaration_list compound_statement
    | declaration_specifiers declarator compound_statement
    | declarator declaration_list compound_statement
    | declarator compound_statement
    '''
    p[0] = FUNCTION(p[len(p)-1])
    pass


def p_error(token):
    if token is not None:
        print("Line %s, illegal token %s(%s)" %
              (token.lineno, token.type, token.value))
        error = True
        parser.errok()
    else:
        print('Unexpected end of input')


lexer = lex.lex()
fh = open("gb.cpp", "r")
lexer.input(fh.read())
#for token in lexer:
#    print("line %d: %s(%s)" % (token.lineno, token.type, token.value))
fh.close()


logging.basicConfig(
    level=logging.DEBUG,
    filename="parselog.txt",
    filemode="w",
    format="%(filename)10s:%(lineno)4d:%(message)s"
)
log = logging.getLogger()
lexer = lex.lex()
parser = yacc.yacc(debug=True)
fh = open("gb.cpp", "r")
text = fh.read()
root = parser.parse(text, lexer=lexer)
s = Code("""SECTION "Header", ROM0[$100] ;

EntryPoint:	
    di 
    jp Start 

REPT $150 - $104
    db 0
ENDR

SECTION "Game Code", ROM0[$200]

Start:
""")
root.generate_code(s)
s += """
.lockup
    jp .lockup"""
#print("================")
#print(s)
#print(symbols_table)
if not error:
    print("Assembly generated!")
    with open("main.asm", "w") as text_file:
        text_file.write("%s" % s)
else:
    print("Assembly generation aborted, errors detected!")