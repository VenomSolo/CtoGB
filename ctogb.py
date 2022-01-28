import logging
import sys
import ply.lex as lex
import ply.yacc as yacc

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

literals = "+-*/(),;}{[]=~&!"


def t_IDENTIFIER(t):
    r'[a-zA-Z_]\w*'
    return t


def t_CONSTANT(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_STRING_LITERAL(t):
    r'"\w*"'
    t.value = t.value[1:-1]
    return t


t_ignore = '  \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


start = 'translation_unit'


def p_primary_expression(p):
    """primary_expression : IDENTIFIER
        | CONSTANT
        | STRING_LITERAL
        | '(' expression ')'
    """
    print(p.lineno)


def p_postfix_expression(p):
    '''postfix_expression : primary_expression
        | postfix_expression '[' expression ']'
        | postfix_expression '(' ')'
        | postfix_expression '(' argument_expression_list ')'
        | postfix_expression '.' IDENTIFIER
        | postfix_expression PTR_OP IDENTIFIER
        | postfix_expression INC_OP
        | postfix_expression DEC_OP'''
    print(p.lineno)


def p_argument_expression_list(p):
    '''argument_expression_list : assignment_expression
    | argument_expression_list ',' assignment_expression
    '''
    print(p.lineno)


def p_unary_expression(p):
    '''unary_expression : postfix_expression
    | INC_OP unary_expression
    | DEC_OP unary_expression
    | unary_operator cast_expression
    | SIZEOF unary_expression
    | SIZEOF '(' type_name ')'
    '''
    print(p.lineno)


def p_unary_operator(p):
    '''unary_operator : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | '!'
    '''
    print(p.lineno)


def p_cast_expression(p):
    '''cast_expression : unary_expression
    | '(' type_name ')' cast_expression
    '''
    print(p.lineno)


def p_multiplicative_expression(p):
    '''multiplicative_expression : cast_expression
    | multiplicative_expression '*' cast_expression
    | multiplicative_expression '/' cast_expression
    | multiplicative_expression '%' cast_expression
    '''
    print(p.lineno)


def p_additive_expression(p):
    '''additive_expression : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression
    '''
    print(p.lineno)


def p_shift_expression(p):
    '''shift_expression : additive_expression
    | shift_expression LEFT_OP additive_expression
    | shift_expression RIGHT_OP additive_expression
    '''
    print(p.lineno)


def p_relational_expression(p):
    '''relational_expression : shift_expression
    | relational_expression '<' shift_expression
    | relational_expression '>' shift_expression
    | relational_expression LE_OP shift_expression
    | relational_expression GE_OP shift_expression
    '''
    print(p.lineno)


def p_equality_expression(p):
    '''equality_expression : relational_expression
    | equality_expression EQ_OP relational_expression
    | equality_expression NE_OP relational_expression
    '''
    print(p.lineno)


def p_and_expression(p):
    '''and_expression : equality_expression
    | and_expression '&' equality_expression
    '''
    print(p.lineno)


def p_exclusive_or_expression(p):
    '''exclusive_or_expression : and_expression
    | exclusive_or_expression '^' and_expression
    '''
    print(p.lineno)


def p_inclusive_or_expression(p):
    '''inclusive_or_expression : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression
    '''
    print(p.lineno)


def p_logical_and_expression(p):
    '''logical_and_expression : inclusive_or_expression
    | logical_and_expression AND_OP inclusive_or_expression
    '''
    print(p.lineno)


def p_logical_or_expression(p):
    '''logical_or_expression : logical_and_expression
    | logical_or_expression OR_OP logical_and_expression
    '''
    print(p.lineno)


def p_conditional_expression(p):
    '''conditional_expression : logical_or_expression
    | logical_or_expression '?' expression ':' conditional_expression
    '''
    print(p.lineno)


def p_assignment_expression(p):
    '''assignment_expression : conditional_expression
    | unary_expression assignment_operator assignment_expression
    '''
    print(p.lineno)


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
    print(p.lineno)


def p_expression(p):
    '''expression : assignment_expression
    | expression ',' assignment_expression
    '''
    print(p.lineno)


def p_constant_expression(p):
    '''constant_expression : conditional_expression
    '''
    print(p.lineno)


def p_declaration(p):
    '''declaration : declaration_specifiers ';'
    | declaration_specifiers init_declarator_list ';'
    '''
    print(p.lineno)


def p_declaration_specifiers(p):
    '''declaration_specifiers : storage_class_specifier
    | storage_class_specifier declaration_specifiers
    | type_specifier
    | type_specifier declaration_specifiers
    | type_qualifier
    | type_qualifier declaration_specifiers
    '''
    print(p.lineno)


def p_init_declarator_list(p):
    '''init_declarator_list : init_declarator
    | init_declarator_list ',' init_declarator
    '''
    print(p.lineno)


def p_init_declarator(p):
    '''init_declarator : declarator
    | declarator '=' initializer
    '''
    print(p.lineno)


def p_storage_class_specifier(p):
    '''storage_class_specifier : TYPEDEF
    | EXTERN
    | STATIC
    | AUTO
    | REGISTER
    '''
    print(p.lineno)


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
    | enum_specifier
    | TYPE_NAME
    '''
    print(p.lineno)


def p_specifier_qualifier_list(p):
    '''specifier_qualifier_list : type_specifier specifier_qualifier_list
    | type_specifier
    | type_qualifier specifier_qualifier_list
    | type_qualifier
    '''
    print(p.lineno)


def p_struct_declarator_list(p):
    '''struct_declarator_list : struct_declarator
    | struct_declarator_list ',' struct_declarator
    '''
    print(p.lineno)


def p_struct_declarator(p):
    '''struct_declarator : declarator
    | ':' constant_expression
    | declarator ':' constant_expression
    '''
    print(p.lineno)


def p_enum_specifier(p):
    '''enum_specifier : ENUM '{' enumerator_list '}'
    | ENUM IDENTIFIER '{' enumerator_list '}'
    | ENUM IDENTIFIER
    '''
    print(p.lineno)


def p_enumerator_list(p):
    '''enumerator_list : enumerator
    | enumerator_list ',' enumerator
    '''
    print(p.lineno)


def p_enumerator(p):
    '''enumerator : IDENTIFIER
    | IDENTIFIER '=' constant_expression
    '''
    print(p.lineno)


def p_type_qualifier(p):
    '''type_qualifier : CONST
    | VOLATILE
    '''
    print(p.lineno)


def p_declarator(p):
    '''declarator : pointer direct_declarator
    | direct_declarator
    '''
    print(p.lineno)


def p_direct_declarator(p):
    '''direct_declarator : IDENTIFIER
    | '(' declarator ')'
    | direct_declarator '[' constant_expression ']'
    | direct_declarator '[' ']'
    | direct_declarator '(' parameter_type_list ')'
    | direct_declarator '(' identifier_list ')'
    | direct_declarator '(' ')'
    '''
    print(p.lineno)


def p_pointer(p):
    '''pointer : '*'
    | '*' type_qualifier_list
    | '*' pointer
    | '*' type_qualifier_list pointer
    '''
    print(p.lineno)


def p_type_qualifier_list(p):
    '''type_qualifier_list : type_qualifier
    | type_qualifier_list type_qualifier
    '''
    print(p.lineno)


def p_parameter_type_list(p):
    '''parameter_type_list : parameter_list
    | parameter_list ',' ELLIPSIS
    '''
    print(p.lineno)


def p_parameter_list(p):
    '''parameter_list : parameter_declaration
    | parameter_list ',' parameter_declaration
    '''
    print(p.lineno)


def p_parameter_declaration(p):
    '''parameter_declaration : declaration_specifiers declarator
    | declaration_specifiers abstract_declarator
    | declaration_specifiers
    '''
    print(p.lineno)


def p_identifier_list(p):
    '''identifier_list : IDENTIFIER
    | identifier_list ',' IDENTIFIER
    '''
    print(p.lineno)


def p_type_name(p):
    '''type_name : specifier_qualifier_list
    | specifier_qualifier_list abstract_declarator
    '''
    print(p.lineno)


def p_abstract_declarator(p):
    '''abstract_declarator : pointer
    | direct_abstract_declarator
    | pointer direct_abstract_declarator
    '''
    print(p.lineno)


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
    print(p.lineno)


def p_initializer(p):
    '''initializer : assignment_expression
    | '{' initializer_list '}'
    | '{' initializer_list ',' '}'
    '''
    print(p.lineno)


def p_initializer_list(p):
    '''initializer_list : initializer
    | initializer_list ',' initializer
    '''
    print(p.lineno)


def p_statement(p):
    '''statement : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    '''
    print(p.lineno)


def p_labeled_statement(p):
    '''labeled_statement : IDENTIFIER ':' statement
    | CASE constant_expression ':' statement
    | DEFAULT ':' statement
    '''
    print(p.lineno)


def p_compound_statement(p):
    '''compound_statement : '{' '}'
    | '{' statement_list '}'
    | '{' declaration_list '}'
    | '{' declaration_list statement_list '}'
    '''
    print(p.lineno)


def p_declaration_list(p):
    '''declaration_list : declaration
    | declaration_list declaration
    '''
    print(p.lineno)


def p_statement_list(p):
    '''statement_list : statement
    | statement_list statement
    '''
    print(p.lineno)


def p_expression_statement(p):
    '''expression_statement : ';'
    | expression ';'
    '''
    print(p.lineno)


def p_selection_statement(p):
    '''selection_statement : IF '(' expression ')' statement
    | IF '(' expression ')' statement ELSE statement
    | SWITCH '(' expression ')' statement
    '''
    print(p.lineno)


def p_iteration_statement(p):
    '''iteration_statement : WHILE '(' expression ')' statement
    | DO statement WHILE '(' expression ')' ';'
    | FOR '(' expression_statement expression_statement ')' statement
    | FOR '(' expression_statement expression_statement expression ')' statement
    '''
    print(p.lineno)


def p_jump_statement(p):
    '''jump_statement : GOTO IDENTIFIER ';'
    | CONTINUE ';'
    | BREAK ';'
    | RETURN ';'
    | RETURN expression ';'
    '''
    print(p.lineno)


def p_translation_unit(p):
    '''translation_unit : external_declaration
    | translation_unit external_declaration
    '''
    print(p.lineno)


def p_external_declaration(p):
    '''external_declaration : function_definition
    | declaration
    '''
    print(p.lineno)


def p_function_definition(p):
    '''function_definition : declaration_specifiers declarator declaration_list compound_statement
    | declaration_specifiers declarator compound_statement
    | declarator declaration_list compound_statement
    | declarator compound_statement
    '''
    print(p.lineno)


def p_error(token):
    if token is not None:
        print("Line %s, illegal token %s" % (token.lineno, token.value))
    else:
        print('Unexpected end of input')


# lexer = lex.lex()
# fh = open(sys.argv[1], "r")
# lexer.input(fh.read())
# for token in lexer:
    # print("line %d: %s(%s)" % (token.lineno, token.type, token.value))
#

logging.basicConfig(
    level=logging.DEBUG,
    filename="parselog.txt",
    filemode="w",
    format="%(filename)10s:%(lineno)4d:%(message)s"
)
lexer = lex.lex()
log = logging.getLogger()
parser = yacc.yacc(debug=True)
fh = open(sys.argv[1], "r")
text = fh.read()
parser.parse(text, lexer=lexer)
