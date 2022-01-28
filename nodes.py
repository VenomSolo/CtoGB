from cProfile import label
from filecmp import cmp
from mimetypes import init
from assembly import *


class Expr:
    pass


class NEXT:
    def __init__(self, next) -> None:
        self.next = next
        pass


class BinaryExpr(Expr):
    def __init__(self, l_op, op, r_op):
        self.l_op = l_op
        self.op = op
        self.r_op = r_op
        self.func = None

    def are_constant(self):
        return type(self.l_op) is INT_CONST and type(self.r_op) is IDENTIFIER

    def is_l_primary(self):
        return type(self.l_op) is INT_CONST or type(self.l_op) is IDENTIFIER

    def is_l_primary(self):
        return type(self.r_op) is INT_CONST or type(self.r_op) is IDENTIFIER

    pass


class PLUS_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)
        if op == '+':
            self.func = add
        else:
            self.func = sub

    def generate_code(self, code):

        if self.are_constant():
            ld(code, 'a', self.l_op.value()+self.r_op.value())
            pushA(code)
        else:
            self.r_op.generate_code(code)
            self.l_op.generate_code(code)
            zeroA(code)
            popBC(code)
            ld(code, A, B)
            popBC(code)
            self.func(code, B)
            pushA(code)


class MULT_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)


class SHIFT_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)


class REL_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)


class EQU_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)

    def generate_code(self, code):
        if self.are_constant():
            ld(code, 'a', self.l_op.value() == self.r_op.value())
            pushA(code)
        else:
            self.l_op.generate_code(code)
            self.r_op.generate_code(code)
            f = 'nz' if self.op == '==' else 'z'
            false = symbols_table.get_label()
            end = symbols_table.get_label()
            zeroA(code)
            popBC(code)
            ld(code, A, B)
            popBC(code)
            cmp_a(code, B)
            jmp(code, f, false)
            ld(code, A, 1)
            pushA(code)
            jump(code, end)
            label(code, false)
            ld(code, A, 0)
            pushA(code)
            label(code, end)


class AND_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)

    def generate_code(self, code):
        if self.are_constant():
            ld(code, 'a', self.l_op.value() & self.r_op.value())
            pushA(code)
        else:
            self.l_op.generate_code(code)
            self.r_op.generate_code(code)
            zeroA(code)
            popBC(code)
            ld(code, A, B)
            popBC(code)
            b_and(code, B)
            pushA(code)


class XOR_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)

    def generate_code(self, code):
        if self.are_constant():
            ld(code, 'a', self.l_op.value() ^ self.r_op.value())
            pushA(code)
        else:
            self.l_op.generate_code(code)
            self.r_op.generate_code(code)
            zeroA(code)
            popBC(code)
            ld(code, A, B)
            popBC(code)
            b_xor(code, B)
            pushA(code)


class IOR_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)

    def generate_code(self, code):
        if self.are_constant():
            ld(code, 'a', self.l_op.value() | self.r_op.value())
            pushA(code)
        else:
            self.l_op.generate_code(code)
            self.r_op.generate_code(code)
            zeroA(code)
            popBC(code)
            ld(code, A, B)
            popBC(code)
            b_or(code, B)
            pushA(code)


class LAND_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)


class LOR_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)


class COND_EXPR(BinaryExpr):
    def __init__(self, l_op, op, r_op):
        super().__init__(l_op, op, r_op)


class INT_CONST:
    def __init__(self, val):
        self.val = val

    def value(self):
        return self.val

    def generate_code(self, code):
        ld(code, A, self.val)
        pushA(code)

class ARRAY_DECL:
    def __init__(self, direct, size):
        self.direct = direct
        self.arr_size = size

    def size(self):
        return self.arr_size.value()

    def get_name(self):
        return self.direct.get_name()

class ARRAY:
    def __init__(self, identifier, offset):
        self.identifier = identifier
        self.offset = offset

    def generate_code(self, code):
        self.offset.generate_code(code)
        ld(code, HL, raw_adr(self.identifier.value()))
        popBC(code)
        ld(code, C, B)
        ld(code, B, 0)
        add_HL(code, BC)
        ld(code, A, '[' + HL + ']')
        pushA(code)

    def generate_code_lval(self, code):
        self.offset.generate_code(code)
        ld(code, HL, raw_adr(self.identifier.value()))
        popBC(code)
        ld(code, C, B)
        ld(code, B, 0)
        add_HL(code, BC)
        push(code, HL)

    def get_name(self):
        return self.identifier.value();

class IDENTIFIER:
    def __init__(self, val):
        self.val = val

    def value(self):
        return self.val

    def generate_code(self, code):
        ld(code, A, adr(self.val))
        pushA(code)


class VAR_DECL:
    def __init__(self, id, type) -> None:
        self.id = id
        self.type = type


class STMT:
    def __init__(self, this, next) -> None:
        self.this = this
        self.next = next
        pass

    def generate_code(self, code):
        if self.next:
            self.next.generate_code(code)
        self.this.generate_code(code)


class LABEL_STMT:
    def __init__(self) -> None:
        pass


class STMT_LIST:
    def __init__(self) -> None:
        pass


class SELECT_STMT:
    def __init__(self) -> None:
        pass


class IF:
    def __init__(self, condition, true, false) -> None:
        self.condition = condition
        self.true = true
        self.false = false
        pass

    def generate_code(self, code):
        false = symbols_table.get_label()
        end = symbols_table.get_label()
        self.condition.generate_code(code)
        popBC(code)
        ld(code, A, B)
        cmp_a(code, 0)
        if self.false:
            jump_zero(code, false)
        else:
            jump_zero(code, end)
        self.true.generate_code(code)
        jump(code, end)
        label(code, false)
        if self.false:
            self.false.generate_code(code)
        label(code, end)


class ITER_STMT:
    def __init__(self) -> None:
        pass


class JUMP_STMT:
    def __init__(self) -> None:
        pass


class COMPOUND_STMT:
    def __init__(self, declarations, statements) -> None:
        self.declarations = declarations
        self.statements = statements

    def generate_code(self, code):
        if self.declarations:
            self.declarations.generate_code(code)
        if self.statements:
            self.statements.generate_code(code)


class EXPR_STMT:
    def __init__(self, expr) -> None:
        self.expr = expr

    def generate_code(self, code):
        self.expr.generate_code(code)


class ASSIGN_EXPR:
    def __init__(self, l_var, op, r_expr) -> None:
        self.l_var = l_var
        self.op = op
        self.r_expr = r_expr
        pass

    def generate_code(self, code):
        if self.op == '=':
            if type(self.l_var) is IDENTIFIER:
                self.r_expr.generate_code(code)
                popBC(code)
                ld(code, A, B)
                ld(code, adr(self.l_var.value()), A)
            if type(self.l_var) is ARRAY:
                self.r_expr.generate_code(code)
                self.l_var.generate_code_lval(code)
                pop(code, HL)
                pop(code, BC)
                ld(code, '[' + HL + ']',  B)
                pass

# DECLARATION


class DECLARATION:
    def __init__(self, decl_specs, init_decl_list) -> None:
        self.spec = decl_specs
        self.init = init_decl_list
        pass

    def generate_code(self, code):
        self.init.generate_code(code)


class DECL_LIST:
    def __init__(self, decl, next) -> None:
        self.decl = decl
        self.next = next
        pass

    def generate_code(self, code):
        if self.next:
            self.next.generate_code(code)
        self.decl.generate_code(code)


class DECL_SPEC:
    def __init__(self, val, next=None) -> None:
        self.val = val
        self.next = next
        pass


class TYPE_SPEC:
    def __init__(self, val) -> None:
        self.val = val
        pass


class TYPE_QUAL:
    def __init__(self, val) -> None:
        self.val = val
        pass


class STORAGE_CLASS:
    def __init__(self, val) -> None:
        self.val = val
        pass


class INIT_DECL:
    def __init__(self, decl, init) -> None:
        self.decl = decl
        self.init = init
        pass

    

    def generate_code(self, code):
        if self.init:
            self.init.generate_code(code)
            popBC(code)
            ld(code, A, B)
            ld(code, adr(self.decl.get_name()), A)


class DECLARATOR:
    def __init__(self, pointer, direct) -> None:
        self.pointer = pointer
        self.direct = direct
        self.size = 1 if type(self.direct) is DIRECT_DECLARATOR else self.direct.size()
        self.reg_symbol();
        pass

    def reg_symbol(self):
        symbols_table.add(self.get_name(), self.size)

    def get_name(self):
        return self.direct.get_name()


class DIRECT_DECLARATOR:
    def __init__(self, id) -> None:
        self.id = id
        pass

    def get_name(self):
        #print(self.id)
        return self.id


class EXTERNAL_DECL:
    def __init__(self, decl, next) -> None:
        self.decl = decl
        self.next = next
        pass

    def generate_code(self, code):
        if self.next:
            self.next.generate_code(code)
        self.decl.generate_code(code)


class FUNCTION:
    def __init__(self, body) -> None:
        self.body = body
        pass

    def generate_code(self, code):
        self.body.generate_code(code)


class WHILE:
    def __init__(self, condition, statement) -> None:
        self.condition = condition
        self.statement = statement
        pass

    def generate_code(self, code):
        loop = symbols_table.get_label()
        end = symbols_table.get_label()
        label(code, loop)
        self.condition.generate_code(code)
        popBC(code)
        ld(code, A, B)
        cmp_a(code, 0)
        jump_zero(code, end)
        self.statement.generate_code(code)
        jump(code, loop)
        label(code, end)

