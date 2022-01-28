from symbols import *

A = 'A'
B = 'B'
C = 'C'
BC = 'BC'
CD = 'CD'
HL = 'HL'
AF = 'AF'


def adr(identifier):
    return '[$' + hex(symbols_table[identifier])[2:] + ']'

def raw_adr(identifier):
    return '$' + hex(symbols_table[identifier])[2:]


def push(code, rr):
    code += f'''
    push {rr}'''


def pushA(code):
    ld(code, B, A)
    push(code, BC)


def pop(code, rr):
    code += f'''
    pop {rr}'''


def popBC(code):
    pop(code, BC)


def ld(code, x, y):
    code += f'''
    ld {x}, {y}'''


def zeroA(code):
    code += f'''
    xor a'''


def add(code, x):
    code += f'''
    add A, {x}'''

def add_HL(code, x):
    code += f'''
    add HL, {x}'''


def sub(code, x):
    code += f'''
    sub A, {x}'''


def b_and(code, x):
    code += f'''
    and A, {x}'''


def b_or(code, x):
    code += f'''
    or A, {x}'''


def b_xor(code, x):
    code += f'''
    xor A, {x}'''


def l_and(code):
    b_and(code, 1)


def l_or(code):
    b_or(code, 1)


def jump(code, label):
    code += f'''
    jp .l{label}'''


def jmp(code, flag, label):
    code += f'''
    jp {flag}, .l{label}'''


def jump_zero(code, label):
    code += f'''
    jp z, .l{label}'''


def jump_non_zero(code, label):
    code += f'''
    jp nz, .l{label}'''


def cmp_a(code, x):
    code += f'''
    cp {x}'''


def label(code, label):
    code += f'''
.l{label}'''


class Code:
    def __init__(self, string) -> None:
        self.code = string
        pass

    def __iadd__(self, other):
        self.code = self.code + other
        return self.code

    def __str__(self) -> str:
        return self.code
