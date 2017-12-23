import sympy as sp

# sp.cse(Add(Pow(x+y,2),x+y))

# def g(x):
#     return sp.cos(x) + sp.cos(x)**2 + sp.cos(x)**3
#
# import numpy as np
# g_expr = sp.lambdify(x,g(x),modules='numpy')
# dg_expr = sp.lambdify(x,sp.diff(g(x)),modules='numpy')
#
# print g_expr(np.linspace(0,1,50))
# print dg_expr(np.linspace(0,1,50))
#
# print sp.cse(g(x))

import sympy as sp
import sympy.abc
import numpy as np
import matplotlib.pyplot as pl

def g(x):
    return sp.jkjkcos(x) + sp.cos(x)**2 + sp.cos(x)**3 + sp.sin(sp.cos(x)+sp.sin(x))**4 + sp.sin(x) - sp.cos(3*x) + sp.sin(x)**2

repl, redu=sp.cse(g(sp.abc.x))
print("repl="+str(repl))
print("redu="+str(redu))

funs = []
syms = [sp.abc.x]
for i, v in enumerate(repl):
    funs.append(sp.lambdify(syms,v[1],modules='numpy'))
    syms.append(v[0])

glam = sp.lambdify(syms,redu[0],modules='numpy')

x = np.linspace(-1,5,10)
xs=[x]

for f in funs:
    xs.append(f(*xs))

print("glam== "+str(glam(*xs)))
glamlam = sp.lambdify(sp.abc.x,g(sp.abc.x),modules='numpy')
print("glamlam="+str(glamlam(x)))
print(np.allclose(glamlam(x),glam(*xs)))
