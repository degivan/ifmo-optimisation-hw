from math import exp, sqrt

N = 58
k = 2.0 + 0.1 * N
EPS = 10 ** (-5)
MAX = 10 ** 4


def f(x):
    return exp(sqrt(x)) + k * exp(-7.8 * x)


def passive_search(f, a, b, k):
    min_f = f(a)
    min_x = a
    for i in range(k):
        x = a + (b - a) * i / float(k)
        val_f = f(x)
        if val_f < min_f:
            min_f = val_f
            min_x = x
    return min_x, k, 2 * k


def dichotomy_method(f, a, b, delta, eps):
    start = a
    fin = b
    it_count = 0
    while (fin - start) / 2.0 > eps:
        c = (start + fin - delta) / 2.0
        d = (start + fin + delta) / 2.0
        if f(c) <= f(d):
            fin = d
        else:
            start = c
        it_count += 1
    return (start + fin) / 2.0, it_count, it_count * 2


C_CONST = (3 - sqrt(5)) / 2.0
D_CONST = (sqrt(5) - 1) / 2.0


def golden_ratio_method(f, a, b, eps):
    it_count = 0
    call_count = 0

    fc = None
    fd = None
    start = a
    fin = b
    c = C_CONST * (fin - start) + start
    d = D_CONST * (fin - start) + start
    while (fin - start) / 2.0 > eps:
        it_count += 1
        if fc is None:
            fc = f(c)
            call_count += 1
        if fd is None:
            fd = f(d)
            call_count += 1
        if fc <= fd:
            fin = d
            d = c
            fd = fc
            fc = None
            c = C_CONST * (fin - start) + start
        else:
            start = c
            c = d
            fc = fd
            fd = None
            d = D_CONST * (fin - start) + start
    return (start + fin) / 2.0, it_count, call_count


def fibonacci_method(f, a, b, eps):
    n = 0
    call_count = 0
    fibs = {1: 1, 2: 1}
    while fibs[n + 2] < (b - a) / eps:
        n += 1
        fibs[n + 2] = fibs[n] + fibs[n + 1]

    c = None
    d = None
    fc = None
    fd = None
    for i in range(1, n):
        if c is None:
            c = a + (b - a) * fibs[n + 1 - i] / fibs[n + 3 - i]
            fc = f(c)
            call_count += 1
        if d is None:
            d = a + (b - a) * fibs[n + 2 - i] / fibs[n + 3 - i]
            fd = f(d)
            call_count += 1
        if fc <= fd:
            b = d
            d, fd = c, fc
            c = None
        else:
            a = c
            c, fc = d, fd
            d = None
    return (a + b) / 2.0, n - 1, call_count


if __name__ == '__main__':
    print(passive_search(f, EPS, MAX, 10 ** 6))
    print(dichotomy_method(f, EPS, MAX, EPS / 2, EPS))
    print(golden_ratio_method(f, EPS, MAX, EPS))
    print(fibonacci_method(f, EPS, MAX, EPS))
