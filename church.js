////////////////////////////////////////////////////////////////////////
// Bool
////////////////////////////////////////////////////////////////////////

let yes = t => f => t;

let no = t => f => f;

////////////////////////////////////////////////////////////////////////
// Bool functions
////////////////////////////////////////////////////////////////////////

let not = a => a(no)(yes);

let and = a => b => a(b)(no);

let or = a => b => a(yes)(b);

let xor = a => b => a(not(b))(b);

////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////

let if_then_else = cond => t => f => cond(t)(f)();

// Identity.
let id = a => a;

// Y-combinator.
let y = f =>
    (x => x(x))(
        x => f(y => x(x)(y))
    );

////////////////////////////////////////////////////////////////////////
// Pair
////////////////////////////////////////////////////////////////////////

let pair = x => y => f => f(x)(y);

let fst = p => p(x => y => x);

let snd = p => p(x => y => y);

let flip = p => p(x => y => pair(y)(x));

////////////////////////////////////////////////////////////////////////
// List
////////////////////////////////////////////////////////////////////////
//
// l(f)(z) = fold(f, l, z).
// For l = [a, b, c]. l(f)(z) = f(a, f(b, f(c, z)))

let nil = f => z => z;

let cons = h => t => f => z => f(h)(t(f)(z));

let is_nil = l => l(h => t => no)(yes);

let head = l => l(h => t => h)(no);

let tail = l => f => z => snd(l(
    h => t => pair(h)(t(f))
)(_ => z));

////////////////////////////////////////////////////////////////////////
// List functions
////////////////////////////////////////////////////////////////////////

let filter = f => l => (
    l(h => t =>
        f(h)
            (cons(h)(t))
            (t)
    )
    (nil)
);

let concat = k => l => k(cons)(l);

let sort = lt => l => (
    l(h => t =>
        concat
            (filter(v => not(lt(h)(v)))(t))
            (cons
                (h)
                (filter(lt(h))(t))
            )
    )(nil)
);

////////////////////////////////////////////////////////////////////////
// Natural numbers
////////////////////////////////////////////////////////////////////////
//
// n(f) = f^n. n is a function that takes a function and iterates it
//             n times. f^0 = id, i.e. 0 iterates 0 times giving the
//             identify function.

let zero = f => x => x;

let succ = n => f => x => f(n(f)(x));

let one = succ(zero);
let two = succ(one);
let three = succ(two);

////////////////////////////////////////////////////////////////////////
// Basic natural number functions
////////////////////////////////////////////////////////////////////////

let add = n => m => m(succ)(n);

let mul = n => m => f => m(n(f));

let pow = n => m => m(n);

let is_zero = n => n(x => no)(yes);

// Define pred, inverse of succ.
let _self_and_succ = x => pair(x)(succ(x));
let _shift = p => _self_and_succ(snd(p));
let pred = n => fst(n(_shift)(pair(zero)(zero)));

// Subtract, truncated to zero.
let sub = n => m => m(pred)(n);

////////////////////////////////////////////////////////////////////////
// Natural number functions
////////////////////////////////////////////////////////////////////////

// fac(n) = n!
let fac = y(
    f => n =>
        if_then_else
            (is_zero(n))
            (() => one)
            (() => mul(n)(f(pred(n))))
);

let greater_than = x => y => not(is_zero(sub(x)(y)));

// div(x, y)
let div = y(
    f => x => y =>
        if_then_else
            (greater_than(y)(x))
            (() => zero)
            (() => succ(f(sub(x)(y))(y)))
);

// mod(x, y)
let mod = y(
    f => x => y =>
        if_then_else
            (greater_than(y)(x))
            (() => x)
            (() => f(sub(x)(y))(y))
);

let is_prime = n => (
    if_then_else
        (greater_than(two)(n))
        (() => no)
        (() =>
            y(f => i =>
                if_then_else
                    (greater_than(n)(i))
                    (() => 
                        if_then_else
                            (is_zero(mod(n)(i)))
                            (() => no)
                            (() => f(succ(i)))
                    )
                    (() => yes)
            )(two)
        )
);

// f(a,n) Fast-growing heirarchy.
let f = y(
    f => a => n =>
        is_zero(a)
            (() => succ(n))
            (() => n(f(pred(a)))(n))
            ()
);

////////////////////////////////////////////////////////////////////////
// Integers
////////////////////////////////////////////////////////////////////////
//
// n = a-b = pair(a)(b) where a and b are naturals.

let nat_to_int = n => pair(n)(zero);

let inegate = n => flip(n);

let iadd = n => m => (
    pair
        (add(fst(n))(fst(m)))
        (add(snd(n))(snd(m)))
);

let isub = n => m => iadd(n)(inegate(m));

let imul = n => m => (
    pair
        (
            add
                (mul(fst(n))(fst(m)))
                (mul(snd(n))(snd(m)))
        )
        (
            add
                (mul(fst(n))(snd(m)))
                (mul(snd(n))(fst(m)))
        )
);

let igreater_than = n => m => (
    greater_than
        (add(fst(n))(snd(m)))
        (add(snd(n))(fst(m)))
);

////////////////////////////////////////////////////////////////////////
// Rationals
////////////////////////////////////////////////////////////////////////
//
// x = a/b = pair(a)(b) where a and b are integers.

let int_to_rat = i => pair(i)(nat_to_int(one));

let rinvert = x => flip(x);

let rnegate = x => pair(inegate(fst(x)))(snd(x));

let rmul = x => y => (
    pair
        (imul(fst(x))(fst(y)))
        (imul(snd(x))(snd(y)))
);

let radd = x => y => (
    pair
        (
            iadd
                (imul(fst(x))(snd(y)))
                (imul(snd(x))(fst(y)))
        )
        (
            (imul(snd(x))(snd(y)))
        )
);

let rsub = x => y => radd(x)(rnegate(y));

let rdiv = x => y => rmul(x)(rinvert(y));

let rgreater_than = x => y => (
    igreater_than
        (imul(fst(x))(snd(y)))
        (imul(snd(x))(fst(y)))   
);

////////////////////////////////////////////////////////////////////////
// Break glass functions
////////////////////////////////////////////////////////////////////////

let ShowBool = b => b(true)(false);
let ToBool = b => b ? yes : no;

let ShowNat = n => n(x => x+1)(0);
let ToNat = n => n ? succ(ToNat(n-1)) : zero;

let ShowInt = n => ShowNat(fst(n)) - ShowNat(snd(n));
let ToInt = n => n < 0 ? inegate(ToInt(-n)) : nat_to_int(ToNat(n));

let ShowRat = x => ShowInt(fst(x)) / ShowInt(snd(x));
let ToRat = (n, m) => pair(ToInt(n))(ToInt(m||1));

let ShowList = l => l(h => t => [h].concat(t))([]);
let ToList = l => l.reverse().reduce(((a, v) => cons(v)(a)), nil);

// Decorates a function, printing the result whenever it is called.
let LOG = function(n, show) {
    print(show(n));
    return n;
};
let LOG_N = n => LOG(n, ShowNat);

// Decorates a function to be able to count the number of times it is called.
let COUNT = function(f) {
    let count = 0;
    return function(x, command) {
        if (command) {
            if (command == 'reset') {
                count = 0;
            }
            return count;
        }
        count += 1;
        return f(x);
    }
}

////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////

function assertEquals(expected, actual) {
    let j_expected = JSON.stringify(expected);
    let j_actual = JSON.stringify(actual);
    if (j_expected != j_actual) {
        throw new Error(`Test failed: ${j_expected} != ${j_actual}`);
    }
}

function assertFloatEquals(expected, actual) {
    let eps = 0.0000001
    if (expected < actual-eps || expected > actual+eps ) {
        throw new Error(`Test failed: ${expected} != ${actual}`);
    }
}

function assertEqualsBool(expected, actual) {
    assertEquals(expected, ShowBool(actual));
}

function assertEqualsList(expected, actual) {
    assertEquals(expected, ShowList(actual));
}

function assertEqualsNat(expected, actual) {
    assertEquals(expected, ShowNat(actual));
}

function assertEqualsInt(expected, actual) {
    assertEquals(expected, ShowInt(actual));
}

function assertEqualsRat(expected, actual) {
    assertFloatEquals(expected, ShowRat(actual));
}

function RunTests() {
    // Bools
    assertEqualsBool(true, yes);
    assertEqualsBool(false, no);
    assertEqualsBool(true, ToBool(true));
    assertEqualsBool(false, ToBool(false));

    assertEqualsBool(false, not(yes));
    assertEqualsBool(true, not(no));

    assertEqualsBool(true, and(yes)(yes));
    assertEqualsBool(false, and(yes)(no));
    assertEqualsBool(false, and(no)(yes));
    assertEqualsBool(false, and(no)(no));

    assertEqualsBool(true, or(yes)(yes));
    assertEqualsBool(true, or(yes)(no));
    assertEqualsBool(true, or(no)(yes));
    assertEqualsBool(false, or(no)(no));

    assertEqualsBool(false, xor(yes)(yes));
    assertEqualsBool(true, xor(yes)(no));
    assertEqualsBool(true, xor(no)(yes));
    assertEqualsBool(false, xor(no)(no));

    // Pair
    assertEquals(true, fst(pair(true)(false)));
    assertEquals(false, snd(pair(true)(false)));
    assertEquals(false, fst(flip(pair(true)(false))));
    assertEquals(true, snd(flip(pair(true)(false))));

    // List
    assertEqualsList([], ToList([]));
    assertEqualsList([3, 1, 2], ToList([3, 1, 2]));
    assertEqualsList([], nil);
    assertEqualsList([3, 1, 2], cons(3)(cons(1)(cons(2)(nil))));
    assertEqualsBool(true, is_nil(nil));
    assertEqualsBool(false, is_nil(cons(1)(nil)));

    assertEquals(3, head(ToList([3, 1, 2])));
    assertEqualsBool(false, head(nil));
    assertEqualsList([1, 2], tail(ToList([3, 1, 2])));
    assertEqualsList([], tail(ToList([3])));
    assertEqualsList([], tail(nil));

    // List functions
    assertEqualsList([], filter(x => yes)(nil));
    assertEqualsList([2, 3, 1, 4], filter(x => ToBool(x < 5))(ToList([2, 3, 1, 4])));
    assertEqualsList([2, 1], filter(x => ToBool(x < 3))(ToList([2, 3, 1, 4])));
    assertEqualsList([], filter(x => ToBool(x < 0))(ToList([2, 3, 1, 4])));

    assertEqualsList([2, 3], concat(ToList([2, 3]))(nil));
    assertEqualsList([1, 4], concat(nil)(ToList([1, 4])));
    assertEqualsList([2, 3, 1, 4], concat(ToList([2, 3]))(ToList([1, 4])));

    assertEqualsList([], sort(x => y => ToBool(x < y))(nil));
    assertEqualsList([1, 2, 3, 4], sort(x => y => ToBool(x < y))(ToList([2, 3, 1, 4])));

    // Naturals
    assertEqualsNat(0, zero);
    assertEqualsNat(1, one);
    assertEqualsNat(2, two);
    assertEqualsNat(3, three);
    assertEqualsNat(0, ToNat(0));
    assertEqualsNat(5, ToNat(5));

    assertEqualsNat(1, succ(zero));
    assertEqualsNat(4, succ(three));

    // Basic natural functions
    assertEqualsNat(3, add(zero)(three));
    assertEqualsNat(2, add(two)(zero));
    assertEqualsNat(5, add(two)(three));
    assertEqualsNat(5, add(three)(two));

    assertEqualsNat(0, mul(zero)(three));
    assertEqualsNat(0, mul(two)(zero));
    assertEqualsNat(6, mul(two)(three));
    assertEqualsNat(6, mul(three)(two));

    assertEqualsNat(0, pow(zero)(three));
    assertEqualsNat(1, pow(two)(zero));
    assertEqualsNat(8, pow(two)(three));
    assertEqualsNat(9, pow(three)(two));

    assertEqualsBool(true, is_zero(zero));
    assertEqualsBool(false, is_zero(one));
    assertEqualsBool(false, is_zero(three));

    assertEqualsNat(0, pred(one));
    assertEqualsNat(2, pred(three));
    assertEqualsNat(0, pred(zero));

    assertEqualsNat(3, sub(three)(zero));
    assertEqualsNat(1, sub(three)(two));
    assertEqualsNat(0, sub(three)(three));
    assertEqualsNat(0, sub(two)(three));

    // Natural functions
    assertEqualsNat(1, fac(zero));
    assertEqualsNat(1, fac(one));
    assertEqualsNat(2, fac(two));
    assertEqualsNat(6, fac(three));

    assertEqualsBool(true, greater_than(one)(zero));
    assertEqualsBool(false, greater_than(zero)(one));
    assertEqualsBool(false, greater_than(zero)(zero));
    assertEqualsBool(false, greater_than(three)(three));
    assertEqualsBool(true, greater_than(three)(one));
    assertEqualsBool(false, greater_than(one)(three));

    assertEqualsNat(0, div(zero)(one));
    assertEqualsNat(1, div(one)(one));
    assertEqualsNat(1, div(three)(three));
    assertEqualsNat(0, div(one)(three));
    assertEqualsNat(1, div(three)(three));
    assertEqualsNat(1, div(three)(two));
    assertEqualsNat(4, div(ToNat(29))(ToNat(6)));
    assertEqualsNat(5, div(ToNat(30))(ToNat(6)));
    assertEqualsNat(5, div(ToNat(31))(ToNat(6)));

    assertEqualsNat(0, mod(zero)(one));
    assertEqualsNat(0, mod(one)(one));
    assertEqualsNat(0, mod(three)(three));
    assertEqualsNat(1, mod(one)(three));
    assertEqualsNat(0, mod(three)(three));
    assertEqualsNat(1, mod(three)(two));
    assertEqualsNat(5, mod(ToNat(29))(ToNat(6)));
    assertEqualsNat(0, mod(ToNat(30))(ToNat(6)));
    assertEqualsNat(1, mod(ToNat(31))(ToNat(6)));

    assertEqualsBool(false, is_prime(ToNat(0)));
    assertEqualsBool(false, is_prime(ToNat(1)));
    assertEqualsBool(true, is_prime(ToNat(2)));
    assertEqualsBool(true, is_prime(ToNat(3)));
    assertEqualsBool(false, is_prime(ToNat(4)));
    assertEqualsBool(false, is_prime(ToNat(9)));
    assertEqualsBool(true, is_prime(ToNat(11)));

    assertEqualsNat(1, f(ToNat(0))(ToNat(0)));
    assertEqualsNat(2, f(ToNat(0))(ToNat(1)));
    assertEqualsNat(6, f(ToNat(0))(ToNat(5)));
    assertEqualsNat(0, f(ToNat(1))(ToNat(0)));
    assertEqualsNat(2, f(ToNat(1))(ToNat(1)));
    assertEqualsNat(10, f(ToNat(1))(ToNat(5)));
    assertEqualsNat(0, f(ToNat(2))(ToNat(0)));
    assertEqualsNat(2, f(ToNat(2))(ToNat(1)));
    assertEqualsNat(160, f(ToNat(2))(ToNat(5)));

    // Integers
    assertEqualsInt(0, ToInt(0));
    assertEqualsInt(1, ToInt(1));
    assertEqualsInt(5, ToInt(5));
    assertEqualsInt(-1, ToInt(-1));
    assertEqualsInt(-5, ToInt(-5));

    assertEqualsInt(0, nat_to_int(zero));
    assertEqualsInt(1, nat_to_int(one));
    assertEqualsInt(2, nat_to_int(two));

    assertEqualsInt(0, inegate(ToInt(0)));
    assertEqualsInt(2, inegate(ToInt(-2)));
    assertEqualsInt(-2, inegate(ToInt(2)));

    assertEqualsInt(12, iadd(pair(ToNat(16))(ToNat(11)))
                            (pair(ToNat(21))(ToNat(14))));
    assertEqualsInt(0, iadd(ToInt(0))(ToInt(0)));
    assertEqualsInt(2, iadd(ToInt(0))(ToInt(2)));
    assertEqualsInt(2, iadd(ToInt(2))(ToInt(0)));
    assertEqualsInt(-2, iadd(ToInt(0))(ToInt(-2)));
    assertEqualsInt(0, iadd(ToInt(2))(ToInt(-2)));
    assertEqualsInt(12, iadd(ToInt(5))(ToInt(7)));
    assertEqualsInt(3, iadd(ToInt(5))(ToInt(-2)));
    assertEqualsInt(-2, iadd(ToInt(5))(ToInt(-7)));

    assertEqualsInt(-2, isub(pair(ToNat(16))(ToNat(11)))
                            (pair(ToNat(21))(ToNat(14))));
    assertEqualsInt(0, isub(ToInt(0))(ToInt(0)));
    assertEqualsInt(-2, isub(ToInt(0))(ToInt(2)));
    assertEqualsInt(2, isub(ToInt(2))(ToInt(0)));
    assertEqualsInt(2, isub(ToInt(0))(ToInt(-2)));
    assertEqualsInt(0, isub(ToInt(2))(ToInt(2)));
    assertEqualsInt(4, isub(ToInt(2))(ToInt(-2)));
    assertEqualsInt(-2, isub(ToInt(5))(ToInt(7)));
    assertEqualsInt(7, isub(ToInt(5))(ToInt(-2)));
    assertEqualsInt(3, isub(ToInt(5))(ToInt(2)));

    assertEqualsInt(35, imul(pair(ToNat(16))(ToNat(11)))
                            (pair(ToNat(21))(ToNat(14))));
    assertEqualsInt(0, imul(ToInt(0))(ToInt(0)));
    assertEqualsInt(0, imul(ToInt(0))(ToInt(2)));
    assertEqualsInt(0, imul(ToInt(2))(ToInt(0)));
    assertEqualsInt(3, imul(ToInt(1))(ToInt(3)));
    assertEqualsInt(3, imul(ToInt(3))(ToInt(1)));
    assertEqualsInt(12, imul(ToInt(3))(ToInt(4)));
    assertEqualsInt(-12, imul(ToInt(3))(ToInt(-4)));
    assertEqualsInt(-12, imul(ToInt(-3))(ToInt(4)));
    assertEqualsInt(12, imul(ToInt(-3))(ToInt(-4)));

    assertEqualsBool(false, igreater_than(pair(ToNat(16))(ToNat(11)))
                                         (pair(ToNat(8))(ToNat(1))));
    assertEqualsBool(false, igreater_than(ToInt(0))(ToInt(0)));
    assertEqualsBool(true, igreater_than(ToInt(1))(ToInt(0)));
    assertEqualsBool(false, igreater_than(ToInt(0))(ToInt(1)));
    assertEqualsBool(true, igreater_than(ToInt(0))(ToInt(-1)));
    assertEqualsBool(false, igreater_than(ToInt(9))(ToInt(9)));
    assertEqualsBool(true, igreater_than(ToInt(6))(ToInt(4)));
    assertEqualsBool(false, igreater_than(ToInt(4))(ToInt(6)));

    // Rationals
    assertEqualsRat(0, ToRat(0));
    assertEqualsRat(3, ToRat(3));
    assertEqualsRat(1/2, ToRat(1, 2));
    assertEqualsRat(1/2, ToRat(6, 12));
    assertEqualsRat(5/3, ToRat(5, 3));

    assertEqualsRat(0, int_to_rat(ToInt(0)));
    assertEqualsRat(-1, int_to_rat(ToInt(-1)));
    assertEqualsRat(2, int_to_rat(ToInt(2)));

    assertEqualsRat(1, rinvert(ToRat(1, 1)));
    assertEqualsRat(3/5, rinvert(ToRat(5, 3)));
    assertEqualsRat(5/3, rinvert(ToRat(3, 5)));

    assertEqualsRat(0, rnegate(ToRat(0, 1)));
    assertEqualsRat(-1, rnegate(ToRat(1, 1)));
    assertEqualsRat(-5/3, rnegate(ToRat(5, 3)));
    assertEqualsRat(5/3, rnegate(ToRat(5, -3)));

    assertEqualsRat(5/6, radd(ToRat(1, 2))(ToRat(1, 3)));
    assertEqualsRat(4/3, radd(ToRat(2))(ToRat(-2, 3)));
    assertEqualsRat(0, radd(ToRat(3, 2))(ToRat(-3, 2)));

    assertEqualsRat(1/6, rsub(ToRat(1, 2))(ToRat(1, 3)));
    assertEqualsRat(0, rsub(ToRat(3, 2))(ToRat(3, 2)));

    assertEqualsRat(3/10, rmul(ToRat(3, 5))(ToRat(4, 8)));
    assertEqualsRat(-3/10, rmul(ToRat(-3, 5))(ToRat(-4, -8)));
    assertEqualsRat(0, rmul(ToRat(0, 10))(ToRat(3)));
    assertEqualsRat(1/2, rmul(ToRat(2, 2))(ToRat(1, 2)));

    assertEqualsRat(6/5, rdiv(ToRat(3, 5))(ToRat(4, 8)));
    assertEqualsRat(-6/5, rdiv(ToRat(-3, 5))(ToRat(-4, -8)));
    assertEqualsRat(0, rdiv(ToRat(0, 10))(ToRat(3)));
    assertEqualsRat(2, rdiv(ToRat(2, 2))(ToRat(1, 2)));

    assertEqualsBool(false, rgreater_than(ToRat(1, 2))(ToRat(1, 2)));
    assertEqualsBool(true, rgreater_than(ToRat(1, 2))(ToRat(2, 5)));
    assertEqualsBool(false, rgreater_than(ToRat(1, 2))(ToRat(3, 5)));

    print("All tests pass!");
}

RunTests();