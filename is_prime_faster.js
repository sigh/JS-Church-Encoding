is_prime = n =>
    (
        yes =>
        no =>
        not =>
        y =>
        (
            base_number =>
            prepend =>
            head =>
            tail =>
            is_base =>
            xor =>
            add_single_carry =>
            (
                zero =>
                one =>
                two =>
                add =>
                double =>
                double1 =>
                greater_than =>
                (
                    negate =>
                    (   
                        wrap =>
                        mod =>
                            wrap(
                                y(fn => i => i2 =>
                                    greater_than(i2)(n)
                                        (() => yes)
                                        (() => greater_than(mod(n)(i))(zero)
                                            (() => fn
                                                (add(i)(two))
                                                (add(i2)(double1(double(i))))
                                            )
                                            (() => no)
                                            ()
                                        )
                                        ()
                                )
                            )
                    )
                    // wrap(fn)
                    (fn =>
                        greater_than(two)(n)
                            (no)
                            (greater_than(double(two))(n)
                                (yes)
                                (not(head(n))
                                    (() => no)
                                    (() => fn
                                        (double1(one))
                                        (double1(double(two)))
                                    )
                                    ()
                                )
                            )
                    )
                    // mod(a, b) - only positive
                    (y(fn => a => b =>
                        greater_than(b)(a)
                            (() => a)
                            (() =>
                                (new_a =>
                                    greater_than(b)(new_a)
                                        (new_a)
                                        (add(new_a)(negate(b)))
                                )(fn(a)(double(b)))
                            )
                            ()
                    ))
                )
                // negate(a).
                (a =>
                    add
                        // flip(a) + 1
                        (y(fn => a =>
                            is_base(a)
                                (() => base_number(not(head(a))))
                                (() =>
                                    prepend
                                        (not(head(a)))
                                        (fn(tail(a)))
                                )
                                ()
                        )(a))
                        (one)
                )
            )
            // zero.
            (base_number(no))
            // one.
            (prepend(yes)(base_number(no)))
            // two.
            (prepend(no)(prepend(yes)(base_number(no))))
            // add(a, b).
            (y(fn => carry => a => b =>
                (new_value => new_carry =>
                    (is_base(a)
                        (is_base(b)
                            (not(xor(new_carry)(carry)))
                            (no)
                        )
                        (no)
                    )
                        (() => base_number(new_value))
                        (() => prepend
                            (new_value)
                            (fn(new_carry)(tail(a))(tail(b)))
                        )
                        ()
                )(xor(xor(head(a))(head(b)))(carry))
                 (add_single_carry(head(a))(head(b))(carry))
            )(no))
            // double(a) = 2*a
            (prepend(no))
            // double1(a) = 2*a + 1
            (prepend(yes))
            // greater_than(a, b) - only positives
            (y(fn => gt => a => b =>
                is_base(a)(is_base(b))(no)
                    (() => gt)
                    (() => fn
                        (xor(head(a))(head(b))
                            (head(a))
                            (gt)
                        )
                        (tail(a))
                        (tail(b))
                    )
                    ()
            )(no))
        )
        // base_number(value).
        (y(fn => value => selector => selector(value)(fn(value))(yes)))
        // prepend(el, list).
        (el => list => selector => selector(el)(list)(no))
        // head(list)
        (list => list(h => t => e => h))
        // tail(list)
        (list => list(h => t => e => t))
        // is_base(list)
        (list => list(h => t => e => e))
        // xor(a, b) - single bit
        (a => b => a(not(b))(b))
        // add_single_carry(a, b, c) - single bit
        (a => b => c => a(b(yes)(c))(b(c)(no)))
    )
    // yes.
    (t => f => t)
    // no.
    (t => f => f)
    // not.
    (fn => t => f => fn(f)(t))
    // y(fn).
    (le =>
        (f => f(f))(
            f => le(x => f(f)(x))
        )
    );

////////////////////////////////////////////////////////////////////////
// Break the glass
////////////////////////////////////////////////////////////////////////

ShowBool = b => b(true)(false);
ToBool = b => b ? (t => f => t) : (t => f => f);

ToInt = n => selector =>
    selector
        (ToBool(n & 1))
        (ToInt(n >> 1))
        (ToBool(!n));

let IsPrime = n => ShowBool(is_prime(ToInt(n)));

////////////////////////////////////////////////////////////////////////
// Test
////////////////////////////////////////////////////////////////////////

function IsPrimeTest(a, expected) {
    var result = IsPrime(a);
    if (result != expected) {
        throw("is_prime(" + a + ") != " + result);
    }
}

function RunTests() {
    IsPrimeTest(0, false);
    IsPrimeTest(1, false);
    IsPrimeTest(2, true);
    IsPrimeTest(3, true);
    IsPrimeTest(4, false);
    IsPrimeTest(5, true);
    IsPrimeTest(6, false);
    IsPrimeTest(7, true);
    IsPrimeTest(8, false);
    IsPrimeTest(9, false);

    IsPrimeTest(21, false);
    IsPrimeTest(23, true);

    print("All tests pass!");
}

function IsPrimeTime(a, expected) {
    let n = ToInt(a);

    let start_ms = (new Date()).getTime();
    let result = is_prime(n);
    let end_ms = (new Date()).getTime();    
    
    if (ShowBool(result) != expected) {
        throw("is_prime(" + a + ") != " + ShowBool(result));
    }

    let t = end_ms - start_ms;

    print(`is_prime(${a}): ${t}ms`);
}

function RunBenchmarks() {
    IsPrimeTime(11, true);
    IsPrimeTime(103, true);
    IsPrimeTime(1009, true);
    IsPrimeTime(10007, true);
    IsPrimeTime(100003, true);
    IsPrimeTime(1000003, true);
}

RunTests();
RunBenchmarks();