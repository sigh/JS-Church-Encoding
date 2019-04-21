let is_prime = n => (
    (yes => no => s => pair => y =>
        (pred =>
            (gt =>
                (mod =>
                    gt(s(s(no)))(n)
                        (no)
                        (y(f => i =>
                                gt(n)(i)
                                    (() => 
                                        gt(mod(n)(i))(no)
                                            (() => f(s(i)))
                                            (() => no)
                                            ()
                                    )
                                    (() => yes)
                                    ()
                            )(s(s(no)))
                        )
                )
                (y(f => x => y =>
                    gt(y)(x)
                        (() => x)
                        (() => f(y(pred)(x))(y))
                        ()
                ))
            )(x => y =>
                (n => n(x => yes)(no))
                (y(pred)(x))
            )
        )(n => (
            n
                (p => pair(p(no))(s(p(no))))
                (pair(no)(no))
        )(yes))
    )
    (t => f => t)
    (t => f => f)
    (n => f => x => f(n(f)(x)))
    (x => y => f => f(x)(y))
    (f =>
        (x => x(x))(
            x => f(y => x(x)(y))
        )
    )
);

////////////////////////////////////////////////////////////////////////
// Break the glass
////////////////////////////////////////////////////////////////////////

let ShowBool = b => b(true)(false);
let ToNat = n => (f => x => n ? f(ToNat(n-1)(f)(x)) : x);
let IsPrime = n => ShowBool(is_prime(ToNat(n)));

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
    let n = ToNat(a);

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
    IsPrimeTime(23, true);
    IsPrimeTime(53, true);
    IsPrimeTime(103, true);
    IsPrimeTime(211, true);
}

RunTests();
RunBenchmarks();