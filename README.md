# SHOULD-TEST -- you should test even Common Lisp code sometimes

`SHOULD-TEST` is a minimal yet feature-rich Common Lisp test framework.

`SHOULD-TEST` is methodology-agnostic and non-opinionated,
i.e. it doesn't care what kind of test approach you'd like to take
(like, unit testing, random testing a la quickcheck or something else).

It cares about the things that a test framework should make easy:

- defining and running arbitrary tests
- analyzing the test output
- piping the test output to upstream systems, like CI
  (by supporting common protocols, such as xUnit & TAP) - coming soon :)

## Usage

### Defining tests

Test are defined with `deftest`:

    (deftest some-fn ()
      (should be = 1 (some-fn 2))
      (should be = 2 (some-fn 1)))

Each individual form in test's body is treated as an assertion
that should be true. If it doesn't satisfy, it is expected
that 2 values will be returned:

- `NIL` to indicate that assertion has failed
- a list of an expression being tested, expected and actual outputs

Being run, `deftest` returns either `T` or `NIL` as primary value.
Secondary and third values in case of `NIL` are lists of:

- all failed assertions returned by individual assertions
- and all uncaught errors signalled inside assertions

`should` is a macro that takes care of checking assertions and following
this protocol. Under the hood it calls the generic function `should-check`
and passes it a keyword produced from the first symbol (in this case, `:be`),
a test predicate (here, `'=`), and a tested expression as thunk
(here it will be e.g. `(lambda () (some-fn 1))`), and expected results, if any.
If multiple expected results are given, like in
`(should be eql nil #{:failed 1} (some-other-fn :dummy))`,
it means that multiple `values` are expected.

As you see, the keyword and test predicate are passed unevaluated,
so you can't use expressions here.

The pre-defined types of assertions are `be`, `signal` and `print-to`.
They check correspondingly:

- `be` - that some predicate holds
- `signal` - some condition is signalled
- `print-to` - some text is printed to the following stream

Obviously, it's possible to define more assertion types
as methods of `should-check` generic function.

`deftest` and `should` write summary of test results to `*test-output*`
(by default bound to `*standard-output*`).
The var `*verbose*` (default `T`) controls if the summary contains
full failure reports or just test names.

Tests are defined as lambda-functions attached to a symbol's `test` property,
so `(deftest some-fn ...` will do the following:

    (setf (get some-fn 'test)
          (lambda () ...))

### Running tests

To run the tests use `test`. Without arguments it runs all the tests
in the current package. Given a `:package` argument it will do the same
for that package, and given a `:test` argument it will run that individual test.

In case of individual test's failure it will return `NIL`
and a list of failed assertions and a list of assertions,
that triggered uncaught errors.

In case of failed test of a package it will return `NIL`
and 2 hash-tables holding the same lists as above keyed by failed test's names.

As you see, the system uses a somewhat recursive protocol for test results:

- at the lowest level `should` returns `T` or `NIL`
  and information about the failed assertion
- this information is aggregated by `deftest` which will return
  aggregate information about all the failed assertions in the hash-table
- at the highest level `test` will once again aggregate information
  over all tests

So the structure of the summary, returned from `test`, will be the following:

    #{
      failed-test-1 ((failed-assertion-1 expected actual)
                     (failed-assertion-2 ...
      failed-test-2 ...
     }

(`#{}` are `rutils` literal hash-table delimiters)

### Usage patterns

As `SHOULD-TEST` is agnostic, it doesn't impose any restrictions on
how each individual project organizes its tests. Yet, having established
patterns and best-practices never hearts. This section collects some of them.

There's no restriction on naming tests. Though it seems like a good approach
to name them the same as functions they test. As for generic functions,
it, probably, makes sense to have different tests for different methods.
In this case I add some suffix to the test's name to indicate which method
is tested (like `transform-string` for one of the methods of gf `transform`
that is specialized for the `string` class of arguments).

As for code organization, I use the following directory structure
of the typical project:

## Self-test

There's a minimal test suite defined in `src/self-test.lisp`.
The test suite is also hooked to `asdf:test-op` for the `should-test` system.


## License

 Copyright (c) 2013 Vsevolod Dyomkin <vseloved@gmail.com>

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 Except as contained in this notice, the name(s) of the above
 copyright holders shall not be used in advertising or otherwise
 to promote the sale, use or other dealings in this Software
 without prior written authorization.

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.


  [ql]: http://quicklisp.org
  [asdf]: http://common-lisp.net/project/asdf
