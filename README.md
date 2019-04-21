Implements basic arithmetic and data-structures using only javascript function
calls. Data is represented using
[Church Encoding](https://en.wikipedia.org/wiki/Church_encoding).

All computation uses only single-argument javascript function calls. There are
also "break-glass" functions which help convert to standard type for input and
output.

* `church.js` implements basic arithmetic and types.
* `is_prime.js` is a consise, stand-alone prime checker. It checks if the number
   is divisible by any numbers in \[2, n).
* `is_prime_faster.js` is a faster prime checker. However, numbers are no longer
  represented as repeated function application, but rather a list of bits. It
  checks if the number is even or divisible by any odd numbers in \[3, sqrt(n)\].
