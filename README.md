`nbyte_int` – Custom Sized Integer Types


# Features
* Option<nbyte_int> is the same size as nbyte_int
* it has all byte variants of integers what else do you want?
* a constant macro ig


# Constant Macros
Every type has a corresponding constant macro:

```rust
use nbyte_int::const_macro::*;

const A: u40 = u40!(123456789012);
const B: i24 = i24!(-1024);
```

These will panic at compile time if the value doesn't fit in the target bit width.