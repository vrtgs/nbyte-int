#![no_std]
#![doc = include_str!("../README.md")]
#![allow(
    clippy::partialeq_ne_impl,
    clippy::non_canonical_partial_ord_impl,
    reason = "bulk forwarding to integer methods is faster than handwriting the methods"
)]

mod conversions;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
#[repr(u8)]
enum Zero {
    _0 = 0,
}

const fn from_str_error(bad_val: &str) -> core::num::ParseIntError {
    match i8::from_str_radix(bad_val, 10) {
        Err(err) => err,
        Ok(_) => unreachable!(),
    }
}

const POSITIVE_OVERFLOW: core::num::ParseIntError = from_str_error("999");

const NEGATIVE_OVERFLOW: core::num::ParseIntError = from_str_error("-999");

#[cfg(feature = "bytemuck")]
unsafe impl bytemuck::Zeroable for Zero {}

#[cfg(feature = "bytemuck")]
unsafe impl bytemuck::NoUninit for Zero {}

const _: () = match (cfg!(target_endian = "big"), cfg!(target_endian = "little")) {
    (true, false) | (false, true) => {}
    (false, false) => panic!("unknown endianness"),
    (true, true) => panic!("invalid endianness"),
};

macro_rules! declare_inner_struct {
    ($name: ident @[$size: expr] { $($fields: tt)* }) => {
        #[derive(Copy, Clone)]
        #[repr(C, align($size))]
        struct $name {
            $($fields)*
        }

        // Safety: all fields are NoUninit and Zeroable and there is no padding
        // there is no padding as that is statically asserted,
        // and they are NoUninit and Zeroable since all the fields are either `u8` or `Zero`
        #[cfg(feature = "bytemuck")]
        unsafe impl bytemuck::NoUninit for $name {}
        #[cfg(feature = "bytemuck")]
        unsafe impl bytemuck::Zeroable for $name {}


        const _: () = assert!(size_of::<$name>() == $size);
    };
}

macro_rules! match_signedness {
    (match I {
        SIGNED => { $($tt:tt)* },
        UNSIGNED => { $($_tt:tt)* } $(,)?
    }) => {
        $($tt)*
    };

    (match U {
        SIGNED => { $($_tt:tt)* },
        UNSIGNED => { $($tt:tt)* } $(,)?
    }) => {
        $($tt)*
    };
}

macro_rules! forward_ref_op_assign {
    (impl $imp:ident, $method:ident for $t:ty) => {
        impl core::ops::$imp<&$t> for $t {
            #[inline(always)]
            fn $method(&mut self, other: &$t) {
                core::ops::$imp::$method(self, *other);
            }
        }
    };
}

macro_rules! forward_ref_binop {
    (impl $imp:ident, $method:ident for $t:ty) => {
        impl core::ops::$imp<$t> for &$t {
            type Output = <$t as core::ops::$imp<$t>>::Output;

            #[inline(always)]
            fn $method(self, other: $t) -> Self::Output {
                core::ops::$imp::$method(*self, other)
            }
        }

        impl core::ops::$imp<&$t> for $t {
            type Output = <$t as core::ops::$imp<$t>>::Output;

            #[inline(always)]
            fn $method(self, other: &$t) -> Self::Output {
                core::ops::$imp::$method(self, *other)
            }
        }

        impl core::ops::$imp<&$t> for &$t {
            type Output = <$t as core::ops::$imp<$t>>::Output;

            #[inline(always)]
            fn $method(self, other: &$t) -> Self::Output {
                core::ops::$imp::$method(*self, *other)
            }
        }
    };
}

macro_rules! impl_wrapping_binop_inner {
    ($(impl $imp:ident, $method:ident for ($ty:ty as $inner: ty) @ $fetch:ident)+) => {$(
        impl core::ops::$imp for $ty {
            type Output = $ty;

            #[inline(always)]
            fn $method(self, rhs: Self) -> Self::Output {
                let lhs = self.$fetch();
                let rhs = rhs.$fetch();
                let result = pastey::paste!(<$inner>::[<wrapping_ $method>](lhs, rhs));
                Self::new_truncated(result)
            }
        }

        forward_ref_binop! { impl $imp, $method for $ty }

        pastey::paste! {
            impl core::ops::[<$imp Assign>] for $ty {
                #[inline(always)]
                fn [<$method _assign>](&mut self, rhs: Self) {
                    *self = <Self as core::ops::$imp>::$method(*self, rhs);
                }
            }

            forward_ref_op_assign! { impl [<$imp Assign>], [<$method _assign>] for $ty }
        }
    )+}
}

macro_rules! impl_wrapping_bits_binop {
    ($(impl $imp:ident, $method:ident for $t:ty as $inner: ty)+) => {$(
        impl_wrapping_binop_inner! { impl $imp, $method for ($t as $inner) @ to_bits }
    )+}
}

macro_rules! impl_wrapping_inner_binop {
    ($(impl $imp:ident, $method:ident for $t:ty as $inner: ty)+) => {$(
        impl_wrapping_binop_inner! { impl $imp, $method for ($t as $inner) @ get }
    )+}
}

macro_rules! handle_argument {
    ($arg_name: ident : Self) => {
        $arg_name.get()
    };
    ($arg_name: ident : &Self) => {
        &$arg_name.get()
    };
    ($arg_name: ident : &mut Self) => {
        &mut $arg_name.get()
    };
    ($arg_name: ident : $arg_ty: ty) => {
        $arg_name
    };
}

macro_rules! defer_basic {
    (
        $(impl ($path: path) for $ty: ident as $inner: ty {
            $(
            $(#[$meta: meta])*
            fn $method: ident ($(& $([$has_ref: tt])? $(mut $([$has_mut: tt])? )?)? self $(, $arg_name: ident : [$($arg_ty: tt)*])* $(,)?) -> $ret: ty;
            )*
        })*
    ) => {$(
        impl $path for $ty {
            $(#[inline(always)]
            $(#[$meta])*
            fn $method($(& $($has_ref)? $(mut $($has_mut)? )?)? self $(, $arg_name : $($arg_ty)*)*) -> $ret {
                <$inner as $path>::$method($(& $($has_ref)? $(mut $($has_mut)? )?)? self.get() $(, handle_argument!($arg_name: $($arg_ty)*))*)
            })*
        }
    )*};
}

macro_rules! defer_impl {
    (@($_sign_prefix: ident) $ty: ident as $inner: ty) => {
        defer_basic! {
            impl (core::fmt::Display) for $ty as $inner {
                fn fmt(&self, f: [&mut core::fmt::Formatter<'_>]) -> core::fmt::Result;
            }

            impl (core::fmt::Debug) for $ty as $inner {
                fn fmt(&self, f: [&mut core::fmt::Formatter<'_>]) -> core::fmt::Result;
            }

            impl (core::cmp::PartialEq) for $ty as $inner {
                fn eq(&self, other: [&Self]) -> bool;

                fn ne(&self, other: [&Self]) -> bool;
            }

            impl (core::cmp::Eq) for $ty as $inner {}


            impl (core::cmp::PartialOrd) for $ty as $inner {
                fn partial_cmp(&self, other: [&Self]) -> Option<core::cmp::Ordering>;


                fn lt(&self, other: [&Self]) -> bool;
                fn le(&self, other: [&Self]) -> bool;

                fn gt(&self, other: [&Self]) -> bool;
                fn ge(&self, other: [&Self]) -> bool;
            }

            impl (core::cmp::Ord) for $ty as $inner {
                fn cmp(&self, other: [&Self]) -> core::cmp::Ordering;
            }
        }

        impl core::hash::Hash for $ty {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                <$inner as core::hash::Hash>::hash(
                    // Safety: self is always in the same layout as backing
                    unsafe { &*(self as *const Self as *const $inner) },
                    state,
                )
            }

            fn hash_slice<H: core::hash::Hasher>(data: &[Self], state: &mut H)
            where
                Self: Sized,
            {
                <$inner as core::hash::Hash>::hash_slice(
                    // $ty has the same layout and everything as $inner
                    // its inner with some restrictions so this is fine
                    unsafe { &*(data as *const [Self] as *const [$inner]) },
                    state,
                )
            }
        }

        impl_wrapping_bits_binop! {
            impl Add, add for $ty as $inner
            impl Sub, sub for $ty as $inner
            impl Mul, mul for $ty as $inner
        }

        impl_wrapping_inner_binop! {
            impl Div, div for $ty as $inner
            impl Rem, rem for $ty as $inner
        }

        impl $ty {
            pub const fn from_str_radix(
                str: &str,
                radix: u32,
            ) -> Result<Self, core::num::ParseIntError> {
                match <$inner>::from_str_radix(str, radix) {
                    Err(err) => Err(err),
                    Ok(int) => match Self::new(int) {
                        Some(casted_int) => Ok(casted_int),
                        // for unsigned integers this obviously gets optimized out
                        #[allow(unused_comparisons)]
                        None if int < 0 => Err(NEGATIVE_OVERFLOW),
                        None => Err(POSITIVE_OVERFLOW),
                    },
                }
            }
        }

        impl core::str::FromStr for $ty {
            type Err = core::num::ParseIntError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Self::from_str_radix(s, 10)
            }
        }

        #[cfg(feature = "serde")]
        impl serde::Serialize for $ty {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                match serializer.is_human_readable() {
                    true => <$inner as serde::Serialize>::serialize(&self.get(), serializer),
                    false => <[u8; { (<$ty>::BITS / 8) as usize }] as serde::Serialize>::serialize(
                        &self.to_be_bytes(),
                        serializer,
                    ),
                }
            }
        }

        #[cfg(feature = "serde")]
        impl<'de> serde::Deserialize<'de> for $ty {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                match deserializer.is_human_readable() {
                    true => {
                        let int = <$inner as serde::Deserialize>::deserialize(deserializer)?;
                        match $ty::new(int) {
                            Some(int) => Ok(int),
                            None => Err(<D::Error as serde::de::Error>::custom(format_args!(
                                "invalid {int} as {}",
                                stringify!($ty)
                            ))),
                        }
                    }
                    false => {
                        <[u8; { (<$ty>::BITS / 8) as usize }] as serde::Deserialize>::deserialize(
                            deserializer,
                        )
                        .map(<$ty>::from_be_bytes)
                    }
                }
            }
        }

        #[cfg(feature = "num-traits")]
        impl num_traits::One for $ty {
            #[inline(always)]
            fn one() -> Self {
                <Self as num_traits::ConstOne>::ONE
            }

            #[inline]
            fn is_one(&self) -> bool {
                *self == <Self as num_traits::ConstOne>::ONE
            }
        }

        #[cfg(feature = "num-traits")]
        impl num_traits::ConstOne for $ty {
            const ONE: Self = const_macro::$ty!(1);
        }

        #[cfg(feature = "num-traits")]
        impl num_traits::Zero for $ty {
            #[inline(always)]
            fn zero() -> Self {
                <Self as num_traits::ConstZero>::ZERO
            }

            #[inline(always)]
            fn is_zero(&self) -> bool {
                *self == <Self as num_traits::ConstZero>::ZERO
            }
        }

        #[cfg(feature = "num-traits")]
        impl num_traits::ConstZero for $ty {
            const ZERO: Self = const_macro::$ty!(0);
        }

        #[cfg(feature = "num-traits")]
        impl num_traits::Num for $ty {
            type FromStrRadixErr = core::num::ParseIntError;

            fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
                Self::from_str_radix(str, radix)
            }
        }
    };
}

macro_rules! nbyte_int_inner {
    (
        $($backing_ty: ident @($signed_prefix: ident $size: expr) [$($byte_count: tt @ $bit_count: tt),+])*
    ) => {
        pastey::paste! {
            pub mod types {
            //! The integer types: `i24`, `i40`, `i48`, `i56`, `i72` through `i120`, and
            //! the unsigned equivalents `u24`, `u40`, ... `u120`.
            //!
            //! Each type is a fixed-width integer whose width is a whole number of bytes
            //! but not one of the native widths (8, 16, 32, 64 or 128 bits). For those,
            //! use the primitive types.
            //!
            //! # Layout
            //!
            //! Every type has the same size and alignment as the primitive that backs it
            //! (`i24` and `u24` use `i32` and `u32`; `i40` through `i56` use 64-bit
            //! integers; `i72` through `i120` use 128-bit integers). The unused upper
            //! bytes are always zero. That leaves invalid bit patterns, so
            //! `Option<i24>` is **usually** the same size as `i24`.
            //!
            //! # Creating values
            //!
            //! - [`new`](i24::new): checked, returns `None` if the value doesn't fit
            //! - `new_truncated`: wraps like an `as` cast, never fails
            //! - `new_unchecked`: `unsafe`, for values already known to be in range
            //! - `from_bits`: `unsafe`, takes the raw, pre-masked bit pattern
            //! - the constant macros in [`literal`](crate::literal): checked at
            //!   compile time
            //!
            //! `get` returns the value in the backing type, sign-extended for signed
            //! types. `to_bits` returns the raw, unextended pattern.
            //!
            //! # Conversions
            //!
            //! Every integer type in this crate, including the primitives, converts to
            //! every other. Conversions that can never fail implement [`From`]; the rest
            //! implement [`TryFrom`] with [`TryFromIntError`](core::num::TryFromIntError).
            //!
            //! # Examples
            //!
            //! ```
            //! use nbyte_int::prelude::*;
            //!
            //! let x = i24::new(-5).unwrap();
            //! assert_eq!(x.get(), -5);
            //!
            //! // out of range
            //! assert!(i24::new(i32::MAX).is_none());
            //! assert_eq!(i24::MAX.get(), 8_388_607);
            //!
            //! // same size as the backing type, and Option is free (usually - not a guarantee)
            //! assert_eq!(size_of::<i24>(), size_of::<i32>());
            //! assert_eq!(size_of::<Option<u40>>(), size_of::<u40>());
            //!
            //! // lossless conversions use `From`, the rest `TryFrom`
            //! assert_eq!(i24::from(100_i16).get(), 100);
            //! assert!(u24::try_from(-1_i32).is_err());
            //! assert_eq!(u24::try_from(70_000_i64).unwrap().get(), 70_000);
            //!
            //! // byte order
            //! assert_eq!(u24::new(0x010203).unwrap().to_le_bytes(), [0x03, 0x02, 0x01]);
            //! ```
            use super::*;

            $(const _: () = {
                let min = $size / 2;
                let max = size_of::<$backing_ty>();
                assert!($size == max, "invalid backing type size");
                $(
                assert!(
                    $bit_count == $byte_count * 8,
                    concat!(
                        "invalid nbyte bit count for ",
                        stringify!($byte_count),
                        " bytes",
                    )
                );
                assert!(min < $byte_count && $byte_count < max, "invalid nbyte int");
                )*
                let len = <[_]>::len(&[$($byte_count),*]);
                // len = (min..max).len() - 1
                // len = max - min - 1
                assert!(len == max - min - 1, "not all byte integers implemented");
            };

            $(
            declare_inner_struct! {
                [<$signed_prefix $bit_count LittleEndian Inner>] @[$size] {
                    bytes: [u8; $byte_count],
                    zeros: [Zero; { $size - $byte_count }],
                }
            }

            declare_inner_struct! {
                [<$signed_prefix $bit_count BigEndian Inner>] @[$size] {
                    zeros: [Zero; { $size - $byte_count }],
                    bytes: [u8; $byte_count]
                }
            }


            cfg_select! {
                target_endian = "little" => {
                    type [<$signed_prefix $bit_count Inner>] = [<$signed_prefix $bit_count LittleEndian Inner>];
                    type [<Flipped $signed_prefix $bit_count Inner>] = [<$signed_prefix $bit_count BigEndian Inner>];
                }
                target_endian = "big" => {
                    type [<$signed_prefix $bit_count Inner>] = [<$signed_prefix $bit_count BigEndian Inner>];
                    type [<Flipped $signed_prefix $bit_count Inner>] = [<$signed_prefix $bit_count LittleEndian Inner>];
                }
            }


            #[allow(non_camel_case_types)]
            #[derive(Copy, Clone)]
            #[repr(transparent)]
            pub struct [<$signed_prefix:lower $bit_count>]([<$signed_prefix $bit_count Inner>]);

            // Safety: the inner type is also both NoUninit and Zeroable
            #[cfg(feature = "bytemuck")]
            unsafe impl bytemuck::NoUninit for [<$signed_prefix:lower $bit_count>] {}
            #[cfg(feature = "bytemuck")]
            unsafe impl bytemuck::Zeroable for [<$signed_prefix:lower $bit_count>] {}

            impl [<$signed_prefix:lower $bit_count>] {
                const DATA_BITS_MASK: $backing_ty = (1 as $backing_ty)
                    .strict_shl($bit_count)
                    .strict_sub(1);

                const MIN_INNER: $backing_ty = match_signedness! {
                    match $signed_prefix {
                        SIGNED => {
                            {
                                let abs_value = Self::DATA_BITS_MASK.isolate_highest_one();
                                // in twos compliment the sign bit = -expected value
                                // so if we negate its absolute value we get the minimum
                                abs_value.strict_neg()
                            }
                        },
                        UNSIGNED => { 0 },
                    }
                };

                const MAX_INNER: $backing_ty = match_signedness! {
                    match $signed_prefix {
                        SIGNED => { Self::DATA_BITS_MASK >> 1 },
                        UNSIGNED => { Self::DATA_BITS_MASK },
                    }
                };


                pub const BITS: u32 = $bit_count;

                pub const MIN: Self = Self::new(Self::MIN_INNER).unwrap();
                pub const MAX: Self = Self::new(Self::MAX_INNER).unwrap();

                #[doc = concat!(
                    "Creates a [`", stringify!([<$signed_prefix:lower $bit_count>]), "`](struct@",
                    stringify!([<$signed_prefix:lower $bit_count>]), ") from a `",
                    stringify!($backing_ty), "`, returning [`None`] if the value does not fit in ",
                    stringify!($bit_count), " bits.\n\n",
                    "The valid range is [`MIN`](Self::MIN)`..=`[`MAX`](Self::MAX).\n\n",
                    "This is the checked counterpart of [`new_unchecked`](Self::new_unchecked) ",
                    "and [`new_truncated`](Self::new_truncated). It is `const`, so it can be used ",
                    "to build constants; the type's constant macro does exactly that and panics ",
                    "at compile time on failure.\n\n",
                    "# Examples\n\n",
                    "```\n",
                    "use nbyte_int::prelude::*;\n\n",
                    "assert_eq!(", stringify!([<$signed_prefix:lower $bit_count>]), "::new(0).map(|x| x.get()), Some(0));\n",
                    "assert_eq!(", stringify!([<$signed_prefix:lower $bit_count>]), "::new(",
                    stringify!([<$signed_prefix:lower $bit_count>]), "::MAX.get()).map(|x| x.get()), ",
                    "Some(", stringify!([<$signed_prefix:lower $bit_count>]), "::MAX.get()));\n",
                    "// one past the maximum does not fit\n",
                    "assert!(", stringify!([<$signed_prefix:lower $bit_count>]), "::MAX.get().checked_add(1)",
                    ".and_then(", stringify!([<$signed_prefix:lower $bit_count>]), "::new).is_none());\n",
                    "```"
                )]
                #[inline(always)]
                pub const fn new(x: $backing_ty) -> Option<Self> {
                    match x {
                        Self::MIN_INNER..=Self::MAX_INNER => Some(unsafe { Self::new_unchecked(x) }),
                        _ => None
                    }
                }

                #[doc = concat!(
                    "Creates a [`", stringify!([<$signed_prefix:lower $bit_count>]), "`](struct@",
                    stringify!([<$signed_prefix:lower $bit_count>]), ") from a `",
                    stringify!($backing_ty), "` without checking that it fits in ",
                    stringify!($bit_count), " bits.\n\n",
                    "Prefer [`new`](Self::new) (checked) or [`new_truncated`](Self::new_truncated) ",
                    "(wrapping) unless the range check is provably redundant and measurably matters.\n\n",
                    "# Safety\n\n",
                    "`x` must be within [`MIN`](Self::MIN)`..=`[`MAX`](Self::MAX), i.e. the value ",
                    "[`new`](Self::new) would accept. Violating this is immediate UB",
                )]
                #[inline(always)]
                pub const unsafe fn new_unchecked(x: $backing_ty) -> Self {
                    unsafe {
                        core::hint::assert_unchecked(Self::MIN_INNER <= x);
                        core::hint::assert_unchecked(x <= Self::MAX_INNER);
                    }

                    match_signedness! {
                        match $signed_prefix {
                            // the sign bit and friends may be on so mask it just in case
                            SIGNED => { Self::new_truncated(x) },
                            // unsigned no need to mask the top bits since they are already off
                            UNSIGNED => { unsafe { Self::from_bits(x) } },
                        }
                    }
                }

                #[doc = concat!(
                    "Creates a ",
                    $byte_count,
                    "-byte integer from its exact raw bit pattern, bypassing all masking."
                )]
                ///
                /// Unlike [`new_unchecked`](Self::new_unchecked), which takes a logical integer value
                /// and handles any necessary sign-bit masking, this transmutes the bits directly.
                /// Only use it when you already have a pre-masked bit pattern matching the
                /// type's exact internal representation.
                ///
                /// # Safety
                #[doc = concat!(
                    "Only the low ",
                    $byte_count,
                    " bytes of `bits` may be non-zero. The upper bytes must all be zero. ",
                    "Violating this is immediate undefined behavior (UB)."
                )]
                #[inline(always)]
                pub const unsafe fn from_bits(bits: $backing_ty) -> Self {
                    // Safety: upheld by user
                    unsafe {
                        core::hint::assert_unchecked(bits & const { !Self::DATA_BITS_MASK } == 0);
                        core::mem::transmute(bits)
                    }
                }

                #[doc = concat!(
                    "Creates a ",
                    $byte_count,
                    "-byte integer by truncating the upper bits of the provided value."
                )]
                ///
                /// This applies a bitmask to clear any bits outside the target bit-width.
                /// This behaves identically to primitive narrowing casts (e.g., `u32 as u16`),
                /// where values out of bounds wrap around.
                ///
                /// Unlike [`new`](Self::new), this method will not return `None` on overflow.
                /// Unlike [`new_unchecked`](Self::new_unchecked) or [`from_bits`](Self::from_bits),
                /// this method is always safe to call with any value because it strictly enforces
                /// the memory layout invariants via masking.
                #[inline(always)]
                pub const fn new_truncated(bits: $backing_ty) -> Self {
                    unsafe { Self::from_bits(bits & Self::DATA_BITS_MASK) }
                }

                #[doc = concat!(
                    "Returns the raw bit pattern of the ",
                    $byte_count,
                    "-byte integer as its underlying backing type."
                )]
                ///
                /// This returns the exact memory representation of the integer where the upper
                /// padding bytes are guaranteed to be zero.
                ///
                /// **Warning on Signed Integers:** This method does *not* perform sign extension.
                /// If you call `to_bits()` on a negative value, the returned backing primitive
                /// will have its upper bytes set to `0`, which will change its logical value.
                ///
                /// To retrieve the logical value with proper sign extension, always use [`get`](Self::get) instead.
                #[inline(always)]
                pub const fn to_bits(self) -> $backing_ty {
                    // Safety: self is always in the same layout as backing
                    unsafe { core::mem::transmute(self) }
                }

                #[inline(always)]
                const fn get_swapped_bytes_inner(self) -> [u8; $byte_count] {
                    let bits = self.to_bits();
                    let swapped = bits.swap_bytes();

                    // Safety:
                    // swapped just swaps the bytes and so is a valid but flipped
                    // version of self which is what [<Flipped $byte_count IntInner>] is
                    let swapped: [<Flipped $signed_prefix $bit_count Inner>] = unsafe {
                        core::mem::transmute(swapped)
                    };

                    swapped.bytes
                }

                #[inline(always)]
                pub const fn swap_bytes(self) -> Self {
                    // this has surprisingly very good codegen
                    // and is as good if not better than anything manual after inlining
                    Self::from_ne_bytes(self.get_swapped_bytes_inner())
                }

                #[inline(always)]
                pub const fn from_le(x: Self) -> Self {
                    #[cfg(target_endian = "little")]
                    {
                        x
                    }

                    #[cfg(target_endian = "big")]
                    {
                        x.swap_bytes()
                    }
                }

                #[inline(always)]
                pub const fn from_be(x: Self) -> Self {
                    #[cfg(target_endian = "big")]
                    {
                        x
                    }

                    #[cfg(target_endian = "little")]
                    {
                        x.swap_bytes()
                    }
                }

                #[inline(always)]
                pub const fn to_le(self) -> Self {
                    Self::from_le(self)
                }

                #[inline(always)]
                pub const fn to_be(self) -> Self {
                    Self::from_be(self)
                }

                #[inline(always)]
                pub const fn from_ne_bytes(bytes: [u8; $byte_count]) -> Self {
                    Self([<$signed_prefix $bit_count Inner>]{
                        zeros: [Zero::_0; { $size - $byte_count }],
                        bytes
                    })
                }

                #[inline(always)]
                pub const fn from_le_bytes(bytes: [u8; $byte_count]) -> Self {
                    Self::from_le(Self::from_ne_bytes(bytes))
                }

                #[inline(always)]
                pub const fn from_be_bytes(bytes: [u8; $byte_count]) -> Self {
                    Self::from_be(Self::from_ne_bytes(bytes))
                }

                #[inline(always)]
                pub const fn to_ne_bytes(self) -> [u8; $byte_count] {
                    self.0.bytes
                }

                #[inline(always)]
                pub const fn to_le_bytes(self) -> [u8; $byte_count] {
                    #[cfg(target_endian = "little")]
                    {
                        self.0.bytes
                    }

                    #[cfg(target_endian = "big")]
                    {
                        self.get_swapped_bytes_inner()
                    }
                }

                #[inline(always)]
                pub const fn to_be_bytes(self) -> [u8; $byte_count] {
                    #[cfg(target_endian = "big")]
                    {
                        self.0.bytes
                    }

                    #[cfg(target_endian = "little")]
                    {
                        self.get_swapped_bytes_inner()
                    }
                }

                #[doc = concat!(
                    "Returns the logical value of the ",
                    $byte_count,
                    "-byte integer as its underlying `",
                    stringify!($backing_ty),
                    "` backing type."
                )]
                ///
                /// For signed integers, this method performs proper sign extension. This ensures
                /// that negative values correctly retain their negative logical value when cast
                /// up to the wider backing primitive. For unsigned integers, the value is simply
                /// zero-extended.
                ///
                /// If you need the raw, unextended memory layout (where the upper padding bytes
                /// are strictly zeroed out regardless of sign), use [`to_bits`](Self::to_bits) instead.
                #[inline(always)]
                pub const fn get(self) -> $backing_ty {
                    match_signedness! {
                        match $signed_prefix {
                            SIGNED => {
                                {
                                    const OFFSET: u32 = $backing_ty::BITS
                                        .strict_sub(<[<$signed_prefix:lower $bit_count>]>::BITS);

                                    let bits = self.to_bits();
                                    // this aligns the integers sign bit
                                    // to the sign bit of the backing type
                                    // and then sign extends backwards
                                    unsafe { bits.unchecked_shl(OFFSET).unchecked_shr(OFFSET) }
                                }
                            },
                            // if the data is not signed; no special handling is required
                            UNSIGNED => { self.to_bits() },
                        }
                    }
                }
            }

            defer_impl!(@($signed_prefix) [<$signed_prefix:lower $bit_count>] as $backing_ty);
            )*
        )*
        }


        pub mod literal {
            //! Compile-time checked literal constructors, one macro per type (`i24!`, `u40!`, ...).
            //!
            //! They live here instead of the crate root so they don't clutter it.

            #[cfg(doc)]
            use crate::types::*;

            $($(
                #[macro_export]
                #[doc = concat!(
                    "Creates a [`", stringify!([<$signed_prefix:lower $bit_count>]), "`](struct@", stringify!([<$signed_prefix:lower $bit_count>]), ") from an integer literal, ",
                    "checked at compile time.\n\n",
                    "Fails to compile if the literal is outside [`MIN`](", stringify!([<$signed_prefix:lower $bit_count>]), "::MIN)`..=`",
                    "[`MAX`](", stringify!([<$signed_prefix:lower $bit_count>]), "::MAX). Only literals are accepted; for other constant ",
                    "expressions use `const { ", stringify!([<$signed_prefix:lower $bit_count>]), "::new(expr).unwrap() }`.\n\n",
                    "# Examples\n\n",
                    "```\n",
                    "use nbyte_int::const_macro::", stringify!([<$signed_prefix:lower $bit_count>]), ";\n\n",
                    "const X: nbyte_int::types::", stringify!([<$signed_prefix:lower $bit_count>]), " = ", stringify!([<$signed_prefix:lower $bit_count>]), "!(1);\n",
                    "assert_eq!(X.get(), 1);\n",
                    "```\n\n",
                    "An out-of-range literal is a compile error.",
                )]
                #[doc(hidden)]
                macro_rules! [<__ $signed_prefix:lower $bit_count __>] {
                    ($__inner_expr__: literal) => {
                        const {
                            match $crate::types::[<$signed_prefix:lower $bit_count>]::new($__inner_expr__) {
                                Some(x) => x,
                                None => panic!(concat!(
                                    "invalid ",
                                    stringify!([<$signed_prefix:lower $bit_count>]),
                                    " literal `",
                                    $__inner_expr__,
                                    "`"
                                ))
                            }
                        }
                    }
                }

                #[doc(inline)]
                pub use [<__ $signed_prefix:lower $bit_count __>] as [<$signed_prefix:lower $bit_count>];
            )*)*
        }
        }
    };
}

macro_rules! nbyte_int {
    (
        $($backing: tt @($size: expr) [$($byte_count: tt @ $bit_count: tt),+ $(,)?])*
    ) => {
        pastey::paste! {
            nbyte_int_inner! {
                $(
                [<i $backing>] @(I $size) [$($byte_count @ $bit_count),+]
                [<u $backing>] @(U $size) [$($byte_count @ $bit_count),+]
                )*
            }
        }
    };
}

nbyte_int! {
    // 8 bits and 16 bits have no in between
    32 @(4) [3 @ 24]
    64 @(8) [5 @ 40, 6 @ 48, 7 @ 56]
    128 @(16) [9 @ 72, 10 @ 80, 11 @ 88, 12 @ 96, 13 @ 104, 14 @ 112, 15 @ 120]
}

pub mod prelude {
    //! Everything you normally need, in one import.
    //!
    //! This re-exports all the integer types from [`types`](crate::types) and
    //! the compile-time literal macros from [`literal`](crate::literal).

    pub use crate::literal::*;
    pub use crate::types::*;
}
