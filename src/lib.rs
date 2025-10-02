#![no_std]

#![doc = include_str!("../README.md")]

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
#[repr(u8)]
enum Zero {
    _0 = 0
}


const fn from_str_error(bad_val: &str) -> core::num::ParseIntError {
    match i8::from_str_radix(bad_val, 10) {
        Err(err) => err,
        Ok(_) => unreachable!(),
    }
}

const POSITIVE_OVERFLOW: core::num::ParseIntError = from_str_error("9999999999999999999999999999999999999999");

const NEGATIVE_OVERFLOW: core::num::ParseIntError = from_str_error("-9999999999999999999999999999999999999999");

#[cfg(feature = "bytemuck")]
unsafe impl bytemuck::Zeroable for Zero {}

#[cfg(feature = "bytemuck")]
unsafe impl bytemuck::NoUninit for Zero {}


#[cfg(all(target_endian = "big", target_endian = "little"))]
compile_error!("invalid endianness");

#[cfg(all(not(target_endian = "big"), not(target_endian = "little")))]
compile_error!("unknown endianness");


macro_rules! declare_inner_struct {
    ($name: ident @[$size: expr] { $($fields: tt)* }) => {
        #[derive(Copy, Clone)]
        #[repr(C, align($size))]
        struct $name {
            $($fields)*
        }

        // Safety: all fields are NoUninit and Zeroable and there is no padding
        // there is no padding as that is statically asserted
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
    }
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
    }
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
    ($arg_name: ident : Self) => { $arg_name.get() };
    ($arg_name: ident : &Self) => { &$arg_name.get() };
    ($arg_name: ident : &mut Self) => { &mut $arg_name.get() };
    ($arg_name: ident : $arg_ty: ty) => { $arg_name };
}

macro_rules! defer_basic {
    (
        $(impl ($path: path) for $ty: ident as $inner: ty {
            $(fn $method: ident ($(& $([$has_ref: tt])? $(mut $([$has_mut: tt])? )?)? self $(, $arg_name: ident : [$($arg_ty: tt)*])* $(,)?) -> $ret: ty;)*
        })*
    ) => {$(
        impl $path for $ty {
            $(#[inline(always)]
            fn $method($(& $($has_ref)? $(mut $($has_mut)? )?)? self $(, $arg_name : $($arg_ty)*)*) -> $ret {
                <$inner as $path>::$method($(& $($has_ref)? $(mut $($has_mut)? )?)? self.get() $(, handle_argument!($arg_name: $($arg_ty)*))*)
            })*
        }
    )*};
}

macro_rules! defer_impl {
    (@($sign_prefix: ident) $ty: ident as $inner: ty) => {
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
                <$inner as core::hash::Hash>::hash(&self.get(), state)
            }

            fn hash_slice<H: core::hash::Hasher>(data: &[Self], state: &mut H) where Self: Sized {
                <$inner as core::hash::Hash>::hash_slice(
                    // $ty has the same layout and abi and everything as $inner
                    // its inner with some restrictions so this is fine
                    unsafe { &*(data as *const [Self] as *const [$inner]) },
                    state
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
            pub const fn from_str_radix(str: &str, radix: u32) -> Result<Self, core::num::ParseIntError> {
                match <$inner>::from_str_radix(str, radix) {
                    Err(err) => Err(err),
                    Ok(int) => match Self::new(int) {
                        Some(casted_int) => Ok(casted_int),
                        // for unsigned integers this obviously gets optimized out
                        #[allow(unused_comparisons)]
                        None if int < 0 => Err(NEGATIVE_OVERFLOW),
                        None => Err(POSITIVE_OVERFLOW)
                    }
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
                    false => <[u8; { (<$ty>::BITS / 8) as usize }] as serde::Serialize>::serialize(&self.to_be_bytes(), serializer)
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
                            None => Err(<D::Error as serde::de::Error>::custom(
                                format_args!("invalid {int} as {}", stringify!(i24))
                            )),
                        }
                    },
                    false => {
                        <[u8; { (<$ty>::BITS / 8) as usize }] as serde::Deserialize>::deserialize(deserializer)
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
                [<$signed_prefix $bit_count LitteEndian Inner>] @[$size] {
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


            #[cfg(target_endian = "little")]
            type [<$signed_prefix $bit_count Inner>] = [<$signed_prefix $bit_count LitteEndian Inner>];

            #[cfg(target_endian = "little")]
            type [<Flipped $signed_prefix $bit_count Inner>] = [<$signed_prefix $bit_count BigEndian Inner>];

            #[cfg(target_endian = "big")]
            type [<$signed_prefix $bit_count Inner>] = [<$signed_prefix $bit_count BigEndian Inner>];

            #[cfg(target_endian = "big")]
            type [<Flipped $signed_prefix $bit_count Inner>] = [<$signed_prefix $bit_count LitteEndian Inner>];


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
                const DATA_BITS_MASK: $backing_ty = (1 << ($byte_count * 8)) - 1;

                const MIN_INNER: $backing_ty = match_signedness! {
                    match $signed_prefix {
                        SIGNED => {
                            {
                                // FIXME (isolate_most_significant_one)
                                // just the sign bit; but sadly this is not a negative value
                                // in the underlying type
                                let abs_value = (Self::DATA_BITS_MASK + 1) >> 1;
                                // in twos compliment the sign bit = -expected value
                                // so if we negate its absolute value we get the minimum

                                -abs_value
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

                /// truncates the upper bytes and sets them to zero
                #[inline(always)]
                pub const fn new(bits: $backing_ty) -> Option<Self> {
                    if Self::MIN_INNER <= bits && bits <= Self::MAX_INNER {
                        return Some(match_signedness! {
                            match $signed_prefix {
                                // the sign bit and friends may be on
                                // mask it just in case
                                SIGNED => { Self::new_truncated(bits) },
                                // unsigned no need to mask the top bits
                                // they are already off
                                UNSIGNED => { unsafe { Self::from_bits(bits) } },
                            }
                        })
                    }

                    None
                }

                /// # Safety
                #[doc = concat!(
                    "the number must only have the first ",
                    $byte_count,
                    " with zeros, the upper bytes must NOT be filled"
                )]
                #[inline(always)]
                pub const unsafe fn from_bits(bits: $backing_ty) -> Self {
                    // Safety: upheld by user
                    unsafe { core::mem::transmute(bits) }
                }

                /// truncates the upper bytes and sets them to zero
                #[inline(always)]
                pub const fn new_truncated(bits: $backing_ty) -> Self {
                    unsafe {
                        Self::from_bits(bits & Self::DATA_BITS_MASK)
                    }
                }

                #[inline(always)]
                pub const fn to_bits(self) -> $backing_ty {
                    // Safety: self is always in the same layout as backing
                    unsafe { core::mem::transmute(self) }
                }

                #[inline(always)]
                pub const fn swapped_bytes(self) -> [u8; $byte_count] {
                    let bits = self.to_bits();
                    let swapped = bits.swap_bytes();

                    // Safety:
                    // swapped just swaps the bytes and so is a valid
                    // flipped version of self which is what
                    // [<Flipped $byte_count IntInner>] is
                    let swapped: [<Flipped $signed_prefix $bit_count Inner>] = unsafe {
                        core::mem::transmute(swapped)
                    };
                    
                    swapped.bytes
                }

                #[inline(always)]
                pub const fn swap_bytes(self) -> Self {
                    // this has surprisingly very good codegen
                    // and is as good if not better than anything manual after inlining
                    Self::from_ne_bytes(self.swapped_bytes())
                }

                #[inline(always)]
                pub const fn to_le(self) -> Self {
                    #[cfg(target_endian = "little")]
                    {
                        self
                    }

                    #[cfg(target_endian = "big")]
                    {
                        self.swap_bytes()
                    }
                }

                #[inline(always)]
                pub const fn to_be(self) -> Self {
                    #[cfg(target_endian = "big")]
                    {
                        self
                    }

                    #[cfg(target_endian = "little")]
                    {
                        self.swap_bytes()
                    }
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
                    Self::from_ne_bytes(bytes).to_le()
                }

                #[inline(always)]
                pub const fn from_be_bytes(bytes: [u8; $byte_count]) -> Self {
                    Self::from_ne_bytes(bytes).to_be()
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
                        self.swapped_bytes()
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
                        self.swapped_bytes()
                    }
                }

                #[inline(always)]
                pub const fn get(self) -> $backing_ty {
                    match_signedness! {
                        match $signed_prefix {
                            SIGNED => {
                                {
                                    const OFFSET: u32 = $backing_ty::BITS - <[<$signed_prefix:lower $bit_count>]>::BITS;
                                    // FIXME unchecked_shifts
                                    // this aligns the integers sign bit
                                    // to the sign bit of the backing type
                                    // and then sign extends backwards
                                    (self.to_bits() << OFFSET) >> OFFSET
                                }
                            },
                            // if the data is not signed no special handling is required
                            UNSIGNED => { self.to_bits() },
                        }
                    }
                }
            }

            defer_impl!(@($signed_prefix) [<$signed_prefix:lower $bit_count>] as $backing_ty);
            )*
        )*
        }


        pub mod const_macro {
            $($(
                #[macro_export]
                macro_rules! [<$signed_prefix:lower $bit_count>] {
                    ($__inner_expr__: expr) => {
                        const {
                            match $crate::types::[<$signed_prefix:lower $bit_count>]::new($__inner_expr__) {
                                Some(x) => x,
                                None => panic!(
                                    concat!("invalid number constant")
                                )
                            }
                        }
                    }
                }

                pub use [<$signed_prefix:lower $bit_count>];
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

            pub mod prelude {
                pub use $crate::types::*;
                pub use $crate::const_macro::*;
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