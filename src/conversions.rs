use super::types::*;
use core::num::TryFromIntError;

#[cold]
#[inline(always)]
fn neg_overflow() -> TryFromIntError {
    match u8::try_from(-1_i8) {
        Err(err) => err,
        // this runs at runtime, don't panic
        Ok(_) => unsafe { core::hint::unreachable_unchecked() },
    }
}

#[cold]
#[inline(always)]
fn pos_overflow() -> TryFromIntError {
    match u8::try_from(256_u16) {
        Err(err) => err,
        // this runs at runtime, don't panic
        Ok(_) => unsafe { core::hint::unreachable_unchecked() },
    }
}

// `P` = primitive, `C` = custom; the value of a custom int is its backing type
macro_rules! to_val {
    (P $x:expr) => {
        $x
    };
    (C $x:expr) => {
        $x.get()
    };
}

macro_rules! lossless {
    // primitive <-> primitive already exists in core
    (P $s:ident $sb:ident => P $d:ident $db:ident) => {};

    ($sk:ident $s:ident $sb:ident => P $d:ident $db:ident) => {
        impl From<$s> for $d {
            #[inline(always)]
            fn from(x: $s) -> Self {
                to_val!($sk x) as $d
            }
        }
    };

    ($sk:ident $s:ident $sb:ident => C $d:ident $db:ident) => {
        impl From<$s> for $d {
            #[inline(always)]
            fn from(x: $s) -> Self {
                // `as` sign/zero extends, and the value is known to fit
                unsafe { <$d>::new_unchecked(to_val!($sk x) as $db) }
            }
        }
    };
}

macro_rules! fallible {
    (P $s:ident $sb:ident => P $d:ident $db:ident) => {};

    ($sk:ident $s:ident $sb:ident => $dk:ident $d:ident $db:ident) => {
        impl TryFrom<$s> for $d {
            type Error = TryFromIntError;

            #[inline]
            fn try_from(x: $s) -> Result<Self, Self::Error> {
                // step 1: source value -> destination's backing primitive
                // (the error is `Infallible` when that step is lossless)
                match <$db as TryFrom<$sb>>::try_from(to_val!($sk x)) {
                    Ok(v) => fallible!(@finish $dk $d v; $db),
                    Err(err) => Err(TryFromIntError::from(err)),
                }
            }
        }
    };
    // step 2: backing primitive -> destination
    (@finish P $d:ident $v:ident; $db: ident) => { Ok($v) };

    (@finish C $d:ident $v:ident; $db: ident) => {{
        const MIN: $db = $d::MIN.get();
        const MAX: $db = $d::MAX.get();

        // Note: we can't use a match stmt in a neat way
        // because of unsigned values since
        // ..MIN is an invalid range (its empty) and emits a compile error

        let value = $v;
        if value < MIN {
            Err(neg_overflow())
        } else if value > MAX {
            Err(pos_overflow())
        } else {
            Ok(unsafe { $d::new_unchecked(value) })
        }
    }};
}

macro_rules! conv_pair {
    // `s` is strictly narrower than `b`
    (
        ($sk:ident $si:ident $su:ident as $sib:ident $sub:ident)
        ($bk:ident $bi:ident $bu:ident as $bib:ident $bub:ident)
    ) => {
        // widening
        lossless! { $sk $si $sib => $bk $bi $bib } // iN -> iM
        lossless! { $sk $su $sub => $bk $bu $bub } // uN -> uM
        lossless! { $sk $su $sub => $bk $bi $bib } // uN -> iM
        fallible! { $sk $si $sib => $bk $bu $bub } // iN -> uM (negatives)

        // narrowing
        fallible! { $bk $bi $bib => $sk $si $sib }
        fallible! { $bk $bi $bib => $sk $su $sub }
        fallible! { $bk $bu $bub => $sk $si $sib }
        fallible! { $bk $bu $bub => $sk $su $sub }
    };
}

macro_rules! conversions {
    () => {};
    (
        ($k:ident $i:ident $u:ident as $ib:ident $ub:ident)
        $($rest:tt)*
    ) => {
        // same width, different sign
        fallible! { $k $i $ib => $k $u $ub }
        fallible! { $k $u $ub => $k $i $ib }

        $(conv_pair! { ($k $i $u as $ib $ub) $rest })*

        conversions! { $($rest)* }
    };
}

// MUST be sorted by ascending bit-width
conversions! {
    (P i8   u8   as i8   u8)
    (P i16  u16  as i16  u16)
    (C i24  u24  as i32  u32)
    (P i32  u32  as i32  u32)
    (C i40  u40  as i64  u64)
    (C i48  u48  as i64  u64)
    (C i56  u56  as i64  u64)
    (P i64  u64  as i64  u64)
    (C i72  u72  as i128 u128)
    (C i80  u80  as i128 u128)
    (C i88  u88  as i128 u128)
    (C i96  u96  as i128 u128)
    (C i104 u104 as i128 u128)
    (C i112 u112 as i128 u128)
    (C i120 u120 as i128 u128)
    (P i128 u128 as i128 u128)
}
