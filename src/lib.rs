#![no_std]


#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, bytemuck::Zeroable, bytemuck::NoUninit)]
#[repr(u8)]
enum Zero {
    _0 = 0
}

#[cfg(all(target_endian = "big", target_endian = "little"))]
compile_error!("invalid endianness");

#[cfg(all(not(target_endian = "big"), not(target_endian = "little")))]
compile_error!("unknown endianness");


macro_rules! declare_inner_struct {
    ($name: ident @[$size: expr] { $($fields: tt)* }) => {
        #[derive(Copy, Clone, bytemuck::Zeroable)]
        #[repr(C, align($size))]
        struct $name {
            $($fields)*
        }

        // Safety: all fields are NoUninit and there is no padding
        unsafe impl bytemuck::NoUninit for $name {}

        const _: () = {
            const fn assert_impl<T: ::bytemuck::NoUninit>() {}
            assert_impl::<Zero>();
        };

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

macro_rules! nbyte_int_inner {
    (
        $($backing: ident @($signed_prefix: ident $size: expr) [$($byte_count: tt @ $bit_count: tt),+])*
    ) => {
        $(
        const _: () = {
            let min = $size / 2;
            let max = size_of::<$backing>();
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

        pastey::paste! {$(
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
            #[derive(Copy, Clone, bytemuck::Zeroable, bytemuck::NoUninit)]
            #[repr(transparent)]
            pub struct [<$signed_prefix:lower $bit_count>]([<$signed_prefix $bit_count Inner>]);

            impl [<$signed_prefix:lower $bit_count>] {
                pub const BITS: u32 = $bit_count;
                const DATA_BITS_MASK: $backing = (1 << ($byte_count * 8)) - 1;

                const MIN_INNER: $backing = match_signedness! {
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

                const MAX_INNER: $backing = match_signedness! {
                    match $signed_prefix {
                        SIGNED => { Self::DATA_BITS_MASK >> 1 },
                        UNSIGNED => { Self::DATA_BITS_MASK },
                    }
                };

                /// truncates the upper bytes and sets them to zero
                #[inline(always)]
                pub const fn new(bits: $backing) -> Option<Self> {
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
                pub const unsafe fn from_bits(bits: $backing) -> Self {
                    // Safety: upheld by user
                    unsafe { core::mem::transmute(bits) }
                }

                /// truncates the upper bytes and sets them to zero
                #[inline(always)]
                pub const fn new_truncated(bits: $backing) -> Self {
                    unsafe {
                        Self::from_bits(bits & Self::DATA_BITS_MASK)
                    }
                }

                #[inline(always)]
                pub const fn to_bits(self) -> $backing {
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
                    Self([<$signed_prefix $bit_count Inner>] {
                        zeros: [Zero::_0; { $size - $byte_count }],
                        bytes: self.swapped_bytes()
                    })
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
                pub const fn get(self) -> $backing {
                    match_signedness! {
                        match $signed_prefix {
                            SIGNED => {
                                {
                                    const OFFSET: u32 = $backing::BITS - <[<$signed_prefix:lower $bit_count>]>::BITS;
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
        )*}
        )*
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