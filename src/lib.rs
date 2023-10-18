use std::{cmp, fmt, hash, iter, ops};

#[cfg(feature = "i32")]
pub type RawValue = i32;
#[cfg(feature = "i64")]
pub type RawValue = i64;
#[cfg(feature = "i128")]
pub type RawValue = i128;
#[cfg(feature = "isize")]
pub type RawValue = isize;

pub trait Measure: Clone + Copy + PartialEq {
    fn label(self) -> &'static str;
    fn precision(self) -> RawValue;
    fn conversion(self) -> (RawValue, RawValue);
    fn format(self, f: &mut fmt::Formatter, value: RawValue) -> fmt::Result {
        let precision = self.precision();
        let int = value / precision;
        let fract = value % precision;
        write!(f, "{}", int)?;
        if precision > 1 {
            let decimals = precision.ilog10();
            write!(f, ".{:0>1$}", fract, decimals as _)?;
        }
        write!(f, "\u{00A0}{}", self.label())
    }
}

impl<T: Measure> ops::Add for Value<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        let unit = self.unit();
        let other = other.convert(unit);
        let value = self.raw() + other.raw();
        Self(value, unit)
    }
}

impl<T: Measure> ops::Sub for Value<T> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        let unit = self.unit();
        let other = other.convert(unit);
        let value = self.raw() - other.raw();
        Self(value, unit)
    }
}

macro_rules! impl_ops_float {
    ($ty:ty) => {
        impl<T: Measure> ops::Mul<$ty> for Value<T> {
            type Output = Self;

            fn mul(self, other: $ty) -> Self::Output {
                Self((<$ty>::from(self) * other).round() as _, self.unit())
            }
        }

        impl<T: Measure> ops::Mul<Value<T>> for $ty {
            type Output = Value<T>;

            fn mul(self, other: Value<T>) -> Self::Output {
                other * self
            }
        }

        impl<T: Measure> ops::Div<$ty> for Value<T> {
            type Output = Self;

            fn div(self, other: $ty) -> Self::Output {
                Self((<$ty>::from(self) / other).round() as _, self.unit())
            }
        }
    };
}

impl_ops_float!(f64);
impl_ops_float!(f32);

impl<T: Measure> ops::Div for Value<T> {
    type Output = f64;

    fn div(self, other: Self) -> Self::Output {
        <f64>::from(self) / <f64>::from(other.convert(self.unit()))
    }
}

impl<T: Measure + Default> iter::Sum for Value<T> {
    fn sum<I: Iterator<Item = Value<T>>>(iter: I) -> Self {
        iter.reduce(|acc, e| acc + e).unwrap_or_default()
    }
}

#[derive(Clone, Copy, Default)]
pub struct Value<T: Measure>(pub RawValue, pub T);

#[cfg(feature = "implicit-clone")]
impl<T: Measure> implicit_clone::ImplicitClone for Value<T> {}

impl<T: Measure> fmt::Display for Value<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.unit().format(f, self.raw())
    }
}

impl<T: Measure> fmt::Debug for Value<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.unit().format(f, self.raw())
    }
}

impl<T: Measure> Value<T> {
    pub fn convert(self, unit: T) -> Self {
        if self.unit() == unit {
            self
        } else {
            let dst_precision = unit.precision();
            let src_precision = self.unit().precision();
            let (src_ratio, src_offset) = self.unit().conversion();
            let (dst_ratio, dst_offset) = unit.conversion();
            let ratio = dst_precision * dst_ratio / src_ratio;
            let offset = src_offset * ratio - dst_offset;
            let value = self.raw() * ratio / src_precision + offset;
            Self(value, unit)
        }
    }

    #[inline]
    pub fn raw(self) -> RawValue {
        self.0
    }

    #[inline]
    pub fn as_f32(self) -> f32 {
        self.0 as _
    }

    #[inline]
    pub fn as_f64(self) -> f64 {
        self.0 as _
    }

    #[inline]
    pub fn unit(self) -> T {
        self.1
    }

    pub fn min(self, min: impl Into<Option<Self>>) -> Self {
        if let Some(min) = min.into() {
            if self > min {
                return min.convert(self.unit());
            }
        }

        self
    }

    pub fn max(self, max: impl Into<Option<Self>>) -> Self {
        if let Some(max) = max.into() {
            if self < max {
                return max.convert(self.unit());
            }
        }

        self
    }

    pub fn clamp(self, min: impl Into<Option<Self>>, max: impl Into<Option<Self>>) -> Self {
        if let Some(min) = min.into() {
            if self < min {
                return min.convert(self.unit());
            }
        }

        if let Some(max) = max.into() {
            if self > max {
                return max.convert(self.unit());
            }
        }

        self
    }

    pub fn abs(self) -> Self {
        Self(self.raw().abs(), self.unit())
    }
}

impl<T: Measure> PartialEq for Value<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == cmp::Ordering::Equal
    }
}

impl<T: Measure> Eq for Value<T> {}

impl<T: Measure> PartialOrd for Value<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Measure> Ord for Value<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.raw().cmp(&other.convert(self.unit()).raw())
    }
}

impl<T: Measure + hash::Hash> hash::Hash for Value<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.raw().hash(state);
        self.unit().hash(state);
    }
}

impl<T: Measure> From<Value<T>> for f64 {
    fn from(value: Value<T>) -> Self {
        value.as_f64()
    }
}

impl<T: Measure> From<Value<T>> for f32 {
    fn from(value: Value<T>) -> Self {
        value.as_f32()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format() {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
        enum U {
            #[default]
            A,
            B,
            C,
        }

        impl Measure for U {
            fn label(self) -> &'static str {
                match self {
                    Self::A => "A",
                    Self::B => "B",
                    Self::C => "C",
                }
            }

            fn precision(self) -> RawValue {
                match self {
                    Self::A => 1000,
                    Self::B => 10,
                    Self::C => 1,
                }
            }

            fn conversion(self) -> (RawValue, RawValue) {
                (1, 0)
            }
        }

        assert_eq!(Value(1_000, U::A).to_string(), "1.000\u{00A0}A");
        assert_eq!(Value(1_0, U::B).to_string(), "1.0\u{00A0}B");
        assert_eq!(Value(1, U::C).to_string(), "1\u{00A0}C");
    }

    #[test]
    fn convert() {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
        enum U {
            #[default]
            A,
            B,
            C,
        }

        impl Measure for U {
            fn label(self) -> &'static str {
                match self {
                    Self::A => "A",
                    Self::B => "B",
                    Self::C => "C",
                }
            }

            fn precision(self) -> RawValue {
                match self {
                    Self::A | Self::B => 1000,
                    Self::C => 10,
                }
            }

            fn conversion(self) -> (RawValue, RawValue) {
                match self {
                    Self::A => (1, 0),
                    Self::B => (10, 0),
                    Self::C => (100, 0),
                }
            }
        }

        assert_eq!(Value(1_000, U::A).convert(U::B), Value(10_000, U::B));
        assert_eq!(Value(10_000, U::B).convert(U::A), Value(1_000, U::A));
        assert_eq!(Value(1_000, U::A).convert(U::C), Value(100_0, U::C));
        assert_eq!(Value(100_0, U::C).convert(U::A), Value(1_000, U::A));
    }
}
