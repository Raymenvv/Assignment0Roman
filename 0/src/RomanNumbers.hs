{-|
Module      : RomanNumbers
Description : Convert Roman numbers to integers and the other way around.
Copyright   : Raymen van Veldhoven (1439499)
              Dalton Harmsen (1293885)

RomanNumbers is a module to convert Roman numbers to integers and vice versa. Roman numbers are represented by the characters 'M', 'D', 'C', 'L', 'X', 'V', and 'I'

-}

module RomanNumbers
    ( -- * Convert Roman numbers to integers 
      r2i
      -- * Convert integers to Roman numbers
    , i2r
    ) where


-- Implement and document r2i
-- | Convert a string of Roman numerals (additive definition) to an integer.
--
-- Example:
--
-- >>> r2i "DCL"
-- 650
r2i :: [Char] -- ^ Roman numeral representation
    -> Int -- ^ Integer representation
r2i [] = 0 -- Start with a result of zero.
r2i (r:rs) | r == 'I' = 1 + r2i rs -- Recurse on the roman numerals, reading the characters and appending the result.
           | r == 'V' = 5 + r2i rs
           | r == 'X' = 10 + r2i rs
           | r == 'L' = 50 + r2i rs
           | r == 'C' = 100 + r2i rs
           | r == 'D' = 500 + r2i rs
           | r == 'M' = 1000 + r2i rs
           | otherwise = error ("Unexpected character;" ++ [r] ++ " is not a Roman numeral.") -- When the numeral read is not a valid Roman numeral,  throw an error.


-- Implement and document i2r
-- | Convert an integer to a string of Roman numerals (additive definition).
--
-- Example:
--
-- >>> i2r 1984
-- "MDCCCCLXXXIIII"
i2r :: Int -- ^ Integer representation
    -> [Char] -- ^ Roman numeral representation
i2r 0 = "" -- When the Roman numeral is empty, return 0.
i2r a | a >= 1000 = 'M' : i2r (a-1000) -- Keep subsracting the biggest possible Roman numeral from the integer and append appropriate Roman numeral.
      | a >= 500 = 'D' : i2r (a-500)
      | a >= 100 = 'C' : i2r (a-100)
      | a >= 50 = 'L' : i2r (a-50)
      | a >= 10 = 'X' : i2r (a-10)
      | a >= 5 = 'V' : i2r (a-5)
      | a >= 1 = 'I' : i2r (a-1)
      | otherwise = error "There are no negative Roman numbers" -- When integer is negative, throw an error

