{-|
Module     : Random.SplitMix
Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
Copyright  : (c) 2021-2023 Effecful
License    : MPL 2.0

This Source Code Form is subject to the terms of the Mozilla Public
License, version 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Random.SplitMix
  ( SMGen(..)
  , mk
  , nextInt
  , nextNumber
  , nextUInt64
  , split
  )
  where

import Prelude
import Data.Ord (abs)
import Data.UInt64 (UInt64)
import Data.UInt64 as UInt64
import Data.Tuple (Tuple(..))
import Data.Int as Int
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)

newtype SMGen = Unsafe_SMGen { seed :: UInt64, gamma :: UInt64 }

derive newtype instance Show SMGen

-------------------------------------------------------
-- Initialization
--

mk :: Int -> SMGen
mk int32 = Unsafe_SMGen { seed: mix64 seed, gamma: mixGamma (seed + goldenGamma) }
  where seed = unsafePartial (fromJust <<< UInt64.fromInt <<< abs $ int32)

-------------------------------------------------------
-- Operations
--

-- | Generate a 'UInt64'.
--
-- >>> take 3 $ map (printf "%x") $ unfoldr (Just . nextUInt64) (mk 1337) :: [String]
-- ["b5c19e300e8b07b3","d600e0e216c0ac76","c54efc3b3cc5af29"]
--
nextUInt64 :: SMGen -> Tuple UInt64 SMGen
nextUInt64 (Unsafe_SMGen { seed, gamma }) =
  let seed' = seed + gamma in Tuple (mix64 seed') (Unsafe_SMGen { seed: seed', gamma })

nextInt :: SMGen -> Tuple Int SMGen
nextInt (Unsafe_SMGen { seed, gamma }) = Tuple (mix32 seed) (Unsafe_SMGen { seed: seed + gamma, gamma })

nextNumber :: SMGen -> Tuple Number SMGen
nextNumber gen = case nextUInt64 gen of
  Tuple uint64 gen' -> Tuple (UInt64.toNumber (uint64 `UInt64.zshr` UInt64.unsafeFromInt 11) * Int.toNumber ulp) gen'

-------------------------------------------------------
-- Splitting
--

-- | Split a generator into a two uncorrelated generators.
split :: SMGen -> Tuple SMGen SMGen
split (Unsafe_SMGen { seed, gamma }) =
  let
    seed'  = seed + gamma
    seed'' = seed' + gamma
    gen' = Unsafe_SMGen { seed: seed'', gamma }
    gen'' = Unsafe_SMGen { seed: mix64 seed', gamma: mixGamma seed'' }
  in Tuple gen' gen''

-------------------------------------------------------
-- Algorithm
--

ulp :: Int
ulp = Int.pow 2 (-53)

-- | 9E37 79B9 7F4A 7C15
goldenGamma :: UInt64
goldenGamma = unsafePartial (fromJust $ UInt64.fromString "11400714819323198485")

-- | FF51 AFD7 ED55 8CCD
mixA :: UInt64
mixA = unsafePartial (fromJust $ UInt64.fromString "18397679294719823053")

-- | C4CE B9FE 1A85 EC53
mixB :: UInt64
mixB = unsafePartial (fromJust $ UInt64.fromString "14181476777654086739")

-- | BF58 476D 1CE4 E5B9
mixVariantA :: UInt64
mixVariantA = unsafePartial (fromJust $ UInt64.fromString "13787848793156543929")


-- | 94D0 49BB 1331 11EB
mixVariantB :: UInt64
mixVariantB = unsafePartial (fromJust $ UInt64.fromString "10723151780598845931")

-- | AAAA AAAA AAAA AAAA
mixGammaA :: UInt64
mixGammaA = unsafePartial (fromJust $ UInt64.fromString "12297829382473034410")

mix64 :: UInt64 -> UInt64
mix64 z0 =
   -- MurmurHash3Mixer
    let z1 = shiftXorMultiply 33 mixA z0
        z2 = shiftXorMultiply 33 mixB z1
        z3 = shiftXor 33 z2
    in z3

mix32 :: UInt64 -> Int
mix32 z0 =
    let z1 = shiftXorMultiply 33 mixA z0
        z2 = shiftXorMultiply 33 mixB z1
        z3 = shiftXor 32 z2
    in UInt64.lowBits z3

-- used only in mixGamma
mix64variant13 :: UInt64 -> UInt64
mix64variant13 z0 =
   -- Better Bit Mixing - Improving on MurmurHash3's 64-bit Finalizer
   -- http://zimbry.blogspot.fi/2011/09/better-bit-mixing-improving-on.html
   --
   -- Stafford's Mix13
    let z1 = shiftXorMultiply 30 mixVariantA z0 -- MurmurHash3 mix constants
        z2 = shiftXorMultiply 27 mixVariantB z1
        z3 = shiftXor 31 z2
    in z3

mixGamma :: UInt64 -> UInt64
mixGamma z0 =
    let z1 = mix64variant13 z0 `UInt64.or` (UInt64.unsafeFromInt 1)             -- force to be odd
        n  = popCount (z1 `UInt64.xor` (z1 `UInt64.zshr` UInt64.unsafeFromInt 1))
    -- see: http://www.pcg-random.org/posts/bugs-in-splitmix.html
    -- let's trust the text of the paper, not the code.
    in if n >= 24
        then z1
        else z1 `UInt64.xor` mixGammaA

foreign import intPopCount :: Int -> Int

popCount :: UInt64 -> Int
popCount x = intPopCount (UInt64.lowBits x) + intPopCount (UInt64.highBits x)

shiftXor :: Int -> UInt64 -> UInt64
shiftXor n w = w `UInt64.xor` (w `UInt64.zshr` (UInt64.unsafeFromInt n))

shiftXorMultiply :: Int -> UInt64 -> UInt64 -> UInt64
shiftXorMultiply n k w = shiftXor n w * k
