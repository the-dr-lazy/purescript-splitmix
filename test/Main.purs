module Test.Main where

import Prelude
import Test.Spec

import Control.Monad.Rec.Class (tailRec)
import Control.Monad.Rec.Class as Recursive
import Control.Monad.State (State, evalState)
import Control.Monad.State as State
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.UInt64 (UInt64)
import Data.UInt64 as UInt64
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Random.SplitMix (SMGen)
import Random.SplitMix as SplitMix
import Test.QuickCheck (quickCheck)
import Test.QuickCheck as QuickCheck
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- https://gist.github.com/blixt/9abfafdd0ada0f4f6f26
--

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "SplitMix" do
    it "should work" do
      let gen = SplitMix.mk 42
      show gen `shouldEqual` "SMGen 9297814886316923340ul 13679457532755275413ul"

      let Tuple n _ = SplitMix.nextUInt64 gen
      show n `shouldEqual` "1275548033995301424ul"

      let Tuple lgen rgen = SplitMix.split gen
      show lgen `shouldEqual` "SMGen 18209985878117922550ul 13679457532755275413ul"
      show rgen `shouldEqual` "SMGen 1275548033995301424ul 10514482549683702313ul"

    it "two splitted generator should never collide" do
      liftEffect <<< quickCheck $
        ( \seed ->
            let
              go :: _ -> Recursive.Step _ QuickCheck.Result
              go { lgen, rgen, results } = do
                let Tuple l lgen' = SplitMix.nextUInt64 lgen
                let Tuple r rgen' = SplitMix.nextUInt64 rgen
                case Set.member l results, Set.member r results of
                  false, false -> do
                    let results' = Set.insert l $ Set.insert r $ results
                    if Set.size results' < 10000 then Recursive.Loop { lgen: lgen', rgen: rgen', results: results' }
                    else Recursive.Done QuickCheck.Success
                  _, _ -> Recursive.Done $ QuickCheck.Failed "Duplicate!"
              Tuple lgen rgen = SplitMix.split (SplitMix.mk seed)
            in
              tailRec go { results: mempty, lgen, rgen }
        )
