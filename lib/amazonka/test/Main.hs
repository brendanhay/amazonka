module Main (main) where

import qualified Test.Amazonka.Auth.Background as AuthBackground
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain (testGroup "amazonka" [AuthBackground.tests])
