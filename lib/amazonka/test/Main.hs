module Main (main) where

import qualified Test.Amazonka.Auth.Background as AuthBackground
import qualified Test.Amazonka.Send as Send
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain (testGroup "amazonka" [AuthBackground.tests, Send.tests])
