{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main (main) where

import Test.Amazonka.S3.Encryption.Envelope
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "S3-encryption"
      [ testGroup "envelope" envelopeTests
      ]
