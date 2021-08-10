-- |
-- Module      : Test.AWS.Sign.V$
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.AWS.Sign.V4.Chunked (tests) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.List as Conduit
import qualified Data.Foldable as Fold
import Network.AWS.Core
import Network.AWS.Lens ((.~))
import Network.AWS.Prelude hiding
  ( elem,
  )
import Network.AWS.Sign.V4
import qualified Network.HTTP.Conduit as Client
import Numeric (showHex)
import Test.AWS.Arbitrary ()
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Property ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "headers"
    [ testProperty "empty body" testEmptyBody,
      testProperty "1 chunk body" testOneChunkBody,
      testProperty "2 chunks body" testTwoChunksBody
    ]

testEmptyBody :: Property
testEmptyBody =
  QC.forAll (mkSigned []) $ \Signed {..} ->
    let elem = (`Fold.elem` Client.requestHeaders signedRequest)
     in elem ("Content-Encoding", "aws-chunked")
          && elem ("X-Amz-Decoded-Content-Length", "0")
          && elem ("Content-Length", "86")

testOneChunkBody :: Property
testOneChunkBody =
  let n = 123
      str = BS8.pack . show
      inp = BS8.replicate n 'a'
   in QC.forAll (mkSigned [inp]) $ \Signed {..} ->
        let elem = (`Fold.elem` Client.requestHeaders signedRequest)
         in elem ("Content-Encoding", "aws-chunked")
              && elem ("X-Amz-Decoded-Content-Length", str n)
              && elem ("Content-Length", str (87 + n + 86))

testTwoChunksBody :: Property
testTwoChunksBody =
  let size = fromIntegral defaultChunkSize
      sizeLen = length (showHex size "")
      n = 123
      str = BS8.pack . show
      full = BS8.replicate size 'a' -- full-sized chunk
      final = BS8.replicate n 'b' -- final non-empty chunk
   in QC.forAll (mkSigned [full, final]) $ \Signed {..} ->
        let elem = (`Fold.elem` Client.requestHeaders signedRequest)
         in elem ("Content-Encoding", "aws-chunked")
              && elem ("X-Amz-Decoded-Content-Length", str (size + n))
              && elem ("Content-Length", str (sizeLen + 85 + size + 87 + n + 86))

mkSigned :: [BS8.ByteString] -> Gen (Signed ())
mkSigned bs = do
  aReq <- arbitrary
  aService <- arbitrary

  let svc =
        aService
          & serviceSigner .~ v4

      req =
        aReq
          & requestBody .~ Chunked (mkBody bs)
          & requestService .~ svc

  requestSign req <$> arbitrary <*> arbitrary <*> arbitrary

mkBody :: [BS8.ByteString] -> ChunkedBody
mkBody bs =
  ChunkedBody
    defaultChunkSize
    (fromIntegral . sum $ fmap BS8.length bs)
    (Conduit.sourceList bs)
