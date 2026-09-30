-- |
-- Module      : Test.Amazonka.Sign.V$
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Sign.V4.Chunked (tests) where

import Amazonka.Core hiding (length)
import Amazonka.Prelude hiding (elem)
import Amazonka.Sign.V4
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.List as Conduit
import qualified Data.Foldable as Fold
import qualified Network.HTTP.Conduit as Client
import Test.Amazonka.Arbitrary ()
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
     in elem ("X-Amz-Decoded-Content-Length", "0")

testOneChunkBody :: Property
testOneChunkBody =
  let n = 123
      str = BS8.pack . show
      inp = BS8.replicate n 'a'
   in QC.forAll (mkSigned [inp]) $ \Signed {..} ->
        let elem = (`Fold.elem` Client.requestHeaders signedRequest)
         in elem ("X-Amz-Decoded-Content-Length", str n)

testTwoChunksBody :: Property
testTwoChunksBody =
  let size = fromIntegral defaultChunkSize
      n = 123
      str = BS8.pack . show
      full = BS8.replicate size 'a' -- full-sized chunk
      final = BS8.replicate n 'b' -- final non-empty chunk
   in QC.forAll (mkSigned [full, final]) $ \Signed {..} ->
        let elem = (`Fold.elem` Client.requestHeaders signedRequest)
         in elem ("X-Amz-Decoded-Content-Length", str (size + n))

mkSigned :: [BS8.ByteString] -> Gen (Signed ())
mkSigned bs = do
  aReq <- arbitrary :: Gen (Request ())
  aService <- arbitrary

  let svc = aService {signer = v4}

      req =
        aReq
          { body = Chunked (mkBody bs),
            service = svc
          }

  requestSign req <$> arbitrary <*> arbitrary <*> arbitrary

mkBody :: [BS8.ByteString] -> ChunkedBody
mkBody bs =
  ChunkedBody
    defaultChunkSize
    (fromIntegral . sum $ fmap BS8.length bs)
    (Conduit.sourceList bs)
