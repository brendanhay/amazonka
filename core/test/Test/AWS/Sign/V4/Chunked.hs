{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Test.AWS.Sign.V$
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Sign.V4.Chunked (tests) where

import Prelude hiding (elem)

import Data.List   (sort)
import Data.Monoid
import Data.String

import Network.AWS.Lens    ((%~), (&), (.~))
import Network.AWS.Prelude
import Network.AWS.Sign.V4

import Numeric (showHex)

import Test.AWS.Arbitrary       ()
import Test.QuickCheck.Property ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.List     as Conduit
import qualified Data.Foldable         as Fold
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Network.HTTP.Conduit  as Client
import qualified Test.QuickCheck       as QC

tests :: TestTree
tests = testGroup "headers"
    [ testProperty "empty body"    testEmptyBody
    , testProperty "1 chunk body"  testOneChunkBody
    , testProperty "2 chunks body" testTwoChunksBody
    ]

testEmptyBody :: Property
testEmptyBody =
    QC.forAll (mkSigned []) $ \Signed{..} ->
        let elem = (`Fold.elem` Client.requestHeaders sgRequest)
         in elem ("Content-Encoding", "aws-chunked")
         && elem ("X-Amz-Decoded-Content-Length", "0")
         && elem ("Content-Length", "86")

testOneChunkBody :: Property
testOneChunkBody =
    let n   = 123
        str = BS8.pack . show
        inp = BS8.replicate n 'a'

     in QC.forAll (mkSigned [inp]) $ \Signed{..} ->
        let elem = (`Fold.elem` Client.requestHeaders sgRequest)
         in elem ("Content-Encoding", "aws-chunked")
         && elem ("X-Amz-Decoded-Content-Length", str n)
         && elem ("Content-Length", str (87 + n + 86))

testTwoChunksBody :: Property
testTwoChunksBody =
    let size    = fromIntegral defaultChunkSize
        sizeLen = length (showHex size "")
        n       = 123
        str     = BS8.pack . show
        full    = BS8.replicate size 'a' -- full-sized chunk
        final   = BS8.replicate n    'b' -- final non-empty chunk

     in QC.forAll (mkSigned [full, final]) $ \Signed{..} ->
        let elem = (`Fold.elem` Client.requestHeaders sgRequest)
         in elem ("Content-Encoding", "aws-chunked")
         && elem ("X-Amz-Decoded-Content-Length", str (size + n))
         && elem ("Content-Length", str (sizeLen + 85 + size + 87 + n + 86))

mkSigned :: [BS8.ByteString] -> Gen (Signed ())
mkSigned bs = do
    aReq     <- arbitrary
    aService <- arbitrary

    let svc = aService
            & serviceSigner .~ v4

        req = aReq
            & rqBody    .~ Chunked (mkBody bs)
            & rqService .~ svc

    rqSign req <$> arbitrary <*> arbitrary <*> arbitrary

mkBody :: [BS8.ByteString] -> ChunkedBody
mkBody bs =
    ChunkedBody defaultChunkSize
        (fromIntegral . sum $ fmap BS8.length bs)
        (Conduit.sourceList bs)

