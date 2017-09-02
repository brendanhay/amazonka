{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Test.AWS.Sign.V$
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Sign.V4.Chunked (tests) where

import qualified Data.ByteString.Char8     as BS8
import qualified Data.Conduit.List         as CL
import qualified Data.Foldable             as Fold
import           Data.List                 (sort)
import           Data.Monoid
import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Network.HTTP.Conduit      as Client
import           Network.AWS.Lens          ((%~), (.~))
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Numeric (showHex)
import           Test.AWS.Arbitrary        ()
import qualified Test.QuickCheck           as QC
import           Test.QuickCheck.Property  ()
import           Test.Tasty
import           Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "headers"
    [ testProperty "empty body" testEmptyBody
    , testProperty "1 chunk body" testOneChunkBody
    , testProperty "2 chunks body" testTwoChunksBody
    ]


testEmptyBody :: Property
testEmptyBody = QC.forAll (mkSigned []) $ \Signed{..} ->
  let hdrs = Client.requestHeaders sgRequest
   in Fold.elem ("Content-Encoding", "aws-chunked") hdrs &&
      Fold.elem ("X-Amz-Decoded-Content-Length","0") hdrs &&
      Fold.elem ("Content-Length","86") hdrs


testOneChunkBody :: Property
testOneChunkBody = QC.forAll (mkSigned [bsIn]) $ \Signed{..} ->
  let hdrs = Client.requestHeaders sgRequest
   in Fold.elem ("Content-Encoding", "aws-chunked") hdrs &&
      Fold.elem ("X-Amz-Decoded-Content-Length", "123") hdrs &&
      Fold.elem ("Content-Length", (BS8.pack . show) (87 + 123 + 86 :: Int)) hdrs
  where bsIn = BS8.pack $ replicate 123 'a'


testTwoChunksBody :: Property
testTwoChunksBody = QC.forAll (mkSigned [bsIn1, bsIn2]) $ \Signed{..} ->
  let hdrs = Client.requestHeaders sgRequest
   in Fold.elem ("Content-Encoding", "aws-chunked") hdrs &&
      Fold.elem ("X-Amz-Decoded-Content-Length", (BS8.pack . show) (defCs + 123)) hdrs &&
      Fold.elem ("Content-Length", (BS8.pack . show) (defCsHexLen + 85 + defCs + 87 + 123 + 86 :: Int)) hdrs
  where bsIn1 = BS8.pack $ replicate defCs 'a' -- full-sized chunk
        bsIn2 = BS8.pack $ replicate 123 'b' -- final non-empty chunk
        defCs = fromIntegral defaultChunkSize
        defCsHexLen = length $ showHex defCs ""


mkSigned :: [BS8.ByteString] -> Gen (Signed ())
mkSigned bs = do
  aReq <- arbitrary
  aService <- arbitrary

  let svc = serviceSigner .~ v4 $ aService
      req = (rqBody .~ mkBody bs) . (rqService .~ svc) $ aReq

  rqSign req <$> arbitrary <*> arbitrary <*> arbitrary


mkBody :: [BS8.ByteString] -> RqBody
mkBody bs = Chunked $
    ChunkedBody defaultChunkSize
                ((fromIntegral . sum) (BS8.length <$> bs))
                (CL.sourceList bs)
